pub const PRELUDE: &str = r#"
use core::iter::Peekable;
use tree_sitter::{Node, TreeCursor};

const DEBUG: bool = false;

/// Stucture that contains exactly one [Node]. Typed nodes implement this.
pub trait NodeContainer<'a> {
    fn upcast(&self) -> Node<'a>;

    fn str_text<'text>(&self, source: &'text str) -> &'text str {
        // 'str' should only contain valid utf8
        self.upcast().utf8_text(source.as_bytes()).unwrap()
    }
}

impl<'a> NodeContainer<'a> for Node<'a> {
    fn upcast(&self) -> Node<'a> {
        *self
    }
}

/// Marks a structure that represents a single node kind. Theses structures contain a single [Node].
pub trait GenericNode<'a>: NodeContainer<'a>
where
    Self::Child: DeserializeNode<'a>,
    Self::Container<Self::Child>: DeserializeNode<'a>,
    // Pretty sure this should be implied but I guess the compiler doesn't want
    // to infer that `NodeOrAny<'a, _>: DeserializeNode<'a>` is always implemented.
    Self::Container<NodeOrAny<'a, Self::Child>>: DeserializeNode<'a>,
{
    const NODE_ID_SET: IntU16Set;
    const NODE_KIND: &'static str;
    const NAMED: bool;

    type Child;
    type Container<T>
    where
        T: DeserializeNode<'a>;

    fn downcast(value: Node<'a>) -> Result<Self, Node<'a>>
    where
        Self: Sized;

    /// All this nodes children (includes unnamed literals in the grammer). Unnamed literals are represented by [NodeOrAny::Any]. Typed children by [NodeOrAny::Typed].
    fn children_any(
        &self,
    ) -> Result<Self::Container<NodeOrAny<'a, Self::Child>>, DeserializeError> {
        Self::Container::<NodeOrAny<Self::Child>>::deserialize_at_root(
            &mut self.upcast().walk(),
            DeserializeMode::AllChildren,
        )
    }

    /// Return all named children of the current node.
    fn children_named(&self) -> Result<Self::Container<Self::Child>, DeserializeError> {
        Self::Container::<Self::Child>::deserialize_at_root(
            &mut self.upcast().walk(),
            DeserializeMode::NamedChildren,
        )
    }

    /// Return children associated with a specific field.
    fn children_field(
        &self,
        field_id: u16,
    ) -> Result<Vec<Self::Child>, DeserializeError> {
        Vec::<Self::Child>::deserialize_at_root(
            &mut self.upcast().walk(),
            DeserializeMode::Field(field_id),
        )
    }
}

/// Error type when turning [Node] into a [GenericNode] fails.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "thiserror", derive(thiserror::Error))]
pub enum DeserializeError {
    /// The current node has no children (when children where expected).
    #[cfg_attr(feature = "thiserror", error("No child node present"))]
    NoChild,
    /// The [Node::kind_id] of one of the [Node] was wrong.
    #[cfg_attr(
        feature = "thiserror",
        error("Wrong node type found (found '{unexpected_kind_id}')")
    )]
    WrongType { unexpected_kind_id: u16 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeserializeMode {
    AllChildren,
    NamedChildren,
    Field(u16),
}

enum DeserializeIter<'a, A, B, C>
where
    A: Iterator<Item = Node<'a>>,
    B: Iterator<Item = Node<'a>>,
    C: Iterator<Item = Node<'a>>,
{
    A(A),
    B(B),
    C(C),
}

impl<'a, A, B, C> Iterator for DeserializeIter<'a, A, B, C>
where
    A: Iterator<Item = Node<'a>>,
    B: Iterator<Item = Node<'a>>,
    C: Iterator<Item = Node<'a>>,
{
    type Item = Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            DeserializeIter::A(e) => e.next(),
            DeserializeIter::B(e) => e.next(),
            DeserializeIter::C(e) => e.next(),
        }
    }
}

/// Implements runtime typechecking of all [Node] into [GenericNode] or variant types. This can also include containers containing nodes such as [Vec] or [Option].
pub trait DeserializeNode<'a>
where
    Self: Sized,
{
    /// Downcast children nodes of the current tree position into a [GenericNode], variant, or a container of the former.
    fn deserialize_at_root(
        tree: &mut TreeCursor<'a>,
        mode: DeserializeMode,
    ) -> Result<Self, DeserializeError>;
    /// Downcast children nodes into a [GenericNode], variant, or a container of the former.
    fn deserialize_at_current(
        iter: &mut Peekable<impl Iterator<Item = Node<'a>>>,
    ) -> Result<Self, DeserializeError>;
}

fn generic_deserialize_at_root<'a, T>(
    tree: &mut TreeCursor<'a>,
    mode: DeserializeMode,
    // Stop monomophizing when all the usages can be function pointers
    // result_on_empty: impl FnOnce() -> Result<T, DeserializeError>,
    result_on_empty: fn() -> Result<T, DeserializeError>,
) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a>,
{
    if DEBUG {
        let root_default_name = core::any::type_name::<T>();
        dbg!(root_default_name);
    }

    let iter = match mode {
        DeserializeMode::AllChildren => DeserializeIter::A(tree.node().children(tree)),
        DeserializeMode::NamedChildren => DeserializeIter::B(tree.node().named_children(tree)),
        DeserializeMode::Field(field_id) => {
            DeserializeIter::C(tree.node().children_by_field_id(field_id, tree))
        }
    };

    let mut iter = iter.peekable();

    if iter.peek().is_some() {
        let res = T::deserialize_at_current(&mut iter)?;
        Ok(res)
    } else {
        result_on_empty()
    }
}

fn default_deserialize_at_root<'a, T>(
    tree: &mut TreeCursor<'a>,
    mode: DeserializeMode,
) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a>,
{
    generic_deserialize_at_root(tree, mode, || Err(DeserializeError::NoChild))
}

fn default_deserialize_at_current<'a, T>(
    iter: &mut Peekable<impl Iterator<Item = Node<'a>>>,
) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a> + GenericNode<'a>,
    // These should be implied by implementing `T: GenericNode<'a>` but whatever, gotta keep the compiler happy.
    <T as GenericNode<'a>>::Container<<T as GenericNode<'a>>::Child>:
        DeserializeNode<'a>,
    <T as GenericNode<'a>>::Container<NodeOrAny<'a, <T as GenericNode<'a>>::Child>>:
        DeserializeNode<'a>,
{
    if DEBUG {
        let curr_default_name = core::any::type_name::<T>();
        dbg!(curr_default_name);
    }
    T::downcast(*iter.peek().ok_or(DeserializeError::NoChild)?).map_err(|node| {
        DeserializeError::WrongType {
            unexpected_kind_id: node.kind_id(),
        }
    })
}

fn variants_deserialize_at_current<'a, const N: usize, T, I>(
    iter: &mut Peekable<I>,
    variant_funs: [fn(&mut Peekable<I>) -> Result<T, DeserializeError>; N],
) -> Result<T, DeserializeError>
where
    I: Iterator<Item = Node<'a>>,
{
    if DEBUG {
        let curr_variant_name = core::any::type_name::<T>();
        dbg!(curr_variant_name);
    }

    let mut node_kind_id: Option<u16> = None;
    for variant_fun in variant_funs {
        match variant_fun(iter) {
            Ok(node) => return Ok(node),
            Err(DeserializeError::WrongType { unexpected_kind_id }) => {
                node_kind_id.get_or_insert(unexpected_kind_id);
                continue;
            }
            Err(error) => return Err(error),
        }
    }

    Err(DeserializeError::WrongType {
        unexpected_kind_id: node_kind_id.expect("Cannot deserialize enum type with no variants"),
    })
}

impl<'a, T> DeserializeNode<'a> for Option<T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(
        tree: &mut TreeCursor<'a>,
        mode: DeserializeMode,
    ) -> Result<Self, DeserializeError> {
        generic_deserialize_at_root(tree, mode, || Ok(None))
    }

    fn deserialize_at_current(
        iter: &mut Peekable<impl Iterator<Item = Node<'a>>>,
    ) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_opt_name = core::any::type_name::<Self>();
            dbg!(curr_opt_name);
        }
        Ok(Some(T::deserialize_at_current(iter)?))
    }
}

impl<'a> DeserializeNode<'a> for () {
    fn deserialize_at_root(
        tree: &mut TreeCursor<'a>,
        _mode: DeserializeMode,
    ) -> Result<Self, DeserializeError> {
        if DEBUG {
            let root_unit_name = core::any::type_name::<Self>();
            dbg!(root_unit_name);
        }
        let has_child = tree.goto_first_child();
        dbg!(!has_child);
        Ok(())
    }

    fn deserialize_at_current(
        _iter: &mut Peekable<impl Iterator<Item = Node<'a>>>,
    ) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_unit_name = core::any::type_name::<Self>();
            dbg!(curr_unit_name);
        }
        // This makes it possible to count children without copying nodes by deserializing into 'Vec<()>'
        Ok(())
    }
}

impl<'a, T> DeserializeNode<'a> for Vec<T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(
        tree: &mut TreeCursor<'a>,
        mode: DeserializeMode,
    ) -> Result<Self, DeserializeError> {
        generic_deserialize_at_root(tree, mode, || Ok(vec![]))
    }

    fn deserialize_at_current(
        iter: &mut Peekable<impl Iterator<Item = Node<'a>>>,
    ) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_vec_name = core::any::type_name::<Self>();
            dbg!(curr_vec_name);
        }
        let mut nodes = Vec::new();
        loop {
            if DEBUG {
                let vec_deserializing = core::any::type_name::<T>();
                let i = nodes.len();
                dbg!((i, vec_deserializing));
            }
            nodes.push(T::deserialize_at_current(iter)?);

            iter.next(); // Move to next node

            if iter.peek().is_none() {
                if DEBUG {
                    let vec_ended = core::any::type_name::<Self>();
                    dbg!(vec_ended);
                }
                break Ok(nodes);
            }
        }
    }
}

/// For when you want to deserialize with [DeserializeNode] but if typechecking a node fails you still want to store it.
#[derive(Debug, Clone, Copy)]
pub enum NodeOrAny<'a, T>
where
    T: DeserializeNode<'a>,
{
    /// A typed node or variant
    Typed(T),
    /// Catch all for when a node isn't [NodeOrAny::Typed]
    Any(Node<'a>),
}

impl<'a, T> DeserializeNode<'a> for NodeOrAny<'a, T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(
        tree: &mut TreeCursor<'a>,
        mode: DeserializeMode,
    ) -> Result<Self, DeserializeError> {
        default_deserialize_at_root(tree, mode)
    }

    fn deserialize_at_current(
        iter: &mut Peekable<impl Iterator<Item = Node<'a>>>,
    ) -> Result<Self, DeserializeError> {
        match T::deserialize_at_current(iter) {
            Ok(result) => Ok(NodeOrAny::Typed(result)),
            Err(DeserializeError::WrongType { .. }) => Ok(NodeOrAny::Any(*iter.peek().unwrap())),
            Err(error) => Err(error),
        }
    }
}

/// A memory efficient set of [u16] that can be check against in a const context.
#[derive(Debug, Clone, Copy)]
pub enum IntU16Set {
    Value(u16),
    Range(u16, u16),
    StaticSorted(&'static [u16]),
}

impl IntU16Set {
    /// Check if the set contains a [u16] value
    #[inline]
    pub const fn contains(&self, value: u16) -> bool {
        match self {
            IntU16Set::Value(e) => *e == value,
            IntU16Set::Range(lower, upper) => {
                let lower = *lower;
                let upper = *upper;
                lower <= value && value <= upper
            }
            IntU16Set::StaticSorted(lst) => {
                // Need to use a recusive solution so that it works in const context.
                // Tail call optimization will turn this into a loop in non-const context.
                // I also tried a binary search version but the generated code was
                // worst when there are only a few values (which is most of the time).
                const fn recursive_search(lst: &[u16], value: u16) -> bool {
                    match lst {
                        [first, rest @ ..] => {
                            if (*first) == value {
                                true
                            } else {
                                recursive_search(rest, value)
                            }
                        }
                        [] => false,
                    }
                }
                recursive_search(lst, value)
            }
        }
    }

    /// [Iterator] of all values in the set
    pub fn iter(&self) -> IntU16SetIter {
        IntU16SetIter(*self)
    }
}

#[derive(Debug, Clone)]
pub struct IntU16SetIter(IntU16Set);

impl Iterator for IntU16SetIter {
    type Item = u16;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            IntU16Set::Value(value) => {
                let value = *value;
                self.0 = IntU16Set::StaticSorted(&[]);
                Some(value)
            }
            IntU16Set::Range(lower, upper) => {
                let next_lower = *lower + 1;
                let lower = *lower;
                let upper = *upper;

                debug_assert!(lower < upper, "Upper range values should always be larger than lower range");

                if next_lower < upper {
                    self.0 = IntU16Set::Range(next_lower, upper);
                } else {
                    self.0 = IntU16Set::Value(next_lower);
                }
                Some(lower)
            }
            IntU16Set::StaticSorted(slice) => match slice {
                [value, rest @ ..] => {
                    let value = *value;
                    self.0 = IntU16Set::StaticSorted(rest);
                    Some(value)
                }
                [] => None,
            },
        }
    }
}

"#;
