pub const PRELUDE: &str = r#"
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    iter::Peekable,
};
use tree_sitter::{Node, TreeCursor};

const DEBUG: bool = false;

pub trait GenericNode<'a>
where
    Self::Child: DeserializeNode<'a>,
{
    const NODE_ID_SET: IntU16Set;
    const NODE_KIND: &'static str;
    const NAMED: bool;

    type Child;

    fn inner_node(&self) -> &Node<'a>;
    fn inner_node_mut(&mut self) -> &mut Node<'a>;
    fn downcast(value: Node<'a>) -> Result<Self, Node<'a>>
    where
        Self: Sized;

    fn all_children(&self) -> Result<Self::Child, DeserializeError> {
        Self::Child::deserialize_at_root(
            &mut self.inner_node().walk(),
            DeserializeMode::AllChildren,
        )
    }

    fn named_children(&self) -> Result<Self::Child, DeserializeError> {
        Self::Child::deserialize_at_root(
            &mut self.inner_node().walk(),
            DeserializeMode::NamedChildren,
        )
    }

    fn field_children(&self, field_id: u16) -> Result<Self::Child, DeserializeError> {
        Self::Child::deserialize_at_root(
            &mut self.inner_node().walk(),
            DeserializeMode::Field(field_id),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeTy<'a, T>(pub T, pub PhantomData<Node<'a>>)
where
    T: GenericNode<'a>;

impl<'a, T> TryFrom<Node<'a>> for NodeTy<'a, T>
where
    T: GenericNode<'a>,
{
    type Error = Node<'a>;

    fn try_from(value: Node<'a>) -> Result<Self, Self::Error> {
        Ok(Self(T::downcast(value)?, PhantomData))
    }
}

impl<'a, T> Deref for NodeTy<'a, T>
where
    T: GenericNode<'a>,
{
    type Target = Node<'a>;

    fn deref(&self) -> &Self::Target {
        self.0.inner_node()
    }
}

impl<'a, T> DerefMut for NodeTy<'a, T>
where
    T: GenericNode<'a>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.inner_node_mut()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeserializeError {
    NoChild,
    WrongType
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeserializeMode {
    AllChildren,
    NamedChildren,
    Field(u16)
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

pub trait DeserializeNode<'a>
where
    Self: Sized,
{
    fn deserialize_at_root(
        tree: &mut TreeCursor<'a>,
        mode: DeserializeMode,
    ) -> Result<Self, DeserializeError>;
    fn deserialize_at_current(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<Self, DeserializeError>;
}

fn generic_deserialize_at_root<'a, T>(
    tree: &mut TreeCursor<'a>,
    mode: DeserializeMode,
    result_on_empty: impl FnOnce() -> Result<T, DeserializeError>,
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
        let res = T::deserialize_at_current(&mut iter);
        debug_assert!(iter.next().is_none(), "Other node on the tree");
        Ok(res?)
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

fn default_deserialize_at_current<'a, T>(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a> + GenericNode<'a>,
{
    if DEBUG {
        let curr_default_name = core::any::type_name::<T>();
        dbg!(curr_default_name);
    }
    T::downcast(*iter.peek().ok_or(DeserializeError::NoChild)?).map_err(|_| DeserializeError::WrongType)
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

    for variant_fun in variant_funs {
        match variant_fun(iter) {
            Ok(node) => return Ok(node),
            Err(DeserializeError::WrongType) => continue,
            Err(error) => return Err(error),
        }
    }

    Err(DeserializeError::WrongType)
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

    fn deserialize_at_current(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_opt_name = core::any::type_name::<Self>();
            dbg!(curr_opt_name);
        }
        Ok(Some(T::deserialize_at_current(iter)?))
    }
}

impl<'a> DeserializeNode<'a> for () {
    fn deserialize_at_root(tree: &mut TreeCursor<'a>, mode: DeserializeMode) -> Result<Self, DeserializeError> {
        if DEBUG {
            let root_unit_name = core::any::type_name::<Self>();
            dbg!(root_unit_name);
        }
        let has_child = tree.goto_first_child();
        dbg!(!has_child);
        Ok(())
    }

    fn deserialize_at_current(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<Self, DeserializeError> {
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
    fn deserialize_at_root(tree: &mut TreeCursor<'a>, mode: DeserializeMode) -> Result<Self, DeserializeError> {
        generic_deserialize_at_root(tree, mode, || Ok(vec![]))
    }

    fn deserialize_at_current(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_vec_name = core::any::type_name::<Self>();
            dbg!(curr_vec_name);
        }
        let mut nodes = Vec::new();
        loop {
            nodes.push(T::deserialize_at_current(iter)?);

            iter.next(); // Move to next node

            if iter.peek().is_none() {
                break Ok(nodes);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IntU16Set {
    Value(u16),
    Range(u16, u16),
    StaticSorted(&'static [u16]),
}

impl IntU16Set {
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
