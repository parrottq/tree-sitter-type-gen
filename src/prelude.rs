pub const PRELUDE: &str = r#"
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
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

    fn children(&self) -> Result<Self::Child, DeserializeError> {
        Self::Child::deserialize_at_root(&mut self.inner_node().walk())
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

pub trait DeserializeNode<'a>
where
    Self: Sized,
{
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError>;
    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError>;
}

fn default_deserialize_at_root<'a, T>(tree: &mut TreeCursor<'a>) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a>
{
    if DEBUG {
        let root_default_name = core::any::type_name::<T>();
        dbg!(root_default_name);
    }
    if tree.goto_first_child() {
        let res = T::deserialize_at_current(tree);
        let has_sibling = tree.goto_next_sibling();
        debug_assert!(!has_sibling);
        let has_parent = tree.goto_parent();
        debug_assert!(has_parent);
        Ok(res?)
    } else {
        Err(DeserializeError::NoChild)
    }
}

fn default_deserialize_at_current<'a, T>(tree: &mut TreeCursor<'a>) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a> + GenericNode<'a>,
{
    if DEBUG {
        let curr_default_name = core::any::type_name::<T>();
        dbg!(curr_default_name);
    }
    T::downcast(tree.node()).map_err(|_| DeserializeError::WrongType)
}

fn variants_deserialize_at_current<'a, const N: usize, T>(
    tree: &mut TreeCursor<'a>,
    variant_funs: [fn(&mut TreeCursor<'a>) -> Result<T, DeserializeError>; N]
) -> Result<T, DeserializeError> {
    if DEBUG {
        let curr_variant_name = core::any::type_name::<T>();
        dbg!(curr_variant_name);
    }
    for variant_fun in variant_funs {
        match variant_fun(tree) {
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
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let root_opt_name = core::any::type_name::<Self>();
            dbg!(root_opt_name);
        }
        if tree.goto_first_child() {
            let res = Self::deserialize_at_current(tree);
            let has_sibling = tree.goto_next_sibling();
            debug_assert!(!has_sibling);
            let has_parent = tree.goto_parent();
            debug_assert!(has_parent);
            Ok(res?)
        } else {
            Err(DeserializeError::NoChild) // TODO: Reuse this function with default impl?
        }
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_opt_name = core::any::type_name::<Self>();
            dbg!(curr_opt_name);
        }
        Ok(Some(T::deserialize_at_current(tree)?))
    }
}

impl<'a> DeserializeNode<'a> for () {
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let root_unit_name = core::any::type_name::<Self>();
            dbg!(root_unit_name);
        }
        let has_child = tree.goto_first_child();
        dbg!(!has_child);
        Ok(())
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
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
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let root_vec_name = core::any::type_name::<Self>();
            dbg!(root_vec_name);
        }
        if tree.goto_first_child() {
            let res = Self::deserialize_at_current(tree);
            let has_parent = tree.goto_parent();
            debug_assert!(has_parent);
            Ok(res?)
        } else {
            Ok(vec![])
        }
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if DEBUG {
            let curr_vec_name = core::any::type_name::<Self>();
            dbg!(curr_vec_name);
        }
        let mut nodes = Vec::new();
        loop {
            nodes.push(T::deserialize_at_current(tree)?);

            if !tree.goto_next_sibling() {
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
