
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
    const NODE_ID: u16;
    const NODE_KIND: &'static str;
    const NAMED: bool;

    type Fields;
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
}"#;
