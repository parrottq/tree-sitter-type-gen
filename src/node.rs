use std::ops::Add;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Node {
    #[serde(flatten)]
    pub ident: TypeIdent,
    #[serde(default)]
    pub fields: IndexMap<String, Field>,
    #[serde(default)]
    pub children: Option<Field>,
    #[serde(default)]
    pub subtypes: Vec<TypeIdent>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Field {
    pub multiple: bool,
    pub required: bool,
    pub types: Vec<TypeIdent>,
}

impl Field {
    pub fn bounds(&self) -> (usize, Bound) {
        (
            if self.required { 1 } else { 0 },
            if self.multiple {
                Bound::Unbounded
            } else {
                Bound::Bounded(1)
            },
        )
    }

    pub fn from_bounds(bounds: (usize, Bound), types: Vec<TypeIdent>) -> Self {
        match bounds {
            (0, Bound::Unbounded) => Field {
                multiple: true,
                required: false,
                types,
            },
            (_, Bound::Unbounded) => Field {
                multiple: true,
                required: true,
                types,
            },
            (_, Bound::Bounded(0)) => Field {
                multiple: false,
                required: false,
                types,
            },
            (_, Bound::Bounded(_)) => Field {
                multiple: true,
                required: false,
                types,
            },
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeIdent {
    #[serde(rename(deserialize = "type", serialize = "type"))]
    pub ty: String,
    pub named: bool,
}

impl TypeIdent {
    pub fn new(ty: &str, named: bool) -> Self {
        Self {
            ty: ty.into(),
            named,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bound {
    Bounded(usize),
    Unbounded,
}

impl Add for Bound {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Bound::Unbounded, _) => Bound::Unbounded,
            (_, Bound::Unbounded) => Bound::Unbounded,
            (Bound::Bounded(l), Bound::Bounded(r)) => Bound::Bounded(l + r),
        }
    }
}

#[test]
fn test_bound_order() {
    assert_eq!(
        [Bound::Bounded(1), Bound::Unbounded].into_iter().max(),
        Some(Bound::Unbounded)
    );
}
