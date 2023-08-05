use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
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

#[derive(Serialize, Deserialize, Debug)]
pub struct Field {
    pub multiple: bool,
    pub required: bool,
    pub types: Vec<TypeIdent>,
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
