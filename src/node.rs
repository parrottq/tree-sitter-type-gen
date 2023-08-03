use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Node {
    #[serde(rename(deserialize = "type", serialize = "type"))]
    pub ty: String,
    pub named: bool,
    #[serde(default)]
    pub fields: IndexMap<String, Field>,
    #[serde(default)]
    pub children: Option<Field>,
    #[serde(default)]
    pub subtypes: Vec<Subtype>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Field {
    pub multiple: bool,
    pub required: bool,
    pub types: Vec<Subtype>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Subtype {
    #[serde(rename(deserialize = "type", serialize = "type"))]
    pub ty: String,
    pub named: bool,
}
