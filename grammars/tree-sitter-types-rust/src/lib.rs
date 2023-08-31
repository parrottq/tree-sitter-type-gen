#![allow(non_camel_case_types)]

pub use tree_sitter_rust;
pub use tree_sitter_types_common::{self, tree_sitter};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
