#![allow(non_camel_case_types)]

pub use tree_sitter_rust as ts_rust;
pub use tree_sitter_types_common::{self as ts_common, tree_sitter as ts};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
