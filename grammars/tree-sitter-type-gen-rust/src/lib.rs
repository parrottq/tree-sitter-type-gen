#![allow(non_camel_case_types)]

pub use tree_sitter;
pub use tree_sitter_rust;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
