# tree-sitter-type-gen

Tired of having to refer to tree-sitter's `grammar.js` to find out which nodes go where? tree-sitter-type-gen will generate Rust type definitions for you!

## Usage

Example generated for the `tree-sitter-rust` grammar.
```rust
// main.rs
use tree_sitter_type_gen_rust::{
    nodes::{DeclarationStatement, SourceFile, SourceFileChild},
    tree_sitter, tree_sitter_rust, GenericNode,
};

fn main() {
    let source = r#"
    fn main() {
        println!("Hello world!");
    }
    "#;

    // Tree sitter parsing
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(tree_sitter_rust::language()).unwrap();
    let tree = parser.parse(source, None).unwrap();

    let root = tree.root_node();

    let source_file = SourceFile::downcast(root).unwrap();
    //                ^^^^^^^^^^ Generated type

    for child in source_file.children_named().unwrap() {
        if let SourceFileChild::DeclarationStatement(DeclarationStatement::FunctionItem(function_dec)) = child {
            // ^^^^^^^^^^^^^^^ Possible children     ^^^^^^^^^^^^^^^^^^^^ Generated subtype

            let all_child_nodes = function_dec.children_any().unwrap();
            dbg!(all_child_nodes);
        }
    }
}
```

## Setting Up a Custom tree-sitter Grammar

_Disclaimer: This crate was specifically built and tested on the Rust grammar_

Creating a binding crate for any tree-sitter grammar should be relatively straight forward. Check `grammars/tree-sitter-type-gen-rust` for a template.


## How it Works
tree-sitter generates a static type information file call `node-types.json`. That information is used by a build script to generate the Rust types. This files has a few caviates that need to be worked around:
1. Node kind ids are not store in this file so a runtime language object is needed to embed them.
2. `extras` are not part of this file (you can find them in `grammar.js`)
