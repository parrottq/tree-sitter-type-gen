use std::{env, fs, path::PathBuf};

use tree_sitter_type_gen::{
    symbols::{COMMON_PROGRAMMING_SYMBOL_PATTERNS, SYMBOLS_PATTERNS},
    GeneratorBuilder, TypeIdent,
};

fn main() {
    let lang = tree_sitter_rust::language();
    let nodes = tree_sitter_rust::NODE_TYPES;

    let output = GeneratorBuilder::new(lang, nodes)
        .add_extras([ // Extras taken from `grammar.js`
            TypeIdent::new("line_comment", true),
            TypeIdent::new("block_comment", true),
        ])
        .replace_symbol_substitution(
            SYMBOLS_PATTERNS
                .into_iter()
                .chain(COMMON_PROGRAMMING_SYMBOL_PATTERNS)
                .chain([("self", "self_ty"), ("macro_rules!", "macro_rule")]),
        );

    let mut out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    out_path.push("bindings.rs");
    fs::write(out_path, output.build()).unwrap();
}
