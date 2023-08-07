use tree_sitter_type_gen::{
    symbols::{COMMON_PROGRAMMING_SYMBOL_PATTERNS, SYMBOLS_PATTERNS},
    GeneratorBuilder, TypeIdent,
};

fn main() {
    let lang = tree_sitter_rust::language();
    let nodes = tree_sitter_rust::NODE_TYPES;

    let output = GeneratorBuilder::new(lang, nodes)
        .add_extras([
            TypeIdent::new("line_comment", true),
            TypeIdent::new("block_comment", true),
        ])
        .replace_symbol_substitution(
            SYMBOLS_PATTERNS
                .into_iter()
                .chain(COMMON_PROGRAMMING_SYMBOL_PATTERNS)
                .chain([("self", "self_ty"), ("macro_rules!", "macro_rule")]),
        )
        .build();

    println!("{}", output);
}
