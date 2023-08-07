use tree_sitter_type_gen::{GeneratorBuilder, TypeIdent};

fn main() {
    let lang = tree_sitter_rust::language();
    let nodes = tree_sitter_rust::NODE_TYPES;

    let output = GeneratorBuilder::new(lang, nodes)
        .add_extras([
            TypeIdent::new("line_comment", true),
            TypeIdent::new("block_comment", true),
        ])
        .build();

    println!("{}", output);
}
