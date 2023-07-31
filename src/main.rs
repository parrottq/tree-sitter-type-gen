use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
};

use convert_case::{Case, Casing};
use structures::TyConstuctorIncomplete;
use treeedbgen::{Node, Subtype};

use crate::structures::{Container, Enum, Struct, TyConstuctor, TyDefinition, TyName};

mod structures;

fn rename_type(ty_name: &str) -> String {
    match ty_name {
        "_" => "Base".into(),
        _ => ty_name.from_case(Case::Snake).to_case(Case::Pascal),
    }
}

#[derive(Debug, Default, Clone)]
struct RenameTable<T>(HashMap<String, String>, T)
where
    T: FnMut(&str) -> String;

impl<T> RenameTable<T>
where
    T: FnMut(&str) -> String,
{
    fn new(convert: T) -> Self {
        Self(Default::default(), convert)
    }

    fn rename(&mut self, ty_name: &str) -> String {
        if let Some(e) = self.0.get(ty_name) {
            return e.clone();
        }

        match self.0.entry(ty_name.to_owned()) {
            Entry::Occupied(_) => unreachable!(),
            Entry::Vacant(entry) => {
                let ty_new = (self.1)(ty_name);
                entry.insert(ty_new).clone()
            }
        }
    }

    fn insert_rename(&mut self, ty_src: String, ty_dst: String) {
        if let Some(_) = self.0.insert(ty_src, ty_dst) {
            panic!()
        }
    }
}

enum FieldType {
    Multiple,
    Required,
    Optional,
}

impl FieldType {
    fn wrap_ty(&self, ty_name: &str) -> String {
        match self {
            FieldType::Multiple => format!("Vec<{ty_name}>"),
            FieldType::Required => format!("{ty_name}"),
            FieldType::Optional => format!("Option<{ty_name}>"),
        }
    }

    fn cast_wrapper(&self, src: &str, ty_name: &str) -> String {
        match self {
            FieldType::Multiple => {
                format!("{src}.iter().map(|x| {ty_name}(x.clone())).collect()")
            }
            FieldType::Required => format!("{ty_name}({src}.clone())"),
            FieldType::Optional => format!("{src}.map(|x| {ty_name}(x.clone()))"),
        }
    }
}

struct Field {
    wrapper: FieldType,
    field_name: String,
    field_ty: String,
}

impl Field {
    fn compound_ty(&self) -> String {
        self.wrapper
            .wrap_ty(&format!("{}<'a>", self.field_ty.as_str()))
    }

    fn cast_ty(&self, src: &str) -> String {
        self.wrapper.cast_wrapper(src, &self.field_ty)
    }
}

enum BuildTypeResult<T> {
    DeferUntilPresent(TyName),
    DeclareFirst { defer_ty_name: TyName, append: T },
    Error(String),
}

fn build_types_with_defer<T>(
    definitions: &mut HashMap<TyName, TyDefinition<TyConstuctorIncomplete>>,
    mut inputs: Vec<T>,
    mut fun: impl FnMut(
        &mut HashMap<TyName, TyDefinition<TyConstuctorIncomplete>>,
        &T,
    ) -> Result<TyDefinition<TyConstuctorIncomplete>, BuildTypeResult<T>>,
) where
    T: Debug,
{
    const DEBUG: bool = true;

    let inputs_len = inputs.len();
    let mut deferals: HashMap<TyName, Vec<T>> = Default::default();
    let mut errors = vec![];

    while let Some(input) = inputs.pop() {
        if DEBUG {
            println!(
                "// deferals({}) errors({}) inputs({})",
                deferals.len(),
                errors.len(),
                inputs.len()
            );
        }

        match fun(definitions, &input) {
            Ok(ty_def) => {
                if DEBUG {
                    println!("// Ok");
                }
                let ty_name = ty_def.name();

                if let Some(deferals) = deferals.remove(&ty_name) {
                    inputs.extend(deferals)
                }

                let res = definitions.insert(ty_name, ty_def);
                assert!(res.is_none());
            }
            Err(BuildTypeResult::DeferUntilPresent(defer_ty_name)) => {
                if DEBUG {
                    println!("// Defer {defer_ty_name}");
                }
                deferals
                    .entry(defer_ty_name)
                    .or_insert_with(|| Default::default())
                    .push(input);
            }
            Err(BuildTypeResult::DeclareFirst {
                append: new_declaration,
                defer_ty_name,
            }) => {
                if DEBUG {
                    println!("// First {new_declaration:?}");
                }
                inputs.push(new_declaration);
                deferals
                    .entry(defer_ty_name)
                    .or_insert_with(|| Default::default())
                    .push(input);
            }
            Err(BuildTypeResult::Error(err)) => {
                if DEBUG {
                    println!("// Error {err}");
                };
                errors.push(err)
            }
        }
    }

    if !errors.is_empty() {
        dbg!(&errors);
        panic!(
            "Errors while processing types total({inputs_len}) errors({})",
            errors.len()
        );
    }

    if !deferals.is_empty() {
        dbg!(deferals.keys().collect::<Vec<_>>());
        panic!(
            "Not all declarations processed total({inputs_len}) left({})",
            deferals.len()
        );
    }
}

fn main() {
    println!("use std::ops::Deref;");
    println!("use tree_sitter::Node;");
    let mut function_rename_table = RenameTable::new(|x| x.into());
    function_rename_table.insert_rename("trait".into(), "trait_".into());
    function_rename_table.insert_rename("type".into(), "type_".into());
    function_rename_table.insert_rename("macro".into(), "macro_".into());

    let mut ty_rename_table = RenameTable::new(rename_type);
    ty_rename_table.insert_rename("self".into(), "SelfTy".into());
    ty_rename_table.insert_rename("!=".into(), "NotEqual".into());
    ty_rename_table.insert_rename("%".into(), "Modulus".into());
    ty_rename_table.insert_rename("&".into(), "BitwiseAnd".into());
    ty_rename_table.insert_rename("&&".into(), "And".into());
    ty_rename_table.insert_rename("*".into(), "Multiply".into());
    ty_rename_table.insert_rename("+".into(), "Addition".into());
    ty_rename_table.insert_rename("-".into(), "Subtract".into());
    ty_rename_table.insert_rename("/".into(), "Divide".into());
    ty_rename_table.insert_rename("<".into(), "LessThan".into());
    ty_rename_table.insert_rename("<<".into(), "LeftShift".into());
    ty_rename_table.insert_rename("<=".into(), "LessThanEqual".into());
    ty_rename_table.insert_rename("==".into(), "Equal".into());
    ty_rename_table.insert_rename(">".into(), "GreaterThan".into());
    ty_rename_table.insert_rename(">=".into(), "GreaterThanEqual".into());
    ty_rename_table.insert_rename(">>".into(), "RightShift".into());
    ty_rename_table.insert_rename("^".into(), "Xor".into());
    ty_rename_table.insert_rename("|".into(), "BitwiseOr".into());
    ty_rename_table.insert_rename("||".into(), "Or".into());
    ty_rename_table.insert_rename("%=".into(), "AssignModulus".into());
    ty_rename_table.insert_rename("&=".into(), "AssignBitwiseAnd".into());
    ty_rename_table.insert_rename("*=".into(), "AssignMultiply".into());
    ty_rename_table.insert_rename("+=".into(), "AssignAddition".into());
    ty_rename_table.insert_rename("-=".into(), "AssignSubtract".into());
    ty_rename_table.insert_rename("/=".into(), "AssignDivide".into());
    ty_rename_table.insert_rename("<<=".into(), "AssignLeftShift".into());
    ty_rename_table.insert_rename(">>=".into(), "AssignRightShift".into());
    ty_rename_table.insert_rename("^=".into(), "AssignXor".into());
    ty_rename_table.insert_rename("|=".into(), "AssignOr".into());

    #[derive(Debug)]
    enum Input {
        Node(Node),
        FieldValues {
            name: TyName,
            subtypes: Vec<(String, bool)>,
        },
    }

    let nodes = treeedbgen::nodes(tree_sitter_rust::NODE_TYPES).unwrap();
    let nodes: Vec<_> = nodes
        .into_iter()
        .filter(|x| x.named)
        .map(Input::Node)
        .collect();

    let mut declarations: HashMap<TyName, TyDefinition<TyConstuctorIncomplete>> =
        HashMap::with_capacity(nodes.len());

    build_types_with_defer(&mut declarations, nodes, |declarations, input| {
        match input {
            Input::FieldValues { name, subtypes } => {
                println!("// Processing field '{name}'");
                // let mut enum_def = String::new();
                // let f = &mut enum_def;

                let mut variants = HashMap::new();
                for (ty, named) in subtypes {
                    let sub_ty_name = ty_rename_table.rename(&ty);
                    let sub_ty_name = TyName::new(sub_ty_name);

                    if *named {
                        let variant_ty_const = TyConstuctorIncomplete {
                            name: sub_ty_name.clone(),
                            lifetime_param: None,
                        }; //variant.ty_constructor();
                        let res =
                            variants.insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
                        assert!(res.is_none());
                        // writeln!(f, "    {}({}<'a>),", sub_ty_name, sub_ty_name).unwrap();
                    } else {
                        let res = variants.insert(sub_ty_name, Container::Tuple(vec![]));
                        assert!(res.is_none());
                        // writeln!(f, "    {},", sub_ty_name).unwrap();
                    }
                }
                // writeln!(f, "}}").unwrap();

                // writeln!(
                //     &mut defered_def,
                //     "enum {}<'a> {{\n{enum_def}",
                //     name,
                //     // if lifetime { "<'a>" } else { "" }
                // )
                // .unwrap();

                // Err(BuildTypeResult::Error(format!(
                //     "Field values subtype not implemented"
                // )));

                Ok(Enum {
                    name: name.clone(),
                    variants,
                }
                .into())
            }
            Input::Node(node) => {
                let ty_name = ty_rename_table.rename(&node.ty);
                println!("// Processing '{ty_name}' '{}'", node.ty);
                // dbg!(node);
                // let ty_name = ty_rename_table
                //     .entry(node.ty.clone())
                //     .or_insert_with(|| rename_type(&node.ty).into());

                if node.subtypes.len() > 0 {
                    assert_eq!(node.fields.len(), 0);
                    // let mut output = String::new();
                    // let f = &mut output;
                    let mut variants = HashMap::with_capacity(node.subtypes.len());

                    for subtype in &node.subtypes {
                        let sub_ty_name = TyName::new(ty_rename_table.rename(&subtype.ty));
                        // writeln!(f, "    /// '{}' renamed to '{}'", subtype.ty, sub_ty_name).unwrap();
                        if subtype.named {
                            // let variant = declarations.get(&sub_ty_name).ok_or_else(|| {
                            //     BuildTypeResult::DeferUntilPresent(sub_ty_name.clone())
                            // })?;
                            let variant_ty_const = TyConstuctorIncomplete {
                                name: sub_ty_name.clone(),
                                lifetime_param: None,
                            }; //variant.ty_constructor();
                            let res = variants
                                .insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
                            assert!(res.is_none());
                            // writeln!(f, "    {}({}<'a>),", sub_ty_name, sub_ty_name).unwrap();
                        } else {
                            let res = variants.insert(sub_ty_name, Container::Tuple(vec![]));
                            assert!(res.is_none());
                            // writeln!(f, "    {},", sub_ty_name).unwrap();
                        }
                    }

                    // println!("enum {}<'a> {{", ty_name);
                    // println!("enum {}{} {{", ty_name, if lifetime { "<'a>" } else { "" });
                    // println!("{output}}}");
                    return Ok(Enum {
                        name: TyName::new(ty_name),
                        variants,
                    }
                    .into());
                }

                if node.fields.len() > 0 {
                    // println!("struct {}<'a>(Node<'a>);", ty_name);

                    let mut defered_def = String::new();
                    // println!("impl<'a> {}<'a> {{", ty_name);
                    // println!("}}");
                    let mut fields: Vec<_> = Vec::with_capacity(node.fields.len());

                    for (field_name, field) in &node.fields {
                        let field_ty = match field.types.as_slice() {
                            [] => "()".to_string(),
                            [single_type] => ty_rename_table.rename(&single_type.ty),
                            types => {
                                let name = format!("{}_{}", ty_name, field_name);

                                let sub_ty_name = TyName::new(name.clone());
                                if let Some(ty_def) = declarations.get(&sub_ty_name) {
                                    ty_def.name().to_string() // TODO: Pass TyConst to fields instead
                                } else {
                                    // dbg!(declarations.keys().collect::<Vec<_>>());
                                    return Err(BuildTypeResult::DeclareFirst {
                                        defer_ty_name: sub_ty_name.clone(),
                                        append: Input::FieldValues {
                                            name: sub_ty_name,
                                            subtypes: types
                                                .iter()
                                                .map(|x| (x.ty.clone(), x.named))
                                                .collect(),
                                        },
                                    });
                                }
                            }
                        };

                        let wrapper = if field.multiple {
                            FieldType::Multiple
                        } else {
                            if field.required {
                                FieldType::Required
                            } else {
                                FieldType::Optional
                            }
                        };

                        fields.push(Field {
                            wrapper,
                            field_name: field_name.clone(),
                            field_ty,
                        });
                    }

                    let field_ty_name = format!("{ty_name}Fields");
                    println!("impl<'a> {}<'a> {{", ty_name);
                    for (i, field) in fields.iter().enumerate() {
                        // println!(
                        //     "    pub fn fields(&self) -> {} {{ {} }}",
                        //     field_ty_name,
                        //     field.cast_ty(&format!("self.{i}"))
                        // );
                    }
                    println!("}}");

                    if !defered_def.is_empty() {
                        println!("{defered_def}");
                    }

                    println!("struct {}<'a>(", field_ty_name);
                    for field in fields.iter() {
                        println!("    {},", field.compound_ty())
                    }
                    println!(");");

                    println!("impl<'a> {}<'a> {{", field_ty_name);
                    for (i, field) in fields.iter().enumerate() {
                        println!(
                            "    pub fn {}_node(&self) -> {} {{ self.{i}.clone() }}",
                            field.field_name,
                            field.wrapper.wrap_ty("Node<'a>")
                        );
                        println!(
                            "    pub fn {}(&self) -> {} {{ self.{i}.clone() }}",
                            function_rename_table.rename(&field.field_name),
                            field.compound_ty(),
                            // field.cast_ty(&format!("self.{i}"))
                        );
                    }
                    println!("}}");
                } else {
                    // println!("struct {}<'a>(Node<'a>);", ty_name);
                }

                println!("impl<'a> Deref for {ty_name}<'a> {{ type Target = Node<'a>; fn deref(&self) -> &Self::Target {{ &self.0 }} }}");

                let node_ty = TyConstuctorIncomplete {
                    name: TyName::new("Node".into()),
                    lifetime_param: Some(vec!["a".into()]),
                };

                Ok(Struct {
                    name: TyName::new(ty_name),
                    contents: Container::Tuple(vec![node_ty]),
                }
                .into())
            }
        }
        // Err(BuildTypeResult::DeferUntilPresent(TyName::new(ty_name)))
    });

    let mut declarations_incomplete = declarations;
    let mut declarations_completed: HashMap<TyName, TyDefinition<TyConstuctor>> =
        HashMap::with_capacity(declarations_incomplete.len());
    let mut checking_stack: Vec<TyName> = vec![];

    loop {
        let ty_name = match checking_stack.as_slice() {
            [front @ .., ty_name] => {
                if let Some(e) = front.iter().find(|x| ty_name.eq(x)) {
                    todo!("Cycle found"); // TODO: Handle cycles
                }

                ty_name.clone()
            }
            [] => {
                if let Some(ty_name) = declarations_incomplete.keys().next() {
                    checking_stack.push(ty_name.clone());
                    ty_name.clone()
                } else {
                    break;
                }
            }
        };

        let ty_def = declarations_incomplete.get_mut(&ty_name).unwrap();
        match ty_def.to_complete() {
            Ok(completed) => {
                declarations_incomplete.remove(&ty_name).unwrap();
                declarations_completed.insert(ty_name.clone(), completed);
                checking_stack.pop();
            }
            Err(incomplete_ty_def) => {
                let next_ty_name = incomplete_ty_def.name.clone();

                if let Some(e) = declarations_completed.get(&next_ty_name) {
                    incomplete_ty_def.lifetime_param = Some(e.ty_constructor().lifetime_param);
                } else {
                    checking_stack.push(declarations_incomplete.get(&next_ty_name).unwrap().name());
                }
            }
        }
    }

    assert!(declarations_incomplete.is_empty());

    let mut declare_keys = declarations_completed.keys().collect::<Vec<_>>();
    declare_keys.sort();

    for ty_name in declare_keys {
        let ty_def = declarations_completed.get(ty_name).unwrap();
        println!("/// {}", ty_name);
        println!("{}", ty_def);
        println!();
    }
}
