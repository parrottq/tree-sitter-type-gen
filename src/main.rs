use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Write,
};

use convert_case::{Case, Casing};

fn rename_type(ty_name: &str) -> String {
    match ty_name {
        "_" => "Base".into(),
        _ => ty_name.from_case(Case::Snake).to_case(Case::Pascal),
    }
}

enum Lazy<'a, const N: usize> {
    Unparsed(&'a ()),
    Parsed([(); N]),
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
        self.wrapper.wrap_ty(self.field_ty.as_str())
    }

    fn cast_ty(&self, src: &str) -> String {
        self.wrapper.cast_wrapper(src, &self.field_ty)
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

    let nodes = treeedbgen::nodes(tree_sitter_rust::NODE_TYPES).unwrap();
    for node in nodes.into_iter().filter(|node| node.named) {
        let ty_name = ty_rename_table.rename(&node.ty);
        // let ty_name = ty_rename_table
        //     .entry(node.ty.clone())
        //     .or_insert_with(|| rename_type(&node.ty).into());

        if node.subtypes.len() > 0 {
            assert_eq!(node.fields.len(), 0);
            let mut output = String::new();
            let f = &mut output;
            let mut lifetime = false;
            for subtype in &node.subtypes {
                let sub_ty_name = ty_rename_table.rename(&subtype.ty);
                writeln!(f, "    /// '{}' renamed to '{}'", subtype.ty, sub_ty_name).unwrap();
                if subtype.named {
                    lifetime = true;
                    writeln!(f, "    {}({}<'a>),", sub_ty_name, sub_ty_name).unwrap();
                } else {
                    writeln!(f, "    {},", sub_ty_name).unwrap();
                }
            }

            println!("enum {}{} {{", ty_name, if lifetime { "<'a>" } else { "" });
            println!("{output}}}");
        } else {
            if node.fields.len() > 0 {
                println!("struct {}<'a>(Node<'a>);", ty_name);
                println!("impl<'a> Deref for {}<'a> {{ type Target = Node<'a>; fn deref(&self) -> &Self::Target {{ &self.0 }} }}", ty_name);

                let mut defered_def = String::new();
                // println!("impl<'a> {}<'a> {{", ty_name);
                // println!("}}");

                let fields: Vec<_> = node
                    .fields
                    .iter()
                    .map(|(field_name, field)| {
                        let field_type = match field.types.as_slice() {
                            [] => "()".to_string(),
                            [single_type] => ty_rename_table.rename(&single_type.ty),
                            types => {
                                let name = format!("{}_{}", ty_name, field_name);

                                let mut enum_def = String::new();
                                let f = &mut enum_def;

                                let mut lifetime = false;
                                for subtype in types {
                                    let sub_ty_name = ty_rename_table.rename(&subtype.ty);
                                    if subtype.named {
                                        lifetime = true;
                                        writeln!(f, "    {}({}<'a>),", sub_ty_name, sub_ty_name)
                                            .unwrap();
                                    } else {
                                        writeln!(f, "    {},", sub_ty_name).unwrap();
                                    }
                                }
                                writeln!(f, "}}").unwrap();

                                writeln!(
                                    &mut defered_def,
                                    "enum {}{} {{{enum_def}",
                                    name,
                                    if lifetime { "<'a>" } else { "" }
                                )
                                .unwrap();

                                name
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
                        Field {
                            wrapper,
                            field_name: field_name.clone(),
                            field_ty: field_type,
                        }
                    })
                    .collect();

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
                        "    pub fn {}(&self) -> {} {{ {} }}",
                        function_rename_table.rename(&field.field_name),
                        field.compound_ty(),
                        field.cast_ty(&format!("self.{i}"))
                    );
                }
                println!("}}");

            } else {
                println!("struct {}<'a>(Node<'a>);", ty_name);
                println!("impl<'a> Deref for {ty_name}<'a> {{ type Target = Node<'a>; fn deref(&self) -> &Self::Target {{ &self.0 }} }}");
            }
        }
    }
}
