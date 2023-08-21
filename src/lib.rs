use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Write,
};

use convert_case::{Case, Casing};
use tree_sitter::Language;

mod lang_gen;
mod node;
mod prelude;
mod rename;
pub mod symbols;
mod type_inference;

use lang_gen::{
    Container, ContainerDef, Enum, Impl, ImplInstruction, Struct, TyConstuctor,
    TyConstuctorIncomplete, TyName,
};
pub use node::TypeIdent;
use node::{Field, Node};
use rename::{RenameTable, SubstitutionTable};

fn make_node_id_set(ids: &[u16]) -> String {
    // Collecting into 'BTreeSet' sorts and removed duplicates
    let mut ids = ids.iter().copied().collect::<BTreeSet<_>>();

    assert!(ids.len() > 0, "Node must have at least one id");

    if ids.len() == 1 {
        return format!("IntU16Set::Value({})", ids.first().unwrap());
    }

    let mut tailing_iter = ids.iter();
    let mut leading_iter = ids.iter();
    leading_iter.next().unwrap(); // ids should have at least one value here

    let mut is_contiguous = true;
    while let (Some(tailing), Some(leading)) = (tailing_iter.next(), leading_iter.next()) {
        if (leading - tailing) == 1 {
            continue;
        } else {
            is_contiguous = false;
            break;
        }
    }

    if is_contiguous {
        format!(
            "IntU16Set::Range({}, {})",
            ids.pop_first().unwrap(),
            ids.pop_last().unwrap()
        )
    } else {
        let mut s = String::from("IntU16Set::StaticSorted(&[");

        for (i, id) in ids.iter().copied().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }
            write!(s, "{}", id).unwrap();
        }

        s.push_str("])");
        s
    }
}

// TODO: Roll this into TypeDef?
pub(crate) type TyDefBare = (
    ContainerDef<TyConstuctorIncomplete>,
    Vec<Impl<TyConstuctorIncomplete>>,
    Vec<Cow<'static, str>>,
);

fn build_variant_type<'a>(
    ty_rename_table: &mut RenameTable<impl FnMut(&TypeIdent) -> Cow<'static, str>>,
    ty_name: TyName,
    subtypes: &'a [TypeIdent],
) -> TyDefBare {
    let mut variants = BTreeMap::new();
    for sub_ty in subtypes {
        let sub_ty_name = TyName::new(ty_rename_table.rename(sub_ty).to_string());

        let variant_ty_const = TyConstuctorIncomplete::new_simple(sub_ty_name.clone());
        let res = variants.insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
        assert!(res.is_none());
    }

    let node_ty: TyConstuctorIncomplete =
        TyConstuctor::new_simple(TyName::new("Node".into()), Cow::Borrowed(&['a'])).into();

    let node_container_ty =
        TyConstuctor::new_simple(TyName::new("NodeContainer".into()), Cow::Borrowed(&['a'])).into();

    let node_container_parts = Impl::new({
        let front_parts = [
            "impl".into(),
            ImplInstruction::DeclareLifetimes,
            " ".into(),
            ImplInstruction::TyConstructor(node_container_ty),
            " for ".into(),
            ImplInstruction::SelfType,
            " { ".into(),
            "fn upcast(&self) -> ".into(),
            ImplInstruction::TyConstructor(node_ty.clone()),
            " { match self { ".into(),
        ];

        let variant_function_parts = subtypes
            .iter()
            .map(|variant_name| {
                let variant_name = ty_rename_table.rename(variant_name).to_string();

                [
                    ty_name.as_ref().to_owned().into(),
                    "::".into(),
                    variant_name.clone().into(),
                    "(inner) => inner.upcast(), ".into(),
                ]
            })
            .flatten();

        let back_parts = ["} } }".into()];
        front_parts
            .into_iter()
            .chain(variant_function_parts)
            .chain(back_parts.into_iter())
            .collect()
    });

    let deserialize_node_parts = Impl::new({
        let deserialize_node_ty =
            TyConstuctor::new_simple(TyName::new("DeserializeNode".into()), Cow::Borrowed(&['a']))
                .into();

        let front_parts = [
                    "impl".into(),
                    ImplInstruction::DeclareLifetimes,
                    " ".into(),
                    ImplInstruction::TyConstructor(deserialize_node_ty),
                    " for ".into(),
                    ImplInstruction::SelfType,
                    " { fn deserialize_at_root(tree: &mut TreeCursor<'a>, mode: DeserializeMode) -> Result<Self, DeserializeError> { default_deserialize_at_root(tree, mode) } fn deserialize_at_current(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<Self, DeserializeError> { let variant_funs: [for<'b> fn(&'b mut Peekable<_>) -> Result<Self, DeserializeError>; ".into(),
                    format!("{}", subtypes.len()).into(),
                    "] = [ ".into()
                ];

        let variant_function_parts = subtypes
            .iter()
            .map(|variant_name| {
                let variant_name = ty_rename_table.rename(variant_name).to_string();

                [
                    "|iter| Ok(Self::".into(),
                    variant_name.clone().into(),
                    "(".into(),
                    variant_name.into(),
                    "::deserialize_at_current(iter)?)), ".into(),
                ]
            })
            .flatten();

        let back_part = ["]; variants_deserialize_at_current(iter, variant_funs) } }".into()];

        front_parts
            .into_iter()
            .chain(variant_function_parts)
            .chain(back_part.into_iter())
            .collect()
    });

    (
        Enum {
            name: ty_name.clone(),
            variants,
        }
        .into(),
        vec![node_container_parts, deserialize_node_parts],
        vec!["#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]".into()],
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContainerType {
    Vec,
    Option,
    Ident,
}

fn build_field_type<'a>(
    declarations: &mut HashMap<TyName, TyDefBare>,
    ty_rename_table: &mut RenameTable<impl FnMut(&TypeIdent) -> Cow<'static, str>>,
    field: &'a Field,
    variant_type_name_fun: impl FnOnce() -> TyName,
) -> (TyConstuctorIncomplete, ContainerType) {
    let (ty_name, lifetime): (_, Option<Cow<'static, [char]>>) = match field.types.as_slice() {
        [] => (TyName::new("()".to_string()), Some(Cow::Borrowed(&[]))),
        [single_type] => (
            TyName::new(ty_rename_table.rename(single_type).to_string()),
            None,
        ),
        types => {
            let sub_ty_name = variant_type_name_fun();
            let res = declarations.insert(
                sub_ty_name.clone(),
                build_variant_type(ty_rename_table, sub_ty_name.clone(), types),
            );
            assert!(res.is_none());
            (sub_ty_name, None)
        }
    };

    if field.multiple {
        // TODO: If multiple and required then at least one item must be inserted into the list. Create data type that verifies this?
        (
            TyConstuctorIncomplete::new_wrapped(ty_name, ("Vec<", ">"), lifetime),
            ContainerType::Vec,
        )
    } else {
        if field.required {
            let mut ty = TyConstuctorIncomplete::new_simple(ty_name);
            ty.lifetime_param = lifetime;
            (ty, ContainerType::Ident)
        } else {
            (
                TyConstuctorIncomplete::new_wrapped(ty_name, ("Option<", ">"), lifetime),
                ContainerType::Option,
            )
        }
    }
}

pub struct GeneratorBuilder {
    lang: Language,
    node_types: &'static str,
    extras: BTreeSet<TypeIdent>,
    symbol_renamer: SubstitutionTable,
}

impl GeneratorBuilder {
    pub fn new(lang: Language, node_types: &'static str) -> Self {
        let empty: [(Cow<'static, str>, Cow<'static, str>); 0] = [];
        let symbol_renamer = SubstitutionTable::new(empty);

        Self {
            lang,
            node_types,
            extras: BTreeSet::new(),
            symbol_renamer,
        }
    }

    pub fn add_extras(mut self, extras: impl IntoIterator<Item = TypeIdent>) -> Self {
        self.extras.extend(extras);
        self
    }

    pub fn replace_symbol_substitution(
        mut self,
        values: impl IntoIterator<Item = (impl Into<Cow<'static, str>>, impl Into<Cow<'static, str>>)>,
    ) -> Self {
        self.symbol_renamer = SubstitutionTable::new(values);
        self
    }

    pub fn build(self) -> String {
        // TODO: Error handling
        build(self)
    }
}

pub(crate) const DEBUG: bool = false;

fn build(build_opts: GeneratorBuilder) -> String {
    let mut nodes = serde_json::from_str::<Vec<Node>>(build_opts.node_types)
        .expect("Could not parse given 'node-types.json'");

    // Add extras to all named nodes
    nodes.iter_mut().for_each(|n| {
        n.fields
            .iter_mut()
            .map(|(name, field)| (Some(name), field))
            .chain(n.children.iter_mut().map(|x| (None, x)))
            .for_each(|(field_name, ident)| {
                // I think you only need to add to fields that have multiple since otherwise the node won't be parse (?)
                if ident.multiple {
                    let mut missing_extras = build_opts.extras.clone();
                    ident.types.iter().for_each(|extra| {
                        missing_extras.remove(extra);
                    });
                    if missing_extras.len() > 0 {
                        if DEBUG {
                            println!(
                                "// {}{} added {:?}",
                                n.ident.ty,
                                field_name
                                    .map(|x| format!("::{x}"))
                                    .unwrap_or(String::new()),
                                missing_extras
                            );
                        }
                    }
                    ident.types.extend(missing_extras);
                }
            });
    });

    let mut kind_id_lookup: HashMap<TypeIdent, Vec<u16>> =
        HashMap::with_capacity(build_opts.lang.node_kind_count());
    for id in 0..build_opts.lang.node_kind_count().try_into().unwrap() {
        let kind = build_opts
            .lang
            .node_kind_for_id(id)
            .expect("Node kind id not present within id range");
        let is_named = build_opts.lang.node_kind_is_named(id);

        let ident = TypeIdent::new(kind, is_named);
        let res = kind_id_lookup.entry(ident).or_default();
        res.push(id);
    }

    let mut counts: BTreeMap<&str, (u8, u8)> = BTreeMap::new();
    for node in nodes.iter() {
        let count = counts.entry(node.ident.ty.as_str()).or_insert((0, 0));
        let count = if node.ident.named {
            &mut count.0
        } else {
            &mut count.1
        };
        *count += 1;
    }

    let mut flagged_count = 0;
    for (name, (t_count, f_count)) in counts {
        assert!(t_count < 2);
        assert!(f_count < 2);
        if t_count > 0 && f_count > 0 {
            flagged_count += 1;
            // println!("{} (true: {}, false: {})", name, t_count, f_count);
        }
    }

    // assert!(flagged_count == 0, "Found overlapping type names");

    // TODO: Check for overlapping names explicitly (or give a nice panic message)
    // TODO: Use more uniform symbol sanitizer
    let mut ty_rename_table = RenameTable::new(|x| match x.ty.as_ref() {
        "_" => "Base".into(),
        _ => format!(
            "{}{}",
            build_opts
                .symbol_renamer
                .substitute_symbols(x.ty.as_str())
                .from_case(Case::Snake)
                .to_case(Case::Pascal),
            if x.named { "" } else { "Lit" }
        )
        .into(),
    });

    let invalid_names = nodes
        .iter()
        .filter_map(|node| {
            let ident: Result<syn::Ident, _> =
                syn::parse_str(ty_rename_table.rename(&node.ident).as_ref());
            if ident.is_err() {
                Some((node.ident.ty.as_str(), node.ident.named))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    if invalid_names.len() > 0 {
        invalid_names
            .into_iter()
            .for_each(|(name, named)| println!("(name: '{}', named: {})", name, named));
        panic!("Invalid names need to be renamed");
    }

    let mut declarations: HashMap<TyName, TyDefBare> = HashMap::with_capacity(nodes.len());

    for node in nodes.iter() {
        let ty_name = ty_rename_table.rename(&node.ident).to_string();
        if DEBUG {
            println!("// Processing '{ty_name}' '{}'", node.ident.ty);
        }

        let children_fields = node
            .children
            .iter()
            .chain(node.fields.iter().map(|x| x.1))
            .collect::<Vec<_>>();

        let child_field: Option<Field> = match children_fields.as_slice() {
            [] => None,
            [field] => Some((*field).clone()),
            slice => {
                let children_bounds = slice
                    .into_iter()
                    .map(|x| x.bounds())
                    .reduce(|r, l| (r.0 + l.0, r.1 + l.1))
                    .unwrap();
                let children_types = slice
                    .into_iter()
                    .map(|field| field.types.as_slice().into_iter())
                    .flatten()
                    .collect::<BTreeSet<_>>(); // Dedup types
                Some(Field::from_bounds(
                    children_bounds,
                    children_types.into_iter().cloned().collect(),
                ))
            }
        };

        // TODO: Create deserializer for just positional children
        let child_ty = if let Some(children) = &child_field {
            build_field_type(&mut declarations, &mut ty_rename_table, children, || {
                TyName::new(format!("{}Child", ty_name))
            })
        } else {
            (
                TyConstuctor::new_simple(TyName::new("()".to_owned()), Cow::Borrowed(&[])).into(),
                ContainerType::Ident,
            )
        };

        if node.subtypes.len() > 0 {
            let mut ty_def = build_variant_type(
                &mut ty_rename_table,
                TyName::new(ty_name.to_string()),
                &node.subtypes,
            );
            ty_def.2.insert(
                0,
                format!(
                    "/// `{}` [tree_sitter::Node]{}",
                    node.ident.ty,
                    if node.ident.named { "" } else { " (literal)" }
                )
                .into(),
            );
            let res = declarations.insert(ty_def.0.name(), ty_def);
            assert!(res.is_none());
            continue;
        }

        let mut impls = vec![];
        if node.fields.len() > 0 {
            let impl_parts = Impl::new(
                [
                    "impl".into(),
                    ImplInstruction::DeclareLifetimes,
                    " ".into(),
                    ImplInstruction::SelfType,
                    " { ".into(),
                ]
                .into_iter()
                .chain(node.fields.iter().flat_map(|(field_name, field)| {
                    let (field_ty, _) =
                        build_field_type(&mut declarations, &mut ty_rename_table, field, || {
                            TyName::new(format!("{}Field_{}", ty_name, field_name)) // TODO: Change rename
                        });

                    let field_name_upper = field_name.to_uppercase();
                    let const_ident_field_name = format!("FIELD_NAME_{field_name_upper}");
                    let const_ident_field_id = format!("FIELD_ID_{field_name_upper}");
                    let field_id = build_opts.lang.field_id_for_name(field_name).unwrap();
                    [
                        "pub const ".into(),
                        const_ident_field_name.into(),
                        ": &str = \"".into(),
                        field_name.clone().into(),
                        "\"; pub const ".into(),
                        const_ident_field_id.clone().into(),
                        ": u16 = ".into(),
                        format!("{field_id}").into(),
                        "; pub fn ".into(),
                        field_name.clone().into(),
                        "_field(self) -> Result<".into(),
                        ImplInstruction::TyConstructor(field_ty),
                        ", DeserializeError> { DeserializeNode::deserialize_at_root(&mut self.0.walk(), DeserializeMode::Field(Self::".into(),
                        const_ident_field_id.into(),
                        ")) }".into(),
                    ]
                }))
                .chain([" }".into()])
                .collect(),
            );

            impls.push(impl_parts);
        }

        let node_ty: TyConstuctorIncomplete =
            TyConstuctor::new_simple(TyName::new("Node".into()), Cow::Borrowed(&['a'])).into();

        let node_container_ty =
            TyConstuctor::new_simple(TyName::new("NodeContainer".into()), Cow::Borrowed(&['a']))
                .into();

        let node_container_parts = Impl::new(
            [
                "impl".into(),
                ImplInstruction::DeclareLifetimes,
                " ".into(),
                ImplInstruction::TyConstructor(node_container_ty),
                " for ".into(),
                ImplInstruction::SelfType,
                " { ".into(),
                "fn upcast(&self) -> ".into(),
                ImplInstruction::TyConstructor(node_ty.clone()),
                " { self.0 } }".into(),
            ]
            .to_vec(),
        );

        let generic_node_ty =
            TyConstuctor::new_simple(TyName::new("GenericNode".into()), Cow::Borrowed(&['a']))
                .into();

        let node_ids = kind_id_lookup.get(&node.ident).unwrap_or_else(|| {
            panic!(
                "ID for node '{}' was not in lookup table",
                node.ident.ty.as_str()
            )
        });

        let generic_node_parts = Impl::new([
            "impl".into(),
            ImplInstruction::DeclareLifetimes,
            " ".into(),
            ImplInstruction::TyConstructor(generic_node_ty),
            " for ".into(),
            ImplInstruction::SelfType,
            " { const NODE_ID_SET: IntU16Set = ".into(),
            make_node_id_set(node_ids.as_slice()).into(),
            "; const NODE_KIND: &'static str = \"".into(),
            format!("{}", node.ident.ty.escape_default()).into(),
            "\"; const NAMED: bool = ".into(),
            format!("{}", node.ident.named).into(),
            "; type Container<T> = ".into(),
            {
                match child_ty.1 {
                    ContainerType::Vec => "Vec<T>",
                    ContainerType::Option => "Option<T>",
                    ContainerType::Ident => "T",
                }
            }.into(),
            " where T: DeserializeNode<'a>; type Child = ".into(),
            ImplInstruction::TyConstructor(child_ty.0.strip_decorators()),
            "; fn downcast(value: ".into(),
            ImplInstruction::TyConstructor(node_ty.clone()),
            ") -> Result<Self, ".into(),
            ImplInstruction::TyConstructor(node_ty.clone()),
            "> { if Self::NODE_ID_SET.contains(value.kind_id()) { Ok(Self(value)) } else { Err(value) } } }".into(),
        ].to_vec());

        let deserialize_node_ty =
            TyConstuctor::new_simple(TyName::new("DeserializeNode".into()), Cow::Borrowed(&['a']))
                .into();

        let deserialize_node_parts = Impl::new([
            "impl".into(),
            ImplInstruction::DeclareLifetimes,
            " ".into(),
            ImplInstruction::TyConstructor(deserialize_node_ty),
            " for ".into(),
            ImplInstruction::SelfType,
            " { fn deserialize_at_root(tree: &mut TreeCursor<'a>, mode: DeserializeMode) -> Result<Self, DeserializeError> { default_deserialize_at_root(tree, mode) } fn deserialize_at_current(iter: &mut Peekable<impl Iterator<Item=Node<'a>>>) -> Result<Self, DeserializeError> { default_deserialize_at_current(iter) } }".into()
        ].to_vec());

        // TODO: Implement literal marker trait
        // TODO: Implement GenericNode downcast (and specialized literal downcast)
        impls.extend([
            node_container_parts,
            generic_node_parts,
            deserialize_node_parts,
        ]);

        let attr = vec![
            format!(
                "/// A typed `{}` [Node]{}",
                node.ident.ty,
                if node.ident.named { "" } else { " (literal)" }
            )
            .into(),
            "#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]".into(),
        ];
        let ty_def: TyDefBare = (
            Struct {
                name: TyName::new(ty_name.to_string()),
                contents: Container::Tuple(vec![node_ty.clone().into()]),
            }
            .into(),
            impls,
            attr,
        );
        let res = declarations.insert(ty_def.0.name(), ty_def);
        assert!(res.is_none());
        continue;
    }

    let declarations_incomplete: BTreeMap<TyName, TyDefBare> = declarations.into_iter().collect();
    // TypeDef lifetime completion
    let declarations_partial_completed =
        type_inference::complete_type_def_generics(declarations_incomplete);
    // Impl lifetime completion
    let declarations_completed =
        type_inference::complete_impl_generics(declarations_partial_completed);

    let mut output = String::new();
    output.push_str(prelude::PRELUDE.trim());
    output.push_str("\npub mod nodes { use super::*;\n");

    for (_ty_name, ty_def) in declarations_completed {
        writeln!(output, "{}\n", ty_def).unwrap();
    }
    output.push_str("}\n");
    output
}
