use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap},
    fmt::Debug,
};

use convert_case::{Case, Casing};
use treeedbgen::Node;

mod lang_gen;

use lang_gen::{
    Container, ContainerDef, Enum, Impl, ImplInstruction, IntoCompleted, Struct, TyConstuctor,
    TyConstuctorIncomplete, TyName, TypeDef,
};

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

struct Field {
    field_name: String,
    field_ty: TyConstuctorIncomplete,
}

enum BuildTypeResult<T> {
    DeclareFirst { defer_ty_name: TyName, append: T },
    Error(String),
}

// TODO: Roll this into TypeDef?
type TyDefBare = (
    ContainerDef<TyConstuctorIncomplete>,
    Vec<Impl<TyConstuctorIncomplete>>,
);

fn build_types_with_defer<T>(
    definitions: &mut HashMap<TyName, TyDefBare>,
    mut inputs: Vec<T>,
    mut fun: impl FnMut(&mut HashMap<TyName, TyDefBare>, &T) -> Result<TyDefBare, BuildTypeResult<T>>,
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
                let ty_name = ty_def.0.name();

                if let Some(deferals) = deferals.remove(&ty_name) {
                    inputs.extend(deferals)
                }

                let res = definitions.insert(ty_name, ty_def);
                assert!(res.is_none());
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
    let prelude = "
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};
use tree_sitter::Node;

pub trait GenericNode<'a> {
    const NODE_ID: u16;
    const NODE_KIND: &'static str;

    type Fields;

    fn inner_node(&self) -> &Node<'a>;
    fn inner_node_mut(&mut self) -> &mut Node<'a>;
    fn downcast(value: Node<'a>) -> Result<Self, Node<'a>>
    where
        Self: Sized;
}

pub struct NodeTy<'a, T>(T, PhantomData<Node<'a>>)
where
    T: GenericNode<'a>;

impl<'a, T> TryFrom<Node<'a>> for NodeTy<'a, T>
where
    T: GenericNode<'a>,
{
    type Error = Node<'a>;

    fn try_from(value: Node<'a>) -> Result<Self, Self::Error> {
        Ok(Self(T::downcast(value)?, PhantomData))
    }
}

impl<'a, T> Deref for NodeTy<'a, T>
where
    T: GenericNode<'a>,
{
    type Target = Node<'a>;

    fn deref(&self) -> &Self::Target {
        self.0.inner_node()
    }
}

impl<'a, T> DerefMut for NodeTy<'a, T>
where
    T: GenericNode<'a>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.inner_node_mut()
    }
}
    "
    .trim();
    println!("{}", prelude);

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

    let lang = tree_sitter_rust::language();

    let nodes = treeedbgen::nodes(tree_sitter_rust::NODE_TYPES).unwrap();
    let nodes: Vec<_> = nodes
        .into_iter()
        .filter(|x| x.named)
        .map(Input::Node)
        .collect();

    let mut declarations: HashMap<TyName, TyDefBare> = HashMap::with_capacity(nodes.len());

    build_types_with_defer(&mut declarations, nodes, |declarations, input| {
        match input {
            Input::FieldValues { name, subtypes } => {
                println!("// Processing field '{name}'");

                let mut variants = BTreeMap::new();
                for (ty, named) in subtypes {
                    let sub_ty_name = ty_rename_table.rename(&ty);
                    let sub_ty_name = TyName::new(sub_ty_name);

                    if *named {
                        let variant_ty_const =
                            TyConstuctorIncomplete::new_simple(sub_ty_name.clone());
                        let res =
                            variants.insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
                        assert!(res.is_none());
                    } else {
                        let res = variants.insert(sub_ty_name, Container::Tuple(vec![]));
                        assert!(res.is_none());
                    }
                }

                Ok((
                    Enum {
                        name: name.clone(),
                        variants,
                    }
                    .into(),
                    vec![],
                ))
            }
            Input::Node(node) => {
                let ty_name = ty_rename_table.rename(&node.ty);
                println!("// Processing '{ty_name}' '{}'", node.ty);

                if node.subtypes.len() > 0 {
                    assert_eq!(node.fields.len(), 0);
                    let mut variants = BTreeMap::new();

                    for subtype in &node.subtypes {
                        let sub_ty_name = TyName::new(ty_rename_table.rename(&subtype.ty));
                        if subtype.named {
                            let variant_ty_const =
                                TyConstuctorIncomplete::new_simple(sub_ty_name.clone());
                            let res = variants
                                .insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
                            assert!(res.is_none());
                        } else {
                            let res = variants.insert(sub_ty_name, Container::Tuple(vec![]));
                            assert!(res.is_none());
                        }
                    }

                    return Ok((
                        Enum {
                            name: TyName::new(ty_name),
                            variants,
                        }
                        .into(),
                        vec![],
                    ));
                }

                let fields_ty: TyConstuctorIncomplete = if node.fields.len() > 0 {
                    let mut fields: Vec<_> = Vec::with_capacity(node.fields.len());

                    let mut pre_fields = node.fields.iter().collect::<Vec<_>>();
                    pre_fields.sort_by_key(|x| x.0);
                    for (field_name, field) in pre_fields {
                        let ty_name = match field.types.as_slice() {
                            [] => TyName::new("()".to_string()),
                            [single_type] => TyName::new(ty_rename_table.rename(&single_type.ty)),
                            types => {
                                let name = format!("{}_{}", ty_name, field_name);

                                let sub_ty_name = TyName::new(name.clone());
                                if let Some(ty_def) = declarations.get(&sub_ty_name) {
                                    ty_def.0.name()
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

                        let field_ty = if field.multiple {
                            TyConstuctorIncomplete::new(("Vec<".into(), ty_name, ">".into()), None)
                        } else {
                            if field.required {
                                TyConstuctorIncomplete::new_simple(ty_name)
                            } else {
                                TyConstuctorIncomplete::new(
                                    ("Option<".into(), ty_name, ">".into()),
                                    None,
                                )
                            }
                        };

                        fields.push(Field {
                            field_name: field_name.clone(),
                            field_ty,
                        });
                    }

                    let field_ty_name = format!("{ty_name}Fields");
                    // println!("impl<'a> {}<'a> {{", ty_name);
                    for (i, field) in fields.iter().enumerate() {
                        // println!(
                        //     "    pub fn fields(&self) -> {} {{ {} }}",
                        //     field_ty_name,
                        //     field.cast_ty(&format!("self.{i}"))
                        // );
                    }
                    // println!("}}");

                    let contents =
                        Container::Tuple(fields.into_iter().map(|x| x.field_ty).collect());

                    let res = declarations.insert(
                        TyName::new(field_ty_name.clone()),
                        (
                            Struct {
                                name: TyName::new(field_ty_name.clone()),
                                contents,
                            }
                            .into(),
                            vec![],
                        ),
                    );
                    assert!(res.is_none());

                    TyConstuctorIncomplete::new_simple(TyName::new(field_ty_name.into()))
                } else {
                    TyConstuctor::new_simple(TyName::new("()".into()), vec![]).into()
                };

                let node_ty: TyConstuctorIncomplete =
                    TyConstuctor::new_simple(TyName::new("Node".into()), vec!["a".into()]).into();

                let generic_node_ty =
                    TyConstuctor::new_simple(TyName::new("GenericNode".into()), vec!["a".into()])
                        .into();

                let node_id = lang.id_for_node_kind(&node.ty, true);
                let generic_node_parts =
                Impl::new([
                    "impl".into(),
                    ImplInstruction::DeclareLifetimes,
                    " ".into(),
                    ImplInstruction::TyConstructor(generic_node_ty),
                    " for ".into(),
                    ImplInstruction::SelfType,
                    " { const NODE_ID: u16 = ".into(),
                    format!("{node_id}").into(),
                    "; const NODE_KIND: &'static str = \"".into(),
                    format!("{}", node.ty.escape_default()).into(),
                    "\"; type Fields = ".into(),
                    ImplInstruction::TyConstructor(fields_ty),
                    "; fn inner_node(&self) -> &".into(),
                    ImplInstruction::TyConstructor(node_ty.clone()),
                    " { &self.0 } fn inner_node_mut(&mut self) -> &mut ".into(),
                    ImplInstruction::TyConstructor(node_ty.clone()),
                    " { &mut self.0 } fn downcast(value: ".into(),
                    ImplInstruction::TyConstructor(node_ty.clone()),
                    ") -> Result<Self, ".into(),
                    ImplInstruction::TyConstructor(node_ty.clone()),
                    "> { if value.kind_id() == Self::NODE_ID { Ok(Self(value)) } else { Err(value) } } }".into(),
                ].to_vec());

                Ok((
                    Struct {
                        name: TyName::new(ty_name),
                        contents: Container::Tuple(vec![node_ty.clone().into()]),
                    }
                    .into(),
                    vec![generic_node_parts],
                ))
            }
        }
    });

    let mut declarations_incomplete: BTreeMap<TyName, TyDefBare> =
        declarations.into_iter().collect();
    let mut declarations_partial_completed: BTreeMap<
        TyName,
        (
            ContainerDef<TyConstuctor>,
            Vec<Impl<TyConstuctorIncomplete>>,
        ),
    > = BTreeMap::new();
    let mut checking_stack: Vec<TyName> = vec![];

    loop {
        let ty_name = match checking_stack.as_slice() {
            [front @ .., ty_name] => {
                if let Some(_ty) = front.iter().find(|x| ty_name.eq(x)) {
                    for stack in checking_stack.iter() {
                        println!("- {}", stack);
                    }

                    todo!("Cycle found"); // TODO: Handle cycles
                }

                ty_name.clone()
            }
            [] => {
                if let Some((ty_name, _)) = declarations_incomplete.first_key_value() {
                    checking_stack.push(ty_name.clone());
                    ty_name.clone()
                } else {
                    break;
                }
            }
        };

        println!(
            "// name({}) completed({}) incomplete({})",
            ty_name,
            declarations_partial_completed.len(),
            declarations_incomplete.len()
        );

        let ty_def = declarations_incomplete.get_mut(&ty_name).unwrap();
        match ty_def.0.into_completed() {
            Ok(completed) => {
                println!("// Ok");
                let (_, impls) = declarations_incomplete.remove(&ty_name).unwrap();
                declarations_partial_completed.insert(ty_name.clone(), (completed, impls));
                checking_stack.pop();
            }
            Err(incomplete_ty_def) => {
                println!(
                    "// Err {} {}",
                    incomplete_ty_def.primary_type_name(),
                    ty_name
                );
                let next_ty_name = incomplete_ty_def.primary_type_name().clone();

                if let Some((container, _)) = declarations_partial_completed.get(&next_ty_name) {
                    incomplete_ty_def.lifetime_param =
                        Some(container.ty_constructor().lifetime_param);
                } else {
                    checking_stack
                        .push(declarations_incomplete.get(&next_ty_name).unwrap().0.name());
                }
            }
        }
    }

    assert!(declarations_incomplete.is_empty());

    let mut declarations_completed: BTreeMap<TyName, TypeDef<TyConstuctor>> = BTreeMap::new();
    while let Some(entry) = declarations_partial_completed.first_entry() {
        let (ty_name, (ty_container, ty_impls)) = entry.remove_entry();

        let mut ty_def = TypeDef::new(ty_container);

        for mut ty_impl in ty_impls {
            let f = loop {
                match ty_impl.into_completed() {
                    Ok(done) => break done,
                    Err(partial) => {
                        let partial_ty_name = partial.primary_type_name();
                        if partial_ty_name == &ty_name {
                            partial.lifetime_param = Some(ty_def.ty_constructor().lifetime_param);
                            continue;
                        }
                        if let Some(ty_def) = declarations_completed.get(&partial_ty_name) {
                            partial.lifetime_param = Some(ty_def.ty_constructor().lifetime_param);
                            continue;
                        }
                        if let Some((ty_container, _ty_impls)) =
                            declarations_partial_completed.get(&partial_ty_name)
                        {
                            partial.lifetime_param =
                                Some(ty_container.ty_constructor().lifetime_param);
                            continue;
                        }

                        panic!("Type name {} not found", partial_ty_name);
                    }
                }
            };
            ty_def.push_impl(f);
        }

        let res = declarations_completed.insert(ty_name, ty_def);
        assert!(res.is_none());
    }

    for (ty_name, ty_def) in declarations_completed {
        println!("/// {}", ty_name);
        println!("{}", ty_def);
        println!();
    }
}
