use std::{
    collections::{hash_map::Entry, BTreeMap, HashMap},
    fmt::Debug,
};

use convert_case::{Case, Casing};

mod lang_gen;
mod node;

use lang_gen::{
    Container, ContainerDef, Enum, Impl, ImplInstruction, IntoCompleted, Struct, TyConstuctor,
    TyConstuctorIncomplete, TyName, TypeDef,
};
use node::Field;

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

enum BuildTypeResult {
    Error(String),
}

// TODO: Roll this into TypeDef?
type TyDefBare = (
    ContainerDef<TyConstuctorIncomplete>,
    Vec<Impl<TyConstuctorIncomplete>>,
    Vec<&'static str>,
);

fn build_types_with_defer<T>(
    definitions: &mut HashMap<TyName, TyDefBare>,
    mut inputs: Vec<T>,
    mut fun: impl FnMut(&mut HashMap<TyName, TyDefBare>, &T) -> Result<TyDefBare, BuildTypeResult>,
) where
    T: Debug,
{
    const DEBUG: bool = false;

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

fn build_variant_type(
    ty_rename_table: &mut RenameTable<impl FnMut(&str) -> String>,
    name: TyName,
    subtypes: Vec<(String, bool)>,
) -> TyDefBare {
    let mut variants = BTreeMap::new();
    for (ty, named) in subtypes {
        let sub_ty_name = ty_rename_table.rename(&ty);
        let sub_ty_name = TyName::new(sub_ty_name);

        if named {
            let variant_ty_const = TyConstuctorIncomplete::new_simple(sub_ty_name.clone());
            let res = variants.insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
            assert!(res.is_none());
        } else {
            let res = variants.insert(sub_ty_name, Container::Tuple(vec![]));
            assert!(res.is_none());
        }
    }

    (
        Enum {
            name: name.clone(),
            variants,
        }
        .into(),
        vec![],
        vec![], // TODO: Add attr
    )
}

fn build_field_type(
    declarations: &mut HashMap<TyName, TyDefBare>,
    ty_rename_table: &mut RenameTable<impl FnMut(&str) -> String>,
    field: &Field,
    variant_type_name_fun: impl FnOnce() -> TyName,
) -> TyConstuctorIncomplete {
    let (ty_name, lifetime) = match field.types.as_slice() {
        [] => (TyName::new("()".to_string()), Some(vec![])),
        [single_type] => (TyName::new(ty_rename_table.rename(&single_type.ty)), None),
        types => {
            let sub_ty_name = variant_type_name_fun();
            let v = build_variant_type(
                ty_rename_table,
                sub_ty_name.clone(),
                types.iter().map(|x| (x.ty.clone(), x.named)).collect(),
            );
            let res = declarations.insert(sub_ty_name.clone(), v);
            assert!(res.is_none());
            (sub_ty_name, None)
        }
    };

    if field.multiple {
        TyConstuctorIncomplete::new(("Vec<".into(), ty_name, ">".into()), lifetime)
    } else {
        if field.required {
            TyConstuctorIncomplete::new(("".into(), ty_name, "".into()), lifetime)
        } else {
            TyConstuctorIncomplete::new(("Option<".into(), ty_name, ">".into()), lifetime)
        }
    }
}

fn main() {
    let lang = tree_sitter_rust::language();

    let nodes = serde_json::from_str::<Vec<node::Node>>(tree_sitter_rust::NODE_TYPES).unwrap();
    let nodes: Vec<_> = nodes.into_iter().filter(|x| x.named).collect();

    let prelude = r#"
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};
use tree_sitter::{Node, TreeCursor};

pub trait GenericNode<'a> {
    const NODE_ID: u16;
    const NODE_KIND: &'static str;

    type Fields;
    type Child;

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

pub trait DeserializeNode<'a> {
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Self;
    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Self;
}

fn default_deserialize_at_root<'a, T>(tree: &mut TreeCursor<'a>) -> T
where
    T: DeserializeNode<'a> + GenericNode<'a>,
{
    if tree.goto_first_child() {
        let res = T::deserialize_at_current(tree);
        let has_sibling = tree.goto_next_sibling();
        debug_assert!(!has_sibling);
        let has_parent = tree.goto_parent();
        debug_assert!(has_parent);
        res
    } else {
        panic!("Tried to deserialize '{}' but no child of type '{}' is present", tree.node().kind(), T::NODE_KIND);
    }
}

fn default_deserialize_at_current<'a, T>(tree: &mut TreeCursor<'a>) -> T
where
    T: DeserializeNode<'a> + GenericNode<'a>,
{
    T::downcast(tree.node()).unwrap()
}

impl<'a, T> DeserializeNode<'a> for Option<T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Self {
        if tree.goto_first_child() {
            let res = Self::deserialize_at_current(tree);
            let has_sibling = tree.goto_next_sibling();
            debug_assert!(!has_sibling);
            let has_parent = tree.goto_parent();
            debug_assert!(has_parent);
            res
        } else {
            None
        }
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Self {
        Some(T::deserialize_at_current(tree))
    }
}

impl<'a> DeserializeNode<'a> for () {
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Self {
        let has_child = tree.goto_first_child();
        dbg!(!has_child);
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Self {
        // This makes it possible to count children without copying nodes by deserializing into 'Vec<()>'
        ()
    }
}

impl<'a, T> DeserializeNode<'a> for Vec<T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Self {
        if tree.goto_first_child() {
            let res = Self::deserialize_at_current(tree);
            let has_parent = tree.goto_parent();
            debug_assert!(has_parent);
            res
        } else {
            vec![]
        }
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Self {
        let mut nodes = Vec::new();
        loop {
            nodes.push(T::deserialize_at_current(tree));

            if !tree.goto_next_sibling() {
                break nodes;
            }
        }
    }
}
    "#
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

    let mut declarations: HashMap<TyName, TyDefBare> = HashMap::with_capacity(nodes.len());

    const DEBUG: bool = false;

    build_types_with_defer(&mut declarations, nodes, |declarations, node| {
        let ty_name = ty_rename_table.rename(&node.ty);
        if DEBUG {
            println!("// Processing '{ty_name}' '{}'", node.ty);
        }

        let child_ty = node
            .children
            .as_ref()
            .map(|children| {
                build_field_type(declarations, &mut ty_rename_table, children, || {
                    TyName::new(format!("{}Child", ty_name))
                })
            })
            .unwrap_or_else(|| TyConstuctor::new_simple(TyName::new("()".to_owned()), vec![]).into());

        if node.subtypes.len() > 0 {
            assert_eq!(node.fields.len(), 0);
            let mut variants = BTreeMap::new();

            for subtype in &node.subtypes {
                let sub_ty_name = TyName::new(ty_rename_table.rename(&subtype.ty));
                if subtype.named {
                    let variant_ty_const = TyConstuctorIncomplete::new_simple(sub_ty_name.clone());
                    let res =
                        variants.insert(sub_ty_name, Container::Tuple(vec![variant_ty_const]));
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
                vec![], // TODO: Add attr
            ));
        }

        let fields_ty: TyConstuctorIncomplete = if node.fields.len() > 0 {
            let fields: Vec<_> = node
                .fields
                .iter()
                .map(|(field_name, field)| {
                    build_field_type(declarations, &mut ty_rename_table, field, || {
                        TyName::new(format!("{}_{}", ty_name, field_name))
                    })
                })
                .collect();

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

            let contents = Container::Tuple(fields);

            let res = declarations.insert(
                TyName::new(field_ty_name.clone()),
                (
                    Struct {
                        name: TyName::new(field_ty_name.clone()),
                        contents,
                    }
                    .into(),
                    vec![],
                    vec![], // TODO: Add attr
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
            TyConstuctor::new_simple(TyName::new("GenericNode".into()), vec!["a".into()]).into();

        let node_id = lang.id_for_node_kind(&node.ty, true);
        let generic_node_parts = Impl::new([
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
            "; type Child = ".into(),
            ImplInstruction::TyConstructor(child_ty),
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

        let deserialize_node_ty =
            TyConstuctor::new_simple(TyName::new("DeserializeNode".into()), vec!["a".into()])
                .into();

        let deserialize_node_parts = Impl::new([
            "impl".into(),
            ImplInstruction::DeclareLifetimes,
            " ".into(),
            ImplInstruction::TyConstructor(deserialize_node_ty),
            " for ".into(),
            ImplInstruction::SelfType,
            " { fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Self { default_deserialize_at_root(tree) } fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Self { default_deserialize_at_current(tree) } }".into()
        ].to_vec());

        let attr = vec!["#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]"];

        Ok((
            Struct {
                name: TyName::new(ty_name),
                contents: Container::Tuple(vec![node_ty.clone().into()]),
            }
            .into(),
            vec![generic_node_parts, deserialize_node_parts],
            attr,
        ))
    });

    let mut declarations_incomplete: BTreeMap<TyName, TyDefBare> =
        declarations.into_iter().collect();
    let mut declarations_partial_completed: BTreeMap<
        TyName,
        (
            ContainerDef<TyConstuctor>,
            Vec<Impl<TyConstuctorIncomplete>>,
            Vec<&'static str>,
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

        if DEBUG {
            println!(
                "// name({}) completed({}) incomplete({})",
                ty_name,
                declarations_partial_completed.len(),
                declarations_incomplete.len()
            );
        }

        let ty_def = declarations_incomplete.get_mut(&ty_name).unwrap();
        match ty_def.0.into_completed() {
            Ok(completed) => {
                if DEBUG {
                    println!("// Ok");
                }

                let (_, impls, attr) = declarations_incomplete.remove(&ty_name).unwrap();
                declarations_partial_completed.insert(ty_name.clone(), (completed, impls, attr));
                checking_stack.pop();
            }
            Err(incomplete_ty_def) => {
                if DEBUG {
                    println!(
                        "// Err {} {}",
                        incomplete_ty_def.primary_type_name(),
                        ty_name
                    );
                }

                let next_ty_name = incomplete_ty_def.primary_type_name().clone();

                if let Some((container, _, _)) = declarations_partial_completed.get(&next_ty_name) {
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
        let (ty_name, (ty_container, ty_impls, attrs)) = entry.remove_entry();

        let mut ty_def = TypeDef::new(ty_container);
        attrs.into_iter().for_each(|attr| ty_def.push_attr(attr));

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
                        if let Some((ty_container, _ty_impls, _attr)) =
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
