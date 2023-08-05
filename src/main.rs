use std::{
    borrow::Cow,
    collections::{hash_map::Entry, BTreeMap, HashMap},
};

use convert_case::{Case, Casing};

mod lang_gen;
mod node;

use lang_gen::{
    Container, ContainerDef, Enum, Impl, ImplInstruction, IntoCompleted, Struct, TyConstuctor,
    TyConstuctorIncomplete, TyName, TypeDef,
};
use node::{Field, TypeIdent};

use crate::node::Node;

#[derive(Default, Clone)]
struct RenameTable<T>(HashMap<TypeIdent, Cow<'static, str>>, T)
where
    T: FnMut(&TypeIdent) -> Cow<'static, str>;

impl<T> RenameTable<T>
where
    T: FnMut(&TypeIdent) -> Cow<'static, str>,
{
    fn new(convert: T) -> Self {
        Self(Default::default(), convert)
    }

    fn rename<'a>(&'a mut self, ty_name: &TypeIdent) -> Cow<'static, str> {
        if let Some(e) = self.0.get(ty_name) {
            return e.clone();
        }

        match self.0.entry(ty_name.clone()) {
            Entry::Occupied(_) => unreachable!(),
            Entry::Vacant(entry) => {
                let ty_new = (self.1)(entry.key());
                entry.insert(ty_new).clone()
            }
        }
    }

    fn insert_rename(&mut self, ty_src: TypeIdent, ty_dst: Cow<'static, str>) {
        if let Some(_) = self.0.insert(ty_src, ty_dst) {
            panic!()
        }
    }
}

// TODO: Roll this into TypeDef?
type TyDefBare = (
    ContainerDef<TyConstuctorIncomplete>,
    Vec<Impl<TyConstuctorIncomplete>>,
    Vec<&'static str>,
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

    let deserialize_node_parts = Impl::new({
        let deserialize_node_ty =
            TyConstuctor::new_simple(TyName::new("DeserializeNode".into()), vec!["a".into()])
                .into();

        let front_parts = [
                    "impl".into(),
                    ImplInstruction::DeclareLifetimes,
                    " ".into(),
                    ImplInstruction::TyConstructor(deserialize_node_ty),
                    " for ".into(),
                    ImplInstruction::SelfType,
                    " { fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> { default_deserialize_at_root(tree) } fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> { let variant_funs: [fn(&mut TreeCursor<'a>) -> Result<Self, DeserializeError>; ".into(),
                    format!("{}", subtypes.len()).into(),
                    "] = [ ".into()
                ];

        let variant_function_parts = subtypes
            .iter()
            .map(|variant_name| {
                let variant_name = ty_rename_table.rename(variant_name).to_string();

                [
                    "|tree| Ok(".into(),
                    ty_name.as_ref().to_owned().into(),
                    "::".into(),
                    variant_name.clone().into(),
                    "(".into(),
                    variant_name.into(),
                    "::deserialize_at_current(tree)?)), ".into(),
                ]
            })
            .flatten();

        let back_part = ["]; variants_deserialize_at_current(tree, variant_funs) } }".into()];

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
        vec![deserialize_node_parts],
        vec![], // TODO: Add attr
    )
}

fn build_field_type<'a>(
    declarations: &mut HashMap<TyName, TyDefBare>,
    ty_rename_table: &mut RenameTable<impl FnMut(&TypeIdent) -> Cow<'static, str>>,
    field: &'a Field,
    variant_type_name_fun: impl FnOnce() -> TyName,
) -> TyConstuctorIncomplete {
    let (ty_name, lifetime) = match field.types.as_slice() {
        [] => (TyName::new("()".to_string()), Some(vec![])),
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

    let nodes = serde_json::from_str::<Vec<Node>>(tree_sitter_rust::NODE_TYPES).unwrap();

    // TODO: Move into own file
    let prelude = r#"
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};
use tree_sitter::{Node, TreeCursor};

pub trait GenericNode<'a>
where
    Self::Child: DeserializeNode<'a>,
{
    const NODE_ID: u16;
    const NODE_KIND: &'static str;
    const NAMED: bool;

    type Fields;
    type Child;

    fn inner_node(&self) -> &Node<'a>;
    fn inner_node_mut(&mut self) -> &mut Node<'a>;
    fn downcast(value: Node<'a>) -> Result<Self, Node<'a>>
    where
        Self: Sized;

    fn children(&self) -> Result<Self::Child, DeserializeError> {
        Self::Child::deserialize_at_root(&mut self.inner_node().walk())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeTy<'a, T>(pub T, pub PhantomData<Node<'a>>)
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeserializeError {
    NoChild,
    WrongType
}

pub trait DeserializeNode<'a>
where
    Self: Sized,
{
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError>;
    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError>;
}

fn default_deserialize_at_root<'a, T>(tree: &mut TreeCursor<'a>) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a>
{
    if tree.goto_first_child() {
        let res = T::deserialize_at_current(tree);
        let has_sibling = tree.goto_next_sibling();
        debug_assert!(!has_sibling);
        let has_parent = tree.goto_parent();
        debug_assert!(has_parent);
        Ok(res?)
    } else {
        Err(DeserializeError::NoChild)
    }
}

fn default_deserialize_at_current<'a, T>(tree: &mut TreeCursor<'a>) -> Result<T, DeserializeError>
where
    T: DeserializeNode<'a> + GenericNode<'a>,
{
    T::downcast(tree.node()).map_err(|_| DeserializeError::WrongType)
}

fn variants_deserialize_at_current<'a, const N: usize, T>(
    tree: &mut TreeCursor<'a>,
    variant_funs: [fn(&mut TreeCursor<'a>) -> Result<T, DeserializeError>; N]
) -> Result<T, DeserializeError> {
    for variant_fun in variant_funs {
        match variant_fun(tree) {
            Ok(node) => return Ok(node),
            Err(DeserializeError::WrongType) => continue,
            Err(error) => return Err(error),
        }
    }

    Err(DeserializeError::WrongType)
}

impl<'a, T> DeserializeNode<'a> for Option<T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if tree.goto_first_child() {
            let res = Self::deserialize_at_current(tree);
            let has_sibling = tree.goto_next_sibling();
            debug_assert!(!has_sibling);
            let has_parent = tree.goto_parent();
            debug_assert!(has_parent);
            Ok(res?)
        } else {
            Err(DeserializeError::NoChild) // TODO: Reuse this function with default impl?
        }
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        Ok(Some(T::deserialize_at_current(tree)?))
    }
}

impl<'a> DeserializeNode<'a> for () {
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        let has_child = tree.goto_first_child();
        dbg!(!has_child);
        Ok(())
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        // This makes it possible to count children without copying nodes by deserializing into 'Vec<()>'
        Ok(())
    }
}

impl<'a, T> DeserializeNode<'a> for Vec<T>
where
    T: DeserializeNode<'a>,
{
    fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        if tree.goto_first_child() {
            let res = Self::deserialize_at_current(tree);
            let has_parent = tree.goto_parent();
            debug_assert!(has_parent);
            Ok(res?)
        } else {
            Ok(vec![])
        }
    }

    fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> {
        let mut nodes = Vec::new();
        loop {
            nodes.push(T::deserialize_at_current(tree)?);

            if !tree.goto_next_sibling() {
                break Ok(nodes);
            }
        }
    }
}
    "#
    .trim();

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

    // TODO: Check for overlapping names explicitly
    let mut ty_rename_table = RenameTable::new(|x| match x.ty.as_ref() {
        "_" => "Base".into(),
        _ => format!(
            "{}{}",
            x.ty.from_case(Case::Snake).to_case(Case::Pascal),
            if x.named { "" } else { "Lit" }
        )
        .into(),
    });

    ty_rename_table.insert_rename(TypeIdent::new("!", false), "NotLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("!=", false), "NotEqualLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("self", true), "SelfTyLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("%", false), "ModulusLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("&", false), "BitwiseAndLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("&&", false), "AndLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("*", false), "MultiplyLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("+", false), "AdditionLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("-", false), "SubtractLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("/", false), "DivideLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("<", false), "LessThanLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("<<", false), "LeftShiftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("<=", false), "LessThanEqualLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("==", false), "EqualLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(">", false), "GreaterThanLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(">=", false), "GreaterThanEqualLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(">>", false), "RightShiftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("^", false), "XorLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("|", false), "BitwiseOrLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("||", false), "OrLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("%=", false), "AssignModulusLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("&=", false), "AssignBitwiseAndLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("*=", false), "AssignMultiplyLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("+=", false), "AssignAdditionLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("-=", false), "AssignSubtractLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("/=", false), "AssignDivideLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("<<=", false), "AssignLeftShiftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(">>=", false), "AssignRightShiftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("^=", false), "AssignXorLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("|=", false), "AssignOrLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("\"", false), "QuoteDoubleLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("#", false), "PoundLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("$", false), "DollarLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("'", false), "QuoteSingleLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("(", false), "ParenLeftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(")", false), "ParentRightLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(",", false), "CommaLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("->", false), "RArrowLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(".", false), "DotLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("..", false), "DotDotLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("...", false), "DotDotDotLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("..=", false), "DotDotEqLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(":", false), "ColonLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("::", false), "PathSepLit".into());
    ty_rename_table.insert_rename(TypeIdent::new(";", false), "SemiLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("=", false), "EqLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("=>", false), "FatArrowLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("?", false), "QuestionLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("@", false), "AtLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("[", false), "BracketLeftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("]", false), "BracketRightLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("macro_rules!", false), "MacroRuleLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("{", false), "BraceLeftLit".into());
    ty_rename_table.insert_rename(TypeIdent::new("}", false), "BraceRightLit".into());

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

    const DEBUG: bool = false;

    for node in nodes.iter() {
        let ty_name = ty_rename_table.rename(&node.ident).to_string();
        if DEBUG {
            println!("// Processing '{ty_name}' '{}'", node.ident.ty);
        }

        let child_ty = node
            .children
            .as_ref()
            .map(|children| {
                build_field_type(&mut declarations, &mut ty_rename_table, children, || {
                    TyName::new(format!("{}Child", ty_name))
                })
            })
            .unwrap_or_else(|| {
                TyConstuctor::new_simple(TyName::new("()".to_owned()), vec![]).into()
            });

        if node.subtypes.len() > 0 {
            let ty_def = build_variant_type(
                &mut ty_rename_table,
                TyName::new(ty_name.to_string()),
                &node.subtypes,
            );
            let res = declarations.insert(ty_def.0.name(), ty_def);
            assert!(res.is_none());
            continue;
        }

        let fields_ty: TyConstuctorIncomplete = if node.fields.len() > 0 {
            let fields: Vec<_> = node
                .fields
                .iter()
                .map(|(field_name, field)| {
                    build_field_type(&mut declarations, &mut ty_rename_table, field, || {
                        TyName::new(format!("{}_{}", ty_name, field_name))
                    })
                })
                .collect();

            // TODO: Implement field access
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

        let node_id = lang.id_for_node_kind(&node.ident.ty, true);
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
            format!("{}", node.ident.ty.escape_default()).into(),
            "\"; const NAMED: bool = ".into(),
            format!("{}", node.ident.named).into(),
            "; type Fields = ".into(),
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
            " { fn deserialize_at_root(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> { default_deserialize_at_root(tree) } fn deserialize_at_current(tree: &mut TreeCursor<'a>) -> Result<Self, DeserializeError> { default_deserialize_at_current(tree) } }".into()
        ].to_vec());

        let attr = vec!["#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]"];

        let ty_def: TyDefBare = (
            Struct {
                name: TyName::new(ty_name.to_string()),
                contents: Container::Tuple(vec![node_ty.clone().into()]),
            }
            .into(),
            vec![generic_node_parts, deserialize_node_parts],
            attr,
        );
        let res = declarations.insert(ty_def.0.name(), ty_def);
        assert!(res.is_none());
        continue;
    }

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

    println!("{}", prelude);
    for (ty_name, ty_def) in declarations_completed {
        println!("/// {}", ty_name);
        println!("{}", ty_def);
        println!();
    }
}
