use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};

#[derive(Debug, Clone)]
pub enum TyDefinition<T> {
    Struct(Struct<T>),
    Enum(Enum<T>),
}

impl<T> TyDefinition<T> {
    pub fn name(&self) -> TyName {
        match self {
            TyDefinition::Struct(e) => e.name(),
            TyDefinition::Enum(e) => e.name(),
        }
    }
}

impl TyDefinition<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        match self {
            TyDefinition::Struct(e) => e.ty_constructor(),
            TyDefinition::Enum(e) => e.ty_constructor(),
        }
    }
}

impl TyDefinition<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        match self {
            TyDefinition::Struct(e) => e.next_incomplete(),
            TyDefinition::Enum(e) => e.next_incomplete(),
        }
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        match self {
            TyDefinition::Struct(e) => e.next_incomplete_mut(),
            TyDefinition::Enum(e) => e.next_incomplete_mut(),
        }
    }

    pub fn into_completed(
        &mut self,
    ) -> Result<TyDefinition<TyConstuctor>, &mut TyConstuctorIncomplete> {
        match self {
            TyDefinition::Struct(e) => e.into_completed().map(Into::into),
            TyDefinition::Enum(e) => e.into_completed().map(Into::into),
        }
    }
}

impl<T> From<Struct<T>> for TyDefinition<T> {
    fn from(value: Struct<T>) -> Self {
        TyDefinition::Struct(value)
    }
}

impl<T> From<Enum<T>> for TyDefinition<T> {
    fn from(value: Enum<T>) -> Self {
        TyDefinition::Enum(value)
    }
}

impl Display for TyDefinition<TyConstuctor> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyDefinition::Struct(e) => write!(f, "{}", e),
            TyDefinition::Enum(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct<T> {
    pub name: TyName,
    pub contents: Container<T>,
}

impl<T> Struct<T> {
    pub fn name(&self) -> TyName {
        self.name.clone()
    }
}

impl Struct<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        let mut params = BTreeSet::new();
        match &self.contents {
            Container::Tuple(fields) => {
                for ty_const in fields {
                    params.extend(ty_const.lifetime_param.clone())
                }
            }
            Container::Named(fields) => {
                for ty_const in fields.values() {
                    params.extend(ty_const.lifetime_param.clone())
                }
            }
        }

        let mut params = params.into_iter().collect::<Vec<_>>();
        params.sort();

        TyConstuctor {
            name: self.name(),
            lifetime_param: params,
        }
    }
}

impl Struct<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        self.contents.next_incomplete()
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        self.contents.next_incomplete_mut()
    }

    pub fn into_completed(&mut self) -> Result<Struct<TyConstuctor>, &mut TyConstuctorIncomplete> {
        self.contents.into_completed().map(|x| Struct {
            name: self.name.clone(),
            contents: x,
        })
    }
}

impl Display for Struct<TyConstuctor> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "pub struct {}", self.ty_constructor())?;
        let tail = match self.contents {
            Container::Tuple(_) => ";",
            Container::Named(_) => "",
        };
        write!(f, "{}{}", self.contents, tail)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Enum<T> {
    pub name: TyName,
    pub variants: HashMap<TyName, Container<T>>, // TODO: TyName shoud be VariantName
}

impl<T> Enum<T> {
    pub fn name(&self) -> TyName {
        self.name.clone()
    }
}

impl Enum<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        let mut params = BTreeSet::new();
        for variant in self.variants.values() {
            match variant {
                Container::Tuple(fields) => {
                    for ty_const in fields {
                        params.extend(ty_const.lifetime_param.clone())
                    }
                }
                Container::Named(fields) => {
                    for ty_const in fields.values() {
                        params.extend(ty_const.lifetime_param.clone())
                    }
                }
            }
        }

        let mut params = params.into_iter().collect::<Vec<_>>();
        params.sort();

        TyConstuctor {
            name: self.name(),
            lifetime_param: params,
        }
    }
}

impl Enum<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        self.variants.values().find_map(|x| x.next_incomplete())
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        self.variants
            .values_mut()
            .find_map(|x| x.next_incomplete_mut())
    }

    pub fn into_completed(&mut self) -> Result<Enum<TyConstuctor>, &mut TyConstuctorIncomplete> {
        let mut variants = HashMap::with_capacity(self.variants.len());
        for (ty_name, container) in &mut self.variants {
            let e = container.into_completed()?;
            variants.insert(ty_name.clone(), e);
        }
        Ok(Enum {
            name: self.name.clone(),
            variants,
        })
    }
}

impl Display for Enum<TyConstuctor> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "pub enum {} {{", self.ty_constructor())?;
        let mut variants = self.variants.iter().collect::<Vec<_>>();
        variants.sort_by_key(|x| x.0);
        for (i, (ty_name, container)) in variants.into_iter().enumerate() {
            writeln!(
                f,
                "    {}{}{}",
                ty_name,
                container,
                if i == self.variants.len() - 1 {
                    ""
                } else {
                    ", "
                }
            )?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Container<T> {
    Tuple(Vec<T>),
    Named(HashMap<FieldName, T>),
}

impl Container<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        match self {
            Container::Tuple(t) => t.iter().find(|x| x.lifetime_param.is_none()),
            Container::Named(n) => n.values().find(|x| x.lifetime_param.is_none()),
        }
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        match self {
            Container::Tuple(t) => t.iter_mut().find(|x| x.lifetime_param.is_none()),
            Container::Named(n) => n.values_mut().find(|x| x.lifetime_param.is_none()),
        }
    }

    pub fn into_completed(
        &mut self,
    ) -> Result<Container<TyConstuctor>, &mut TyConstuctorIncomplete> {
        match self {
            Container::Tuple(e) => {
                let mut l = Vec::with_capacity(e.len());
                for incomplete_ty in e {
                    l.push(incomplete_ty.into_complete().ok_or(incomplete_ty)?);
                }
                Ok(Container::Tuple(l))
            }
            Container::Named(e) => {
                let mut l = HashMap::with_capacity(e.len());
                for (ty_name, incomplete_ty) in e {
                    l.insert(
                        ty_name.clone(),
                        incomplete_ty.into_complete().ok_or(incomplete_ty)?,
                    );
                }
                Ok(Container::Named(l))
            }
        }
    }
}

impl<T: Display> Display for Container<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Container::Tuple(container) => {
                if !container.is_empty() {
                    write!(f, "(")?;
                    for (i, ty_name) in container.iter().enumerate() {
                        write!(
                            f,
                            "{}{}",
                            ty_name,
                            if i == container.len() - 1 { "" } else { ", " }
                        )?;
                    }
                    write!(f, ")")?;
                }
            }
            Container::Named(container) => {
                if !container.is_empty() {
                    write!(f, "{{")?;
                    for (i, (field_name, ty_name)) in container.iter().enumerate() {
                        write!(
                            f,
                            "{}: {}{}",
                            field_name,
                            ty_name,
                            if i == container.len() - 1 { "" } else { ", " }
                        )?;
                    }
                    writeln!(f, "}}")?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TyConstuctor {
    pub name: TyName,
    pub lifetime_param: Vec<String>,
}

impl TyConstuctor {
    pub fn new_simple(name: TyName, lifetime_param: Vec<String>) -> Self {
        Self {
            name,
            lifetime_param,
        }
    }
}

impl Display for TyConstuctor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.lifetime_param.is_empty() {
            write!(f, "<")?;
            for (i, lifetime) in self.lifetime_param.iter().enumerate() {
                write!(
                    f,
                    "'{lifetime}{}",
                    if i == self.lifetime_param.len() - 1 {
                        ""
                    } else {
                        ", "
                    }
                )?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TyConstuctorIncomplete {
    name: TyName,
    pub lifetime_param: Option<Vec<String>>,
}

impl TyConstuctorIncomplete {
    pub fn new_simple(name: TyName) -> Self {
        Self {
            name,
            lifetime_param: None,
        }
    }

    pub fn into_complete(&self) -> Option<TyConstuctor> {
        let lifetime_param = self.lifetime_param.as_ref()?.clone();
        Some(TyConstuctor::new_simple(self.name.clone(), lifetime_param))
    }

    pub fn name(&self) -> &TyName {
        &self.name
    }
}

impl From<TyConstuctor> for TyConstuctorIncomplete {
    fn from(value: TyConstuctor) -> Self {
        Self {
            name: value.name,
            lifetime_param: Some(value.lifetime_param),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyName(String);

impl TyName {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl Display for TyName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldName(String);

impl Display for FieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
