use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};

#[derive(Debug, Clone)]
pub enum TyDefinition {
    Struct(Struct),
    Enum(Enum),
}

impl TyDefinition {
    pub fn name(&self) -> TyName {
        match self {
            TyDefinition::Struct(e) => e.name(),
            TyDefinition::Enum(e) => e.name(),
        }
    }

    pub fn ty_constructor(&self) -> TyConstuctor {
        match self {
            TyDefinition::Struct(e) => e.ty_constructor(),
            TyDefinition::Enum(e) => e.ty_constructor(),
        }
    }
}

impl From<Struct> for TyDefinition {
    fn from(value: Struct) -> Self {
        TyDefinition::Struct(value)
    }
}

impl From<Enum> for TyDefinition {
    fn from(value: Enum) -> Self {
        TyDefinition::Enum(value)
    }
}

impl Display for TyDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyDefinition::Struct(e) => write!(f, "{}", e),
            TyDefinition::Enum(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: TyName,
    pub contents: Container,
}

impl Struct {
    pub fn name(&self) -> TyName {
        self.name.clone()
    }

    pub fn ty_constructor(&self) -> TyConstuctor {
        let mut params = BTreeSet::new();
        match &self.contents {
            Container::Tuple(fields) => {
                for ty_const in fields {
                    params.extend(ty_const.lifetime_parma.clone())
                }
            }
            Container::Named(fields) => {
                for ty_const in fields.values() {
                    params.extend(ty_const.lifetime_parma.clone())
                }
            }
        }

        let mut params = params.into_iter().collect::<Vec<_>>();
        params.sort();

        TyConstuctor {
            name: self.name(),
            lifetime_parma: params,
        }
    }
}

impl Display for Struct {
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
pub struct Enum {
    pub name: TyName,
    pub variants: HashMap<TyName, Container>, // TODO: TyName shoud be VariantName
}

impl Enum {
    pub fn name(&self) -> TyName {
        self.name.clone()
    }

    pub fn ty_constructor(&self) -> TyConstuctor {
        let mut params = BTreeSet::new();
        for variant in self.variants.values() {
            match variant {
                Container::Tuple(fields) => {
                    for ty_const in fields {
                        params.extend(ty_const.lifetime_parma.clone())
                    }
                }
                Container::Named(fields) => {
                    for ty_const in fields.values() {
                        params.extend(ty_const.lifetime_parma.clone())
                    }
                }
            }
        }

        let mut params = params.into_iter().collect::<Vec<_>>();
        params.sort();

        TyConstuctor {
            name: self.name(),
            lifetime_parma: params,
        }
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "pub enum {} {{", self.ty_constructor())?;
        for (i, (ty_name, container)) in self.variants.iter().enumerate() {
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
pub enum Container {
    Tuple(Vec<TyConstuctor>),
    Named(HashMap<FieldName, TyConstuctor>),
}

impl Display for Container {
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
    pub lifetime_parma: Vec<String>,
}

impl Display for TyConstuctor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.lifetime_parma.is_empty() {
            write!(f, "<")?;
            for (i, lifetime) in self.lifetime_parma.iter().enumerate() {
                write!(
                    f,
                    "'{lifetime}{}",
                    if i == self.lifetime_parma.len() - 1 {
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FieldName(String);

impl Display for FieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
