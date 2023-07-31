use std::{borrow::Cow, fmt};

#[derive(Debug, Clone)]
pub struct TyConstuctor {
    parts: (Cow<'static, str>, TyName, Cow<'static, str>),
    pub lifetime_param: Vec<String>,
}

impl TyConstuctor {
    pub fn new_simple(name: TyName, lifetime_param: Vec<String>) -> Self {
        Self {
            parts: ("".into(), name, "".into()),
            lifetime_param,
        }
    }

    pub fn new(
        parts: (Cow<'static, str>, TyName, Cow<'static, str>),
        lifetime_param: Vec<String>,
    ) -> Self {
        Self {
            parts,
            lifetime_param,
        }
    }

    pub fn fmt_lifetime(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl fmt::Display for TyConstuctor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.parts.0)?;
        write!(f, "{}", self.parts.1)?;
        self.fmt_lifetime(f)?;
        write!(f, "{}", self.parts.2)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TyConstuctorIncomplete {
    parts: (Cow<'static, str>, TyName, Cow<'static, str>),
    pub lifetime_param: Option<Vec<String>>,
}

impl TyConstuctorIncomplete {
    pub fn new_simple(name: TyName) -> Self {
        Self {
            parts: ("".into(), name, "".into()),
            lifetime_param: None,
        }
    }

    pub fn new(
        parts: (Cow<'static, str>, TyName, Cow<'static, str>),
        lifetime_param: Option<Vec<String>>,
    ) -> Self {
        Self {
            parts,
            lifetime_param,
        }
    }

    pub fn into_completed(&self) -> Option<TyConstuctor> {
        let lifetime_param = self.lifetime_param.as_ref()?.clone();
        Some(TyConstuctor::new(self.parts.clone(), lifetime_param))
    }

    pub fn primary_type_name(&self) -> &TyName {
        &self.parts.1
    }
}

impl From<TyConstuctor> for TyConstuctorIncomplete {
    fn from(value: TyConstuctor) -> Self {
        Self {
            parts: value.parts,
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

impl fmt::Display for TyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldName(String);

impl fmt::Display for FieldName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
