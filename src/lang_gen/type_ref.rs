use std::{borrow::Cow, fmt};

use super::IntoCompleted;

#[derive(Debug, Clone)]
pub struct TyConstuctor {
    ty_name: TyName,
    pub lifetime_param: Cow<'static, [char]>,
    parts: Cow<'static, [&'static str]>,
    ty_pos: u8,
    lifetime_pos: u8,
    inline_lifetime: bool,
}

impl TyConstuctor {
    pub fn new_simple(ty_name: TyName, lifetime_param: Cow<'static, [char]>) -> Self {
        Self {
            lifetime_param,
            parts: Cow::Borrowed(&[]),
            ty_name,
            ty_pos: 0,
            lifetime_pos: 0,
            inline_lifetime: false,
        }
    }

    pub fn fmt_lifetime(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.lifetime_param.is_empty() {
            if !self.inline_lifetime {
                write!(f, "<")?;
            }
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
            if !self.inline_lifetime {
                write!(f, ">")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for TyConstuctor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0.. {
            if i == self.ty_pos {
                write!(f, "{}", self.ty_name)?;
            }
            if i == self.lifetime_pos {
                self.fmt_lifetime(f)?;
            }
            if let Some(part) = self.parts.get(i as usize) {
                write!(f, "{}", part)?;
            } else {
                break;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TyConstuctorIncomplete {
    ty_name: TyName,
    pub lifetime_param: Option<Cow<'static, [char]>>,
    parts: Cow<'static, [&'static str]>,
    ty_pos: u8,
    lifetime_pos: u8,
    inline_lifetime: bool,
}

impl TyConstuctorIncomplete {
    pub fn new_simple(ty_name: TyName) -> Self {
        Self {
            parts: Cow::Borrowed(&[]),
            lifetime_param: None,
            ty_name,
            ty_pos: 0,
            lifetime_pos: 0,
            inline_lifetime: false,
        }
    }

    pub fn new_wrapped(
        ty_name: TyName,
        parts: (&'static str, &'static str),
        lifetime_param: Option<Cow<'static, [char]>>,
    ) -> Self {
        Self {
            parts: Cow::Owned(vec![parts.0, parts.1]),
            lifetime_param,
            ty_name,
            ty_pos: 1,
            lifetime_pos: 1,
            inline_lifetime: false,
        }
    }

    pub fn primary_type_name(&self) -> &TyName {
        &self.ty_name
    }

    pub fn strip_decorators(self) -> Self {
        let mut f = Self::new_simple(self.ty_name);
        f.lifetime_param = self.lifetime_param;
        f
    }
}

impl IntoCompleted for TyConstuctorIncomplete {
    type Result = TyConstuctor;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        if let Some(lifetime_param) = self.lifetime_param.as_ref() {
            Ok(TyConstuctor {
                ty_name: self.ty_name.clone(),
                lifetime_param: lifetime_param.clone(),
                parts: self.parts.clone(),
                ty_pos: self.ty_pos,
                lifetime_pos: self.lifetime_pos,
                inline_lifetime: self.inline_lifetime,
            })
        } else {
            Err(self)
        }
    }
}

impl From<TyConstuctor> for TyConstuctorIncomplete {
    fn from(value: TyConstuctor) -> Self {
        Self {
            lifetime_param: Some(value.lifetime_param),
            parts: value.parts,
            ty_pos: value.ty_pos,
            lifetime_pos: value.lifetime_pos,
            ty_name: value.ty_name,
            inline_lifetime: value.inline_lifetime,
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

impl AsRef<str> for TyName {
    fn as_ref(&self) -> &str {
        self.0.as_str()
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
