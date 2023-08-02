use std::{borrow::Cow, collections::HashSet, fmt};

use super::TyConstuctor;

#[derive(Debug, Clone)]
pub enum ImplInstruction {
    Literal(Cow<'static, str>),
    SelfType,
    TyConstructor(TyConstuctor),
    DeclareLifetimes,
}

impl From<&'static str> for ImplInstruction {
    fn from(value: &'static str) -> Self {
        Self::Literal(value.into())
    }
}

impl From<String> for ImplInstruction {
    fn from(value: String) -> Self {
        Self::Literal(value.into())
    }
}

impl From<Cow<'static, str>> for ImplInstruction {
    fn from(value: Cow<'static, str>) -> Self {
        Self::Literal(value)
    }
}

#[derive(Debug, Clone)]
pub struct Impl {
    parts: Vec<ImplInstruction>,
}

impl Impl {
    pub fn new(parts: Vec<ImplInstruction>) -> Self {
        Self { parts }
    }

    pub fn displayer<'a>(&'a self, self_ty: &'a TyConstuctor) -> ImplDisplay<'a> {
        ImplDisplay { imp: self, self_ty }
    }
}

#[derive(Debug, Clone)]
pub struct ImplDisplay<'a> {
    imp: &'a Impl,
    self_ty: &'a TyConstuctor,
}

impl<'a> fmt::Display for ImplDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut active_lifetime_cache = None;
        for inst in self.imp.parts.iter() {
            match inst {
                ImplInstruction::Literal(lit) => write!(f, "{}", lit)?,
                ImplInstruction::SelfType => write!(f, "{}", self.self_ty)?,
                ImplInstruction::TyConstructor(ty) => write!(f, "{}", ty)?,
                ImplInstruction::DeclareLifetimes => {
                    let active_lifetimes = active_lifetime_cache.get_or_insert_with(|| {
                        let mut lifetimes = HashSet::new();
                        lifetimes.extend(self.self_ty.lifetime_param.iter());

                        for part in self.imp.parts.iter() {
                            match part {
                                ImplInstruction::TyConstructor(ty) => {
                                    lifetimes.extend(ty.lifetime_param.iter())
                                }
                                _ => {}
                            }
                        }

                        let mut lifetimes = lifetimes.into_iter().collect::<Vec<_>>();
                        lifetimes.sort();
                        lifetimes
                    });

                    if !active_lifetimes.is_empty() {
                        write!(f, "<")?;
                        for (i, lifetime) in active_lifetimes.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "'{}", lifetime)?;
                        }
                        write!(f, ">")?;
                    }
                }
            }
        }
        Ok(())
    }
}
