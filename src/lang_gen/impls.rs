use std::{borrow::Cow, collections::HashSet, fmt};

use super::{IntoCompleted, TyConstuctor, TyConstuctorIncomplete};

#[derive(Debug, Clone)]
pub enum ImplInstruction<T> {
    Literal(Cow<'static, str>),
    SelfType,
    TyConstructor(T),
    DeclareLifetimes,
}

impl<T> From<&'static str> for ImplInstruction<T> {
    fn from(value: &'static str) -> Self {
        Self::Literal(value.into())
    }
}

impl<T> From<String> for ImplInstruction<T> {
    fn from(value: String) -> Self {
        Self::Literal(value.into())
    }
}

impl<T> From<Cow<'static, str>> for ImplInstruction<T> {
    fn from(value: Cow<'static, str>) -> Self {
        Self::Literal(value)
    }
}

impl IntoCompleted for ImplInstruction<TyConstuctorIncomplete> {
    type Result = ImplInstruction<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        Ok(match self {
            ImplInstruction::Literal(e) => ImplInstruction::Literal(e.clone()),
            ImplInstruction::SelfType => ImplInstruction::SelfType,
            ImplInstruction::TyConstructor(ty) => {
                ImplInstruction::TyConstructor(ty.into_completed()?)
            }
            ImplInstruction::DeclareLifetimes => ImplInstruction::DeclareLifetimes,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Impl<T> {
    parts: Vec<ImplInstruction<T>>,
}

impl<T> Impl<T> {
    pub fn new(parts: Vec<ImplInstruction<T>>) -> Self {
        Self { parts }
    }
}

impl IntoCompleted for Impl<TyConstuctorIncomplete> {
    type Result = Impl<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        let mut new_parts = Vec::with_capacity(self.parts.len());
        for part in self.parts.iter_mut() {
            new_parts.push(part.into_completed()?);
        }

        Ok(Impl { parts: new_parts })
    }
}

impl Impl<TyConstuctor> {
    pub fn displayer<'a>(&'a self, self_ty: &'a TyConstuctor) -> ImplDisplay<'a> {
        ImplDisplay { imp: self, self_ty }
    }
}

#[derive(Debug, Clone)]
pub struct ImplDisplay<'a> {
    imp: &'a Impl<TyConstuctor>,
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
