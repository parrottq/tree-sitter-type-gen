use std::fmt;

use super::{Enum, IntoCompleted, Struct, TyConstuctor, TyConstuctorIncomplete, TyName};

#[derive(Debug, Clone)]
pub enum ContainerDef<T> {
    Struct(Struct<T>),
    Enum(Enum<T>),
}

impl<T> ContainerDef<T> {
    pub fn name(&self) -> TyName {
        match self {
            ContainerDef::Struct(e) => e.name(),
            ContainerDef::Enum(e) => e.name(),
        }
    }
}

impl ContainerDef<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        match self {
            ContainerDef::Struct(e) => e.ty_constructor(),
            ContainerDef::Enum(e) => e.ty_constructor(),
        }
    }
}

impl ContainerDef<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        match self {
            ContainerDef::Struct(e) => e.next_incomplete(),
            ContainerDef::Enum(e) => e.next_incomplete(),
        }
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        match self {
            ContainerDef::Struct(e) => e.next_incomplete_mut(),
            ContainerDef::Enum(e) => e.next_incomplete_mut(),
        }
    }
}

impl IntoCompleted for ContainerDef<TyConstuctorIncomplete> {
    type Result = ContainerDef<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        match self {
            ContainerDef::Struct(e) => e.into_completed().map(Into::into),
            ContainerDef::Enum(e) => e.into_completed().map(Into::into),
        }
    }
}

impl<T> From<Struct<T>> for ContainerDef<T> {
    fn from(value: Struct<T>) -> Self {
        ContainerDef::Struct(value)
    }
}

impl<T> From<Enum<T>> for ContainerDef<T> {
    fn from(value: Enum<T>) -> Self {
        ContainerDef::Enum(value)
    }
}

impl fmt::Display for ContainerDef<TyConstuctor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContainerDef::Struct(e) => write!(f, "{}", e),
            ContainerDef::Enum(e) => write!(f, "{}", e),
        }
    }
}
