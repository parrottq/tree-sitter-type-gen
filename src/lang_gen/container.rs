use std::{collections::BTreeMap, fmt};

use super::{FieldName, IntoCompleted, TyConstuctor, TyConstuctorIncomplete};

#[derive(Debug, Clone)]
pub enum Container<T> {
    Tuple(Vec<T>),
    Named(BTreeMap<FieldName, T>),
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
}

impl IntoCompleted for Container<TyConstuctorIncomplete> {
    type Result = Container<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        match self {
            Container::Tuple(e) => {
                let mut l = Vec::with_capacity(e.len());
                for incomplete_ty in e {
                    l.push(incomplete_ty.into_completed()?);
                }
                Ok(Container::Tuple(l))
            }
            Container::Named(e) => {
                let mut l = BTreeMap::new();
                for (ty_name, incomplete_ty) in e {
                    l.insert(ty_name.clone(), incomplete_ty.into_completed()?);
                }
                Ok(Container::Named(l))
            }
        }
    }
}

impl<T: fmt::Display> fmt::Display for Container<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
