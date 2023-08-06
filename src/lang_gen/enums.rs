use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use super::{Container, IntoCompleted, TyConstuctor, TyConstuctorIncomplete, TyName};

#[derive(Debug, Clone)]
pub struct Enum<T> {
    pub name: TyName,
    pub variants: BTreeMap<TyName, Container<T>>, // TODO: TyName shoud be VariantName
}

impl<T> Enum<T> {
    pub fn name(&self) -> TyName {
        self.name.clone()
    }
}

impl Enum<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        let mut params = BTreeSet::new(); // Dedup and sorted
        for variant in self.variants.values() {
            match variant {
                Container::Tuple(fields) => {
                    for ty_const in fields {
                        params.extend(ty_const.lifetime_param.as_ref())
                    }
                }
                Container::Named(fields) => {
                    for ty_const in fields.values() {
                        params.extend(ty_const.lifetime_param.as_ref())
                    }
                }
            }
        }

        let params = params.into_iter().collect::<Vec<char>>();

        TyConstuctor::new_simple(self.name(), params.into())
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
}

impl IntoCompleted for Enum<TyConstuctorIncomplete> {
    type Result = Enum<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        let mut variants = BTreeMap::new();
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

impl fmt::Display for Enum<TyConstuctor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                    ","
                }
            )?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
