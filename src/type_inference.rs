use std::collections::BTreeMap;

use super::lang_gen::{
    ContainerDef, Impl, IntoCompleted, TyConstuctor, TyConstuctorIncomplete, TyName, TypeDef,
};
use super::{TyDefBare, DEBUG};

pub fn complete_type_def_generics(
    mut declarations_incomplete: BTreeMap<TyName, TyDefBare>,
) -> BTreeMap<
    TyName,
    (
        ContainerDef<TyConstuctor>,
        Vec<Impl<TyConstuctorIncomplete>>,
        Vec<&'static str>,
    ),
> {
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

    declarations_partial_completed
}

pub fn complete_impl_generics(
    mut declarations_partial_completed: BTreeMap<
        TyName,
        (
            ContainerDef<TyConstuctor>,
            Vec<Impl<TyConstuctorIncomplete>>,
            Vec<&'static str>,
        ),
    >,
) -> BTreeMap<TyName, TypeDef<TyConstuctor>> {
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

    declarations_completed
}
