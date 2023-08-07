use std::{
    borrow::Cow,
    collections::{hash_map::Entry, BTreeMap, HashMap},
    marker::PhantomData,
};

use super::TypeIdent;

#[derive(Default, Clone)]
pub(crate) struct RenameTable<T>(HashMap<TypeIdent, Cow<'static, str>>, T)
where
    T: FnMut(&TypeIdent) -> Cow<'static, str>;

impl<T> RenameTable<T>
where
    T: FnMut(&TypeIdent) -> Cow<'static, str>,
{
    pub fn new(convert: T) -> Self {
        Self(Default::default(), convert)
    }

    pub fn rename<'a>(&'a mut self, ty_name: &TypeIdent) -> Cow<'static, str> {
        if let Some(e) = self.0.get(ty_name) {
            return e.clone();
        }

        match self.0.entry(ty_name.clone()) {
            Entry::Occupied(_) => unreachable!(),
            Entry::Vacant(entry) => {
                let ty_new = (self.1)(entry.key());
                entry.insert(ty_new).clone()
            }
        }
    }

    pub fn insert_rename(&mut self, ty_src: TypeIdent, ty_dst: Cow<'static, str>) {
        if let Some(_) = self.0.insert(ty_src, ty_dst) {
            panic!()
        }
    }
}

pub struct SubstitutionTable {
    replacements: Vec<(usize, HashMap<Cow<'static, str>, Cow<'static, str>>)>,
}

impl SubstitutionTable {
    pub fn new(
        table: impl IntoIterator<Item = (impl Into<Cow<'static, str>>, impl Into<Cow<'static, str>>)>,
    ) -> Self {
        let mut sizes: BTreeMap<usize, HashMap<Cow<'_, str>, Cow<'_, str>>> = BTreeMap::new();

        for (key, value) in table.into_iter() {
            let key = key.into();
            let value = value.into();

            if let Some(existing) = sizes.get_mut(&key.len()) {
                existing.insert(key, value);
            } else {
                sizes.insert(key.len(), [(key, value)].into_iter().collect());
            }
        }

        Self {
            replacements: sizes.into_iter().rev().collect(),
        }
    }

    pub fn substitute_symbols<'a>(&self, value: &'a str) -> Cow<'a, str> {
        let mut replacements_iter = self.replacements.iter().enumerate().peekable();

        let mut output: Option<String> = None;

        struct PanicIter<T>(PhantomData<T>);
        impl<T> Iterator for PanicIter<T> {
            type Item = T;

            fn next(&mut self) -> Option<Self::Item> {
                panic!("Too many loops")
            }
        }

        let mut i = 0;
        'is_done: for _ in (0..value.len() + 1).chain(PanicIter(PhantomData)) {
            let (_front, back) = value.split_at(i);
            if back.is_empty() {
                break 'is_done;
            }

            let remaining_tables = 'table_cull: loop {
                if let Some((i, (str_len, _replacement_table))) = replacements_iter.peek() {
                    if back.len() < *str_len {
                        replacements_iter.next();
                    } else {
                        break 'table_cull &self.replacements[*i..];
                    }
                } else {
                    if let Some(output) = &mut output {
                        output.push_str(back);
                    }
                    break 'is_done;
                }
            };

            let mut replaced = None;
            'replacement: for (str_len, table) in remaining_tables {
                if !back.is_char_boundary(*str_len) {
                    // Skip because otherwise the table would need to contain an invalid character (which isn't possible)
                    continue;
                }

                let sub_str = back.split_at(*str_len).0;
                if let Some(replacement_sub_str) = table.get(sub_str) {
                    let dest = output.get_or_insert_with(|| {
                        // First replacement. Switch to [String] so that we can mutate
                        let mut e = String::with_capacity(value.len());
                        e.push_str(&value[..i]);
                        e
                    });
                    dest.push_str(&replacement_sub_str);
                    replaced = Some(*str_len);
                    break 'replacement; // Found a replacement. Stop searching
                }
            }

            if replaced.is_none() {
                if let Some(output) = &mut output {
                    output.push(value[i..].chars().next().unwrap())
                }
            }

            if let Some(next_i) = ((i + replaced.unwrap_or(1))..value.len())
                .find(|next_i| value.is_char_boundary(*next_i))
            {
                i = next_i;
            } else {
                break 'is_done;
            }
        }

        if let Some(out) = output {
            Cow::Owned(out)
        } else {
            Cow::Borrowed(value)
        }
    }
}

#[test]
fn test_substitute() {
    let test1 = SubstitutionTable::new([
        ("a", "b"),
        ("abc", "p"),
        ("                 ", ""),
        ("fgh", "z"),
    ]);

    assert_eq!(test1.substitute_symbols(""), Cow::Borrowed(""));
    assert_eq!(
        test1.substitute_symbols("a"),
        Cow::<str>::Owned("b".to_string())
    );
    assert_eq!(
        test1.substitute_symbols("aaa"),
        Cow::<str>::Owned("bbb".to_string())
    );
    assert_eq!(
        test1.substitute_symbols("abc"),
        Cow::<str>::Owned("p".to_string())
    );
    assert_eq!(
        test1.substitute_symbols("ababcc"),
        Cow::<str>::Owned("bbpc".to_string())
    );
    assert_eq!(
        test1.substitute_symbols("more abc"),
        Cow::<str>::Owned("more p".to_string())
    );
    assert_eq!(
        test1.substitute_symbols("more bc"),
        Cow::Borrowed("more bc")
    );
    assert_eq!(
        test1.substitute_symbols("hghghfh"),
        Cow::Borrowed("hghghfh")
    );
}
