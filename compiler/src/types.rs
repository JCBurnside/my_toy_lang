use std::collections::HashSet;

use crate::{typed_ast, util::ExtraUtilFunctions};

use itertools::Itertools;
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum IntWidth {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
}

impl IntWidth {
    pub fn from_str(s: &str) -> Self {
        match s.as_ref() {
            "8" => Self::Eight,
            "16" => Self::Sixteen,
            "32" => Self::ThirtyTwo,
            "64" => Self::SixtyFour,
            _ => unimplemented!("custom width ints not supported currently"),
        }
    }
    #[allow(unused)]
    fn as_bits(&self) -> u64 {
        match self {
            Self::Eight => 8,
            Self::Sixteen => 16,
            Self::ThirtyTwo => 32,
            Self::SixtyFour => 64,
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Eight => "8".to_string(),
            Self::Sixteen => "16".to_string(),
            Self::ThirtyTwo => "32".to_string(),
            Self::SixtyFour => "64".to_string(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum FloatWidth {
    ThirtyTwo,
    SixtyFour,
}
impl FloatWidth {
    pub fn from_str(s: &str) -> Self {
        match s.as_ref() {
            "32" => Self::ThirtyTwo,
            "64" => Self::SixtyFour,
            _ => unimplemented!("custom width floats not supported"),
        }
    }
    #[allow(unused)]
    fn as_bits(&self) -> u64 {
        match self {
            Self::ThirtyTwo => 32,
            Self::SixtyFour => 64,
        }
    }

    pub(crate) fn to_string(&self) -> String {
        match self {
            Self::ThirtyTwo => "32".to_string(),
            Self::SixtyFour => "64".to_string(),
        }
    }
}

#[derive(Hash, Clone, Debug, Eq)]
pub enum ResolvedType {
    Unknown(usize),
    Bool,
    Number,
    Int {
        signed: bool,
        width: IntWidth,
    },
    Float {
        width: FloatWidth,
    },
    Char,
    Str,
    //probably going to be implimented as a null checked pointer (at time of creation).
    Ref {
        underlining: Box<ResolvedType>,
    },
    Pointer {
        underlining: Box<ResolvedType>,
    },
    Unit,
    //used as rt only
    Void,
    Slice {
        underlining: Box<ResolvedType>,
    },
    Function {
        arg: Box<ResolvedType>,
        returns: Box<ResolvedType>,
        loc: crate::Location,
    },
    User {
        name: String,
        generics: Vec<ResolvedType>,
        loc: crate::Location,
    },

    Array {
        underlining: Box<ResolvedType>,
        size: usize,
    },
    // ForwardUser{name:String},
    Alias {
        actual: Box<ResolvedType>,
        loc: crate::Location,
    },
    Tuple {
        underlining: Vec<ResolvedType>,
        loc: crate::Location,
    },
    Generic {
        name: String,
        loc: crate::Location,
    },
    Error,
}

impl PartialEq for ResolvedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unknown(l0), Self::Unknown(r0)) => l0 == r0,
            (Self::Int { .. } | Self::Float { .. }, Self::Number) => true,
            (Self::Number, Self::Int { .. } | Self::Float { .. }) => true,
            (
                Self::Int {
                    signed: l_signed,
                    width: l_width,
                },
                Self::Int {
                    signed: r_signed,
                    width: r_width,
                },
            ) => l_signed == r_signed && l_width == r_width,
            (Self::Float { width: l_width }, Self::Float { width: r_width }) => l_width == r_width,
            (
                Self::Ref {
                    underlining: l_underlining,
                },
                Self::Ref {
                    underlining: r_underlining,
                },
            ) => l_underlining == r_underlining,
            (
                Self::Pointer {
                    underlining: l_underlining,
                },
                Self::Pointer {
                    underlining: r_underlining,
                },
            ) => l_underlining == r_underlining,
            (
                Self::Slice {
                    underlining: l_underlining,
                },
                Self::Slice {
                    underlining: r_underlining,
                },
            ) => l_underlining == r_underlining,
            (
                Self::Function {
                    arg: l_arg,
                    returns: l_returns,
                    ..
                },
                Self::Function {
                    arg: r_arg,
                    returns: r_returns,
                    ..
                },
            ) => l_arg == r_arg && l_returns == r_returns,
            (
                Self::User {
                    name: l_name,
                    generics: l_generics,
                    ..
                },
                Self::User {
                    name: r_name,
                    generics: r_generics,
                    ..
                },
            ) => l_name == r_name && l_generics == r_generics,
            (
                Self::Array {
                    underlining: l_underlining,
                    size: l_size,
                },
                Self::Array {
                    underlining: r_underlining,
                    size: r_size,
                },
            ) => l_underlining == r_underlining && l_size == r_size,
            (
                Self::Alias {
                    actual: l_actual, ..
                },
                Self::Alias {
                    actual: r_actual, ..
                },
            ) => l_actual == r_actual,
            (Self::Generic { name: l_name, .. }, Self::Generic { name: r_name, .. }) => {
                l_name == r_name
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl ResolvedType {
    pub(crate) fn replace_unkown_with(&mut self, id: usize, ty: Self) {
        match self {
            Self::Unknown(i) if *i == id => *self = ty,
            Self::Ref { underlining }
            | Self::Pointer { underlining }
            | Self::Slice { underlining }
            | Self::Array { underlining, .. } => underlining.replace_unkown_with(id, ty),
            Self::Function {
                arg,
                returns,
                loc: _,
            } => {
                arg.replace_unkown_with(id, ty.clone());
                returns.replace_unkown_with(id, ty);
            }
            Self::User { generics, .. } => {
                for generic in generics {
                    generic.replace_unkown_with(id, ty.clone());
                }
            }
            _ => (),
        }
    }

    pub(crate) fn check_equality(&self, other: &Self) -> bool {
        match (self, other) {
            (lhs, &Self::Number) => lhs.is_float() || lhs.is_int(),
            (&Self::Number, rhs) => rhs.is_float() || rhs.is_int(),
            _ => self == other,
        }
    }

    pub(crate) fn check_function(&self, args: &[Self]) -> bool {
        if args.len() == 0 {
            true
        } else if let Self::Function {
            arg,
            returns,
            loc: _,
        } = self
        {
            arg.check_equality(&args[0]) && returns.check_function(&args[1..])
        } else {
            false
        }
    }

    pub fn fn_ty(&self, returns: &Self) -> Self {
        Self::Function {
            arg: self.clone().boxed(),
            returns: returns.clone().boxed(),
            loc: (0, 0),
        }
    }

    pub fn is_void_or_unit(&self) -> bool {
        match self {
            Self::Void | Self::Unit => true,
            _ => false,
        }
    }

    pub fn is_user(&self) -> bool {
        matches!(self, Self::User { .. })
    }

    pub fn get_all_types(&self) -> HashSet<String> {
        match self {
            ResolvedType::User { .. }
            | ResolvedType::Bool
            | ResolvedType::Int { .. }
            | ResolvedType::Float { .. }
            | ResolvedType::Char
            | ResolvedType::Str => [self.to_string()].into(),
            ResolvedType::Ref { underlining }
            | ResolvedType::Pointer { underlining }
            | ResolvedType::Slice { underlining }
            | ResolvedType::Array { underlining, .. } => {
                let tys = underlining.get_all_types();
                tys
            }
            ResolvedType::Function {
                arg,
                returns,
                loc: _,
            } => {
                let mut tys = arg.get_all_types();
                tys.extend(returns.get_all_types().into_iter());
                tys
            }
            ResolvedType::Alias { actual, loc: _ } => actual.get_all_types(),
            ResolvedType::Tuple {
                underlining: underling,
                loc: _,
            } => underling.iter().flat_map(Self::get_all_types).collect(),
            ResolvedType::Unit
            | ResolvedType::Number
            | ResolvedType::Void
            | ResolvedType::Generic { .. }
            | ResolvedType::Unknown(_) => HashSet::new(),
            ResolvedType::Error => todo!(),
        }
    }

    pub fn as_c_function(&self) -> (Vec<Self>, Self) {
        // (args, return type)
        match dbg!(self) {
            Self::Function { arg, returns, .. } => {
                let (mut args, rt) = returns.as_c_function();
                args.push(arg.as_ref().clone());
                (args, rt)
            }
            _ => (Vec::new(), self.clone()),
        }
    }

    pub fn replace_generic(&self, name: &str, new_ty: Self) -> Self {
        match self {
            Self::Generic { name: old_name, .. } if old_name == name => new_ty,
            Self::Function { arg, returns, loc } => Self::Function {
                arg: Box::new(arg.replace_generic(name, new_ty.clone())),
                returns: Box::new(returns.replace_generic(name, new_ty)),
                loc: *loc,
            },
            _ => self.clone(),
        }
    }

    fn find_first_generic_arg(&self) -> String {
        match self {
            Self::Function { arg, returns, .. } => {
                if arg.is_generic() {
                    arg.find_first_generic_arg()
                } else {
                    returns.find_first_generic_arg()
                }
            }
            Self::Generic { name, .. } => name.clone(),
            _ => unreachable!(),
        }
    }

    pub fn replace_first_generic(self, new_ty: Self) -> Self {
        let name = self.find_first_generic_arg();
        self.replace_generic(&name, new_ty)
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Self::Function { arg, returns, .. } => arg.is_generic() || returns.is_generic(),
            Self::Alias { actual, .. } => actual.is_generic(),
            Self::Slice { underlining }
            | Self::Pointer { underlining }
            | Self::Ref { underlining } => underlining.is_generic(),
            Self::Generic { .. } => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function { .. } => true,
            Self::Alias { actual, .. } => actual.is_function(),
            Self::Pointer { underlining } | Self::Ref { underlining } => underlining.is_function(),
            _ => false,
        }
    }

    pub(crate) fn replace_user_with_generic(self, target_name: &str) -> Self {
        match self {
            ResolvedType::Ref { underlining } => Self::Ref {
                underlining: underlining.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Pointer { underlining } => Self::Ref {
                underlining: underlining.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Slice { underlining } => Self::Slice {
                underlining: underlining.replace_user_with_generic(target_name).boxed(),
            },
            ResolvedType::Function { arg, returns, loc } => Self::Function {
                arg: arg.replace_user_with_generic(target_name).boxed(),
                returns: returns.replace_user_with_generic(target_name).boxed(),
                loc: loc,
            },
            ResolvedType::Array {
                underlining: underlying,
                size,
            } => Self::Array {
                underlining: underlying.replace_user_with_generic(target_name).boxed(),
                size,
            },
            ResolvedType::User { name, loc, .. } if &name == target_name => {
                ResolvedType::Generic { name, loc }
            }
            _ => self,
        }
    }

    pub(crate) fn lower_generics(&mut self, context: &mut typed_ast::LoweringContext) {
        match self {
            ResolvedType::Bool
            | ResolvedType::Int { .. }
            | ResolvedType::Float { .. }
            | ResolvedType::Char
            | ResolvedType::Str
            | ResolvedType::Unit
            | ResolvedType::Generic { .. }
            | ResolvedType::Void => (),
            ResolvedType::Pointer { underlining }
            | ResolvedType::Slice { underlining }
            | ResolvedType::Array { underlining, .. }
            | ResolvedType::Alias {
                actual: underlining,
                ..
            }
            | ResolvedType::Ref { underlining } => underlining.as_mut().lower_generics(context),
            ResolvedType::Function { arg, returns, .. } => {
                arg.as_mut().lower_generics(context);
                returns.as_mut().lower_generics(context);
            }
            ResolvedType::User { name, generics, .. } => {
                if generics.iter().any(ResolvedType::is_generic) || generics.len() == 0 {
                    return;
                }
                let new_name = format!(
                    "{}<{}>",
                    name.clone(),
                    generics.iter().map(ResolvedType::to_string).join(",")
                );
                if !context.generated_generics.contains_key(&new_name) {
                    let mut target = context.globals.get(name).unwrap().clone();
                    let zipped = target
                        .get_generics()
                        .into_iter()
                        .zip(generics.iter().cloned())
                        .collect_vec();
                    target.replace_types(&zipped, context);
                    target.lower_generics(context);
                    let _ = context.generated_generics.insert(new_name.clone(), target);
                }
                *name = new_name;
                *generics = Vec::new();
            }
            ResolvedType::Tuple {
                underlining: underling,
                loc: _,
            } => {
                for ty in underling {
                    ty.lower_generics(context);
                }
            }
            ResolvedType::Unknown(_) | ResolvedType::Number | ResolvedType::Error => (),
        }
    }

    pub fn replace(&self, nice_name: &str, actual: &str) -> Self {
        match self {
            Self::User {
                name,
                generics,
                loc,
            } => {
                if name == nice_name {
                    Self::User {
                        name: actual.to_string(),
                        generics: generics.clone(),
                        loc: *loc,
                    }
                } else {
                    Self::User {
                        name: name.clone(),
                        generics: generics
                            .iter()
                            .map(|it| it.replace(nice_name, actual))
                            .collect(),
                        loc: *loc,
                    }
                }
            }
            Self::Function { arg, returns, loc } => Self::Function {
                arg: arg.replace(nice_name, actual).boxed(),
                returns: returns.replace(nice_name, actual).boxed(),
                loc: *loc,
            },
            _ => self.clone(),
        }
    }

    pub fn remove_args(&self, arg: usize) -> Self {
        if arg == 0 {
            self.clone()
        } else {
            let Self::Function { returns, .. } = self else {
                unreachable!()
            };
            returns.remove_args(arg - 1)
        }
    }

    pub(crate) fn is_error(&self) -> bool {
        match self {
            Self::Error => true,
            _ => false,
        }
    }

    pub(crate) fn is_unknown(&self) -> bool {
        match self {
            Self::Number //since it can be one of many types.
            | Self::Unknown(_) => true,
            Self::Function { arg, returns, loc:_ } => arg.is_unknown() || returns.is_unknown(),
            _ => false,
        }
    }

    pub(crate) fn get_nth_arg(&self, idx: usize) -> Self {
        match self {
            Self::Function { arg, .. } if idx == 0 => arg.as_ref().clone(),
            Self::Function { returns, .. } => returns.get_nth_arg(idx - 1),
            _ => Self::Error,
        }
    }
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float { .. })
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int { .. })
    }
    #[allow(unused)]
    // TODO! examine why this is unused.
    pub(crate) fn get_dependant_unknowns(&self) -> Vec<usize> {
        match self {
            Self::Unknown(id) => vec![*id],
            Self::Pointer { underlining }
            | Self::Slice { underlining }
            | Self::Array { underlining, .. }
            | Self::Ref { underlining } => underlining.get_dependant_unknowns(),
            Self::Function {
                arg,
                returns,
                loc: _,
            } => {
                let mut out = arg.get_dependant_unknowns();
                out.extend(returns.get_dependant_unknowns());
                out
            }
            Self::User {
                name: _,
                generics,
                loc: _,
            } => generics
                .iter()
                .flat_map(|it| it.get_dependant_unknowns())
                .collect(),
            Self::Alias { actual: _, loc: _ } => Vec::new(), //aliases can't be infered
            _ => Vec::new(),
        }
    }

    pub(crate) fn contains_unknown(&self, id: usize) -> bool {
        match self {
            Self::Unknown(i) => i == &id,
            Self::Array { underlining, .. }
            | Self::Ref { underlining }
            | Self::Pointer { underlining }
            | Self::Slice { underlining } => underlining.contains_unknown(id),
            Self::Function {
                arg,
                returns,
                loc: _,
            } => arg.contains_unknown(id) || returns.contains_unknown(id),
            _ => false,
        }
    }
}

impl ToString for ResolvedType {
    fn to_string(&self) -> String {
        match self {
            ResolvedType::Number => "{number}".to_string(),
            ResolvedType::Bool => "bool".to_string(),
            ResolvedType::Alias { actual, .. } => actual.to_string(),
            ResolvedType::Char => "char".to_string(),
            ResolvedType::Float { width } => "float".to_string() + &width.to_string(),
            ResolvedType::Function { arg, returns, .. } => {
                let arg = if arg.is_function() {
                    format!("({})", arg.to_string())
                } else {
                    arg.to_string()
                };
                format!("{}->{}", arg, returns.to_string())
            }
            ResolvedType::Generic { name, .. } => {
                format!("<GenericArg:{name}>")
            }
            ResolvedType::Int {
                signed: false,
                width,
            } => "uint".to_string() + &width.to_string(),
            ResolvedType::Int {
                signed: true,
                width,
            } => "int".to_string() + &width.to_string(),
            ResolvedType::Pointer { underlining } => "p_".to_string() + &underlining.to_string(),
            ResolvedType::Ref { .. } | ResolvedType::Slice { .. } => {
                todo!("how to serialize this")
            }
            ResolvedType::Str => "str".to_string(),
            ResolvedType::Unit => "()".to_string(),
            ResolvedType::Void => "".to_string(),
            ResolvedType::User {
                name,
                generics,
                loc: _,
            } => {
                if generics.len() > 0 {
                    format!(
                        "{}<{}>",
                        name,
                        &generics.iter().map(Self::to_string).join(",")
                    )
                } else {
                    name.clone()
                }
            }
            ResolvedType::Array {
                underlining: underlying,
                size,
            } => {
                format!("[{};{}]", underlying.to_string(), size)
            }
            ResolvedType::Tuple {
                underlining: underling,
                loc: _,
            } => format!(
                "({})",
                underling.iter().map(ResolvedType::to_string).join(", ")
            ),
            ResolvedType::Unknown(_) | ResolvedType::Error => "<ERROR>".to_string(),
        }
    }
}
#[allow(unused)]
mod consts {
    use super::*;
    pub const BOOL: ResolvedType = ResolvedType::Bool;
    pub const ERROR: ResolvedType = ResolvedType::Error;
    pub const NUMBER: ResolvedType = ResolvedType::Number;
    pub const INT8: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::Eight,
    };
    pub const INT16: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::Sixteen,
    };
    pub const INT32: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::ThirtyTwo,
    };
    pub const INT64: ResolvedType = ResolvedType::Int {
        signed: true,
        width: IntWidth::SixtyFour,
    };
    pub const UINT8: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::Eight,
    };
    pub const UINT16: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::Sixteen,
    };
    pub const UINT32: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::ThirtyTwo,
    };
    pub const UINT64: ResolvedType = ResolvedType::Int {
        signed: false,
        width: IntWidth::SixtyFour,
    };
    pub const FLOAT32: ResolvedType = ResolvedType::Float {
        width: FloatWidth::ThirtyTwo,
    };
    pub const FLOAT64: ResolvedType = ResolvedType::Float {
        width: FloatWidth::SixtyFour,
    };
    pub const STR: ResolvedType = ResolvedType::Str;
    pub const CHAR: ResolvedType = ResolvedType::Char;
    pub const UNIT: ResolvedType = ResolvedType::Unit;
}
pub use consts::*;

#[cfg(test)]
mod tests {
    use crate::util::ExtraUtilFunctions;

    #[test]
    #[ignore = "for testing equality"]
    fn eq() {
        println!(
            "{}",
            super::ResolvedType::Function {
                arg: super::INT32.boxed(),
                returns: super::INT32.boxed(),
                loc: (0, 0)
            } == super::ResolvedType::Function {
                arg: super::INT32.boxed(),
                returns: super::INT32.boxed(),
                loc: (1, 0)
            }
        )
    }
}
