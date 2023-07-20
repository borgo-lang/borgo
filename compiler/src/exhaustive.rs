// Most of this file is taken from the Roc compiler by <roc-lang.org/authors>
// Licensed under: The Universal Permissive License (UPL), Version 1.0

//! Exhaustiveness checking, based on "Warning for pattern matching" (Luc Maranget, 2007).
//! http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

use crate::ast::{Expr, Literal, Pat, Span};
use crate::error;
use crate::infer;
use crate::type_::Type;

use self::Pattern::*;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Union {
    pub alternatives: Vec<Ctor>,
    pub render_as: RenderAs,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RenderAs {
    Tag,
    Opaque,
    Record(Vec<Lowercase>),
    Guard,
}

pub type TagId = String;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CtorName {
    Tag(TagName),
    Opaque(Symbol),
}

impl CtorName {
    pub fn is_tag(&self, tag_name: &TagName) -> bool {
        match self {
            Self::Tag(test) => test == tag_name,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub name: CtorName,
    pub tag_id: TagId,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Anything,
    Literal(Literal),
    Ctor(Union, TagId, std::vec::Vec<Pattern>),
}

/// Error

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Incomplete(Region, Context, Vec<Pattern>),
    Redundant {
        overall_region: Region,
        branch_region: Region,
        index: HumanIndex,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Context {
    BadArg,
    BadDestruct,
    BadCase,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Guard {
    HasGuard,
    NoGuard,
}

/// Check

pub fn check_internal(
    region: Region,
    context: Context,
    matrix: Vec<Vec<Pattern>>,
) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();
    let bad_patterns = is_exhaustive(&matrix, 1);
    if !bad_patterns.is_empty() {
        // TODO i suspect this is like a concat in in practice? code below can panic
        // if this debug_assert! ever fails, the theory is disproven
        debug_assert!(bad_patterns.iter().map(|v| v.len()).sum::<usize>() == bad_patterns.len());
        let heads = bad_patterns.into_iter().map(|mut v| v.remove(0)).collect();
        errors.push(Error::Incomplete(region, context, heads));
        return Err(errors);
    }
    Ok(())
}

/// EXHAUSTIVE PATTERNS

/// INVARIANTS:
///
///   The initial rows "matrix" are all of length 1
///   The initial count of items per row "n" is also 1
///   The resulting rows are examples of missing patterns
fn is_exhaustive(matrix: &RefPatternMatrix, n: usize) -> PatternMatrix {
    if matrix.is_empty() {
        vec![std::iter::repeat(Anything).take(n).collect()]
    } else if n == 0 {
        vec![]
    } else {
        let ctors = collect_ctors(matrix);
        let num_seen = ctors.len();

        if num_seen == 0 {
            let new_matrix: Vec<_> = matrix
                .iter()
                .filter_map(|row| specialize_row_by_anything(row))
                .collect();
            let mut rest = is_exhaustive(&new_matrix, n - 1);

            for row in rest.iter_mut() {
                row.push(Anything);
            }

            rest
        } else {
            let alts = ctors.iter().next().unwrap().1;

            let alt_list = &alts.alternatives;
            let num_alts = alt_list.len();

            if num_seen < num_alts {
                let new_matrix: Vec<_> = matrix
                    .iter()
                    .filter_map(|row| specialize_row_by_anything(row))
                    .collect();
                let rest: Vec<Vec<Pattern>> = is_exhaustive(&new_matrix, n - 1);

                let last: _ = alt_list
                    .iter()
                    .filter_map(|r| is_missing(alts.clone(), &ctors, r));

                let mut result = Vec::new();

                for last_option in last {
                    for mut row in rest.clone() {
                        row.push(last_option.clone());

                        result.push(row);
                    }
                }

                result
            } else {
                let is_alt_exhaustive = |Ctor { arity, tag_id, .. }| {
                    let new_matrix: Vec<_> = matrix
                        .iter()
                        .filter_map(|r| specialize_row_by_ctor(tag_id.clone(), arity, r))
                        .collect();
                    let rest: Vec<Vec<Pattern>> = is_exhaustive(&new_matrix, arity + n - 1);

                    let mut result = Vec::with_capacity(rest.len());
                    for row in rest {
                        result.push(recover_ctor(alts.clone(), tag_id.clone(), arity, row));
                    }

                    result
                };

                alt_list
                    .iter()
                    .cloned()
                    .flat_map(is_alt_exhaustive)
                    .collect()
            }
        }
    }
}

fn is_missing<T>(union: Union, ctors: &HashMap<TagId, T>, ctor: &Ctor) -> Option<Pattern> {
    let Ctor { arity, tag_id, .. } = ctor;

    if ctors.contains_key(tag_id) {
        None
    } else {
        let anythings = std::iter::repeat(Anything).take(*arity).collect();
        Some(Pattern::Ctor(union, tag_id.clone(), anythings))
    }
}

fn recover_ctor(
    union: Union,
    tag_id: TagId,
    arity: usize,
    mut patterns: Vec<Pattern>,
) -> Vec<Pattern> {
    let mut rest = patterns.split_off(arity);
    let args = patterns;

    rest.push(Ctor(union, tag_id, args));

    rest
}

/// Check if a new row "vector" is useful given previous rows "matrix"
pub fn is_useful(mut old_matrix: PatternMatrix, mut vector: Row) -> bool {
    let mut matrix = Vec::with_capacity(old_matrix.len());

    // this loop ping-pongs the rows between old_matrix and matrix
    'outer: loop {
        match vector.pop() {
            _ if old_matrix.is_empty() => {
                // No rows are the same as the new vector! The vector is useful!
                break true;
            }
            None => {
                // There is nothing left in the new vector, but we still have
                // rows that match the same things. This is not a useful vector!
                break false;
            }
            Some(first_pattern) => {
                // NOTE: if there are bugs in this code, look at the ordering of the row/matrix

                match first_pattern {
                    // keep checking rows that start with this Ctor or Anything
                    Ctor(_, id, args) => {
                        specialize_row_by_ctor2(id, args.len(), &mut old_matrix, &mut matrix);

                        std::mem::swap(&mut old_matrix, &mut matrix);

                        vector.extend(args);
                    }

                    Anything => {
                        // check if all alternatives appear in matrix
                        match is_complete(&old_matrix) {
                            Complete::No => {
                                // This Anything is useful because some Ctors are missing.
                                // But what if a previous row has an Anything?
                                // If so, this one is not useful.
                                for mut row in old_matrix.drain(..) {
                                    if let Some(Anything) = row.pop() {
                                        matrix.push(row);
                                    }
                                }

                                std::mem::swap(&mut old_matrix, &mut matrix);
                            }
                            Complete::Yes(alternatives) => {
                                // All Ctors are covered, so this Anything is not needed for any
                                // of those. But what if some of those Ctors have subpatterns
                                // that make them less general? If so, this actually is useful!
                                for alternative in alternatives {
                                    let Ctor { arity, tag_id, .. } = alternative;

                                    let mut old_matrix = old_matrix.clone();
                                    let mut matrix = vec![];
                                    specialize_row_by_ctor2(
                                        tag_id,
                                        arity,
                                        &mut old_matrix,
                                        &mut matrix,
                                    );

                                    let mut vector = vector.clone();
                                    vector.extend(std::iter::repeat(Anything).take(arity));

                                    if is_useful(matrix, vector) {
                                        break 'outer true;
                                    }
                                }

                                break false;
                            }
                        }
                    }

                    Literal(literal) => {
                        // keep checking rows that start with this Literal or Anything

                        for mut row in old_matrix.drain(..) {
                            let head = row.pop();
                            let patterns = row;

                            match head {
                                Some(Literal(lit)) => {
                                    if lit == literal {
                                        matrix.push(patterns);
                                    } else {
                                        // do nothing
                                    }
                                }
                                Some(Anything) => matrix.push(patterns),

                                Some(Ctor(_, _, _)) => panic!(
                                    r#"Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."#
                                ),

                                None => panic!(
                                    "Compiler error! Empty matrices should not get specialized."
                                ),
                            }
                        }
                        std::mem::swap(&mut old_matrix, &mut matrix);
                    }
                }
            }
        }
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_row_by_ctor2(
    tag_id: TagId,
    arity: usize,
    old_matrix: &mut PatternMatrix,
    matrix: &mut PatternMatrix,
) {
    for mut row in old_matrix.drain(..) {
        let head = row.pop();
        let mut patterns = row;

        match head {
        Some(Ctor(_, id, args)) =>
            if id == tag_id {
                patterns.extend(args);
                matrix.push(patterns);
            } else {
                // do nothing
            }
        Some(Anything) => {
            // TODO order!
            patterns.extend(std::iter::repeat(Anything).take(arity));
            matrix.push(patterns);
            }
        Some(Literal(_)) => panic!( "Compiler bug! After type checking, constructors and literal should never align in pattern match exhaustiveness checks."),
        None => panic!("Compiler error! Empty matrices should not get specialized."),
    }
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_row_by_ctor(tag_id: TagId, arity: usize, row: &RefRow) -> Option<Row> {
    let mut row = row.to_vec();

    let head = row.pop();
    let patterns = row;

    match head {
        Some(Ctor(_, id, args)) => {
            if id == tag_id {
                // TODO order!
                let mut new_patterns = Vec::new();
                new_patterns.extend(args);
                new_patterns.extend(patterns);
                Some(new_patterns)
            } else {
                None
            }
        }
        Some(Anything) => {
            // TODO order!
            let new_patterns = std::iter::repeat(Anything)
                .take(arity)
                .chain(patterns)
                .collect();
            Some(new_patterns)
        }
        Some(Literal(_)) => unreachable!(
            r#"Compiler bug! After type checking, a constructor can never align with a literal: that should be a type error!"#
        ),
        None => panic!("Compiler error! Empty matrices should not get specialized."),
    }
}

/// INVARIANT: (length row == N) ==> (length result == N-1)
fn specialize_row_by_anything(row: &RefRow) -> Option<Row> {
    let mut row = row.to_vec();

    match row.pop() {
        Some(Anything) => Some(row),
        _ => None,
    }
}

/// ALL CONSTRUCTORS ARE PRESENT?

pub enum Complete {
    Yes(Vec<Ctor>),
    No,
}

fn is_complete(matrix: &RefPatternMatrix) -> Complete {
    let ctors = collect_ctors(matrix);
    let length = ctors.len();
    let mut it = ctors.into_iter();

    match it.next() {
        None => Complete::No,
        Some((_, Union { alternatives, .. })) => {
            if length == alternatives.len() {
                Complete::Yes(alternatives)
            } else {
                Complete::No
            }
        }
    }
}

/// COLLECT CTORS

type RefPatternMatrix = [Vec<Pattern>];
type PatternMatrix = Vec<Vec<Pattern>>;
type RefRow = [Pattern];
type Row = Vec<Pattern>;

fn collect_ctors(matrix: &RefPatternMatrix) -> HashMap<TagId, Union> {
    let mut ctors = HashMap::default();

    for row in matrix {
        if let Some(Ctor(union, id, _)) = row.last() {
            ctors.insert(id.clone(), union.clone());
        }
    }

    ctors
}

// --------
// My changes
// --------

type HumanIndex = usize;
type Lowercase = String;
type TagName = String;
type Symbol = String;
type Region = usize;

pub fn check(expr: &Expr, instance: &infer::Infer) -> Result<(), error::Error> {
    match expr {
        Expr::Literal { lit, .. } => match lit {
            Literal::Slice(expr) => expr.iter().try_for_each(|e| check(e, instance)),
            _ => Ok(()),
        },

        Expr::Closure { fun, .. } => check(&fun.body, instance),

        Expr::Block { stmts, .. } => stmts.iter().try_for_each(|e| check(e, instance)),

        Expr::Let { value, .. } => check(value, instance),

        Expr::Var { .. } => Ok(()),

        Expr::Call { func, args, .. } => {
            check(func, instance)?;
            args.iter().try_for_each(|a| check(a, instance))
        }

        Expr::If {
            cond, then, els, ..
        } => {
            check(cond, instance)?;
            check(then, instance)?;
            check(els, instance)
        }

        Expr::CheckType { .. } => Ok(()),

        Expr::Match {
            subject,
            arms,
            span,
            ..
        } => {
            subject_is_matchable(instance, &subject.get_type(), &subject.get_span())?;

            let matrix = arms
                .iter()
                .map(|a| vec![simplify(instance, &a.pat)])
                .collect();

            check_internal(0, Context::BadCase, matrix)
                .map_err(|e| error::Error::from_exhaustive(e, span.clone()))
        }

        Expr::Tuple { elems, .. } => elems.iter().try_for_each(|a| check(a, instance)),

        Expr::EnumDef { .. } => Ok(()),
        Expr::StructDef { .. } => Ok(()),
        Expr::StructCall { rest, .. } => {
            if let Some(expr) = &**rest {
                return check(expr, instance);
            }
            Ok(())
        }
        Expr::FieldAccess { expr, .. } => check(expr, instance),
        Expr::VarUpdate { .. } => Ok(()),
        Expr::MethodCall { .. } => Ok(()),

        Expr::Return { expr, .. } => check(expr, instance),
        Expr::Try { expr, .. } => check(expr, instance),

        Expr::Trait { .. } => Ok(()),
        Expr::ImplBlock { items, .. } => items.iter().try_for_each(|a| check(a, instance)),

        Expr::Binary { left, right, .. } => {
            check(left, instance)?;
            check(right, instance)
        }

        Expr::Paren { expr, .. } => check(expr, instance),
        Expr::Unary { expr, .. } => check(expr, instance),
        Expr::Const { expr, .. } => check(expr, instance),
        Expr::Debug { expr, .. } => check(expr, instance),
        Expr::Spawn { expr, .. } => check(expr, instance),
        Expr::Select { arms, .. } => arms.iter().try_for_each(|a| check(&a.expr, instance)),
        Expr::Defer { expr, .. } => check(expr, instance),
        Expr::Reference { expr, .. } => check(expr, instance),
        Expr::Index { expr, index, .. } => {
            check(expr, instance)?;
            check(index, instance)
        }

        Expr::Loop { kind, body, .. } => {
            match kind {
                crate::ast::Loop::NoCondition => Ok(()),
                crate::ast::Loop::WithCondition { expr, .. } => check(expr, instance),
                crate::ast::Loop::While { expr } => check(expr, instance),
            }?;

            check(body, instance)
        }

        Expr::Flow { .. } => Ok(()),
        Expr::TypeAlias { .. } => Ok(()),
        Expr::NewtypeDef { .. } => Ok(()),
        Expr::UsePackage { .. } => Ok(()),
        Expr::Unit { .. } => Ok(()),
        Expr::Raw { .. } => Ok(()),
        Expr::Noop => Ok(()),
        Expr::Todo => Ok(()),
    }
}

fn simplify(instance: &infer::Infer, pat: &Pat) -> Pattern {
    match pat {
        Pat::Type { .. } => Anything,
        Pat::Wild { .. } => Anything,
        Pat::Unit { .. } => Anything,
        Pat::Lit { lit, .. } => {
            // Treat booleans as Enums, because there are effectively only two constructors.
            // That's not true of other literals which can have infinite combinations.
            if let Literal::Bool(b) = lit {
                return simplify_bool(*b);
            }

            Pattern::Literal(lit.clone())
        }

        Pat::Pat {
            ident, elems, ty, ..
        } => {
            let def = match ty {
                Type::Con { id, .. } => instance.gs.get_enum(id).unwrap(),

                // If it's not a constructor, then it's likely there is already
                // an error for this pattern match raised during inference. We
                // can just skip exhaustiveness checking in this case.
                _ => return Anything,
            };
            let pats = elems.iter().map(|p| simplify(instance, p)).collect();
            let alts = def
                .cons
                .iter()
                .map(|c| {
                    let qualified = c.to_qualified(&def.name);
                    let name = CtorName::Tag(qualified.clone());
                    let arity = c.fields.len();
                    Ctor {
                        name,
                        tag_id: qualified,
                        arity,
                    }
                })
                .collect();

            let union = Union {
                alternatives: alts,
                render_as: RenderAs::Tag,
            };

            let name = ident.to_string();

            Ctor(union, name, pats)
        }

        Pat::Struct {
            ident, fields, ty, ..
        } => {
            let def = match ty {
                Type::Con { id, .. } => instance.gs.get_struct(id).unwrap(),

                // Same as enums: if it's not a Con then there's an error somewhere else and we
                // just bail.
                _ => return Anything,
            };

            // A struct can be thought of as an enum with a single Ctor, right?
            // So it's important to maintain the same order as in the struct definition when
            // simplifying fields, because Foo { x, y } is different than Foo { y, x }
            // What we want is something like Foo::Foo(x, y) no matter the order.

            let pats = def
                .fields
                .iter()
                .map(|f| {
                    fields
                        .iter()
                        .find_map(|p| {
                            if p.name == f.name {
                                Some(simplify(instance, &p.value))
                            } else {
                                None
                            }
                        })
                        .unwrap_or(Anything)
                })
                .collect();

            let name = CtorName::Tag(ident.clone());
            let alts = Ctor {
                name,
                tag_id: ident.clone(),
                arity: def.fields.len(),
            };

            let union = Union {
                alternatives: vec![alts],
                render_as: RenderAs::Tag,
            };

            Ctor(union, ident.clone(), pats)
        }
    }
}

fn simplify_bool(b: bool) -> Pattern {
    let make_alt = |the_bool: bool| {
        let ident = the_bool.to_string();
        let name = CtorName::Tag(ident.clone());
        Ctor {
            name,
            tag_id: ident,
            arity: 0,
        }
    };

    let union = Union {
        alternatives: vec![make_alt(true), make_alt(false)],
        render_as: RenderAs::Tag,
    };

    Ctor(union, b.to_string(), vec![])
}

fn subject_is_matchable(
    instance: &infer::Infer,
    subject_ty: &Type,
    subject_span: &Span,
) -> Result<(), error::Error> {
    match subject_ty {
        Type::Con { id: _, args } => {
            args.iter()
                .try_for_each(|a| subject_is_matchable(instance, a, subject_span))?;

            Ok(())
        }

        Type::Fun { .. } => Err(error::Error::Generic(
            "Can't match on functions".to_string(),
            subject_span.clone(),
        )),

        Type::Var(_) => Ok(()),
    }
}
