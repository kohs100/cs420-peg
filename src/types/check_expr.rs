use crate::ast::expr::*;
use crate::ast::types::*;

use crate::types::defs::*;
use crate::types::scope::ScopeGuard;

use crate::types::defs::ATyp::*;
use crate::types::defs::Typ::*;

fn check_lval(expr: &Expr, sg: &ScopeGuard) -> TypeResult<Option<Typ>> {
    match expr {
        Expr::LUnop(LeftUnaryOp::Ref, iexpr) => {
            if let Address(inner) = check_expr(iexpr, sg)? {
                Ok(Some(*inner))
            } else {
                Err(TypeError::TypeIncompatible.into())
            }
        }
        Expr::Index(arr, idx) => {
            if let Array(elemtyp, _) = check_expr(arr, sg)? {
                if let Arith(Int(_, _)) = check_expr(idx, sg)? {
                    Ok(Some(*elemtyp))
                } else {
                    Err(TypeError::TypeIncompatible.into())
                }
            } else {
                Err(TypeError::TypeIncompatible.into())
            }
        }
        Expr::Ternary(bexpr, texpr, fexpr) => {
            let btyp = check_expr(bexpr, sg)?;
            if btyp.becomes(Bool) {
                let ttyp = check_lval(texpr, sg)?;
                let ftyp = check_lval(fexpr, sg)?;
                if let (Some(ttyp), Some(ftyp)) = (ttyp, ftyp) {
                    if ttyp == ftyp {
                        Ok(Some(ttyp))
                    } else {
                        Err(TypeError::TypeIncompatible.into())
                    }
                } else {
                    Err(TypeError::TypeIncompatible.into())
                }
            } else {
                Err(TypeError::TypeIncompatible.into())
            }
        }
        Expr::Comma(_, nxt) => check_lval(nxt, sg),
        _ => Ok(None),
    }
}

fn check_params(atyps: Vec<Typ>, ptyps: Vec<Typ>) -> TypeResult<()> {
    if atyps.len() != ptyps.len() {
        Err(TypeError::TypesMismatch("Different length".to_owned(), atyps, ptyps).into())
    } else {
        if atyps
            .iter()
            .zip(ptyps.iter())
            .all(|(atyp, ptyp)| atyp.becomes(ptyp))
        {
            Ok(())
        } else {
            Err(TypeError::TypesMismatch("Incompatible types".to_owned(), atyps, ptyps).into())
        }
    }
}

pub fn check_expr(expr: &Expr, sg: &ScopeGuard) -> TypeResult<Typ> {
    match expr {
        Expr::UnaryIncRight(iexpr)
        | Expr::UnaryDecRight(iexpr)
        | Expr::LUnop(LeftUnaryOp::Inc | LeftUnaryOp::Dec, iexpr) => {
            if let Some(ityp) = check_lval(iexpr, sg)? {
                match ityp {
                    Address(_) => Ok(ityp),
                    Arith(atyp) => {
                        let after_promotion = atyp.promote().into();
                        // use of an operand of type ‘bool’ in ‘operator++’ is forbidden
                        match atyp {
                            Float(_) => Ok(after_promotion),
                            Int(_, _) => Ok(after_promotion),
                            Bool => Err(TypeError::TypeIncompatible.into()),
                        }
                    }
                    _ => Err(TypeError::TypeIncompatible.into()),
                }
            } else {
                Err(TypeError::TypeIncompatible.into())
            }
        }
        Expr::Cast(TypeName(ptyp, adecl), _expr) => Ok(adecl.to_typ(ptyp)),
        Expr::Call(fexpr, aexprs) => {
            let atyps = aexprs
                .iter()
                .map(|e| check_expr(e, sg))
                .collect::<Result<Vec<Typ>, PositionalTypeError>>()?;

            let ftyp = check_expr(fexpr, sg)?;

            if let FuncDecl(rettyp, ptyps) = ftyp {
                check_params(atyps, ptyps).map(|_| *rettyp)
            } else if let Address(boxed) = ftyp.clone() {
                if let FuncDecl(rettyp, ptyps) = *boxed {
                    check_params(atyps, ptyps).map(|_| *rettyp)
                } else {
                    Err(TypeError::NotCallable(ftyp).into())
                }
            } else {
                Err(TypeError::NotCallable(ftyp).into())
            }
        }
        // arr[idx] is same with *(arr+idx)
        Expr::Index(aexpr, iexpr) => match check_expr(iexpr, sg)? {
            Arith(Bool | Int(_, _)) => {
                if let Array(e, _) = check_expr(aexpr, sg)? {
                    Ok(*e)
                } else {
                    Err(TypeError::TypeIncompatible.into())
                }
            }
            _ => Err(TypeError::TypeIncompatible.into()),
        },
        Expr::LUnop(lunop, iexpr) => {
            let ityp = check_expr(iexpr, sg)?;

            match lunop {
                LeftUnaryOp::Plus | LeftUnaryOp::Minus => match ityp {
                    Arith(atyp) => Ok(Arith(atyp.promote())),
                    _ => Err(TypeError::TypeIncompatible.into()),
                },
                LeftUnaryOp::BoolNot => {
                    if ityp.becomes(Bool) {
                        Ok(ityp)
                    } else {
                        Err(TypeError::TypeIncompatible.into())
                    }
                }
                LeftUnaryOp::BitNot => match ityp {
                    Arith(Int(_, _)) => Ok(ityp),
                    _ => Err(TypeError::TypeIncompatible.into()),
                },
                LeftUnaryOp::Deref => Ok(Address(Box::new(ityp))),
                LeftUnaryOp::Ref => {
                    if let Address(i) = ityp {
                        Ok(*i)
                    } else {
                        Err(TypeError::TypeIncompatible.into())
                    }
                }
                LeftUnaryOp::Inc | LeftUnaryOp::Dec => unreachable!("Already covered"),
            }
        }
        Expr::Binop(lhs, binop, rhs) => {
            let ltyp = check_expr(lhs, sg)?;
            let rtyp = check_expr(rhs, sg)?;
            match binop {
                // Valid with all arithmetic types
                BinOp::Mul | BinOp::Div => match (ltyp, rtyp) {
                    (Arith(lat), Arith(rat)) => Ok((lat + rat).into()),
                    _ => Err(TypeError::TypeIncompatible.into()),
                },
                // Only i % i
                BinOp::Mod => match (ltyp, rtyp) {
                    (Arith(lat @ Int(_, _)), Arith(rat @ Int(_, _))) => Ok((lat + rat).into()),
                    _ => Err(TypeError::TypeIncompatible.into()),
                },
                BinOp::Add | BinOp::Sub => {
                    match (ltyp, rtyp) {
                        // ARITH + ARITH
                        (Arith(lat), Arith(rat)) => Ok((lat + rat).into()),
                        // p+i | i+p
                        (Arith(Int(_, _)), it @ Address(_))
                        | (it @ Address(_), Arith(Int(_, _))) => Ok(it),
                        // arr+i | i+arr
                        (Arith(Int(_, _)), at @ Array(_, _))
                        | (at @ Array(_, _), Arith(Int(_, _))) => Ok(at),
                        _ => Err(TypeError::TypeIncompatible.into()),
                    }
                }
                // Only i >> i, i << i
                BinOp::ShftL | BinOp::ShftR => match (ltyp, rtyp) {
                    (Arith(lat @ Int(_, _)), Arith(rat @ Int(_, _))) => Ok((lat + rat).into()),
                    _ => Err(TypeError::TypeIncompatible.into()),
                },
                BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge | BinOp::Eq | BinOp::Ne => {
                    if match (ltyp, rtyp) {
                        (Arith(Int(_, _)), Arith(Int(_, _))) => true,
                        (ptrtyp @ Address(_), arrtyp @ Array(_, _))
                        | (arrtyp @ Array(_, _), ptrtyp @ Address(_)) => arrtyp.becomes(&ptrtyp),
                        (Address(la), Address(ra)) => la == ra,
                        (Array(la, _), Array(ra, _)) => la == ra,
                        _ => false,
                    } {
                        Ok(Bool.into())
                    } else {
                        Err(TypeError::TypeIncompatible.into())
                    }
                }
                BinOp::BitAnd | BinOp::BitXor | BinOp::BitOr => match (ltyp, rtyp) {
                    (Arith(lat @ Int(_, _)), Arith(rat @ Int(_, _))) => Ok((lat + rat).into()),
                    _ => Err(TypeError::TypeIncompatible.into()),
                },

                BinOp::BoolAnd | BinOp::BoolOr => {
                    if ltyp.becomes(Bool) && rtyp.becomes(Bool) {
                        Ok(Bool.into())
                    } else {
                        Err(TypeError::TypeIncompatible.into())
                    }
                }
            }
        }
        Expr::Ternary(cond, tb, fb) => {
            if check_expr(cond, sg)?.becomes(Bool) {
                let tt = check_expr(tb, sg)?;
                let ft = check_expr(fb, sg)?;
                if tt == ft {
                    Ok(tt)
                } else if tt.becomes(&ft) {
                    Ok(ft)
                } else if ft.becomes(&tt) {
                    Ok(tt)
                } else {
                    Err(TypeError::TypeIncompatible.into())
                }
            } else {
                Err(TypeError::TypeIncompatible.into())
            }
        }
        Expr::Assign(lexpr, assop, rexpr) => {
            if let Some(tgttyp) = check_lval(lexpr, sg)? {
                let assexpr = match assop.to_binop() {
                    Some(binop) => Expr::Binop(lexpr.clone(), binop, rexpr.clone()),
                    None => *rexpr.clone(),
                };
                if check_expr(&assexpr, sg)?.becomes(&tgttyp) {
                    Ok(tgttyp)
                } else {
                    Err(TypeError::TypeIncompatible.into())
                }
            } else {
                Err(TypeError::TypeIncompatible.into())
            }
        }
        Expr::Comma(_, i) => check_expr(i, sg),
        Expr::Ident(nm) => {
            if let Some(typ) = sg.get_type(nm) {
                Ok(typ)
            } else {
                Err(TypeError::InvalidIdentifier(nm.to_owned()).into())
            }
        }
        c @ (Expr::Const(_) | Expr::ConstFloat(_) | Expr::ConstBool(_)) => {
            Ok(Typ::from_constexpr(c))
        }
        Expr::StringLiteral(_) => Ok(Address(Box::new(Int(true, 1).into()))),
    }
}
