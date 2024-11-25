use crate::ast::*;

use crate::types::defs::*;
use crate::types::scope::ScopeGuard;

fn check_lval(expr: &Expr, sg: &ScopeGuard) -> TypeResult<Option<Typ>> {
    match expr {
        Expr::LUnop(LeftUnaryOp::Ref, iexpr) => {
            if let Typ::Address(inner) = check_expr(iexpr, sg)? {
                Ok(Some(*inner))
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Expr::Index(arr, idx) => {
            if let Typ::Array(elemtyp, _) = check_expr(arr, sg)? {
                if let Typ::Arith(ATyp::Int(_, _)) = check_expr(idx, sg)? {
                    Ok(Some(*elemtyp))
                } else {
                    Err(TypeError::TypeIncompatible)
                }
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Expr::Ternary(bexpr, texpr, fexpr) => {
            let btyp = check_expr(bexpr, sg)?;
            if btyp.becomes(ATyp::Bool) {
                let ttyp = check_lval(texpr, sg)?;
                let ftyp = check_lval(fexpr, sg)?;
                if let (Some(ttyp), Some(ftyp)) = (ttyp, ftyp) {
                    if ttyp == ftyp {
                        Ok(Some(ttyp))
                    } else {
                        Err(TypeError::TypeIncompatible)
                    }
                } else {
                    Err(TypeError::TypeIncompatible)
                }
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Expr::Comma(_, nxt) => check_lval(nxt, sg),
        _ => Ok(None),
    }
}

pub fn check_expr(expr: &Expr, sg: &ScopeGuard) -> TypeResult<Typ> {
    match expr {
        Expr::UnaryIncRight(iexpr)
        | Expr::UnaryDecRight(iexpr)
        | Expr::LUnop(LeftUnaryOp::Inc | LeftUnaryOp::Dec, iexpr) => {
            if let Some(ityp) = check_lval(iexpr, sg)? {
                match ityp {
                    Typ::Address(_) => Ok(ityp),
                    Typ::Arith(atyp) => {
                        let after_promotion = atyp.promote().into();
                        // use of an operand of type ‘bool’ in ‘operator++’ is forbidden
                        match atyp {
                            ATyp::Float(_) => Ok(after_promotion),
                            ATyp::Int(_, _) => Ok(after_promotion),
                            ATyp::Bool => Err(TypeError::TypeIncompatible),
                        }
                    }
                    _ => Err(TypeError::TypeIncompatible),
                }
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Expr::Call(fexpr, pexprs) => {
            let atyps = pexprs
                .iter()
                .map(|e| check_expr(e, sg))
                .collect::<Result<Vec<Typ>, TypeError>>()?;
            if let Typ::FuncDecl(rettyp, ptyps) = check_expr(fexpr, sg)? {
                if atyps == ptyps {
                    Ok(*rettyp)
                } else {
                    Err(TypeError::TypeIncompatible)
                }
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        // arr[idx] is same with *(arr+idx)
        Expr::Index(aexpr, iexpr) => match check_expr(iexpr, sg)? {
            Typ::Arith(ATyp::Bool | ATyp::Int(_, _)) => {
                if let Typ::Array(e, _) = check_expr(aexpr, sg)? {
                    Ok(*e)
                } else {
                    Err(TypeError::TypeIncompatible)
                }
            }
            _ => Err(TypeError::TypeIncompatible),
        },
        Expr::LUnop(lunop, iexpr) => {
            let ityp = check_expr(iexpr, sg)?;

            match lunop {
                LeftUnaryOp::Plus | LeftUnaryOp::Minus => match ityp {
                    Typ::Arith(atyp) => Ok(Typ::Arith(atyp.promote())),
                    _ => Err(TypeError::TypeIncompatible),
                },
                LeftUnaryOp::BoolNot => {
                    if ityp.becomes(ATyp::Bool) {
                        Ok(ityp)
                    } else {
                        Err(TypeError::TypeIncompatible)
                    }
                }
                LeftUnaryOp::BitNot => match ityp {
                    Typ::Arith(ATyp::Int(_, _)) => Ok(ityp),
                    _ => Err(TypeError::TypeIncompatible),
                },
                LeftUnaryOp::Deref => Ok(Typ::Address(Box::new(ityp))),
                LeftUnaryOp::Ref => {
                    if let Typ::Address(i) = ityp {
                        Ok(*i)
                    } else {
                        Err(TypeError::TypeIncompatible)
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
                    (Typ::Arith(lat), Typ::Arith(rat)) => Ok((lat + rat).into()),
                    _ => Err(TypeError::TypeIncompatible),
                },
                // Only i % i
                BinOp::Mod => match (ltyp, rtyp) {
                    (Typ::Arith(lat @ ATyp::Int(_, _)), Typ::Arith(rat @ ATyp::Int(_, _))) => {
                        Ok((lat + rat).into())
                    }
                    _ => Err(TypeError::TypeIncompatible),
                },
                BinOp::Add | BinOp::Sub => match (ltyp, rtyp) {
                    // ARITH + ARITH
                    (Typ::Arith(lat), Typ::Arith(rat)) => Ok((lat + rat).into()),
                    // p+i | i+p
                    (Typ::Arith(ATyp::Int(_, _)), it @ Typ::Address(_))
                    | (it @ Typ::Address(_), Typ::Arith(ATyp::Int(_, _))) => Ok(it),
                    // arr+i | i+arr
                    (Typ::Arith(ATyp::Int(_, _)), at @ Typ::Array(_, _))
                    | (at @ Typ::Array(_, _), Typ::Arith(ATyp::Int(_, _))) => Ok(at),
                    _ => Err(TypeError::TypeIncompatible),
                },
                // Only i >> i, i << i
                BinOp::ShftL | BinOp::ShftR => match (ltyp, rtyp) {
                    (Typ::Arith(lat @ ATyp::Int(_, _)), Typ::Arith(rat @ ATyp::Int(_, _))) => {
                        Ok((lat + rat).into())
                    }
                    _ => Err(TypeError::TypeIncompatible),
                },
                BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge | BinOp::Eq | BinOp::Ne => {
                    if match (ltyp, rtyp) {
                        (Typ::Arith(ATyp::Int(_, _)), Typ::Arith(ATyp::Int(_, _))) => true,
                        (ptrtyp @ Typ::Address(_), arrtyp @ Typ::Array(_, _))
                        | (arrtyp @ Typ::Array(_, _), ptrtyp @ Typ::Address(_)) => {
                            arrtyp.becomes(&ptrtyp)
                        }
                        (Typ::Address(la), Typ::Address(ra)) => la == ra,
                        (Typ::Array(la, _), Typ::Array(ra, _)) => la == ra,
                        _ => false,
                    } {
                        Ok(ATyp::Bool.into())
                    } else {
                        Err(TypeError::TypeIncompatible)
                    }
                }
                BinOp::BitAnd | BinOp::BitXor | BinOp::BitOr => match (ltyp, rtyp) {
                    (Typ::Arith(lat @ ATyp::Int(_, _)), Typ::Arith(rat @ ATyp::Int(_, _))) => {
                        Ok((lat + rat).into())
                    }
                    _ => Err(TypeError::TypeIncompatible),
                },

                BinOp::BoolAnd | BinOp::BoolOr => {
                    if ltyp.becomes(ATyp::Bool) && rtyp.becomes(ATyp::Bool) {
                        Ok(ATyp::Bool.into())
                    } else {
                        Err(TypeError::TypeIncompatible)
                    }
                }
            }
        }
        Expr::Ternary(cond, tb, fb) => {
            if check_expr(cond, sg)?.becomes(ATyp::Bool) {
                let tt = check_expr(tb, sg)?;
                let ft = check_expr(fb, sg)?;
                if tt == ft {
                    Ok(tt)
                } else if tt.becomes(&ft) {
                    Ok(ft)
                } else if ft.becomes(&tt) {
                    Ok(tt)
                } else {
                    Err(TypeError::TypeIncompatible)
                }
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Expr::Assign(lexpr, assop, rexpr) => {
            if let Some(tgttyp) = check_lval(lexpr, sg)? {
                let assexpr = match assop.to_binop() {
                    Some(binop) => Expr::Binop(lexpr.clone(), binop, rexpr.clone()),
                    None => *rexpr.clone(),
                };
                let asstyp = check_expr(&assexpr, sg)?;
                if asstyp.becomes(&tgttyp) {
                    Ok(tgttyp)
                } else {
                    Err(TypeError::TypeIncompatible)
                }
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Expr::Comma(_, i) => check_expr(i, sg),
        Expr::Ident(nm) => sg.get_type(nm),
        c @ (Expr::Const(_) | Expr::ConstFloat(_) | Expr::ConstBool(_)) => {
            Ok(Typ::from_constexpr(c))
        }
        Expr::StringLiteral(_) => Ok(Typ::Address(Box::new(ATyp::Int(true, 1).into()))),
    }
}
