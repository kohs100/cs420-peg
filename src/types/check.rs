use crate::ast::expr::*;
use crate::ast::types::*;

use crate::types::check_expr::check_expr;
use crate::types::defs::*;
use crate::types::scope::ScopeGuard;

use crate::types::defs::ATyp::*;
use crate::types::defs::Typ::*;

fn check_stmt_inner(stmt: &Stmt, sg: &ScopeGuard) -> TypeResult<()> {
    use Stmt::*;
    let _res: () = match stmt {
        CompoundStmt(cstmt, _) => {
            check_cstmt(cstmt, sg, "cstmt")?;
        }
        ExprStmt(None, _) => (),
        ExprStmt(Some(expr), _) => {
            check_expr(expr, &sg).map(|_| ())?;
        }
        If(cexpr, stmt, _) => {
            check_expr(cexpr, &sg)?.if_bool(check_stmt(stmt, sg)?)?;
        }
        IfElse(cexpr, tstmt, fstmt, _) => {
            check_expr(cexpr, &sg)?.if_bool({
                check_stmt(tstmt, sg)?;
                check_stmt(fstmt, sg)?;
            })?;
        }
        While(cexpr, stmt, _) => {
            check_expr(cexpr, &sg)?.if_bool(check_stmt(stmt, sg)?)?;
        }
        DoWhile(stmt, cexpr, _) => {
            check_expr(cexpr, &sg)?.if_bool(check_stmt(stmt, sg)?)?;
        }
        Continue(_) => (),
        Break(_) => (),
        Return(opt_expr, _) => {
            let rettyp = sg.get_rettyp();
            let valtyp = if let Some(expr) = opt_expr {
                check_expr(expr, sg)?
            } else {
                Void
            };
            if !valtyp.becomes(rettyp) {
                return Err(TypeError::TypeMismatch(
                    "Return type mismatch".to_owned(),
                    rettyp.clone(),
                    valtyp,
                )
                .into());
            };
        }
        For(iexpr, cexpr, rexpr, stmt, _) => {
            if let Some(expr) = iexpr {
                check_expr(expr, sg)?;
            }
            if let Some(expr) = cexpr {
                if !check_expr(expr, sg)?.becomes(Bool) {
                    return Err(TypeError::TypeIncompatible.into());
                }
            }
            if let Some(expr) = rexpr {
                check_expr(expr, sg)?;
            }
            check_stmt(stmt, sg)?;
        }
    };
    Ok(())
}

fn check_stmt(stmt: &Stmt, sg: &ScopeGuard) -> TypeResult<()> {
    let res = match check_stmt_inner(stmt, sg) {
        Ok(i) => Ok(i),
        Err(mut err) => {
            err.at(stmt.clone());
            Err(err)
        }
    };
    res
}

fn check_cstmt(
    cstmt: &CompoundStmt,
    outer_sg: &ScopeGuard,
    scope_nm: &'static str,
) -> TypeResult<()> {
    check_cstmt_with_sg(cstmt, outer_sg.scope_in(scope_nm))
}

fn check_cstmt_with_sg(cstmt: &CompoundStmt, mut use_this_sg: ScopeGuard) -> TypeResult<()> {
    let CompoundStmt(decls, stmts) = cstmt;

    for decln in decls {
        check_declaration(decln, &mut use_this_sg)?;
    }

    for stmt in stmts {
        check_stmt(stmt, &use_this_sg)?;
    }
    Ok(())
}

fn check_init(init: &Initializer, tgt: &Typ, sg: &ScopeGuard) -> TypeResult<()> {
    let res = match init {
        Initializer::Array(inits) => {
            if let Array(ityp, sz) = tgt {
                if inits.len() > *sz as usize {
                    Err(TypeError::InvalidDefinition("Too many initial values".to_owned()).into())
                } else {
                    for init in inits.iter() {
                        check_init(init, ityp, sg)?;
                    }
                    Ok(())
                }
            } else {
                Err(TypeError::InvalidDefinition("Invalid array initialization".to_owned()).into())
            }
        }
        Initializer::Scala(expr) => {
            let typ = check_expr(expr, sg)?;
            let tgt = tgt.clone();
            if typ == tgt {
                Ok(())
            } else {
                Err(TypeError::TypeMismatch("".to_owned(), tgt, typ).into())
            }
        }
    };
    res
}

fn check_decl(ptyp: &PrimType, decl: &Declarator, sg: &mut ScopeGuard) -> TypeResult<()> {
    let nm = decl.name();
    let typ = decl.to_typ(ptyp);
    use Declarator::*;
    match decl {
        Init(_, _, init) => {
            check_init(init, &typ, &sg)?;
            sg.declare(nm, typ)?;
        }
        NoInit(_, _) => {
            sg.declare(nm, typ)?;
        }
    };
    Ok(())
}

fn check_declaration(decln: &Declaration, sg: &mut ScopeGuard) -> TypeResult<()> {
    let Declaration(ptype, decls, _) = decln;

    let mut res = Ok(());
    for decl in decls {
        if let Err(mut err) = check_decl(ptype, decl, sg) {
            err.at(decln.clone());
            res = Err(err);
            break;
        }
    }
    res
}

fn check_tu(tu: &TranslationUnit, root_sg: &mut ScopeGuard) -> TypeResult<()> {
    use TranslationUnit::*;
    match tu {
        Glob(decln) => check_declaration(decln, root_sg),
        Func(Function(ptype, decl, cstmt)) => {
            let func_nm = decl.name();
            let func_typ = decl.to_typ(ptype);

            if let Some(etyp) = root_sg.get_type(func_nm) {
                if etyp == func_typ {
                } else {
                    return Err(TypeError::TypeMismatch(
                        "Incompatible prototype".to_owned(),
                        etyp,
                        func_typ,
                    )
                    .into());
                }
            } else {
                root_sg.declare(func_nm, func_typ.clone())?;
            }

            if let Declarator::NoInit(_, ddbox) = decl {
                if let DirectDeclarator::Func(_, pdds) = ddbox.as_ref() {
                    let params: Vec<(Typ, Option<&String>)> = pdds
                        .iter()
                        .map(|pdd| match pdd {
                            ParamDeclaration::Abstract(ptype, decl) => (decl.to_typ(ptype), None),
                            ParamDeclaration::Named(ptype, decl) => {
                                (decl.to_typ(ptype), Some(decl.name()))
                            }
                        })
                        .collect();

                    let mut inner_sg = root_sg.scope_in(func_nm);
                    if let FuncDecl(rettyp, _) = func_typ {
                        inner_sg.set_rettyp(*rettyp);
                        for (ptype, pname) in params.into_iter() {
                            if let Some(pname) = pname {
                                inner_sg.declare(&pname, ptype)?;
                            }
                        }
                        check_cstmt_with_sg(cstmt, inner_sg)?;
                        Ok(())
                    } else {
                        Err(
                            TypeError::InvalidDefinition(format!("Must be function: {}", func_nm))
                                .into(),
                        )
                    }
                } else {
                    Err(
                        TypeError::InvalidDefinition(format!("Must be function: {}", func_nm))
                            .into(),
                    )
                }
            } else {
                Err(
                    TypeError::InvalidDefinition(format!("Cannot be init declarator: {}", func_nm))
                        .into(),
                )
            }
        }
    }
}

pub fn check(tus: &Vec<TranslationUnit>) -> TypeResult<()> {
    let mut root_sg = ScopeGuard::new("root", false);
    for tu in tus {
        check_tu(tu, &mut root_sg)?;
    }
    Ok(())
}
