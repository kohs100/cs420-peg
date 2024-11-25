use crate::ast::*;

use crate::types::check_expr::check_expr;
use crate::types::defs::*;
use crate::types::scope::ScopeGuard;

fn check_stmt(stmt: &Stmt, sg: &ScopeGuard) -> TypeResult<()> {
    match stmt {
        Stmt::CompoundStmt(cstmt) => check_cstmt(cstmt, sg, "cstmt"),
        Stmt::ExprStmt(None) => Err(TypeError::TypeIncompatible),
        Stmt::ExprStmt(Some(expr)) => check_expr(expr, &sg).map(|_| ()),
        Stmt::If(cexpr, stmt) => {
            if check_expr(cexpr, &sg)?.becomes(ATyp::Bool) {
                check_stmt(stmt, sg)
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Stmt::IfElse(cexpr, tstmt, fstmt) => {
            if check_expr(cexpr, &sg)?.becomes(ATyp::Bool) {
                check_stmt(tstmt, sg)?;
                check_stmt(fstmt, sg)
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Stmt::While(cexpr, stmt) => {
            if check_expr(cexpr, &sg)?.becomes(ATyp::Bool) {
                check_stmt(stmt, sg)
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Stmt::DoWhile(stmt, cexpr) => {
            if check_expr(cexpr, &sg)?.becomes(ATyp::Bool) {
                check_stmt(stmt, sg)
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Stmt::Continue => Ok(()),
        Stmt::Break => Ok(()),
        Stmt::Return(opt_expr) => {
            let rettyp = sg.get_rettyp();
            let valtyp = if let Some(expr) = opt_expr {
                check_expr(expr, sg)?
            } else {
                Typ::Void
            };
            if valtyp.becomes(rettyp) {
                Ok(())
            } else {
                Err(TypeError::TypeIncompatible)
            }
        }
        Stmt::For(iexpr, cexpr, rexpr, stmt) => {
            if let Some(expr) = iexpr {
                check_expr(expr, sg)?;
            }
            if let Some(expr) = cexpr {
                if !check_expr(expr, sg)?.becomes(ATyp::Bool) {
                    return Err(TypeError::TypeIncompatible);
                }
            }
            if let Some(expr) = rexpr {
                check_expr(expr, sg)?;
            }
            check_stmt(stmt, sg)
        }
    }
}

fn check_cstmt(
    cstmt: &CompoundStmt,
    outer_sg: &ScopeGuard,
    reason: &'static str,
) -> TypeResult<()> {
    check_cstmt_with_sg(cstmt, outer_sg.scope_in(reason))
}

fn check_cstmt_with_sg(cstmt: &CompoundStmt, mut use_this_sg: ScopeGuard) -> TypeResult<()> {
    let CompoundStmt(decls, stmts) = cstmt;

    for decln in decls {
        let Declaration(ptype, decls) = decln;
        for decl in decls {
            check_decl(ptype, decl, &mut use_this_sg)?;
        }
    }

    for stmt in stmts {
        check_stmt(stmt, &use_this_sg)?;
    }
    Ok(())
}

fn check_init(init: &Initializer, tgt: &Typ, sg: &ScopeGuard) -> TypeResult<()> {
    match init {
        Initializer::Array(inits) => {
            if let Typ::Array(ityp, sz) = tgt {
                if inits.len() > *sz as usize {
                    Err(TypeError::InvalidDefinition(
                        "Too many initial values".to_owned(),
                    ))
                } else {
                    for init in inits.iter() {
                        check_init(init, ityp, sg)?;
                    }
                    Ok(())
                }
            } else {
                Err(TypeError::InvalidDefinition(
                    "Invalid array initialization".to_owned(),
                ))
            }
        }
        Initializer::Scala(expr) => {
            let typ = check_expr(expr, sg)?;
            let tgt = tgt.clone();
            if typ == tgt {
                Ok(())
            } else {
                Err(TypeError::TypeMismatch(tgt, typ))
            }
        }
    }
}

fn check_decl(ptyp: &PrimType, decl: &Declarator, sg: &mut ScopeGuard) -> TypeResult<()> {
    let nm = decl.name();
    let typ = Typ::from_decl(ptyp, decl);
    match decl {
        Declarator::Init(_, _, init) => {
            check_init(init, &typ, &sg)?;
            sg.declare(nm, typ)?;
        }
        Declarator::NoInit(_, _) => {
            sg.declare(nm, typ)?;
        }
    };
    Ok(())
}

fn check_tu(tu: &TranslationUnit, root_sg: &mut ScopeGuard) -> TypeResult<()> {
    match tu {
        TranslationUnit::Glob(Declaration(typ, decls)) => {
            for decl in decls {
                check_decl(typ, decl, root_sg)?;
            }
            Ok(())
        }
        TranslationUnit::Func(Function(ptype, decl, cstmt)) => {
            let func_nm = decl.name();
            let func_typ = Typ::from_decl(ptype, decl);

            if let Declarator::NoInit(_, DirectDeclarator::Func(_, pdds)) = decl {
                let params: Vec<(Typ, &String)> = pdds
                    .iter()
                    .map(|pdd| {
                        let ParamDeclaration(ptype, decl) = pdd;
                        (Typ::from_decl(ptype, decl), decl.name())
                    })
                    .collect();

                // Declare function name for recursion
                root_sg.declare(func_nm, func_typ.clone())?;
                let mut inner_sg = root_sg.scope_in(func_nm);

                if let Typ::FuncDecl(rettyp, _) = func_typ {
                    inner_sg.set_rettyp(*rettyp);
                    for (ptype, pname) in params.into_iter() {
                        inner_sg.declare(&pname, ptype)?;
                    }

                    check_cstmt_with_sg(cstmt, inner_sg)?;
                    Ok(())
                } else {
                    Err(TypeError::InvalidDefinition(func_nm.to_owned()))
                }
            } else {
                Err(TypeError::InvalidDefinition(func_nm.to_owned()))
            }
        }
    }
}

pub fn check(tus: &Vec<TranslationUnit>) -> TypeResult<()> {
    let mut root_sg = ScopeGuard::new("root", true);
    for tu in tus {
        check_tu(tu, &mut root_sg)?;
    }
    Ok(())
}
