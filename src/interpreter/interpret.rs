use std::cell::RefCell;
use std::rc::Rc;

use super::allocator::*;
use super::value::*;
use super::vptr::*;
use crate::ast::expr::*;
use crate::ast::types::*;

use super::defs::*;
use super::runtime::*;

use super::IpretError::{Misc, MiscOwned};
use super::IpretResult;

fn do_incdec<T: Allocator + VirtualMemory>(
    operand: &Expr,
    return_prev: bool,
    is_inc: bool,
    sg: &mut StackGuard<T>,
) -> IpretResult<Value> {
    if let Some(mo) = interp_expr(operand, sg)?.unwrap_memobj() {
        let val = mo.load(sg);

        let val_incdec = if is_inc {
            val.clone().add(InterValue::from_const_int(1))?
        } else {
            val.clone().sub(InterValue::from_const_int(1))?
        };

        Ok(if return_prev {
            mo.store(sg, val_incdec.cast_into(&mo.typ, false)?);
            val.into()
        } else {
            mo.store(sg, val_incdec.clone());
            val_incdec.into()
        })
    } else {
        Err(Misc("Unary Inc/dec with non-lvalue"))
    }
}

fn interp_malloc<T: Allocator + VirtualMemory>(
    args: &Vec<Box<Expr>>,
    sg: &mut StackGuard<T>,
) -> IpretResult<Value> {
    if args.len() != 1 {
        Err(MiscOwned(format!(
            "Invalid malloc call with arguments: {:?}",
            args
        )))
    } else {
        let arg = &args[0];
        let sz_val = interp_expr(arg.as_ref(), sg)?.unwrap_value(sg);
        let (is_neg, abs) = sz_val.iabs()?;
        if is_neg {
            Err(MiscOwned(format!(
                "Invalid malloc call with size: {:?}",
                sz_val
            )))
        } else {
            let ptr: AddrType = sg.get_runtime().borrow_mut().alloc_heap(abs).into();
            Ok(InterValue::from_type(Typ::Address(Box::new(Typ::Void)), ptr).into())
        }
    }
}

fn interp_free<T: Allocator + VirtualMemory>(
    args: &Vec<Box<Expr>>,
    sg: &mut StackGuard<T>,
) -> IpretResult<Value> {
    if args.len() != 1 {
        Err(MiscOwned(format!(
            "Invalid free call with arguments: {:?}",
            args
        )))
    } else {
        let arg = &args[0];
        let sz_val = interp_expr(arg.as_ref(), sg)?.unwrap_value(sg);
        match &sz_val.typ {
            Typ::Address(_) => {
                let ptr = sz_val.get_as::<AddrType>();
                sg.get_runtime()
                    .borrow_mut()
                    .free_heap(VirtualPointer::with(ptr));
                Ok(InterValue::void().into())
            }
            _ => Err(MiscOwned(format!("Invalid free call with arg: {:?}", arg))),
        }
    }
}

fn interp_printf<T: Allocator + VirtualMemory>(
    args: &Vec<Box<Expr>>,
    sg: &mut StackGuard<T>,
) -> IpretResult<Value> {
    if args.len() < 2 {
        Err(MiscOwned(format!(
            "Invalid printf call with arguments: {:?}",
            args
        )))
    } else {
        let fstr = match args[0].as_ref() {
            Expr::StringLiteral(fstr) => fstr.clone(),
            _ => {
                return Err(MiscOwned(format!(
                    "First argument of printf must be string literal."
                )))
            }
        };
        let args = &args[1..];
        let args_val = args
            .iter()
            .map(|arg| Ok(interp_expr(arg, sg)?.unwrap_value(sg)))
            .collect::<IpretResult<Vec<InterValue>>>()?;
        let args_lst: Vec<String> = args_val.into_iter().map(|iv| format!("{}", iv)).collect();
        println!("printf(\"{}\", {})", fstr, args_lst.join(","));
        Ok(InterValue::void().into())
    }
}

fn interp_hook_function<T: Allocator + VirtualMemory>(
    fname: &str,
    args: &Vec<Box<Expr>>,
    sg: &mut StackGuard<T>,
) -> Option<IpretResult<Value>> {
    match fname {
        "printf" => Some(interp_printf(args, sg)),
        "malloc" => Some(interp_malloc(args, sg)),
        "free" => Some(interp_free(args, sg)),
        _ => None,
    }
}

fn interp_expr<T: Allocator + VirtualMemory>(
    input_expr: &Expr,
    sg: &mut StackGuard<T>,
) -> IpretResult<Value> {
    let res = match input_expr {
        Expr::UnaryDecRight(iexpr) => do_incdec(iexpr, true, false, sg),
        Expr::UnaryIncRight(iexpr) => do_incdec(iexpr, true, true, sg),
        Expr::Call(fexpr, args) => {
            if let Expr::Ident(fname) = fexpr.as_ref() {
                // println!("Calling function {}", fname);
                if let Some(hooked) = interp_hook_function(fname, args, sg) {
                    hooked
                } else {
                    let func = sg.lookup_func(fname);
                    if let Some(func) = func {
                        Ok(interp_function(
                            &func,
                            args.into_iter()
                                .map(|arg| {
                                    let res = interp_expr(arg, sg)?;
                                    Ok(res.unwrap_value(sg))
                                })
                                .collect::<IpretResult<Vec<InterValue>>>()?,
                            sg.get_runtime(),
                        )?
                        .into())
                    } else {
                        Err(MiscOwned(format!("No function named {}!!", fname)))
                    }
                }
            } else {
                Err(Misc("Only function identifier is allowed in call context."))
            }
        }
        Expr::Index(ptr_expr, idx_expr) => {
            let ptr_v = interp_expr(ptr_expr.as_ref(), sg)?.unwrap_value(sg);
            let idx_v = interp_expr(idx_expr.as_ref(), sg)?.unwrap_value(sg);

            let res_typ = match &ptr_v.typ {
                Typ::Address(innertyp) => *innertyp.clone(),
                Typ::Array(innertyp, _) => *innertyp.clone(),
                _ => {
                    return Err(MiscOwned(format!(
                        "Invalid type for index operator: {}",
                        ptr_v.typ
                    )))
                }
            };

            let res = ptr_v.add(idx_v)?;
            let res_addr = VirtualPointer::with(res.get_as::<AddrType>());

            Ok(Value::OnMemory(MemObj::new(res_typ, res_addr)))
        }
        Expr::LUnop(lunop, operand) => match lunop {
            LeftUnaryOp::Inc => do_incdec(operand, false, false, sg),
            LeftUnaryOp::Dec => do_incdec(operand, false, true, sg),
            LeftUnaryOp::Plus => {
                let val = interp_expr(operand, sg)?.unwrap_value(sg);
                Ok(val.promote()?.into())
            }
            LeftUnaryOp::Minus => {
                let val = interp_expr(operand, sg)?.unwrap_value(sg);
                Ok(val.complement()?.into())
            }
            LeftUnaryOp::BoolNot => {
                let val = interp_expr(operand, sg)?
                    .unwrap_value(sg)
                    .cast_into(&Typ::Arith(ATyp::Bool), false)?;
                let val = !val.get_as::<bool>();
                Ok(InterValue::from_const_bool(val).into())
            }
            LeftUnaryOp::BitNot => {
                let val = interp_expr(operand, sg)?.unwrap_value(sg).bitneg()?;
                Ok(val.into())
            }
            LeftUnaryOp::Deref => {
                let mo = interp_expr(operand, sg)?.unwrap_value(sg).deref()?;
                Ok(mo.into())
            }
            LeftUnaryOp::Ref => {
                if let Some(mo) = interp_expr(operand, sg)?.unwrap_memobj() {
                    Ok(mo.clone().into())
                } else {
                    Err(Misc("Cannot reference non-lvalue"))
                }
            }
        },
        Expr::Cast(tnm, expr) => {
            let TypeName(ptyp, adecl) = tnm;
            let dst_typ = adecl.to_typ(ptyp);
            let val = interp_expr(expr.as_ref(), sg)?
                .unwrap_value(sg)
                .cast_into(&dst_typ, true)?;
            Ok(val.into())
        }
        Expr::Binop(lexpr, binop, rexpr) => {
            let lval = interp_expr(lexpr, sg)?.unwrap_value(sg);
            let rval = interp_expr(rexpr, sg)?.unwrap_value(sg);
            let res = match binop {
                BinOp::Mul => lval.mul(rval),
                BinOp::Div => lval.div(rval),
                BinOp::Mod => lval.modulo(rval),
                BinOp::Add => lval.add(rval),
                BinOp::Sub => lval.sub(rval),
                BinOp::ShftL => lval.shftl(rval),
                BinOp::ShftR => lval.shftr(rval),
                BinOp::Lt => lval.lt(rval),
                BinOp::Le => lval.le(rval),
                BinOp::Gt => lval.gt(rval),
                BinOp::Ge => lval.ge(rval),
                BinOp::Eq => lval.eq(rval),
                BinOp::Ne => lval.ne(rval),
                BinOp::BitAnd => lval.bitand(rval),
                BinOp::BitXor => lval.bitxor(rval),
                BinOp::BitOr => lval.bitor(rval),
                BinOp::BoolAnd => lval.booland(rval),
                BinOp::BoolOr => lval.boolor(rval),
            };
            Ok(res?.into())
        }
        Expr::Ternary(cexpr, texpr, fexpr) => {
            let cval = interp_expr(cexpr, sg)?.unwrap_value(sg).get_as::<bool>();
            interp_expr(if cval { texpr } else { fexpr }, sg)
        }
        Expr::Assign(lexpr, assop, rexpr) => {
            if let Some(mo) = interp_expr(lexpr, sg)?.unwrap_memobj() {
                let rval = interp_expr(rexpr, sg)?
                    .unwrap_value(sg)
                    .cast_into(&mo.typ, false)?;
                let lval = mo.load(sg);

                let res = match assop {
                    AssOp::Non => Ok(rval),
                    AssOp::Mul => lval.mul(rval),
                    AssOp::Div => lval.div(rval),
                    AssOp::Mod => lval.modulo(rval),
                    AssOp::Add => lval.add(rval),
                    AssOp::Sub => lval.sub(rval),
                    AssOp::ShftL => lval.shftl(rval),
                    AssOp::ShftR => lval.shftr(rval),
                    AssOp::BitAnd => lval.bitand(rval),
                    AssOp::BitXor => lval.bitxor(rval),
                    AssOp::BitOr => lval.bitor(rval),
                }?
                .cast_into(&mo.typ, false)?;

                mo.store(sg, res);
                Ok(mo.clone().into())
            } else {
                Err(MiscOwned(format!(
                    "Cannot assign to non-lvalue expression: {}",
                    lexpr
                )))
            }
        }
        Expr::Comma(expr, nxt_expr) => {
            interp_expr(expr, sg)?;
            interp_expr(nxt_expr, sg)
        }
        Expr::Ident(nm) => Ok(sg
            .lookup(nm)
            .expect(&format!("Invalid identifier: {}", nm))
            .into()),
        Expr::ConstBool(val) => Ok(InterValue::from_const_bool(*val).into()),
        Expr::Const(val) => Ok(InterValue::from_const_int((*val).try_into().unwrap()).into()),
        Expr::ConstFloat(sval) => Ok(InterValue::from_float_str(sval).into()),
        Expr::StringLiteral(sval) => Ok(sg.get_runtime().borrow_mut().string_literal(sval).into()),
    }?;

    // println!(
    //     "EXPR {:>20} => {:?}",
    //     format!("{}", input_expr),
    //     res.unwrap_value(sg)
    // );

    Ok(res)
}

fn interp_function<T: Allocator + VirtualMemory>(
    func: &Function,
    args: Vec<InterValue>,
    rt: Rc<RefCell<Runtime<T>>>,
) -> IpretResult<InterValue> {
    let mut sg = StackGuard::from_runtime(rt.clone());
    let Function(ptyp, decl, cstmt) = func;

    let ftyp = decl.to_typ(ptyp);
    let fnm = decl.name();

    let rettyp = if let Typ::FuncDecl(rettyp, _) = ftyp {
        rettyp
    } else {
        return Err(Misc("Invalid function type"));
    };

    let mut repr_args: Vec<String> = Vec::new();

    if let Declarator::NoInit(_, dd) = decl {
        if let DirectDeclarator::Func(_, pds) = dd.as_ref() {
            assert_eq!(pds.len(), args.len());
            for ((param_typ, param_nm), arg) in
                pds.iter().map(|pd| pd.typ_and_name()).zip(args.into_iter())
            {
                let argval = arg.cast_into(&param_typ, false)?;
                repr_args.push(format!("{}", argval));

                if let Some(param_nm) = param_nm {
                    sg.grow_stk(param_nm, param_typ);
                    let mobj = sg.lookup(param_nm).unwrap();
                    mobj.store(&mut *rt.borrow_mut(), argval);
                }
            }
        }
    }

    println!("{}({})", fnm, repr_args.join(", "));

    match interp_cstmt(cstmt, sg)? {
        RewindContext::Returning(retval) => retval.cast_into(&rettyp, false),
        RewindContext::None => InterValue::void().cast_into(&rettyp, false),
        rwc => return Err(MiscOwned(format!("Invalid rewind context: {:?}", rwc))),
    }
}

#[derive(Debug, Clone)]
enum RewindContext {
    None,
    Returning(InterValue),
    Break,
    Continue,
}
impl RewindContext {
    fn is_rewind(&self) -> bool {
        match self {
            Self::None => false,
            _ => true,
        }
    }
}

fn interp_stmt<T: Allocator + VirtualMemory>(
    stmt: &Stmt,
    sg: &mut StackGuard<T>,
) -> IpretResult<RewindContext> {
    // println!("Interpreting stmt: {}", stmt);
    match stmt {
        Stmt::CompoundStmt(cstmt, _) => {
            let rwc = interp_cstmt(cstmt, sg.scope_in())?;
            if rwc.is_rewind() {
                return Ok(rwc);
            }
        }
        Stmt::ExprStmt(oexpr, _) => {
            if let Some(expr) = oexpr {
                interp_expr(&expr, sg)?;
            }
        }
        Stmt::If(cexpr, stmt, _) => {
            let v = interp_expr(&cexpr, sg)?
                .unwrap_value(sg)
                .cast_into(&ATyp::Bool.into(), false)?
                .get_as::<bool>();
            if v {
                let rwc = interp_stmt(stmt, sg)?;
                if rwc.is_rewind() {
                    return Ok(rwc);
                }
            }
        }
        Stmt::IfElse(cexpr, tstmt, fstmt, _) => {
            let v = interp_expr(&cexpr, sg)?
                .unwrap_value(sg)
                .cast_into(&ATyp::Bool.into(), false)?
                .get_as::<bool>();
            let rwc = interp_stmt(if v { tstmt } else { fstmt }, sg)?;
            if rwc.is_rewind() {
                return Ok(rwc);
            }
        }
        Stmt::While(cexpr, stmt, _) => loop {
            let v = interp_expr(&cexpr, sg)?
                .unwrap_value(sg)
                .cast_into(&ATyp::Bool.into(), false)?
                .get_as::<bool>();
            if v {
                match interp_stmt(stmt, sg)? {
                    RewindContext::Break => break,
                    RewindContext::Continue => continue,
                    rwc @ RewindContext::Returning(_) => return Ok(rwc),
                    RewindContext::None => (),
                }
            }
        },
        Stmt::DoWhile(stmt, cexpr, _) => loop {
            match interp_stmt(stmt, sg)? {
                RewindContext::Break => break,
                RewindContext::Continue => continue,
                rwc @ RewindContext::Returning(_) => return Ok(rwc),
                RewindContext::None => (),
            }
            if !interp_expr(&cexpr, sg)?
                .unwrap_value(sg)
                .cast_into(&ATyp::Bool.into(), false)?
                .get_as::<bool>()
            {
                break;
            }
        },
        Stmt::Continue(_) => return Ok(RewindContext::Continue),
        Stmt::Break(_) => return Ok(RewindContext::Break),
        Stmt::Return(retval, _) => {
            return Ok(RewindContext::Returning({
                if let Some(retexpr) = retval.as_ref() {
                    let retval = interp_expr(retexpr, sg)?.unwrap_value(sg);
                    retval
                } else {
                    InterValue::void()
                }
            }))
        }
        Stmt::For(iexpr, cexpr, rexpr, stmt, _) => {
            if let Some(iexpr) = iexpr {
                interp_expr(iexpr, sg)?;
            }
            loop {
                if let Some(cexpr) = cexpr {
                    let v = interp_expr(cexpr, sg)?
                        .unwrap_value(sg)
                        .cast_into(&ATyp::Bool.into(), false)?
                        .get_as::<bool>();
                    if !v {
                        break;
                    }
                }
                interp_stmt(stmt, sg)?;
                if let Some(rexpr) = rexpr {
                    interp_expr(rexpr, sg)?;
                }
            }
        }
    };
    Ok(RewindContext::None)
}

fn interp_cstmt<T: Allocator + VirtualMemory>(
    cstmt: &CompoundStmt,
    mut sg: StackGuard<T>,
) -> IpretResult<RewindContext> {
    // println!("Interpreting cstmt: {}", cstmt);
    let CompoundStmt(declns, stmts) = cstmt;

    for Declaration(ptyp, decls, _) in declns {
        for decl in decls {
            let typ = decl.to_typ(ptyp);
            let name = decl.name().clone();
            sg.grow_stk(&name, typ);

            if let Declarator::Init(_, _, init) = decl {
                interp_declaration(&name, init, &mut sg)?
            }
        }
    }
    for stmt in stmts {
        let rwc = interp_stmt(stmt, &mut sg)?;
        if rwc.is_rewind() {
            return Ok(rwc);
        }
    }
    // println!("Done Interpreting cstmt: {}", cstmt);
    Ok(RewindContext::None)
}

fn interp_declaration<T: Allocator + VirtualMemory>(
    name: &str,
    init: &Initializer,
    sg: &mut StackGuard<T>,
) -> IpretResult<()> {
    // println!("Interpreting declaration of variable: {}", name);
    match init {
        Initializer::Scala(iexpr) => {
            let mo = sg.lookup(&name).unwrap();
            let ival = interp_expr(iexpr.as_ref(), sg)?
                .unwrap_value(sg)
                .cast_into(&mo.typ, false)?;
            mo.store(sg, ival)
        }
        Initializer::Array(_) => {
            unimplemented!("Array initialization is not supported.")
        }
    }
    Ok(())
}

pub fn interp_src(src: &Source) -> IpretResult<()> {
    let Source(tus) = src;

    let prt = {
        let mut prt = PreRuntime::<LinearAllocator>::new();

        for tu in tus {
            match tu {
                TranslationUnit::Glob(decl) => {
                    let Declaration(ptyp, decls, _) = decl;
                    for decl in decls {
                        let name = decl.name();
                        let typ = decl.to_typ(ptyp);
                        prt.alloc_glob(name, &typ);
                    }
                }
                TranslationUnit::Func(func) => {
                    let Function(_, decl, _) = func;
                    let name = decl.name();
                    prt.alloc_func(name, func);
                }
            }
        }

        let mut sg = StackGuard::from_preruntime(prt);

        // Initialize global declarations
        for tu in tus {
            match tu {
                TranslationUnit::Glob(decln) => {
                    let Declaration(_, decls, _) = decln;
                    for decl in decls {
                        let name = decl.name().clone();

                        if let Declarator::Init(_, _, init) = decl {
                            interp_declaration(&name, init, &mut sg)?
                        }
                    }
                }
                _ => (),
            }
        }

        sg.extract_prt()
    };

    const DEFAULT_STK_SIZE: u64 = 1u64 << 22; // 4MB
    let rt = Rc::new(RefCell::new(Runtime::new(prt, DEFAULT_STK_SIZE)));

    let omain = rt.clone().borrow().lookup_func("main").cloned();

    if let Some(main_func) = omain {
        let retcode = interp_function(&main_func, vec![], rt)?;
        println!("Process exited with retcode {:?}", retcode);
        Ok(())
    } else {
        Err(Misc("Main function not found!!"))
    }
}
