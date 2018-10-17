#[macro_use]
extern crate lazy_static;
extern crate rug;
#[macro_use]
extern crate rustler;

use rug::Float;
use rug::Integer;
use rug::ops::Pow;
use rustler::{Encoder, Env, NifResult, Term};
use rustler::resource::ResourceArc;
use std::sync::RwLock;

const RADIX_BASE: i32 = 32;

mod atoms {
    rustler_atoms! {
        atom ok;
        atom err;
        atom yes;
        atom no;
        atom vn1;
    }
}

rustler_export_nifs!(
    "eapa",
    [
        ("lxcode", 0, lxcode), // library version code
        ("float_to_bigint", 2, float_to_bigint), //float to bigint
        ("bigint_to_float", 2, bigint_to_float), //bigint to float
        ("bigint_add", 2, bigint_add), //bigint + bigint
        ("bigint_sub", 2, bigint_sub), //bigint - bigint
        ("bigint_mul", 3, bigint_mul), //bigint * bigint
        ("bigint_div", 3, bigint_div), //bigint / bigint
        ("bigint_min", 2, bigint_min), //min(bigint, bigint)
        ("bigint_max", 2, bigint_max), //min(bigint, bigint)
        ("bigint_lt",  2, bigint_lt),  //bigint < bigint
        ("bigint_lte", 2, bigint_lte), //bigint =< bigint
        ("bigint_gt",  2, bigint_gt),  //bigint > bigint
        ("bigint_gte", 2, bigint_gte), //bigint >= bigint
    ],
    Some(on_load)
);

fn on_load<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    true
}

fn lxcode<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    Ok((atoms::ok(), atoms::vn1()).encode(env))
}

fn float_to_bigint<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let src: String = args[0].decode()?;
    let p: u32 = args[1].decode()?;

    let valid = Float::parse(src);
    let f: Float = Float::with_val(128, valid.unwrap());

    let base: Integer = Integer::from(10);
    let res: Float = base.pow(p) * f;

    match res.to_integer() {
        Some(i) => Ok((i.to_string_radix(RADIX_BASE), p).encode(env)),
        None => {
            Ok((atoms::err()).encode(env))
        }
    }
}

fn bigint_to_float<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let src: String = args[0].decode()?;
    let p: u32 = args[1].decode()?;

    let valid = Float::parse("1.0");
    let t: Float = Float::with_val(128, valid.unwrap());

    let i = Integer::from_str_radix(src.to_string().as_str(), RADIX_BASE).unwrap() * t;
    let base: Integer = Integer::from(10);
    let res: Float = i / base.pow(p);

    let r: f64 = res.to_string().parse().unwrap();

    Ok((format!("{}", r)).encode(env))
}

fn bigint_add<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();
    let res = x1int + x2int;

    Ok((res.to_string_radix(RADIX_BASE)).encode(env))
}

fn bigint_sub<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();
    let res = x1int - x2int;

    Ok((res.to_string_radix(RADIX_BASE)).encode(env))
}

fn bigint_mul<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let src1: String = args[0].decode()?;
    let src2: String = args[1].decode()?;
    let p: u32 = args[2].decode()?;

    let i1 = Integer::from_str_radix(src1.to_string().as_str(), RADIX_BASE).unwrap();
    let i2 = Integer::from_str_radix(src2.to_string().as_str(), RADIX_BASE).unwrap();
    let base: Integer = Integer::from(10);
    let mul = i1 * i2;
    let res = mul / base.pow(p);

    Ok((res.to_string_radix(RADIX_BASE)).encode(env))
}

fn bigint_div<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let src1: String = args[0].decode()?;
    let src2: String = args[1].decode()?;
    let p: u32 = args[2].decode()?;

    let i1 = Integer::from_str_radix(src1.to_string().as_str(), RADIX_BASE).unwrap();
    let i2 = Integer::from_str_radix(src2.to_string().as_str(), RADIX_BASE).unwrap();
    let base: Integer = Integer::from(10);
    let res = (i1 * base.pow(p)) / i2;

    Ok((res.to_string_radix(RADIX_BASE)).encode(env))
}

fn bigint_max<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();

    let res = x1int.max(x2int);

    Ok((res.to_string_radix(RADIX_BASE)).encode(env))
}

fn bigint_min<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();

    let res = x1int.min(x2int);

    Ok((res.to_string_radix(RADIX_BASE)).encode(env))
}

fn bigint_gt<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();

    if x1int > x2int {
        Ok((atoms::yes()).encode(env))
    } else {
        Ok((atoms::no()).encode(env))
    }
}

fn bigint_gte<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();

    if x1int >= x2int {
        Ok((atoms::yes()).encode(env))
    } else {
        Ok((atoms::no()).encode(env))
    }
}

fn bigint_lt<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();

    if x1int < x2int {
        Ok((atoms::yes()).encode(env))
    } else {
        Ok((atoms::no()).encode(env))
    }
}

fn bigint_lte<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x1: String = args[0].decode()?;
    let x2: String = args[1].decode()?;

    let x1int = Integer::from_str_radix(x1.to_string().as_str(), RADIX_BASE).unwrap();
    let x2int = Integer::from_str_radix(x2.to_string().as_str(), RADIX_BASE).unwrap();

    if x1int <= x2int {
        Ok((atoms::yes()).encode(env))
    } else {
        Ok((atoms::no()).encode(env))
    }
}