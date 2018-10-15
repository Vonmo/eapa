#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler;

use rustler::{Encoder, Env, NifResult, Term};
use rustler::resource::ResourceArc;
use std::sync::RwLock;

mod atoms {
    rustler_atoms! {
        atom ok;
        atom vn1;
    }
}

rustler_export_nifs!(
    "eapa",
    [
        ("lxcode", 0, lxcode), // library version code
    ],
    Some(on_load)
);

fn on_load<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    true
}

fn lxcode<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    Ok((atoms::ok(), atoms::vn1()).encode(env))
}