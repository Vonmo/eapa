-module(eapa_int_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, int}
  ].

groups() ->
  [
    {int,
      [parallel, shuffle],
      [float_to_bigint,
        add, add_prec1, add_prec2,
        sub, sub_prec1, sub_prec2,
        mul, mul_prec1, mul_prec2,
        divp, divp_prec1, divp_prec2,
        min, min_prec1, min_prec2,
        max, max_prec1, max_prec2,
        lt, lt_prec1, lt_prec2,
        lte, lte_prec1, lte_prec2,
        gt, gt_prec1, gt_prec2,
        gte, gte_prec1, gte_prec2,
        big, big_negative]}
  ].


%% =============================================================================
%% init
%% =============================================================================
init_per_group(_Group, Config) ->
  ok = application:load(eapa),
  {ok, _} = application:ensure_all_started(eapa, temporary),
  [{init, true} | Config].


%% =============================================================================
%% end
%% =============================================================================
end_per_group(_Group, _Config) ->
  ok = application:stop(eapa),
  ok = application:unload(eapa),
  ok.


%% =============================================================================
%% group: int
%% =============================================================================
float_to_bigint(_) ->
  X = eapa_int:with_val(6, <<"1280001.10345">>),
  <<"1280001.103">> = eapa_int:to_float(3, X),
  <<"1280001.103450">> = eapa_int:to_float(6, X),
  ok.

add(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  Sum = eapa_int:add(X1, X2),
  <<"0.055673">> = eapa_int:to_float(6, Sum),
  ok.

add_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  Sum = eapa_int:add(X1, X2),
  <<"0.055673">> = eapa_int:to_float(6, Sum),
  ok.

add_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  Sum = eapa_int:add(X1, X2),
  <<"0.055673">> = eapa_int:to_float(6, Sum),
  ok.

sub(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:sub(X1, X2),
  <<"0.055427">> = eapa_int:to_float(6, R),
  ok.

sub_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:sub(X1, X2),
  <<"0.055427">> = eapa_int:to_float(6, R),
  ok.

sub_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  R = eapa_int:sub(X1, X2),
  <<"0.055427">> = eapa_int:to_float(6, R),
  ok.

mul(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:mul(X1, X2),
  <<"0.000006">> = eapa_int:to_float(6, R),
  ok.

mul_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:mul(X1, X2),
  <<"0.00000683">> = eapa_int:to_float(8, R),
  ok.

mul_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  R = eapa_int:mul(X1, X2),
  <<"0.00000683">> = eapa_int:to_float(8, R),
  ok.

divp(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:divp(X1, X2),
  <<"451.626016">> = eapa_int:to_float(6, R),
  ok.

divp_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:divp(X1, X2),
  <<"451.62601626">> = eapa_int:to_float(8, R),
  ok.

divp_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  R = eapa_int:divp(X1, X2),
  <<"451.62601626">> = eapa_int:to_float(8, R),
  ok.

min(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:min(X1, X2),
  <<"0.000123">> = eapa_int:to_float(6, R),
  ok.

min_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:min(X1, X2),
  <<"0.000123">> = eapa_int:to_float(6, R),
  ok.

min_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  R = eapa_int:min(X1, X2),
  <<"0.000123">> = eapa_int:to_float(6, R),
  ok.

max(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:max(X1, X2),
  <<"0.055550">> = eapa_int:to_float(6, R),
  ok.

max_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  R = eapa_int:max(X1, X2),
  <<"0.055550">> = eapa_int:to_float(6, R),
  ok.

max_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  R = eapa_int:max(X1, X2),
  <<"0.055550">> = eapa_int:to_float(6, R),
  ok.

lt(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  false = eapa_int:lt(X1, X2),
  true = eapa_int:lt(X2, X1),
  ok.

lt_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  false = eapa_int:lt(X1, X2),
  true = eapa_int:lt(X2, X1),
  ok.

lt_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  false = eapa_int:lt(X1, X2),
  true = eapa_int:lt(X2, X1),
  ok.

lte(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  false = eapa_int:lte(X1, X2),
  true = eapa_int:lte(X2, X1),
  ok.

lte_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  false = eapa_int:lte(X1, X2),
  true = eapa_int:lte(X2, X1),
  ok.

lte_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  false = eapa_int:lte(X1, X2),
  true = eapa_int:lte(X2, X1),
  ok.

gt(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  true = eapa_int:gt(X1, X2),
  false = eapa_int:gt(X2, X1),
  ok.

gt_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  true = eapa_int:gt(X1, X2),
  false = eapa_int:gt(X2, X1),
  ok.

gt_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  true = eapa_int:gt(X1, X2),
  false = eapa_int:gt(X2, X1),
  ok.

gte(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  true = eapa_int:gte(X1, X2),
  false = eapa_int:gte(X2, X1),
  ok.

gte_prec1(_) ->
  X1 = eapa_int:with_val(12, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  true = eapa_int:gte(X1, X2),
  false = eapa_int:gte(X2, X1),
  ok.

gte_prec2(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(12, <<"0.000123">>),
  true = eapa_int:gte(X1, X2),
  false = eapa_int:gte(X2, X1),
  ok.

big(_) ->
  X = <<"2147483648.2147483647">>,
  C = eapa_int:with_val(10, X),
  X = eapa_int:to_float(C).

big_negative(_) ->
  X = <<"-2147483648.2147483647">>,
  C = eapa_int:with_val(10, X),
  X = eapa_int:to_float(C).