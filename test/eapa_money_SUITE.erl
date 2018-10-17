-module(eapa_money_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, money}
  ].

groups() ->
  [
    {money,
      [parallel, shuffle],
      [float_to_bigint,
        add, add_prec1, add_prec2,
        sub, sub_prec1, sub_prec2,
        mul, mul_prec1, mul_prec2,
        divp, divp_prec1, divp_prec2,
        min, min_prec1, min_prec2,
        max, max_prec1, max_prec2]}
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
%% group: money
%% =============================================================================
float_to_bigint(_) ->
  X = eapa_money:with_val(6, <<"1280001.10345">>),
  <<"1280001.103">> = eapa_money:to_float(3, X),
  <<"1280001.103450">> = eapa_money:to_float(6, X),
  ok.

add(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  Sum = eapa_money:add(X1, X2),
  <<"0.055673">> = eapa_money:to_float(6, Sum),
  ok.

add_prec1(_) ->
  X1 = eapa_money:with_val(12, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  Sum = eapa_money:add(X1, X2),
  <<"0.055673">> = eapa_money:to_float(6, Sum),
  ok.

add_prec2(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(12, <<"0.000123">>),
  Sum = eapa_money:add(X1, X2),
  <<"0.055673">> = eapa_money:to_float(6, Sum),
  ok.

sub(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:sub(X1, X2),
  <<"0.055427">> = eapa_money:to_float(6, R),
  ok.

sub_prec1(_) ->
  X1 = eapa_money:with_val(12, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:sub(X1, X2),
  <<"0.055427">> = eapa_money:to_float(6, R),
  ok.

sub_prec2(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(12, <<"0.000123">>),
  R = eapa_money:sub(X1, X2),
  <<"0.055427">> = eapa_money:to_float(6, R),
  ok.

mul(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:mul(X1, X2),
  <<"0.000006">> = eapa_money:to_float(6, R),
  ok.

mul_prec1(_) ->
  X1 = eapa_money:with_val(12, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:mul(X1, X2),
  <<"0.00000683">> = eapa_money:to_float(8, R),
  ok.

mul_prec2(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(12, <<"0.000123">>),
  R = eapa_money:mul(X1, X2),
  <<"0.00000683">> = eapa_money:to_float(8, R),
  ok.

divp(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:divp(X1, X2),
  <<"451.626016">> = eapa_money:to_float(6, R),
  ok.

divp_prec1(_) ->
  X1 = eapa_money:with_val(12, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:divp(X1, X2),
  <<"451.62601626">> = eapa_money:to_float(8, R),
  ok.

divp_prec2(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(12, <<"0.000123">>),
  R = eapa_money:divp(X1, X2),
  <<"451.62601626">> = eapa_money:to_float(8, R),
  ok.

min(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:min(X1, X2),
  <<"0.000123">> = eapa_money:to_float(6, R),
  ok.

min_prec1(_) ->
  X1 = eapa_money:with_val(12, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:min(X1, X2),
  <<"0.000123">> = eapa_money:to_float(6, R),
  ok.

min_prec2(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(12, <<"0.000123">>),
  R = eapa_money:min(X1, X2),
  <<"0.000123">> = eapa_money:to_float(6, R),
  ok.

max(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:max(X1, X2),
  <<"0.055550">> = eapa_money:to_float(6, R),
  ok.

max_prec1(_) ->
  X1 = eapa_money:with_val(12, <<"0.05555">>),
  X2 = eapa_money:with_val(6, <<"0.000123">>),
  R = eapa_money:max(X1, X2),
  <<"0.055550">> = eapa_money:to_float(6, R),
  ok.

max_prec2(_) ->
  X1 = eapa_money:with_val(6, <<"0.05555">>),
  X2 = eapa_money:with_val(12, <<"0.000123">>),
  R = eapa_money:max(X1, X2),
  <<"0.055550">> = eapa_money:to_float(6, R),
  ok.