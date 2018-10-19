-module(eapa_money).

%% API
-export([
  with_val/2,
  to_float/2,
  to_float/1,
  add/2,
  sub/2,
  mul/2,
  divp/2,
  min/2,
  max/2,
  lt/2,
  lte/2,
  gt/2,
  gte/2
]).

with_val(Prec, Val) when is_integer(Prec), Prec < 127, is_binary(Val) ->
  {X, Prec} = eapa:float_to_bigint(Val, Prec),
  <<Prec/integer, X/binary>>;
with_val(Prec, Val) when is_integer(Prec), Prec < 127 ->
  ValBin = iolist_to_binary(io_lib:format("~p", [Val])),
  with_val(Prec, ValBin).

to_float(Prec, <<VPrec:8/integer, Val/binary>>) when is_integer(Prec), Prec > 0, Prec < 127, is_binary(Val) ->
  A = binary_to_list(eapa:bigint_to_str(Val, VPrec)),
  {H, T} = case length(A) > VPrec of
    true ->
      {Hs, Ts} = lists:split(length(A) - VPrec, A),
      {list_to_binary(Hs), list_to_binary(Ts)};
    false ->
      {<<"0">>, iolist_to_binary([[$0 || _ <- lists:seq(1, VPrec - length(A))], A])}
  end,
  iolist_to_binary(case byte_size(T) of
                     X when X >= Prec ->
                       <<V:Prec/binary, _/binary>> = T,
                       [H, $., V];
                     Y ->
                       <<V:Y/binary, _/binary>> = T,
                       [H, $., V, [$0 || _ <- lists:seq(1, Prec - Y)]]
                   end);
to_float(Prec, <<VPrec:8/integer, Val/binary>>) when is_integer(Prec), Prec =:= 0 ->
  A = binary_to_list(eapa:bigint_to_str(Val, VPrec)),
  {Hs, _Ts} = lists:split(length(A) - VPrec, A),
  list_to_binary(Hs).

to_float(<<VPrec:8/integer, _Val/binary>> = X) ->
  to_float(VPrec, X).

add(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_add(Val1, Val2) end).

sub(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_sub(Val1, Val2) end).

mul(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, Prec) -> eapa:bigint_mul(Val1, Val2, Prec) end).

divp(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, Prec) -> eapa:bigint_div(Val1, Val2, Prec) end).

min(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_min(Val1, Val2) end).

max(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_max(Val1, Val2) end).

lt(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_lt(Val1, Val2) end).

lte(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_lte(Val1, Val2) end).

gt(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_gt(Val1, Val2) end).

gte(X1, X2) ->
  operation_prec(X1, X2, fun(Val1, Val2, _Prec) -> eapa:bigint_gte(Val1, Val2) end).

%% =====================================================================================================================
%% helpers
%% =====================================================================================================================
operation_prec(<<Prec1:8/integer, _Val1/binary>> = X1, <<Prec2:8/integer, _Val2/binary>> = X2, Fun) when Prec1 > Prec2 ->
  F2 = to_float(X2),
  X22 = with_val(Prec1, F2),
  operation_prec(X1, X22, Fun);
operation_prec(<<Prec1:8/integer, _Val1/binary>> = X1, <<Prec2:8/integer, _Val2/binary>> = X2, Fun) when Prec1 < Prec2 ->
  F1 = to_float(X1),
  X12 = with_val(Prec2, F1),
  operation_prec(X12, X2, Fun);
operation_prec(<<Prec:8/integer, Val1/binary>>, <<Prec:8/integer, Val2/binary>>, Fun) ->
  <<Prec/integer, (Fun(Val1, Val2, Prec))/binary>>.