-module(effects_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, g30000000000000004}
  ].

groups() ->
  [
    {g30000000000000004,
      [parallel, shuffle],
      [t30000000000000004, t30000000000000004_eapa,
        float_overflow, float_overflow_eapa,
        tiny, tiny_eapa,
        eq, eq_eapa,
        sqrt,
        reduction, reduction_eapa]}
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
%% group: g30000000000000004
%% =============================================================================
t30000000000000004(_) ->
  ["0.30000000000000004"] = io_lib:format("~w", [0.1 + 0.2]).

t30000000000000004_eapa(_) ->
  %% prec = 1 symbols after comma
  X = eapa_int:with_val(1, <<"0.1">>),
  Y = eapa_int:with_val(1, <<"0.2">>),
  <<"0.3">> = eapa_int:to_float(1, eapa_int:add(X, Y)),

  %% prec = 17 symbols after comma
  X1 = eapa_int:with_val(17, <<"0.1">>),
  Y1 = eapa_int:with_val(17, <<"0.2">>),
  <<"0.30000000000000000">> = eapa_int:to_float(17, eapa_int:add(X1, Y1)),
  <<"0.30000000000000000">> = eapa_int:to_float(17, eapa_int:add(X, Y)).

float_overflow(_) ->
  1.0 = 9007199254740991.0 - 9007199254740990.0,
  1.0 = 9007199254740992.0 - 9007199254740991.0,
  0.0 = 9007199254740993.0 - 9007199254740992.0,
  2.0 = 9007199254740994.0 - 9007199254740993.0.

float_overflow_eapa(_)->
  X11 = eapa_int:with_val(1, <<"9007199254740992.0">>),
  X21 = eapa_int:with_val(1, <<"9007199254740991.0">>),
  <<"1.0">> = eapa_int:to_float(1, eapa_int:sub(X11, X21)),
  X12 = eapa_int:with_val(1, <<"9007199254740993.0">>),
  X22 = eapa_int:with_val(1, <<"9007199254740992.0">>),
  <<"1.0">> = eapa_int:to_float(1, eapa_int:sub(X12, X22)),
  X13 = eapa_int:with_val(1, <<"9007199254740994.0">>),
  X23 = eapa_int:with_val(1, <<"9007199254740993.0">>),
  <<"1.0">> = eapa_int:to_float(1, eapa_int:sub(X13, X23)).

tiny(_)->
  X = 1.0,
  Y = 0.0000000000000000000000001,
  1.0 = X + Y.

tiny_eapa(_)->
  X1 = eapa_int:with_val(1, <<"1.0">>),
  X2 = eapa_int:with_val(25, <<"0.0000000000000000000000001">>),
  <<"1.0000000000000000000000001">> = eapa_int:to_float(eapa_int:add(X1, X2)).

eq(_)->
  true = list_to_float("0.0") =:= list_to_float("-0.0").

eq_eapa(_)->
  X = eapa_int:with_val(1, <<"0.0">>),
  Y = eapa_int:with_val(1, <<"-0.0">>),
  true = eapa_int:eq(X, Y).

sqrt(_)->
  0.0 = math:sqrt(list_to_float("-0.0")).

reduction(_)->
  X = float(87654321098765432),
  Y = float(87654321098765431),
  16.0 = X-Y. %% has to be 1.0

reduction_eapa(_)->
  X = eapa_int:with_val(1, <<"87654321098765432">>),
  Y = eapa_int:with_val(1, <<"87654321098765431">>),
  <<"1.0">> = eapa_int:to_float(eapa_int:sub(X, Y)).

%%find_fm(NextFun) ->
%%  case NextFun() of
%%    [X | NewNextFun] ->
%%      X1 = list_to_float(integer_to_list(X)++".9921875"),
%%      X2 = list_to_float(integer_to_list(X)++".9921875"),
%%      R = tl(binary:split(list_to_binary(io_lib:format("~w",[(X1 + X2)])), <<".">>)),
%%      case R of
%%        [<<"984375">>] ->
%%          find_fm(NewNextFun);
%%        _Else ->
%%          ?debugVal({X1, io_lib:format("~w",[(X1 + X2)])})
%%      end;
%%    _ ->
%%      ok
%%  end.
%%
%%seq(M, N) when M =< N ->
%%  fun() -> [M | seq(M + 1, N)] end;
%%seq(_, _) ->
%%  fun() -> [] end.