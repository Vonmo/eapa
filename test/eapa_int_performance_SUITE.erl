-module(eapa_int_performance_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, perf}
  ].

groups() ->
  [
    {perf,
      [shuffle],
      [with_val, to_float, add, sub, mul, divp]}
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
%% group: common_app_checks
%% =============================================================================
with_val(_) ->
  W = perftest:comprehensive(1000,
    fun() ->
      eapa_int:with_val(6, <<"0.05555">>)
    end),
  true = lists:all(fun(E) -> E >= 30000 end, W). %% 30 microseconds

to_float(_) ->
  X = eapa_int:with_val(6, <<"0.05555">>),
  W = perftest:comprehensive(1000,
    fun() ->
      <<"0.055550">> = eapa_int:to_float(6, X)
    end),
  true = lists:all(fun(E) -> E >= 30000 end, W). %% 30 microseconds

add(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  W = perftest:comprehensive(1000,
    fun() ->
      eapa_int:add(X1, X2)
    end),
  true = lists:all(fun(E) -> E >= 30000 end, W). %% 30 microseconds

sub(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  W = perftest:comprehensive(1000,
    fun() ->
      eapa_int:sub(X1, X2)
    end),
  true = lists:all(fun(E) -> E >= 30000 end, W). %% 30 microseconds

mul(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  W = perftest:comprehensive(1000,
    fun() ->
      eapa_int:mul(X1, X2)
    end),
  true = lists:all(fun(E) -> E >= 30000 end, W). %% 30 microseconds

divp(_) ->
  X1 = eapa_int:with_val(6, <<"0.05555">>),
  X2 = eapa_int:with_val(6, <<"0.000123">>),
  W = perftest:comprehensive(1000,
    fun() ->
      eapa_int:divp(X1, X2)
    end),
  true = lists:all(fun(E) -> E >= 30000 end, W). %% 30 microseconds
