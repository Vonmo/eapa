-module(common_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, common_app_checks}
  ].

groups() ->
  [
    {common_app_checks,
      [parallel, shuffle],
      [app_module_load, sup_module_load, lib_module_load]}
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
app_module_load(_) ->
  {module, eapa_app} = code:load_file(eapa_app).

sup_module_load(_) ->
  {module, eapa_sup} = code:load_file(eapa_sup).

lib_module_load(_) ->
  {ok,vn1} = eapa:lxcode().
