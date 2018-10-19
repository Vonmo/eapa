-module(eapa_int_proper_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).

-include_lib("proper/include/proper.hrl").

all() ->
  [
    {group, int}
  ].

groups() ->
  [
    {int,
      [parallel, shuffle],
      [propers]}
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
propers(_) ->
  run_proper(module,
    fun() ->
      [] = proper:module(?MODULE, [long_result, {max_size, 42}, {numtests, 1000}])
    end).

prop_enc_dec_small() ->
  ?FORALL({X1, X2, P}, {integer(0, 100), integer(2147483647, 4294967294), integer(14, 126)},
    begin
      Orig = integer_to_list(X1)++"."++string:left(integer_to_list(X2), P, $0),
      OrigBin = list_to_binary(Orig),
      A = eapa_int:with_val(P, OrigBin),
      OrigBin =:= eapa_int:to_float(P, A)
    end).

prop_enc_dec_big() ->
  ?FORALL({X1, X2, P}, {
    integer(2147483647, 4294967294),
    integer(2147483647, 4294967294),
    integer(10, 126)
  },
    begin
      Orig = integer_to_list(X1)++"."++string:left(integer_to_list(X2), P, $0),
      OrigBin = list_to_binary(Orig),
      A = eapa_int:with_val(P, OrigBin),
      OrigBin =:= eapa_int:to_float(P, A)
    end).

prop_enc_dec_huge() ->
  ?FORALL({X1, P}, {
    integer(1, 429496729491),
    integer(15, 126)
  },
    begin
      %%      9223372036854775807
      Orig = "92233720368547758079223372036854775807."++string:left(integer_to_list(X1), P, $0),
      OrigBin = list_to_binary(Orig),
      A = eapa_int:with_val(P, OrigBin),
      OrigBin =:= eapa_int:to_float(P, A)
    end).

run_proper(What, Fun) ->
  Leader = erlang:group_leader(),
  erlang:group_leader(whereis(user), self()),
  _ExpectedReturn = case What of
                      module -> [];
                      function -> true
                    end,
  Fun(),
  timer:sleep(100),
  erlang:group_leader(Leader, self()).
