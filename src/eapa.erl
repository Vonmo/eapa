-module(eapa).

%% API
-export([
  lxcode/0,
  float_to_bigint/2,
  bigint_to_float/2,
  bigint_add/2,
  bigint_sub/2,
  bigint_mul/3,
  bigint_div/3,
  bigint_min/2,
  bigint_max/2,
  bigint_lt/2,
  bigint_lte/2,
  bigint_gt/2,
  bigint_gte/2
]).

%% Native library support
-export([load/0]).
-on_load(load/0).

lxcode() ->
  not_loaded(?LINE).

float_to_bigint(_FloatVal, _Prec) ->
  not_loaded(?LINE).

bigint_to_float(_IntVal, _Prec) ->
  not_loaded(?LINE).

bigint_add(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_sub(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_mul(_Val1, _Val2, _Prec) ->
  not_loaded(?LINE).

bigint_div(_Val1, _Val2, _Prec) ->
  not_loaded(?LINE).

bigint_min(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_max(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_lt(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_lte(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_gt(_Val1, _Val2) ->
  not_loaded(?LINE).

bigint_gte(_Val1, _Val2) ->
  not_loaded(?LINE).

load() ->
  erlang:load_nif(filename:join(priv(), "libeapa"), none).

not_loaded(Line) ->
  erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
  case code:priv_dir(?MODULE) of
    {error, _} ->
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Path ->
      Path
  end.
