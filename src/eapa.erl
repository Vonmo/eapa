-module(eapa).

%% API
-export([lxcode/0
         ]).

%% Native library support
-export([load/0]).
-on_load(load/0).

lxcode() ->
    not_loaded(?LINE).

load() ->
    erlang:load_nif(filename:join(priv(), "libeapa"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
  case code:priv_dir(?MODULE) of
      {error, _} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end.
