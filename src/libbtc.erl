-module(libbtc).
-export([derive/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./libbtc", 0).

derive(_, _) ->
    exit(nif_library_not_loaded).
