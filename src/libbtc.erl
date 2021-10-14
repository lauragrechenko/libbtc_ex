-module(libbtc).
-export([derive/2]).
-on_load(init/0).

-define(APPNAME, libbtc).
-define(LIBNAME, libbtc_nif).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

derive(_, _) ->
    exit(nif_library_not_loaded).
