-module(libbtc_ex).
-export([derive/2]).
-export([init/0]).
%%-on_load(init/0).

-define(APPNAME, libbtc_ex).
-define(LIBNAME, libbtc_ex_nif).

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
    case erlang:load_nif(SoName, 0) of
        ok->
            io:format("Succesfully loaded: libbtc_ex_nif ~n");
        Error->
            io:format("Error during load ~p~n", [Error])
    end.

derive(_, _) ->
    exit(nif_library_not_loaded).
