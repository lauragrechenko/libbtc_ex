-module(libbtc_ex).
-export([derive/2]).
-on_load(init/0).

-define(APPNAME, libbtc_ex).
-define(LIBNAME, libbtc_nif).

init() ->
    io:format ("Init started~n"),
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
    io:format("~p ~n", [SoName]), 
    case erlang:load_nif(SoName, 0) of
        ok->
            io:format("Succesfully loaded~n");
        Error->
            io:format("Error during load ~p~n", [Error])
    end.

derive(_, _) ->
    exit(nif_library_not_loaded).
