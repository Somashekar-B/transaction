%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2024 6:01 pm
%%%-------------------------------------------------------------------
-module(transaction_handler).
-author("somashekar.b").

-include("transaction_constants.hrl").

%% API
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get_request(Req0, Opts)
    end.

handle_get_request(Req0, Opts) ->
    Res = cowboy_req:reply(200, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(<<"Hello">>), Req0),
    {ok, Res, Opts}.
