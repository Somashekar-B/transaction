%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2024 5:58 pm
%%%-------------------------------------------------------------------
-module(transaction_sup).

-behavior(supervisor).
-author("somashekar.b").

%% API
-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    transaction_router:start(),
    {ok, {{one_for_one, 10, 10}, []}}.
