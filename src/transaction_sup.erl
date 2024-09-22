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

-include("transaction_constants.hrl").

%% API
-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    start_palma_pools(),
    start_ets_tables(),
    transaction_router:start(),
    {ok, {{one_for_one, 10, 10}, []}}.

start_palma_pools() ->
    {ok, PalmaPools} = application:get_env(?Application, palma_pools),
    NonStartedPools = lists:foldl(
        fun({PoolName, NoOfPools, PoolSpecs, ShutdownDelay, RevolverOptions}, Acc) ->
            try
                Res = palma:new(PoolName, NoOfPools, PoolSpecs, ShutdownDelay, RevolverOptions),
                io:format("~p\n",[{PoolName, Res}]),
                Acc
            catch
                _C:_E ->
                    [PoolName | Acc]
            end
        end, [], PalmaPools
    ),
    case NonStartedPools of
        [] ->
            ok;
        _ ->
            lists:foreach(
                fun(PoolName) ->
                    palma:stop(PoolName)
                end, NonStartedPools
            )
    end.

start_ets_tables() ->
    {ok, ETSTables} = application:get_env(?Application, ets_tables),
    lists:foreach(
        fun(Table) ->
            ets:new(Table, [
                named_table,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ])
        end, ETSTables
    ).