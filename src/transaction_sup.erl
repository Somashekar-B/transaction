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
%%    io:format("~p",[application:get_env(?Application, palma_pools)]),
    start_palma_pools(),
    transaction_router:start(),
    {ok, {{one_for_one, 10, 10}, []}}.

start_palma_pools() ->
    {ok, PalmaPools} = application:get_env(?Application, palma_pools),
    NonStartedPools = lists:foldl(
        fun({PoolName, NoOfPools, PoolSpecs, ShutdownDelay, RevolverOptions}, Acc) ->
            try
                palma:new(PoolName, NoOfPools, PoolSpecs, ShutdownDelay, RevolverOptions),
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