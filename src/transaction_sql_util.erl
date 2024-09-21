%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2024 1:26 am
%%%-------------------------------------------------------------------
-module(transaction_sql_util).
-author("somashekar.b").

%% API
-export([query/4]).

query(PoolName, Query, Params, ConnectionTimeOut) ->
    try
        case palma:pid(PoolName) of
            Pid when is_pid(Pid) ->
                case mysql:query(Pid, Query, Params, ConnectionTimeOut) of
                    {ok, _ColumnNames, []} ->
                        {error, notfound};
                    {ok, ColumnNames, RowValues} ->
                        {ok, {ColumnNames, RowValues}};
                    {error, Error} ->
                        {error, Error};
                    Result ->
                        Result
                end;
            _ ->
                {error, not_found}
        end
    catch
        _C:_E  ->
            {error, disconnected}
    end.
