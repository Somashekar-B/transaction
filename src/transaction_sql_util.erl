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
-export([query/5]).

query(PoolName, Query, Params, ConnectionTimeOut, ConvertToMap) ->
    try
        case palma:pid(PoolName) of
            Pid when is_pid(Pid) ->
                case mysql:query(Pid, Query, Params, ConnectionTimeOut) of
                    {ok, ColumnNames, []} ->
                        {error, notfound};
                    {ok, ColumnNames, RowValues} ->
                        FinalResult = convert_to_map(ColumnNames, RowValues, ConvertToMap),
                        {ok, FinalResult};
                    {error, Error} ->
                        {error, Error};
                    ok ->
                        LastInsertId = mysql:insert_id(Pid),
                        {ok, LastInsertId};
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

convert_to_map(ColumnNames, [RowValues], true) ->
    CombinedList = lists:zip(ColumnNames, RowValues),
    maps:from_list(CombinedList);

convert_to_map(ColumnNames, RowValues, true) ->
    Fun = fun(Row) ->
        CombinedList = lists:zip(ColumnNames, Row),
        maps:from_list(CombinedList)
          end,
    [Fun(X) || X <- RowValues];

convert_to_map(ColumnNames, RowValues, _) ->
    {ColumnNames, RowValues}.

