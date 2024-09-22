%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2024 1:12 pm
%%%-------------------------------------------------------------------
-module(transaction_cache_util).
-author("somashekar.b").

-include("transaction_constants.hrl").
%% API
-export([get_all_users/0,
    invalidate_allusers/0,
    invalidate_user/1,
    get_user_data/1,
    get_user_login/1]).

get_all_users() ->
    get_all_users(cachefirst).

get_all_users(cachefirst) ->
    Key = <<"allusers">>,
    case ets:lookup(?Users_Table, Key) of
        [] ->
            case get_all_users(nocache) of
                {error, not_found} ->
                    ets:insert(?Users_Table, {Key, not_in_db}),
                    {error, notfound};
                {error, _} ->
                    {error, notfound};
                Data ->
                    ets:insert(?Users_Table, {Key, Data}),
                    Data
            end;
        [{Key, Data}] ->
            case Data of
                not_in_db ->
                    {error, notfound};
                _ ->
                    Data
            end
    end;

get_all_users(nocache) ->
    Query = <<"SELECT userid, username, email, phone, address FROM users_data">>,
    case transaction_sql_util:query(?DBReadPool, Query, [], 10000, true) of
        {error, notfound} ->
            {error, not_found};
        {error, _Error} ->
            {error, someerror};
        {ok, UsersData} ->
            UsersData
    end.

get_user_login(UserId) ->
    get_user_login(UserId, cachefirst).

get_user_login(UserId, cachefirst) ->
    Key = <<"Login", UserId/binary>>,
    LK = ets:lookup(?Users_Table, Key),
    case LK of
        [] ->
            case get_user_login(UserId, nocache) of
                {error, not_found} ->
                    ets:insert(?Users_Table, {Key, not_in_db}),
                    {error, notfound};
                {error, _} ->
                    {error, notfound};
                Data ->
                    ets:insert(?Users_Table, {Key, Data}),
                    Data
            end;
        [{_, Data}] ->
            case Data of
                not_in_db ->
                    {error, notfound};
                _ ->
                    Data
            end
    end;

get_user_login(UserId, nocache) ->
    Query = <<"SELECT * FROM users WHERE userid = ?">>,
    Params = [UserId],
    case transaction_sql_util:query(?DBReadPool, Query, Params, 1000, true) of
        {error, notfound} ->
            {error, not_found};
        {error, _Error} ->
            {error, someerror};
        {ok, UsersData} ->
            UsersData
    end.


get_user_data(UserId) ->
    get_user_data(UserId, cachefirst).

get_user_data(UserId, cachefirst) ->
    case ets:lookup(?Users_Table, UserId) of
        [] ->
            case get_user_data(UserId, nocache) of
                {error, not_found} ->
                    ets:insert(?Users_Table, {UserId, not_in_db}),
                    {error, notfound};
                {error, _} ->
                    {error, notfound};
                Data ->
                    ets:insert(?Users_Table, {UserId, Data}),
                    Data
            end;
        [{_, Data}] ->
            case Data of
                not_in_db ->
                    {error, notfound};
                _ ->
                    Data
            end
    end;

get_user_data(UserId, nocache) ->
    Query = <<"SELECT * FROM users_data WHERE userid = ?">>,
    Params = [UserId],
    case transaction_sql_util:query(?DBReadPool, Query, Params, 1000, true) of
        {error, notfound} ->
            {error, not_found};
        {error, _Error} ->
            {error, someerror};
        {ok, UsersData} ->
            UsersData
    end.


invalidate_allusers() ->
    Key = <<"allusers">>,
    ets:delete(?Users_Table, Key).

invalidate_user(UserId) ->
    ets:delete(?Users_Table, UserId).