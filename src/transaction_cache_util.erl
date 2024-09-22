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
    get_user_data/1,
    get_user_login/1]).

-export([
    get_account_details/1
]).

-export([
    is_transaction_allowed/1,
    update_transactions/1
]).

-export([
    invalidate_allusers/0,
    invalidate_user/1,
    invalidate_account_details/1
]).

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

get_account_details(AccountNo) ->
    get_account_details(AccountNo, cachefirst).

get_account_details(AccountNo, cachefirst) ->
    case ets:lookup(?AccountsTable, AccountNo) of
        [] ->
            case get_account_details(AccountNo, nocache) of
                {error, notfound} ->
                    ets:insert(?AccountsTable, {AccountNo, not_in_db}),
                    {error, notfound};
                {error, _} ->
                    {error, notfound};
                Data ->
                    ets:insert(?AccountsTable, {AccountNo, Data}),
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

get_account_details(AccountNo, nocache) ->
    Query = <<"SELECT * FROM account_details WHERE accountNo = ?">>,
    Params = [AccountNo],
    case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, true) of
        {error, Error} ->
            {error, Error};
        {ok, AccountData} ->
            AccountData
    end.

is_transaction_allowed(AccountNo) ->
    is_transaction_allowed(AccountNo, cachefirst).

is_transaction_allowed(AccountNo, cachefirst) ->
    case ets:lookup(?Transactions, AccountNo) of
        [] ->
            case is_transaction_allowed(AccountNo, nocache) of
                {error, notfound} ->
                    ets:insert(?Transactions, {AccountNo, not_in_db}),
                    false;
                {error, _} ->
                    false;
                Transactions ->
                    {ok, NoOfTranPerDay} = application:get_env(?Application, number_of_transactions),
                    IsValid = length(Transactions) < NoOfTranPerDay,
                    ets:insert(?Transactions, {AccountNo, {length(Transactions), Transactions}}),
                    IsValid
            end;
        [{_, Data}] ->
            case Data of
                not_in_db ->
                    false;
                {TransactionCount, Transactions} ->
                    {ok, NoOfTranPerDay} = application:get_env(?Application, number_of_transactions),

                    case TransactionCount < NoOfTranPerDay of
                        true ->
                            true;
                        _ ->
                            CurrentTime = datetime_to_seconds(calendar:now_to_datetime(erlang:timestamp())),
                            TimeLimit = CurrentTime - (24 * 3600),
                            ValidTransactions = remove_transactions(Transactions, TimeLimit),
                            ets:insert(?Transactions, {AccountNo, {length(ValidTransactions), ValidTransactions}}),
                            case length(ValidTransactions) < NoOfTranPerDay of
                                true ->
                                    true;
                                _ ->
                                    false
                            end
                    end
            end
    end;

is_transaction_allowed(AccountNo, nocache) ->
    Query = <<"SELECT UNIX_TIMESTAMP(transaction_datetime) as transaction_seconds FROM transactions WHERE accountNo = ? AND txn_type = ? AND transaction_datetime >= NOW() - INTERVAL 1 DAY">>,
    Params = [AccountNo, 0],
    case transaction_sql_util:query(?DBReadPool, Query, Params, 1000, false) of
        {error, Error} ->
            {error, Error};
        {ok, {_, Rows}} ->
            lists:sort(lists:flatten(Rows))
    end.

update_transactions(AccountNo) ->
    case ets:lookup(?Transactions, AccountNo) of
        [{_, Data}] ->
            CurrentTime = datetime_to_seconds(calendar:now_to_datetime(erlang:timestamp())),
            case Data of
                not_in_db ->
                    ets:insert(?Transactions, {AccountNo, {1, [CurrentTime]}});
                {TransactionCount, Transactions} ->
                    ets:insert(?Transactions, {AccountNo, {TransactionCount + 1, Transactions ++ [CurrentTime]}})
            end;
        [] ->
%%            This case never occurs
            ets:insert(?Transactions, {AccountNo, {1, [datetime_to_seconds(calendar:now_to_datetime(erlang:timestamp()))]}})
    end.

remove_transactions([], _TimeLimit) ->
    [];

remove_transactions([Transaction | Rem], TimeLimit) ->
    case Transaction < TimeLimit of
        true ->
            remove_transactions(Rem, TimeLimit);
        _ ->
            [Transaction | Rem]
    end.

datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.

invalidate_allusers() ->
    Key = <<"allusers">>,
    ets:delete(?Users_Table, Key).

invalidate_user(UserId) ->
    ets:delete(?Users_Table, UserId).

invalidate_account_details(AccountNo) ->
    ets:delete(?AccountsTable, AccountNo).