%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2024 7:01 pm
%%%-------------------------------------------------------------------
-module(transaction_util).
-author("somashekar.b").

-include("transaction_constants.hrl").

%% API
-export([
    create_user_account/1,
    deposit_amount/1,
    withdraw_amount/1,
    get_all_transactions_for_user_account/2
]).

get_all_transactions_for_user_account(UserId, Account) ->
    case transaction_cache_util:get_all_trex_for_user_account(UserId, Account) of
        {error, notfound} ->
            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"No Transactions Found For Given User Account">>}};
        Data ->
            {?HTTP_OK_RESPONSE_CODE, Data}
    end.

create_user_account(Request) ->
    UserId = maps:get(<<"userid">>, Request, <<>>),
    case user_profile_util:is_nil_or_empty(UserId) of
        true ->
            {?HTTP_BAD_REQUEST_CODE, #{<<"Message">> => <<"Invalid UserId">>}};
        _ ->
            Amount = maps:get(<<"amount">>, Request, 0),
            Query = <<"INSERT INTO account_details(userid, amount) VALUES(?,?)">>,
            Params = [UserId, Amount],
            case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, true) of
                {ok, Id} ->
                    {?HTTP_OK_RESPONSE_CODE, #{<<"accountNo">> => Id, <<"Message">> => <<"Success">>}};
                _ ->
                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Creation Failed">>}}
            end
    end.

deposit_amount(Request) ->
    Account = maps:get(<<"accountNo">>, Request, <<>>),
    Amount = maps:get(<<"amount">>, Request, <<>>),
    {ok, MinAmount} = application:get_env(?Application, min_dep_amount),
    {ok, MaxAmount} = application:get_env(?Application, max_dep_amount),
    case {user_profile_util:is_nil_or_empty(Account), user_profile_util:is_nil_or_empty(Amount)} of
        {false, false} ->
            case Amount >= MinAmount andalso Amount =< MaxAmount of
                true ->
                     case transaction_cache_util:get_account_details(Account) of
                         {error, _} ->
                             {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Details Doesn't Exist">>}};
                         AccountData ->
                             AccountUserId = maps:get(<<"userid">>, AccountData, -2),
                             LoggedInUserId = maps:get(<<"userid">>, Request, -1),
%%                             lager:info("~p ~p",[AccountUserId, LoggedInUserId]),
                             case AccountUserId of
                                 LoggedInUserId ->
                                     CurrentAmount = maps:get(<<"amount">>, AccountData),
                                     TotalAmount = CurrentAmount + Amount,
                                     case update_amount_in_account(Account, TotalAmount) of
                                         {error, _} ->
                                             {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Update Failed">>}};
                                         _ ->
                                             transaction_cache_util:invalidate_account_details(Account),
                                             case create_transaction(AccountData, Amount, 1) of
                                                 error ->
%%                                             pushing the failed messages to queue and process later
                                                     {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Transaction Entry Creation Falied">>}};
                                                 TransactionId ->
                                                     {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"SUCCESS">>, <<"TransactionId">> => TransactionId, <<"BalanceAmount">> => TotalAmount}}
                                             end
                                     end;
                                 _ ->
                                     {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Doesn't Belong to User">>}}
                             end
                     end;
                _ ->
                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Amount Invalid">>}}
            end;
        _ ->
            {?HTTP_BAD_REQUEST_CODE, #{<<"Message">> => <<"Invalid Inputs">>}}
    end.

withdraw_amount(Request) ->
    Account = maps:get(<<"accountNo">>, Request, <<>>),
    case transaction_cache_util:is_transaction_allowed(Account) of
        true ->
            Amount = maps:get(<<"amount">>, Request, <<>>),
            {ok, MinAmount} = application:get_env(?Application, min_withdraw_amount),
            {ok, MaxAmount} = application:get_env(?Application, max_withdraw_amount),
            case {user_profile_util:is_nil_or_empty(Account), user_profile_util:is_nil_or_empty(Amount)} of
                {false, false} ->
                    case Amount >= MinAmount andalso Amount =< MaxAmount of
                        true ->
                            case transaction_cache_util:get_account_details(Account) of
                                {error, _} ->
                                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Details Doesn't Exist">>}};
                                AccountData ->
                                    AccountUserId = maps:get(<<"userid">>, AccountData, -2),
                                    LoggedInUserId = maps:get(<<"userid">>, Request, -1),
                                    case LoggedInUserId of
                                        AccountUserId ->
                                            CurrentAmount = maps:get(<<"amount">>, AccountData),
                                            case CurrentAmount < Amount of
                                                true ->
                                                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Insufficient Balance">>}};
                                                _ ->
                                                    TotalAmount = CurrentAmount - Amount,
                                                    case update_amount_in_account(Account, TotalAmount) of
                                                        {error, _} ->
                                                            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Update Failed">>}};
                                                        _ ->
                                                            transaction_cache_util:invalidate_account_details(Account),
                                                            case create_transaction(AccountData, Amount, 0) of
                                                                error ->
%%                                             pushing the failed messages to queue and process later
                                                                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Transaction Entry Creation Falied">>}};
                                                                TransactionId ->
                                                                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"SUCCESS">>, <<"TransactionId">> => TransactionId, <<"BalanceAmount">> => TotalAmount}}
                                                            end
                                                    end
                                            end;
                                        _ ->
                                            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Account Doesn't Belong to User">>}}
                                    end
                            end;
                        _ ->
                            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Amount Invalid">>}}
                    end
            end;
        _ ->
            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Transaction Not Allowed">>}}
    end.



update_amount_in_account(AccountNo, Amount) ->
    Query = <<"UPDATE account_details SET amount = ? WHERE accountNo = ?">>,
    Params = [Amount, AccountNo],
    case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, true) of
        {error, _} ->
            {error, #{<<"Message">> =>  <<"Amount Updation Failed">>}};
        _ ->
            {ok, #{<<"Message">> => <<"Success">>}}
    end.

create_transaction(AccountData, Amount, TxnType) ->
    AccountNo = maps:get(<<"accountNo">>, AccountData),
    UserId = maps:get(<<"userid">>, AccountData),
    Query = <<"INSERT INTO transactions(userid, accountNo, amount, txn_type) VALUES(?,?,?,?)">>,
    Params = [UserId, AccountNo, Amount, TxnType],
    case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, true) of
        {ok, TransactionId} ->
            transaction_cache_util:update_transactions(AccountNo),
            TransactionId;
        {error, _} ->
            error
    end.

