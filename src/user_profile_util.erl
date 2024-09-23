%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2024 11:14 pm
%%%-------------------------------------------------------------------
-module(user_profile_util).
-author("somashekar.b").

-include("transaction_constants.hrl").

%% API
-export([authorize_user/1,
    fetch_all_users/0,
    add_user/1,
    encrypt_password/1,
    update_user/1,
    get_user_data/1,
    delete_user/1,
    is_nil_or_empty/1]).

fetch_all_users() ->
    case transaction_cache_util:get_all_users() of
        {error, _} ->
            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Users Data not found">>}};
        Data ->
            {?HTTP_OK_RESPONSE_CODE, Data}
    end.

add_user(Request) ->
    Result = lists:foldl(
        fun(Body, Acc) ->
            UserName = maps:get(<<"username">>, Body, <<>>),
            Password =  maps:get(<<"password">>, Body, <<>>),
            Role = maps:get(<<"role">>, Body, <<>>),
            case {is_nil_or_empty(UserName), is_nil_or_empty(Password), is_nil_or_empty(Role)} of
                {false, false, false} ->
                    Result = case add_to_db(UserName, Password, Role) of
                                 {error, Error} ->
                                     Error;
                                 {ok, UserId} ->
                                     case insert_to_users_data(Body, UserId) of
                                         success ->
                                             transaction_cache_util:invalidate_user(UserId),
                                             transaction_cache_util:invalidate_allusers(),
                                             case transaction_util:create_user_account(Body#{<<"userid">> => UserId}) of
                                                 {?HTTP_BAD_REQUEST_CODE, Err} ->
                                                     #{<<"Message">> => <<"Success">>, <<"Id">> => UserId, <<"accountMsg">> => maps:get(<<"Message">>, Err, <<"Failure">>)};
                                                 {_, Account} ->
                                                     case maps:is_key(<<"accountNo">>, Account) of
                                                         true ->
                                                             AccountNo = maps:get(<<"accountNo">>, Account),
                                                             #{<<"Message">> => <<"Success">>, <<"Id">> => UserId, <<"accountNo">> => AccountNo};
                                                         _ ->
                                                             #{<<"Message">> => <<"Success">>, <<"Id">> => UserId, <<"accountMsg">> => maps:get(<<"Message">>, Account, <<"Failure">>)}
                                                     end
                                             end;
                                         _ ->
                                             do_db_rollback(UserId),
                                             #{<<"Message">> => <<"Failure">>}
                                     end
                             end,
                    [Result | Acc];
                _ ->
                    [#{<<"Message">> => <<"Invalid Inputs">>} | Acc]
            end
        end, [], Request
    ),
    {?HTTP_OK_RESPONSE_CODE, Result}.

add_to_db(UserName, Password, Role) ->
    Query = <<"INSERT INTO users(username, password, role) VALUES(?, ?, ?)">>,
    Params = [UserName, encrypt_password(Password), Role],
    case transaction_sql_util:query(?DBWritePool, Query, Params, 10000, true) of
        {error, _Err} ->
            {error, #{<<"Message">> => <<"Add To DB Failed">>}};
        {ok, UserId} ->
            {ok, UserId}
    end.

insert_to_users_data(Request, UserId) ->
    Username = maps:get(<<"username">>, Request),
    Email = maps:get(<<"email">>, Request),
    Phone = maps:get(<<"phone">>, Request),
    Address = maps:get(<<"address">>, Request),
    Query = <<"INSERT INTO users_data(userid, username, email, phone, address) VALUES (?,?,?,?,?)">>,
    Params = [UserId, Username, Email, Phone, Address],
    case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, false) of
        {error, _} ->
            error;
        _ ->
            success
    end.

do_db_rollback(UserId) ->
    Query = <<"DELETE FROM users WHERE userid = ?">>,
    Params = [UserId],
    transaction_sql_util:query(?DBWritePool, Query, Params, 1000, false).

update_user(Request) ->
    UserId = maps:get(<<"userId">>, Request, <<>>),
    case is_nil_or_empty(UserId) of
        true ->
            {?HTTP_BAD_REQUEST_CODE, #{<<"Message">> => <<"Invalid Inputs">>}};
        _ ->
            case transaction_cache_util:get_user_data(UserId) of
                {error, _} ->
                    {?HTTP_NOT_FOUND_CODE, #{<<"Message">> => <<"UserId Not Found">>}};
                UserData ->
                    {UserName, Phone, Address, Email} = get_params(Request, UserData),
                    Query = <<"UPDATE users_data SET username = ?, phone = ?, address = ?, email = ? WHERE userid = ?">>,
                    Params = [UserName, Phone, Address, Email, UserId],
                    case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, true) of
                        {error, _} ->
                            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Update Failed">>}};
                        _ ->
                            transaction_cache_util:invalidate_allusers(),
                            transaction_cache_util:invalidate_user(UserId),
                            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Success">>}}
                    end
            end
    end.

get_params(Request, ExistingData) ->
    UserName  = maps:get(<<"username">>, Request, maps:get(<<"username">>, ExistingData, <<>>)),
    Phone = maps:get(<<"phone">>, Request, maps:get(<<"phone">>, ExistingData, <<>>)),
    Address = maps:get(<<"address">>, Request, maps:get(<<"address">>, ExistingData, <<>>)),
    Email = maps:get(<<"email">>, Request, maps:get(<<"email">>, ExistingData, <<>>)),
    {UserName, Phone, Address, Email}.

get_user_data(UserId) ->
    case transaction_cache_util:get_user_data(UserId) of
        {error, _} ->
            {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"UserDetails Not Found">>}};
        Data ->
            FinalAccounts = case transaction_cache_util:get_user_accounts(UserId) of
                            {error, _} ->
                                [];
                            Accounts ->
                                Accounts
                        end,
            {?HTTP_OK_RESPONSE_CODE, Data#{<<"accounts">> => FinalAccounts}}
    end.

delete_user(UserId) ->
    case is_nil_or_empty(UserId) of
        true ->
            {?HTTP_BAD_REQUEST_CODE, #{<<"Message">> => <<"Invalid Inputs">>}};
        _ ->
            Query = <<"DELETE FROM users_data where userid = ?">>,
            Params = [UserId],
            case transaction_sql_util:query(?DBWritePool, Query, Params, 1000, ok) of
                {error, _} ->
                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"User Not Deleted">>}};
                _ ->
                    transaction_cache_util:invalidate_allusers(),
                    transaction_cache_util:invalidate_user(UserId),
                    {?HTTP_OK_RESPONSE_CODE, #{<<"Message">> => <<"Success">>}}
            end
    end.

encrypt_password(Password) ->
    base64:encode(Password).

decrypt_password(Password) ->
    base64:decode(Password).

authorize_user(Req0) ->
    case fetch_credentials(Req0) of
        {?AdminUserName, ?AdminPassword} ->
            {true, admin};
        {undefined, undefined} ->
            false;
        {UserId, Password} ->
            case transaction_cache_util:get_user_login(UserId) of
                {error, _} ->
                    false;
                UserData ->
                    case UserId =:= integer_to_binary(maps:get(<<"userid">>, UserData, -1)) andalso Password =:= decrypt_password(maps:get(<<"password">>, UserData)) of
                        true ->
                            case maps:get(<<"role">>, UserData, <<>>) of
                                <<"admin">> ->
                                    {true, admin};
                                _ ->
                                    {true, user, UserId}
                            end;
                        _ ->
                            false
                    end
            end;
        _ ->
            false
    end.

fetch_credentials(Req0) ->
    AuthHeader = cowboy_req:header(<<"authorization">>, Req0, undefined),
    case AuthHeader of
        undefined ->
            {undefined, undefined};
        _ ->
            case binary:split(AuthHeader, <<" ">>) of
                [<<"Basic">>, EncodedCredentials] ->
                    decode_credentials(EncodedCredentials);
                _ ->
                    {undefined, undefined}
            end
    end.

decode_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<":">>) of
        [UserName, PassWord] ->
            {UserName, PassWord};
        _ ->
            {undefined, undefined}
    end.

is_nil_or_empty(Data) ->
    case Data of
        <<>> -> true;
        null -> true;
        undefined -> true;
        <<"null">> -> true;
        _ ->
            false
    end.