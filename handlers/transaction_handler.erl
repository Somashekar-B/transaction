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

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> ->
            handle_get_request(Req0, State);
        <<"POST">> ->
            handle_post_request(Req0, State);
        <<"PUT">> ->
            handle_put_request(Req0, State)
    end.

handle_get_request(Req0, State) ->
    UserId = cowboy_req:binding(userid, Req0),
    AccountNo = cowboy_req:binding(accountNo, Req0),
    {Status, Result} = case State of
                           [?GetUserTransactions] ->
                               case user_profile_util:authorize_user(Req0) of
                                   {true, user, LoggedUserId} ->
                                       case UserId of
                                           LoggedUserId ->
                                               transaction_util:get_all_transactions_for_user_account(UserId, AccountNo);
                                           _ ->
                                               {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                                       end;
                                   {true, admin} ->
                                       transaction_util:get_all_transactions_for_user_account(UserId, AccountNo);
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           _ ->
                               {?HTTP_NOT_FOUND_CODE, #{<<"Message">> => <<"Method Not Allowed">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.

handle_post_request(Req0, State) ->
    {ok, Request, _} = cowboy_req:read_body(Req0),
    Body = jiffy:decode(Request, [return_maps]),
    {Status, Result} = case State of
                           [?CreateUserAccount] ->
                               case user_profile_util:authorize_user(Req0) of
                                   {true, admin} ->
                                       transaction_util:create_user_account(Body);
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           _ ->
                               {?HTTP_NOT_FOUND_CODE, #{<<"Message">> => <<"Method Not Allowed">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.

handle_put_request(Req0, State) ->
    UserId = cowboy_req:binding(userid, Req0),
    {ok, Request, _} = cowboy_req:read_body(Req0),
    Body = jiffy:decode(Request, [return_maps]),
    {Status, Result} = case State of
                           [?DepositAmount] ->
                               case user_profile_util:authorize_user(Req0) of
                                   {true, user, LoggedUserId} ->
                                       case UserId of
                                           LoggedUserId ->
                                               transaction_util:deposit_amount(Body#{<<"userid">> => binary_to_integer(UserId)});
                                           _ ->
                                               {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                                       end;
                                   {true, admin} ->
                                       transaction_util:deposit_amount(Body#{<<"userid">> => binary_to_integer(UserId)});
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           [?WithDrawAmount] ->
                               case user_profile_util:authorize_user(Req0) of
                                   {true, user, LoggedUserId} ->
                                       case UserId of
                                           LoggedUserId ->
                                               transaction_util:withdraw_amount(Body#{<<"userid">> => binary_to_integer(UserId)});
                                           _ ->
                                               {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                                       end;
                                   {true, admin} ->
                                       transaction_util:withdraw_amount(Body#{<<"userid">> => binary_to_integer(UserId)});
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           _ ->
                               {?HTTP_NOT_FOUND_CODE, #{<<"Message">> => <<"Method Not Allowed">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.