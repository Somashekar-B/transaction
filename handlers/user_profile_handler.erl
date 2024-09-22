%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2024 11:03 pm
%%%-------------------------------------------------------------------
-module(user_profile_handler).
-author("somashekar.b").

-include("transaction_constants.hrl").

%% API
-export([
    init/2
]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            handle_post_request(Req0, State);
        <<"PUT">> ->
            handle_put_request(Req0, State);
        <<"GET">> ->
            handle_get_request(Req0, State);
        <<"DELETE">> ->
            handle_delete_request(Req0, State)
    end.

handle_post_request(Req0, State) ->
    IsAdmin = user_profile_util:authorize_user(Req0),
    {Status, Result} = case IsAdmin of
                           {true, admin} ->
                               {ok, Body, _} = cowboy_req:read_body(Req0),
                               Request = jiffy:decode(Body, [return_maps]),
                               user_profile_util:add_user(Request);
                           _ ->
                               {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.



handle_put_request(Req0, State) ->
    IsAdmin = user_profile_util:authorize_user(Req0),
    {Status, Result} = case IsAdmin of
                           {true, admin} ->
                               {ok, Body, _} = cowboy_req:read_body(Req0),
                               Request = jiffy:decode(Body, [return_maps]),
                               user_profile_util:update_user(Request);
                           _ ->
                               {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.

handle_get_request(Req0, State) ->
    UserId = cowboy_req:binding(userid, Req0, undefined),
    {Status, Result} = case State of
                           [?AddRemoveGetUsers] ->
                               IsAdmin = user_profile_util:authorize_user(Req0),
                               case IsAdmin of
                                   {true, admin} ->
                                       user_profile_util:fetch_all_users();
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           [?GetOrDeleteUserData] ->
                               case user_profile_util:authorize_user(Req0) of
                                   {true, admin} ->
                                       user_profile_util:get_user_data(UserId);
                                   {true, user, LoggedUserId} ->
                                       case LoggedUserId of
                                           UserId ->
                                               user_profile_util:get_user_data(UserId);
                                           _ ->
                                               {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                                       end;
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           _ ->
                               {?HTTP_NOT_FOUND_CODE, #{<<"Message">> => <<"Method Not Allowed">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.

handle_delete_request(Req0, State) ->
    UserId = cowboy_req:binding(userid, Req0, undefined),
    {Status, Result} = case State of
                           [?GetOrDeleteUserData] ->
                               IsAdmin = user_profile_util:authorize_user(Req0),
                               case IsAdmin of
                                   {true, admin} ->
                                       user_profile_util:delete_user(UserId);
                                   _ ->
                                       {?HTTP_UNAUTHORIZED_CODE, #{<<"Message">> => <<"UnAuthorized">>}}
                               end;
                           _ ->
                               {?HTTP_NOT_FOUND_CODE, #{<<"Message">> => <<"Method Not Allowed">>}}
                       end,
    Res = cowboy_req:reply(Status, ?DEFAULT_HTTP_JSON_HEADER, jiffy:encode(Result), Req0),
    {ok, Res, State}.