%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2024 6:03 pm
%%%-------------------------------------------------------------------
-module(transaction_router).
-author("somashekar.b").

-include("transaction_constants.hrl").

%% Application callbacks
-export([start/0,
    stop/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start() ->
    Dispatch = cowboy_router:compile(get_routes()),
    {ok, _} = cowboy:start_clear(http, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}),
    ok
    .


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> term()).
stop() ->
    cowboy:stop_listener(http),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


get_routes() ->
    [
        {'_', [
            {"/", transaction_handler, []},
            {"/api/user/:userid", user_profile_handler, [?GetOrDeleteUserData]},
            {"/api/user/[:userid]", user_profile_handler, [?AddRemoveGetUsers]},

            {"/api/transaction/deposit/:userid", transaction_handler, [?DepositAmount]},
            {"/api/transaction/withdraw/:userid", transaction_handler, [?WithDrawAmount]}
        ]}
    ].