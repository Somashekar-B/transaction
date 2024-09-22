%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2024 10:35 pm
%%%-------------------------------------------------------------------
-author("somashekar.b").

-define(Application, transaction).

-define(DEFAULT_HTTP_JSON_HEADER, #{<<"content-type">> => <<"application/json; charset=utf-8">>,
    <<"connection">> => <<"keep-alive">>}).
-define(PORT, 8000).
-define(HTTP_OK_RESPONSE_CODE, 200).
-define(HTTP_UNAUTHORIZED_CODE, 401).
-define(HTTP_NOT_FOUND_CODE, 404).
-define(HHTP_BAD_REQUEST_CODE, 400).

-define(AdminUserName, <<"admin">>).
-define(AdminPassword, <<"payNow123">>).

-define(DBReadPool, dbro_pool).
-define(DBWritePool, dbrw_pool).

-define(AddRemoveGetUsers, "addRemoveGetUser").
-define(GetAllUsers, "getallusers").
-define(GetOrDeleteUserData, "getordeleteuserdata").
-define(Users_Table, usersdata).
