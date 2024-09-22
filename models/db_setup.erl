%%%-------------------------------------------------------------------
%%% @author somashekar.b
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2024 1:26 am
%%%-------------------------------------------------------------------
-module(db_setup).
-author("somashekar.b").

%% API
-export([
    initialize_db/1
]).

initialize_db(PoolName) ->
    create_user_table(PoolName),
    create_userdata_table(PoolName),
    create_accounts_table(PoolName),
    create_transactions_table(PoolName).

create_user_table(PoolName) ->
    Query = "
    CREATE TABLE IF NOT EXISTS `users` (
      `userid` int NOT NULL AUTO_INCREMENT,
      `username` varchar(100) DEFAULT NULL,
      `password` varchar(200) DEFAULT NULL,
      `onboarddate` datetime DEFAULT CURRENT_TIMESTAMP,
      `role` varchar(50) NOT NULL,
      PRIMARY KEY (`userid`)
    ) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci",

    case palma:pid(PoolName) of
        Pid when is_pid(Pid) ->
            case mysql:query(Pid, Query) of
                {error, Reason} ->
                    io:format("Users Table creation failed with error ~p~n",[Reason]);
                _ ->
                    io:format("Users Table Created ~n")
            end;
        _ ->
            io:format("No Connection Found For Pool ~p~n",[PoolName])
    end.


create_userdata_table(PoolName) ->
    Query = "
    CREATE TABLE IF NOT EXISTS `users_data` (
      `userid` int NOT NULL,
      `username` varchar(100) DEFAULT NULL,
      `email` varchar(100) DEFAULT NULL,
      `phone` varchar(20) DEFAULT NULL,
      `address` varchar(255) DEFAULT NULL,
      `onboarddate` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
      `updated_datetime` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`userid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci",

    case palma:pid(PoolName) of
        Pid when is_pid(Pid) ->
            case mysql:query(Pid, Query) of
                {error, Reason} ->
                    io:format("UsersData Table creation failed with error ~p~n",[Reason]);
                _ ->
                    io:format("UsersData Table Created ~n")
            end;
        _ ->
            io:format("No Connection Found For Pool ~p~n",[PoolName])
    end.


create_accounts_table(PoolName) ->
    Query = "
    CREATE TABLE IF NOT EXISTS `account_details` (
      `accountNo` int NOT NULL AUTO_INCREMENT,
      `userid` int DEFAULT NULL,
      `amount` decimal(10,2) DEFAULT NULL,
      `onboardeddatetime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
      `updateddatetime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (`accountNo`),
      KEY `fk_user` (`userid`),
      CONSTRAINT `fk_user` FOREIGN KEY (`userid`) REFERENCES `users` (`userid`),
      CONSTRAINT `chk_amount` CHECK ((`amount` >= 0))
    ) ENGINE=InnoDB AUTO_INCREMENT=1002 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci",

    case palma:pid(PoolName) of
        Pid when is_pid(Pid) ->
            case mysql:query(Pid, Query) of
                {error, Reason} ->
                    io:format("Accounts Table creation failed with error ~p~n",[Reason]);
                _ ->
                    io:format("Accounts Table Created ~n")
            end;
        _ ->
            io:format("No Connection Found For Pool ~p~n",[PoolName])
    end.


create_transactions_table(PoolName) ->
    Query = "
    CREATE TABLE IF NOT EXISTS `transactions` (
      `transaction_id` int NOT NULL AUTO_INCREMENT,
      `userid` int DEFAULT NULL,
      `accountNo` int DEFAULT NULL,
      `amount` decimal(10,2) DEFAULT NULL,
      `txn_type` tinyint DEFAULT NULL,
      `transaction_datetime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (`transaction_id`),
      KEY `userid` (`userid`),
      KEY `accountNo` (`accountNo`),
      CONSTRAINT `transactions_ibfk_1` FOREIGN KEY (`userid`) REFERENCES `users` (`userid`),
      CONSTRAINT `transactions_ibfk_2` FOREIGN KEY (`accountNo`) REFERENCES `account_details` (`accountNo`)
    ) ENGINE=InnoDB AUTO_INCREMENT=100038 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci",

    case palma:pid(PoolName) of
        Pid when is_pid(Pid) ->
            case mysql:query(Pid, Query) of
                {error, Reason} ->
                    io:format("Transactions Table creation failed with error ~p~n",[Reason]);
                _ ->
                    io:format("Transactions Table Created ~n")
            end;
        _ ->
            io:format("No Connection Found For Pool ~p~n",[PoolName])
    end.