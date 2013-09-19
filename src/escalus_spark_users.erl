-module(escalus_spark_users).
-export([login_users/1,
         login_users/2,
         logout_users/1,
         logout_users/2
]).

login_users(Config) ->
    login_users(Config, all).

login_users(Config, Who) ->
    Users = escalus_users:get_users(Who),
    LoginResults = 
    	[login_user(Config, User) || User <- Users],
    lists:foreach(fun verify_login/1, LoginResults),
    [{escalus_users, Users}] ++ Config.

login_user(Config, {_Name, UserSpec}) ->

	ok.    
    
logout_users(Config) ->
    logout_users(Config, all).

logout_users(Config, Who) ->
    Users = case Who of
        config ->
            escalus_config:get_config(escalus_users, Config, []);
        _ ->
            escalus_users:get_users(Who)
    end,
    [logout_user(Config, User) || User <- Users].

verify_login({ok, result, _}) ->

    ok.



logout_user(Config, {_Name, UserSpec}) ->
	
    ok.
