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
    Token = spark_im_test_util:create_password(Config),
    {ok, Conn, Props} = escalus_connection:start(Config),
    Props0 = update_field(Props, password, Token),
    LoginResult = escalus_session:authenticate(Conn, Props0),    
	LoginResult.    
	
update_field([],_,_) -> [];
update_field(Props, Key, Val) ->
    Props0 = lists:keyreplace(password, Props, {Key, Val}), 
    Props0.
    
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
    error_logger:info("Login success: ~p~n", result),
    ok;
verify_login({_, result, _}) ->
	error_logger:info("Login failed: ~p~n", result),
    {error, {login_failed, result}}.


logout_user(Config, {_Name, UserSpec}) ->
	
    ok.
