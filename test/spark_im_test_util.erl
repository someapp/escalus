-module(spark_im_test_util).
-compile(export_all).
-import(error_logger).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

access_token_urls(Config)->
  BaseUrl = escalus_config:get_config(spark_api_endpoint, Config),   
  AccessTokenUrl = 
	escalus_config:get_config(spark_create_oauth_accesstoken, Config),
  lists:concat([BaseUrl,AccessTokenUrl]).
  
get_idMap(Config) ->
  escalus_config:get_config(community2brandId, Config).
  
get_access_token_url_for(Jid, 
			 ResourceFullUrl,
 			 AppId,
			 BrandId,
			 Email,
			 Password,
			 ClientSecret)->
  BrandId = lookup_brandid(Jid), 
  Url = get_create_access_token_url(
			 ResourceFullUrl,
 			 AppId,
			 BrandId,
			 Email,
			 Password,
			 ClientSecret),
       
  {ok, Token} = 
	case restc:request(post, json, Url, [200], [],[""]) of
        	{ok, 200, _, Body} -> check_access_token(Body); 
       		{error, Status, _H, _B} -> {error, {Status, _H, _B}}
  	end,
  Token.


get_create_access_token_url(ResourceFullUrl,
 			    AppId,
			    BrandId,
			    Email,
			    Password, ClientSecret)->

   ReplaceOpt = [global, {return, list}],	
   FullUrl = re:replace(ResourceFullUrl, "{brandId}", BrandId, ReplaceOpt),
   FullUrl1 = re:replace(FullUrl, "{applicationId}", AppId, ReplaceOpt),
  
   restc:construct_url(
		FullUrl1,
		[{"client_secret", ClientSecret},
		 {"email", Email},
		 {"password", Password}]).

check_access_token([]) -> {error, missing_body};
check_access_token(Body) ->
    V1 = case proplists:get_value(<<"data">>,Body) of 
		undefined -> {error, missing_body};
		List -> List
	end,
    case proplists:get_value
		  (<<"AccessToken">>, V1) of
	  {error, Reason} -> {error, Reason};
	  [] -> {error, no_access_token_empty_body};
	  undefined -> {error, no_access_token_undefined};
	  Token ->  {ok, erlang:binary_to_list(Token)}
    end.



populate_table(Name, IdMap) when is_atom(Name)->
   error_logger:info_msg("Store idMap ~p into ets table", [IdMap]),
   Tab = ets:new(Name, [set, named_table]),
   lists:map(fun(L)-> true = ets:insert(Tab, L) end, IdMap);
populate_table(_, _)->
   {error, badarg}.   

lookup_brandid(Jid)->
   UserName = jlib:jid_to_string(Jid),
   lookup_brandid_from_user(id_map, UserName). 

lookup_brandid_from_user(Name, UserName) when is_atom(Name) ->
   [MemberId, CommunityId] = split_composite_id(UserName),
   C = case ets:match_object(Name,{'$1',CommunityId,'$2'}) of
    	[{_,_,B}] -> B;
    	[] -> [];
	R -> []
   end,
   error_logger:info_msg("Found BrandId ~p", [C]),
   C. 


split_composite_id(UserName)-> 
   case re:split(UserName,"-")	of
	[MemberId, CommunityId]->  [MemberId, CommunityId];
	[] -> [UserName, ""];	
	R -> [R, ""]
   end.








