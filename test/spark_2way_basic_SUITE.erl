%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(spark_2way_basic_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, messages}].

groups() ->
    [{messages, [sequence], 
     [aa2aa_2way_should_pass_story,
      aa2sub_2way_should_pass_story,
      aa2nonsub_2way_should_pass_story,
      sub2sub_2way_should_pass_story,
      sub2nonsub_2way_should_block_pass_story,
      nonsub2non_2way_should_block_pass_story,
      nonsub2sub_2way_should_block_pass_story,
      nonsub2aa_2way_should_block_pass_story           
      ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
	error_logger:info_msg("Init Test Suite with Config ~p~n",[Config]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
	error_logger:info_msg("End Test Suite with Config ~p~n",[Config]),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
	error_logger:info_msg("Init Test Group ~p with Config ~p~n",[GroupName, Config]),
    escalus:create_users(Config).

end_per_group(GroupName, Config) ->
	error_logger:info_msg("End Test Group ~p with Config ~p~n",[GroupName, Config]),
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
	error_logger:info_msg("Init Test Case ~p with Config ~p~n",[CaseName, Config]),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
	error_logger:info_msg("End Test Case ~p with Config ~p~n",[CaseName, Config]),
    escalus:end_per_testcase(CaseName, Config).



ensure_dependency()->
    Apps = [
	    	inets,
	    	crypto, 
        	public_key,
        	ssl,
	    	restc, 
            ets ],
    app_util:start_apps(Apps),    
    ok.



%%--------------------------------------------------------------------
%% Message tests
%%--------------------------------------------------------------------

aa2aa_2way_should_pass_story(Config) ->
    escalus:story(Config, 
    [{allaccess, 1},{allaccess2, 1}], 
    
    fun(AllAccess1, AllAccess2) ->
    	Msg = timestamp_as_msg("AA1 to AA2"),
        escalus:send(AllAccess1,
        	 escalus_stanza:chat_to(AllAccess2, Msg)),

        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(AllAccess2))

    end).



aa2sub_2way_should_pass_story(Config) ->
    escalus:story(Config,
    [{allaccess1, 1},{subscribed1, 1}], 
    
    fun(AllAccess, Sub) ->
    	Msg = timestamp_as_msg("AA1 to Sub"),
       
        escalus:send(AllAccess, 
        	escalus_stanza:chat_to(Sub, Msg)),

        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(Sub))

    end).



aa2nonsub_2way_should_pass_story(Config) ->

    escalus:story(Config, 
    [{allaccess1, 1},{notsubscribed1, 1}], 
    fun(AllAccess, NonSub) ->
    	Msg = timestamp_as_msg("AA1 to NonSub"),

        escalus:send(AllAccess,
        	 escalus_stanza:chat_to(NonSub, Msg)),

        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(NonSub))

    end).


sub2sub_2way_should_pass_story(Config) ->

    escalus:story(Config,
    
    [{subscribed1, 1},{subscribed2, 1}], 
    
    fun(Sub1, Sub2) ->
  	 	Msg = timestamp_as_msg("Sub1 to Sub2"),

        
        escalus:send(Sub1, 
        	escalus_stanza:chat_to(Sub2, Msg)),

  
        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(Sub2))

    end).


sub2nonsub_2way_should_block_pass_story(Config) ->

    escalus:story(Config, 
    
    [{subscribed1, 1},{nonsubscribed1, 1}], 
    
    fun(Sub1, NonSub1) ->

  	 	Msg = timestamp_as_msg("Sub1 to NonSub1"),
    
        escalus:send(Sub1, escalus_stanza:chat_to(NonSub1, Msg)),

  
        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(NonSub1))

    end).


nonsub2non_2way_should_block_pass_story(Config) ->

    escalus:story(Config,
     
       [{nonsubscribed1, 1},{nonsubscribed2, 1}], 
       
       fun(NonSub1, NonSub2) ->

  	 	Msg = timestamp_as_msg("NonSub1 to NonSub2"),
        
        escalus:send(NonSub1, escalus_stanza:chat_to(NonSub2, Msg)),
  
        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(NonSub2))

    end).


nonsub2sub_2way_should_block_pass_story(Config) ->

    escalus:story(Config,
       
       [{nonsubscribed1, 1},{subscribed1, 1}], 
       
       
        fun(NonSub1, Sub1) ->
  	 	Msg = timestamp_as_msg("NonSub1 to Sub1"),        
        
        escalus:send(NonSub1, escalus_stanza:chat_to(Sub1, Msg)),

  
        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(Sub1))

    end).


nonsub2aa_2way_should_block_pass_story(Config) ->           

    escalus:story(Config,
                  
       [{nonsubscribed1, 1},{allaccess1, 1}], 
              
       fun(NonSub1, AA1) ->

   	 	Msg = timestamp_as_msg("NonSub1 to AA1"),              
        escalus:send(NonSub1, escalus_stanza:chat_to(AA1, Msg)),

  
        escalus:assert(is_chat_message, [Msg],
                       escalus:wait_for_stanza(AA1))

    end).

iso_8601_fmt(DateTime)->
   {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
   io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.B:~2.10.0B",
   	[Year, Month, Day, Hour, Min, Sec]).

timestamp_as_msg(Seed)->
   DateTime = erlang:localtime(),
   Str = iso_8601_fmt(DateTime),
   <<Str/binary,Seed/binary>>.
  
