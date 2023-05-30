% TwitterClone-DOSP-Anisha Wadhwani
% Handles all functionality related to simulating actions like 
%  - Registering user profile 
%  - User 1 subscribing to User 2 
%  - User Tweeting 
%  - Searching tweets 
%  - User Retweeting 
% These actions are simulated by first creating N user profiles, and then sending messages to the user_profile process to initiate the respective functions.

-module(simulator).

-export([simulateUsers/2,subscribe/3,tweet/3,search/4,retweet/3]).

% Register N Users (NumUsers) : Call register function from client module to create a profile for User_K
simulateUsers(ServerPID,0)->
    ServerPID ! {getUserListMap,self()},
    % Print list of all usernames registered
    receive
    {userMap,UserListMap}  ->
        io:format("Users created :: ~n"),
        maps:foreach(fun(K,_V)->io:format("~p~n",[K])end, UserListMap),
        io:format("~n")
    end;

simulateUsers(ServerPID,NumUsers)->
   client:register(ServerPID,"user"++integer_to_list(NumUsers),"pwd"++NumUsers),
   simulateUsers(ServerPID,NumUsers-1).

% Invoking the subscribe function simulates the action "User 1 subscribes to User 2"
subscribe(ServerPID,User1,User2)->
    ServerPID! {getUserListMap,self()}, 
    receive
    {userMap,UserListMap}  ->
        %io:format("~p~n",[User1]),
        UserID2 = maps:get(User2, UserListMap),
        UserID2 ! {subscribe,User1},
        subscribed
end.

% Invoking the tweet function simulates the action "User 1 tweeting"
tweet(ServerPID,User1,Tweet)->
    ServerPID! {getUserListMap,self()}, 
    receive
    {userMap,UserListMap}  ->
        UserID1 = maps:get(User1, UserListMap),
        UserID1 ! {sendTweet,Tweet},
        ok
end.

% Invoking the search function simulates the action "User 1 searching by hashtag/mention/subscribed"
search(ServerPID,Query,User,QueryKeyword)->
    ServerPID! {getUserListMap,self()}, 
    receive
    {userMap,UserListMap}  ->
        UserID = maps:get(User, UserListMap),
        ok
    end,
    if 
        Query=="hashtag"->
            UserID ! {search_hashtag,QueryKeyword};
        Query=="mention"->
            UserID ! {search_mention,QueryKeyword};
        Query=="subscribed"->
            UserID ! {search_subscribed,QueryKeyword}
        end,
    ok.

% Invoking the retweet function simulates the action "User 1 retweeting a tweet"
retweet(ServerPID,User1,Tweet)->
    
    ServerPID! {getUserListMap,self()}, 
    receive
    {userMap,UserListMap}  ->
        UserID1 = maps:get(User1, UserListMap),
        io:fwrite("~p ~n",[UserID1]),
        UserID1 ! {retweet,Tweet},
        ok
end.


