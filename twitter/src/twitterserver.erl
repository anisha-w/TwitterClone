% TwitterClone-DOSP-Anisha Wadhwani
% Manages list of all users and a global list of tweets grouped by HashTag and a global list of tweets grouped by Mentions

-module(twitterserver).
-export([start_server/0,server/3]).

start_server()->
    ServerPid = spawn_link(twitterserver,server,[maps:new(),maps:new(),maps:new()]),
    ServerPid.

server(UserListMap,HashTagMap,MentionMap)->
    receive
        % Message Received: Add new user to list
        {newUser,Username,Pid}->
            UserBufferMap = maps:put(Username,Pid,UserListMap),
            server(UserBufferMap,HashTagMap,MentionMap);
        
	{getUserPIDforSubscribtion,User1,User2} ->
            UserID2 = maps:get(User2, UserListMap),
            User1 ! {subscribtionPID,UserID2},
            server(UserListMap,HashTagMap,MentionMap);

        {getUserPIDforSearchTweets,User1,User2} ->
            UserID2 = maps:get(User2, UserListMap),
            User1 ! {searchUserPID,UserID2},
            server(UserListMap,HashTagMap,MentionMap);
        
	% Message Received: Send tweet to profiles in SubscriberList
        {tweet,Username,SubscriberList,Tweet}->
            lists:foreach(fun(P)-> maps:get(P,UserListMap) ! {newTweetFeed,Username,Tweet} end , SubscriberList),
            server(UserListMap,HashTagMap,MentionMap);
           
        % Message Received: Add tweet into Hashtag global list 
        {hashtag,Username,Tweet,HashTag} ->
            HashTagList = maps:get(HashTag,HashTagMap,[]),
            NewHashTagList = [{Username,Tweet}]++HashTagList,
            HashTagMapBuffer = maps:put(HashTag,NewHashTagList,HashTagMap),
            server(UserListMap,HashTagMapBuffer,MentionMap);

        % Message Received: Send all tweets by Hashtag 
        {getHashTag,UserPID,HashTag} ->
            HashTagList = maps:get(HashTag,HashTagMap,[]),
            UserPID ! {tweetByHashtag,HashTag,HashTagList},
            server(UserListMap,HashTagMap,MentionMap);

        % Message Received: Add tweet into Mention global list 
        {mention,Username,Tweet,Mention} ->
            MentionList = maps:get(Mention,MentionMap,[]),
            NewMentionList = [{Username,Tweet}]++MentionList,
            MentionMapBuffer = maps:put(Mention,NewMentionList,MentionMap),
            server(UserListMap,HashTagMap,MentionMapBuffer);

        % Message Received: Send all tweets by Mention 
        {getMention,UserPID,Mention} ->
            MentionList = maps:get(Mention,MentionMap,[]),
            UserPID ! {tweetByMention,Mention,MentionList},
            server(UserListMap,HashTagMap,MentionMap);

        % Message Received: Send list of users
        {getUserListMap,Simulator}->
            %io:format("received request~n"),
            Simulator ! {userMap,UserListMap},
            server(UserListMap,HashTagMap,MentionMap)

end.


 