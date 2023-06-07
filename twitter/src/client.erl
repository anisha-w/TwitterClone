% TwitterClone-DOSP-Anisha Wadhwani
% Handles all client functionality such as :
%  - Registering profile 
%  - Tweet 
%  - Retweet
%  - Display tweets by subscriptions 
%  - Keep track of profiles that the client has subscribed to and the profiles that have taken their subscription. 

-module(client).
-export([register/4,profileHandler/6,extractHashTag/1,extractMention/1,findSymbol/4]). % functions accessible from outside of the module

% Start a new process (thread) for the new profile. Message the process_ID to server to keep track 
register(ServerPID,HandlerPID,Username,_Password)->

    PID = spawn_link(client,profileHandler,[ServerPID,HandlerPID,Username,[],[],[]]), % create independent process for <username> profile 
    ServerPID ! {newUser,Username,PID},
    PID. %Print statement


% Receives and sends messages; Keeps track of all details related to the profile ; Each profile has their own profileHandler process
%  - SubscriberList : Users who have subscribed to current user's content
%  - SelfTweetList : List of current user's tweets 
%  - SubscribedTweetList : Users whom current user has subscribed. 
profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList)->
    %io:format("[DEBUG] Subscriber list ~p~n",[SubscriberList]),
    receive

        {subscribeToUser2,User2}-> %Message from Handler to subscribe to user2's content
            ServerPID! {getUserPIDforSubscribtion,self(),User2}, %Message to Server requesting for PID of user2 
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {subscribtionPID,User2PID}-> %Message from server with PID of user2
            User2PID! {subscriber,Username}, 
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        % messages from simulator%
        %message from User1 who is now subscribed to User2
        %{subscribe,User2}-> %ANISHA
	    {subscriber,User2}->
            profileHandler(ServerPID,HandlerPID,Username,[User2]++SubscriberList,SelfTweetList,SubscribedTweetList);

        % messages for server - client interaction%
        
        {newTweetFeed,User,TweetBody} ->
            io:format("\n\n [~p FEED] ~p : ~p ~n",[Username,User,TweetBody]),
            io:format("Handler ~p and self ~p",[HandlerPID,self()]),
            HandlerPID ! {updateFeed,{User,TweetBody}},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList++[{User,TweetBody}]);
           
        {sendTweet,TweetBody}->
            ServerPID ! {tweet,Username,SubscriberList,TweetBody},

            %check for hashtags 
            HashTagList = extractHashTag(TweetBody),
            lists:foreach(fun(HT)->ServerPID ! {hashtag,Username,TweetBody,HT} end,HashTagList),

            % check for mentions
            MentionList = extractMention(TweetBody),

            lists:foreach(fun(M)->ServerPID ! {mention,Username,TweetBody,M} end,MentionList),
            % send message to mentions
            if length(MentionList) > 0 ->
                ServerPID ! {tweet,Username,MentionList,TweetBody};
            true ->
                ok
            end,

            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList++[TweetBody],SubscribedTweetList);

        {retweet,User,TweetBody}->
           
            TweetPattern = fun(T) -> element(2,T) == TweetBody end,
            Found = lists:any(TweetPattern,SubscribedTweetList),
            if Found == true ->
                ServerPID ! {tweet,Username,SubscriberList,"[Retweet] "++User++":"++TweetBody},
                profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList++[TweetBody],SubscribedTweetList);
                true ->
                   io:format("Tweet not found ~n"),
                   profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList),
                   ok
            end;

        {search_hashtag,QueryKeyword}->
            
            HashTag = string:sub_string(QueryKeyword, 2),
            ServerPID ! {getHashTag,self(),HashTag},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {tweetByHashtag,HashTag,HashTagList}->
            HandlerPID ! {searchResult,{hashtag,HashTag,HashTagList}},
            % io:format("Tweets containing ~p : ~p ~n",[HashTag,HashTagList]),
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {search_mention,QueryKeyword}->
            Mention = string:sub_string(QueryKeyword, 2),
            ServerPID ! {getMention,self(),Mention},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {tweetByMention,Mention,MentionList}->
            % io:format("Tweets containing @~p : ~p~n",[Mention,MentionList]),
            HandlerPID ! {searchResult,{mention,Mention,MentionList}},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        
        {search_subscribed,QueryKeyword}->

            ServerPID ! {getUserPIDforSearchTweets,self(),QueryKeyword},

            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {searchUserPID,UserID2}->
            UserID2 ! {getAllTweets,self()},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {getAllTweets,UserID1}->
            UserID1 ! {allTweets,Username,SelfTweetList},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);
        
        {allTweets,SearchUser,TweetList}->
            HandlerPID ! {allTweets,{SearchUser,TweetList}},
            profileHandler(ServerPID,HandlerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList)
        end.

extractHashTag(TweetBody)->
    SplitTweet = string:split(TweetBody, " ", all),
    HashTagList = findSymbol(SplitTweet,length(SplitTweet),"#",[]),
    HashTagList.

extractMention(TweetBody)->
    SplitTweet = string:split(TweetBody, " ", all),
    MentionList = findSymbol(SplitTweet,length(SplitTweet),"@",[]),
    MentionList.

findSymbol(_SplitTweet,0,_Symbol,ResultList)->
    ResultList;
findSymbol(SplitTweet,Index,Symbol,ResultList)->
    
    Found = string:equal(Symbol, string:sub_string(lists:nth(Index,SplitTweet), 1, 1)),

    ReturnList = if Found->
        findSymbol(SplitTweet,Index-1,Symbol,ResultList++[string:sub_string(lists:nth(Index,SplitTweet), 2)]);
    true->
        findSymbol(SplitTweet,Index-1,Symbol,ResultList)
    end,
    ReturnList.