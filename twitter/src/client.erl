% TwitterClone-DOSP-Anisha Wadhwani
% Handles all client functionality such as :
%  - Registering profile 
%  - Tweet 
%  - Retweet
%  - Display tweets by subscriptions 
%  - Keep track of profiles that the client has subscribed to and the profiles that have taken their subscription. 

-module(client).
-export([register/3,profileHandler/5,extractHashTag/1,extractMention/1,findSymbol/4]). % functions accessible from outside of the module

% Start a new process (thread) for the new profile. Message the process_ID to server to keep track 
register(ServerPID,Username,_Password)->

    PID = spawn_link(client,profileHandler,[ServerPID,Username,[],[],[]]), % create independent process for <username> profile 
    ServerPID ! {newUser,Username,PID},
    PID. %Print statement


% Receives and sends messages; Keeps track of all details related to the profile ; Each profile has their own profileHandler process
%  - SubscriberList : Users who have subscribed to current user's content
%  - SelfTweetList : List of current user's tweets 
%  - SubscribedTweetList : Users whom current user has subscribed. 
profileHandler(ServerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList)->
    %io:format("[DEBUG] Subscriber list ~p~n",[SubscriberList]),
    receive

        % messages from simulator%
        {subscribe,User2}->
            profileHandler(ServerPID,Username,[User2]++SubscriberList,SelfTweetList,SubscribedTweetList);

        % messages for server - client interaction%
        
        {newTweetFeed,User,TweetBody} ->
            io:format("[~p FEED] ~p : ~p ~n",[Username,User,TweetBody]),
            profileHandler(ServerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList++[{User,TweetBody}]);
           
        {sendTweet,TweetBody}->
            ServerPID ! {tweet,Username,SubscriberList,TweetBody},

            %check for hashtags 
            HashTagList = extractHashTag(TweetBody),
            lists:foreach(fun(HT)->ServerPID ! {hashtag,Username,TweetBody,HT} end,HashTagList),

            % check for mentions
            MentionList = extractMention(TweetBody),

            lists:foreach(fun(M)->ServerPID ! {mention,Username,TweetBody,M} end,MentionList),
            % send message to mentions
            ServerPID ! {tweet,Username,MentionList,TweetBody},

            profileHandler(ServerPID,Username,SubscriberList,SelfTweetList++[TweetBody],SubscribedTweetList);

        {retweet,TweetBody}->
            TweetPattern = fun(T) -> element(2,T) == TweetBody end,
            Found = lists:any(TweetPattern,SubscribedTweetList),
            if Found == true ->
                ServerPID ! {tweet,Username,SubscriberList,TweetBody},
                profileHandler(ServerPID,Username,SubscriberList,SelfTweetList++[TweetBody],SubscribedTweetList);
                true ->
                   io:format("Tweet not found ~n"),
                   profileHandler(ServerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList),
                   ok
            end;

        {search_hashtag,QueryKeyword}->
            
            HashTag = string:sub_string(QueryKeyword, 2),
            ServerPID ! {getHashTag,self(),HashTag},
            profileHandler(ServerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {tweetByHashtag,HashTag,HashTagList}->
            io:format("Tweets containing #~p : ~p ~n",[HashTag,HashTagList]);

        {search_mention,QueryKeyword}->
            Mention = string:sub_string(QueryKeyword, 2),
            ServerPID ! {getMention,self(),Mention},
            profileHandler(ServerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList);

        {tweetByMention,Mention,MentionList}->
            io:format("Tweets containing @~p : ~p~n",[Mention,MentionList]),
            profileHandler(ServerPID,Username,SubscriberList,SelfTweetList,SubscribedTweetList)

        
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
    if Found->
        ReturnList = findSymbol(SplitTweet,Index-1,Symbol,ResultList++[string:sub_string(lists:nth(Index,SplitTweet), 2)]);
    true->
        ReturnList = findSymbol(SplitTweet,Index-1,Symbol,ResultList)
    end,
    ReturnList.