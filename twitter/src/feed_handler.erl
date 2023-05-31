% TwitterClone-DOSP-Anisha Wadhwani
-module(feed_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


% State : serverPID, profile
init(Req, State) ->
	
	QsParameters = cowboy_req:parse_qs(Req),
	{_, Username} = lists:keyfind(<<"uname">>, 1, QsParameters),
	{_, _Email} = lists:keyfind(<<"email">>, 1, QsParameters),
	{_, Password} = lists:keyfind(<<"password">>, 1, QsParameters),

	UpdateState = State ++ [{username,Username},{password,Password}],
	
	{cowboy_websocket, Req, UpdateState}.
	
	%{ok,Req,State}.

websocket_init(State) ->
	% io:format("anisha feed 0"),
	{_, ServerPID} = lists:keyfind(serverPID, 1, State),
	{_, Username} = lists:keyfind(username, 1, State),
	{_, Password} = lists:keyfind(password, 1, State),
	ProfilePID = client:register(ServerPID,self(),binary_to_list(Username),Password),
	{[],State++[{profile,ProfilePID}]}.

websocket_handle({text, Data}, State) -> 
	% io:format("anisha websocket handle ~p",[Data]),
	DataMap =  extractData(Data),
	% io:format("\nDataMap ~p ",[DataMap]),
	Type = maps:get("type",DataMap),
	% io:format("\nType ~p ",[Type]),
	if Type == "tweetBody" ->
			% io:format("\nDEBUG ~p : MESSAGE RECIEVED ~p ",[self(),DataMap]),
			%{_, ServerPID} = lists:keyfind(serverPID, 1, State),
			% io:format("\nDEBUG ~p : ServerPID ~p",[self(),ServerPID]),
			
			{_, UserID} = lists:keyfind(profile, 1, State),
			{_, Username} = lists:keyfind(username, 1, State),

			UserID ! {sendTweet,maps:get("data",DataMap)},
			self() ! {updateFeed,{binary_to_list(Username),maps:get("data",DataMap)}};


		Type == "subscribeUser" ->
			% io:format("\nDEBUG ~p : MESSAGE RECIEVED ~p ",[self(),Data]),
			%{_, ServerPID} = lists:keyfind(serverPID, 1, State),
			% io:format("\nDEBUG ~p : ServerPID ~p",[self(),ServerPID]),
			 
			{_, UserID} = lists:keyfind(profile, 1, State),

			UserID ! {subscribeToUser2,maps:get("data",DataMap)};

		Type == "retweet" ->
			io:format("\nDEBUG ~p : MESSAGE RECIEVED ~p ",[self(),Data]),

			%{_, ServerPID} = lists:keyfind(serverPID, 1, State),
			% io:format("\nDEBUG ~p : ServerPID ~p",[self(),ServerPID]),
			{_, Username} = lists:keyfind(username, 1, State),
			{_, UserID} = lists:keyfind(profile, 1, State),
			
			UserID ! {retweet,maps:get("user",DataMap),maps:get("data",DataMap)},
			RetweetUsername = maps:get("user",DataMap),
			RetweetMessage = maps:get("data",DataMap),

			self() ! {updateFeed,{binary_to_list(Username),"[Retweet] "++RetweetUsername++":"++RetweetMessage}}; %doesnt work

		Type == "searchBox" ->
			io:format("\nDEBUG ~p : MESSAGE RECIEVED ~p ",[self(),Data]),

			{_, UserID} = lists:keyfind(profile, 1, State),

			{Query,SearchList} = getSearchType(maps:get("data",DataMap)),
			if 
				Query==hashtag->
					UserID ! {search_hashtag,lists:nth(1,SearchList)};
				Query==mention->
					UserID ! {search_mention,lists:nth(1,SearchList)};
				Query==subscribed->
					UserID ! {search_subscribed,lists:nth(1,SearchList)}
				end,
			
			self() ! {searchBegin,{atom_to_list(Query),lists:nth(1,SearchList)}};

		Type == "searchBySubscriber" ->
			io:format("\nDEBUG ~p : MESSAGE RECIEVED ~p ",[self(),Data]),

			%{_, ServerPID} = lists:keyfind(serverPID, 1, State),
			{_, UserID} = lists:keyfind(profile, 1, State),

			UserID ! {search_subscribed,maps:get("data",DataMap)};
		true ->
			ok
		end,
	
	{[], State}.
	


websocket_info({updateFeed,Data}, State) ->
	% io:format("\n\nanisha websocket info ~p",[Data]),

	PreparedData = "feed|" ++ element(1,Data) ++"|"++ element(2,Data),
	
	% io:format("senddata ~p",[PreparedData]),
	DataString = list_to_binary(PreparedData),
	{reply, {text, << DataString/binary >>},State };
	%{[], State}.

websocket_info({searchBegin,{Query,Keyword}}, State) ->
	PreparedData = "info|Search Result for "++ Query ++ " " ++Keyword,
	DataString = list_to_binary(PreparedData),
	{reply, {text, << DataString/binary >>},State };
	

websocket_info({searchEnd,_Data}, State) ->
	{[], State};

websocket_info({searchResult,{_Type,_Keyword,Tweets}}, State) ->

	TweetList = lists:flatmap(fun(X)->[element(1,X)++"|"++element(2,X)] end, Tweets),
	PreparedSearchResult = concatTweets(TweetList,""),
	PreparedData = "search"++ PreparedSearchResult,
	
	io:format("SERACHRESULT DATA  ~p",[PreparedData]),
	DataString = list_to_binary(PreparedData),
	{reply, {text, << DataString/binary >>},State };

websocket_info({allTweets,{UsernameSearch,Tweets}}, State) ->

	%TweetList = lists:flatmap(fun(X)->[X] end, Tweets),
	PreparedSearchResult = concatTweets(Tweets,""),
	PreparedData = "searchUser|"++UsernameSearch++ PreparedSearchResult,
	
	io:format("SERACHRESULT DATA  ~p",[PreparedData]),
	DataString = list_to_binary(PreparedData),
	{reply, {text, << DataString/binary >>},State }.



concatTweets([],Result)->
	Result;
concatTweets(TweetList,Result)->
	[Tweet | _RestList] = TweetList,
	concatTweets(_RestList,Result++"|"++Tweet).

extractData(Data)->
	DataString = binary_to_list(Data),
	DataSplit = string:tokens(DataString, "\\\""),
	jsonDecoder(DataSplit,#{}).

jsonDecoder(Data,Map) when length(Data)==1->
	Map;
jsonDecoder(Data,Map)->
	% io:format("\nLIST data ~p ",[Data]),
	MapNew = #{lists:nth(2,Data)=>lists:nth(4,Data)},
	UpdatedData = lists:split(4, Data),
	% io:format("split data ~p ",[Data]),
	jsonDecoder(element(2,UpdatedData), maps:merge(Map, MapNew)).

getSearchType(Data)->
	SplitData = string:split(Data, " ", all),
	ResultHash = string:find(Data,"#"),
	ResultMention = string:find(Data,"@"),
	
	SearchList = 
		if ResultHash /= nomatch ->
				findSymbol(SplitData,length(SplitData),"#",[]);
			ResultMention /= nomatch ->
				findSymbol(SplitData,length(SplitData),"@",[]);
			true->
				lists:new()
		end,

	Query = 
		if ResultHash /= nomatch ->
				hashtag;
			ResultMention /= nomatch ->
				mention;
			true->
				subscribed
			end,
	{Query,SearchList}.

findSymbol(_SplitTweet,0,_Symbol,ResultList)->
	ResultList;
findSymbol(SplitTweet,Index,Symbol,ResultList)->
	
	Found = string:equal(Symbol, string:sub_string(lists:nth(Index,SplitTweet), 1, 1)),


	ReturnList = if Found->
		findSymbol(SplitTweet,Index-1,Symbol,ResultList++[lists:nth(Index,SplitTweet)]);
	true->
		findSymbol(SplitTweet,Index-1,Symbol,ResultList)
	end,


	ReturnList.
