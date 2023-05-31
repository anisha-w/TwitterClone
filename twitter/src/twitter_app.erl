% TwitterClone-DOSP-Anisha Wadhwani
% Handles webpage routing 
-module(twitter_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	
	ServerPID = twitterserver:start_server(),

	Dispatch = cowboy_router:compile([
		%% {HostMatch, list({PathMatch, Handler module name (erl), InitialState})}
        {'_', [
			{"/[...]", cowboy_static, {priv_dir, twitter, "assets"}}]}  % html pages path ; static file ; lesson: only the first cowboy_static statement will work
		
    ]),

	DispatchWs = cowboy_router:compile([
		{'_', [
			{"/homepage.html", feed_handler, [{serverPID,ServerPID}]}
		]}
	  ]),
	
    {ok, _} = cowboy:start_clear(
		my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

	{ok, _} = cowboy:start_clear(
		ws,
		[{port, 8889}],
		#{env => #{dispatch => DispatchWs}}
	),
 
	twitter_sup:start_link().

stop(_State) ->
	ok.
