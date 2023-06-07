PROJECT = twitter
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy 
dep_cowboy_commit = 2.9.0

DEP_PLUGINS = cowboy

BUILD_DEPS += relx

simulator : 
	@erlc -o ebin src/*.erl
	@erl -pa ebin -eval

include erlang.mk
