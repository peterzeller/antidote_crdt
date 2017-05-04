-module(orset_driver).


-define(NODE, 'antidote_crdt@127.0.0.1').
-define(CRDT, 'antidote_crdt_orset').

% basho_bench callbacks
-export([new/1, run/4]).

new(Id) ->
	erlang:set_cookie(node(), 'basho_bench'),
	{ok, new()}.

run(add, KeyGen, _ValueGen, State) ->
	% generate random key to insert into set
	Value = KeyGen(),
	{ok, Effect} = downstream(State, {add, Value}),
	{ok, NewState} = update(State, Effect),
	{ok, NewState};

run(value, _KeyGen, _ValueGen, State) ->
	Res = value(State),
	{ok, State}.

new() ->
	rpc:call(?NODE, ?CRDT, new, []).

update(State, Effect) ->
	rpc:call(?NODE, ?CRDT, update, [Effect, State]).

downstream(State, Operation) ->
	rpc:call(?NODE, ?CRDT, downstream, [Operation, State]).

value(State) ->
	rpc:call(?NODE, ?CRDT, value, [State]).