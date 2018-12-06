-module(kv).

-behaviour(gen_server).

-export([ start_link/0
        , stop/0
        , get/2
        , get/3
        , values/1
        , key_values/1
        , tables/0
        , new_table/1
        , put/3
        , dump_state/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%% External API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

get(TableName, Key) ->
  gen_server:call(?MODULE, {get, TableName, Key, undefined}).

get(TableName, Key, Default) ->
  gen_server:call(?MODULE, {get, TableName, Key, Default}).

values(TableName) ->
  gen_server:call(?MODULE, {values, TableName}).

key_values(TableName) ->
  gen_server:call(?MODULE, {key_values, TableName}).

tables() ->
  gen_server:call(?MODULE, tables).

new_table(TableName) ->
  gen_server:call(?MODULE, {new_table, TableName}).

put(TableName, Key, Value) ->
  gen_server:call(?MODULE, {put, TableName, Key, Value}).

dump_state() ->
  gen_server:call(?MODULE, dump_state).

%% GenServer callbacks

init([]) ->
  {ok, []}.

handle_call({get, TableName, Key, Default}, _From, State) ->
  FailureF = fun() -> Default end,
  SuccessF = fun(Table) -> proplists:get_value(Key, Table, Default) end,
  Response = table_operation(TableName, State, FailureF, SuccessF),
  {reply, Response, State};
handle_call({values, TableName}, _From, State) ->
  FailureF = fun() -> error end,
  SuccessF = fun(Table) -> [Value || {_, Value} <- Table] end,
  Response = table_operation(TableName, State, FailureF, SuccessF),
  {reply, Response, State};
handle_call({key_values, TableName}, _From, State) ->
  FailureF = fun() -> error end,
  SuccessF = fun(Table) -> Table end,
  Response = table_operation(TableName, State, FailureF, SuccessF),
  {reply, Response, State};
handle_call(tables, _From, State) ->
  {reply, proplists:get_keys(State), State};
handle_call({new_table, TableName}, _From, State) ->
  FailureF = fun() -> {ok, [{TableName, []} | State]} end,
  SuccessF = fun(_Table) -> {{error, table_exists}, State} end,
  {Response, NewState} = table_operation(TableName, State, FailureF, SuccessF),
  {reply, Response, NewState};
handle_call({put, TableName, Key, Value}, _From, State) ->
  FailureF = fun() -> {error, State} end,
  SuccessF =
    fun(Table) ->
        NewTable = [{Key, Value} | lists:keydelete(Key, 1, Table)],
        {ok, lists:keyreplace(TableName, 1, State, {TableName, NewTable})}
    end,
  {Response, NewState} = table_operation(TableName, State, FailureF, SuccessF),
  {reply, Response, NewState};
handle_call(dump_state, _From, State) ->
  {reply, State, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%% Internal functions

table_operation(TableName, Tables, FailureF, SuccessF) ->
  case proplists:get_value(TableName, Tables) of
    undefined ->
      FailureF();
    Table ->
      SuccessF(Table)
  end.
