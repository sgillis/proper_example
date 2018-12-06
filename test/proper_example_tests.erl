-module(proper_example_tests).

-behaviour(proper_statem).

%% Run prop tests
-export([ keyvalue_props/0
        , run_test/1
        ]).

%% proper_statem exports
-export([ initial_state/0
        , command/1
        , next_state/3
        , precondition/2
        , postcondition/3
        ]).

%% State transitions
-export([ new_table/1
        , new_table_existing/1
        , put/3
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {tables, values}).

%%%_* Property tests ===========================================================

run_test_() ->
  N = 100,
  ?_assert(run_test(N)).

run_test(N) ->
  proper:quickcheck(?MODULE:keyvalue_props(), [{numtests, N}]).

keyvalue_props() ->
  ?FORALL(Cmds, commands(?MODULE, initial_state()),
          begin
            kv:start_link(),
            {Hist, State, Result} = run_commands(?MODULE, Cmds),
            fmt(cmds, "~nCmds: ~p~n", [Cmds]),
            Tables = lists:sort(kv:tables()),
            TableKeyValues = [{Table, kv:key_values(Table)} || Table <- Tables],
            kv:stop(),
            ?WHENFAIL(
               log(Cmds, Hist, State, Result),
               conjunction(
                 [ {result, Result =:= ok}
                 , {tables, State#state.tables =:= Tables}
                 , {key_values, compare_key_values(State, TableKeyValues)}
                 ]))
          end).

compare_key_values(State, TableKeyValues) ->
  AllValues = lists:flatten(
                [ pair_table_and_key(Table, KVs)
                  || {Table, KVs} <- TableKeyValues ]),
  lists:sort(State#state.values) =:= lists:sort(AllValues).

pair_table_and_key(_Table, []) ->
  [];
pair_table_and_key(Table, [{Key, Value} | KVs]) ->
  [{{Table, Key}, Value} | pair_table_and_key(Table, KVs)].

%%%_* proper_statem ===========================================================

initial_state() ->
  #state{ tables = []
        , values = []
        }.

command(State) ->
  StatelessActions =
    [ {call, ?MODULE, new_table, [table_name(State)]} ],
  StatefulActions =
    case State#state.tables =/= [] of
      true ->
        [ {call, ?MODULE, new_table_existing, [table_name_existing(State)]}
        , {call, ?MODULE, put, [table_name_existing(State), key(), value()]}
        ];
      false ->
        []
    end,
  PossibleActions = StatelessActions ++ StatefulActions,
  oneof(PossibleActions).

precondition(_S, {call, _Mod, _F, _Args}) ->
  true.

postcondition(_S, {call, _Mod, new_table, _Args}, Res) ->
  Res =:= ok;
postcondition(_S, {call, _Mod, new_table_existing, _Args}, Res) ->
  Res =:= {error, table_exists};
postcondition(_S, {call, _Mod, put, _Args}, Res) ->
  Res =:= ok.

next_state(State, _Result, {call, ?MODULE, new_table, [Name]}) ->
  State#state{ tables = lists:usort([ Name | State#state.tables ]) };
next_state(State, _Result, {call, ?MODULE, new_table_existing, [_Name]}) ->
  State;
next_state(State, _Result, {call, ?MODULE, put, [Name, Key, Value]}) ->
  Index = {Name, Key},
  Values = State#state.values,
  NewValues = [{Index, Value} | lists:keydelete(Index, 1, Values)],
  State#state{ values = NewValues }.

%%%_* State transitions ========================================================

new_table(Name) ->
  fmt({cmd, new_table}, "Generate new table: ~p~n", [Name]),
  kv:new_table(Name).

new_table_existing(Name) ->
  fmt({cmd, new_table}, "Generate new table: ~p~n", [Name]),
  kv:new_table(Name).

put(Name, Key, Value) ->
  fmt({cmd, put}, "Put value: ~p ~p ~p~n", [Name, Key, Value]),
  kv:put(Name, Key, Value).

%%%_* Generators ===============================================================

table_name(State) ->
  ?SUCHTHAT(Atom, atom(), not lists:member(Atom, State#state.tables)).

table_name_existing(State) ->
  oneof(State#state.tables).

key() ->
  atom().

value() ->
  int().

%%%_* Logging ==================================================================

log(Cmds, History, State, Result) ->
  fmt(fail, "Commands: ~p~nHistory: ~p~nState: ~p~nResult: ~p~n",
      [Cmds, History, State, Result]).

fmt(Tag, S, Xs) ->
  case dbgmsg(Tag) of
    true -> ?debugFmt(S, Xs);
    _    -> ok
  end.

dbgmsg(cmds) ->
  false;
dbgmsg({cmd, _}) ->
  false;
dbgmsg(fail) ->
  true;
dbgmsg(_) ->
  false.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
