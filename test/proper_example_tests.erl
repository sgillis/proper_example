-module(proper_example_tests).

-behaviour(proper_statem).

%% Run prop tests
-export([ keyvalue_props/0 ]).

%% proper_statem exports
-export([ initial_state/0
        , command/1
        , next_state/3
        , precondition/2
        , postcondition/3
        ]).

%% State transitions
-export([ new_table/1
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {tables}).

%%%_* Property tests ===========================================================

run_test_() ->
  N = 10,
  ?_assert(proper:quickcheck(?MODULE:keyvalue_props(),
                             [ {numtests, N} ])).

keyvalue_props() ->
  ?FORALL(Cmds, commands(?MODULE, initial_state()),
          begin
            kv:start_link(),
            {Hist, State, Result} = run_commands(?MODULE, Cmds),
            fmt(cmds, "~nCmds: ~p~n", [Cmds]),
            Tables = lists:sort(kv:tables()),
            kv:stop(),
            ?WHENFAIL(
               log(Cmds, Hist, State, Result),
               conjunction(
                 [ {result, Result =:= ok}
                 , {tables, State#state.tables =:= Tables}
                 ]))
          end).

%%%_* proper_statem ===========================================================

initial_state() ->
  #state{ tables = [] }.

command(_S) ->
  PossibleActions = [ { call, ?MODULE, new_table, [table_name()] } ],
  oneof(PossibleActions).

precondition(_S, {call, _Mod, _F, _Args}) ->
  true.

postcondition(_S, {call, _Mod, _F, _Args}, _Res) ->
  true.

next_state(State, _Result, {call, ?MODULE, new_table, [Name]}) ->
  State#state{ tables = lists:sort([ Name | State#state.tables ]) }.

%%%_* State transitions ========================================================

new_table(Name) ->
  fmt({cmd, new_table}, "Generate new table: ~p~n", [Name]),
  kv:new_table(Name).

%%%_* Generators ===============================================================

table_name() ->
  atom().

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
