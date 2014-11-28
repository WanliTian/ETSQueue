-module(queue_worker).
-behaviour(gen_server).
-compile(export_all).

worker_name(Key) ->
  Id   = erlang:phash(Key, ets_queue_config:get(<<"worker">>, <<"number">>)),
  list_to_atom("queue_worker_" ++ integer_to_list(Id)).

put(TrailIdentifier, List) ->
  WorkerName = worker_name(TrailIdentifier),
  gen_server:call(WorkerName, {put, TrailIdentifier, List}).

get(ServerName) ->
  gen_server:call(ServerName, get).

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, Name, []).

init(State) ->
  ets:new(State, [ordered_set, named_table]),
  {ok, State}.

handle_call({put, TrailIdentifier, List}, _From, State) ->
  Key = case ets:last(State) of
    '$end_of_table' -> 0;
    Counter -> Counter + 1
  end,
  true = ets:insert(State, {Key, {[{<<"name">>, TrailIdentifier}, {<<"loglist">>, List}]}}),
  {reply, ok, State};
handle_call(get, _From, State) ->
   Value = case ets:first(State) of
     '$end_of_table' ->
        empty;
     First ->
       [{_, Term}] = ets:lookup(State, First),
       true       = ets:delete(State, First),
       {value, Term}
   end,
  {reply, Value, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
