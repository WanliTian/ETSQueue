-module(queue_sup).
-behaviour(application).
-compile(export_all).

-define(QUEUEWORKER(ID, I, NAME), {ID, {I, start_link, [NAME]}, permanent, 5000, worker, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  N = ets_queue_config:get(<<"worker">>, <<"number">>),
  WorkerSpecList = worker_spec_list(N, []),
  {ok, {{one_for_one, 10, 10}, WorkerSpecList }}.

worker_spec_list(0, SpecList) ->
  SpecList;
worker_spec_list(N, SpecList) ->
  Spec = ?QUEUEWORKER(worker_id(N), queue_worker, worker_id(N)),
  worker_spec_list(N-1, [Spec | SpecList]).

worker_id(N) ->
  list_to_atom("queue_worker_" ++ integer_to_list(N)).
