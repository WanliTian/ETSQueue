-module(ets_queue_config).
-compile(export_all).

get(<<"worker">>, <<"number">>) ->
  7;

get(_, _) ->
  undefined.
