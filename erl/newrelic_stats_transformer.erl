% Stats Transformer
% Based On: https://github.com/wooga/newrelic-erlang/blob/master/src/newrelic_statman.erl
% Improvements: stacktraces, improved metrics

-module(newrelic_stats_transformer).
-compile([export_all]).

poll() ->
    Metrics = maps:to_list(maps:get(counters, 'Elixir.Exmetrics':snapshot())),
    'Elixir.Exmetrics':reset(),
    transform_aggregated_metrics(Metrics).


transform_aggregated_metrics(Metrics) ->
    Ms = lists:filter(
           fun (M) -> M =/= [] end,
           lists:foldl(fun (M, Acc) ->
                               transform_metric(M) ++ Acc
                       end, [], Metrics)),

    Counters = lists:filter(
                   fun (Metric) ->
                     case Metric of
                       {{_Scope, {error, {_, _, _}}}, _Value} ->
                         true;
                       _ ->
                         false
                      end
                   end,
                   Metrics),
    Errs = lists:flatmap(
               fun (Metric) ->
                       transform_error_counter(Metric)
               end,
               Counters),

    {[webtransaction_total(Ms), db_total(Ms) | errors_total(Errs) ++ Ms], Errs}.


transform_error_counter(Metric) ->
    {{Scope, {error, {Type, Message, StackTrace}}}, Value} = Metric,
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Error = [MegaSecs * 1000000 + Secs + MicroSecs / 1000000,
             scope2bin(Scope),
             to_bin(Message),
             to_bin(Type),
             {[{<<"parameter_groups">>,{[]}},
                {<<"stack_trace">>, StackTrace},
                {<<"request_params">>, {[]}},
                {<<"request_uri">>, Scope}
              ]}],
    lists:duplicate(Value, Error).


transform_metric(Metric) ->
    case Metric of
        {{_Scope, total, _Elapsed}, _Value} ->
            transform_histogram(Metric);
        {{_Scope, {error, {_, _, _}}}, _Value} ->
            transform_counter(Metric);
        _ ->
            []
    end.

transform_counter(Metric) ->
  {{Scope, {error, {_Type, _Message, _StackTrace}}}, Value} = Metric,
  case Value of
    Errors when Errors > 0 ->
        [[{[{name, <<"Errors/WebTransaction/Uri", Scope/binary>>},
            {scope, <<"">>}]},
          [Errors, 0.0, 0.0, 0.0, 0.0, 0.0]
         ]];
    _ ->
        []
  end.

transform_histogram(Metric) ->
    {Key, Value} = Metric,
    {Scope, Type, Elapsed} = Key,
    KeyMatch = {Scope, Type},
    Summary = summary([{Elapsed, Value}]),
    Data = [proplists:get_value(observations, Summary),
            proplists:get_value(sum, Summary) / 1000000,
            proplists:get_value(sum, Summary) / 1000000,
            proplists:get_value(min, Summary) / 1000000,
            proplists:get_value(max, Summary) / 1000000,
            proplists:get_value(sum2, Summary) / 1000000000
           ],

    case KeyMatch of
        {Scope, {db, Segment}} when is_binary(Scope) ->
            [
             [{[{name, <<"Database", "/", (to_bin(Segment))/binary>>},
                {scope, scope2bin(Scope)}]},
              Data],

             [{[{name, <<"Database/allWeb">>},
                {scope, <<"">>}]},
              Data],

             [{[{name, <<"Database/all">>},
                {scope, <<"">>}]},
              Data]

            ];

        {Scope, {ext, Host}} when is_binary(Scope) andalso is_binary(Host) ->
            [
             [{[{name, <<"External/all">>},
               {scope, <<"">>}]},
             Data],

             [{[{name, <<"External/allWeb">>},
               {scope, <<"">>}]},
             Data],

             [{[{name, <<"External/", Host/binary>>},
               {scope, <<"">>}]},
             Data],

             [{[{name, <<"External/", Host/binary, "/all">>},
               {scope, <<"">>}]},
             Data],

             [{[{name, <<"External/", Host/binary>>},
               {scope, scope2bin(Scope)}]},
              Data]

            ];

        {{background, Scope}, total} when is_binary(Scope) ->
            [[{[{name, bgscope2bin(Scope)},
                {scope, <<"">>}]},
              Data]];

        {{background, Scope}, {Class, Segment}} when is_binary(Scope) ->
            [[{[{name, <<(class2bin(Class))/binary, "/", (to_bin(Segment))/binary>>},
                {scope, bgscope2bin(Scope)}]},
             Data]];

        {Scope, {Class, Segment}} when is_binary(Scope) ->
            [[{[{name, <<(class2bin(Class))/binary, "/", (to_bin(Segment))/binary>>},
                {scope, scope2bin(Scope)}]},
              Data]];

        {Scope, total} when is_binary(Scope) ->
            [[{[{name, <<"WebTransaction/Uri", Scope/binary>>},
                {scope, <<"">>}]},
             Data]];

        {A, B} when is_atom(A) andalso is_atom(B) ->
            [[{[{name, <<"OtherTransaction/",
                         (to_bin(A))/binary, "/",
                         (to_bin(B))/binary>>},
                {scope, <<"">>}]},
             Data]];
        _ ->
            []
    end.


webtransaction_total(Ms) ->
    Name = <<"WebTransaction">>,
    N    = lists:sum(pluck(Name, 1, Ms)),
    Sum  = lists:sum(pluck(Name, 2, Ms)),
    Min  = lists:min(pluck(Name, 4, Ms)),
    Max  = lists:max(pluck(Name, 5, Ms)),
    Sum2 = lists:sum(pluck(Name, 6, Ms)),

    [{[{name, <<"HttpDispatcher">>},
       {scope, <<"">>}]},
     [N, Sum, Sum, Min, Max, Sum2]].


db_total(Ms) ->
    Name = <<"Database">>,
    N    = lists:sum(pluck(Name, 1, Ms)),
    Sum  = lists:sum(pluck(Name, 2, Ms)),
    Min  = lists:min(pluck(Name, 4, Ms)),
    Max  = lists:max(pluck(Name, 5, Ms)),
    Sum2 = lists:sum(pluck(Name, 6, Ms)),

    [{[{name, <<"Database/all">>},
       {scope, <<"">>}]},
     [N, Sum, Sum, Min, Max, Sum2]].


errors_total(Errors) ->
    Data = [length(Errors), 0.0, 0.0, 0.0, 0.0, 0.0],
    [[{[{name, <<"Errors/all">>},
        {scope, <<"">>}]},
      Data],
     [{[{name, <<"Errors/allWeb">>},
        {scope, <<"">>}]},
      Data],
     [{[{name, <<"Instance/Reporting">>},
        {scope, <<"">>}]},
      Data]
    ].


pluck(_, _, []) ->
    [0];
pluck(Name, N, L) ->
    lists:map(fun ([_, []]) -> 0;
                  ([{Struct}, D]) ->
                      case binary:match(proplists:get_value(name, Struct), Name) of
                          nomatch ->
                              0;
                          _ ->
                              case proplists:get_value(scope, Struct) =:= <<"">> of
                                  true ->
                                      lists:nth(N, D);
                                  false ->
                                      0
                              end
                      end
              end, L).



class2bin(db) -> <<"Database">>;
class2bin(Atom) when is_atom(Atom) ->
    [F | R] = atom_to_list(Atom),
    list_to_binary([string:to_upper(F) | R]).

to_bin(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
to_bin(Bin) when is_binary(Bin)-> Bin.

bgscope2bin(Scope) ->
    <<"OtherTransaction/Python/", Scope/binary>>.

scope2bin(Url) when is_binary(Url) ->
    <<"WebTransaction/Uri", Url/binary>>.

summary([]) ->
    [];
summary(Data) ->
    {N, Sum, Sum2, Max} = scan(Data),

    [{observations, N},
     {min, find_quantile(Data, 0)},
     {max, Max},
     {sum, Sum},
     {sum2, Sum2}
    ].

scan(Data) ->
    scan(0, 0, 0, 0, Data).

scan(N, Sum, Sum2, Max, []) ->
    {N, Sum, Sum2, Max};
scan(N, Sum, Sum2, Max, [{Value, Weight} | Rest]) ->
    V = Value * Weight,
    scan(N + Weight,
         Sum + V,
         Sum2 + ((Value * Value) * Weight),
         max(Max, Value),
         Rest).

find_quantile(Freqs, NeededSamples) ->
    find_quantile(Freqs, 0, NeededSamples).

find_quantile([{Value, _Freq} | []], _Samples, _NeededSamples) ->
    Value;
find_quantile([{Value, Freq} | Rest], Samples, NeededSamples) ->
    Samples2 = Samples + Freq,
    if
        Samples2 < NeededSamples ->
            find_quantile(Rest, Samples2, NeededSamples);
        true ->
            Value
    end.
