[AP-Assignment5](https://github.com/tudorgk/AP-Assignment5)
================

Remember that song? - The objective of this assignment is to gain hands-on
programming experience with Erlang and to get an understanding of how to
implement map-reduce algorithms.  The goal is to implement a simple map-reduce
framework, and then use this framework to process the musiXmatch dataset (mxm),
the official collection of lyrics from the Million Song Dataset.

Part I Framework Implementation
-------------------------------

### 1. Initialization phase

```erlang
start(N) ->
  %% we get the reducer and mappers form the init function
  {Reducer, Mappers} = init(N),
  %% after that we get start the coordinator loop
  {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.

%%starting the reducer loop function
init(N) ->
    init(N,spawn(fun() -> reducer_loop() end), []).
%% initializing the mappers using pattern matching and adding them to the Map list recursively
init(N, Red, Map) when N > 0 ->
    init(N-1, Red, [spawn(fun() -> mapper_loop(Red, fun() -> mapper end) end)]++Map);
init(0, Red, Map) -> {Red, Map}. %% returning the reducer and mappers list
```

The `init` function starts a Reducer by spawning a reducer_loop and the using
pattern matching instanciates the mapper processes and the retuns the tuple with
the Reducer (Red) and list of Mappers (Map).

After that we spawn a coordonator with the Reducer and list of Mappers.

#### Coordinator init

```erlang
{From, {init, MapFun, RedFun, RedInit, Data}} ->
  % io:format("~p initializing the reducer and mappers~n", [self()]),
  %% get the length of the list of elements to analyze
  DataLength = length(Data),
  %% initialize the reducer
  async(Reducer,{self(),{start,RedFun, RedInit, DataLength, From}}),
  %% init the mappers
  init_mappers(Mappers, MapFun),
  %% everything went ok
  reply_ok(From),
  coordinator_loop(Reducer, Mappers);
```

We find the length of the data list (`DataLength`). Then we send the reducer function
(`RedFun`), the initial value of the reducer (`RedInit`), the data length and the Pid (`From`) of
the coordinator. After that we init the mappers with the map function (`MapFun`).

### 2. Putting the processes to work!

#### Coordinator start

```erlang
...

{_, {start, Data}} ->
  %% send data to the mappers
  send_data(Mappers, Data),
  coordinator_loop(Reducer, Mappers);

...

%%send data to mappers
send_data(Mappers, Data) ->
    recursive_send(Mappers, Mappers, Data).

recursive_send(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    recursive_send(Mappers, Queue, Data);
recursive_send(Mappers, [], Data) ->
    recursive_send(Mappers, Mappers, Data);
recursive_send(_, _, []) -> ok.

...
```

The coordinator sends the data to the mappers by redistributing piece by piece
until we are left out of data chunks.

#### Mapper receive

```erlang
...

{data, Data} ->
  % io:format("Data: ~p~n", [Data]),
  Res = lists:map(Fun,[Data]),
  async(Reducer,{self(), {result, Res}}),
  mapper_loop(Reducer, Fun);

...
```

The mapper receives the data and applies the function to the chunk of data and
sends the result to the reducer .

#### Reducer gathering

```erlang
%%% get data from mappers
gather_data_from_mappers(Fun, Acc, Missing) ->
  receive
    {stop_gather, Acc} ->
      %% return the accumulator
      Acc;
    {_, {result, ChunkOfData}} ->
      Res = (lists:foldl(Fun, Acc, ChunkOfData)),
      Miss = Missing - 1,
      if Miss >= 1 ->
        gather_data_from_mappers(Fun, Res, Miss);
        true -> async(self(),{stop_gather,Res})
      end;
  Unknown ->
      io:format("[GDFM] Unknown message: ~p~n",[Unknown]),
      gather_data_from_mappers(Fun, Acc, Missing)
end.
```

We fold to the left the list of data and continue to do so until we finish the
computation that's being added to the accumulator `Acc`. Then we send a `stop_gather`
and return the value in the accumulator `Acc`.

Part II MusicXMatch Dataset Analysis
------------------------------------

TODO

Testing
-------

###Usage:

####1. Compile using make
```bash
user:shell$ make test
```
####2. Run the tests
```erlang
1> tester:test().
```
