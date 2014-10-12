%%%-------------------------------------------------------------------
%%% @author Tudor-Stefan Dragan <me@tudordev.com>
%%% @copyright (C) 2014, Tudor-Stefan Dragan
%%% Created : Oct 2014 by Tudor-Stefan Dragan <me@tudordev.com>
%%%-------------------------------------------------------------------
-module(mr).

-export([start/1, stop/1, job/5]).

%%%% Interface

start(N) ->
  {Reducer, Mappers} = init(N), %% we get the reducer and mappers form the init function
  {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}. %% after that we get start the coordinator loop


stop(Pid) ->
  reply(Pid, stop).

job(CPid, MapFun, RedFun, RedInit, Data) ->
  rpc(CPid, {init, MapFun, RedFun, RedInit, Data}),
  rpc(CPid, {start, Data}).

%%%% Internal implementation

%%starting the reducer loop function
init(N) ->
    init(N,spawn(fun() -> reducer_loop() end), []).
%% initializing the mappers using pattern matching and adding them to the Map list recursively
init(N, Red, Map) when N > 0 ->
    init(N-1, Red, [spawn(fun() -> mapper_loop(Red, fun() -> mapper end) end)]++Map);
init(0, Red, Map) -> {Red, Map}. %% returning the reducer and mappers list


%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
  {Pid, Response} ->
      Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).


%% asynchronous communication

async(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    async(Pid, stop).

data_async(Pid, D) ->
    async(Pid, {data, D}).



%%% Coordinator

coordinator_loop(Reducer, Mappers) ->
  receive
    {From, stop} ->
      io:format("~p stopping~n", [self()]),
      lists:foreach(fun stop_async/1, Mappers),
      stop_async(Reducer),
      reply_ok(From);
    {From, {init, MapFun, RedFun, RedInit, Data}} ->
      io:format("~p initializing the reducer and mappers~n", [self()]),
      %% get the length of the list of elements to analyze
      DataLength = length(Data),
      %% initialize the reducer
      async(Reducer,{self(),{start,RedFun, RedInit, DataLength, From}}),
      %% init the mappers
      init_mappers(Mappers, MapFun),
      %% everything went ok
      reply_ok(From),
      coordinator_loop(Reducer, Mappers);
    {_, {start, Data}} ->
      %% send data to the mappers
      send_data(Mappers, Data),
      coordinator_loop(Reducer, Mappers);
    {_, {done, Result, Jid}} ->
      %% ok when done
      reply_ok(Jid, Result),
      coordinator_loop(Reducer, Mappers);
    Unknown ->
      io:format("[CL] unknown message: ~p~n",[Unknown]),
      coordinator_loop(Reducer, Mappers)
    end.

%%% TODO: Implement init_mappers
init_mappers([Mid|Mappers], MapFun)->
    %% init a mapper with the MapFun function
    async(Mid, {self(), {init, MapFun}}),
    %% init the others
   init_mappers(Mappers, MapFun);
init_mappers([],_) ->
    done.

send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer

reducer_loop() ->
  receive
    stop ->
      io:format("Reducer ~p stopping~n", [self()]),
      ok;
    {From, {start, RedFun, RedInit, Len, Jid}} ->

      {stop_gather, AllResDone} = gather_data_from_mappers(RedFun, RedInit, Len),
      async(From, {self(),{done, AllResDone, Jid}}),
      reducer_loop();
    {stop_gather, Res} ->
      Res,
      reducer_loop();
    Unknown ->
      io:format("[RL] unknown message: ~p~n",[Unknown]),
      reducer_loop()
  end.


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
      io:format("[GDFM] unknown message: ~p~n",[Unknown]),
      gather_data_from_mappers(Fun, Acc, Missing)
end.


%%% Mapper

mapper_loop(Reducer, Fun) ->
  receive
    stop ->
      io:format("Mapper ~p stopping~n", [self()]),
      ok;
    {_, {init, NewFun}} ->
      %reply_ok(From),
      mapper_loop(Reducer, NewFun);
    {data, Data} ->
      % io:format("Data: ~p~n", [Data]),
      Res = lists:map(Fun,[Data]),
      async(Reducer,{self(), {result, Res}}),
      mapper_loop(Reducer, Fun);
    Unknown ->
      io:format("[ML] unknown message: ~p~n",[Unknown]),
      mapper_loop(Reducer, Fun)
  end.
