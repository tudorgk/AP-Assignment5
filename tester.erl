%Author: Tudor Dragan

-module(tester).
-include_lib("eunit/include/eunit.hrl").


%% sum and factorial test
sum_fac_test() ->
  {ok, MR}  = mr:start(3),
  {ok, Sum} = mr:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X+Acc end,
			    0,
			    lists:map(fun(X) -> [X] end, lists:seq(1,10))),
    {ok, Fac} = mr:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X*Acc end,
			    1,
			    lists:map(fun(X) -> [X] end, lists:seq(1,10))),
  mr:stop(MR),
  ?_assert({Sum,Fac} == {52, 3628800}).
