-module(prop_attempt).

%% API
-export([]).

-include_lib("proper/include/proper.hrl").

%%====================================================================
%% Test cases
%%====================================================================
prop_successful_attempt_from_plain_value() ->
  ?FORALL(Value, no_ok_term(), begin
    {ok, Value} =:= attempt:to(fun() -> Value end)
  end).

prop_successful_attempt_from_ok_value() ->
  ?FORALL(Value, term(), begin
    {ok, Value} =:= attempt:to(fun() -> {ok, Value} end)
  end).

prop_failed_attempt_from_error_value() ->
  ?FORALL(Value, term(), begin
    {error, Value} =:= attempt:to(fun() -> {error, Value} end)
  end).

prop_failed_attempt_from_thrown_error() ->
  ?FORALL(Value, term(), begin
    {error, Value} =:= attempt:to(fun() -> error(Value) end)
  end).

%%====================================================================
%% Generators
%%====================================================================
no_ok_term() ->
  case term() of
    {ok, Term} -> Term;
    {error, Term} -> Term;
    Term -> Term
  end.