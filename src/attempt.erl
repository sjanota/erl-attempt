-module(attempt).

%% API
-export([
  to/1,
  map/2,
  recover/2,
  ok/1,
  error/1,
  flatten/1,
  flat_map/2,
  recover_with/2,
  traverse/2,
  sequence/1]).

%%====================================================================
%% API types
%%====================================================================
-type f0(T) :: fun(() -> T).
-type f1(T, U) :: fun((T) -> U).

-export_type([f0/1, f1/2]).

-type attempt(T) :: {ok, Result :: T} | {error, Reason :: any()}.

-export_type([attempt/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc `to` wraps function f() -> T into attempt.
%% Function may return in following ways:
%% - {ok, Result} - will be treated as success
%% - {error, Reason} - will be treated as failure
%% - Result - whatever doesn't match cases above will be treated as success
%% - throw an error - will be converted to failure with reason as reason
-spec to(f0(T)) -> attempt(T).
to(F) ->
  try F() of
    {ok, Result} -> {ok, Result};
    {error, Reason} -> {error, Reason};
    Result -> {ok, Result}
  catch
    error:Reason -> {error, Reason}
  end.

%% @doc `ok` wraps value into successful attempt.
-spec ok(T) -> attempt(T).
ok(Result) ->
  {ok, Result}.

%% @doc `error` wraps reason into failed attempt.
-spec error(Reason :: any()) -> attempt(none()).
error(Reason) ->
  {error, Reason}.

%% @doc `map` converts value of type T if attempt was successful into value of type U using Map function.
%% Successful attempt is mapped using provided function.
%% Failed attempt is returned as is.
-spec map(attempt(T), Map :: f1(T, U)) -> attempt(U).
map({ok, Result} = _Attempt, Map) ->
  {ok, Map(Result)};
map(Attempt, _Map) ->
  Attempt.

%% @doc `flatten` converts nested attempt into non-nested if outer attempt is a success.
%% Successful attempt is unwrapped and inner attempt is returned.
%% Failed attempt is returned as is.
-spec flatten(attempt(attempt(T))) -> attempt(T).
flatten({ok, {ok, _} = Attempt}) -> Attempt;
flatten({ok, {error, _} = Attempt}) -> Attempt;
flatten(Attempt) -> Attempt.

%% @doc `flat_map` converts attempt into another one based on value held using FlatMap function.
%% Successful attempt is mapped using provided function.
%% Failed attempt is returned as is.
-spec flat_map(attempt(T), FlatMap :: f1(T, attempt(U))) -> attempt(U).
flat_map(Attempt, FlatMap) ->
  flatten(map(Attempt, FlatMap)).

%% @doc `recover` allows to handle error held by attempt.
%% Successful attempt is returned as is.
%% Failed attempt is converted to successful attempt with value returned by Recover function.
-spec recover(attempt(T), Recover :: f1(Reason :: any(), T)) -> attempt(T).
recover({error, Reason} = _Attempt, Recover) ->
  {ok, Recover(Reason)};
recover(Attempt, _Map) ->
  Attempt.

%% @doc `recover_with` allows to handle error held by attempt providing new attempt instead.
%% Successful attempt is returned as is.
%% Failed attempt is converted to attempt returned by provided function.
-spec recover_with(attempt(T), Recover :: f1(Reason :: any(), attempt(T))) -> attempt(T).
recover_with(Attempt, Recover) ->
  flatten(recover(Attempt, Recover)).

%% @doc `traverse` allows to apply same operation to each element on a list and get attempt of results in return.
%% If every element on list is successfully traversed traverse results in successful attempt with list of returns.
%% If there is at least one error during traverse first error is returned.
-spec traverse(list(T), f1(T, attempt(U))) -> attempt(list(U)).
traverse(List, Traverse) ->
  Loop = fun
    F([], Acc) ->
      attempt:ok(Acc);
    F([H | T], Acc) ->
      case Traverse(H) of
        {ok, Value} -> F(T, Acc ++ [Value]);
        {error, _} = Error -> Error
      end
  end,
  Loop(List, []).

%% @doc `sequence` allows to convert list of attempts into single attempt containing list of values. If any
%% attempt on a list is failed first failure will be returned.
-spec sequence(list(attempt(T))) -> attempt(list(T)).
sequence(List) ->
  traverse(List, fun(A) -> A end).