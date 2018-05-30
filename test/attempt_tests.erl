-module(attempt_tests).

%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").

successful_attempt_can_be_mapped_test() ->
  Attempt = attempt:ok(1),
  AttemptMapped = attempt:map(Attempt, fun(N) -> N * 2 end),
  ?assertEqual({ok, 2}, AttemptMapped).

failed_attempt_map_does_nothing_test() ->
  Attempt = attempt:error(test_error),
  AttemptMapped = attempt:map(Attempt, fun(N) -> N * 2 end),
  ?assertEqual({error, test_error}, AttemptMapped).

successful_attempt_with_inner_success_can_be_flattened_test() ->
  Attempt = attempt:ok(attempt:ok(1)),
  AttemptFlattened = attempt:flatten(Attempt),
  ?assertEqual({ok, 1}, AttemptFlattened).

successful_attempt_with_inner_failure_can_be_flattened_test() ->
  Attempt = attempt:ok(attempt:error(test_error)),
  AttemptFlattened = attempt:flatten(Attempt),
  ?assertEqual({error, test_error}, AttemptFlattened).

failed_attempt_flatten_does_nothing_test() ->
  Attempt = attempt:error(test_error),
  AttemptFlattened = attempt:flatten(Attempt),
  ?assertEqual({error, test_error}, AttemptFlattened).

successful_attempt_with_inner_success_can_be_flat_mapped_test() ->
  Attempt = attempt:ok(1),
  AttemptFlatMapped = attempt:flat_map(Attempt, fun(N) -> attempt:ok(N * 2) end),
  ?assertEqual({ok, 2}, AttemptFlatMapped).

successful_attempt_with_inner_failure_can_be_flat_mapped_test() ->
  Attempt = attempt:ok(1),
  AttemptFlatMapped = attempt:flat_map(Attempt, fun(_) -> attempt:error(test_error) end),
  ?assertEqual({error, test_error}, AttemptFlatMapped).

failed_attempt_flat_map_does_nothing_test() ->
  Attempt = attempt:error(test_error),
  AttemptFlatMapped = attempt:flat_map(Attempt, fun(N) -> attempt:ok(N * 2) end),
  ?assertEqual({error, test_error}, AttemptFlatMapped).

successful_attempt_recover_does_nothing_test() ->
  Attempt = attempt:ok(1),
  AttemptRecovered = attempt:recover(Attempt, fun(test_error) -> 2; (other_error) -> 3 end),
  ?assertEqual({ok, 1}, AttemptRecovered).

failed_attempt_can_be_recovered_test() ->
  Attempt = attempt:error(test_error),
  AttemptRecovered = attempt:recover(Attempt, fun(test_error) -> 2; (other_error) -> 3 end),
  ?assertEqual({ok, 2}, AttemptRecovered).

successful_attempt_recover_with_does_nothing_test() ->
  Attempt = attempt:ok(1),
  AttemptRecovered = attempt:recover_with(Attempt, fun
    (test_error) -> attempt:ok(2);
    (other_error) -> attempt:ok(3)
  end),
  ?assertEqual({ok, 1}, AttemptRecovered).

failed_attempt_can_be_recovered_with_success_test() ->
  Attempt = attempt:error(test_error),
  AttemptRecovered = attempt:recover_with(Attempt, fun
    (test_error) -> attempt:ok(2);
    (other_error) -> attempt:ok(3)
  end),
  ?assertEqual({ok, 2}, AttemptRecovered).

failed_attempt_can_be_recovered_with_failure_test() ->
  Attempt = attempt:error(test_error),
  AttemptRecovered = attempt:recover_with(Attempt, fun
    (test_error) -> attempt:error(2);
    (other_error) -> attempt:ok(3)
  end),
  ?assertEqual({error, 2}, AttemptRecovered).

traverse_all_successful_attempts_test() ->
  Strings = ["123", "234", "456"],
  Parsed = attempt:traverse(Strings, fun(S) -> attempt:to(fun() -> list_to_integer(S) end) end),
  ?assertEqual({ok, [123, 234, 456]}, Parsed).

traverse_with_error_attempt_test() ->
  Strings = ["123", "aaa", "456"],
  Parsed = attempt:traverse(Strings, fun(S) -> attempt:to(fun() -> list_to_integer(S) end) end),
  ?assertEqual({error, badarg}, Parsed).

sequence_all_successful_attempts_test() ->
  Strings = ["123", "234", "456"],
  Parsed = [attempt:to(fun() -> list_to_integer(S) end) || S <- Strings],
  ?assertEqual({ok, [123, 234, 456]}, attempt:sequence(Parsed)).

sequence_with_error_attempt_test() ->
  Strings = ["123", "aaa", "456"],
  Parsed = [attempt:to(fun() -> list_to_integer(S) end) || S <- Strings],
  ?assertEqual({error, badarg}, attempt:sequence(Parsed)).