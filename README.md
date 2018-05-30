# About 'attempt'
[![Build Status](https://travis-ci.org/sjanota/erl-attempt.svg?branch=master)](https://travis-ci.org/sjanota/erl-attempt)

attempt is a functional wrapper for possibility of an error. It is 
inspired by Scala's Try.

### What is `attempt`?

`attempt` is a result of computation where this computation might fail 
in some exceptional way. `attempt` is defined as follows:

```erlang
-type attempt(T) :: {ok, Result :: T} | {error, Reason :: any()}.
```

Library uses popular convention for representing errors so it may be 
used along wih code which does not rely on it.

### Why `attempt`?

Imagine we have a function which may fail:

```erlang
-spec divide(number(), number()) -> {ok, number()} | {error, badarith}.
divide(_No, De) when De == 0 -> {error, badarith};
divide(No, De) -> {ok, No / De}.
```

Now if we want to use result of this function we need to protect ourselves 
from error using pattern matching:

```erlang
-spec divide(number(), number()) -> {ok, number()} | {error, badarith}.
divide_and_double(No, De) ->
  case divide(No, De) of
    {ok, Divided} -> {ok, Divided * 2};
    {error, _} = Error -> Error
  end.
```

If chain of operations is longer it can (and usually does) get out of hand. 
`attempt` aim to help with that providing functional constructs. E.g. above 
can written differently using `attempt`:

```erlang
-spec divide(number(), number()) -> {ok, number()} | {error, badarith}.
devide_and_double(No, De) ->
  Division = divide(No, De),
  attempt:map(Division, fun(N) -> N * 2 end).
```

If we want to further simplify, even divide function could be omitted:

```erlang
-spec divide(number(), number()) -> attempt(number()).
devide_and_double(No, De) ->
  Division = attempt:to(fun() -> No / De end),
  attempt:map(Division, fun(N) -> N * 2 end).
```

Construction function `attempt:to/1` takes a function and returns an attempt 
based on what this function has returned:
- success if function returned `{ok, Value}` or just `Value`
- failure if function returned `{error, Reason}` or thrown `error(Reason)`

### Is that all there is to `attempt`?

Basically yes, that's what `attempt` is about. However it provides couple more 
of useful functions to work with potential errors.

#### `attempt:flat_map/2`

`flat_map` works almost like regular map, but provided function instead of 
returning new value should return new attempt. This is useful if there is 
another operation in chain that may fail:

```erlang
File = attempt:to(fun() -> file:open("my-file.txt", read) end),
Content = attempt:flat_map(File, fun(F) -> read(F, 1000) end).
```

`flat_map` may also be used to inject error in the middle of a chain, like this:

```erlang
UpdatedRecordsNumber = attempt:to(fun() -> db:update(Record) end),
FaildIfNotUpdated = attempt:flat_map(UpdatedRecordsNumber, fun
  (0) -> attempt:error(none_updated);
  (Else) -> attempt:ok(Else)
end). 
```

#### `attempt:recover/2`

`recover` is a map, but for failure. It allows to handle an error and provide 
successful value instead, e.g.

```erlang
-spec calculate_timeout(number(), number()) -> attempt(timeout()).
calculate_timeout(BaseTimeout, Denominator) -> 
    Divided = attempt:to(fun() -> BaseTimeout / Denominator end),
    attempt:recover(Divided, fun(_) -> infinity end).
```

#### `attempt:recover_with/2`

`recover_with` works like recover, but function should return new attempt. 
Useful for partial handling of errors. E.g. if we want to provide default value 
when badarith was thrown:

```erlang
-spec calculate_timeout(number(), number()) -> attempt(timeout()).
calculate_timeout(BaseTimeout, Denominator) -> 
    Divided = attempt:to(fun() -> BaseTimeout / Denominator end),
    attempt:recover_with(Divided, fun
      (badarith) -> attempt:ok(infinity);
      (Else) -> attempt:error(Else) 
  end).
```

#### `attempt:traverse/2`

`traverse` is first of two helpers two make working with lists of values easier. 
Imagine we have list of strings and want to parse them into numbers:

```erlang
Strings = ["123", "12345", "aaaa"],
Ints = [attempt:to(fun() -> list_to_integer(S) end) || S <- Strings].
```

`Ints` in above example will be `[{ok, 123}, {ok, 12345}, {error, badarg}]`. 
This way we get list of attempts which may not be very useful. There are times 
where we want have all or nothing. That's where `traverse` comes in: 

```erlang
Strings = ["123", "12345", "aaaa"],
Ints = attempt:traverse(Strings, fun(S) -> 
  attempt:to(fun() -> list_to_integer(S) end) 
end).
```

Here `Ints` is `{error, badarg}`. If there were only valid values:

```erlang
Strings = ["123", "12345"],
Ints = attempt:traverse(Strings, fun(S) -> 
  attempt:to(fun() -> list_to_integer(S) end) 
end).
```

We would get `{ok, [123, 12345]}`. When using traverse we don't care about 
partial results. Whole list needs to be valid for attempt to be successful.

#### `attempt:sequence/2`

`sequence` allows to group several attempts into one attempt. If for some 
reason you already have list of attempts and want to convert it into attempt 
containing a list, that's what sequence is for, e.g.:

```erlang
File1 = attempt:to(fun() -> file:read_file("my-file1.txt") end),
File2 = attempt:to(fun() -> file:read_file("my-file2.txt") end),
File3 = attempt:to(fun() -> file:read_file("my-file3.txt") end),
Files = attempt:sequence([File1, File2, File3]),
AllContent = attempt:map(Files, fun(Fs) -> 
  lists:foldl(fun(F, Acc) -> <<Acc/binary, F/binary>> end, Fs, <<>>)
end).
```

In following example we can make attempts to read 3 files, then concat their contents. 
If anything goes wrong first error will be passed along the chain.

Build
-----

    $ rebar3 compile
