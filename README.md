rebind
======

rebind parse transform for erlang

Usage
-----

```erlang
-module(test).
-compile({parse_transform, rebind}).

-export([sum/0]).

sum() ->
  State = 1,
  rebind(State) = State + 1,
  rebind(State) = State + 1,
  rebind(State) = State + 1,
  rebind(State) = State + 1,
  State.
```