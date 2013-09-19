Adventure Game Engine
====

Design and development of a toy engine in Haskell.

Introduction and example of higher-order functions, laziness, and types.

Imperative Design
----

Big state machine with one big global state.

Simple.

Doesn't match logical situation-- no isolation of logically separate room states.

Functional Design
----

Rooms are evolving processes.

No externally accessible state.

Player is a process whose state is inventory and a bunch of references to rooms.

Types
----


