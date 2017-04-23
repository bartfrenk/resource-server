# Resource server

Solution to the *Supervision in the Frequency Server* exercise of the *Concurrent
Programming in Erlang* course on FutureLearn.

I refer to the frequency server in the exercise as the resource server. The
entire system consists of three processes:

1. A supervisor, called the overseer. It is implemented in `overseer.erl`.
2. A client-facing server, called the resource server. It is implemented in `server.erl`.
3. A store, that duplicates the state of the server; this process could be
   responsible for persisting allocated resources if necessary. It is
   implemented in `store.erl`.

The basic premise is that clients should only depend on the server being up when
they are allocating and deallocating resource, otherwise they shouldn't. I
haven't been very careful: there are probably scenarios in which the client and
server states will be out of sync.

The four remaining files hold:

* `log.erl`: A simple logger abstraction.
* `utils.erl`: Utility functions for generating random numbers, and for doing
  synchronous calls to registered and unregistered processes.
* `client.erl`: Client processes parametrized state machines. The actual state
  machines are referred to as *behaviours*.
* `simulation.erl`: Contains behaviours specific to the resource server context,
  and a number of scenarios.

