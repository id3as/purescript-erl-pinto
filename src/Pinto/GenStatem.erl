-module(pinto_genStatem@foreign).

-include_lib("kernel/include/logger.hrl").

-export([ mkReply/1
        , selfFFI/0
        ]).

-export([
        ]).

-import('pinto_types@foreign',
        [
        ]).

%%% ----------------------------------------------------------------------------
%%% FFI API
%%% ----------------------------------------------------------------------------
mkReply(_) ->
  fun() ->
      ok
  end.

selfFFI() ->
  fun() ->
      self()
  end.
