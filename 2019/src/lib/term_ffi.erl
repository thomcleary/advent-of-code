-module(term_ffi).
-export([columns/0]).

columns() ->
  case io:columns() of
    {ok, Width} -> {ok, Width};
    {error, _} -> {error, nil}
  end.