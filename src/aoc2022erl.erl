-module(aoc2022erl).

-export([main/1]).

main([]) ->
  aoc:tabulate(aoc:timings());
main(Args) ->
  io:format("Erlang version: ~s~n", [otp_version()]),
  Modules =
    lists:map(fun(A) ->
                  list_to_atom(A)
              end, Args),
  aoc:tabulate(aoc:timings(Modules)).


otp_version() ->
  {ok, Version} =
    file:read_file(filename:join([code:root_dir(), "releases",
                                  erlang:system_info(otp_release), "OTP_VERSION"])),
  string:trim(binary_to_list(Version)).
