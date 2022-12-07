#!/bin/bash

erl_version() {
    erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell
}

echo "ERLANG VERSION: $(erl_version)"

export PROFILE=inline
export APP=aoc2022erl

rebar3 as $PROFILE do compile

d=01
"$HOME"/dev/erlperf/_build/default/bin/erlperf "day$d:solve()." \
       -pa _build/$PROFILE/lib/$APP/ebin -w 5

for d in 02 03 04 05 06 06b; do
    "$HOME"/dev/erlperf/_build/default/bin/erlperf "day$d:solve()." \
           -pa _build/$PROFILE/lib/$APP/ebin -w 5 | tail -n 1
done
