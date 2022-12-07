#!/bin/bash
# shellcheck disable=SC2001

erl_version() {
    erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell
}

echo "ERLANG VERSION: $(erl_version)"

export PROFILE=inline
export APP=aoc2022erl

rebar3 as $PROFILE "do" compile

if [ "$#" -eq 0 ]; then
    DAYS=(src/day*.erl)
else
    DAYS=("$@")
fi

for d in "${DAYS[@]}"; do
    DAY=$(echo "$d" | sed -e 's|.*/\(day.*\).erl|\1|')
    "$HOME"/dev/erlperf/_build/default/bin/erlperf "$DAY:solve()." \
           -pa _build/$PROFILE/lib/$APP/ebin -w 5

done
