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
    DAYS=(priv/input*.txt)
else
    DAYS=("$@")
fi

MODULES=()
REBAR3_EUNIT_ARGS="--module="
for d in "${DAYS[@]}"; do
    DAY=$(echo "$d" | sed -e 's|.*/input\(.*\).txt|\1|')
    MODULES+=("day$DAY")
    REBAR3_EUNIT_ARGS="${REBAR3_EUNIT_ARGS},day$DAY"
done

echo "Found input files for puzzles: ${MODULES[*]}"
echo "Running eunit tests..."

if ! rebar3 as $PROFILE eunit "${REBAR3_EUNIT_ARGS}"; then
    exit 1
fi

echo "Running erlperf..."

TMPFILE=$(mktemp)

for m in "${MODULES[@]}"; do
    CMD=("$HOME"/dev/erlperf/_build/default/bin/erlperf
         "$m:solve()."
         -pa "_build/$PROFILE/lib/$APP/ebin"
         -w 5
        )
    if [ "$m" = "${MODULES[0]}" ]; then
        "${CMD[@]}" | tee -a "$TMPFILE"
    else
        "${CMD[@]}" | tail -n 1 | tee -a "$TMPFILE"
    fi
done

awk '{s+=$4} END {print "Total runtime", s, "us"}' <"$TMPFILE"

rm -f "$TMPFILE"
