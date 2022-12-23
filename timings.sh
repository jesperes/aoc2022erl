#!/bin/bash
# shellcheck disable=SC2001

export APP=aoc2022erl

rebar3 "do" compile,escriptize

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

if ! rebar3 eunit "${REBAR3_EUNIT_ARGS}"; then
    exit 1
fi

_build/default/bin/aoc2022erl "${MODULES[@]}"
