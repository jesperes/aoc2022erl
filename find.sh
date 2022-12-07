#!/bin/bash

find /tmp/day07 -type d | while read -r d; do
    size=$(du -sb "$d" | awk '{print $1}')
    if [ "$size" -le 100000 ]; then
        echo "$size $d"
    fi
done # | awk '{s+=$1} END {print s}'
