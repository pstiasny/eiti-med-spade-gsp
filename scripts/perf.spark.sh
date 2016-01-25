#!/bin/bash 
#set -euo pipefail

#rm -f GSP_LOG SPADE_LOG

for cores in 1 2 3 4; do
    scripts/run_spark_experiment 200000 100 3 10 0.4 ${cores}
done

for num_sequences in 10 100 1000 10000 100000 500000 1000000; do
    scripts/run_spark_experiment ${num_sequences} 100 3 10 0.4 4
done

for support in $(seq 1 9); do
    scripts/run_spark_experiment 200000 100 3 10 0.${support} 4
done
#22*4*4*3*2

cat SPADE_LOG GSP_LOG | grep "GSP\|SPADE" | sed 's/Command exited with non-zero status 1/NA/' > data.txt
