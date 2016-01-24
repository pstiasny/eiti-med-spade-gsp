#!/bin/bash
set -euo pipefail

run_experiment() {
    NUM_SEQUENCES=$1
    NUM_ITEMS=$2
    ITEMS_PER_ITEMSET=$3
    ITEMSETS_PER_SEQ=$4
    export MIN_SUPPORT=$5
    export MIN_ABS_SUP="$( echo "scale=0; $NUM_SEQUENCES * $MIN_SUPPORT / 1" | bc )"
    echo $@

    java -jar spmf.jar run \
        Generate_a_sequence_database_with_timestamps no_input_file TEST_IN.spmf \
        $NUM_SEQUENCES $NUM_ITEMS $ITEMS_PER_ITEMSET $ITEMSETS_PER_SEQ \
        > /dev/null

    ./spmf2horizontal.py < TEST_IN.spmf > TEST_IN.horizontal

    cd spade
    echo -n $@ " " >> ../SPADE_LOG
    /usr/bin/time -f "%e" -o ../SPADE_LOG -a sbt "run-main SpadePureApp ../TEST_IN.horizontal" > /dev/null
    cd ../gsp
    echo -n $@ " " >> ../GSP_LOG
    export DATA_FILE=../TEST_IN.spmf
    /usr/bin/time -f "%e" -o ../GSP_LOG -a sbt "run-main GspPureApp" > /dev/null
    cd ..
}

rm -f GSP_LOG SPADE_LOG

for s in $(seq 1 9); do
    run_experiment 10000 100 3 10 0.$s
done
