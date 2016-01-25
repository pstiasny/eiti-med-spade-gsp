#!/bin/bash -x
#set -euo pipefail

run_experiment() {
    NUM_SEQUENCES=$1
    NUM_ITEMS=$2
    ITEMS_PER_ITEMSET=$3
    ITEMSETS_PER_SEQ=$4
    export MIN_SUPPORT=$5
    export MIN_ABS_SUP="$( echo "scale=0; $NUM_SEQUENCES * $MIN_SUPPORT / 1" | bc )"
    MEM=$6
    echo $@

    java -jar spmf.jar run \
        Generate_a_sequence_database_with_timestamps no_input_file TEST_IN.spmf \
        $NUM_SEQUENCES $NUM_ITEMS $ITEMS_PER_ITEMSET $ITEMSETS_PER_SEQ \
        > /dev/null

    ./spmf2horizontal.py < TEST_IN.spmf > TEST_IN.horizontal

    #cd spade
    echo -n $@ " "
    echo -n "SPADE" $@ " " >> SPADE_LOG
    CMD="java -Xmx${MEM}m -Xms${MEM}m -cp spade/target/scala-2.11/SPADE-assembly-0.0.1.jar SpadePureApp"
    /usr/bin/time -f "%e" -o SPADE_LOG -a ${CMD} TEST_IN.horizontal > /dev/null
    echo "spade done"
    #cd ../gsp
    echo -n "GSP" $@ " " >> GSP_LOG
    export DATA_FILE=TEST_IN.spmf
    CMD="java -Xmx${MEM}m -Xms${MEM}m -cp gsp/target/scala-2.11/eiti-med-gsp-assembly-1.0.jar GspPureApp"
    /usr/bin/time -f "%e" -o GSP_LOG -a ${CMD} > /dev/null
    echo "gsp done"
    #cd ..
}

rm -f GSP_LOG SPADE_LOG

for rep in $(seq 1 1 3); do
    for mem in "256" "512" "1024" "2048"; do
        cat num_sequences.txt | while read num_sequences; do
            for support in $(seq 2 2 9); do
                run_experiment ${num_sequences} 100 3 10 0.${support} ${mem}
            done
        done
    done
done
#22*4*4*3*2

cat SPADE_LOG GSP_LOG | grep "GSP\|SPADE" | sed 's/Command exited with non-zero status 1/NA/' > data.txt
