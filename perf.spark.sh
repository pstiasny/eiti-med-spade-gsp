#!/bin/bash -x
#set -euo pipefail

SPARK="/root/spark/bin/spark-submit --master spark://ec2-52-3-251-24.compute-1.amazonaws.com:7077"
HADOOP=/root/ephemeral-hdfs/bin/hadoop

run_experiment() {
    NUM_SEQUENCES=$1
    NUM_ITEMS=$2
    ITEMS_PER_ITEMSET=$3
    ITEMSETS_PER_SEQ=$4
    export MIN_SUPPORT=$5
    export MIN_ABS_SUP="$( echo "scale=0; $NUM_SEQUENCES * $MIN_SUPPORT / 1" | bc )"
    CORES=$6
    echo $@

    java8 -jar spmf.jar run \
        Generate_a_sequence_database_with_timestamps no_input_file TEST_IN.spmf \
        $NUM_SEQUENCES $NUM_ITEMS $ITEMS_PER_ITEMSET $ITEMSETS_PER_SEQ \
        > /dev/null
    ./spmf2horizontal.py < TEST_IN.spmf > TEST_IN.horizontal

    $HADOOP fs -rm -f /TEST_IN.horizontal /TEST_IN.spmf
    $HADOOP fs -copyFromLocal TEST_IN.spmf /TEST_IN.spmf
    $HADOOP fs -copyFromLocal TEST_IN.horizontal /TEST_IN.horizontal

    echo -n $@ " "
    echo -n "SPADESPARK" $1 $2 $3 $4 $5 1024 $6 " " >> SPADE_LOG
    CMD="$SPARK --total-executor-cores $CORES --class SpadeSparkApp spade/target/scala-2.10/spade_2.10-0.0.1.jar hdfs:///TEST_IN.horizontal"
    /usr/bin/time -f "%e" -o SPADE_LOG -a ${CMD} TEST_IN.horizontal > /dev/null
    echo "spade done"
    echo -n "GSPSPARK" $1 $2 $3 $4 $5 1024 $6 " " >> GSP_LOG
    export DATA_FILE=hdfs:///TEST_IN.spmf
    CMD="$SPARK --total-executor-cores $CORES --class GspSparkApp gsp/target/scala-2.10/eiti-med-gsp_2.10-1.0.jar"
    /usr/bin/time -f "%e" -o GSP_LOG -a ${CMD} > /dev/null
    echo "gsp done"
}

#rm -f GSP_LOG SPADE_LOG

for cores in 1 2 3 4; do
    for num_sequences in 10 100 1000 10000 1000000; do
        for support in $(seq 2 2 9); do
            run_experiment ${num_sequences} 100 3 10 0.${support} ${cores}
        done
    done
done
#22*4*4*3*2

cat SPADE_LOG GSP_LOG | grep "GSP\|SPADE" | sed 's/Command exited with non-zero status 1/NA/' > data.txt
