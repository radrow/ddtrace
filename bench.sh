#!/usr/bin/bash
ARG=$1

make

SEED=0
TIMESTAMP=$(date +%d-%m-%Y--%T)

append_meta() {
    OUT_FILE=$1
    TEST_FILE=$2
    PDELAY=$3
    TIMEOUT=$4

    METADATA="# TEST FILE=$(md5sum ${TEST_FILE}); PROBE DELAY=${PDELAY}; TIMEOUT=${TIMEOUT}; SEED=${SEED}"
    echo $METADATA >> $OUT_FILE
}

eval_echo() {
    tput bold; echo "$*"; tput sgr0
    "$@"
}

run_bench() {
    TEST_FILE=$1
    TEST_TYPE=$2
    PDELAY=$3
    TIMEOUT=$4

    [[ "$TEST_TYPE" != bc && "$TEST_TYPE" != ts ]] && echo "Bad test type ${TEST_TYPE}" && return 1

    # Backup name
    STATS_FILE="${TEST_TYPE}_${TIMESTAMP}_${PDELAY}_${TIMEOUT}.csv"
    LOGS_DIR="${TEST_TYPE}_${TIMESTAMP}_${PDELAY}_${TIMEOUT}"
    mkdir -p "${LOGS_DIR}"

    time eval_echo ./dlstalk\
              "${TEST_FILE}"\
              "--seed=${SEED}"\
              --trace-proc\
              --silent\
              $(if [[ "$PDELAY" = "unmonitored" ]]; then echo -n --unmonitored; else echo -n "--probe-delay=$PDELAY"; fi)\
              "--stats-csv=${STATS_FILE}"\
              "--csv=${LOGS_DIR}/"\
              "--timeout=$TIMEOUT"

    append_meta "${STATS_FILE}" "${TEST_FILE}" "${PDELAY}" "${TIMEOUT}"

    # Save under deterministic name
    DETNAME="${TEST_TYPE}_${PDELAY}.csv"
    eval_echo cp "${STATS_FILE}" "${DETNAME}"
}

# This works only because there are two options; execute all if none given.
if [[ "${ARG}" != bc ]]; then
    run_bench "scenarios/big.conf" ts -1 10000
    run_bench "scenarios/big.conf" ts 500 10000
    run_bench "scenarios/big.conf" ts 1000 10000
    run_bench "scenarios/big.conf" ts 5000 10000
fi

if [[ "${ARG}" != ts ]]; then
    run_bench "scenarios/bench.conf" bc unmonitored 10000
    run_bench "scenarios/bench.conf" bc -1 10000
    run_bench "scenarios/bench.conf" bc 500 10000
    run_bench "scenarios/bench.conf" bc 1000 10000
    run_bench "scenarios/bench.conf" bc 5000 10000
fi

# python3 python/trace_log.py
