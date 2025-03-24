ARG=$1

make

SEED=2137
TIMESTAMP=$(date +%d-%m-%Y--%T)

append_meta() {
    OUT_FILE=$1
    TEST_FILE=$2
    PDELAY=$3
    TIMEOUT=$4

    METADATA="# TEST FILE=$(md5sum ${TEST_FILE}); PROBE DELAY=${PDELAY}; TIMEOUT=${TIMEOUT}; SEED=${SEED}"
    # echo $METADATA >> $OUT_FILE
}

eval_echo() {
    echo "$*"
    "$@"
}

run_bench() {
    TESTFILE=$1
    TESTTYPE=$2
    PDELAY=$3
    TIMEOUT=$4

    [[ $TESTTYPE != bc && $TESTTYPE != ts ]] && echo "Bad test type ${TESTTYPE}" && return 1

    # Backup name
    OUT_FILE="${TESTTYPE}_${TIMESTAMP}.csv"
    OUT_FILE_ARG=$(if [[ $TESTTYPE = ts ]]; then echo -n "csv"; elif [[ $TESTTYPE = bc ]]; then echo -n "stats-csv"; else return 1; fi)

    time eval_echo ./dlstalk\
              "${TESTFILE}"\
              "--seed=${SEED}"\
              --trace-proc\
              --silent\
              $(if [[ $PDELAY = "unmonitored" ]]; then echo -n --unmonitored; else echo -n "--probe-delay=$PDELAY"; fi)\
              "--${OUT_FILE_ARG}=${OUT_FILE}"\
              "--timeout=$TIMEOUT"

    append_meta $OUT_FILE $TESTFILE $PDELAY $TIMEOUT

    # Save under deterministic name
    DETNAME="${TESTTYPE}_${PDELAY}.csv"
    eval_echo cp $OUT_FILE $DETNAME
}

# This works only because there are two options; execute all if none given.
if [[ $ARG != bc ]]; then
    run_bench scenarios/big.conf ts -1 20000
    run_bench scenarios/big.conf ts 500 20000
    run_bench scenarios/big.conf ts 5000 20000
fi

if [[ $ARG != ts ]]; then
    run_bench scenarios/bench.conf bc unmonitored 1000
    run_bench scenarios/bench.conf bc -1 10000
    run_bench scenarios/bench.conf bc 1000 10000
    run_bench scenarios/bench.conf bc 5000 20000
fi
