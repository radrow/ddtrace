#!/usr/bin/bash
ARG=$1

make

SEED=0
TIMESTAMP=$(date +%d-%m-%Y--%T)

if [[ ! $DDMON_OUTDIR ]]
then
    DDMON_OUTDIR="${PWD}/output"
fi

OUTDIR="${DDMON_OUTDIR}/${TIMESTAMP}"
mkdir -p "${OUTDIR}"

OUTDIR=$(realpath "${OUTDIR}")
echo "Output: ${OUTDIR}"

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

    # STATS_FILE="${OUTDIR}/${TEST_TYPE}_p${PDELAY}_to${TIMEOUT}.csv"
    # LOGS_DIR="${OUTDIR}/${TEST_TYPE}_p${PDELAY}_to${TIMEOUT}"
    STATS_FILE="${OUTDIR}/${TEST_TYPE}_p${PDELAY}.csv"
    LOGS_DIR="${OUTDIR}/${TEST_TYPE}_p${PDELAY}"
    mkdir -p "${LOGS_DIR}"

    time eval_echo ./ddmon\
              "${TEST_FILE}"\
              "--seed=${SEED}"\
              --trace-proc\
              --silent\
              $(if [[ "$PDELAY" = "unmonitored" ]]; then echo -n --unmonitored; else echo -n "--probe-delay=$PDELAY"; fi)\
              "--stats-csv=${STATS_FILE}"\
              "--csv=${LOGS_DIR}/"\
              "--timeout=$TIMEOUT"

    append_meta "${STATS_FILE}" "${TEST_FILE}" "${PDELAY}" "${TIMEOUT}"
}

if [[ -z "${ARG:-}" ]]; then
    run_bench "scenarios/ts.conf" ts -1 10000
    run_bench "scenarios/ts.conf" ts 1000 10000
    run_bench "scenarios/ts.conf" ts 5000 10000

    run_bench "scenarios/bc.conf" bc unmonitored 10000
    run_bench "scenarios/bc.conf" bc -1 10000
    run_bench "scenarios/bc.conf" bc 1000 10000
    run_bench "scenarios/bc.conf" bc 5000 10000
fi

if [[ "${ARG:-}" == 'small' ]]; then
    run_bench "scenarios/ts-small.conf" ts -1 1000
    run_bench "scenarios/ts-small.conf" ts 1000 3000
    run_bench "scenarios/ts-small.conf" ts 5000 80000

    run_bench "scenarios/bc-small.conf" bc unmonitored 1000
    run_bench "scenarios/bc-small.conf" bc -1 1000
    run_bench "scenarios/bc-small.conf" bc 1000 3000
    run_bench "scenarios/bc-small.conf" bc 5000 8000
fi

if [[ "${ARG:-}" == 'medium' ]]; then
    run_bench "scenarios/ts-mid.conf" ts -1 10000
    run_bench "scenarios/ts-mid.conf" ts 1000 10000
    run_bench "scenarios/ts-mid.conf" ts 5000 10000

    run_bench "scenarios/bc-mid.conf" bc unmonitored 5000
    run_bench "scenarios/bc-mid.conf" bc -1 10000
    run_bench "scenarios/bc-mid.conf" bc 1000 10000
    run_bench "scenarios/bc-mid.conf" bc 5000 10000
fi


# Save under deterministic name
DETDIR="${OUTDIR}/../last"
if [ -L "${DETDIR}" ]
then
    unlink "${DETDIR}"
fi
eval_echo ln -s "${OUTDIR}" "${DETDIR}"


# Find a good timeseries example for each test type
for s in '-1' 1000 5000
do
    TS_FILE="${OUTDIR}/ts_p${s}.csv"

    rm -f $TS_FILE

    # First try to get one that presents a deadlock.
    grep -rnl "${OUTDIR}/ts_p${s}" -e 'deadlocked' -m 1 | head -n 1 | xargs -I{} cp {} $TS_FILE

    # Whoops, no deadlock. It's fine, just get anything then.
    if [ ! -f $TS_FILE ]
    then
        find "${OUTDIR}/ts_p${s}" -type f -print -quit | xargs -I{} cp {} $TS_FILE
    fi
done

eval_echo python3 python/trace_log.py

# Fix names
mv -f "${OUTDIR}/bc_site.pdf" output/fig_15_a.pdf
mv -f "${OUTDIR}/bc_probes.pdf" output/fig_15_b.pdf
mv -f "${OUTDIR}/bc_sent.pdf" output/fig_15_c.pdf

# mv -f "${OUTDIR}/ts_p-1.pdf" output/fig_16_a.pdf
# mv -f "${OUTDIR}/ts_p1000.pdf" output/fig_16_b.pdf
# mv -f "${OUTDIR}/ts_p5000.pdf" output/fig_16_c.pdf

mkdir -p ./output/fig_16_a/
for f in $(find "${OUTDIR}/ts_p-1/" -type f -exec basename {} \;)
do
    outfile="./output/fig_16_a/${f%.*}.pdf"
    python3 python/trace_log.py -t "${OUTDIR}/ts_p-1/${f}" -o $outfile --pcolor red
done

mkdir -p ./output/fig_16_b/
for f in $(find "${OUTDIR}/ts_p1000/" -type f -exec basename {} \;)
do
    outfile="./output/fig_16_b/${f%.*}.pdf"
    python3 python/trace_log.py -t "${OUTDIR}/ts_p1000/${f}" -o $outfile --pcolor orange
done

mkdir -p ./output/fig_16_c/
for f in $(find "${OUTDIR}/ts_p5000/" -type f -exec basename {} \;)
do
    outfile="./output/fig_16_c/${f%.*}.pdf"
    python3 python/trace_log.py -t "${OUTDIR}/ts_p5000/${f}" -o $outfile --pcolor limegreen
done

find output -name "*.pdf" | xargs -n1 chmod 666
find output -type d | xargs -n1 chmod 777

echo Done
