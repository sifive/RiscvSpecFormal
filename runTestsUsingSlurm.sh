# This script generates a Slurm job that runs runElf for most
# possible runElf configurations.

source common.sh

slurmNumCpus='2'
slurmMemSize='10G' # 5G < mem req for rv64ui-v-lb <= 10G
testDir='/nettmp/netapp1a/vmurali/riscv-tests/isa'
resultsDir='runTestsUsingSlurmResults'
sims=('coq-sim' 'haskell-sim')
xlens=('64' '32')

options=$(getopt --options="h" --longoptions="help" -- "$@")
[ $? == 0 ] || error "Invalid command line. The command line includes one or more invalid command line parameters."

eval set -- "$options"
while true
do
  case "$1" in
    -h | --help)
      cat <<- EOF
Usage
-----

 ./runTestsUsingSlurm.sh [OPTIONS]

SUMMARY
-------

For every possible combination of XLEN and simulator (excluding
Verilog) and RISCV test, this script generates a SLURM job that runs
the test using runElf with the selected configuration parameters
and stores the results in runTestsUsingSlurmResults.

Each time these jobs are invoked, they append their log output to
their associated log file.

A summary of the job results can be found in
runTestsUsingSlurmResults/summary.out. 0 indicates success. 1
indicates that the test failed. 137 indicates that the slurm job
exceeded its memory limit. Other code indicate failure.

You can observe the slurm queue using:

  watch squeue -u USERNAME

You can observe the test results using:

  watch tail --lines 40 runTestsUsingSlurmResults/summary.out

OPTIONS
-------

  -h|--help
  Displays this message.

AUTHORS
-------

* Murali Vijayaraghavan
* Larry Lee
EOF
      exit 0;;
    --)
      shift
      break;;
  esac
done
shift $((OPTIND - 1))

mkdir -p $resultsDir

function runTest () {
  local sim=$1
  local xlen=$2
  local testName=$3
  local cmd="srun --cpus-per-task=$slurmNumCpus --mem=$slurmMemSize runElf.sh --verbose --$sim --xlen $xlen --path $testDir/$testName >> $resultsDir/$testName-$sim.out 2>&1; echo \"\$? $testName $xlen $sim\" >> $resultsDir/summary.out" 
  execute "$cmd"
}

for sim in ${sims[@]}
do
  for xlen in ${xlens[@]}
  do
    testNames=$(./blackList.sh $testDir $xlen)
    for testName in $testNames
    do
      runTest $sim $xlen $(basename $testName) &
    done
  done
done
