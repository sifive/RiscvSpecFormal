# This script generates a Slurm job that runs runElf for most
# possible runElf configurations.

source common.sh

slurmNumCpus='2'
slurmMinMemSize='3G'
slurmMaxMemSize='15G'
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

You can observe the test results by monitoring coq-sim.out and haskell-sim.out

  watch tail --lines 40 runTestsUsingSlurmResults/coq-sim.out
  watch tail --lines 40 runTestsUsingSlurmResults/haskell-sim.out

OPTIONS
-------

  -h|--help
  Displays this message.

AUTHORS
-------

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

function isPTest () {
  local testName=$1
  [[ ${testName:7:1} == 'p' ]]
}

function getMemLimit () {
  local testName=$1
  local simName=$2
  if isPTest $testName || [[ $simName == 'haskell-sim' ]]
  then echo $slurmMinMemSize
  else echo $slurmMaxMemSize
  fi
}

function runTest () {
  local sim=$1
  local xlen=$2
  local testName=$3
  local cmd="srun --cpus-per-task=$slurmNumCpus --mem=$(getMemLimit $testName $sim) runElf.sh --debug --verbose --$sim --xlen $xlen --path $testDir/$testName >> $resultsDir/$testName-$sim.out 2>&1; echo \"\$? $testName $xlen $sim\" >> $resultsDir/$sim.summary" 
  execute "$cmd"
}

rm -rf $resultsDir

rm -rf haskelldump
rm -rf coqdump
rm -rf verilogdump

mkdir $resultsDir
./doGenerate.sh --parallel --coq-sim
./doGenerate.sh --parallel --haskell-sim

for sim in ${sims[@]}
do
  for xlen in ${xlens[@]}
  do
    testNames=$(./blackList.sh $testDir $xlen)
    for testName in $testNames
    do
      runTest $sim $xlen $(basename $testName) &
      true
    done
  done
done
