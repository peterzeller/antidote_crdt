#!/usr/bin/env bash

set -e
set -x #debug
set -o xtrace

if [ $# -eq 0 ]; then
	echo "Specify test to run"
	exit
fi

TEST=$1
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR/../
mkdir -p _build
cd _build/

if [ ! [ -a basho_bench ] ]; then
	echo "Downloading basho-bench"
	git clone https://github.com/SyncFree/basho_bench/
fi
cd basho_bench
# Switch to antidote branch
git checkout antidote

# Clean repository:
git checkout .
git clean -f

# copy benchmark code to basho_bench
cp $DIR/*.erl ./src/

# build basho bench
make

cd $DIR/../

# Start node
EBIN=$(./rebar3 path --ebin)
erl -name 'antidote_crdt@127.0.0.1' -setcookie 'basho_bench' -noshell -pa $EBIN & pid_shell=$!
#./rebar3 shell --apps antidote_crdt --name 'antidote_crdt@127.0.0.1' --setcookie 'basho_bench' --noshell & pid_shell=$!

# Run the benchmark:
_build/basho_bench/_build/default/bin/basho_bench --results-dir $DIR/results benchmark/$TEST.config

# Create graph
Rscript --vanilla _build/basho_bench/priv/summary.r -i $DIR/results/current

# Kill node
kill $pid_shell

