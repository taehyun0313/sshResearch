#!/bin/bash

if test $# -eq 4
then
	FILENAME=$1
	N=$2
	COL=$3
	TS=$4
else
	echo 'usage: ./run.sh [filename] [number_split] [column_number] [timeseries]'
	exit
fi

FILENAME="${FILENAME%.*}"

rm -rf $FILENAME-*.res.tmp
rm -rf $FILENAME-*.csv

Rscript splitData.R $FILENAME.csv $N

FILES=$FILENAME-*

for f in $FILES
do
	Rscript rowsAndSum.R "$TS" $f $COL &
done

PIDS="$(pgrep -P $$)"
for pid in $PIDS
do
	wait $pid
done

Rscript addUp.R $FILENAME $N

rm -rf $FILENAME-*.res.tmp
rm -rf $FILENAME-*.csv
