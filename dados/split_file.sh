#!/usr/bin/bash

if [ "$#" -gt 2 ]; then
    echo "ERRO: excesso de argumentos: "
    echo "$@"
    exit 1
fi

f=$1
n=$2
suffix=".csv"
foo=${f%"$suffix"}
echo "splitting $f"

# safely create temp file
tmp_file=`mktemp -p .`
tail -n +2 "$f" > $tmp_file
split $tmp_file split_"$foo"_ --number=l/"$n" -d --additional-suffix=".csv"
for file in split_"$foo"*.csv
do
    head -n 1 "$f" > $tmp_file
    cat "$file" >> $tmp_file
    mv -f $tmp_file "$file"
done

