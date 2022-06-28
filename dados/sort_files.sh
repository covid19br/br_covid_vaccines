#!/bin/bash

# loop sobre arquivos da linha de comando
for i in "$@"; do
    (head -n 1 "$i" && tail -n +2 "$i" | sort -k 1 -t"," --parallel=4 -u ) > "sorted_${i}" &&
        rm "${i}" &&
        echo "sorted ${i}"
done
