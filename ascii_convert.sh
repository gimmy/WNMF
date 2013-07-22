#!/bin/bash
#
path=${PWD}

for file in s1/*.pgm; 
do 
./convert/pgmb_to_pgma.out $path/$file $path/img/`basename $file`; 
echo "Converto file " $path/$file "in" $path/img/`basename $file`;
done

# for i in 'ls'

# result=${PWD##*/}
# echo $result
