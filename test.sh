#!/bin/bash

bold=$(tput bold)
normal=$(tput sgr0)


function compare
{
	input=$1
	name=$(basename "$input")

	./flp18-log < $input > "./outputs/$name"
    var=$(comm -3 ./outputs/$name ./refs/$name)

   	if [ -z "$var" ]
	then
    	nd="\033[1;32m${bold}PASS${normal}\033[0m"
	else
    	nd="\033[1;31m${bold}FAIL${normal}\033[0m"
	fi

	echo -e  "Processed $name: $nd"

}

rm -R -f ./outputs
mkdir outputs

echo -e "\033[0;35m${bold}Starting deterministic tests...${normal}\033[0m"
for input in $(ls ./tests/*) ; do
		compare $input
done