#!/bin/bash

#the purpose of this script is to
#generate a list of users that have accounts at uchicago.computer
#Howeover, it also uses information gathered in Step 1 to tie users to possible passwords


#Generates A list of unames and their associated passwords
input="amiller68-problem1.pot"
while IFS= read -r line
do
	hash=$(echo $line | cut -f1 -d:)
	pw=$(echo $line | cut -f2 -d:)
#	echo "Hash: $hash"
#	echo "PW: $pw"
	match=$(grep $hash name:hash.txt)
#	echo "Match: $match"
	echo "${match/$hash/$pw}" >> temp
done < "$input"

outfile="guesses.txt"
input="amiller68-problem2.txt"
while IFS= read -r name
do
	if grep -q "$name:" temp; then
		echo "Found Match"
		match=$(grep $name temp)
		echo "$match" >> $outfile
	else
		echo "No Recorded PW"
		#echo "$name:NULL" >> $outfile
	fi
done < "$input"

rm temp
