#!/bin/bash

# ---------------------------------
# This script creates a csv file that includes the 
# title, publication date, newspaper, and word count 
# of every text (.txt) file in the folder
# ---------------------------------

rm "list.csv"

#Creat a csv file
touch "list.csv"
echo -e "filename, title, newspaper, date, word count \n" > "list.csv"


for f in *.txt;
	do 
		v=`echo "$f"` #filename
		x=`echo "$(head -1 "$f")" | tr -d ','` #title, also removes any commas in title
		y=`awk 'NR==2' "$f"` #newspaper
		z=`awk 'NR==3' "$f" | sed 's/\Montag//g' | sed 's/\Dienstag//g' | sed 's/\Mittwoch//g' | sed 's/\Donnerstag//g' | sed 's/\Freitag//g'  | sed 's/\Samstag//g' | sed 's/\Sonntag//g' | grep -Eo '.*[0-9]{4}' | tr -d ',.'` #date, removing day of the week and extraneous trailing text that appears to be a metadata error
		a=`grep -hr "Length:" "$f" | tr -d "^Length:words[:space]"` #wordcount
		echo $v, $x, $y, $z, $a >> list.csv
done