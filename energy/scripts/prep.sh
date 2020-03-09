#!/bin/bash

# ---------------------------------
# This script splits up articles within rtf files, sets the first line (article title) as the file name, and removes duplicates
# ---------------------------------

#Converts all rtf files to plain text files (.txt)
for f in *.rtf; do textutil -convert txt "$f"; done

#Prints everything after "Bibliography" to a new truncated file, then deletes the temporary original text file
for f in *.txt; 
	do 
		sed -n '/Bibliography/,$p' "$f" > "$f"-trunc.txt;
		rm "$f";		
done

#Splits the truncated text file into individual articles
for f in *trunc.txt;
	do
		#The -f flag gives file a prefix that is the original file name
		csplit -k -f "$f" "$f" /'End of Document'/ {99};
done

#Converts split files into .txt files
for f in *trunc.txt??;
	do 
		textutil -convert txt "$f";				
done

#Removes temporary split files and deletes files under 2kb, which are basically the extra citations at the end of the document
find . -name "*trunc*" -size -2 -delete;

#Removes the first line of each document (usually "End of Document"). Then renames the file with filename + an index number, removing spaces and slashes

i=1
for f in *trunc*;
	do 

		sed -i '' -e '1d' "$f" #remove spaces in the first line	
		LINE=`echo "$(head -1 "$f")" | cut -c1-30 | tr -d '[:space:][:punct:]'` #takes first 30 characters of first line of article and removes spaces and punctuation

		#Checks to see if it's a "No Headline in Original" or "WEITERE BRIEFE" article
		if [ "$(sed -n '/^No Headline In Original/p;q' "$f")" ] || [ "$(sed -n '/^WEITERE BRIEFE/p;q' "$f")" ]
			#then printf '%s\n' "$f"
			then mv "$f" "$LINE-$i".txt
			let i++
		else 
			mv "$f" "$LINE".txt
		fi		

done