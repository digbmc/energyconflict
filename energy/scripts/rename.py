#load libraries
import csv
import glob, os

#setting variables
filename = []
date = []
files = []
i = 0

# read in csv and write column data as lists
with open("energy_clean.csv") as csvfile:
	reader = csv.reader(csvfile)
	next(reader) #skips header
	for row in reader:
		filename.append(row[0])
		date.append(row[3])
		#print filename[i], date[i]

#for each .txt file in folder, if first line matches filename column in csv, rename filename to prepend date_clean column
#os.chdir("all_clean")
for file in glob.glob("*.txt"):
	files.append(file)
	files.sort()
	for file in files:
		if file == filename[i]:
			#print (date[i] + "-" + file)
			os.rename(file, (date[i] + "-" + file))
			i += 1