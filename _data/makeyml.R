# R files

readr = list.files("../rscripts/")
writer = "r.yml"

myfile = readr[1]
cat("- r: ", myfile, "\n", sep='', file= writer)

if(length(readr) > 1) {
	for(i in 2:length(readr)) {
		myfile = readr[i]
		cat("- r: ", myfile, "\n", sep='', file= writer, append=TRUE)
	}
}

# Data files

readdat = list.files("../data/")
writedat = "data.yml"

myfile = readdat[1]
cat("- data: ", myfile, "\n", sep='', file= writedat)

for(i in 2:length(readdat)) {
	myfile = readdat[i]
	cat("- data: ", myfile, "\n", sep='', file= writedat, append=TRUE)
}


# Exercises

readexercises = list.files("../exercises/")
writeexercises = "exercises.yml"

myfile = readexercises[1]
cat("- pdf: ", myfile, "\n", sep='', file= writeexercises)

if(length(readexercises) > 1) {
	for(i in 2:length(readexercises)) {
		myfile = readexercises[i]
		cat("- pdf: ", myfile, "\n", sep='', file= writeexercises, append=TRUE)
	}
}



# # # # Scribe files

# readscribe = list.files("../files/scribe/")
# writescribe = "scribe.yml"

# myfile = readscribe[1]
# cat("- scribe: ", myfile, "\n", sep='', file= writescribe)

# if(length(readscribe) > 1) {
	# for(i in 2:length(readscribe)) {
		# myfile = readscribe[i]
		# cat("- scribe: ", myfile, "\n", sep='', file= writescribe, append=TRUE)
	# }
# }

