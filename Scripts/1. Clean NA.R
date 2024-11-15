# Read the CSV file into a data frame
Test <- read.csv("Tully_18_Pesti.csv", na.strings = c("#N/A", "NA","DA","LS"), stringsAsFactors = FALSE)
str(Test)
#Tully_18_Pesti.csv can be replaced with your .csv 

# Replace NA and '#N/A' with ''
Test[is.na(Test)] <- ""
Test[Test == "#N/A"] <- ""
Test[Test == "DA"] <- ""
Test[Test == "LS"] <- ""
str(Test)

# Write the updated data frame to the same CSV file
write.csv(Test, "Tully_18_Pesti_Clean.csv", row.names = FALSE, quote = FALSE)

