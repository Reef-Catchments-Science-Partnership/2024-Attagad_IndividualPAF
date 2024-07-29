# Read the CSV file into a data frame
# Use this script to clean the raw data of 22 pesticides in PRM from Tabil Data Portal 
Test <- read.csv("Input_your_file_name", na.strings = c("#N/A", "NA","DA","LS"), stringsAsFactors = FALSE)
str(Test)

# Replace NA and '#N/A' with ''
Test[is.na(Test)] <- ""
Test[Test == "#N/A"] <- ""
Test[Test == "DA"] <- ""
Test[Test == "LS"] <- ""
str(Test)

# Write the updated data frame to the same CSV file
write.csv(Test, "Input_your_output_file_name", row.names = FALSE, quote = FALSE)

