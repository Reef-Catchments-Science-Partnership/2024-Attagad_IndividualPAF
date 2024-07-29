#SCRIPT:
#Read in stacked years and sites dataset of pesticide concentration data
#NB: It is assumed that the stacked data is made up of the 182 day risk window only, i.e. 182 days from the first flush.- Irrelevant for Nat
#Run Sampling.Year() function over data to create the Sampling.Year variable used for splitting the dataset
#Split by site and sampling year variables
#To each split apply the treat_LORs_df() function to pesticide columns currently included in the Pesticide Risk Metric
#Recombine into a single dataset
#Export the single treated dataset

library(lubridate)

#Function which takes a date or vector of dates and returns a factor of the sampling year it is in
findSamplingYr <- function(dates){
  
  require(lubridate)
  mnth <- month(dates)
  yr <- year(dates)
  sampleYr <- character(length(dates))
  
  for(i in 1:length(dates)){
    if(mnth[i] >= 7)
      sampleYr[i] <- paste(yr[i], "-", yr[i]+1, sep="")
    else
      if(mnth[i] < 7) 
        sampleYr[i] <- paste(yr[i]-1, "-", yr[i], sep="")
      
      i+1
  }
  
  as.factor(sampleYr)
}


#Read in raw dataset, i.e. untreated LOR dataset
#Provide the filepath to the location of the raw dataset as first argument, or the filename if working directory is set in R.
Raw_data_LORs <- read.csv("Tully_22_Pesti_Clean.csv", header=TRUE, colClasses = "character")
#Raw_data_LORs <- Raw_data_LORs[-c(26)] # remove unnecessary columns
str(Raw_data_LORs)

#Tidy column names
#Please note, depending on the input worksheet, the following code may not be appropriate in tidying up the columns and may need to be adapted. 
#Current code applies to an Art Report exported from WASP (Water Analysis Sampling Program).
#names(Raw_data_LORs)

{
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "Site.Code")] <- "STATION"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "Site.Name")] <- "SITENAME"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "Date.Time")] <- "DATE.TIME.SAMPLED"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "X2.4.D")] <- "'2,4-D'"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "Metsulfuron.methyl")] <- "Metsulfuron"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "Haloxyfop..acid.")] <- "Haloxyfop"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "Isoxaflutole.metabolite..DKN.")] <- "Isoxaflutole"
names(Raw_data_LORs)[which(colnames(Raw_data_LORs) == "MCPA..Monochlorophenoxyacetic.acid.")] <- "MCPA"
}


Raw_data_LORs$DATE.TIME.SAMPLED <- as.POSIXct(Raw_data_LORs$DATE.TIME.SAMPLED, format = "%d/%m/%Y %H:%M",  tz = "Australia/Brisbane")
#Raw_data_LORs$DATE <- as.Date(Raw_data_LORs$DATE.TIME.SAMPLED, format="%d/%m/%Y",  tz = "Australia/Brisbane")
str(Raw_data_LORs)


#Create sampling year variable
Raw_data_LORs$Sampling.Year <- findSamplingYr(Raw_data_LORs$DATE) #not required already have this

#Split raw dataset into list of datasets by site and sampling year
list_site_year_dataframes <- split(Raw_data_LORs, f=list(Raw_data_LORs$SITENAME, Raw_data_LORs$Sampling.Year), drop=TRUE)

########################################################################################################################################################
#Creating the treat_LORs_df() function

#FUNCTION:
#Pass the function a dataset and a character vector with the names of the pesticide columns for which the LOR's need to be treated
    #Note these names need to match exactly and are case sensitive. 
    #The names in the list, the dataset columns and the relative_LOR_multipliers table HAVE TO MATCH EXACTLY
        #Mismatches in the relative_LOR_mulitipliers and raw dataset will result in 'NA's' in the relevant column/s in the results from this script
        #Mismatches in any other of the table columns will result in an error which will halt the running of the script
#Convert LOR's according to following rules:
    #For each pesticide of each site-sampling year combination:
        #Find the first detection
        #All LOR's before the first detect are converted to a very small number (1E-11) so that they do not contribute in any meaningful way to the final estimate of risk
        #Detects are left as is
        #All LOR's after the first detect are multiplied by a relative guideline value factor which is matched by pesticide name and taken from the relative_LOR column in the relative_LOR_multipliers table

#Create table of pesticides and their relative LOR multipliers
#Please note, any additions of pesticides or changes to relative GLV's also need to be added or changed here

relative_LOR_mulitipliers <- data.frame(analyte = c("Chlorpyrifos", "Fipronil", "Imidacloprid", "Haloxyfop", "Imazapic", "Metsulfuron", "Pendimethalin", "Metolachlor", 
                                                    "'2,4-D'","MCPA", "Fluroxypyr", "Triclopyr", "Isoxaflutole", "Ametryn", "Atrazine", "Prometryn", 
                                                    "Terbuthylazine","Tebuthiuron", "Simazine", "Diuron", "Hexazinone","Metribuzin"),
                                           relative_LOR = c(0.0000009459, 0.000004776, 0.000136913, 1, 0.0001092979, 0.0000101446, 0.00011788, 0.0000173108,
                                                            0.008930644, 0.00002295733, 0.274507874, 0.00052679, 0.000632348, 0.000213405, 0.000768738, 0.000226446,
                                                            0.001336659, 0.012641533, 0.051769031, 0.000197592, 0.004380853, 0.005731504), stringsAsFactors = FALSE) 


#Please note, any additions of pesticides also need to be added here

names_pesticide_columns <- c("Chlorpyrifos", "Fipronil", "Imidacloprid", "Haloxyfop", "Imazapic", "Metsulfuron", "Pendimethalin", "Metolachlor", "'2,4-D'",
                             "MCPA", "Fluroxypyr", "Triclopyr", "Isoxaflutole", "Ametryn", "Atrazine", "Prometryn", "Terbuthylazine",
                             "Tebuthiuron", "Simazine", "Diuron", "Hexazinone","Metribuzin")

treat_LORs_df <- function(dataset, names_pesticide_columns){
  
  for(i in 1:length(names_pesticide_columns)){
    
    #Gives index of first FALSE when searching for "<" symbol (first detect)
    #Suppressed warnings relate to the occurrence when no detections are found (all values are <LOR's)
    first_detect <- suppressWarnings(min(which(grepl("<", dataset[,names_pesticide_columns[i]]) == FALSE)))
    
    #All detects
    detects <- which(grepl("<", dataset[,names_pesticide_columns[i]]) == FALSE)
    

    for(j in 1:length(dataset[,names_pesticide_columns[i]])){
      
      #Rules for treating <LOR's
      dataset[j, names_pesticide_columns[i]] <- ifelse(j < first_detect, "0.00000000001", 
                                                       ifelse(j %in% detects, dataset[j, names_pesticide_columns[i]], 
                                                              as.numeric(gsub(pattern="<", replacement = "", dataset[j, names_pesticide_columns[i]])) * relative_LOR_mulitipliers$relative_LOR[which(relative_LOR_mulitipliers$analyte == names_pesticide_columns[i])]))
      
      dataset
      
    }
    
  }  
  
  dataset
}



########################################################################################################################################################
#Apply treat_LORs_df to the list of split datasets

#Create copy of list_site_year_dataframes to replace with treated LORs
updated_LOR_list <- list_site_year_dataframes


#Apply treat_LORs_df to the list of split datasets and update with the results
for(k in 1:length(list_site_year_dataframes)){
  
  updated_LOR_list[[k]] <- treat_LORs_df(list_site_year_dataframes[[k]], names_pesticide_columns)
  
  updated_LOR_list

  }


#Combine list of split treated datasets into a single dataset
Treated_LORs_Baseline <- do.call(rbind, updated_LOR_list)

str(Treated_LORs_Baseline)

Treated_LORs_Baseline <- Treated_LORs_Baseline[, c("STATION", "SITENAME", "Sampling.Year", "DATE.TIME.SAMPLED",  
                                                   "Chlorpyrifos", "Fipronil", "Imidacloprid", 
                                                   "Haloxyfop", "Imazapic", "Metsulfuron", "Pendimethalin", "Metolachlor", "'2,4-D'",
                                                   "MCPA", "Fluroxypyr", "Triclopyr", "Isoxaflutole", "Ametryn", "Atrazine", "Prometryn", "Terbuthylazine",
                                                   "Tebuthiuron", "Simazine", "Diuron", "Hexazinone","Metribuzin")]



#Provide the filepath including a filename and file extension (e.g. .csv) to the "file" argument of the function, or just provide the filename and extension if the working directory is set in R.
#Export treated dataset as .csv file to specified location
write.csv(x=Treated_LORs_Baseline, file="RussellRiver_Treated_LORs_22.csv", row.names = FALSE)

#Remove all objects from the R environment
rm(list=ls())




