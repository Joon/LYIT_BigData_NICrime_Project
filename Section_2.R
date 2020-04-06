#Complete these tasks using the NICrimeData datasets described above.
#(a) Using R, amalgamate all of the crime data from each csv file into one dataset.
#    Save this dataset into a csv file called AllNICrimeData. Count and show the number
#    of rows in the AllNICrimeData dataset. Do not hard code the dataset path into your R
#    code.

data_folders_root <- "../NI_Crime_Data/NI Crime Data/"
all_folders <- list.dirs(path = data_folders_root, full.names = TRUE)

all_ni_crime_data <- NULL

for(folder in all_folders[-1]) {
  folder_files <- list.files(folder, full.names = TRUE)
  folder_frame <- read.csv(folder_files[1], header = TRUE)
  if (is.null(all_ni_crime_data)) {
    all_ni_crime_data <- folder_frame
  } else {
    all_ni_crime_data <- rbind(all_ni_crime_data, folder_frame)
  }
}

nrow(all_ni_crime_data)

write.csv(all_ni_crime_data, "../NI_Crime_Data/AllNICrimeData.csv")

#(b) Modify the structure of the newly created AllNICrimeData csv file and remove
#    the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,
#    last outcome and context. Save this new structure and show the structure of the
#    modified file.


retain_columns <- c("Month", "Longitude", "Latitude", "Location", "Crime.type")

all_ni_crime_data <- all_ni_crime_data[,retain_columns]
write.csv(all_ni_crime_data, "../NI_Crime_Data/AllNICrimeData.csv")

head(all_ni_crime_data)

#(c) In the AllNICrimeData csv file, shorten each crime type as follows:
#  Crime type                   Shortened description
#  Anti-social behaviour        ASBO
#  Bicycle theft                BITH
#  Burglary                     BURG
#  Criminal damage and arson    CDAR
#  Drugs                        DRUG
#  Other theft                  OTTH
#  Public order                 PUBO
#  Robbery                      ROBY
#  Shoplifting                  SHOP
#  Theft from the person        THPR
#  Vehicle crime                VECR
#  Violence and sexual offences VISO
#  Other crime                  OTCR

all_ni_crime_data$Crime.type <- as.character(sapply(as.character(all_ni_crime_data$Crime.type), switch, 
       'Anti-social behaviour' = 'ASBO', 
       'Bicycle theft'  = 'BITH',
       'Burglary' = 'BURG',
       'Criminal damage and arson' = 'CDAR',
       'Drugs' = 'DRUG',
       'Other theft' = 'OTTH',
       'Public order' = 'PUBO',
       'Robbery' = 'ROBY',
       'Shoplifting' = 'SHOP',
       'Theft from the person' = 'THPR',
       'Vehicle crime' = 'VECR',
       'Violence and sexual offences' = 'VISO',
       # Note that no abbreviation was provided for Possession of weapons, hence it is being 
       # combined with 'Other' and given the code OTCR
       'Possession of weapons' = 'OTCR',
       'Other crime'= 'OTCR'))

write.csv(all_ni_crime_data, "../NI_Crime_Data/AllNICrimeData.csv")

#(d) Using the prop.table() function, create a one-way proportion table called
#    CrimeFreq using the AllNICrimeData dataset to compute the frequencies of each
#    crime type.

crime_cat_count <- table(all_ni_crime_data$Crime.type)
CrimeFreq <- prop.table(crime_cat_count)

#(e) Show a plot of each crime frequency from the CrimeFreq table. Specify relevant
#options such as title, axes labels, bar colours.

barplot(CrimeFreq, main = "Bar plot of Crime Frequency", xlab = "Crime Type", 
        ylab = "Frequency", col = "#69b3a2", las = 2)

#(f) Factorise the Crime type attribute. Show the modified structure.
all_ni_crime_data$Crime.type <- as.factor(all_ni_crime_data$Crime.type)

str(all_ni_crime_data)

#(g) Modify the AllNICrimeData dataset so that the Location attribute contains only a
#street name. For example, the attribute value “On or near Westrock Square” should
#be modified to only contain “Westrock Square”. Modify the resultant empty location
#attributes with a suitable identifier.

all_ni_crime_data$Location <- substring(all_ni_crime_data$Location, 12)
all_ni_crime_data$Location[all_ni_crime_data$Location == ""] <- NA

#(h) Create a function called get_town_info that examines the information from each
#    crime record in ALLNICrimeData and matches each record with data in the
#    VillageList.csv file. Add population and status attributes to the crime data. Then for
#    each city, town and village, calculate the percentage crime for each crime type.



#(i) Choose 1000 random samples of crime data from the AllNICrimeData dataset
#   where the location attribute contains location information. This means that the
#    location information should NOT contain an NA identifier. Store this data in a data
#    frame called random_crime_sample.


#(j) Save the random crime sample data as random_crime_sample.csv.
#(k) Update the random sample AllNICrimeData dataset so that it contains only the
#following items
#• Month
#• Longitude
#• Latitude
#• Location
#• Crime type
#• City-Town-Village
#• Population
#• Status
#• Percent-Crime
#Save this data into a new data frame called updated_random_sample. Then create
#another data frame called chart_data using the updated_random_sample data
#frame. Sort the chart_data by crime type and then by percent crime for City-TownVillage = Belfast. Show the summary statistics for the percent crime from this
#chart_data data frame.

#(l) Create a bar plot of the crime type from the chart_data data frame. Show a
#suitable main title for the bar chart, and suitable x and y-axis labels. Make sure all
#labels on the x-axis can be read. Show the bar plot in your CA document.