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

all_ni_crime_data$Crime.type <- as.factor(sapply(as.character(all_ni_crime_data$Crime.type), switch, 
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
                                                 'Possession of weapons' = 'POWE',
                                                 'Other crime'= 'OTCR'))

write.csv(all_ni_crime_data, "../NI_Crime_Data/AllNICrimeData.csv")

#(d) Using the plot() function, show a plot of each crime frequency from the
#    crime.type field. Specify relevant options such as title, axes labels, bar colours.
#    Provide a detailed discussion about what you found from this chart.

crime_cat_count <- table(all_ni_crime_data$Crime.type)
CrimeFreq <- prop.table(crime_cat_count)
plot(CrimeFreq, main = "Bar plot of Crime Frequency", xlab = "Crime Type", 
     ylab = "Frequency", col = "#69b3a2", las = 2)

# (e) Modify the AllNICrimeData dataset so that the Location attribute contains only a
#     street name. For example, the attribute value “On or near Westrock Square” should
#     be modified to only contain “Westrock Square”. Modify the resultant empty location
#     attributes with a suitable identifier. Show a sample of your data.

all_ni_crime_data$Location <- substring(all_ni_crime_data$Location, 12)
all_ni_crime_data$Location[all_ni_crime_data$Location == ""] <- NA

head(all_ni_crime_data, 10)

# (f) Choose 5000 random samples of crime data from the AllNICrimeData dataset
#     where the location attribute contains location information. This means that the
#     location information should NOT contain an NA identifier. Set the seed value to 100.
#     Store this data in a data frame called random_crime_sample. Then create a
#     function called find_a_town that uses the CleanNIPostcodeData data frame to find
#     correct town/city information for each location variable within the
#     random_crime_sample dataset. Save each matched town into the
#     random_crime_sample data frame under the column name Town.

crime_with_location <- all_ni_crime_data[!is.na(all_ni_crime_data$Location),]

set.seed(100)
random_crime_sample <- crime_with_location[sample(nrow(crime_with_location), 5000),]

clean_ni_postcode_data <- read.csv("../NI_Crime_Data/CleanNIPostcodeData.csv")
# Load the mapping file between postcode x and y coords and long/lat data. Generated by using the 
# Generate_Postcode_LongLat.R script in this repo
coords <- read.csv('PreComputed/PostCode_XY_to_LL.csv')
clean_ni_postcode_data <- merge(x = clean_ni_postcode_data, y = coords, by.x = 'Primary_Key', by.Y = 'postcode_PK')

find_a_town <- function(crime_data) {
  crime_data <- random_crime_sample
  # First assign a crime id to the input
  crime_data$CrimeID <- 1:nrow(crime_data)
  # Uppercase the location because postcodes have uppercase values
  crime_data$Location <- toupper(crime_data$Location)
  
  # Find all rows where Location in crime data is a Primary Thoroughfare in the postcode data
  byPrimaryThoroughfare <- merge(x = crime_data, y = clean_ni_postcode_data[!is.na(clean_ni_postcode_data$Primary_Thorfare), 
                                                                            c('Primary_Thorfare', 'Town', 'Townland', 'Long', 'Lat', 'Primary_Key')], 
                                 by.x = "Location", by.y = "Primary_Thorfare", all.x = TRUE, all.y = TRUE)
  # Find all rows where Location in crime data is a Locality in the postcode data
  byLocality <-  merge(x = crime_data, y = clean_ni_postcode_data[!is.na(clean_ni_postcode_data$Locality), 
                                                                  c('Locality', 'Town', 'Townland', 'Long', 'Lat', 'Primary_Key')], 
                       by.x = "Location", by.y = "Locality", all.x = TRUE, all.y = TRUE)
  
  # Combine the datasets that matched crime to Street or Locality
  towns <- rbind(byPrimaryThoroughfare, byLocality)
  towns <- towns[!is.na(towns$CrimeID),]
  # Calculate a measure of distance from the crime Long/Lat to the postcode Long/Lat
  towns$TotalLocationDiff <- (towns$Longitude - towns$Long)^2 + (towns$Latitude - towns$Lat)^2
  
  # Sort the data by crime ID and the location measure
  towns <- towns[with(towns, order(CrimeID, TotalLocationDiff)),]
  # Collapse the dataset back down to the first row per CrimeID, and return the original dataset plus location info
  towns[!duplicated(towns$CrimeID) & !is.na(towns$CrimeID), c('Location', 'Month', 'Longitude', 'Latitude', 
                                                              'Crime.type', 'Town', 'Townland', 'Primary_Key')]
}

random_crime_sample <- find_a_town(random_crime_sample)

# Inspect the data after the Town was added
head(random_crime_sample[with(random_crime_sample, order(Location)),], 10)
random_crime_sample[is.na(random_crime_sample$Town),]

# (g) Create a function called add_town_data that examines the information from
#     each crime record in random_crime_sample and matches each record with
#     relevant data in the VillageList.csv file. Add the population attribute to the
#     random_crime_sample data frame.

village_list <- read.csv("../NI_Crime_Data/VillageList.csv")


# (h) Update the random sample AllNICrimeData dataset so that it contains only the
#     following items
#     • Month
#     • Longitude
#     • Latitude
#     • Location
#     • Crime type
#     • City-Town-Village
#     • Population
#     Save this data into a csv file called random_crime_sample.


# (i) Now we want to display crime for both cities in Northern Ireland which are Belfast
#     and Derry. From the random_crime_sample data frame, sort the chart data by crime
#     type for each city and then show both charts side-by-side by setting relevant
#     graphical parameters using the par() command. Show a suitable main title for the
#     bar chart, and suitable x and y-axis labels and scaling. Make sure all labels on the xaxis can be read. Show the bar plot in your CA document.
#     Display both charts and provide some detailed discussions about the results. 

