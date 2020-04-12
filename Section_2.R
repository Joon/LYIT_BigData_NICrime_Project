#Complete these tasks using the NICrimeData datasets described above.
#(a) Using R, amalgamate all of the crime data from each csv file into one dataset.
#    Save this dataset into a csv file called AllNICrimeData. Count and show the number
#    of rows in the AllNICrimeData dataset. Do not hard code the dataset path into your R
#    code.

data_folders_root <- "../NI_Crime_Data/NI Crime Data/"
all_folders <- list.dirs(path = data_folders_root, full.names = TRUE)

all_ni_crime_data <- NULL

# Skip the first folder name in the list, which is the root folder itself
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
# Load the mapping file between postcode x and y coords and long/lat data. This file was 
# generated by using the Generate_Postcode_LongLat.R script in this repo
coords <- read.csv('PreComputed/PostCode_XY_to_LL.csv')
clean_ni_postcode_data <- merge(x = clean_ni_postcode_data, y = coords, by.x = 'Primary_Key', by.Y = 'postcode_PK')


find_a_town <- function(crime_data) {
  # First assign a crime id to the input
  crime_data$CrimeID <- 1:nrow(crime_data)
  
  point_distance <- function(point1, point2) {
    dst <- sqrt((point1[1] - point2[1]) ^ 2+(point1[2] - point2[2]) ^ 2)
    return(dst)
  }

  # Loop through all the crime reports
  for(i in 1:nrow(crime_data)) {
    
    if (i %% 100 == 0) {
      print(sprintf("Calculating closest points for the sample crime dataset of 5000 items. Now at %s rows", i))
    }
    
    crime_row <- crime_data[i,]
    
    # for each crime, find a candidate set of possible post codes where it may have occurred
    possible_locations <- clean_ni_postcode_data[(clean_ni_postcode_data$Long <= crime_row$Longitude + 0.025) &  
                                                 (clean_ni_postcode_data$Long >= crime_row$Longitude - 0.025) &
                                                 (clean_ni_postcode_data$Lat <= crime_row$Latitude + 0.0015) &
                                                 (clean_ni_postcode_data$Lat >= crime_row$Latitude - 0.0015),]
    
    # Calculate the closest post code to the coordinate of the crime report
    # Step 1: Get crime location as a 1-row matrix with the long and lat value
    crime_location <- as.matrix(crime_row[,c('Longitude', 'Latitude')])
    # Step 2: Get the possible list of locations as a matrix of long / lat values
    postcode_locations <- as.matrix(possible_locations[, c('Long', 'Lat')])
    # Build a function to find the index of the point closest to the supplied point
    closest_to_crime <- function(y) which.min(apply(postcode_locations, 1, function(x) point_distance(x,y)))
    # Apply that function to find the closest point to long/lat of current crime row
    closest_point <- possible_locations[apply(crime_location, 1, closest_to_crime),]

    # Enrich the crime dataset with the Town of the closest postcode, use Townland if Town is blank
    if (is.na(closest_point$Town)) {
      crime_data$Town[crime_data$CrimeID == crime_row$CrimeID] <- as.character(closest_point$Townland)  
    } else {
      crime_data$Town[crime_data$CrimeID == crime_row$CrimeID] <- as.character(closest_point$Town)
    }
    crime_data$County[crime_data$CrimeID == crime_row$CrimeID] <- as.character(closest_point$County)
  }
    
  crime_data[, c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Town", "County")]
}

random_crime_sample <- find_a_town(random_crime_sample)

# Inspect the data after the Town was added
head(random_crime_sample, 10)
# Verify that all rows did receive a Town
random_crime_sample[is.na(random_crime_sample$Town),]

# (g) Create a function called add_town_data that examines the information from
#     each crime record in random_crime_sample and matches each record with
#     relevant data in the VillageList.csv file. Add the population attribute to the
#     random_crime_sample data frame.

village_list <- read.csv("../NI_Crime_Data/VillageList.csv")
# Clean up the City-Town-Village column name
colnames(village_list) <- c("City-Town-Village","Population", "Status", "County")
# Upper case the City-Town-Village so it matches with the crime data
village_list$`City-Town-Village` <- toupper(village_list$`City-Town-Village`)
# Upper case the County so it matches with the crime data
village_list$County <- toupper(village_list$County)
# Counties in village_list are stored like "County Antrim", while counties in 
# random_crime_sample are stored like "Antrim". Convert village_list to this format
village_list$County <- lapply(sapply(village_list$County, strsplit, ' '), `[[`, 2)

# Inspect DrapersTown, there is a duplicate row in it
village_list[village_list$`City-Town-Village` == "DRAPERSTOWN",]

add_town_data <- function(crime_data) {
  
  # First assign a crime id to the input
  crime_data$CrimeID <- 1:nrow(crime_data)
  
  # Rename Londonderry to Derry, the village list uses that name instead
  crime_data$Town[crime_data$Town == "LONDONDERRY"] <- "DERRY"
  # Some village names appear in different counties. Match on both town and county name, to ensure correct matching
  crime_with_village_pop <- merge(crime_data, village_list, all.x = TRUE, by.x = c("Town", "County"), by.y = c("City-Town-Village", "County"))
  crime_with_village_pop[duplicated(crime_with_village_pop$CrimeID),]
  
  # Belfast is spread across two counties, add special code to just deal with it
  crime_with_village_pop$Population[crime_with_village_pop$Town == "BELFAST"] <- village_list$Population[village_list$`City-Town-Village` == "BELFAST"]
  
  # There is one village (DRAPERSTOWN) that appears twice with very similar population numbers. Remove
  # any duplicated crime ids to ensure that this and any other duplicated village don't affect the statistics 
  crime_with_village_pop <- crime_with_village_pop[!duplicated(crime_with_village_pop$CrimeID),]
  
  crime_with_village_pop
}

random_crime_sample <- add_town_data(random_crime_sample)


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

# First limit the frame to the required columns
random_crime_sample <- random_crime_sample[, c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Town", "Population")]

# Then rename them
colnames(random_crime_sample) <- c("Month", "Longitude", "Latitude", "Location", "Crime Type", "City-Town-Village", "Population")

write.csv(random_crime_sample, "../NI_Crime_Data/random_crime_sample.csv")

# (i) Now we want to display crime for both cities in Northern Ireland which are Belfast
#     and Derry. From the random_crime_sample data frame, sort the chart data by crime
#     type for each city and then show both charts side-by-side by setting relevant
#     graphical parameters using the par() command. Show a suitable main title for the
#     bar chart, and suitable x and y-axis labels and scaling. Make sure all labels on the 
#     xaxis can be read. Show the bar plot in your CA document. Display both charts and 
#     provide some detailed discussions about the results. 

derry_crime <- random_crime_sample[random_crime_sample$`City-Town-Village` == "DERRY",]
derry_crime <- derry_crime[order(derry_crime$`Crime Type`),]

belfast_crime <- random_crime_sample[random_crime_sample$`City-Town-Village` == "BELFAST",]
belfast_crime <- belfast_crime[order(belfast_crime$`Crime Type`),]

derry_crime_cat <- table(derry_crime$`Crime Type`)
belfast_crime_cat <- table(belfast_crime$`Crime Type`)

opar <- par(no.readonly = TRUE)

graph_range <- range(0, derry_crime_cat, belfast_crime_cat)

par(mfrow = c(1, 2))
par(pin = c(3, 3))
par(oma=c(0,0,5,0))
par(mar = c(6.5, 6.5, 1.5, 1.5), mgp = c(5, 1, 0))

barplot(derry_crime_cat, ylim = graph_range, main = "Crime in Derry", xlab = "Crime Type", 
        ylab = "Sample Occurrences", col = "#69b3a2", las = 2, space = 1)
barplot(belfast_crime_cat, ylim = graph_range, main = "Crime in Belfast", xlab = "Crime Type", 
        ylab = "Sample Occurrences", col = "#69b3a2", las = 2, space = 1)
mtext("Sample crime occurrence count by type",side=3,line=1,cex=2,outer=TRUE)
