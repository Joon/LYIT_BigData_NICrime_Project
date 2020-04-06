# Section 1
#   Complete the following tasks using the NIPostcodes dataset described above.

post_code_data <- read.csv("../NI_Crime_Data/NIPostcodes.csv", header = FALSE)

#a) Show the total number of rows, the structure of the data frame, and first 10
#   rows of the data frame containing all of the NIPostcode data.
nrow(post_code_data)
str(post_code_data)
head(post_code_data, 10)

#b) Add a suitable title for each attribute of the data.
column_headers <- c("Organisation_Name", "Sub_building_Name", "Building_Name", "Number", "Primary_Thorfare", 
                   "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", "County", 
                   "Postcode", "x_coordinates", "y_coordinates", "Primary_Key")
colnames(post_code_data) <- column_headers

#c) Remove or replace missing entries with a suitable identifier. Decide whether it is
#   best to remove missing data or to recode it. Discuss missing data and justify
#   your decision in detail.

for(col_name in column_headers) {
  blank_count <- sum(get(col_name, post_code_data) == "")
  print(sprintf("%s  Blank: %s", col_name, blank_count))
}

post_code_data$Organisation_Name[post_code_data$Organisation_Name == ""] <- NA
post_code_data$Sub_building_Name[post_code_data$Sub_building_Name == ""] <- NA
post_code_data$Building_Name[post_code_data$Building_Name == ""] <- NA
post_code_data$Number[post_code_data$Number == ""] <- NA
post_code_data$Primary_Thorfare[post_code_data$Primary_Thorfare == ""] <- NA
post_code_data$Alt_Thorfare[post_code_data$Alt_Thorfare == ""] <- NA
post_code_data$Secondary_Thorfare[post_code_data$Secondary_Thorfare == ""] <- NA
post_code_data$Locality[post_code_data$Locality == ""] <- NA
post_code_data$Town[post_code_data$Town == ""] <- NA
post_code_data$Postcode[post_code_data$Postcode == ""] <- NA
post_code_data$x_coordinates[post_code_data$x_coordinates == ""] <- NA
post_code_data$y_coordinates[post_code_data$y_coordinates == ""] <- NA
post_code_data$Primary_Key[post_code_data$Primary_Key == ""] <- NA

#d) Show the total number of missing values for each column in the postcode data
#   frame.

for(col_name in column_headers) {
  na_count <- sum(is.na(get(col_name, post_code_data)))
  blank_count <- sum(get(col_name, post_code_data) == "")
  print(sprintf("%s  NA: %s", col_name, na_count))
}

#e) Move the primary key identifier to the start of the dataset.
new_column_headers <- c(column_headers[15], column_headers[1:14])
post_code_data <- post_code_data[,new_column_headers]

#f) Create a new dataset called Limavady_data. Store within it only information
#   where locality, townland and town contain the name Limavady. Count and
#   display the number of rows. Store this information in a csv file called Limavady.

all_limavady_rows <- grepl("LIMAVADY", post_code_data$Locality) & grepl("LIMAVADY", post_code_data$Townland) & 
  grepl("LIMAVADY", post_code_data$Town)
Limavady_data <- post_code_data[all_limavady_rows,]

nrow(Limavady_data)
write.csv(Limavady_data, "../NI_Crime_Data/Limavady.csv")

#g) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData
write.csv(post_code_data, "../NI_Crime_Data/CleanNIPostcodeData.csv")
