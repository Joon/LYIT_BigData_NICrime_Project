# Section 1
#   Complete the following tasks using the NIPostcodes dataset described above.

post_code_data <- read.csv("../NI_Crime_Data/NIPostcodes.csv", header = FALSE)

# a) Show the total number of rows, the structure of the data frame, and first 10
#    rows of the data frame containing all of the NIPostcode data.
nrow(post_code_data)
str(post_code_data)
head(post_code_data, 10)

# b) Add a suitable title for each attribute of the data.
column_headers <- c("Organisation_Name", "Sub_building_Name", "Building_Name", "Number", "Primary_Thorfare", 
                   "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", "County", 
                   "Postcode", "x_coordinates", "y_coordinates", "Primary_Key")
colnames(post_code_data) <- column_headers

# c) Replace and recode all missing entries with a suitable identifier. Decide
#    whether it is best to remove none, some or all of the missing data. Discuss the
#    missing data using suitable graphical output to explain your answer and justify
#    your decision in detail.

blank_columns = integer()
blank_columns['TotalRows'] <- nrow(post_code_data)
for (c in column_headers) { blank_columns[c] <- sum(get(c, post_code_data) == "") }

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


# d) Show the total number of missing values for each column in the postcode data
#    frame both before and after your decision to deal with the missing data
#    variables.
na_columns = integer()
na_columns['TotalRows'] <- nrow(post_code_data)
for (c in column_headers) { na_columns[c] <- sum(is.na(get(c, post_code_data))) }

print(cbind(blank_columns, na_columns))


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
