# Section 1
#   Complete the following tasks using the NIPostcodes dataset described above.

# a) Show the total number of rows, the structure of the data frame, and first 10
#    rows of the data frame containing all of the NIPostcode data.
# Read the data without a header row from CSV into a dataframe
post_code_data <- read.csv("../NI_Crime_Data/NIPostcodes.csv", header = FALSE)

# Count the rows
nrow(post_code_data)
# Show the dataframe structure
str(post_code_data)
# Show the first 10 rows and column headers
head(post_code_data, 10)

# b) Add a suitable title for each attribute of the data.
# Build up the list of column headers as a string vector
column_headers <- c("Organisation_Name", "Sub_building_Name", "Building_Name", "Number", "Primary_Thorfare", 
                   "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", "County", 
                   "Postcode", "x_coordinates", "y_coordinates", "Primary_Key")
# Assign the column names
colnames(post_code_data) <- column_headers
# Show the first 10 rows and column headers
head(post_code_data, 10)

# c) Replace and recode all missing entries with a suitable identifier. Decide
#    whether it is best to remove none, some or all of the missing data. Discuss the
#    missing data using suitable graphical output to explain your answer and justify
#    your decision in detail.

# Store the total row count and count of columns containing an empty string for later comparison
blank_columns = integer()
blank_columns['TotalRows'] <- nrow(post_code_data)
for (c in column_headers) { blank_columns[c] <- sum(get(c, post_code_data) == "") }

print(blank_columns)

# Replace all empty string values in the dataset with the NA identifier
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

# Remove rows without Town
post_code_data <- post_code_data[!is.na(post_code_data$Town),]

nrow(post_code_data)
# Show the first 10 rows and column headers
head(post_code_data, 10)


# d) Show the total number of missing values for each column in the postcode data
#    frame both before and after your decision to deal with the missing data
#    variables.
# Count all NA values per column
na_columns = integer()
na_columns['TotalRows'] <- nrow(post_code_data)
for (c in column_headers) { na_columns[c] <- sum(is.na(get(c, post_code_data))) }

# Print the previously calculated blank counts and current NA counts
print(cbind(blank_columns, na_columns))


#e) Move the primary key identifier to the start of the dataset.

# Print the structure before moving PK
str(post_code_data)

# Re-order the column headers to put PK at the front
new_column_headers <- c(column_headers[15], column_headers[1:14])
# Replace the post_code_data dataframe with the values in it selected in a new order
post_code_data <- post_code_data[,new_column_headers]

# Print the structure after moving PK
str(post_code_data)


#f) Create a new dataset called Limavady_data. Store within it only information
#   where locality, townland and town contain the name Limavady. Count and
#   display the number of rows. Store this information in a csv file called Limavady.

# Construct a logical vector of the length of the post_code_data frame, with each
# logical value set to TRUE where the locality, townland and town values all 
# contain LIMAVADY
all_limavady_rows <- grepl("LIMAVADY", post_code_data$Locality) & grepl("LIMAVADY", post_code_data$Townland) & 
  grepl("LIMAVADY", post_code_data$Town)
# Select the post_code_data rows where the limavady condition above 
# is met into a new frame
Limavady_data <- post_code_data[all_limavady_rows,]

# Count the rows
nrow(Limavady_data)

# Dump the limavady data structure
str(Limavady_data)

# Save the rows to a local CSV
write.csv(Limavady_data, "../NI_Crime_Data/Limavady.csv")

#g) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData
# Save the post_code_data frame as csv in the staging folder
write.csv(post_code_data, "../NI_Crime_Data/CleanNIPostcodeData.csv")
