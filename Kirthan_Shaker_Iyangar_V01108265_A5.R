# Set the working directory and verify it
setwd('/Users/kirthanshaker/Desktop/SCMA 631 Data Files ')
getwd()
install.packages("sf")

#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("/Users/kirthanshaker/Desktop/SCMA 631 Data Files /NSSO68.csv")

# Filtering for KE
df <- data %>%
  filter(state_1 == "KE")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
KEnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
KEnew$Meals_At_Home <- impute_with_mean(KEnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  KEnew <- remove_outliers(KEnew, col)
}

# Summarize consumption
KEnew$total_consumption <- rowSums(KEnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- KEnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Kasaragod","2" = "Kannur","3" = "Wayand","4" = "Kozhikode","5" = "Malappuram","6" = "Palakkad","7" = "Thrissur","8" = "Eranakulam","9" = "Idukki", "10" = "Kottayam", "11" = "Alappuzha","12" = "Pathanamthitta","13" = "Kollam","14" = "Thiruvanathapuram")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

KEnew$District <- as.character(KEnew$District)
KEnew$Sector <- as.character(KEnew$Sector)
KEnew$District <- ifelse(KEnew$District %in% names(district_mapping), district_mapping[KEnew$District], KEnew$District)
KEnew$Sector <- ifelse(KEnew$Sector %in% names(sector_mapping), sector_mapping[KEnew$Sector], KEnew$Sector)

View(KEnew)

hist(KEnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Mizoram State")

KE_consumption <- aggregate(total_consumption ~ District, data = KEnew, sum) 
View(KE_consumption)
??barplot
barplot(KE_consumption$total_consumption, 
        names.arg = KE_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

# b) Plot {'any variable of your choice'} on the Karnataka state map using NSSO68.csv data
install.packages("sf")
install.packages("sf", type = "https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/sf_1.0-16.tgz")

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("/Users/kirthanshaker/Desktop/SCMA 631 Data Files /KERALA_DISTRICTS (1).geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(KE_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")



