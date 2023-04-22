# PLAN372 - HW6
# Sarah Followill

# First, we need to load libraries
library(tidyverse)
library(stringr)

# Next, we'll load our data
trees = read_csv("./RDS-2016-0005/Data/TS3_Raw_tree_data.csv")

# Q1

# Use a regular expression to split the City column into separate
# city and state columns

# Extract city name
trees$City_Name = str_extract(trees$City, "\\w+(\\s\\w+)?")

# Extract state abbreviation 
trees$State = str_extract(trees$City, "[:upper:]{2}")


# How many records in each state?
# Make a table

# Calculate count of records per state
states_df <- data.frame(trees$State)

# Create a frequency table to find count per state
counts_states_df <- table(states_df$trees.State)

# Use the names function to match the state names from the original dataset, 
# then assign the corresponding count to each state
states_df$count <- counts_states_df[match(states_df$trees.State,  names(counts_states_df))]

# Make table of count per state with only one result for each state
count_by_state = group_by(states_df, trees.State) %>%
  summarize(count_per_state = mean(count))

view(count_by_state)


# Potential Extra Credit? Maybe? I did it by city first on accident!
# Calculate count of records per city
cities_df <- data.frame(trees$City_Name)

# Create a frequency table to find count per city
counts_cities_df <- table(cities_df$trees.City_Name)

# Use the names function to match the city names from the original dataset, 
# then assign the corresponding count to each city
cities_df$count <- counts_cities_df[match(cities_df$trees.City_Name,  names(counts_cities_df))]

# Make table of count per city with only one result for each city
count_by_city = group_by(cities_df, trees.City_Name) %>%
  summarize(count_per_city = mean(count))

view(count_by_city)



# Q2 

# Limit features to NC and SC
# Filter your table to only include cities in NC and SC 
trees_ncsc = filter(trees, State == "NC" | State == "SC")

# Which cities in NC and SC were evaluated?
unique(trees_ncsc$City_Name)



# Q3

# Write a regex to extract the genus from trees in NC and SC
trees_ncsc$Genus = str_extract(trees_ncsc$ScientificName, "\\w+")

# Make new table of genera and their mean crown diameters
genus_crowns = group_by(trees_ncsc, Genus) %>%
  summarize(avg_crown_diameter_meters = mean(`AvgCdia (m)`))

# Arrange genera by crown diameter, largest to smallest
genus_crowns = arrange(genus_crowns, -avg_crown_diameter_meters)
view(genus_crowns)




# Extra Credit

# Are there differences in average age of the different genera of trees 
# in NC and SC?

# Make new table of genera and their mean ages
genus_age = group_by(trees_ncsc, Genus) %>%
  summarize(avg_age_years = mean(Age))

# Arrange genera by crown diameter, largest to smallest
genus_age = arrange(genus_age, -avg_age_years)
view(genus_age)

