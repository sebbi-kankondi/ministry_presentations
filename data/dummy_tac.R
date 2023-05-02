#load multiple libraries function
source("C:/Users/Sebbitwo/Desktop/Academic/PhD/Write_Ups/multiple_libraries.R")
windowsFonts(A=windowsFont("TimesNewRoman"))

# create an empty dataframe to store the catch data
tac_data <- data.frame()

# create a vector of years from 1947 to 2022
years <- as.numeric(1947:2022)

# create a vector of fish species
species <- c("anchovy", "hake", "horse mackerel", "monk", "sardine", "rock lobster")

# set seed for reproducibility
set.seed(92187)

# use a for loop to iterate through each species
for(i in 1:length(species)) {
  
  # create a variable to store the catch data for the current species
  catch <- rep(NA, length(years))
  
  # use a for loop to iterate through each year
  for(j in 1:length(years)) {
    
    # generate a random catch value for the current species and year, with a left skewed distribution
    catch[j] <- rbeta(1, shape1 = 2, shape2 = j/10) * 1000
  }
  
  # add the catch data for the current species to the tac_data dataframe
  tac_data <- rbind(tac_data, data.frame(species = species[i], year = years, catch = catch))
}

# Add a group column to tac_data
# tac_data$group <- tac_data$species


# write data frame to csv file in specified folder
write.csv(tac_data, "C:/Users/Sebbitwo/Documents/National Marine Aquarium/Presentations/data/tac_data.csv", row.names = FALSE)

