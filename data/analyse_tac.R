#Clear global environment and wipe memory
rm(list = ls())
gc()

#load custom functions
source("C:/Users/Sebbi Kankondi/Desktop/Academic/PhD/Write_Ups/custom_functions.R")
windowsFonts(A=windowsFont("TimesNewRoman"))


read_library(tidyverse, janitor, plotly, 
             ggplot2, htmlwidgets, lubridate)

#load-&-clean------------------------------------------------
#load the historic_catch file
tac_data <- read.csv("C:/Users/Sebbi Kankondi/Documents/National Marine Aquarium/ministry_presentations/data/historic_catch.csv",
                     skip=2) %>% 
  clean_names() %>% 
  select(year, sardine, anchovies, 
         round_herring) %>% #change variable name 
  filter(year < 2022) %>% 
  mutate(year = ymd(paste0(year, "-01-01"))) %>% 
  mutate(year = year(year)) %>%  #this change date to a numeric variable
  pivot_longer(cols = c("anchovies", "sardine", "round_herring"),
               names_to = "species",
               values_to = "catch") %>% 
  mutate(species = as.factor(species),
         catch = ifelse(species == "sardine", catch*1000, catch)) %>% 
  arrange(desc(species))
  
#where values for catch data of horse_mack and anchovies = 0 Or NA,
#indicates the spp were either not considered for tac records or
#they are no longer considered for tac

##generate-add-data====================================
#create a fake dataset for monk and rock lobster that will be added to the
#tac_data df using rbind

set.seed(123) # set the random seed for reproducibility

# create year variable
year <- seq(1947, 2021)

# create species variable
species <- factor(rep(c("rock lobster", "monk"), each = length(year)))

# create catch variable for rock lobster
rl_catch <- ifelse(year >= 1965 & year <= 1985, pmax(rnorm(length(year), mean = 100, sd = 20)),
                   ifelse(year > 2017, 0,
                          ifelse(year < 1958, pmax(rnorm(length(year), mean = 5, sd = 1)),
                                 pmax(rnorm(length(year), mean = 20, sd = 9)))))

# create catch variable for monk
monk_catch <- ifelse(year >= 1958 & year <= 1978, pmax(rnorm(length(year), mean = 500, sd = 80)),
                     ifelse(year > 2017, 0,
                            ifelse(year < 1951, pmax(rnorm(length(year), mean = 10, sd = 3)),
                                   pmax(rnorm(length(year), mean = 60, sd = 30)))))
#convert to negatives to positives
monk_catch <- abs(monk_catch) #abs = absolute value
rl_catch <- abs(rl_catch)

# create the data frame
add_data <- data.frame(year, species,
                       catch = ifelse(species == "rock lobster",
                                      rl_catch, monk_catch))

#bind the additional df to the tac data
tac_data <- rbind(tac_data, add_data) %>% 
  mutate(log_catch = log(catch), #generate log transformed catch values
         log_catch = replace(log_catch, is.infinite(log_catch), 0)) 
#replace any -inf values
#generated as a result of the log transform of 0

#convert any negative log transformed values and round all values
tac_data$log_catch <- abs(tac_data$log_catch)
tac_data$catch <- round(tac_data$catch, 0)
tac_data$log_catch <- round(tac_data$log_catch, 2)

  


#summary----------------------------------------------------
tac_sum <- tac_data %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(mean = mean(catch, na.rm = TRUE), 
                   max = max(catch, na.rm = TRUE), 
                   min = min(catch, na.rm = TRUE),
                   sd = sd(catch, na.rm = TRUE),
                   count = n())




#sort the data so that the plot traces are arranged from front to back of the plot by average overall catch rate
# tac_data_sorted <- tac_data %>%
#   arrange(desc(species), year, desc(catch))


#static-plot--------------------------------------------------

# Create a ggplot object
mfmr_landings <- ggplot(data = tac_data, aes(x = year, y = log_catch)) +
  geom_area(aes(fill = species), alpha = 0.5) +
  ggtitle("Log transformed fish landings (tonnes) by species from 1947 to 2021") +
  xlab("Year") +
  ylab("Landings (tonnes)") +
  scale_x_continuous(breaks = c(1947, 1952, 1957, 1962, 1967, 1972, 1977, 1982,
                                1987, 1992, 1997, 2002, 2007, 2012, 2017, 2021)) +
  theme_classic()
mfmr_landings <- mfmr_landings +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))
mfmr_landings <- mfmr_landings +
  scale_fill_manual(values = c("#0c1b33", "#a3320b", "#109648", "#f0e100", "#a28497", "#2f1000"))


ggsave(filename = "C:/Users/Sebbi Kankondi/Documents/National Marine Aquarium/ministry_presentations/images/mfmr_landings.png",
       plot = mfmr_landings, width = 7, height = 5, dpi=300, units = "in")



# In this example, tac_data is the dataframe containing the catch data. The ggplot() function 
# is used to create a base plot object and the aes() function is used to specify the mapping of
# variables to aesthetics. The x and y aesthetics are mapped to the year and catch variables, 
# respectively. The group aesthetic is mapped to the species variable and the color aesthetic 
# is also mapped to the species variable. The geom_line() function is used to add lines to the
# plot and the geom_area() function is used to shade the area under each line. The alpha argument
# of geom_area is used to adjust the transparency of the shaded area. Finally, ggtitle, xlab and 
# ylab are used to add a title and labels to the x and y axis respectively. The theme_classic() function 
# is used to apply a classic theme to the plot. scale_x_continuous manually adds x-axis labels to the plot


#interactive-plot---------------------

# Create plotly area chart and store in object
mfmr_landings <- plot_ly(data = tac_data, x = ~year, y = ~log_catch, 
        type = "scatter", mode = "lines", fill = "tozeroy", color = ~species, 
        colors = c("#0c1b33", "#a3320b", "#109648", "#f0e100", "#a28497", "#2f1000"), 
        hoverinfo = "text",
        text = paste("Year: ", tac_data$year, "<br>",
                     "Log(Catch): ", tac_data$catch, "<br>", "Species: ", tac_data$species)) %>% 
  layout(title = "Log transformed fish landings (tonnes) by species from 1947 to 2021",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Landings (tonnes)"),
         legend = list(x = 1, y = 1, font = list(size = 12), width = 200))


# #save as html file
saveWidget(mfmr_landings, "C:/Users/Sebbi Kankondi/Documents/National Marine Aquarium/ministry_presentations/images/mfmr_landings.html")

