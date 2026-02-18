library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(car)
library(rstatix)

#Read in multiple files

files <- list.files(path="data_raw",
                    pattern="\\.csv",
                    full.names=TRUE)

surveys <- read_csv(files, id="source")

#get average hindfoot and weight with piping

surveys |>
  group_by(species_id)|>
  summarise(across(c(hindfoot_length, weight), mean, na.rm=TRUE)) |>
  na.omit() |>
  write.csv("data_cleaned/hindfoot_weight-mean.csv")
  

#create function to make plot of all species

plot_species <- function(data, x_var, y_var, species){
  #create plots
  plots <- data|>
    filter(species_id=={{ species }})|>
    ggplot(aes(x={{ x_var }},
               y={{ y_var }})) +
    geom_point(alpha=0.2) +
    labs(title=paste0("Species:", species)) +
    theme_minimal()
  # Construct filename and save
  file_path <- paste0("figures/", species, "_plot.png")
  ggsave(filename=file_path, plot=plots, width=6, height=4)
}

#iterate over each species to create plots

#for few
walk(c("DM", "NL"), ~ plot_species(
  data=surveys,
  x_var = weight,
  y_var = hindfoot_length,
  species = .x
))

#OVERALL DATA
walk(surveys$species_id, ~ plot_species(
  data=surveys,
  x_var = weight,
  y_var = hindfoot_length,
  species = .x
))




