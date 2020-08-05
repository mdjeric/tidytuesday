library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(geofacet)
library(patchwork)


# Load data ---------------------------------------------------------------

tt <- tt_load(2020, week = 32)


# Prepare for geofacets ---------------------------------------------------

## This part is fully taken from Cedric Scherer
## https://github.com/Z3tt/TidyTuesday/blob/master/R/2020_32_EuropeanEnergy.Rmd

my_grid <- 
  europe_countries_grid1 %>% 
  filter(!code %in% c("IS", "BY", "RU", "MD", "CH")) %>% 
  add_row(row = 6, col = 10, code = "GE", name = "Georgia") %>% 
  mutate(row = if_else(code == "IE", 2, row),
         name = if_else(code == "MK", "North Macedonia", name),
         name = if_else(code == "BH", "Bosnia and\nHertzegovina", name)) 


# Transform the data ------------------------------------------------------

enrgy_2018 <- tt$energy_types %>% 
  filter(level == "Level 1") %>% 
  select(-level, -`2016`, -`2017`) %>% 
  rename(production = `2018`) %>% 
  mutate(type = if_else(type %in% c("Conventional thermal", "Nuclear"),
                        type, "Renewable")) %>% 
  group_by(country, country_name, type) %>% 
  summarise(production = sum(production)) %>% 
  group_by(country_name) %>% 
  mutate(prop = production/sum(production)) %>% 
  ungroup() -> enrgy_2018

# Fix names and prepare for geofacet (taken from Cedric Scherer) and make percentages

enrgy_2018 %>% 
  mutate(
    country_name = if_else(country == "EL", "Greece", country_name),
    country = if_else(country == "EL", "GR", country)
  ) %>% 
  mutate(country = if_else(country == "UK", "GB", country)) %>% 
  full_join(my_grid, by = c("country" = "code")) %>% 
  mutate(country_name = if_else(country == "GB", "United Kingdom", country_name)) %>% 
  mutate(percent = scales::percent(prop, accuracy = 1)) -> enrgy_2018
  

# Idea for a function taken from Cedric Scherer,
# If we are doing the same work three times, of course it should be a function

percent_facet <- function(df, energy, color) {
  
  df <- filter(df, type == energy)
  
  ggplot(data = df, aes(x = 1, y = prop)) +
    geom_bar(aes(y = 1), stat = "identity", fill = "grey") +
    geom_bar(stat = "identity", fill = color) +
    geom_text(data = filter(df, prop >= 0.5),
              aes(x = 0.56, label = percent, y = 0),
              vjust = "bottom", hjust = "left",
              nudge_y = 0.03,
              family = "IBM Plex Sans Bold",
              color = "white", size = 11, alpha = 0.6) +
    geom_text(data = filter(df, prop < 0.5),
              aes(x = 0.56, label = percent, y = prop),
              vjust = "bottom", hjust = "left",
              nudge_y = 0.03,
              family = "IBM Plex Sans Bold",
              color = "white", size = 11, alpha = 0.6) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_geo( ~ name, grid = my_grid, scales = "free_x") +
    theme_void() +
    theme(strip.text = element_text(hjust = 0,
                                    family = "IBM Plex Sans Light",
                                    size = 8),
          plot.title = element_text(family = "IBMPlexSans-Bold",
                                    size = 18, face = "plain",
                                    margin = margin(t = 0, r = 10, b = 10, l = 10, unit = "pt")),
          plot.subtitle = element_text(family = "IBMPlexSans-Light", 
                                       size = 13, face = "plain"),
          plot.caption = element_text(family = "IBMPlexSans-Thin", 
                                      size = 9, face = "plain", 
                                      margin = margin(t = 15, r = 10, b = 0, l = 10, unit = "pt")),
          plot.margin = margin(10, 10, 10, 10)) +
    labs(title = str_glue("{energy} energy production in Europe"),
         subtitle = "Percent of total generated electricity during 2018 by select European countries according to Eurostat",
         caption = "@vasingtonskver on #TidyTuesday week 32 in 2020, data from Eurostat.")
  
}

# Make individual plots ---------------------------------------------------

plt_renewable <- percent_facet(enrgy_2018, "Renewable", "#228b22")
plt_nuclear <- percent_facet(enrgy_2018, "Nuclear", "#871a1a")
plt_thermal <- percent_facet(enrgy_2018, "Conventional thermal", "black")

# Save the plot -----------------------------------------------------------

ggsave("tt_2020-32_european_renew_energy.png", plot = plt_renewable, device = "png", width = 13, height = 10)
ggsave("tt_2020-32_european_nuclear_energy.png", plot = plt_nuclear, device = "png", width = 13, height = 10)
ggsave("tt_2020-32_european_thermalw_energy.png", plot = plt_thermal, device = "png", width = 13, height = 10)



