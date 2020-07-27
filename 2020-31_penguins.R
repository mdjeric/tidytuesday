## Tidy tuesday with penguins

library(tidyverse)
library(tidytuesdayR)
library(hrbrthemes)
library(ggimage)
library(extrafont)
library(ggdark)

tt <- tt_load(2020, week = 31)

penguins_plot <- tt$penguins %>% 
  filter(!is.na(sex)) %>% 
  # head() %>% # making plot with emojis can take a while, speed it up to see how it looks like
  mutate(island = str_glue("Penguin was on {island} Island"),
         species = str_glue("{species} species of penguins")) %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  # geom_point() + # making plot with emojis can take a while, speed it up to see how it looks like
  geom_emoji(aes(image = "1f427"), size = 0.05) + 
  facet_grid(species ~ island) +
  dark_theme_linedraw() +
  theme(legend.position = "top",
        text = element_text(family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans Light"),
        axis.title = element_text(hjust = 1, family = "IBM Plex Sans Light"),
        strip.text = element_text(family = "IBM Plex Sans Medium"),
        plot.caption.position = "plot",
        plot.subtitle = element_text(color = "orange"),
        plot.caption = element_text(color = "orange"),
        plot.title = element_text(family = "IBM Plex Sans SemiBold"),
        panel.grid.minor = element_blank()) +
  labs(title = "Penguins and their bills",
       subtitle = "Sizes of penguins' bill are represented with little penguins, separated by island and species, on a plot colored like penguins.",
       x = "Length of penguin's bill (mm)",
       y = "Depth of penguin's bill (mm)",
       caption = "@vasingtonskver on #TidyTuesday week 31 in 2020, data from palmerpenguins (Dr. Kristen Gorman, Dr. Allison Horst, and Dr. Alison Hill).")

# Check spelling ----------------------------------------------------------

gg_check(penguins_plot)

# Save the plot -----------------------------------------------------------

penguins_plot + ggsave("tt_2020-30_penguins.png", device = "png", width = 10, height = 8, dpi = 300)
