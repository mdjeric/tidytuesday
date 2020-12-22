library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(geofacet)
library(patchwork)
library(hrbrthemes)


# Load data ---------------------------------------------------------------

tt <- tt_load(2020, week = 52)


big_mac <- tt$`big-mac` %>% mutate(year = lubridate::year(date))


# Select countries and years ----------------------------------------------

big_mac %>% 
  filter(date == min(date)) %>% 
  pull(iso_a3) -> countries

years <- c(2000, 2005, 2015, 2010, 2020)


# Adjust to current dollars, and calculate changes ------------------------

# not 100% sure that it's needed to adjust

big_mac %>% 
  filter(year %in% years, iso_a3 %in% countries) %>% 
  group_by(iso_a3, name, year) %>% 
  summarise(dollar_price = mean(dollar_price)) %>% 
  filter(year %in% c(2000, 2020)) %>% 
  mutate(year = as.character(year),
         dollar_price = if_else(year == "2000",
                                dollar_price * 1.51, dollar_price)) %>% 
  arrange(name, year) %>% 
  group_by(name) %>% 
  mutate(change_num = (dollar_price - lead(dollar_price))/dollar_price,
         change_num = if_else(is.na(change_num), 
                              lag(change_num), change_num)) %>% 
  ungroup() %>% 
  mutate(increase = change_num > 0,
         rel_chane = case_when(change_num > 0.1 ~ "More than 10% decrease",
                               change_num < -0.1 ~ "More than 10% increase",
                               TRUE ~ "Changes within 10%"),
         rel_chane = fct_relevel(rel_chane, 
                                 "More than 10% increase",
                                 "Changes within 10%")) -> bm_plot


# Make central "axis" -----------------------------------------------------

bm_plot %>% 
  select(rel_chane) %>% 
  filter(rel_chane == "Changes within 10%") %>% 
  head(8) %>% 
  rowid_to_column(var = "dollar_price") %>% 
  mutate(dollar_price = dollar_price - 1) -> middle_line


# Select countries for highlighting ---------------------------------------

bm_plot %>% 
  select(iso_a3, change_num) %>% 
  unique() -> bm_changes

slice_min(bm_changes, change_num, n = 3) %>% 
  mutate(status = "Three min") %>% 
  bind_rows(slice_min(bm_changes, abs(change_num), n = 3) %>% 
              mutate(status = "Three least")) %>% 
  bind_rows(slice_max(bm_changes, change_num, n = 3) %>% 
              mutate(status = "Three max")) -> bm_changes

bm_plot %>% 
  left_join(bm_changes) %>% 
  mutate(status = if_else(is.na(status), "Regular", status)) -> bm_plot


# Make the plot -----------------------------------------------------------


bm_plot %>% 
  ggplot(aes(x = year, y = dollar_price)) +
  geom_line(data = middle_line, 
            aes(x = 1.5, y = dollar_price),
            size = 3, 
            color = "grey") +
  geom_point(data = middle_line, 
             aes(x = 1.5, y = dollar_price),
             shape = 21, 
             colour = "white", 
             fill = "grey", 
             size = 3, 
             stroke = 3) +
  geom_label(data = middle_line, 
             aes(x = 1.5, y = dollar_price, label = str_glue("$ {dollar_price}")),
             nudge_x = 0.1,
             label.size = NA, 
             color = "grey",
             family = "IBMPlexSans-Light"
             ) +
  ggrepel::geom_text_repel(data = filter(bm_plot, year == "2020"),
                           aes(label = name, color = status),
                           xlim = c(2, NA),
                           direction = "y",
                           force        = 0.5,
                           nudge_x      = 0.05,
                           hjust        = 0,
                           family = "IBMPlexSans",
                           segment.size = 0.1) +
  ggrepel::geom_text_repel(data = filter(bm_plot, year == "2000"),
                           aes(label = name, color = status),
                           xlim = c(NA, 1),
                           family = "IBMPlexSans",
                           direction = "y",
                           force        = 0.5,
                           nudge_x      = -0.05,
                           hjust        = 1,
                           segment.size = 0.1) +
  geom_line(aes(group = name, color = status)) +
  scale_color_manual(values = c("#000000bf", "#762a83", "#1b7837", "#1b7837"), 
                     guide = FALSE) +
  ggforce::facet_row(~ rel_chane) +
  scale_x_discrete(expand = c(0.3, 0.3)) +
  scale_y_continuous(breaks = c(0:7)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  theme(text = element_text(family = "IBMPlexSans"),
        plot.title = element_text(size = 22, 
                                  family = "IBMPlexSans-Bold",
                                  margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
        plot.subtitle = element_text(size = 12,
                                     margin = margin(t = 0, r = 0, b = 30, l = 0, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        plot.caption = element_text(),
        strip.text = element_text(size = 16)) +
  labs(title = "Just plain old burgers",
       subtitle = "Changes of Big Mac prices from 2000 to 2020, in today's dollars. Highlighted countries with most extreme and least extreme (relative) changes in prices.",
       caption = "@vasingtonskver on #TidyTuesday week 52 in 2020, data from The Economist") -> plt

# Check spelling ----------------------------------------------------------

gg_check(plt)

# Save the plot -----------------------------------------------------------

ggsave("tt_2020-52_big-macs.png", plot = plt, device = "png", width = 14, height = 8)
