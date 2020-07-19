## Tidy tuesday with austronaouts

library(tidyverse)
library(tidytuesdayR)
library(hrbrthemes)
library(patchwork)

tt <- tt_load("2020-07-14")

astronauts <- tt$astronauts %>% 
  mutate(sex = str_to_sentence(sex),
         military_civilian = str_to_sentence(military_civilian))


# Make histogram for all the space missions -------------------------------


astronauts %>% 
  ggplot(aes(x = year_of_mission, fill = paste(sex, military_civilian), alpha = paste(sex, military_civilian))) +
  geom_histogram(stat = "count", color = NA) +
  scale_fill_manual(values = c("#482576", "#482576", "#43BF71", "#43BF71"), labels = c("", "", " Civilians", " Military")) +
  scale_alpha_manual(values = c(0.5, 0.9, 0.5, 0.9), labels = c("", "", " Civilians", " Military")) +
  facet_wrap(~sex) +
  scale_y_continuous(expand = c(0,0)) +
  theme_ipsum_ps(grid = "Yy",
                 ticks = TRUE) +
  # Uncoment for small legends in the corners of the plot so they can be stand-still
  
  # guides(fill = guide_legend(title = NULL, nrow = 2),
  #        alpha = guide_legend(title = NULL, nrow = 2)) +
  theme(legend.position = "none",
        # legend.position = c(0.02, 0.9),
        # legend.justification = "left",
        # legend.spacing.x =  unit(0, "lines"),
        # legend.spacing.y =  unit(0, "lines"),
        # legend.background = element_rect(fill = "#FFFFFFFF", color = NA),
        # legend.key.width = unit(1, "lines"), 
        # legend.key.height = unit(0.3, "lines"), 
        axis.ticks.y = element_blank(),
        plot.caption.position = "plot") +
  labs(title = "Astronauts in all space missions",
       x = "Year",
       y = "Number of missions",
       caption = "* Same Astronauts flying multiple times.") -> plt_all_missions


# Make histogram for all indivudals ---------------------------------------


astronauts %>%
  group_by(name) %>% 
  filter(year_of_mission == min(year_of_mission)) %>% 
  ggplot(aes(x = year_of_mission, fill = paste(sex, military_civilian), alpha = paste(sex, military_civilian))) +
  geom_histogram(stat = "count", color = NA) +
  scale_fill_manual(values = c("#482576", "#482576", "#43BF71", "#43BF71"), labels = c("", "", " Civilians", " Military")) +
  scale_alpha_manual(values = c(0.5, 0.9, 0.5, 0.9), labels = c("", "", " Civilians", " Military")) +
  facet_wrap(~sex) +
  scale_y_continuous(expand = c(0,0)) +
  theme_ipsum_ps(grid = "Yy",
                 ticks = TRUE) +
  # Uncomment for small legend in corner of the plot
  # guides(fill = guide_legend(title = NULL, nrow = 2),
  #        alpha = guide_legend(title = NULL, nrow = 2)) +
  theme(legend.position = "none",
        # legend.position = c(0.02, 0.9),
        # legend.justification = "left",
        # legend.spacing.x =  unit(0, "lines"),
        # legend.spacing.y =  unit(0, "lines"),
        # legend.background = element_rect(fill = "#FFFFFFFF", color = NA),
        # legend.key.width = unit(1, "lines"), 
        # legend.key.height = unit(0.3, "lines"), 
        axis.ticks.y = element_blank(),
        plot.caption.position = "plot") +
  labs(title = "Astronauts on their first mission",
       x = "Year of first mission",
       y = "Number of first-time Astronauts") -> plt_first_mission


# Check the names ---------------------------------------------------------

# See if all the names are unique

astronauts %>% 
  select(name, sex, military_civilian) %>% 
  unique() %>% 
  count(name) %>% 
  arrange(desc(n)) %>% 
  filter(n == 2) %>% 
  left_join(astronauts)

# There are two persons with the same name, but they are different pesrsons, 
# and their military/civilan status is differnt, so selecting unique on that 
# basis will be ok. But just in case, we will add the year of birth.


# Make the breakdown by sex and military status ---------------------------


astronauts %>% 
  select(name, sex, military_civilian, year_of_birth) %>% 
  unique() %>% 
  group_by(sex, military_civilian) %>% 
  summarise(n = n()) %>% 
  mutate(n_sex = sum(n)) %>% 
  ungroup() %>% 
  mutate(sex_perc = n_sex / sum(n_sex) * 2) -> astronauts_tally

labels_df <- group_by(astronauts_tally, sex) %>%
  filter(n != 0) %>%
  arrange(desc(military_civilian)) %>%
  mutate(y = (cumsum(n) - 0.5*n)/n_sex)

astronauts_tally %>% 
  ggplot(aes(x = sex, y = n, fill = sex, width = sex_perc, alpha = military_civilian)) +
  geom_bar(stat = "identity", position = "fill", colour = "white", size = 1) +
  geom_text(data = labels_df,
            aes(y = y, label = n, color = military_civilian),
            family = "IBMPlexSans-Bold"
            ) +
  scale_fill_manual(values = c("#482576", "#43BF71"), guide = FALSE) +
  scale_alpha_manual(values = c(0.5, 0.9), guide = FALSE) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  facet_grid(~ sex, scales = "free_x", space = "free_x") +
  scale_y_continuous(name = NULL,
                     expand = c(0, 0),
                     breaks = NULL,
                     labels = NULL,
                     sec.axis = dup_axis(
                       breaks = filter(labels_df, sex == "Male")$y,
                       labels = filter(labels_df, sex == "Male")$military_civilian
                       )
                     ) +
  scale_x_discrete(expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_ipsum_ps(grid = "") +
  labs(x = "", y = "", title = "All Astronauts") +
  theme(
    axis.text.y.right = element_text(angle = 90, hjust = 0.5, size = 12),
    line = element_blank(),
    strip.text = element_blank(),
    # axis.ticks.length = unit(0, "pt"),
    panel.spacing.x = unit(0, "pt")
  ) -> plt_breakdown

patched_plot <- (plt_all_missions / plt_first_mission) | plt_breakdown


# See general data for subtitle -------------------------------------------

astronauts %>% 
  group_by(sex) %>% 
  filter(year_of_mission == min(year_of_mission)) %>% 
  select(name, year_of_mission)

astronauts %>%
  filter(year_of_mission == max(year_of_mission)) %>%
  select(year_of_mission, name) %>% 
  unique()
  
astronauts %>%
  select(name, year_of_birth) %>% 
  unique() %>% 
  nrow() -> total_persons

nrow(astronauts) -> total_missions


# Combine all three plots in one ------------------------------------------


combined_plot_astr <- patched_plot + plot_layout(widths = c(6, 1)) +
  plot_annotation(title = "Sex and military/civilian status of astronauts",
                  subtitle = str_glue("Historical overview of persons in space ",
                                      "(all {total_missions} missions by {total_persons} individuals), ",
                                      "from Yuri Gagarin in 1961 and Valentina Tereshkova in 1963 to 9 persons flying in 2019.",
                                      "\nTrivia: only two persons in space had the same name (Aleksandr Aleksandrov from USSR and Aleksandr Aleksandrov from Bulgaria)."),
                  caption = "@vasingtonskver on #TidyTuesday week 29 in 2020, data from Mariya Stavnichuk and Tatsuya Corlett.") & 
  theme(plot.title = element_text(family = "IBMPlexSans-Bold", size = 18, face = "plain", margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")),
        plot.subtitle = element_text(family = "IBMPlexSans-Light", size = 13, face = "plain"),
        plot.caption = element_text(family = "IBMPlexSans-Thin", size = 9, face = "plain", margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")))



# Check spelling ----------------------------------------------------------

gg_check(combined_plot_astr)

# Save the plot -----------------------------------------------------------

combined_plot_astr + ggsave("tt_2020-29_astronauts.png", device = "png", width = 15, height = 10, dpi = 300)



