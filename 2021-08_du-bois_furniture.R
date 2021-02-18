library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(geofacet)
library(patchwork)
library(extrafont)
library(ggrepel)

# Load data ---------------------------------------------------------------

tt <- tt_load(2021, week = 8)

tt$furniture %>% 
  janitor::clean_names() -> furniture_data


# As Archimedeus spiral will be used, figure out what is the length/shape
# of the  longest one (1899) and determine the length of the others 
# could be overly complicated, and there might be easier ways to do the 
# calculations

# we create a spiral, and then calculate its length

# there was a lot of playing with different numbers to decide on precise values
# which are recreating the chart in a good way.

make_spiral <- function(x, color, endpoint = 0) {
  
  theta <- seq(endpoint, 6.5*pi, 0.0001)
  r <- 4+x+2*theta
  spiral_points <- data.frame(x    = r*cos(theta),
                              y    = r*sin(theta),
                              boja = color) 
  
  spiral_points %>% 
    as_tibble() %>% 
    mutate(x_ld = lead(x),
           y_ld = lead(y),
           dist = sqrt((x-x_ld)*(x-x_ld) + (y-y_ld)*(y-y_ld)),
           dist = if_else(is.na(dist), 0, dist)) %>% 
    rownames_to_column() %>% 
    mutate(rowname = as.numeric(rowname)) %>%  # to calculate distance from the top "start"
    arrange(desc(rowname)) %>%                 # we have to reverse the order of the spiral
    mutate(total_dist = cumsum(dist))          # as it originally goes from the center to the outside
}

# this function makes the spiral, and truncates it based on the length

make_proper_length_spiral <- function(position, name, length, razmak) {
  make_spiral(razmak * position, name) %>% 
    filter(total_dist < length)
}

# this function makes the same spiral, which is just slightly longer, because we are
# using it as a cheat to create illusion of hte border

make_proper_length_spiral_cheat <- function(position, name, length, razmak) {
  make_spiral(razmak * position, name) %>% 
    filter(total_dist < length) -> basic
  
  make_spiral(razmak * position, name) %>% 
    filter(total_dist > length) %>% 
    filter(total_dist < min(total_dist) + 10*0.00469) -> extra_line
  
  basic %>% 
    bind_rows(extra_line)
  
}


# Calculate all the values for the spiral ---------------------------------


# with all the playing, with different values, to the eye it seems most approximate
# htat the "end" of the long spiral is at value 3*pi-0.8

# in addition "razmak_test" is addiing spacing between the different spirals, so
# the width of them can be better

# to these numbers I came randomly by trial and error and trying different maual editing
# when I was making data. There might be a thing where it can be optimised

end_long_spiral <- 3*pi-0.8
razmak_test <- 2.1

# abs(6-7) - this is for positioning of the start of each spiral, 
# it's weird, but combination of this and its use in for loop allows to
# have all the spirals ordered properly and sized in a way that if each year was the same length (i.e. max)
# there would be  no space between the spirals when they countinue spiraling in

baseline <- make_spiral(abs(6-7)*razmak_test, "black", end_long_spiral)

# to see how it looks like, and that we aproximately guessed the lengt
baseline %>%
  ggplot(aes(x,y)) + geom_path(size = 3.7) +
  coord_fixed()

# Now we calculate relative distance of each spiral in comparison to the 
# longest one, we use as baseline, and relative to it, we will use all other lengths

furniture_data %>% 
  mutate(relative_dist = 
           houshold_value_dollars/max(houshold_value_dollars)*max(baseline$total_dist)) -> furniture_data

# in this step, we take each year and create the spiral for it, with its appropriate length
# and coordinate system coresponding to its proper position - i.e. abs(i-7)

all_data <- NULL
for (i in c(1:6)) {
  all_data <- all_data %>% 
    bind_rows(make_proper_length_spiral(abs(i-7),
                                        as.character(furniture_data$year[i]),
                                        furniture_data$relative_dist[i],
                                        razmak_test))
}

# now we make a cheat line to get the borders

cheating_line <- NULL
for (i in c(1:6)) {
  cheating_line <- cheating_line %>% 
    bind_rows(make_proper_length_spiral_cheat(abs(i-7),
                                        as.character(furniture_data$year[i]),
                                        furniture_data$relative_dist[i],
                                        razmak_test))
}


# Write annotations, colors, etc. -----------------------------------------


# following step is to create annotations for the years

all_data %>% 
  filter(rowname == max(rowname)) %>% # select the "start points" of each spiral on top
  select(x, y, boja) %>% 
  mutate(year = as.numeric(as.character(boja))) %>% 
  left_join(furniture_data) -> text_anot

# it's just easier to type out the numbers, than to deal with messed up transformations
# and conditions when to change the sign, padding, etc.

format_dollars <- c('$ 21,186',
                    '$  498,532',
                    '"  736,170',
                    '"1,173,624',
                    '"1,322,694',
                    '"1,434,975')
                    
# add the formated nottation, write the title and caption
text_anot$format_dollars <- format_dollars
title_text <- "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES."
caption_text <- str_glue("W.E.B. Du Bois chart from The Exhibit of American Negroes at the ",
                         "1900 World's Fair in Paris.\n",
                         "@vasingtonskver recreation for #TidyTuesday week 8 in 2021, ",
                         "data from Anthony Starks, Allen Hillery, and Sekou Tyler.")
# and colors
clr_plt <- c("#ffc0ca",
             "#afc4de",
             "#d2b48b",
             "#ffd600",
             "#d2d2d2",
             "#dc133b")


# Make the plot -----------------------------------------------------------



all_data %>% 
  ggplot(aes(x,y, color = boja, group = boja)) +
  # first goes the cheat line plotted so we get a slightly greater width and black "border"
  geom_path(data = cheating_line, size = 3.7, color = "black") + 
  # main spyral, narrower, which uses colors
  geom_path(size = 3.5) +
  # the "0 line" on top from which graph starts
  geom_segment(aes(x = 0, xend = 0, y = 45.9, yend = 58.4),
               color = "black", size = 0.05) +
  # using ggrepel to get the different length dash wich goes from the dollar sign, by
  # pushing the text label left getting the segment line and
  # and in the end ploting label above it with dollar values
  geom_text_repel(data = text_anot, 
                           aes(x = -4,
                               y = y,
                               label = year),
                           hjust = "right",
                           color = "black",
                           xlim  = c(-Inf, -20),
                           direction = "x",
                           segment.size = 0.1,
                           family = "IBM Plex Mono", 
                           size = 2.5) + 
  geom_label(data = text_anot,
             aes(x = -4,
                 y = y,
                 label = format_dollars),
             hjust = "right",
             color = "black",
             label.padding = unit(0, "lines"),
             label.size = 0,
             family = "IBM Plex Mono", #IBM Plex Sans
             size = 2.5) +
  scale_color_manual(guide = FALSE, values = clr_plt) +
  coord_fixed() + # very important!
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "IBM Plex Sans Medium",
                                  margin = margin(b = 1.2, t = 0, unit = "cm")),
        plot.caption = element_text(family = "IBM Plex Sans Light",
                                    size = 6,
                                    margin = margin(t = 1.3, b= 0, unit = "cm")),
        plot.margin = margin(t = 0.4, b = 0.2, r = 0.8, l = 0.8, "cm")) +
  labs(title = title_text,
       caption = caption_text) -> plt



# Save the plot -----------------------------------------------------------


ggsave("tt_2021-08_du-bois_furniture.png", plot = plt, device = "png", width = 6, height = 7.5)

