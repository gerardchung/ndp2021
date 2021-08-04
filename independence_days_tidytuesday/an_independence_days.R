# LOAD DATA #####

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
#install.packages("tidytuesdayR")

rm(list = ls())
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(ggplot2)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2021-07-06')
#tuesdata <- tidytuesdayR::tt_load(2021, week = 28)

holidays <- tuesdata$holidays
rm(tuesdata)
# Or read in the data manually

#holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07#-06/holidays.csv')


# CLEAN DATA ####
## Credits for parts of this section of code: https://github.com/ZainulArifin1/WeeklyPlot/blob/main/Week_12/commonwealth.R


month <- 
    holidays %>%
    filter(!is.na(month)) %>% 
    group_by(month) %>%
    count() %>%
    ungroup()

year <- 
    holidays %>%
    filter(!is.na(year)) %>% 
    group_by(year) %>%
    count() %>%
    ungroup()

sum(month$n)

month$month <- factor(month$month, 
                     labels =  c("Jan","Feb","Mar", "Apr", "May", "Jun", 
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                      levels = c("Jan","Feb","Mar", "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

month <- 
    month %>% 
    mutate(highlight_august = ifelse(month == 'Aug', T, F))


tabyl(month$month)

p <- ggplot(data = month, mapping = aes(x = n, y = month))
p + geom_col(aes(fill = highlight_august)) +
    geom_text(
        aes(label = paste(n,"COUNTRIES")),
        hjust = 1, 
        nudge_x = -0.2,
        fontface = "bold",
        family = "Roboto Condensed",
        size = 7
    ) +
    theme_minimal(base_family = "Roboto Condensed") +
    scale_fill_manual(values = c('#595959', '#ee2536')) +
    labs(title = "MONTHS OF COUNTRIES' NATIONAL DAYS",
        caption = "gerardchung.com | Codes: https://github.com/gerardchung/ndp2021"  
    ) +
    theme(    rect = element_rect(fill = "#ffffff"),
              panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
              plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          axis.text.y = element_text(size = 30, hjust = 1, family = "Roboto Condensed", color = "black"),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = ggtext::element_markdown(size = 32, face="bold"),
          plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
          legend.position = "none"
          ) -> plot_final

ggsave("plots/independencedays.png", plot = plot_final, type = 'cairo', width = 9, height = 6.5, dpi = 300, units = "in", bg = "#ffffff")


