# NDP

# LOAD DATASET ####

##  packages ====
rm(list = ls())
library(readxl)
library(tidyverse)
library(quanteda)

library(ggplot2)
library(ggtext)
library(ggrepel)


## colors =====
red <- "#ee2536" # singapore flag color https://colorswall.com/palette/18520/ 
white <- "#ffffff"

## load dataset =====
themes <- read_excel("ndp.xlsx", sheet = "theme")


# CLEAN DATASET #####

## remove years where there were no themes or missing data
themes <- 
    themes %>% 
    filter(!is.na(Theme))  


# CONVERT TO DFM #####
themes <- 
    themes %>% 
    mutate(id = row_number()) %>% 
    relocate(id, .before = Year)

themes_corp <- corpus(themes,
                      docid_field = "id",
                      text_field = "Theme")

themes_tok <- tokens(themes_corp,
                    remove_punct = T,
                    include_docvars = T,
                    remove_separators = T)


stopwords = c("of", "a", "the", "and", "my", "in", "for", "are")
themes_dfm <- 
    dfm(themes_tok,
        tolower = T) %>% 
    dfm_remove(stopwords)

# TOP FEATURES #####
topfeatures(themes_dfm)   

## Wordcloud with topfeatures ====
library("quanteda.textplots")

set.seed(100)
textplot_wordcloud(themes_dfm, 
                   color = c("#459DE0", red), 
                   min_count = 1, random_order = F ,
                   font = "Roboto Condensed") 


textplot_wordcloud(themes_dfm, 
                   color = c("#459DE0", "#F53446"), 
                   min_count = 1, random_order = F ) 

## Dot plot =====
## Create DF for textstat_freq ==== 
features_plot <- textstat_frequency(themes_dfm, n = 20)
features_plot$feature <- with(features_plot, reorder(feature, docfreq)) 
# docfreq = how many docs the word appear in
# freq = how many times the word appear (can be once or more in a doc)

## Plot the textstat_freq  ==== 
ggplot(features_plot, aes(x = feature, y = (docfreq))) +
    geom_segment( aes(xend=feature, yend=0)) +
    geom_point(color = red, size = 5) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip() + 
    labs(title = "WHAT ARE THE POPULAR WORDS USED IN **NDP THEMES**?",
         x = "",
         y = "Number of occurrences",
         caption = "Plot by Gerard Chung | gerardchung.com | Codes at github.com/gerardchung/ndp2021<br>Note: Not all NDPs had themes; missing data") +
    
    theme_classic(base_family = "Roboto Condensed") + 
    theme(
        rect = element_rect(fill = white),
        panel.background = element_rect(fill = white, color = white),
        plot.background = element_rect(fill = white, color = white),
        plot.title = ggtext::element_markdown(size = 25, face="bold"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown( size = 14, lineheight = 1.2),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "gray20"),
        axis.text.x = element_text(size = 12, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 20, color = "gray20"),
        plot.caption = element_markdown(hjust = 0, lineheight = 1.5 )) -> freqplot

freqplot + 
 #   scale_y_continuous(expand = c(0,2)) + # make words nearer to start of lines
    annotate("text", x=15, y=10, 
             label= "'Together, Our Singapore Spirit”' 
           - NDP, 2021",
             size = 4,
             family = "Roboto Condensed") +
    annotate("text", x=11, y=13, 
             label= "'25 Years of Nation Building, 1959–1984”' 
           - NDP, 1984",
             size = 4,
             family = "Roboto Condensed") +
    annotate("text", x=8, y=10, 
             label= "'Loving Singapore, Our Home”' 
           - NDP, 2012",
             size = 4,
             family = "Roboto Condensed") + 
    annotate("text", x=5, y=8, 
             label= "'National Pride and Confidence in the Future”' 
           - NDP, 1966",
             size = 4,
             family = "Roboto Condensed") -> plot_final


ggsave("plots/topfeatures.png", plot = plot_final, type = 'cairo', width = 9, height = 6.5, dpi = 300, units = "in", bg = white)


# CO-OCCURENCES OF WORDS #####

# Feature-occurrence matrix ######
# https://quanteda.io/articles/pkgdown/examples/twitter.html
# https://tutorials.quanteda.io/basic-operations/fcm/fcm/

# trim down the dfm to those features less than n occurrences
news_dfm_reduced <- dfm_trim(themes_dfm, min_termfreq = 3)
topfeatures(news_dfm_reduced) # top features
nfeat(news_dfm_reduced) # number of features 


library("quanteda.textplots")
news_fcm <- fcm(news_dfm_reduced)
dim(news_fcm)
head(news_fcm)
nfeat(news_fcm)

toptag <- names(topfeatures(news_fcm, 20))
head(toptag, n = 10)
size <- log(colSums(dfm_select(news_dfm_reduced, toptag, selection = "keep")))
top_fcm <- fcm_select(news_fcm, pattern = toptag)

set.seed(100)
textplot_network(top_fcm, min_freq = 0.8, vertex_size = size / max(size) * 3, edge_color = "orange", edge_alpha = 5, edge_size = 2)


set.seed(100)
textplot_network(top_fcm, min_freq = 0.01, edge_color = red, edge_alpha = 0.6, edge_size = 7)

set.seed(100)
textplot_network(top_fcm, min_freq = 0.01, edge_color = red, edge_alpha = 0.4, edge_size = 7)


