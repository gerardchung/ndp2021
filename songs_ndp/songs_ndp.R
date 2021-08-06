# Sentiment analysis on NDP songs

# LOAD DATA #####

rm(list = ls())
library(quanteda)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(tidyverse)
library(readxl)

## colors =====
red <- "#ee2536" # singapore flag color https://colorswall.com/palette/18520/ 
white <- "#ffffff"

getwd()
songs <- read_excel(path = "songs_ndp/ndpsongs.xlsx", 
                    sheet = "songs")

# CLEAN DATA ######

songs <-
    songs %>% 
    mutate(id = row_number()) %>% 
    relocate(id, .before = heading)

## Extract title using regex =====
str_view(songs$heading, pattern = "\"(.*?)\"")
songs$title <- 
    str_extract_all(songs$heading, pattern = "\"(.*?)\"" , simplify = F)

## Remove quotation marks ====
songs$title <- 
    str_remove_all(songs$title, pattern = "\"")

## Remove word chorus from text =====
songs$text <-
    str_remove_all(songs$text, regex(pattern = "pre-Chorus", ignore_case = T))

songs$text <-
    str_remove_all(songs$text, regex(pattern = "chorus", ignore_case = T))

songs$text <-
    str_remove_all(songs$text, regex(pattern = "verse", ignore_case = T))


songs$text <-
    str_remove_all(songs$text, regex(pattern = "bridge", ignore_case = T))

songs$text <-
    str_remove_all(songs$text, regex(pattern = "repeat", ignore_case = T))

# CONVERT TO DFM #####

songs_corp <- corpus(songs,
                     docid_field = "id",
                     text_field = "text")

songs_tok <- tokens(songs_corp,
                    remove_punct = T,
                    remove_symbols = T,
                    remove_numbers = T,
                    include_docvars = T,
                    remove_separators = T)

stopwords = c("oh")

songs_dfm <- 
    dfm(songs_tok,
        tolower = T) %>% 
    dfm_remove(c(stopwords, stopwords("en"))) 

# %>% dfm_wordstem()

# TOP FEATURES #####
topfeatures(songs_dfm)

str_view_all(songs$text, pattern = "oh", match = T) # one song has many "oh"
str_view_all(songs$text, pattern = "can", match = T)
str_view_all(songs$text, pattern = "one", match = T)

 
## Wordcloud with topfeatures ====
library("quanteda.textplots")

set.seed(98)
textplot_wordcloud(songs_dfm, 
                   color = c("gray", "black", "#459DE0", red), 
                   min_count = 5, random_order = F,
                   font = "Roboto Condensed") 

## Dot plot =====
## Create DF for textstat_freq ==== 
features_plot <- textstat_frequency(songs_dfm, n = 20)
features_plot$feature <- with(features_plot, reorder(feature, docfreq)) 
# docfreq = how many docs the word appear in
# freq = how many times the word appear (can be once or more in a doc)

## Plot the textstat_freq  ==== 
ggplot(features_plot, aes(x = feature, y = (docfreq))) +
    geom_segment( aes(xend=feature, yend=0)) +
    geom_point(color = red, size = 5) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_flip() + 
    labs(title = "WHAT ARE THE POPULAR WORDS USED IN **NDP SONGS**?",
         x = "",
         y = "Number of songs the word occurred in",
         caption = "gerardchung.com | Codes at github.com/gerardchung/ndp2021") +
    
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
        plot.caption = element_markdown(hjust = 1, lineheight = 2 )) 


## Plot the textstat_freq in BAR  ==== 
features_plot <-
    features_plot %>% 
    mutate(highlight_key = ifelse( (feature %in% 'together') | (feature %in% 'home'), T, F))
 #   mutate(highlight_key = ifelse(feature == 'together', T, F))

p <- ggplot(data = month, mapping = aes(x = n, y = month))

ggplot(features_plot, aes(x = docfreq , y = (feature))) +
    geom_col(aes(fill = highlight_key)) +
    geom_text(data = features_plot %>% filter(feature == 'together'),
        aes(label = paste(docfreq,"SONGS")),
        hjust = 1, 
        nudge_x = -0.2,
        nudge_y = -0.4,
        fontface = "bold",
        family = "Roboto Condensed",
        size = 13
    ) +
    geom_text(data = features_plot %>% filter(highlight_key != T),
              aes(label = paste(docfreq)),
              hjust = 1, 
              nudge_x = -0.2,
            #  fontface = "bold",
              family = "Roboto Condensed",
              size = 5
    ) +
    theme_minimal(base_family = "Roboto Condensed") +
    scale_fill_manual(values = c('#808080', '#ee2536')) +
    labs(title = "WHAT WORDS ARE MOST USED IN **NDP SONGS**?",
         caption = "gerardchung.com | Codes at github.com/gerardchung/ndp2021"  
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
    ) -> plot_final2

ggsave("plots/topfeatures_songs.png", plot = plot_final2, type = 'cairo', width = 12, height = 8.5, dpi = 400, units = "in", bg = white)


str_view_all(tail(songs$text), pattern = "together", match = T)
str_view_all(tail(songs$text), pattern = "home", match = T)
str_view_all((songs$text), pattern = "home", match = T)



# SENTIMENT ANALYSIS ##### 
    # https://rdrr.io/github/quanteda/quanteda.sentiment/f/README.md
#devtools::install_github("quanteda/quanteda.sentiment")
#library("quanteda.sentiment")
#print(data_dictionary_NRC, max_nval = 8)
#print(data_dictionary_NRC)
#nrc <- data_dictionary_NRC
#print(data_dictionary_AFINN)
#
#songs_dfm %>% 
#textstat_polarity(dictionary = data_dictionary_NRC)
#
#songs_dfm %>% 
#    textstat_valence(dictionary = data_dictionary_NRC)


## Tidyverse =====
# https://www.tidytextmining.com/sentiment.html
library(tidytext)

nrc_sentiments <- get_sentiments("nrc")
unique(nrc_sentiments$sentiment)

### Get into tidy format ====
tidy_songs <-
    songs %>% 
    unnest_tokens(word, text)

### Filter three sentiment  ====

nrc_ndp <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy"| sentiment == "trust"| sentiment == "anticipation")

# Count of sentiments ====
sentiment_wrdcount <- tidy_songs %>% 
                      inner_join(nrc_ndp) %>% 
                      group_by(sentiment) %>% 
                      count(word, sort = T) %>% 
                      ungroup()


# plot top features for each sentiment
sentiment_wrdcount_reduced <- 
                    sentiment_wrdcount %>%
                    group_by(sentiment) %>% 
                    slice_max(n, n = 5) %>% # top five highest counts for each sentiment
                    ungroup() %>% 
                    mutate(word = reorder(word, n))


p <- ggplot(sentiment_wrdcount_reduced, aes(n, word , fill = sentiment)) 
#p <- ggplot(sentiment_wrdcount_reduced, aes(n, word)) 

sentiment_wrdcount_reduced$sentiment <- factor(sentiment_wrdcount_reduced$sentiment, 
                                               levels = c("anticipation", "joy", "trust"),
                                               labels = c("anticipation", "joy", "trust"))

p + geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") + 
    labs(x = "Contribution to sentiment",
         y = NULL) +
    geom_text(
    aes(label = paste(n)),
    hjust = 1, 
    nudge_x = -0.2,
   # fontface = "bold",
    family = "Roboto Condensed",
    size = 7) +
    theme_minimal(base_family = "Roboto Condensed") +
    scale_fill_manual(values = c("#459DE0", "#ee2536", "orange")) +
    labs(title = "POSITIVE FEELINGS IN NDP SONGS",
         subtitle = "What words contribute to anticipation, joy, & trust?",
         caption = "gerardchung.com | Codes: https://github.com/gerardchung/ndp2021") +
    theme(rect = element_rect(fill = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          axis.text.y = element_text(size = 30, hjust = 1, family = "Roboto Condensed", color = "black"),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = ggtext::element_markdown(size = 32, face="bold"),
          plot.subtitle = ggtext::element_markdown(size = 28, face="bold"),
          plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
          legend.position = "none",
          strip.text.x = element_text(
              size = 25, face = "italic", family = "Roboto Condensed"
          )
    ) -> plot_final

ggsave("plots/positivesentiments_words.png", plot = plot_final, type = 'cairo', width = 9, height = 6.5, dpi = 300, units = "in", bg = "#ffffff")


# SENTIMENT BY SONG #####
    # figure out the number of words for each sentiment by each song

## Count of sentiments ====
sentiment_wrdcount_bysong <- 
    tidy_songs %>% 
    inner_join(nrc_ndp) %>% 
    group_by(id, sentiment) %>% 
    count(word, sort = T) %>% 
    summarise(sum = sum(n)) %>% 
    ungroup() 

## Standardized it ====
## proportion of total words of positive emotions that belong to each emotion
sentiment_wrdcount_bysong <- 
    sentiment_wrdcount_bysong %>% 
    group_by(id) %>% 
    mutate(prop = sum/sum(sum)) %>% 
    ungroup()

## Check out sentiment for specific songs like "home" =====

sentiment_focus <-
    sentiment_wrdcount_bysong %>% 
    filter(id == 5|id == 9|id == 31)

sentiment_focus$id <- factor(sentiment_focus$id,
                             levels = c(5,9,31),
                             labels = c("One People, One Nation, One SG (1990)", "Home (1998)", "The road ahead (2021)"))
    
ggplot(data = sentiment_focus, aes(x=sentiment, y = prop, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
   # facet_wrap(~id, scales = "free_y") + 
    facet_wrap(~id) + 
  #  geom_text(
    geom_text(data = sentiment_focus %>% filter(id == "Home (1998)"),
              aes(label = paste(sentiment)),
              vjust = +2, 
              #nudge_x = -0.2,
              fontface = "bold",
              family = "Roboto Condensed",
              size = 5) + 
    scale_fill_manual(values = c("#459DE0", red, "orange")) + 
    theme_minimal(base_family = "Roboto Condensed") +
    labs(title = "COMPARING POSITIVE FEELINGS ACROSS THREE NDP SONGS",
         subtitle = "Proportion of feelings anticipation, joy, & trust",
         caption = "gerardchung.com | Codes: https://github.com/gerardchung/ndp2021") +
    theme(rect = element_rect(fill = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
         # axis.text.y = element_text(size = 15, hjust = 1, family = "Roboto Condensed", color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = ggtext::element_markdown(size = 32, face="bold"),
          plot.subtitle = ggtext::element_markdown(size = 28, face="bold"),
          plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
          legend.position = "none",
          strip.text.x = element_text(
              size = 15, face = "bold.italic", family = "Roboto Condensed"
          )) -> plot_final3

ggsave("plots/sentiments_3songs.png", plot = plot_final3, type = 'cairo', width = 12, height = 8.5, dpi = 400, units = "in", bg = white)

    
    

## plot each of the sentiment by id  ====

p <- ggplot(sentiment_wrdcount_bysong, aes(x = id, y = sum, color = sentiment))
p + geom_point(aes(color = factor(sentiment))) + geom_smooth(method = lm, se = F)


p2 <- ggplot(sentiment_wrdcount_bysong, aes(x = id, y = prop, color = sentiment))
p2 + 
    geom_point(aes(color = factor(sentiment)), size = 1.3) + 
    geom_smooth(method = lm, se = F) + 
    ggrepel::geom_text_repel(data = sentiment_wrdcount_bysong %>% filter(id == "5"),
              aes(label = paste("One People, One Nation, One SG (1990)")),
              vjust = +2, 
              nudge_x = -0.5,direction = "y",
              min.segment.length = 0,
              family = "Roboto Condensed",
              size = 4) + 
    ggrepel::geom_text_repel(data = sentiment_wrdcount_bysong %>% filter(id == "9"),
                             aes(label = paste("Home (1998)")),
                             vjust = +2, 
                              direction = "y",
                             min.segment.length = 0,
                             family = "Roboto Condensed",
                             size = 4) + 
    ggrepel::geom_text_repel(data = sentiment_wrdcount_bysong %>% filter(id == "31"),
                             aes(label = paste("The road ahead (2021)")),
                             vjust = +2, 
                             nudge_x = -0.5,direction = "y",
                             min.segment.length = 0,
                             family = "Roboto Condensed",
                             size = 4) + 
    facet_wrap(~sentiment) + 
    scale_color_manual(values = c("#459DE0", "#ee2536", "orange")) +
    theme_minimal(base_family = "Roboto Condensed") +
    labs(title = "POSITIVE FEELINGS IN NDP SONGS",
         subtitle = "How anticipation, joy, & trust change over time",
         y = "Proportion of feelings within song",
         caption = "Note: each dot is a song | gerardchung.com | Codes: https://github.com/gerardchung/ndp2021") +
    theme(rect = element_rect(fill = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          axis.text.y = element_text(size = 20, family = "Roboto Condensed", color = "black"),
          axis.title.y = element_text(size = 20,  family = "Roboto Condensed", color = "black"),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = ggtext::element_markdown(size = 32, face="bold"),
          plot.subtitle = ggtext::element_markdown(size = 28, face="bold"),
          plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
          legend.position = "none",
          strip.text.x = element_text(
              size = 25, face = "italic", family = "Roboto Condensed"
          )) -> plot_final4

ggsave("plots/sentiments_changeovertime.png", plot = plot_final4, type = 'cairo', width = 12, height = 8.5, dpi = 400, units = "in", bg = white)

p2 + geom_point(aes(color = factor(sentiment))) + geom_smooth(method = loess, se = F)









