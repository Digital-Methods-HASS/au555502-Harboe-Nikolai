library(tidyverse)
library(jsonlite)
library(rvest)
library(tidytext)
library(patchwork)

api = "h3jAADFIZ6k4mH2snzQJskSHV7OxRc0L" # My personal API-key for New York Times

nytime = function (keyword, year) {
  searchQ = URLencode(keyword)
  url = paste('http://api.nytimes.com/svc/search/v2/articlesearch.json?q=',searchQ,
              '&begin_date=',year,'0101&end_date=',year,'1231&api-key=',api,sep="")
  # Get the total number of search results
  initialsearch = fromJSON(url,flatten = T)
  maxPages = round((initialsearch$response$meta$hits / 10)-1)
  
  # Set the max page limit at 9, taking 10 pages (0-9, 10 total for 100 articles)
  maxPages = ifelse(maxPages >= 9, 9, maxPages)
  
  # Create an empty data frame
  df = data.frame(id=as.numeric(),created_time=character(),snippet=character(),
                  headline=character())
  
  # Save search results into data frame
  for(i in 0:maxPages){
    # Get the search results of each page
    nytSearch = fromJSON(paste0(url, "&page=", i), flatten = TRUE)
    message("Retrieving page ", i+1," from ",year)
    temp = data.frame(id=1:nrow(nytSearch$response$docs),
                      created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline.main)
    df = rbind(df,temp)
    Sys.sleep(6) # Set sleep for 6 seconds, the lowest I found that never gave errors due to too many calls
                 # Lower than 6 is possible (and much faster), but a bit riskier
  }
  return(df)
}

nytime_decade = function (search_term) {
  df_2 = data.frame(id=as.numeric(),created_time=character(),snippet=character(),
                   headline=character())
  for (j in 2000:2009){ # The indices are used as the year range to be searched
    temp_2 = nytime(search_term, j)
    df_2 = rbind(df_2,temp_2)
  }
  return(df_2) # The result is a total 1000 article snippets for each search term.
}

muslim_nytime <-  nytime_decade("muslim") # Each run of a given query takes ca. 10 minutes
  
christian_nytime <-  nytime_decade("christian")

jew_nytime <-  nytime_decade("jew+jewish")
# "jewish" included in query to account for e.g. "muslim" being both a noun and an adjective

write.csv(muslim_nytime, "./data/NYT_muslim.csv")
write.csv(christian_nytime, "./data/NYT_christian.csv")
write.csv(jew_nytime, "./data/NYT_jew.csv")

muslim_trove <- read.csv("./data/trove_muslim.csv")
christian_trove <- read.csv("./data/trove_christian.csv")
jew_trove <- read.csv("./data/trove_jew.csv")

# Get sentiment lexicon
get_sentiments(lexicon = "afinn") #from tidytext package


# Creating function to process .csv files
processing = function(filename) {
  filetext = filename["snippet"] # Picking the relevant column
  filetext = unlist(filetext)
  
  tokens <- tibble(txt = filetext) %>%
    unnest_tokens(word, txt) # Tokenizing the snippets using tidytext
  
  text_stop <- tokens %>%
    anti_join(stop_words) # Removing stopwords using predefined list
  
  #word_count <- text_stop %>% 
  #  count(word) %>% 
  #  arrange(-n)
  #View(word_count)
  # THis displays the most used non-stopwords - interesting, but not necessary
  
  text_afinn <- text_stop %>%
    inner_join(get_sentiments("afinn")) #Bind token list without stopwords to lexicon
  
  text_afinn_hist <- text_afinn %>%
    count(value) # For plotting counts of different values
  
  text_exclude <- text_stop %>% 
    anti_join(get_sentiments("afinn")) %>%
    count(word, sort = TRUE)
  #View(head(text_exclude))
  # This displays the most regularly occurring excl. words; comment in to see
  
  return(text_afinn_hist)
}

#View(processing(christian_trove)) # checking the results

# Creating new objects using the processing function on the gathered data from NYT
muslim_nyt_count <- processing(muslim_nytime)
christian_nyt_count <- processing(christian_nytime)
jew_nyt_count <- processing(jew_nytime)

# Doing the same for the Trove data
muslim_trove_count <- processing(muslim_trove)
christian_trove_count <- processing(christian_trove)
jew_trove_count <- processing(jew_trove)


# Writing a function to plot the results in a histogram
total_plot = function(file_muslim, file_christian, file_jew, data_source, height) {
  # Adding column to the data which indicates whether values are positive or negative (for plotting)
  data_muslim <- file_muslim %>%
    mutate(pos_neg = ifelse(value >= 1, 1, 0))
  
  data_christian <- file_christian %>%
    mutate(pos_neg = ifelse(value >= 1, 1, 0))
  
  data_jew <- file_jew %>%
    mutate(pos_neg = ifelse(value >= 1, 1, 0))
  
  plot_muslim <- ggplot(data = data_muslim, aes(x = factor(value), y = n, 
                                                fill = factor(pos_neg))) + # Using integer values for x-axis
    geom_col() +
    ylim(c(0,height)) + # Defining a set y-axis for ease of comparison
    scale_fill_manual(values = c("red", "green"), guide = NULL) + # Adding positive/negative coloring
    ggtitle(label = "Muslims") +
    theme_bw() +
    xlab("") + # Emptying all but one x-labels so it isn't displayed thrice
    ylab("Count")
  
  plot_christian <- ggplot(data = data_christian, aes(x = factor(value), y = n,
                                                      fill = factor(pos_neg))) +
    geom_col() +
    ylim(c(0,height)) +
    scale_fill_manual(values = c("red", "green"), guide = NULL) + 
    ggtitle(label = "Christians") +
    theme_bw() +
    xlab("Positivity/Negativity Rating") +
    ylab("") # Emptying all but one y-labels so it isn't also displayed thrice
  
  plot_jew <- ggplot(data = data_jew, aes(x = factor(value), y = n,
                                          fill = factor(pos_neg))) +
    geom_col() +
    ylim(c(0,height)) +
    scale_fill_manual(values = c("red", "green"), guide = NULL) +
    ggtitle(label = "Jews") +
    theme_bw() +
    xlab("") +
    ylab("")
  
  # Using the patchwork package to stitch together the plots
  conc_plot <- plot_muslim + plot_christian + plot_jew +
    plot_annotation(title = "Religious Sentiments",
                    subtitle = paste0("in the ", data_source, " datasets"))
  
  return(conc_plot)
}

total_plot(muslim_nyt_count,christian_nyt_count,jew_nyt_count,"New York Times",550)
total_plot(muslim_trove_count,christian_trove_count,jew_trove_count,"Trove",5000)

# Getting the token list with a new function
get_token = function(filename) {
  filetext = filename["snippet"]
  filetext = unlist(filetext)
  
  tokens <- tibble(txt = filetext) %>%
    unnest_tokens(word, txt)
  
  text_stop <- tokens %>%
    anti_join(stop_words)
  
  text_afinn <- text_stop %>%
    inner_join(get_sentiments("afinn"))
  
  return(text_afinn)
}

# Filtering outlier values
muslim_minus_2 <- get_token(muslim_nytime) %>% 
  filter(value == -2)

muslim_minus_3 <- get_token(muslim_nytime) %>% 
  filter(value == -3)

# Count and plot the outliers, first for -2 values
muslim_m2_count <- muslim_minus_2 %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = fct_reorder(factor(word), n)) %>%
  slice(1:20) # Picking out the 20 most regularly occurring terms

ggplot(data = muslim_m2_count, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle(label = "Most common -2 valued words (Muslim)") +
  theme_bw() +
  xlab("Word") +
  ylab("Mentions")

# and also for -3 values
muslim_m3_count <- muslim_minus_3 %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = fct_reorder(factor(word), n)) %>%
  slice(1:20)

ggplot(data = muslim_m3_count, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle(label = "Most common -3 valued words (Muslim)") +
  theme_bw() +
  xlab("Word") +
  ylab("Mentions")

# Positive Trove value outlier examination
muslim_plus_2 <- get_token(muslim_trove) %>% 
  filter(value == -3)

muslim_p2_count <- muslim_plus_2 %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = fct_reorder(factor(word), n)) %>%
  slice(1:20)

ggplot(data = muslim_p2_count, aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  ggtitle(label = "Most common +2 valued words (Muslim)") +
  theme_bw() +
  xlab("Word") +
  ylab("Mentions")
# Here, "support" and "peace" are among the most common +2-valued words in the Trove set.
# However, these are not necessarily a positive sign, as calls for peace are found most amid
# references towards war, and support can similarly refer to taking sides militarily.

# Summarize findings for Muslim mentions in NYT
muslim_summary <- get_token(muslim_nytime) %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )

# Summarize findings for Christian mentions in NYT
christian_summary <- get_token(christian_nytime) %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )

# Summarize findings for Jewish mentions in NYT
jew_summary <- get_token(jew_nytime) %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )

# For Trove now
muslim_summary_trove <- get_token(muslim_trove) %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )

christian_summary_trove <- get_token(christian_trove) %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )

jew_summary_trove <- get_token(jew_trove) %>%
  summarize(
    mean_score = mean(value),
    median_score = median(value)
  )
