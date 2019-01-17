# Example.R

library(tidyverse)
library(ggplot2)
library(SentimentAnalysis)
library(furrr)

# Loading in data ---------------------------------------------------------

fin24 <- readRDS("data/finance_text.rds") %>% 
  tbl_df %>% 
  distinct(PublishDate, CmsArticleId, .keep_all = T)

# Key word search ---------------------------------------------------------

key_word <- c("uncertainty", "economic", "markets", "politic", "ANC")

f_key <- function(key){
  
  key
  key_col <- enquo(key)
  
  fin24 %>% 
    mutate(!!quo_name(key_col) := grepl(key, Body, ignore.case = T)) %>% 
    mutate(PublishDate = as.Date(PublishDate)) %>% 
    select(PublishDate, CmsArticleId, !!quo_name(key_col))
  
}

# Set number of parallel cores to use to n -1

cl <- makeClusterPSOCK(availableCores() - 1)
plan(cluster, workers = cl, gc = T)

# allow futures to export objects of size 1GB or more to nodes
options(future.globals.maxSize= 1024*1024^2)

indicator_table <- tibble(key_word = key_word) %>% 
  mutate(outcome = future_map(key_word, ~.x %>% f_key(.)))

parallel::stopCluster(cl)

fin24_boolean <- indicator_table %>% 
  pull(outcome) %>% reduce(left_join, by = c("PublishDate", "CmsArticleId")) %>% 
  distinct
