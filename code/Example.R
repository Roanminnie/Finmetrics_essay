# Example.R

library(tidyverse)
library(ggplot2)
library(SentimentAnalysis)


# Loading in data ---------------------------------------------------------

fin24 <- readRDS("finance_text.rds") %>% 
  tbl_df %>% 
  distinct()


# Key word search ---------------------------------------------------------

key_word <- c("uncertainty", "economic", "markets", "politic", "ANC")[c(1:2)]

f_key <- function(key){

  key
  key_col <- enquo(key)
  
  fin24 %>% 
    mutate(!!quo_name(key_col) := grepl(key, Body, ignore.case = T)) %>% 
    mutate(PublishDate = as.Date(PublishDate)) %>% 
    select(PublishDate, CmsArticleId, !!quo_name(key_col))
  
}

indicator_table <- tibble(key_word = key_word) %>% 
  mutate(outcome = map(key_word, ~.x %>% f_key(.)))

fin24_boolean <- indicator_table %>% 
  pull(outcome) %>% reduce(left_join, by = c("PublishDate", "CmsArticleId")) %>% 
  distinct

# SentimentAnalysis -------------------------------------------------------

data(DictionaryLM)
str(DictionaryLM)

ruleUncertainy <- function (dtm, d) 
{
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  return((rowSums(as.matrix(dtm[, which(colnames(dtm) %in% 
                                          d$uncertainty)])))/rowSums(as.matrix(dtm)))
}


sentiment <- analyzeSentiment(fin24$Body[1],
                              rules = list("UncertaintyLM" = list(ruleSentiment, loadDictionaryLM())))

f_uncertainty <- function(body){
  analyzeSentiment(body,
                                rules = list("UncertaintyLM" = list(ruleSentiment, loadDictionaryLM()))) %>% 
    unlist
}

fin24_uncertainty <- fin24 %>% 
  slice(1:10) %>% 
  mutate(uncertainty_score = map_dbl(Body, ~.x %>% f_uncertainty)) %>% 
  mutate(PublishDate = as.Date(PublishDate)) %>% 
  select(PublishDate, CmsArticleId, uncertainty_score)

# Combine all ------------------------------------------------------------

fin24_uncertainty %>%
  left_join(fin24_boolean)

