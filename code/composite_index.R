
comp <- tibble(words = c("financ", "market", "stock", "exchange")) %>% 
  mutate(index = map(words, boolean_index))

index_composite <- comp %>% unnest %>% 
  spread(words, uncertain) %>% 
  .[complete.cases(.),] %>% 
  select(-year_month) %>% 
  FactoMineR::PCA()

composite_index <- index_composite$ind$coord[,1]

ts.plot(composite_index)

comp %>% unnest %>% 
  spread(words, uncertain) %>%
  .[complete.cases(.),] %>% 
  mutate(composite = composite_index) %>% 
  gather(., type, value, -year_month) %>% 
  ggplot(., aes(year_month, value, color = type, group = type)) + 
  geom_line()

### Naive

article_count <- refine_dataset %>% 
  group_by(year_month) %>% 
  summarise(articles = n())

refine_dataset %>% 
  mutate(uncertain = scale(uncertain)) %>%
  select(-Body) %>% 
  select(year_month, financ:uncertain) %>% 
  select(-uncertain) %>% 
  group_by(year_month) %>% 
  summarise_all(sum, na.rm = T) %>% 
  left_join(article_count)

                