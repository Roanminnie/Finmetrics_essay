fn_boolean_index <- function(criteria){
  
  criteria
  index_criteria <- enquo(criteria)
  
  dataset_boolean %>% 
    mutate(sum = as.numeric(if_else(!!quo_name(index_criteria)=="1",1,0))) 
}

boolean_index <- function(var){
  
  refine_dataset %>% 
    mutate(uncertain = scale(uncertain)) %>%
    select(-Body) %>% 
    select(year_month, financ:uncertain) %>% 
    filter(!!rlang::parse_quosure(var) == 1) %>% 
    group_by(year_month) %>% 
    summarise(uncertain = mean(uncertain, na.rm = T)) %>% 
    mutate(uncertain = scale(uncertain))
}


