fn_keyword <- function(key){
  
  key
  key_col <- enquo(key)
  
  rawdata %>% 
    mutate(!!quo_name(key_col) := as.numeric(if_else((grepl(key, Body, ignore.case = T))=="TRUE",1,0))) %>% 
    select(PubDate, year_month, ID, Body, !!quo_name(key_col))
  
}
