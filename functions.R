# 

Ftable <- function(model){
  t <- anova(model) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'items') %>% 
    mutate(' ' = ifelse(`p-value` < 0.05, '*',
                        ifelse(`p-value` < 0.1, '\u2020', ''))) %>% 
    kable(digits = 4) %>% 
    kable_styling()
  t
}


Ftable_brms <- function(model){
  table <- summary(model)$fixed
  table2 <-summary(model, prob = 0.9)$fixed %>% as_tibble()
  names <- rownames(table)
  table <- table %>% 
    as_tibble() %>% 
    rename('l95%' = `l-95% CI`, 'u95%' = `u-95% CI`) %>% 
    select(-Rhat, -Bulk_ESS, -Tail_ESS)
  table$`l90%` <- table2$`l-90% CI`
  table$`u90%` <- table2$`u-90% CI`
  
  table <-table%>% 
    mutate(Estimate = ifelse(`l95%`	* `u95%` > 0, 
                             cell_spec(round(Estimate, 4),  bold = T), 
                             ifelse(`l90%`	* `u90%` > 0, 
                                    cell_spec(round(Estimate, 4),  italic = T),cell_spec(round(Estimate,4)))),
           rowname = names) %>% 
    mutate(rowname = str_remove_all(rowname, 'clean|_scale|3level')) %>% 
    column_to_rownames(var = 'rowname') %>% 
    kable(digits = 4, escape = F) %>% 
    kable_styling()
  return(table)
}
