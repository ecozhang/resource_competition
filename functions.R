Ftable <- function(model){
  t <- anova(model) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'items') %>% 
    mutate(' ' = ifelse(`p-value` < 0.05, '\u2217',
                        ifelse(`p-value` < 0.1, '\u2020', ''))) %>% 
    kable(digits = 4) %>% 
    kable_styling()
  t
}

