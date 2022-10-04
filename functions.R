# show table
Ftable <- function(model){
  t <- Anova(model, type = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'items') %>% 
    mutate(' ' = ifelse(`Pr(>Chisq)` < 0.05, '\u2217',
                        ifelse(`Pr(>Chisq)` < 0.1, '\u2020', ''))) %>% 
    kable(digits = 4) %>% 
    kable_styling()
  t
}



Ftable_ext <- function(model){
  
  # anova table
  table_fix <- model %>% Anova %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'items') %>% 
    #mutate(df = paste(numDF, denDF, sep = ', '),
    mutate(df = Df,
           ' ' = ifelse(`Pr(>Chisq)` < 0.05, '\u2217',
                        ifelse(`Pr(>Chisq)` < 0.1, '\u2020', '')),
           `Pr(>Chisq)` = sprintf('%.3f',round(`Pr(>Chisq)`,3)),
           `Chisq` = sprintf('%.3f',round(`Chisq`,3))
    ) %>% 
    dplyr::select(items, df, `Chisq`,`Pr(>Chisq)`, ` `)
  
  # variance
  table_rand <- VarCorr(model)[,2] %>% 
    as.data.frame() %>% 
    rename('SD' = ".") %>% 
    unique() %>%
    rownames_to_column('Random effects') %>% 
    mutate(    SD           = sprintf('%.3f',round(as.numeric(as.character(SD)),3))) %>% 
    mutate(    SD           = as.character(SD),
               `Random effects` = str_replace(`Random effects`, 'alone.*|[A-Z].*', ''))
  table_rand[nrow(table_rand),1] <- 'Residual'
  
  # merge both
  length_fix <- nrow(table_fix)
  length_rand<- nrow(table_rand)
  table_fix[(length_fix+1):(length_fix + length_rand + 1),] <- ''
  table_fix[(length_fix+1):(length_fix + length_rand +1),1] <- c('Random effects',table_rand[,1]) # add terms of random effects
  table_fix[(length_fix+1),2] <- 'SD'
  table_fix[(length_fix+2):(length_fix + length_rand + 1),2] <- table_rand[,2]
  return(table_fix)
  
}

# remove legend but keep space

# theme(
#   legend.text = element_text(color = "white"),
#   legend.title = element_text(color = "white"),
#   legend.key = element_rect(fill = "white")
# )