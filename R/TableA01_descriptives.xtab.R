crosstab_table <- function(wide_data) {
  df2 <- wide_data %>%
    filter(!is.na(CW1_COMBWT)) %>%
    filter(CW1_residence != "Living with siblings, no parents" & 
             CW2_residence != "Living with siblings, no parents") %>%
    mutate(CW1_residence = droplevels(CW1_residence),
           CW2_residence = droplevels(CW2_residence)) %>%
    mutate(flag = case_when(
      CW1_residence != CW2_residence ~ 1,
      .default = 0))
  print(weights::wpct(df2$flag, df2$CW2_COMBWT))
  print(table(df2$flag))
  
  df2b <- df2 %>%
    filter(flag == 1) %>%
    mutate(left = case_when(
      CW1_residence != "Left parental home" &
        CW2_residence == "Left parental home" ~ 1,
      .default = 0
    ))
  print(weights::wpct(df2b$left,df2b$CW2_COMBWT))
  
  df3 <- wide_data %>%
    filter(!is.na(CW1_COMBWT)) %>%
    filter(CW1_residence != "Living with siblings, no parents" & 
             CW3_residence != "Living with siblings, no parents") %>%
    mutate(CW1_residence = droplevels(CW1_residence),
           CW3_residence = droplevels(CW3_residence))
  
  df13 <-  survey::svydesign(ids = ~ 1, weights = ~CW3_COMBWT, data = df3)
  df12 <-  survey::svydesign(ids = ~ 1, weights = ~CW2_COMBWT, data = df2)
  
  table12 <- df12 %>% 
    tbl_svysummary(
      digits = list(all_categorical() ~ c(0,1)),
      statistic = list(all_categorical() ~ "{n_unweighted} ({p})"),
      by = CW1_residence,
      include = c(CW2_residence),
      label = list(CW2_residence = "Residence (September – October 2020)")
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md")
    ) %>%
    modify_spanning_header(
      c("stat_1", "stat_2", "stat_3") ~ 
        "Residence May 2020") %>%
    as_gt() %>%
    tab_options(table.width = pct(80))
  
  table13 <- df13 %>% 
    tbl_svysummary(
      digits = list(all_categorical() ~ c(0,1)),
      statistic = list(all_categorical() ~ "{n_unweighted} ({p})"),
      by = CW1_residence,
      include = c(CW3_residence),
      label = list(CW3_residence = "Residence (February – March 2021)")
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md")
    ) %>%
    modify_spanning_header(
      c("stat_1", "stat_2", "stat_3") ~ 
        "Residence May 2020")  %>%
    as_gt() %>%
    tab_source_note(source_note = md("*Notes*: Estimates are unweighted N (weighted %). Weights were provided with the most recent wave of data being explored (i.e., either Wave 2 or Wave 3).")) %>%
    tab_options(table.width = pct(80))

  table13 %>%
    gtsave(filename = 'A01_Table-crosstab.wave13.rtf',
         path = 'Figures_Tables/')
  table12 %>%
    gtsave(filename = 'A01_Table-crosstab.wave12.rtf',
           path = 'Figures_Tables/')
  
  return(list(table13,table12))
    
}
