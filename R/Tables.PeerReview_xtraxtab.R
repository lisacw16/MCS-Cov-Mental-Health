# Cross-tab income quintile vs residence + p-val
descriptives_incquint <- function(df, string){
  # -- Income -- #
  income_wpct <- with(df, 
                      by(df, .imp, 
                         function(x) 
                           c(weights::wpct(x$W6_income,
                                           weight =
                                             x$CW1_COMBWT,
                                           na.rm=TRUE))))
  
  income_wpct <- round(100*(
    Reduce("+", income_wpct)/length(income_wpct)),
    digits = 1)
  income_wpct <- as.data.frame(income_wpct) %>%
    mutate(id = c("1", "2", "3", "4", "5")) %>%
    rename(wpct = income_wpct)
  
  
  income_count1 <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, W6_income == "1") %>%
                              summarise(n=n())
                          )))
  
  income_count1 <- round(
    Reduce("+",income_count1)/length(income_count1),
    digits = 0)
  
  income_count1 <- as.data.frame(income_count1,
                                 row.names ='1') %>%
    mutate(id = "1") %>%
    rename(n = income_count1)
  
  income_count2 <- with(df,
                        by(df, 
                           .imp, 
                           function(x) c(
                             filter(x, W6_income == "2") %>%
                               summarise(n=n())
                           )))
  income_count2 <- round(
    Reduce("+",income_count2)/length(income_count2),
    digits = 0)
  income_count2 <- as.data.frame(income_count2,
                                 row.names ='2') %>%
    mutate(id = "2") %>%
    rename(n = income_count2)
  
  
  income_count3 <- with(df,
                        by(df, 
                           .imp, 
                           function(x) c(
                             filter(x, W6_income == "3") %>%
                               summarise(n=n())
                           )))
  income_count3 <- round(
    Reduce("+",income_count3)/length(income_count3),
    digits = 0)
  income_count3 <- as.data.frame(income_count3,
                                 row.names ='3') %>%
    mutate(id = "3") %>%
    rename(n = income_count3)
  
  income_count4 <- with(df,
                        by(df, 
                           .imp, 
                           function(x) c(
                             filter(x, W6_income == "4") %>%
                               summarise(n=n())
                           )))
  income_count4 <- round(
    Reduce("+",income_count4)/length(income_count4),
    digits = 0)
  income_count4 <- as.data.frame(income_count4,
                                 row.names ='4') %>%
    mutate(id = "4") %>%
    rename(n = income_count4)
  
  income_count5 <- with(df,
                        by(df, 
                           .imp, 
                           function(x) c(
                             filter(x, W6_income == "5") %>%
                               summarise(n=n())
                           )))
  income_count5 <- round(
    Reduce("+",income_count5)/length(income_count5),
    digits = 0)
  income_count5 <- as.data.frame(income_count5,
                                 row.names ='5') %>%
    mutate(id = "5") %>%
    rename(n = income_count5)
  
  ##
  inc_quint <- 
    rbind(income_count1,
          income_count2,
          income_count3,
          income_count4,
          income_count5)
  
  # -- Bind -- #
  income_wpct %>%
    full_join(inc_quint, by = c("id")) %>%
    mutate(summary = paste(n,"(",wpct,")")) %>%
    select(-c(wpct,n)) %>%
    rename_with(~paste0(string, recycle0 = TRUE), starts_with("summary"))
  
}

xtab.inc_res <- function(df){
  nosib <- filter(df, residence == 
                    "Living with parents, no siblings")
  left <-filter(df, residence == 
                  "Left parental home")
  wsib <-filter(df, residence == 
                  "Living with parents & siblings")
  
  overall_bin <- descriptives_incquint(df, "Overall")
  nosib_bin <- descriptives_incquint(nosib, "No Siblings")
  left_bin <- descriptives_incquint(left, "Left Home")
  wsib_bin <- descriptives_incquint(wsib, "W Siblings")
  
  #----#
   dataframe <-  overall_bin %>%
      full_join(nosib_bin, by = c("id")) %>%
      full_join(left_bin, by = c("id")) %>%
      full_join(wsib_bin, by = c("id")) %>%
      rename(inc_quint = id)
    
   
   gt::gt(dataframe, rownames_to_stub = TRUE) %>%
     tab_source_note(source_note = md("*Notes*: Each category is summarised with unweighted N and weighted percent in parentheses. ")) %>%
     tab_header("PR03 Table 3: Cross tabulation of age 14 family income quintile and age 19 COVID-19 residence during the May 2020 lockdown") %>%
     gtsave(filename = 'PR03_Table_incres_xtab.rtf',
            path = 'Figures_Tables/')
   
   chi <- with(df, 
               by(df, .imp, 
                  function(x) 
                    c(weights::wtd.chi.sq(x$W6_income, 
                                          x$residence,
                                          weight =
                                            x$CW1_COMBWT,
                                    na.rm=TRUE))))
   
   chi <- round(100*(
     Reduce("+", chi)/length(chi)),
     digits = 4)
   chi <- as.data.frame(chi)
   
   print(chi)

    return(dataframe)
}

ols.inc_res <- function(data){
  lowinc <- mice::as.mids(impute_low(data))
  highinc <- mice::as.mids(impute_high(data))

  inc_low_k6 <- with(lowinc,
                     lm(covid_K6 ~
                          existing_K6+cov_symp+ethnicity+region+
                          residence*male+ 
                          moved+economic_activity+
                          fam_ses_par_ed+fam_ses_overcrowded+
                          step_7+single_7+
                          conflict, 
                        weights = CW1_COMBWT))
  inc_low_k6 <- mice::pool(inc_low_k6)
  inc_low_wb <- with(lowinc,
                     lm(covid_SWEMWBS ~
                          existing_SWEMWBS+cov_symp+
                          ethnicity+region+
                          residence*male+ 
                          moved+economic_activity+
                          fam_ses_par_ed+fam_ses_overcrowded+
                          step_7+single_7+
                          conflict, 
                        weights = CW1_COMBWT))
  inc_low_wb <- mice::pool(inc_low_wb)
  inc_high_k6 <- with(highinc,
                     lm(covid_K6 ~
                          existing_K6+cov_symp+ethnicity+region+
                          residence*male+ 
                          moved+economic_activity+
                          fam_ses_par_ed+fam_ses_overcrowded+
                          step_7+single_7+
                          conflict, 
                        weights = CW1_COMBWT))
  inc_high_k6 <- mice::pool(inc_high_k6)
  inc_high_wb <- with(highinc,
                     lm(covid_SWEMWBS ~
                          existing_SWEMWBS+cov_symp+
                          ethnicity+region+
                          residence*male+ 
                          moved+economic_activity+
                          fam_ses_par_ed+fam_ses_overcrowded+
                          step_7+single_7+
                          conflict, 
                        weights = CW1_COMBWT))
  inc_high_wb <- mice::pool(inc_high_wb)
  models <- list("Low Income" = inc_low_k6,
                 "High Income" = inc_high_k6,
                 "Low Income" = inc_low_wb,
                 "High Income" = inc_high_wb)
  gof <- tibble::tribble(
    ~raw,            ~clean,    ~fmt,
    "nobs",          "N",       0)
  cm <- c("(Intercept)" = 'Constant',#1
          "maleMale" = "Male",#2
          "residenceLeft parental home" = 
            "Female, Left Parental Home",#3
          'residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings',#4
          "residenceLeft parental home:maleMale" = 
            'Male, Left the Parental Home',#5
          'residenceLiving with parents & siblings:maleMale' = 
            'Male, Living with Parent & Siblings',#6
          "movedYes" = "COVID-19 Living Arrangements Changed")#7
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = NA,
                        estimate = "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        stars = c('+' = 0.1, '*' = .05, '**' = .01))
  table <- table %>%
    tab_row_group(
      id = "constant",
      label = "",
      rows = c(1)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(6,7)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female, Living with Parents, No Siblings)*"),
      rows = c(4,5)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Female)*"),
      rows = c(3)
    ) %>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2,3,4,5)) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(2,3)
    ) %>%
    tab_spanner(
      label = md("K6"),
      columns = c(2,3),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. > 0 ~ better mental health"),
      columns = c(4,5)
    ) %>%
    tab_spanner(
      label = md("SWEMWBS"),
      columns = c(4,5),
      level = 2
    ) %>%
    tab_header("PR04 Table 4: Family living arrangements and young adult mental health during the first COVID-19 lockdown, as captured by the CLS COVID-19 survey Wave 1 and stratified by high (4,5) and low income quintiles (1,2,3)") %>%
    tab_source_note(source_note = md("*Notes*: Standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for the same variables as Table 2 in the accompanying manuscript. All estimates are weighted with provided survey weights. Missing values specified in the notes of Table 1 are filled with multiple imputation. The full,unweighted sample was N = 2578."))%>% 
    gtsave(filename = 'PR04_Table_inc_strat.rtf',
             path = 'Figures_Tables/')
  
  return(table)
}

impute_low <- function(data){
  data <- data %>%
    filter(residence != "Living with siblings, no parents") %>%
    mutate(residence = droplevels(residence)) %>%
    filter(W6_income == "1" |
             W6_income == "2" |
             W6_income == "3" |
             is.na(W6_income)) %>%
    select(-c(transition,residence2,
              flag_5qs,
              existing_residence,partnered,
              Children,Grandparent,Friend,
              CW1_CNUM00,
              CW1_HHNUMWH_6,
              CW1_HHNUM,
              CW1_HHNUMWH_3,
              CW1_COVPER,
              GDOTHS00,
              parents17,
              siblings17,
              W4_income,
              W3_income,
              W2_income,
              W1_income,
              par_structure_single,
              par_structure_step,
              no_sibling,num_sibling,same_sib_gender,
              sib_bully,sib_bully6,
              is_oldest_sib))
  
  print(str(data))
  pmis <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(pmis[pmis > 0], 
             decreasing = TRUE))
  
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("CW1_COMBWT")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("covid_K6",
         "covid_SWEMWBS",
         "existing_K6",
         "existing_SWEMWBS")] <- "norm"
  meth[c("moved",
         "W6_lowincome",
         "fam_ses_overcrowded",
         "step_7",
         "single_7",
         "fam_ses_par_ed",
         "conflict",
         "ethnicity",
         "cov_symp")] <- "logreg"
  meth[c("W6_income",
         "residence",
         "economic_activity",
         "region")] <- "polyreg"
  print(imp$meth)
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE) %>%
    select(-c(starts_with("CW1_WEMWBS"),
              starts_with("CW1_PH")))
}

impute_high <- function(data){
  data <- data %>%
    filter(residence != "Living with siblings, no parents") %>%
    mutate(residence = droplevels(residence)) %>%
    filter(W6_income == "4" |
             W6_income == "5" |
             is.na(W6_income)) %>%
    select(-c(transition,residence2,
              flag_5qs,
              existing_residence,partnered,
              Children,Grandparent,Friend,
              CW1_CNUM00,
              CW1_HHNUMWH_6,
              CW1_HHNUM,
              CW1_HHNUMWH_3,
              CW1_COVPER,
              GDOTHS00,
              parents17,
              siblings17,
              W4_income,
              W3_income,
              W2_income,
              W1_income,
              par_structure_single,
              par_structure_step,
              no_sibling,num_sibling,same_sib_gender,
              sib_bully,sib_bully6,
              is_oldest_sib))
  
  print(str(data))
  pmis <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(pmis[pmis > 0], 
             decreasing = TRUE))
  
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("CW1_COMBWT")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("covid_K6",
         "covid_SWEMWBS",
         "existing_K6",
         "existing_SWEMWBS")] <- "norm"
  meth[c("moved",
         "W6_lowincome",
         "fam_ses_overcrowded",
         "step_7",
         "single_7",
         "fam_ses_par_ed",
         "conflict",
         "ethnicity",
         "cov_symp")] <- "logreg"
  meth[c("W6_income",
         "residence",
         "economic_activity",
         "region")] <- "polyreg"
  print(imp$meth)
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE) %>%
    select(-c(starts_with("CW1_WEMWBS"),
              starts_with("CW1_PH")))
}

# The `cov_response_df` function combines the COVID waves without filtering for primary cohort members. Instead, both primary and secondary cohort members are allowed to remain to guage the participation against that provided in the CLS COVID survey user manual page 11 (https://cls.ucl.ac.uk/wp-content/uploads/2017/02/UCL-Cohorts-COVID-19-Survey-user-guide.pdf).

cov_response_df <- function(cov1,cov2,cov3) {
  data_files <- c(cov1, cov2, cov3)
  for(i in 1:length(data_files)) {
    assign(paste0("cov_wave", i),
           load_sav_file_function(data_files[i]) %>%
             filter(if_any(contains('CNUM00'), not.na)) %>%
             filter(if_any(contains('MCSID'), not.na)) %>%
             select(c(
               contains("MCSID"),
               contains("CNUM")
               ))
    )
  }
  df <- cov_wave1 %>%
    mutate(CNUM = CW1_CNUM00) %>%
    full_join(mutate(cov_wave2, 
                     CNUM = CW2_CNUM00),
              by = c("MCSID", "CNUM")) %>%
    full_join(mutate(cov_wave3, 
                     CNUM = CW3_CNUM00),
              by = c("MCSID", "CNUM")) %>%
    mutate(waveresponse = as.factor(
      case_when(
          !is.na(CW1_CNUM00) &
            !is.na(CW2_CNUM00) & 
            !is.na(CW3_CNUM00) ~ "All 3 waves",
          !is.na(CW1_CNUM00) & 
            !is.na(CW2_CNUM00) & 
            is.na(CW3_CNUM00) ~ "Two waves – W1, W2",
          !is.na(CW1_CNUM00) & 
            is.na(CW2_CNUM00) & 
            !is.na(CW3_CNUM00) ~ "Two waves – W1, W3",
          is.na(CW1_CNUM00) & 
            !is.na(CW2_CNUM00) & 
            !is.na(CW3_CNUM00) ~ "Two waves – W2, W3",
          !is.na(CW1_CNUM00) & 
            is.na(CW2_CNUM00) & 
            is.na(CW3_CNUM00) ~ "Wave 1 only",
          is.na(CW1_CNUM00) & 
            !is.na(CW2_CNUM00) & 
            is.na(CW3_CNUM00) ~ "Wave 2 only",
          is.na(CW1_CNUM00) & 
            is.na(CW2_CNUM00) & 
            !is.na(CW3_CNUM00) ~ "Wave 3 only")))
  
  print(table(df$waveresponse))
  print(nrow(df))
  return(df)
  
}

not.na <- function(x) {
  !is.na(x)
}


