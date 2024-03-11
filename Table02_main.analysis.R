modelsummary_w1mi<- function(data) {
  data <- mice::as.mids(data)
  
  k61 <- with(data,
              lm(covid_K6 ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence*male, 
                 weights = CW1_COMBWT))
  k61 <- mice::pool(k61)
  
  k62 <- with(data,
              lm(covid_K6  ~ existing_K6+cov_symp+ethnicity+region+
                   residence*male+moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7, 
                 weights = CW1_COMBWT))
  k62 <- mice::pool(k62)
  k63 <- with(data,
              lm(covid_K6 ~
                   existing_K6+cov_symp+ethnicity+region+
                   residence*male+ 
                   moved+economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7+
                   conflict, 
                 weights = CW1_COMBWT))
  k63 <- mice::pool(k63)
  
  wb1 <- with(data, 
               lm(covid_SWEMWBS ~ 
                    existing_SWEMWBS+cov_symp+ethnicity+region+
                    residence*male, 
                  weights = CW1_COMBWT))
  wb1 <- mice::pool(wb1)
  
  wb2 <- with(data,
               lm(covid_SWEMWBS ~
                    existing_SWEMWBS+cov_symp+ethnicity+region+
                    residence*male+
                    moved+
                    economic_activity+
                    fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                    step_7+single_7, 
                  weights = CW1_COMBWT))
  wb2 <- mice::pool(wb2)
  
  wb3 <- with(data, 
              lm(covid_SWEMWBS ~ 
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence*male+ 
                   moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7 +
                   conflict, 
                 weights = CW1_COMBWT))
  wb3 <- mice::pool(wb3)
  
  models <- list("Model 1" = k61,
                 "Model 2" = k62,
                 "Model 3" = k63,
                 "Model 1" = wb1,
                 "Model 2" = wb2,
                 "Model 3" = wb3)
  
  cm <- c("(Intercept)" = 'Constant',#1
          "existing_K6"  = "LDV (Age 17)",#2
          "maleMale" = "Male",#3
          "residenceLeft parental home" = "Left Parental Home",#4
          'residenceLiving with parents & siblings' = 
            'Living with Parents & Siblings',#5
          "movedYes" = "COVID-19 Living Arrangements Changed",#6
          "economic_activityIn Training/Edu" = 
            "In Education (Not University)",#7
          "economic_activityEmployed" = "Employed",#8
          "economic_activityFurloughed" = "Furloughed",#9
          "economic_activityUnemployed" = "Unemployed",#10
          "economic_activityOther Inactive" = "Economically Inactive",#11
          "fam_ses_par_edYes" = "High Parental Education",#12
          "W6_lowincomeYes" = "Low Family Income",#13
          "fam_ses_overcrowdedYes" = "Overcrowded COVID-19 HH", #14
          "step_7Yes" = "Has Stepparent",#15
          "single_7Yes" = "Has Single Parent",#16
          "conflictYes" = "Increased Conflict",#17
          "residenceLeft parental home:maleMale" = 
            'Male, Left the Parental Home',#18
          'residenceLiving with parents & siblings:maleMale' = 
            'Male, Living with Parent & Siblings',#19
          "existing_SWEMWBS" = "LDV (Age 17)") #2

  
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
      id = "ldv",
      label = "",
      rows = c(2)
    ) %>%
    tab_row_group(
      id = "conflict",
      label = md("*COVID-19 HH Conflict*"),
      rows = c(17)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Family Factors*"),
      rows = c(12,13,14,15,16)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Young Adult in University)*"),
      rows = c(7,8,9,10,11)
    ) %>%
    tab_row_group(
      id = "move",
      label = "",
      rows = c(6)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(18,19)
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
               columns = c(2,3,4,5,6,7)) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(2,3,4)
    ) %>%
    tab_spanner(
      label = md("K6"),
      columns = c(2,3,4),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. > 0 ~ better mental health"),
      columns = c(5,6,7)
    ) %>%
    tab_spanner(
      label = md("SWEMWBS"),
      columns = c(5,6,7),
      level = 2
    ) %>%
    tab_source_note(source_note = md("*Notes*: Standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), ethnicity (non-white), and experience of COVID-19 symptoms. All estimates are weighted with provided survey weights. Missing values specified in the notes of Table 1 are filled with multiple imputation. The unweighted, imputed sample was N = 2578."))
  
  table %>% gtsave(filename = '02_Table_mi.interaction.rtf',
                   path = 'Figures_Tables/')

  return(table)
}

modelsummary_w1mi_stratified<- function(data) {
  data <- mice::as.mids(data)
  fe_data <- data %>% filter(male == "Female")
  ma_data <- data %>% filter(male == "Male")
  
  k61fem <- with(fe_data,
              lm(covid_K6 ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence, 
                 weights = CW1_COMBWT))
  k61fem <- mice::pool(k61fem)
  
  k62fem <- with(fe_data,
              lm(covid_K6  ~ 
                   existing_K6+cov_symp+ethnicity+region+
                   residence+moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7, 
                 weights = CW1_COMBWT))
  k62fem <- mice::pool(k62fem)
  k63fem <- with(fe_data,
              lm(covid_K6 ~
                   existing_K6+cov_symp+ethnicity+region+
                   residence+ 
                   moved+economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7+
                   conflict, 
                 weights = CW1_COMBWT))
  k63fem <- mice::pool(k63fem)
  
  k61masc <- with(ma_data,
                 lm(covid_K6 ~ 
                      existing_K6+cov_symp+ethnicity+region+
                      residence, 
                    weights = CW1_COMBWT))
  k61masc <- mice::pool(k61masc)
  
  k62masc <- with(ma_data,
                 lm(covid_K6  ~ 
                      existing_K6+cov_symp+ethnicity+region+
                      residence+moved+
                      economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7, 
                    weights = CW1_COMBWT))
  k62masc <- mice::pool(k62masc)
  k63masc <- with(ma_data,
                 lm(covid_K6 ~
                      existing_K6+cov_symp+ethnicity+region+
                      residence+ 
                      moved+economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7+
                      conflict, 
                    weights = CW1_COMBWT))
  k63masc <- mice::pool(k63masc)
  
  wb1fem <- with(fe_data, 
              lm(covid_SWEMWBS ~ 
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence, 
                 weights = CW1_COMBWT))
  wb1fem <- mice::pool(wb1fem)
  
  wb2fem <- with(fe_data,
              lm(covid_SWEMWBS ~
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence+
                   moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7, 
                 weights = CW1_COMBWT))
  wb2fem<- mice::pool(wb2fem)
  
  wb3fem <- with(fe_data, 
              lm(covid_SWEMWBS ~ 
                   existing_SWEMWBS+cov_symp+ethnicity+region+
                   residence+ 
                   moved+
                   economic_activity+
                   fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                   step_7+single_7 +
                   conflict, 
                 weights = CW1_COMBWT))
  wb3fem <- mice::pool(wb3fem)
  
  wb1masc <- with(ma_data, 
                 lm(covid_SWEMWBS ~ 
                      existing_SWEMWBS+cov_symp+ethnicity+region+
                      residence, 
                    weights = CW1_COMBWT))
  wb1masc <- mice::pool(wb1masc)
  
  wb2masc <- with(ma_data,
                 lm(covid_SWEMWBS ~
                      existing_SWEMWBS+cov_symp+ethnicity+region+
                      residence+
                      moved+
                      economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7, 
                    weights = CW1_COMBWT))
  wb2masc<- mice::pool(wb2masc)
  
  wb3masc <- with(ma_data, 
                 lm(covid_SWEMWBS ~ 
                      existing_SWEMWBS+cov_symp+ethnicity+region+
                      residence+ 
                      moved+
                      economic_activity+
                      fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                      step_7+single_7 +
                      conflict, 
                    weights = CW1_COMBWT))
  wb3masc <- mice::pool(wb3masc)
  
  models <- list("1" = k61fem,
                 "2" = k62fem,
                 "3" = k63fem,
                 "1" = k61masc,
                 "2" = k62masc,
                 "3" = k63masc,
                 "1" = wb1fem,
                 "2" = wb2fem,
                 "3" = wb3fem,
                 "1" = wb1masc,
                 "2" = wb2masc,
                 "3" = wb3masc)
  
  cm <- c("(Intercept)" = 'Constant', #1
          "existing_K6"  = "LDV (Age 17)", #2
          "residenceLeft parental home" = "Left Parental Home", #3
          'residenceLiving with parents & siblings' = 
            'Living with Parents & Siblings', #4
          "movedYes" = "COVID-19 Living Arrangements Changed", #5
          "economic_activityIn Training/Edu" = 
            "In Education (Not University)", #6
          "economic_activityEmployed" = "Employed", #7
          "economic_activityFurloughed" = "Furloughed", #8
          "economic_activityUnemployed" = "Unemployed", #9
          "economic_activityOther Inactive" = 
            "Other Inactive", #10
          "fam_ses_par_edYes" = "High Parental Education", #11
          "W6_lowincomeYes" = "Low Family Income", #12
          "fam_ses_overcrowdedYes" = "Overcrowded COVID-19 HH", #13
          "step_7Yes" = "Has Stepparent", #14
          "single_7Yes" = "Has Single Parent", #15
          "conflictYes" = "Increased Conflict", #16
          "existing_SWEMWBS" = "LDV (Age 17)")#2
  
  gof <- tibble::tribble(
    ~raw,~clean,~fmt,
    "nobs","Observations",0,
    'nimp',"Imputations",0,
    "adj.r.squared","R\U00B2",2)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = "none",
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
      id = "ldv",
      label = "",
      rows = c(2)
    ) %>%
    tab_row_group(
      id = "conflict",
      label = md("*COVID-19 HH Conflict*"),
      rows = c(16)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Family Factors*"),
      rows = c(11,12,13,14,15)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Young Adult in University)*"),
      rows = c(6,7,8,9,10)
    ) %>%
    tab_row_group(
      id = "move",
      label = "",
      rows = c(5)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Living with Parents, No Siblings)*"),
      rows = c(3,4)
    ) %>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2:13)) %>%
    tab_spanner(
      id = 'femK6',
      label = "Women",
      columns = c(2,3,4)
    ) %>%
    tab_spanner(
      id = 'mascK6',
      label = "Men",
      columns = c(5,6,7)
    ) %>%
    tab_spanner(label = "K6",
                columns= c(2:7),
                level = 2)%>%
    tab_spanner(
      id = 'femWB',
      label = "Women",
      columns = c(8,9,10)
    ) %>%
    tab_spanner(
      id = 'mascWb',
      label = "Men",
      columns = c(11,12,13)
    ) %>%
    tab_spanner(label = "SWEMWBS",
                columns= c(8:13),
                level = 2)%>%
    tab_source_note(source_note = md("*Notes*: Standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), ethnicity (non-white), and experience of COVID-19 symptoms. All estimates are weighted with provided weights that account for both design and attrition. Missing values filled with multiple imputation."))
  
  table %>% gtsave(filename = '02_Table_mi.stratified.rtf',
                   path = 'Figures_Tables/')
  
  return(table)
}