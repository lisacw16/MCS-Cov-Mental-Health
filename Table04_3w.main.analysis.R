modelsummary_3w <- function(wave1,wave2,wave3) {
  wave1 <- wave1 %>%
    mutate(economic_activity = 
             as.factor(case_when(
               economic_activity == "In University" |
                 economic_activity == "In Training/Edu" ~ 
                 "In Training/Edu",
               .default = economic_activity)))
  wave1 <- mice::as.mids(wave1)
  wave2 <- mice::as.mids(wave2)
  wave3 <- mice::as.mids(wave3)
  
  k6_wave1 <- with(wave1,
                   lm(covid_K6~
                        existing_K6+cov_symp+ethnicity+region+
                        residence*male+
                        economic_activity+moved+
                        fam_ses_par_ed+
                        W6_lowincome+fam_ses_overcrowded+
                        step_7+single_7+conflict, 
                      weights = CW1_COMBWT))
  k6_wave1 <- mice::pool(k6_wave1)
  wb_wave1 <- with(wave1,
                   lm(covid_SWEMWBS~
                        existing_SWEMWBS+
                        cov_symp+ethnicity+region+
                        residence*male+moved+
                        economic_activity+
                        fam_ses_par_ed+W6_lowincome+fam_ses_overcrowded+
                        step_7+single_7+conflict, 
                      weights = CW1_COMBWT))
  wb_wave1 <- mice::pool(wb_wave1)  
  k6_wave2 <- with(wave2,
                   lm(CW2_Covid_K6  ~ 
                        existing_K6+CW2_cov_symp+
                        CW2_moved+Ethnicity+CW2_Country+
                        CW2_residence*Male+
                        CW2_Economic.Activity+
                        fam_ses_par_ed+W6_lowincome+CW2_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW2_COMBWT))
  k6_wave2 <- mice::pool(k6_wave2)
  wb_wave2 <- with(wave2,
                   lm(CW2_Covid_SWEMWBS ~
                        existing_SWEMWBS+CW2_cov_symp+
                        CW2_moved+Ethnicity+CW2_Country+
                        CW2_residence*Male+
                        CW2_Economic.Activity+
                        fam_ses_par_ed+W6_lowincome+CW2_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW2_COMBWT))
  wb_wave2 <- mice::pool(wb_wave2)
  k6_wave3 <- with(wave3,
                   lm(CW3_Covid_K6  ~ 
                        existing_K6+CW3_cov_symp+
                        CW3_moved+Ethnicity+CW3_Country+
                        CW3_residence*Male+
                        CW3_Economic.Activity+
                        fam_ses_par_ed+W6_lowincome+
                        CW3_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW3_COMBWT))
  k6_wave3 <- mice::pool(k6_wave3)
  wb_wave3 <- with(wave3,
                   lm(CW3_Covid_SWEMWBS ~
                        existing_SWEMWBS+CW3_cov_symp+
                        CW3_moved+Ethnicity+CW3_Country+
                        CW3_residence*Male+
                        CW3_Economic.Activity+
                        fam_ses_par_ed+W6_lowincome+CW3_Overcrowded+
                        W7_steppar+W7_single_par, 
                      weights = CW3_COMBWT))
  wb_wave3 <- mice::pool(wb_wave3)
  
  models <- list("Wave 1" = k6_wave1,
                 "Wave 2" = k6_wave2,
                 "Wave 3" = k6_wave3,
                 "Wave 1" = wb_wave1,
                 "Wave 2" = wb_wave2,
                 "Wave 3" = wb_wave3)
  
  cm <- c("(Intercept)" = 'Constant', #1
          "existing_K6"  = "LDV (Age 17)", #2
          "existing_SWEMWBS" = "LDV (Age 17)", #2
          "MaleMale" = "Male", #3
          "maleMale" = "Male", #3
          "CW3_residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          "CW2_residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          "residenceLeft parental home" = 
            "Female, Left Parental Home", #4
          'CW3_residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          'CW2_residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          'residenceLiving with parents & siblings' = 
            'Female, Living with Parents & Siblings', #5
          "CW2_residenceLeft parental home:MaleMale" = 
            'Male, Left the Parental Home', #6
          "CW3_residenceLeft parental home:MaleMale" = 
            'Male, Left the Parental Home', #6
          "residenceLeft parental home:maleMale" = 
            'Male, Left the Parental Home', #6
          'residenceLiving with parents & siblings:maleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW2_residenceLiving with parents & siblings:MaleMale' = 
            'Male, Living with Parents & Siblings', #7
          'CW3_residenceLiving with parents & siblings:MaleMale' = 
            'Male, Living with Parents & Siblings', #7
          'conflictYes' = 'Increased Intra-HH Conflict', #8
          'CW2_movedYes' = 'Changed COVID-19 Living Arrangements', #9
          'CW3_movedYes' = 'Changed COVID-19 Living Arrangements', #9
          'movedYes' = 'Changed COVID-19 Living Arrangements' #9
          )
  
  gof <- tibble::tribble(
    ~raw,~clean,~fmt,
    "nobs","N",0)
  
  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        gof_map = gof,
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
      id = "moved",
      label = md("*Other Controls*"),
      rows = c(2,8,9)
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
               columns = c(2:7)) %>%
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
      columns = c(5:7),
      level = 2
    ) %>%
    tab_header(title = "", 
               preheader = md("*Living arrangements and well-being outcomes throughout the pandemic (Wave 1, May 2020; Wave 2, September/October 2020; and Wave 3, February/March 2021)*")) %>%
    tab_source_note(source_note = md("*Notes*: Standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All estimates are weighted to account for both sample design and attrition for the respective wave. Weights are provided by the survey team. All models control for the same covariates as Table 2 with minor exceptions. Like Table 2, missing values for single parents, stepparents, family income, and well-being variables are filled with multiple imputation (m = 30). The conflict variable was only asked in the first wave, therefore was only included in the Wave 1 analysis. The economic activity variable, due to slight differences in the questions being asked in each wave, has only five levels (in training/education/university, furloughed, employed, unemployed, and other inactive). The changed living arrangements variable was only asked of participants once, upon entering the survey, regardless of wave (e.g., if a participant joined in Wave 1 the question was only asked in Wave 1; if the participant joined in Wave 2 the question was only asked in Wave 2, etc). Therefore, missing answers for the changed living arrangements variable were filled with information from preceding waves."))
  
  table %>% gtsave(filename = '04_Table_3w.main.rtf',
                   path = 'Figures_Tables/')
  
  return(table)
}