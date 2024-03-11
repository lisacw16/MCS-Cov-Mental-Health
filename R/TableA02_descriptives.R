TableA02_w2_descriptives <- function(df) {
  df <-  df %>% 
    filter(!is.na(CW2_COMBWT) & 
             CW2_residence != "Living with siblings, no parents") %>%
    mutate(CW2_residence = droplevels(CW2_residence),
           CW2_Country = droplevels(CW2_Country))
  df <-  survey::svydesign(ids = ~ 1, weights = ~CW2_COMBWT, data = df)
  table <- df %>% 
    tbl_svysummary(
      digits = list(all_continuous() ~ 2,
                    all_categorical() ~ c(0,1)),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n_unweighted} ({p})"),
      missing_text = "Number Imputed",
      by = Male,
      include = c(CW2_Covid_K6,
                  existing_K6,
                  CW2_Covid_SWEMWBS,
                  existing_SWEMWBS,
                  CW2_moved,
                  CW2_residence,
                  CW2_Economic.Activity,
                  W6_lowincome,
                  fam_ses_par_ed,
                  CW2_Overcrowded,
                  W7_steppar,
                  W7_single_par,
                  Ethnicity,
                  CW2_cov_symp,
                  CW2_Country),
      label = list(CW2_Covid_K6 ~ "September – October 2020 K6", 
                   existing_K6 ~ "K6 (Age 17)",
                   CW2_moved ~ "Living Arrangements have Changed Since Mar '20",
                   CW2_Covid_SWEMWBS ~ "September – October 2020 SWEMWBS",
                   existing_SWEMWBS ~ "SWEMWBS (Age 17)",
                   CW2_residence ~ "COVID-19 Living Arrangements",
                   CW2_Economic.Activity ~ "Young Adult Economic Activity",
                   W6_lowincome ~ "Low Family Income (Age 14)",
                   fam_ses_par_ed ~ "High Parental Education (Age 14)",
                   CW2_Overcrowded ~ "Overcrowded COVID-19 HH",
                   W7_steppar ~ 
                     "Has Stepparent (Age 17)",
                  W7_single_par ~ 
                    "Has Single Parent (Age 17)",
                   Ethnicity ~ "Ethnicity Non-White",
                   CW2_cov_symp ~
                     "Experienced COVID-19 Symptom(s)",
                   CW2_Country ~ "Country")
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md"),
                  stat_0 = "Overall<br>{n_unweighted}, {n}",
                  stat_1 = "Female<br>{n_unweighted}, {n}",
                  stat_2 = "Male<br>{n_unweighted}, {n}"
    ) %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Young Adult Gender**") %>%
    modify_footnote(all_stat_cols() ~ "Estimated by authors. Mental well-being variables are continuous and summarized as weighted mean (weighted standard error); the other variables are categorical and summarized with unweighted N (weighted %). All variables derived from either the MCS mainstage survey or the Wave 2 COVID-19 survey, administered September – October 2020 survey. Weights were provided with the COVID-19 data.") %>%
    as_gt() %>%
    tab_options(table.width = pct(80))
  
  
  table %>%
    gtsave(filename = 'A02_Table.w2_descriptives.rtf',
           path = 'Figures_Tables/')
  
  return(table)
  
}

TableA02_w3_descriptives <- function(df) {
  df <-  df %>% 
    filter(!is.na(CW3_COMBWT) & 
             CW3_residence != "Living with siblings, no parents") %>%
    mutate(CW3_residence = droplevels(CW3_residence),
           CW3_Country = droplevels(CW3_Country))
  df <-  survey::svydesign(ids = ~ 1, weights = ~CW3_COMBWT, data = df)
  table <- df %>% 
    tbl_svysummary(
      digits = list(all_continuous() ~ 2,
                    all_categorical() ~ c(0,1)),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n_unweighted} ({p})"),
      missing_text = "Number Imputed",
      by = Male,
      include = c(CW3_Covid_K6,
                  existing_K6,
                  CW3_Covid_SWEMWBS,
                  existing_SWEMWBS,
                  CW3_residence,
                  CW3_moved,
                  CW3_Economic.Activity,
                  W6_lowincome,
                  fam_ses_par_ed,
                  CW3_Overcrowded,
                  W7_steppar,
                  W7_single_par,
                  Ethnicity,
                  CW3_cov_symp,
                  CW3_Country),
      label = list(CW3_Covid_K6 ~ "February – March 2021 K6", 
                   existing_K6 ~ "K6 (Age 17)",
                   CW3_Covid_SWEMWBS ~ "February – March 2021 SWEMWBS",
                   existing_SWEMWBS ~ "SWEMWBS (Age 17)",
                   CW3_residence ~ "Living Arrangements have Changed Since Mar '20",
                   CW3_moved ~ "HH Members have Changed since May 2020",
                   CW3_Economic.Activity ~ "Young Adult Economic Activity",
                   W6_lowincome ~ "Low Family Income (Age 14)",
                   fam_ses_par_ed ~ "High Parental Education (Age 14)",
                   CW3_Overcrowded ~ "Overcrowded COVID-19 HH",
                   W7_steppar ~ 
                     "Has Stepparent (Age 17)",
                   W7_single_par ~ 
                     "Has Single Parent (Age 17)",
                   Ethnicity ~ "Ethnicity Non-White",
                   CW3_cov_symp ~
                     "Experienced COVID-19 Symptom(s)",
                   CW3_Country ~ "Country")
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md"),
                  stat_0 = "Overall<br>{n_unweighted}, {n}",
                  stat_1 = "Female<br>{n_unweighted}, {n}",
                  stat_2 = "Male<br>{n_unweighted}, {n}"
    ) %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Young Adult Gender**") %>%
    modify_footnote(all_stat_cols() ~ "Estimated by authors. Mental well-being variables are continuous and summarized as weighted mean (weighted standard error); the other variables are categorical and summarized with unweighted N (weighted %). All variables derived from either the MCS mainstage survey or the Wave 2 COVID-19 survey, administered February – March 2021 survey. Weights were provided with the COVID-19 data.") %>%
    as_gt() %>%
    tab_options(table.width = pct(80))
  
  
  table %>%
    gtsave(filename = 'A02_Table.w3_descriptives.rtf',
           path = 'Figures_Tables/')
  
  return(table)
  
}
