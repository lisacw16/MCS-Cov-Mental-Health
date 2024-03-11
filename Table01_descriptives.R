descriptive_table <- function(df) {
  df <-  survey::svydesign(ids = ~ 1, weights = ~CW1_COMBWT, data = df)
  table <- df %>% 
    tbl_svysummary(
      digits = list(all_continuous() ~ 2,
                    all_categorical() ~ c(0,1)),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n_unweighted} ({p})"),
      missing_text = "Number Imputed",
      by = male,
      include = c(covid_K6,
                  existing_K6,
                  covid_SWEMWBS,
                  existing_SWEMWBS,
                  residence,
                  moved, 
                  economic_activity, 
                  W6_lowincome,
                  fam_ses_par_ed,
                  fam_ses_overcrowded,
                  step_7,
                  single_7,
                  conflict, 
                  ethnicity,
                  cov_symp,
                  region),
       label = list(covid_K6 ~ "May 2020 K6", 
                    existing_K6 ~ "K6 (Age 17)",
                    covid_SWEMWBS ~ "May 2020 SWEMWBS",
                    existing_SWEMWBS ~ "SWEMWBS (Age 17)",
                    residence ~ "COVID-19 Living Arrangements",
                    moved ~ "Changed Living Arrangements",
                    economic_activity ~ "Young Adult Economic Activity",
                    W6_lowincome ~ "Low Family Income (Age 14)",
                    fam_ses_par_ed ~ "High Parental Education (Age 14)",
                    fam_ses_overcrowded ~ "Overcrowded COVID-19 HH",
                    step_7 ~ 
                      "Has Stepparent (Age 17)",
                    single_7 ~ 
                      "Has Single Parent (Age 17)",
                    conflict ~ "Increased Conflict", 
                    ethnicity ~ "Ethnicity (Age 14)",
                    cov_symp ~
                      "Experienced COVID-19 Symptom(s)",
                    region ~ "Country")
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md"),
                  stat_0 = "Overall",
                  stat_1 = "Female",
                  stat_2 = "Male"
      ) %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Young Adult Gender**") %>%
    modify_footnote(all_stat_cols() ~ "Estimated by authors. Mental well-being variables are continuous and summarized as weighted mean (weighted standard error); the other variables are categorical and summarized with unweighted N (weighted %). All variables derived from either the MCS mainstage survey or COVID-19 May 2020 survey. Weights were provided with the COVID-19 May 2020 data. Missing values for May 2020 K6 (N = 395), K6 (Age 17) (N = 49), May 2020 SWEMWBS (N = 402), SWEMWBS (Age 17) (N =59), childhood family income (N = 319), and parent structure (N = 148) were imputed with multiple imputation of changed equations (m = 30) and pooling estimates.") %>%
    as_gt() %>%
    tab_options(table.width = pct(80))
  
  
  table %>%
    gtsave(filename = '01_Table_uncorrected.rtf',
         path = 'Figures_Tables/')
  
  return(table)
    
}