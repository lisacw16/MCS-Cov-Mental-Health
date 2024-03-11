descriptive_table_alone <- function(data) {
  df <- data %>%
    filter(residence == "Left parental home")
  
  df <-  survey::svydesign(ids = ~ 1, weights = ~CW1_COMBWT, data = df)
  table <- df %>% 
    tbl_svysummary(
      digits = list(all_continuous() ~ 2,
                    all_categorical() ~ c(0,1)),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n_unweighted} ({p})"),
      by = male,
      include = c(moved, 
                  no_sibling,
                  sib_bully, 
                  economic_activity,
                  partnered, 
                  married,
                  conflict,
                  region,
                  parents17,
                  Children,
                  Grandparent,
                  Friend)
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md"),
                  stat_0 = "Overall<br>(N = 175)",
                  stat_1 = "Female<br>(N = {n_unweighted})",
                  stat_2 = "Male<br>(N = {n_unweighted})"
    ) %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Young Adult Gender**") %>%
    modify_footnote(all_stat_cols() ~ "Estimated by authors. Weighted mean (weighted standard error); unweighted N (weighted %). All variables derived from either the Millennium Cohort Study mainstage survey or CLS COVID-19 Wave 1 survey. The sibling structure variables (birth order, gender composition, and number) were derived from household grid information across all waves (ages 9months to 17 years). All other variables, unless indicated in the variable name, were collected during the Covid-19 survey.",
    ) %>%
    as_gt() %>%
    tab_options(table.width = pct(80))
  
  
  table %>%
    gtsave(filename = '0X_Table_desc.alone.rtf',
           path = 'Figures_Tables/')
  return(table)}

descriptive_table_missing <- function(data) {
  df <- data %>%
    filter(!is.na(residence)) %>%
    filter(is.na(CW1_PHDE) | is.na(CW1_PHHO) | is.na(CW1_PHRF) | 
             is.na(CW1_PHEE) | is.na(CW1_PHWO) | is.na(CW1_PHNE) | 
             is.na(CW1_WEMWBS_1) | is.na(CW1_WEMWBS_2) | is.na(CW1_WEMWBS_3) |
             is.na(CW1_WEMWBS_4) | is.na(CW1_WEMWBS_5)| is.na(CW1_WEMWBS_6)| 
             is.na(CW1_WEMWBS_7))
  
  df <-  survey::svydesign(ids = ~ 1, weights = ~CW1_COMBWT, data = df)
  table <- df %>% 
    tbl_svysummary(
      digits = list(all_continuous() ~ 2,
                    all_categorical() ~ c(0,1)),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n_unweighted} ({p})"),
      by = male,
      include = c(moved, 
                  no_sibling,
                  sib_bully, 
                  economic_activity, 
                  partnered, 
                  married, 
                  W6_income,
                  fam_ses_par_ed,
                  fam_ses_overcrowded,
                  par_structure_step,
                  par_structure_single,
                  conflict, 
                  ethnicity,
                  cov_symp,
                  region,
                  parents17,
                  residence2,
                  Children,
                  Grandparent,
                  Friend,
                  CW1_HHNUM)
    ) %>%
    add_overall() %>%
    modify_header(text_interpret = c("md"),
                  stat_0 = "Overall<br>(N = {n_unweighted})",
                  stat_1 = "Female<br>(N = {n_unweighted})",
                  stat_2 = "Male<br>(N = {n_unweighted})"
    ) %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ 
                             "**Young Adult Gender**") %>%
    modify_footnote(all_stat_cols() ~ "Estimated by authors. Weighted mean (weighted standard error); unweighted N (weighted %). The sibling structure variables (birth order, gender composition, and number) were derived from household grid information across all waves (ages 9months to 17 years). All other variables, unless indicated in the variable name, were collected during the COVID-19 Wave 1 survey.",
    ) %>%
    as_gt() %>%
    tab_options(table.width = pct(80))
  
  
  table %>%
    gtsave(filename = '0X_Table_desc.na.rtf',
           path = 'Figures_Tables/')
  
  return(table)
  
}