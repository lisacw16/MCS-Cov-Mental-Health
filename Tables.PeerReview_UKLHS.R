list_lm_uklhs <- function(data) {
  data <- data %>%
    filter(cb_age > 17)
  print(table(data$cb_residence, useNA = "ifany"))
  data %>%
    select(cb_age, cb_residence, cb_sex) %>%
    filter(cb_residence != "Living with siblings, no parents") %>%
    mutate(cb_residence = droplevels(cb_residence)) %>%
    filter(cb_sex == 1) %>%
    tbl_cross(
      row = cb_age,
      col = cb_residence
    ) %>%
    as_gt()%>%
    gtsave(filename = 'PR01_Table_uklhs_xtabma.rtf',
           path = 'Figures_Tables/')# sex, 1 = men
  data %>%
    select(cb_age, cb_residence, cb_sex) %>%
    filter(cb_residence != "Living with siblings, no parents") %>%
    mutate(cb_residence = droplevels(cb_residence)) %>%
    filter(cb_sex == 2) %>%
    tbl_cross(
      row = cb_age,
      col = cb_residence
    ) %>%
    as_gt()%>%
    gtsave(filename = 'PR01_Table_uklhs_xtabfe.rtf',
           path = 'Figures_Tables/')# sex, 1 = men
  
  
  data <- data %>%
    filter(cb_residence != "Living with siblings, no parents") %>%
    mutate(cb_residence = droplevels(cb_residence)) %>%
    mutate(economic_activity = relevel(
      economic_activity, ref = "Something Else"
    ),
    male = as.factor(case_when(cb_sex == 1 ~ "Male",
                               cb_sex == 2 ~ "Female"))) 
  list(
    "Model 1" = 
      lm(cb_covid_ghq ~ cb_age + existing_ghq + cb_hadsymp + ethnicity + cb_gor_dv + cb_residence*male,
         data = data),
    "Model 2" = 
      lm(cb_covid_ghq  ~ cb_age + existing_ghq + cb_hadsymp + ethnicity + cb_gor_dv + cb_residence*male +
           cb_residence +
           economic_activity +
           par_structure_step + par_structure_single,
         data = data))
  }

summary_models_uklhs <- function(data){
  models <- list_lm_uklhs(data)
  
  cm <- c('(Intercept)' = 'Constant', #1
          "cb_residenceLeft parental home:maleMale" = "(Male) Left Parental Home", #2
          'cb_residenceLiving with parents & siblings:maleMale' = "(Male) Living with Parents & Siblings", #3
          "economic_activityFurloughed" = "Furloughed", #4
          "economic_activityIn Work" = "In Work", #5
          "par_structure_stepYes" = "Has Stepparent (Y.2019)", #6
          "par_structure_singleYes" = "Has Single Parent (Y.2019)", #7
          "existing_ghq" = "GHQ (Y.2019)", #8
          "cb_residenceLeft parental home" = "(Female) Left Parental Home", #9
          'cb_residenceLiving with parents & siblings' = "(Female) Living with Parents & Siblings", #10
          "maleMale" = "Male" #11
  )
  
  table_base <- modelsummary(models,
                        vcov = vcovHC,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        estimate = "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        gof_map = NA,
                        stars = c('+' = 0.1, '*' = .05, '**' = .01))
  
  table <- table_base %>%
    tab_row_group(
      id = "constant",
      label = "",
      rows = c(1)
    ) %>%
    tab_row_group(
      id = "ldv",
      label = "",
      rows = c(8)
    ) %>%
    tab_row_group(
      id = "fam",
      label = md("*Young Adult's Family Factors Y.2019*"),
      rows = c(6,7)
    ) %>%
    tab_row_group(
      label = md("*(Ref: Young Adult Doing Something Else May 2020)*"),
      rows = c(4,5)
    ) %>%
    tab_row_group(
      label = md("*(Ref: (Male) Living with Parents, No Siblings)*"),
      rows = c(2,3)
    ) %>%
    tab_row_group(
      label = md("*(Ref: (Female) Living with Parents, No Siblings)*"),
      rows = c(9,10)
    ) %>%
    tab_row_group(
      id = "Male",
      label = "",
      rows = c(11)
    )%>%
    tab_options(table.width = pct(90)) %>%
    cols_align(align="center",
               columns = c(2,3)) %>%
    cols_align(align="right",
               columns = c(1)) %>%
    tab_spanner(
      label = md("GHQ"),
      columns = c(2,3),
      level = 2
    ) %>%
    tab_spanner(
      label = md("Coef. < 0 ~ better mental health"),
      columns = c(2,3)
    ) %>%
    tab_source_note(source_note = md("*Notes*: Estimated using Understanding Society special COVID-19 data. Heteroskedasticity-consistent standard errors in parentheses. Significance: '+' p < 0.1, '*' p < 0.05, '**' p < 0.01. All models control for country of residence (England, Scotland, Wales, Northern Ireland), young adult age (16-24), ethnicity (non-white), and experience of COVID-19 symptoms. Analytical sample was N = 696."))
  
  table %>% gtsave(filename = 'PR02_Table_uklhs_ldvols.rtf',
                   path = 'Figures_Tables/')
  
  
  return(table)
}