outcome_lag_vars_df <- function(covid_data, file) {
  w7_data <- load_w7_filter_cm(file)
  w7_data <- cm7_derived_outcome(w7_data)
  covid_data <- covid_outcome(covid_data)
  w7_data %>%
    full_join(covid_data, by = c("MCSID"))
}

cm7_derived_outcome <- function(data) {
  data %>%
    mutate(existing_K6 = clean_var(GDCKESSL),
           existing_SWEMWBS = clean_var(GDWEMWBS),
           high_existing_K6 = 
             high_distress_ifelse(existing_K6),
           log_existing_K6 = log_k6_calculation(existing_K6),
           low_existing_SWEMWBS = low_wellbeing_ifelse(existing_SWEMWBS)) 
}

high_distress_ifelse <- function(var) {
  as.factor(case_when(
    var >= 10 ~ "Yes",
    var >=0 ~ "No"
  ))
}

low_wellbeing_ifelse <- function(var) {
  as.factor(case_when(
    var <= 19.5 &
      var > 0 ~ "Yes",
    var > 19.5  ~ "No"
  ))
}

log_k6_calculation <- function(var) {
  case_when(var > -1 ~
              log((1+var)))
}

covid_outcome <- function(data) {
  data %>%
    make_covid_wb_var() %>%
    make_covid_k6_var() %>%
    mutate(CW1_WEMWBS_1 = clean_var(CW1_WEMWBS_1),
           CW1_WEMWBS_2 = clean_var(CW1_WEMWBS_2),
           CW1_WEMWBS_3 = clean_var(CW1_WEMWBS_3), 
           CW1_WEMWBS_4 = clean_var(CW1_WEMWBS_4), 
           CW1_WEMWBS_5 = clean_var(CW1_WEMWBS_5), 
           CW1_WEMWBS_6 = clean_var(CW1_WEMWBS_6), 
           CW1_WEMWBS_7 = clean_var(CW1_WEMWBS_7),
           high_covid_K6 = high_distress_ifelse(covid_K6),
           log_covid_K6 = log_k6_calculation(covid_K6),
           low_covid_SWEMWBS = low_wellbeing_ifelse(covid_SWEMWBS))
}

make_covid_wb_var <- function(data) {
  data %>% 
    mutate(covid_SWEMWBS = 
             wb_metric_conversion(
               add_wb_var(
                 CW1_WEMWBS_1,
                 CW1_WEMWBS_2, 
                 CW1_WEMWBS_3, 
                 CW1_WEMWBS_4, 
                 CW1_WEMWBS_5, 
                 CW1_WEMWBS_6, 
                 CW1_WEMWBS_7)
               )
           )
}

make_covid_k6_var <- function(data) {
  data %>%
    mutate(CW1_PHDE = translate_k6_var(CW1_PHDE),
           CW1_PHHO = translate_k6_var(CW1_PHHO),
           CW1_PHRF = translate_k6_var(CW1_PHRF),
           CW1_PHEE = translate_k6_var(CW1_PHEE),
           CW1_PHWO = translate_k6_var(CW1_PHWO),
           CW1_PHNE = translate_k6_var(CW1_PHNE),
           covid_K6 = add_k6_var(
             CW1_PHDE, 
             CW1_PHHO, 
             CW1_PHRF,
             CW1_PHEE,
             CW1_PHWO,
             CW1_PHNE)
           )
}

translate_k6_var <- function(var) {
  var <- clean_var(var)
  abs(var - 5)
}

add_k6_var <- function(var1, var2, var3, var4, var5, var6) {
  var1 + var2 + var3 + var4 + var5 + var6
}

clean_var <- function(x) {
  case_when(x >= 0 ~ as.numeric(x))
}

add_wb_var <- function(v1, v2, v3, v4, v5, v6, v7) {
  clean_var(v1) + clean_var(v2) + clean_var(v3) + clean_var(v4) + clean_var(v5) + clean_var(v6) + clean_var(v7)
}

wb_metric_conversion <- function(summed_var) {
  case_match(summed_var,
    7 ~ 7.00, 
    8 ~ 9.51, 
    9 ~ 11.25, 
    10 ~ 12.40, 
    11 ~ 13.33, 
    12 ~ 14.08, 
    13 ~ 14.75, 
    14 ~ 15.32, 
    15 ~ 15.84, 
    16 ~ 16.36, 
    17 ~ 16.88, 
    18 ~ 17.43,
    19 ~ 17.98,
    20 ~ 18.59,
    21 ~ 19.25,
    22 ~ 19.98, 
    23 ~ 20.73,
    24 ~ 21.54,
    25 ~ 22.35,
    26 ~ 23.21,
    27 ~ 24.11, 
    28 ~ 25.03, 
    29 ~ 26.02, 
    30 ~ 27.03, 
    31 ~ 28.13, 
    32 ~ 29.31, 
    33 ~ 30.70, 
    34 ~ 32.55, 
    35 ~ 35.00)
}
