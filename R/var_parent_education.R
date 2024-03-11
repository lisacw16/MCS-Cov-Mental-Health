make_par_education_var <- function(data, par6_file, par5_file) {
  par6 <- load_tab_file_function(par6_file)
  par5 <- load_tab_file_function(par5_file)
  par_der6 <- par6 %>% select_highest_par_ed6()
  par_der5 <- par5 %>% select_highest_par_ed5()
  
  data <- data %>% 
    full_join(par_der5, by = c("MCSID")) %>%
    full_join(par_der6, by = c("MCSID")) %>%
    mutate(fam_ses_par_ed = high_par_ed_ifelse(p6, p5))
  print(table(data$p6,data$p5))
  return(data)
}

select_highest_par_ed6 <- function(data) {
  data %>%
    mutate(FDACAQ00 = as.numeric(case_when(
      FDACAQ00 < 6 & FDACAQ00 > 0 ~ FDACAQ00
    )))%>%
    group_by(MCSID) %>%
    summarise(p6 = max(FDACAQ00, na.rm = TRUE)) %>%
    ungroup()
}

select_highest_par_ed5 <- function(data) {
  data %>%
    mutate(EACAQ00 = as.numeric(case_when(
      (EACAQ00 < 6 & EACAQ00 > 0 ~ EACAQ00) 
    ))) %>%
    group_by(MCSID) %>%
    summarise(p5 = max(EACAQ00, na.rm = TRUE)) %>%
    ungroup()
}

high_par_ed_ifelse <- function(p6, p5) {
  as.factor(case_when(
    p6 >= 4 ~ "Yes",
    p5 >= 4 ~ "Yes",
    p6 < 4 ~ "No",
    p5 < 4 ~ "No"
  ))
}
