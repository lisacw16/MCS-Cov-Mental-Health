make_family_income_var <- function(data, 
                                   family5_der_file, 
                                   family4_der_file, 
                                   family3_der_file, 
                                   family2_der_file, 
                                   family1_der_file) {
  data_files <- c(family1_der_file,
                  family2_der_file,
                  family3_der_file,
                  family4_der_file,
                  family5_der_file)
  
  for (i in 1:length(data_files)) {
    assign(paste0("fam", i, "der"),
           load_tab_file_function(data_files[i]) %>%
             mutate(across(matches('^.*OECDUK0$'), 
                           ~x.OECDUK0_factor(.x), 
                           .names = "income")) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with("income"))
    )
  }
  
  data %>%
    mutate(W6_income = x.OECDUK0_factor(FOECDUK0),
           W6_lowincome = relevel(
             binarise_low_income(W6_income),
             ref = "No")
           ) %>%
    full_join(fam5der, by = c("MCSID")) %>%
    full_join(fam4der, by = c("MCSID")) %>%
    full_join(fam3der, by = c("MCSID")) %>%
    full_join(fam2der, by = c("MCSID")) %>%
    full_join(fam1der, by = c("MCSID"))
}

x.OECDUK0_factor <- function(var) {
    case_when(
      factor(var,
             levels = c(1, 2, 3, 4, 5, -1),
             labels = c("1", "2", "3", "4", "5", "Missing")) != "Missing" ~
        factor(var,
               levels = c(1, 2, 3, 4, 5),
               labels = c("1", "2", "3", "4", "5")))
}

fill_in_missing_income <-function(v1, v2, v3, v4, v5, v6) {
  case_when(
    v1 != "Missing" & !is.na(v1) ~ v1,
    v2 != "Missing" & !is.na(v2) ~ v2,
    v3 != "Missing" & !is.na(v3) ~ v3,
    v4 != "Missing" & !is.na(v4) ~ v4,
    v5 != "Missing" & !is.na(v5) ~ v5,
    v6 != "Missing" & !is.na(v6) ~ v6)
}

binarise_low_income <- function(var) {
  factor(var,
         levels = c(1, 2, 3, 4, 5),
         labels = c("Yes", "No", "No", "No", "No"))
}
