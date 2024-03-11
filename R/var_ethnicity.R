make_ethnicity_var <- function(data, cm6_der_file, cm5_der_file, cm4_der_file, cm3_der_file, cm2_der_file, cm1_der_file) {
  cm6_der <- load_w6_filter_cm(cm6_der_file)
  cm5_der <- load_w5_filter_cm(cm5_der_file)
  cm4_der <- load_w4_filter_cm(cm4_der_file)
  cm3_der <- load_w3_filter_cm(cm3_der_file)
  cm2_der <- load_w2_filter_cm(cm2_der_file)
  cm1_der <- load_w1_filter_cm(cm1_der_file)
  
  data %>%
    full_join(cm6_der, by = c("MCSID")) %>%
    full_join(cm5_der, by = c("MCSID")) %>%
    full_join(cm4_der, by = c("MCSID")) %>%
    full_join(cm3_der, by = c("MCSID")) %>%
    full_join(cm2_der, by = c("MCSID")) %>%
    full_join(cm1_der, by = c("MCSID")) %>%
    mutate(ethnicity = 
             binarise_ethnicity(
               fill_in_missing_ethnicity(FDCE0600,
                                         EDCE0600,
                                         DDC06E00,
                                         CDC06E00,
                                         BDC06E00,
                                         ADC06E00)
               )
           )
}

fill_in_missing_ethnicity <- function(v1, v2, v3, v4, v5, v6) {
  case_when(
    v1 > 0  & !is.na(v1) ~ v1,
    v2 > 0  & !is.na(v2) ~ v2,
    v3 > 0  & !is.na(v3) ~ v3,
    v4 > 0  & !is.na(v4) ~ v4,
    v5 > 0  & !is.na(v5) ~ v5,
    v6 > 0  & !is.na(v6) ~ v6
  )
}

binarise_ethnicity <- function(var) {
  as.factor(case_when(
    var > 1 ~ "Not White",
    is.na(var) | var <= 1 ~ "White"
  ))
}
