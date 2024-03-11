make_region_var <- function(cov_data, hh7_grid, family6_der) {
  hh7_grid_filtered <- load_w7_filter_cm(hh7_grid)
  hh7_grid_filtered <-
    hh7_grid_filtered %>%
    mutate(W7_REGION = x.ACTRY00_factor(GACTRY00)) %>%
    select(MCSID, W7_REGION, GACTRY00)
  family6_derived <- load_tab_file_function(family6_der)
  family6_derived <- 
    family6_derived %>%
    mutate(W6_REGION = x.ACTRY00_factor(FACTRY00))
  cov_data <- cov_data %>% 	    
    mutate(CW1_REGION = factor(
      CW1_REGION,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, -8, -1), 
      labels = c("England", "England", "England", "England", "England", "England", "England", "England", "England", "Wales", "Scotland", "NI", "Missing", "Missing")))
  cov_data %>%
    full_join(hh7_grid_filtered, by = c("MCSID")) %>%
    full_join(family6_derived, by = c("MCSID")) %>%
   mutate(region = 
            region_ifesle(CW1_REGION, W7_REGION, W6_REGION))
}

x.ACTRY00_factor <- function(var) {
  factor(var, 
         levels = c(1, 2, 3, 4, -1),
         labels = c("England", "Wales", "Scotland", "NI", "Missing"))
}

region_ifesle <-function(v1, v2, v3) {
  case_when(
    v1 != "Missing" & !is.na(v1) ~ v1,
    v2 != "Missing" & !is.na(v2) ~ v2,
    v3 != "Missing" & !is.na(v3) ~ v3
  )
}