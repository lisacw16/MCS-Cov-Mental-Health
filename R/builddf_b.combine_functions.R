combine_functions_dfs <- function(filecov,
                                  file_cm6,
                                  file_cm5,
                                  hh7_grid, hh6_grid, hh5_grid, 
                                  hh4_grid,hh3_grid, hh2_grid,
                                  hh1_grid,
                                  fam_der7, fam_der6, fam_der5,
                                  fam_der4, fam_der3, fam_der2,
                                  fam_der1,
                                  par_der6, par_der5,
                                  cm_der7, cm_der6, cm_der5,
                                  cm_der4, cm_der3, cm_der2,
                                  cm_der1) {
  covid_data <- combine_functions_cov_covars(filecov)
  outcome_lag_vars <- outcome_lag_vars_df(covid_data, cm_der7)
  fix_region_var <- make_region_var(outcome_lag_vars, hh7_grid, fam_der6)
  family_income <- make_family_income_var(fix_region_var, fam_der5, fam_der4, fam_der3, fam_der2, fam_der1)
  family_structure <- make_family_structure_var(family_income, fam_der7)
  aged17_living <- make_age17_living_var(family_structure)
  siblings <- make_sibling_characteristics(aged17_living, file_cm6,
                               file_cm5, hh7_grid, hh6_grid, 
                               hh5_grid, hh4_grid, hh3_grid, 
                               hh2_grid, hh1_grid)
  par_education <- make_par_education_var(siblings, par_der6, par_der5) 
  make_ethnicity_var(par_education, cm_der6, cm_der5,
                                  cm_der4, cm_der3, cm_der2,
                                  cm_der1)
  }
