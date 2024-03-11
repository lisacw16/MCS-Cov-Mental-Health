# This file has been generated from the template produced by running the command targets::use_targets(). The author followed the comments available in the _targets.R template write this script. Further information about the targets package and set-up can be found in: https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline 

library(targets)
# set target options to load the packages that the scripted targets need to run in the global environment
tar_option_set(
  packages = c("srvyr", "gt", "gtsummary", "modelsummary", "haven", "Hmisc", "weights", "knitr", "tidyverse", "ggpubr", "webshot2", "lmtest", "sandwich", "RColorBrewer",'tseries', "mice"), 
  format = "rds")

# (unchanged from template) tar_make_clustermq() configuration:
options(clustermq.scheduler = "multicore")

# Configure where the R scripts with custom functions will be found:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

# The rest of this file contains a target list. This target list draws information from the loaded files and the R folder (with the custom functions). The list has been filled according to the data analysis flow (i.e., data cleaning, exploration, analysis, robustness checks):
list(
  ######### READ FILES INTO PROGRAM ########
  # Note: while you do not need to update these paths associated with the library, most UKLHS files are temporarily loaded through custom functions. You will need to update these functions to be able to assess UKLHS data. More information is provided with the UKLHS data analysis files below.
  tar_target(uklhs19_indresp_file,
             "UKLHS_data/UKDA-8644-tab/tab/mainstage_data_2019/jk_indresp_cv.tab",
             format = "file"),
  tar_target(uklhs19_egoalt_file,
             "UKLHS_data/UKDA-8644-tab/tab/mainstage_data_2019/jk_egoalt_cv.tab",
             format = "file"),
  tar_target(cov_file,
             "MCS_data/mcs_Cov/spss/spss25/covid-19_wave1_survey_cls.sav", 
             format = "file"),
  tar_target(cov2_file,
             "MCS_data/mcs_Cov/spss/spss25/covid-19_wave2_survey_cls.sav",
             format = "file"),
  tar_target(cov3_file,
             "MCS_data/mcs_Cov/spss/spss25/covid-19_wave3_survey_cls.sav",
             format = "file"),
  tar_target(hh7_grid_file,
             "MCS_data/mcs_wave7/tab/mcs7_hhgrid.tab",
             format = "file"),
  tar_target(hh6_grid_file,
             "MCS_data/mcs_wave6/tab/mcs6_hhgrid.tab",
             format = "file"),
  tar_target(hh5_grid_file,
             "MCS_data/mcs_wave5/tab/mcs5_hhgrid.tab",
             format = "file"),
  tar_target(hh4_grid_file,
             "MCS_data/mcs_wave4/tab/mcs4_hhgrid.tab",
             format = "file"),
  tar_target(hh3_grid_file,
             "MCS_data/mcs_wave3/tab/mcs3_hhgrid.tab",
             format = "file"),
  tar_target(hh2_grid_file,
             "MCS_data/mcs_wave2/tab/mcs2_hhgrid.tab", 
             format = "file"),
  tar_target(hh1_grid_file,
             "MCS_data/mcs_wave1/tab/mcs1_hhgrid.tab",
             format = "file"),
  tar_target(family7_derived_file,
             "MCS_data/mcs_wave7/tab/mcs7_family_derived.tab",
             format = "file"),
  tar_target(family6_derived_file,
             "MCS_data/mcs_wave6/tab/mcs6_family_derived.tab",
             format = "file"),
  tar_target(family5_derived_file,
             "MCS_data/mcs_wave5/tab/mcs5_family_derived.tab",
             format = "file"),
  tar_target(family4_derived_file,
             "MCS_data/mcs_wave4/tab/mcs4_family_derived.tab", 
             format = "file"),
  tar_target(family3_derived_file,
             "MCS_data/mcs_wave3/tab/mcs3_family_derived.tab", 
             format = "file"),
  tar_target(family2_derived_file,
             "MCS_data/mcs_wave2/tab/mcs2_family_derived.tab",
             format = "file"),
  tar_target(family1_derived_file,
             "MCS_data/mcs_wave1/tab/mcs1_family_derived.tab",
             format = "file"),
  tar_target(cm7_derived_file,
             "MCS_data/mcs_wave7/tab/mcs7_cm_derived.tab",
             format = "file"),
  tar_target(cm6_derived_file,
             "MCS_data/mcs_wave6/tab/mcs6_cm_derived.tab",
             format = "file"),
  tar_target(cm5_derived_file,
             "MCS_data/mcs_wave5/tab/mcs5_cm_derived.tab",
             format = "file"),
  tar_target(cm4_derived_file,
             "MCS_data/mcs_wave4/tab/mcs4_cm_derived.tab",
             format = "file"),
  tar_target(cm3_derived_file,"MCS_data/mcs_wave3/tab/mcs3_cm_derived.tab", 
             format = "file"),
  tar_target(cm2_derived_file,"MCS_data/mcs_wave2/tab/mcs2_cm_derived.tab",
             format = "file"),
  tar_target(cm1_derived_file,"MCS_data/mcs_wave1/tab/mcs1_cm_derived.tab",
             format = "file"),
  tar_target(cm4_int_file,"MCS_data/mcs_wave4/tab/mcs4_cm_interview.tab",
             format = "file"),
  tar_target(cm5_int_file,"MCS_data/mcs_wave5/tab/mcs5_cm_interview.tab",
             format = "file"),
  tar_target(cm6_int_file,"MCS_data/mcs_wave6/tab/mcs6_cm_interview.tab",
             format = "file"),
  tar_target(cm7_int_file,"MCS_data/mcs_wave7/tab/mcs7_cm_interview.tab",
             format = "file"),
  tar_target(parent5_derived_file,"MCS_data/mcs_wave5/tab/mcs5_parent_derived.tab",
             format = "file"),
  tar_target(parent6_derived_file,"MCS_data/mcs_wave6/tab/mcs6_parent_derived.tab",
             format = "file"),
  
  ########  DATASET CONSTRUCTION ########
  
  #The `combine_functions_dfs` function: 
  ## 1. cleans the data in each file, 
  ## 2. derives relevant variables, 
  ## 3. combines data files into one large working data set (named: `working_data`)
  tar_target(working_data, 
             combine_functions_dfs(cov_file,cm6_int_file,
                                   cm5_int_file,hh7_grid_file,
                                   hh6_grid_file,hh5_grid_file,
                                   hh4_grid_file,hh3_grid_file,
                                   hh2_grid_file,hh1_grid_file,
                                   family7_derived_file,
                                   family6_derived_file,
                                   family5_derived_file,
                                   family4_derived_file,
                                   family3_derived_file,
                                   family2_derived_file,
                                   family1_derived_file,
                                   parent6_derived_file,
                                   parent5_derived_file,
                                   cm7_derived_file,cm6_derived_file,
                                   cm5_derived_file,cm4_derived_file,
                                   cm3_derived_file,cm2_derived_file,
                                   cm1_derived_file)),
  # The `select_analysis_vars` function:
  # 1. selects only necessary variables for analysis
  # 2. relevels categorical variables for analysis
  tar_target(selected_df, 
             select_analysis_vars(working_data)),
  # The `allcovwaves_wide_df` function:
  # 1. constructs a wide dataset containing all waves of data
  tar_target(alloutcomes_wide, 
             allcovwaves_wide_df(cov_file,cov2_file,cov3_file,
                                 cm7_int_file,cm6_int_file,
                                 cm5_int_file,cm4_int_file,
                                 hh7_grid_file,hh6_grid_file,
                                 hh5_grid_file,hh4_grid_file,
                                 hh3_grid_file,hh2_grid_file,
                                 hh1_grid_file,
                                 family7_derived_file,
                                 family6_derived_file,
                                 family5_derived_file,
                                 family4_derived_file,
                                 family3_derived_file,
                                 family2_derived_file,
                                 family1_derived_file,
                                 parent6_derived_file,
                                 parent5_derived_file,
                                 cm7_derived_file,
                                 cm6_derived_file,
                                 cm5_derived_file,
                                 cm4_derived_file,
                                 cm3_derived_file,
                                 cm2_derived_file,
                                 cm1_derived_file)),
  # `selected_df` and `alloutcomes_wide` are going to be the core 
  # analysis datasets. 
  # The `multipleimpute_cv.w#` and `multipleimpute_sibcv.w1` functions:
  # 1. runs a multiple imputation with 30 reps
  # 2. relevels categorical variables for analysis
  tar_target(imputed.w1, 
             multipleimpute.w1_function(selected_df)),
  tar_target(imputed_sib, 
             multipleimpute_sibcv.w1(selected_df)),
  tar_target(imputed.w2,
             multipleimpute.w2_function(alloutcomes_wide)),
  tar_target(imputed.w3,
             multipleimpute.w3_function(alloutcomes_wide)),
  
  # The following functions construct multiply imputed long datasets
  tar_target(imputed_w01_long, mi_0.1_long(alloutcomes_wide)),
  tar_target(imputed_w02_long, mi_0_2_long(alloutcomes_wide)),
  tar_target(imputed_w03_long, mi_0_3_long(alloutcomes_wide)),
  
  ######## PRODUCE FIGURES AND TABLES ########
  # Produce Figure 1 in 3 steps:
  ## 1. Target `Figure_Df` summarises the `selected_df` data.frame in terms of mean and variance across observed mental health measures.
  ## 2. `Figure01_cc.K6` uses information in `Figure_Df` to produce a line graph across two time points (MCS Mainstage Wave 7 and CLS COVID Wave 1) for K6 outcomes. `Figure01_cc.WB` does the same for SWEMWBS outcomes. 
  ## 3. Combine the two figures by hand in Microsoft Word to make Figure 1
  
  tar_target(Figure_Df, 
             figure_df.function(selected_df)),
  tar_target(Figure01_cc.K6,
             Figure1_function.K6(Figure_Df, k6.se, k6.n)),
  tar_target(Figure01_cc.WB,
             Figure1_function.WB(Figure_Df, wb.se, wb.n)),
  
  # Produce Descriptive Tables
  ## Target 'Table01_cc.descriptives' provides complete-case descriptives of:
  ### 1. overall sample N weighted and unweighted (with and without siblings)
  ### 2. missing observations counted and labelled "number imputed". 
  ## The authors manually edit and format the table in Microsoft Word.
  tar_target(Table01_w1.cc.desc, 
             descriptive_table(selected_df)),
  ## Table A02 presents two separate tables, similar to Table 1, side-by-side. The authors generate each table separately and then merge them together by hand using Microsoft Word. 
  tar_target(TableA2_w2.cc.desc, 
             TableA02_w2_descriptives
             (alloutcomes_wide)),
  tar_target(TableA2_w3.cc.desc, 
             TableA02_w3_descriptives
             (alloutcomes_wide)),
  ## !!! In the descriptive tables just produced above, variables with missing observations must be corrected with imputed values. The functions that corrects imputed summary statistics are called `descriptives_mi.cv.w#`, where the `#` is replaced with the wave number (wave = {1:3}).
  tar_target(Table01_w1.mi.desc,
             descriptives_mi.cv.w1(imputed.w1)),
  tar_target(TableA01_w2.mi.desc,
             descriptives_mi.cv.w2(imputed.w2)),
  tar_target(TableA01_w3.mi.desc,
             descriptives_mi.cv.w3(imputed.w3)),
  # Extra Descriptive Tables
  ## We also create some descriptives to look at patterns of those living away from the parental home or missing outcome responses (these are verbally reported in the manuscript)
  tar_target(descriptives_alone, 
             descriptive_table_alone(selected_df)),
  tar_target(descriptives_na, 
             descriptive_table_missing(selected_df)),
  
  ## The crosstab provides information about the sample of MCS members who participated in all three waves. The code generates two files, each with a crosstab of W1 x W(2-3). These two files must be combined by hand using a word processor. In the manuscript tables, the authors also included  two rows of N via summing each column by hand.
  tar_target(TableA01_crosstab,
             crosstab_table(alloutcomes_wide)),
  
  # Lagged Dependent Variable OLS regressions
  tar_target(Table02_mi,
             modelsummary_w1mi(imputed.w1)),
  tar_target(Table03_mi,
             modelsummary_sib(imputed_sib)),
  tar_target(Table04_mi,
             modelsummary_3w(imputed.w1,
                             imputed.w2,
                             imputed.w3)),
  
  # Fixed effects estimation, t = 0:1
  ## 0 = MCS Mainstage Survey Wave 7
  ## 1 = CLS COVID-19 Survey Wave 1
  tar_target(femodel_TableA03_mi,
             fixedmodels_1(imputed_w01_long)),
  tar_target(TableA03_mi, 
             modelsummary_femodels(
               femodel_TableA03_mi, 
               "A03_Table-fe01")),
  # The `cov_response_df` function combines the COVID waves without filtering for primary cohort members. Instead, both primary and secondary cohort members are allowed to remain to gauge the participation against that provided in the CLS COVID survey user manual page 11 (cls.ucl.ac.uk/wp-content/uploads/2017/02/UCL-Cohorts-COVID-19-Survey-user-guide.pdf).
  tar_target(mcs.cv.response_check, 
             cov_response_df(cov_file,
                             cov2_file,
                             cov3_file)),
  
  #####  PEER REVIEW TABLES #####  
  # For peer review response we considered:
  ## 1. The heterogeneity of effect across income quintiles via a crosstab of MCS COVID-19 Survey Wave 1 family living arrangements by age 14 income quintile. We then estimate our main model stratified by income quintile (high {4:5} vs low {1:3}). 
  ## 2. Fixed effect estimators on all available data (each wave considers observations from wave 7 (t=0) and the respective CLS COVID-19 survey wave)
  ## 3. UKLHS data, producing a sample cross tabulation and an approximation of our main analysis
  
  tar_target(inc_res_xtab, xtab.inc_res(imputed.w1)),
  tar_target(inc_res_ols, ols.inc_res(selected_df)),
  
  tar_target(femodel_PeerReview_mi,
             fixedmodels_0_3(imputed_w01_long, 
                             imputed_w02_long,
                             imputed_w03_long)),
  tar_target(TablePeerReview_mi, 
             modelsummary_femodels(
               femodel_PeerReview_mi,
               "PR_Table-fe010203")),
  tar_target(uklhs_dataset,
             covid_uklhs(uklhs19_indresp_file, 
                         uklhs19_egoalt_file)),
  tar_target(uklhs_model, 
             summary_models_uklhs(uklhs_dataset))
)
