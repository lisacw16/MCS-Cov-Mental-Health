allcovwaves_wide_df <- function(cov1,cov2,cov3, file_cm7,
                          file_cm6,file_cm5,file_cm4,
                          hh7_grid,hh6_grid,hh5_grid,hh4_grid,
                          hh3_grid,hh2_grid,hh1_grid,
                          fam_der7,fam_der6,fam_der5,fam_der4,
                          fam_der3,fam_der2,fam_der1,
                          par_der6, par_der5,
                          cm_der7,cm_der6,cm_der5,cm_der4,cm_der3,
                          cm_der2,cm_der1) {
  pooled.cov.lag_df <- covid.pooled.waves(cov1,cov2,
                                          cov3,cm_der7)
  
  region_fix_df <- preceding.region.vars(hh7_grid,
                                         fam_der6,
                                         fam_der5,
                                         fam_der4)
  
  family_income <- family.income.var(fam_der6,
                                     fam_der5,
                                     fam_der4,
                                     fam_der3,
                                     fam_der2,
                                     fam_der1) %>%
    mutate(W6_lowincome =relevel(binarise_low_income(W6_income),
      ref = "No"))
  
  family_structure <- family.structure.var(fam_der7,
                                           fam_der6,
                                           fam_der5,
                                           fam_der4,
                                           fam_der3,
                                           fam_der2,
                                           fam_der1)
  
  par_education <- family.education.var(par_der6,
                                        par_der5)
  
  ethnicity <- ethnicity.var(cm_der6,
                             cm_der5,
                             cm_der4,
                             cm_der3,
                             cm_der2,
                             cm_der1)
  
  sex <- preceding.sex.var(file_cm6,
                           file_cm4)
  
  w7_residence <- CW0.residence(fam_der7)
  
  w7_econ.act <- CW0.economic.activity(file_cm7)
  
  left_join(pooled.cov.lag_df, 
            region_fix_df,
            by = c("MCSID")) %>%
    left_join(family_income, 
              by = c("MCSID")) %>%
    left_join(family_structure, 
              by = c("MCSID")) %>%
    left_join(par_education, 
              by = c("MCSID")) %>%
    mutate(fam_ses_par_ed = 
             replace(fam_ses_par_ed, 
                     is.na(fam_ses_par_ed), 
                     "No")) %>%
    left_join(ethnicity, by = c("MCSID")) %>%
    left_join(sex, by = c("MCSID")) %>%
    mutate(Male = relevel(as.factor(case_when(
      !is.na(Male) ~ Male,
      !is.na(CW1_Male) ~ CW1_Male,
      !is.na(CW2_Male) ~ CW2_Male,
      !is.na(CW3_Male) ~ CW3_Male)), 
      ref = "Female")) %>%
    select(-c(CW1_Male,CW2_Male,CW3_Male)) %>%
    left_join(w7_residence, by = c("MCSID")) %>%
    left_join(w7_econ.act, by = c("MCSID")) %>%
    mutate(CW0_Country = 
             as.factor(case_when(
               Country_7 != "Missing" ~ Country_7,
               Country_6 != "Missing" ~ Country_6,
               Country_5 != "Missing" ~ Country_5,
               Country_4 != "Missing" ~ Country_4,
               .default = "unknown")),
           CW1_Country = 
             as.factor(case_when(
               CW1_Country != "Missing" ~ CW1_Country,
               Country_7 != "Missing" ~ Country_7,
               Country_6 != "Missing" ~ Country_6,
               Country_5 != "Missing" ~ Country_5,
               Country_4 != "Missing" ~ Country_4,
               .default = "unknown"
             )),
           CW2_Country = 
             as.factor(case_when(
               CW2_Country != "Missing" ~ CW2_Country,
               CW1_Country != "unknown" ~ CW1_Country,
               Country_7 != "Missing" ~ Country_7,
               Country_6 != "Missing" ~ Country_6,
               Country_5 != "Missing" ~ Country_5,
               Country_4 != "Missing" ~ Country_4,
               .default = "unknown")),
           CW3_Country = 
             as.factor(case_when(
               CW3_Country != "Missing" ~ CW3_Country,
               CW2_Country != "unknown" ~ CW2_Country,
               CW1_Country != "unknown" ~ CW1_Country,
               Country_7 != "Missing" ~ Country_7,
               Country_6 != "Missing" ~ Country_6,
               Country_5 != "Missing" ~ Country_5,
               Country_4 != "Missing" ~ Country_4,
               .default = "unknown"))
    ) %>%
    filter(!is.na(CW1_COMBWT) | !is.na(CW2_COMBWT) | !is.na(CW3_COMBWT)) %>%
    mutate(W7_single_par = droplevels(replace(W7_single_par, 
                                   W7_single_par == "Missing", NA))) %>%
    mutate(W7_steppar = droplevels(replace(W7_steppar, 
                                W7_steppar == "Missing", NA)))
  }

CW0.residence <- function(data){
  load_tab_file_function(data) %>%
    mutate(
      GDHTYP00 = as.numeric(GDHTYP00),
      parents17 = as.factor(case_when(
        GDHTYP00 < 0 ~ 0,
        GDHTYP00 == 11 ~ 0,
        GDHTYP00 == 20 ~ 0,
        GDHTYP00 >= 23 & GDHTYP00 <= 24 ~ 0,
        GDHTYP00 >= 1 & GDHTYP00 <= 10 ~ 1,
        GDHTYP00 >= 12 ~ 1)),
      siblings17 = case_when(
        GDOTHS00 >= 0 ~ as.numeric(GDOTHS00),
        .default = 0
      )) %>%
    mutate(CW0_residence = as.factor(case_when(
      parents17 == 0 & siblings17 <= 0 ~ "Left parental home",
      parents17 == 1 & siblings17 <= 0 ~ "Living with parents, no siblings",
      parents17 == 1 & siblings17 > 0 ~ "Living with parents & siblings",
      parents17 == 0 & siblings17 > 0 ~ "Living with siblings, no parents"
    )),
    CW0_residence = relevel(CW0_residence, 
                            ref = "Living with parents, no siblings")
    ) %>%
    select(MCSID, CW0_residence)
}

CW0.economic.activity <- function(data){
  load_tab_file_function(data) %>%
    filter(if_any(contains('CNUM00'), is_yes)) %>%
    mutate(
      inschool = case_when(
        GCEDUC00 == 1 ~ 1,
        GCAPNT00 == 1 ~ 1,
        GCTRIN00 == 1 ~ 1,
        GCPRTM00 == 1 ~ 1,
        .default = 0),
      inwork = case_when(
        GCPRTM00 == 1 ~ 0,
        GCJOBS00 == 1 ~ 1,
        .default = 0),
      CW0_Economic.Activity = as.factor(case_when(
        GCMNAC00 == 2 ~ "Employed",
        GCMNAC00 == 1 | 
          (GCMNAC00 >= 3 & GCMNAC00 <= 6) ~ 
          "In Training/Edu",
        inschool == 1 ~ "In Training/Edu",
        inwork == 1 ~ "Employed",
        GCHMFM00 == 1 ~ "Other Inactive",
        .default = "Other Inactive"))
      ) %>%
    select(MCSID, CW0_Economic.Activity)
}

covid.pooled.waves <- function(cov1, cov2, cov3, cm_der7) {
# This function has two purposes, to clean the COVID variables across every wave, and to combine the variables constructed from mainstage survey waves.

  data_files <- c(cov1, cov2, cov3)
  cm_der7 <- load_w7_filter_cm(cm_der7) %>%
    cm7_derived_outcome()
  
# The following for loop has two parts/functions. The outer body of the code determines the name of the output (i.e., cov_wave#). The inner code loops through each wave and cleans each variable with the appropriate naming convention. The for loop take a list of data files that are unloaded.

  for(i in 1:length(data_files)) {
    assign(paste0("cov_wave", i),
           load_sav_file_function(data_files[i]) %>%
             filter(if_any(contains('CNUM00'), is_yes)) %>%
             mutate(across(matches('^.*_(PHDE|PHHO|PHRF|PHEE|PHWO|PHNE)$'),
                           ~translate_k6_var(.x))) %>%
             mutate(Covid_K6 = rowSums(select(., c(ends_with(
               c("PHDE","PHHO","PHRF","PHEE","PHWO","PHNE"))
               )),na.rm = FALSE)) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("Covid_K6")) %>%
             
             mutate(across(matches('^.*(_WEMWBS_[1-8]|HHNUMWH_[1-10])$'),
                           ~clean_var(.x))) %>%
             mutate(Covid_SWEMWBS = wb_metric_conversion(
               rowSums(select(., c(contains('_WEMWBS_'))), 
                       na.rm = FALSE))) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("Covid_SWEMWBS")) %>%
             
             mutate(residence = 
                      as.factor(
                         case_when(
                           if_all(matches('^.*_HHNUMWH_3'), is_yes) & 
                             if_all(matches('^.*_HHNUMWH_6'), is_no) ~
                             "Living with parents, no siblings",
                           if_all(matches('^.*_HHNUMWH_[3|6]'), is_yes) ~
                             "Living with parents & siblings",
                           if_all(matches('^.*_HHNUMWH_[3|6]'), is_no) ~ 
                             "Left parental home",
                           if_all(matches('^.*_HHNUMWH_3'), is_no) & 
                             if_all(matches('^.*_HHNUMWH_6'), is_yes) ~
                             "Living with siblings, no parents")),
                    residence = 
                      relevel(residence,
                              ref = "Living with parents, no siblings")) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("residence")) %>%
             
             mutate(across(matches('^.*_ECONACTIVITYD$'),
                           ~factor.econ(.x),
                           .names = "Economic.Activity"))  %>%
             
             mutate(across(matches('^.*_COVIDSYMPT_23$'),
                    ~make.cov.symp.var(.x),
                    .names = "cov_symp")) %>%
             mutate(cov_symp = replace(cov_symp, is.na(cov_symp), "No")) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("cov_symp")) %>%
             
             mutate(across(matches('^.*_HHNUMWH_4$'), 
                           ~is_gpar(.x),
                           .names = "Gpar")) %>%
             
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("Gpar")) %>%
             mutate(mobility = as.factor(
               case_when(
                 if_all(matches("^.*_COURSERET$"), is_yes) ~ 
                   "Returning to Course",
                 if_all(matches('^.*EDUOFFERINTENT$'), is_EDUOFFERINTENT) ~
                   "Returning to Course",
                 if_all(matches('^.*WRKLOCATIOND'), is_WRKLOCATIOND) ~
                   "Working in Person",
                 .default = "Unspecified"))) %>%
             mutate(Economic.Activity = relevel(
               as.factor(case_when(
                 Economic.Activity != "MISSING" 
                 & !is.na(Economic.Activity) ~ 
                   Economic.Activity,
                 mobility == "Returning to Course" ~ 
                   "In Training/Edu",
                 .default = "Other Inactive")),
               ref = "In Training/Edu")) %>%
             rename_with(~paste0("CW", i, "_", .x, 
                                 recycle0 = TRUE),
                         starts_with("mobility")) %>%
             rename_with(~paste0("CW", i, "_", .x, 
                                 recycle0 = TRUE),
                         starts_with("Economic.Activity")) %>%
             
             mutate(across(matches('^.*HHNUM$'), 
                           ~clean_var(.x), 
                           .names = "num_hh")) %>%
             mutate(across(matches('^.*NUMROOMS$'), 
                           ~clean_var(.x), 
                           .names = "num_rooms")) %>%
             mutate(Overcrowded = is_hh_overcrowded(
               overcrowded_arithmatic(num_hh, num_rooms))) %>%
             select(-num_hh, -num_rooms) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("Overcrowded")) %>%
             
             mutate(across(matches('^.*REGION$'), 
                           ~factor.region(.x), 
                           .names = "Country")) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("Country")) %>%
             
             mutate(across(matches('^.*COVCHAN$'),
                           ~factor.moved(.x),
                           .names = "moved")) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("moved")) %>%
          
             mutate(across(matches('^.*PSEX$'),
                           ~make.sex(.x),
                           .names = "Male")) %>%
             rename_with(~paste0("CW", i, "_", .x, recycle0 = TRUE),
                         starts_with("Male"))
           )
  }

# Using the naming convention for the for loop output files, we join the files together to create an output dataframe that can either be stored in the working memory (via calling the function in targets) or called on in another function to create a working dataset.
  
  full_join(cm_der7, cov_wave1, by = c("MCSID")) %>%
    full_join(cov_wave2, by = c("MCSID")) %>%
    full_join(cov_wave3, by = c("MCSID")) %>%
    mutate(conflict = 
             factor(CW1_CVDCHNG_4, 
                    levels = c(1,2,3,-8,-1),
                    labels = c("Yes","No","No","No","No"))) %>%
    select_variables_cov.lag() %>%
    mutate(CW2_moved = as.factor(
      case_when(
        !is.na(CW2_moved) ~ CW2_moved,
        !is.na(CW1_moved) ~ CW1_moved
      )),
      CW3_moved = as.factor(
        case_when(
          !is.na(CW3_moved) ~ CW3_moved,
          !is.na(CW2_moved) ~ CW2_moved,
          !is.na(CW1_moved) ~ CW1_moved
        )
      )
      )
    
}

# Respondent Individual Information (Country, Ethnicity, and Sex)
preceding.region.vars <- function(hh7_grid,fam_der6,fam_der5,fam_der4) {
  data_files <- c(fam_der4,fam_der5,fam_der6,hh7_grid)
  
  for (i in 1:length(data_files)) {
    assign(paste0("wave", i+3),
           load_tab_file_function(data_files[i]) %>%
             filter(if_any(contains('CNUM00'), is_yes)) %>%
             mutate(across(matches('^.*ACTRY00$'), 
                           ~x.ACTRY00_factor(.x), 
                           .names = "Country")) %>%
             rename_with(~paste0(.x, "_", i+3, recycle0 = TRUE),
                         starts_with("Country")) %>%
             select(c(MCSID, contains("Country")))
           )
  }
  
  wave7 %>%
    full_join(wave6, by = c("MCSID")) %>%
    full_join(wave5, by = c("MCSID")) %>%
    full_join(wave4, by = c("MCSID")) %>%
     select(c(MCSID,
              Country_4,
              Country_5,
              Country_6,
              Country_7))
}

preceding.sex.var <- function(file_cm6,file_cm4){
  cm4int <- load_w4_filter_cm(file_cm4)
  load_w6_filter_cm(file_cm6) %>%
    full_join(cm4int, by = c("MCSID")) %>%
    mutate(W6_Male = make.sex(FCCSEX00),
           W4_Male = make.sex(DCCSEX00),
           Male = as.factor(case_when(
             !is.na(W6_Male) ~ W6_Male,
             !is.na(W4_Male) ~ W4_Male
           ))
    ) %>%
    select(MCSID, Male)
}

ethnicity.var <- function(cm_der6,
                          cm_der5,
                          cm_der4,
                          cm_der3,
                          cm_der2,
                          cm_der1) {
  data_files <- c(cm_der1,cm_der2,cm_der3,cm_der4, cm_der5,cm_der6)
  
  for (i in 1:length(data_files)) {
    assign(paste0("cm", i, "der"),
           load_tab_file_function(data_files[i]) %>%
             filter(if_any(contains('CNUM00'), is_yes)) %>%
             mutate(across(matches('^.*(C06E00|CE0600)$'),
                           ~clean.eth(.x),
                           .names = 'ethnicity')) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with('ethnicity'))
           )
  }
  cm1der %>%
    full_join(cm2der, by = c("MCSID")) %>%
    full_join(cm3der, by = c("MCSID")) %>%
    full_join(cm4der, by = c("MCSID")) %>%
    full_join(cm5der, by = c("MCSID")) %>%
    full_join(cm6der, by = c("MCSID")) %>%
    find.eth() %>%
    select(MCSID, Ethnicity)
}

# Respondent Family Factors (Income, Single, Step)
family.income.var <- function(fam_der6,
                              fam_der5,
                              fam_der4,
                              fam_der3,
                              fam_der2,
                              fam_der1) {
  data_files <- c(fam_der1,fam_der2,fam_der3,fam_der4,fam_der5,fam_der6)
  
  for (i in 1:length(data_files)) {
    assign(paste0("fam", i, "der"),
           load_tab_file_function(data_files[i]) %>%
             mutate(across(matches('^.*OECDUK0$'), 
                           ~x.OECDUK0_factor(.x), 
                           .names = "income")) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with("income")) %>%
             
             mutate(across(matches('^.*HTYS00$'), 
                           ~x.DHTYS00_factor(.x), 
                           .names = "single_par")) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with("single_par")) %>%
             
             mutate(across(matches('^.*HTYP00$'), 
                           ~step_conversion(.x), 
                           .names = "steppar")) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with("steppar"))
    )
  }
   fam6der %>%
    full_join(fam5der, by = c("MCSID")) %>%
    full_join(fam4der, by = c("MCSID")) %>%
    full_join(fam3der, by = c("MCSID")) %>%
    full_join(fam2der, by = c("MCSID")) %>%
    full_join(fam1der, by = c("MCSID")) %>%
    select(MCSID,
           W6_income,
           W5_income)
}

family.structure.var <- function(fam_der7,
                                 fam_der6,
                                 fam_der5,
                                 fam_der4,
                                 fam_der3,
                                 fam_der2,
                                 fam_der1) {
  
  data_files <- c(fam_der1,
                  fam_der2,
                  fam_der3,
                  fam_der4,
                  fam_der5,
                  fam_der6,
                  fam_der7)
  
  for (i in 1:length(data_files)) {
    assign(paste0("fam", i, "der"),
           load_tab_file_function(data_files[i]) %>%
             mutate(across(matches('^.*HTYS00$'), 
                           ~x.DHTYS00_factor(.x), 
                           .names = "single_par")) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with("single_par")) %>%
             
             mutate(across(matches('^.*HTYP00$'), 
                           ~step_conversion(.x), 
                           .names = "steppar")) %>%
             rename_with(~paste0("W", i, "_", .x, recycle0 = TRUE),
                         starts_with("steppar")) %>%
             
             select(c(MCSID, contains("single_par"), contains("steppar")))
    )
  }
  fam7der %>%
    full_join(fam6der, by = c("MCSID")) %>%
    full_join(fam5der, by = c("MCSID")) %>%
    full_join(fam4der, by = c("MCSID")) %>%
    full_join(fam3der, by = c("MCSID")) %>%
    full_join(fam2der, by = c("MCSID")) %>%
    full_join(fam1der, by = c("MCSID")) %>%
    mutate(par_structure_single = 
             single_par_structure_ifelse(
               W7_single_par,
               W6_single_par,
               W5_single_par,
               W4_single_par,
               W3_single_par,
               W2_single_par,
               W1_single_par),
           par_structure_step = 
             step_par_structure_ifelse(
               W7_steppar, W6_steppar,
               W5_steppar,W4_steppar,
               W3_steppar,W2_steppar)) %>%
    select(c(MCSID, 
             par_structure_single,
             par_structure_step,
             W7_steppar,
             W7_single_par,
             contains("steppar"),
             contains("single_par"))) %>%
    mutate(W5_single_par = 
             droplevels(
               replace(W5_single_par, 
                       W5_single_par == "Missing",
                       NA)),
           W6_single_par = 
             droplevels(
               replace(W6_single_par,
                       W6_single_par == "Missing",
                       NA)),
           W6_steppar = 
             droplevels(
               replace(W6_single_par,
                       W6_single_par == "Missing",
                       NA)))
}

family.education.var <- function(par_der6, par_der5) {
  data_files <- c(par_der5, par_der6)
  for (i in 1:length(data_files)) {
    assign(paste0("par", i+4, "der"),
           load_tab_file_function(data_files[i]) %>%
             mutate(across(matches('^.*ACAQ00$'), 
                           ~clean.edu(.x))) %>%
             group_by(MCSID) %>%
             summarise(across(matches('^.*ACAQ00$'), 
                              ~max(.x, na.rm = TRUE),
                              .names = "par_edu")) %>%
             ungroup() %>%
             rename_with(~paste0("W", i+4, "_", .x, recycle0 = TRUE),
                         starts_with("par_edu"))
             )
  }
  par5der %>%
    full_join(par6der, by = c("MCSID")) %>%
    mutate(fam_ses_par_ed = 
             high_par_ed_ifelse(W6_par_edu, W5_par_edu)) %>%
    select(MCSID, contains("par_ed"))
}

# Parent Education Sub-Function
clean.edu <- function(x){
  as.numeric(case_when(x < 6 & x > 0 ~ x,
                       .default = 0))
}
# Ethnicity and Sex Sub-Functions
clean.eth <- function(x){
  factor(x,
         levels = c(-9,-8,-1,1,2,3,4,5,6),
         labels = c("Missing",
                    "Missing",
                    "Missing",
                    "White",
                    "Non-white",
                    "Non-white",
                    "Non-white",
                    "Non-white",
                    "Non-white"))
}

find.eth <- function(data){
  data %>%
    mutate(Ethnicity = droplevels(as.factor(case_when(
      !is.na(W1_ethnicity) & 
        W1_ethnicity != "Missing" ~ W1_ethnicity,
      !is.na(W2_ethnicity) & 
        W2_ethnicity != "Missing" ~ W2_ethnicity,
      !is.na(W3_ethnicity) & 
        W3_ethnicity != "Missing" ~ W3_ethnicity,
      !is.na(W4_ethnicity) & 
        W4_ethnicity != "Missing" ~ W4_ethnicity,
      !is.na(W5_ethnicity) & 
        W5_ethnicity != "Missing" ~ W5_ethnicity,
      !is.na(W6_ethnicity) & 
        W6_ethnicity != "Missing" ~ W6_ethnicity
    ))))
}

# Covid Waves For-Loop Sub-Function

factor.moved <- function(x){
  factor(x, 
         level = c(-9, -8, -1, 1, 2),
         label = c("No","No","No","Yes", "No"))
}

factor.region <- function(x){
  factor(x, 
         levels = c(1,2,3,4,5,6,7,8,9,10,11,12,-8,-1),
         labels = c("England",
                    "England",
                    "England",
                    "England",
                    "England",
                    "England",
                    "England",
                    "England",
                    "England",
                    "Wales",
                    "Scotland",
                    "NI",
                    "Missing",
                    "Missing"))
}

is_gpar <- function(x) {
  factor(x, 
         levels = c(1,2,-8,-1),
         labels = c("Yes","No","No","No"))
}

is_yes <- function(x) {
  x == 1
}

is_no <- function(x) {
  x != 1
}

make.sex <- function(x){
  factor(x, 
         level = c(1, 2),
         label = c("Male", "Female"))
}

factor.econ <-  function(x) {
  relevel(factor(x, 
         levels = c(-8,-1,1,2,3,4,5,6,
                  7,8,9,10,11,12,13),
         labels = 
           c("MISSING","MISSING", 
           "Employed","Furloughed", 
           "Other Inactive", 
           "In Training/Edu", 
           "Other Inactive","Employed", 
           "Furloughed","Unemployed",
           "Other Inactive", 
           "Other Inactive", 
           "In Training/Edu", 
           "Other Inactive", 
           "MISSING")), ref = "In Training/Edu")
}

make.cov.symp.var <- function(x){
  factor(x, 
         levels = c(-8, -1, 1, 2),
         labels = c("No", "No", "No", "Yes"))
}

select_variables_cov.lag <- function(data) {
  select(data, c(
    MCSID, existing_K6, existing_SWEMWBS,
    conflict,
    starts_with("CW1_WEMWBS"),
    starts_with("CW1_PH"),
    starts_with("CW2_WEMWBS"),
    starts_with("CW2_PH"),
    starts_with("CW3_WEMWBS"),
    starts_with("CW3_PH"),
    ends_with("Covid_SWEMWBS"),
    ends_with("Covid_K6"),
    contains('residence'),
    contains('cov_wave'),
    
    ends_with("Economic.Activity"),
    ends_with("Overcrowded"),
    ends_with("Country"),
    ends_with("moved"),
    ends_with("mobility"),
    ends_with("cov_symp"),
    ends_with("_Male"),
    ends_with("COMBWT")
  ))
}

is_EDUOFFERINTENT <- function(x){
  x>= 1 & x <=2
}

is_WRKLOCATIOND <- function(x){
  x>= 2 & x <=3
}
