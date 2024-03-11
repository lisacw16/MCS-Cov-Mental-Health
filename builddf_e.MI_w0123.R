# MI to build wide datasets
multipleimpute.w1_function <- function(data){
  data <- data %>%
    filter(residence != "Living with siblings, no parents") %>%
    mutate(residence = droplevels(residence)) %>%
    select(-c(transition,residence2,
              flag_5qs,
              existing_residence,partnered,
              Children,Grandparent,Friend,
              CW1_CNUM00,
              CW1_HHNUMWH_6,
              CW1_HHNUM,
              CW1_HHNUMWH_3,
              CW1_COVPER,
              GDOTHS00,
              parents17,
              siblings17,
              W4_income,
              W3_income,
              W2_income,
              W1_income,
              par_structure_single,
              par_structure_step,
              no_sibling,num_sibling,same_sib_gender,
              sib_bully,sib_bully6,
              is_oldest_sib))
  
  print(str(data))
  pmis <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(pmis[pmis > 0], 
             decreasing = TRUE))
  
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("CW1_COMBWT")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("covid_K6",
         "covid_SWEMWBS",
         "existing_K6",
         "existing_SWEMWBS")] <- "norm"
  meth[c("moved",
         "W6_lowincome",
         "fam_ses_overcrowded",
         "step_7",
         "single_7",
         "fam_ses_par_ed",
         "conflict",
         "ethnicity",
         "cov_symp")] <- "logreg"
  meth[c("residence",
         "economic_activity",
         "region")] <- "polyreg"
  print(imp$meth)
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE) %>%
    select(-c(starts_with("CW1_WEMWBS"),
              starts_with("CW1_PH")))
}
multipleimpute.w2_function <- function(wide_data){
  data <- wide_data %>%
    filter(!is.na(CW2_COMBWT) & 
             CW2_residence != 
             "Living with siblings, no parents") %>%
    mutate(CW2_residence = droplevels(CW2_residence),
           CW2_Country = droplevels(CW2_Country)) %>%
    select(-c(ends_with("mobility"),
              starts_with("CW1"),
              starts_with("CW3"),
              CW0_Country,
              CW0_Economic.Activity,
              CW0_residence,
              conflict,
              starts_with("Country"),
              starts_with("W1_"),
              starts_with("W2_"),
              starts_with("W3_"),
              starts_with("W4_"),
              W5_steppar,
              ends_with("par_edu"),
              par_structure_single,
              par_structure_step)) 
  print(head(data))
  print(str(data))
  pmis <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(pmis[pmis > 0], decreasing = TRUE))
  
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("CW2_COMBWT")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("CW2_Covid_K6",
         "existing_K6",
         "CW2_Covid_SWEMWBS",
         "existing_SWEMWBS")] <- "norm"
  meth[c("W6_lowincome",
         "CW2_Overcrowded",
         "CW2_moved",
         "W5_single_par",
         "W6_single_par",
         "W6_steppar",
         "W7_steppar",
         "W7_single_par",
         "fam_ses_par_ed",
         "CW2_cov_symp",
         "Ethnicity",
         "Male")] <- "logreg"
  meth[c("CW2_residence",
         "CW2_Economic.Activity",
         "CW2_Country",
         "W6_income",
         "W5_income")] <- "polyreg"
  print(imp$meth)
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE)
}
multipleimpute.w3_function <- function(wide_data){
  data <- wide_data %>%
    filter(!is.na(CW3_COMBWT) & 
             CW3_residence != 
             "Living with siblings, no parents") %>%
    mutate(CW3_residence = droplevels(CW3_residence),
           CW3_Country = droplevels(CW3_Country)) %>%
    select(-c(ends_with("mobility"),
              starts_with("CW1"),
              starts_with("CW2"),
              CW0_Country,
              CW0_Economic.Activity,
              CW0_residence,
              conflict,
              starts_with("Country"),
              starts_with("W1_"),
              starts_with("W2_"),
              starts_with("W3_"),
              starts_with("W4_"),
              W5_steppar,
              ends_with("par_edu"),
              par_structure_single,
              par_structure_step,
              CW3_PHOURSCHAN)) 
  
  print(str(data))
  pmis <- unlist(
    lapply(data, 
           function(x) sum(is.na(x))))/nrow(data)
  print(sort(pmis[pmis > 0], decreasing = TRUE))
  
  imp <- mice::mice(data, maxit=0)
  meth <-  imp$method
  predM <- imp$predictorMatrix
  predM[, c("CW3_COMBWT")] <- 0
  predM[, c("MCSID")] <- 0
  meth[c("CW3_Covid_K6",
         "existing_K6",
         "CW3_Covid_SWEMWBS",
         "existing_SWEMWBS")] <- "norm"
  meth[c("W6_lowincome",
         "CW3_Overcrowded",
         "W5_single_par",
         "W6_single_par",
         "W6_steppar",
         "W7_steppar",
         "W7_single_par",
         "fam_ses_par_ed",
         "CW3_cov_symp",
         "CW3_moved",
         "Ethnicity",
         "Male")] <- "logreg"
  meth[c("CW3_residence",
         "CW3_Economic.Activity",
         "CW3_Country",
         "W6_income",
         "W5_income")] <- "polyreg"
  
  imp2 <- mice::mice(data, m = 30, maxit = 20,
                     method = meth,
                     predictorMatrix = predM,
                     print =  FALSE,
                     seed = 5000)
  
  mice::complete(imp2, action="long", include = TRUE)
}

