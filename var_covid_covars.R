combine_functions_cov_covars <- function(file) {
  covid_data <- load_cov_file_function(file)
  covid_data %>%
    make_overcrowded_var() %>%
    make_cov_symp_var() %>%
    make_sex_var() %>%
    make_partnered_var() %>%
    make_moved_var() %>%
    make_gpar_var() %>%
    make_uni_var() %>%
    make_married_var() %>%
    make_conflict_var() %>%
    make_residence_var() %>%
    make_transition_var() %>%
    make_economic_var() %>%
    fix_weights()
}

fix_weights <- function(data) {
  data %>%
    mutate(CW1_COMBWT = as.numeric(CW1_COMBWT))
}
  
make_sex_var <- function(data) {
  data %>%
    mutate(
      male = factor(CW1_PSEX, 
                    level = c(1, 2),
                    label = c("Male", "Female")))
}

make_cov_symp_var <- function(data) {
  data %>%
    mutate(cov_symp = 
             factor(CW1_COVIDSYMPT_23, 
                    levels = c(-8, -1, 1, 2),
                    labels = c("No", "No", "No", "Yes")))
}

make_moved_var <- function(data) {
  data %>%
    mutate(
      moved = factor(CW1_COVCHAN,
                     levels = c(-8,-1,1,2),
                     labels = c("No", "No", "Yes","No")))
}
make_gpar_var <- function(data) {
  data %>%
    mutate(par_structure_gpar = 
             factor(CW1_HHNUMWH_4, 
                    levels = c(1,2,-8,-1),
                    labels = c("Yes","No","No","No")))
}
make_partnered_var <- function(data) {
  data %>%
    mutate(
      partnered = factor(CW1_OTHRELA,
        levels = c(-8, -1, 1, 2),
        labels = c("No", "No", "Yes", "No")))
}

make_uni_var <- function(data) {
  data %>%
    mutate(
      pre_cov_uni = factor(CW1_STUDYORG, 
                           levels = c(1,2,3,4,-8,-1),
                           labels = c("No","No","Yes","No","No","No")),
      end_learn = factor(CW1_LEARNACTIVITYCHNG, 
                            levels = c(1,2,3,4,5,6,-8,-1),
                            labels = c("No","Yes","No","No","Yes","No", "", "")), 
      drop_out_uni = case_when(
        pre_cov_uni == "Yes" & end_learn == "Yes" ~ "Yes",
        pre_cov_uni == "No" ~ "No"),
      continue_learning = factor(CW1_LEARNACTIVITYCHNG, 
                                  levels = c(1,2,3,4,5,6,-8,-1),
                                  labels = c("Yes","No","Yes","Yes","No","No","", "")),
      continue_uni = case_when(
        pre_cov_uni == "Yes" & continue_learning == "Yes" ~ "Yes",
        pre_cov_uni == "No" ~ "No")
      )
      
}
make_married_var <- function(data) {
  data %>%
    mutate(married = 
             married_ifelse(
               CW1_HHNUMWH_1, 
               CW1_COVPART))
}
married_ifelse <- function(var1, var2) {
  var1 <- as.numeric(var1)
  var2 <- as.numeric(var2)
  as.factor(case_when(var1 == 1 | var2 == 1 ~"Yes",
            var1 != 1 & var2 != 1 ~"No"))
}

make_residence_var <- function(data){
  data %>%
    mutate(
      residence = residence_ifelse(
        CW1_HHNUMWH_3, 
        CW1_HHNUMWH_6),
      residence_a = as.factor(
        residence_ifelse2(
          CW1_HHNUMWH_3, 
          CW1_HHNUMWH_6)))
}
residence_ifelse <- function(par, sib){
  par <- as.numeric(par)
  sib <- as.numeric(sib)
  as.factor(case_when(par == 1 & sib != 1 ~
              "Living with parents, no siblings",
            par == 1  & sib == 1 ~
              "Living with parents & siblings",
            par != 1 & sib != 1 ~ 
              "Left parental home",
            par != 1 & sib == 1 ~
              "Living with siblings, no parents"
            ))
}

residence_ifelse2 <- function(par, sib){
  par <- as.numeric(par)
  sib <- as.numeric(sib)
  case_when(par == 1 & sib == 1 ~
              "Living with parents & siblings",
            par == 1 & sib != 1 ~
              "Living with parents, no siblings",
            par != 1 & sib != 1 ~ 
              "Left parental home", 
            par != 1 & sib == 1 ~
              "Living with siblings, no parents")
}

make_transition_var <- function(data){
  data %>%
    mutate(transition = as.factor(
      transition_ifelse(
        CW1_HHNUMWH_3, 
        CW1_HHNUMWH_6, 
        CW1_COVPER,
        CW1_COVCHAN)
    ))
}
transition_ifelse <- function(par, sib, par_move, move){
  par <- as.numeric(par)
  sib <- as.numeric(sib)
  par_move <- as.numeric(par_move)
  move <- as.numeric(move)
  case_when(((par == 1 & move == 1)) & sib == 1 ~
              "Moved in with parents & siblings",
            ((par == 1 & move == 1)) & sib != 1 ~
              "Moved in with parents, no siblings",
            par == 1 & move != 1 & sib == 1 ~ 
              "Stayed with parents & siblings",
            par == 1 & move != 1 & sib != 1 ~
              "Stayed with parents, no siblings",
            par != 1 & sib != 1 ~ 
              "Left parental home")
}

make_economic_var <- function(data){
  data %>%
    mutate(economic_activity = economic_ifelse(
      CW1_ECONACTIVITYD,
      CW1_ECONACTIVITYDEDU,
      continue_uni,
      drop_out_uni))
}

factor_econ <-  function(x) {
  factor(x, 
         levels = c(-8,-1,1,2,3,4,5,6,
                    7,8,9,10,11,12,13),
         labels = 
           c("Missing","Missing", 
             "Employed","Furloughed", 
             "Other Inactive", 
             "In Training/Edu", 
             "Other Inactive","Employed", 
             "Furloughed","Unemployed",
             "Other Inactive", 
             "Other Inactive", 
             "In Training/Edu", 
             "Other Inactive", 
             "Missing"))
}

economic_ifelse <- function(econ, econ_learn, uni_cont, drop_uni) {
  econ <- factor_econ(econ)
  
  econ_learn <- factor(econ_learn,
                       levels = c(-8,-1,1,2,3,4,5,
                                  6,7,8,9,10),
                       labels = 
                         c("Missing","Missing",
                           "Employed","Employed", 
                           "Other Inactive", 
                           "In Training/Edu",
                           "Unemployed","Other Inactive",
                           "Other Inactive","In Ed",
                           "Other Inactive","Missing"))
  
  as.factor(case_when(
    uni_cont == "Yes" ~ "In University", 
    econ == "Employed" ~ "Employed",
    econ == "Furloughed" ~ "Furloughed",
    econ == "Other Inactive" ~ "Other Inactive",
    econ == "Unemployed" ~ "Unemployed",
    econ == "In Training/Edu" ~ "In Training/Edu",
    econ == "In Ed" ~ "In Training/Edu",
    econ_learn == "Employed" ~ "Employed",
    econ_learn == "Furloughed" ~ "Furloughed",
    econ_learn == "Other Inactive" ~ "Other Inactive",
    econ_learn == "Unemployed" ~ "Unemployed",
    econ_learn == "In Training/Edu" ~ "In Training/Edu",
    econ_learn == "In Ed" ~ "In Training/Edu",
    drop_uni == "Yes" ~ "Unemployed",
    econ == "Missing" | is.na(econ) ~ "Other Inactive"))
}

make_conflict_var <- function(data) {
  data %>%
    mutate(conflict = 
             factor(CW1_CVDCHNG_4, 
                    levels = c(1,2,3,-8,-1),
                    labels = c("Yes","No","No","No","No")))
}

make_overcrowded_var <- function(data) {
  data %>%
    mutate(fam_ses_overcrowded =
             is_hh_overcrowded(
               overcrowded_arithmatic(
                 convert_to_numeric(CW1_HHNUM),
                 convert_to_numeric(CW1_NUMROOMS))))
}
overcrowded_arithmatic <- function(people, rooms) {
  case_when(!is.na(people) & !is.na(rooms) ~ (people/rooms))
}
is_hh_overcrowded <- function(var) {
  as.factor(case_when(var >= 1 ~ "Yes",
            var < 1 | is.na(var) ~ "No"))
}