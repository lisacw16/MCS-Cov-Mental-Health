# This file is designed to re-run the main analysis with different data. This means: 

# Load the files: You can find the Understanding Society COVID-19 dataset, Study Number 8644 here (https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8644). 

# University of Essex, Institute for Social and Economic Research. (2021). Understanding Society: COVID-19 Study, 2020-2021. [data collection]. 11th Edition. UK Data Service. SN: 8644, DOI: 10.5255/UKDA-SN-8644-11

# UKLHS collected data on siblings and wellbeing starting in MAY, and then monthly/bi monthly for 9 waves

## Small Functions
NAtozero <- function(.x) {
  ifelse(is.na(.x), 0, .x)
}

negtoNA <- function(.x){
  ifelse(.x<0, NA, .x)
}

yes.no <- function(.x){
  ifelse(.x == 1, "Yes",
         ifelse(.x == 2, "No", NA)
         )
}

is.sibling <- function(x){
  x == 5
}

isnot.sibling <- function(x) {
  x != 5
}

is.parent <- function(x){
  x == 4
}

isnot.parent <- function(x){
  x != 4
}

wave_to_number <- function(x){
  recode(x, 
         'ca' = 1,
         'cb' = 2,
         'cc' = 3,
         'cd' = 4,
         'ce' = 5,
         'cf' = 6,
         'cg' = 7,
         'ch' = 8,
         'ci' = 9)
}

uklhs_covid.function <- function(.x){
  case_when(
    .x == 1 ~ "Yes",
    .x == 2 ~ "No",
    .x < 0 ~ NA
  )
}

uklhs_country.function <- function(.x){
  case_when(
    .x >= 1 & .x <=9 ~ "England",
    .x == 10 ~ "Wales",
    .x == 11 ~ "Scotland",
    .x == 12 ~ "NI"
  )
}

uklhs_ethnicity.b.function <- function(.x){
  case_when(
    .x >= 1 & .x <=4 ~ "White",
    .x >= 5 & .x <=8 ~ "Mixed",
    .x >= 9 & .x <=13 ~ "Asian",
    .x >= 14 & .x <=16 ~ "Black",
    .x >= 17 ~ "Other"
  )
}

uklhs_ethnicity.function <- function(.x){
  as.factor(case_when(
    .x >= 1 & .x <=4 ~ "White",
    .x >= 5 ~ "Non-White"
  ))
}

DOB.function <- function(.age){
  .age >= 18 & .age <= 24
}

uklhs_is.sibling <- function(x){
  x >= 10 & x <= 12
}

uklhs_is.parent <- function(x){
  x >= 13 & x <= 15
}

uklhs_is.stepparent <- function(x){
  x == 25
}

uklhs_par.ed <- function(x){
  case_when(
    x >= 1 & x <= 12 ~ 1,
    .default = 0
  )
}

uklhs_semp <- function(x){
  as.factor(case_when(
    x >= 1 & x <= 3 ~ 1,
    .default = 0
  ))
}

## Large Functions

# *uklhs19_indresp_child* is a function that 
  # (using the jk_indresp file from 2019)
  # 1. derives pre-COVID child variables 
  # [lagged ghq, ethnicity, country]
uklhs19_indresp_child <- function(uklhs_indresp_file){
  load_tab_file_function(uklhs_indresp_file) %>%
    mutate(existing_ghq =
             negtoNA(jk_scghq1_dv),
           existing_ghq = 
             case_when(existing_ghq < 36 ~ existing_ghq),
           ethnicity = 
             uklhs_ethnicity.function(jk_ethn_dv),
           ethnicity.b = 
             uklhs_ethnicity.b.function(jk_ethn_dv),
           existing_country = uklhs_country.function(jk_gor_dv)) %>%
    filter(jk_birthy >= 1994 & jk_birthy <= 2004) %>%
    filter(!is.na(existing_ghq)) %>%
    select(
      pidp,
      existing_ghq,
      ethnicity,
      ethnicity.b,
      existing_country
    )
}

# *uklhs19_hhresp_function* is (ideally) a function that derives the existing household income immediately preceding COVID-19. 
# However, the income information for this data release does not contain imputed values for household member income, therefore does not have any variable approximating household income. These can be found in Waves 10 and 11 from the mainstage data release and are not included in this analysis.

uklhs19_egoalt_function <- function(uklhs_egoalt_file, child){
  egoalt <- load_tab_file_function(uklhs_egoalt_file) %>%
    mutate(flag_sib = uklhs_is.sibling(jk_rel_dv),
           flag_par = uklhs_is.parent(jk_rel_dv),
           flag_steppar = uklhs_is.stepparent(jk_rel_dv))
  
  child <- child %>% select(pidp)
  
  sibling <-  egoalt %>%
    filter(flag_sib == TRUE) %>%
    group_by(pidp) %>%
    summarise(nsiblings = n())%>%
    ungroup() %>%
    right_join(child, by = c("pidp")) %>%
    mutate(nsiblings = ifelse(is.na(nsiblings), 0, nsiblings))
    
  parents <- egoalt %>%
    filter(flag_par == TRUE) %>%
    group_by(pidp) %>%
    summarise(nparents = n())%>%
    ungroup() %>%
    right_join(child, by = c("pidp")) %>%
    mutate(nparents = ifelse(is.na(nparents), 
                             0, nparents),
           par_structure_single = as.factor(
             ifelse(nparents == 1, "Yes", "No")))
  
  stepparents <- egoalt %>%
    filter(flag_steppar == TRUE) %>%
    group_by(pidp) %>%
    summarise(nstepparents = n())%>%
    ungroup() %>%
    right_join(child, by = c("pidp")) %>%
    mutate(nstepparents = ifelse(is.na(nstepparents), 0, nstepparents),
           par_structure_step = as.factor(
      ifelse(nstepparents > 0, "Yes", "No")))
  
  sibling %>%
    left_join(parents, by = c("pidp")) %>%
    left_join(stepparents, by = c("pidp")) %>%
    mutate(
      existing_residence = as.factor(
        case_when(
          nparents == 0 & nsiblings == 0 ~ 
            "Left Parental Home",
          nparents > 0 & nsiblings > 0 ~ 
            "Living with parents & siblings",
          nparents > 0 & nsiblings == 0 ~ 
            "Living with parents, and no siblings",
          nparents == 0 & nsiblings > 0 ~ 
            "Living with siblings, no parents")
           )) %>%
    select(
      pidp,
      existing_residence,
      par_structure_step,
      par_structure_single,
      nsiblings,
      nparents
    )
}

# *pre_cov_uklhs* is a function that combines 
# 1. the pre-COVID child, 
# 2. parent, 
# 3. hh,
# 4. egoalt, 
# 5. and family structure datasets
pre_cov_uklhs <- function(indresp_file, egoalt_file){
  indresp_child <-  uklhs19_indresp_child(indresp_file)
  # indresp_par <- uklhs19_indresp_par(egoalt_file, indresp_child, indresp_file)
  hh_structure <- uklhs19_egoalt_function(egoalt_file, indresp_child)
   indresp_child %>%
     left_join(hh_structure, by = c("pidp"))
}

# *covid_uklhs* is a function that
# 1. Loads COVID indresp files waves 2-9 (web-survey only)
# 2. Derives COVID variables
# 3. Connects Pre-COVID information with COVID information 
covid_uklhs <- function(file1,file2){
  
  pre_cov_factors <- pre_cov_uklhs(file1,file2)

  indresp_w_files <- list.files(
    "/Users/lisachristine/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/02_Projects/CovidYAMH/analysis_CovidYAMH/UKLHS_data/UKDA-8644-tab/tab",
    pattern = "^.*c[b-i]_indresp_w.tab$")
  
  for (i in 1:length(indresp_w_files)){
    assign(paste0("covid_", "w", i+1),
           read.table(paste0(
             "/Users/lisachristine/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/02_Projects/CovidYAMH/analysis_CovidYAMH/UKLHS_data/UKDA-8644-tab/tab/",
             indresp_w_files[i]), 
             header = TRUE, fill = TRUE, 
             quote = "", sep = "\t") %>%
             mutate(across(matches('^.*gor_dv$'), # country var
                           ~uklhs_country.function(.x))) %>%
             mutate(across(matches('^.*child1$'), # child var
                           ~yes.no(.x))) %>%
             mutate(across(matches('^.*scghq1_dv$'), # ghq var
                           ~negtoNA(.x))) %>%
             mutate(across(matches("^.*ff_furlough$"), # furloughed
                           ~negtoNA(.x))) %>%
             mutate(across(matches("^.*ff_furlough$"), # furloughed
                           ~NAtozero(.x),
                           .names = "covfurloughed")) %>%
             mutate(across(matches("^.*ff_sempderived$"),
                           ~uklhs_semp(.x),
                           .names = "covemp")) %>%
             mutate(
               residence = relevel(as.factor(case_when(
                 if_any(matches('^.*relation[a-n]'), is.sibling) &
                   if_any(matches('^.*relation[a-n]'), is.parent)
                 ~ "Living with parents & siblings",
                 if_all(matches('^.*relation[a-n]'), isnot.sibling) &
                   if_any(matches('^.*relation[a-n]'), is.parent)
                 ~ "Living with parents, no siblings",
                 if_all(matches('^.*relation[a-n]'), isnot.sibling) &
                   if_all(matches('^.*relation[a-n]'), isnot.parent)
                 ~ "Left parental home",
                 if_any(matches('^.*relation[a-n]'), is.sibling) &
                   if_all(matches('^.*relation[a-n]'), isnot.parent)
                 ~ "Living with siblings, no parents")), 
                 ref = "Living with parents, no siblings")) %>%
             rename_with(~paste0(
               "c", letters[i+1], "_", .x, recycle0 = TRUE),
               starts_with("residence")) %>%
             rename_with(~paste0(
               "c", letters[i+1], "_", "covid_ghq", recycle0 = TRUE),
               ends_with("scghq1_dv")) %>%
             rename_with(~paste0(
               "c", letters[i+1], "_", .x, recycle0 = TRUE),
               ends_with("covfurloughed")) %>%
             rename_with(~paste0(
               "c", letters[i+1], "_", .x, recycle0 = TRUE),
               ends_with("covemp")) %>%
             select(c(
               pidp, # Person Identifier
               ends_with("sex"), # Sex
               ends_with("age"), # Derived age
               ends_with("covid_ghq"), # Derived GHQ
               ends_with("residence"), # Parent and Sib Residence
               ends_with("covfurloughed"), #Furloughed
               ends_with("covemp"), #Emp
               
               ends_with("child1"), # Has school age children
               ends_with("gor_dv"), # Country
               ends_with("_hadsymp") #COVID Symptoms
             ))
    )}
  data <- pre_cov_factors %>%
    left_join(covid_w2, by = c("pidp")) %>%
    mutate(
      economic_activity = as.factor(case_when(
        cb_covfurloughed == 1 ~ "Furloughed",
        cb_covemp == 1 ~ "In Work",
        .default = "Something Else"
        )))%>%
    left_join(covid_w3, by = c("pidp")) %>%
    left_join(covid_w4, by = c("pidp")) %>%
    left_join(covid_w5, by = c("pidp")) %>%
    left_join(covid_w6, by = c("pidp")) %>%
    left_join(covid_w7, by = c("pidp")) %>%
    left_join(covid_w8, by = c("pidp")) %>%
    left_join(covid_w9, by = c("pidp")) %>%
    filter(cb_age >= 16 & cb_age <= 24) 
  
  print(table(data$existing_ghq, useNA = "ifany"))
  print(table(data$cb_covid_ghq, useNA = "ifany"))
  
  data <- data %>%
    filter(!is.na(existing_ghq) & !is.na(cb_covid_ghq))
  
  return(data)
  
}






