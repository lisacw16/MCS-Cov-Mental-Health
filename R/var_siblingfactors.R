make_sibling_characteristics <- function(data, 
                                         cm6_file, 
                                         cm5_file, 
                                         h7, h6, h5, 
                                         h4, h3, h2, 
                                         h1) {
    bullying5 <- sib_bullying5(cm5_file)
    bullying6 <- sib_bullying6(cm6_file)
    data_files <- c(h2,h3,h4,h5,h6,h7)
    
    for(i in 1:length(data_files)) {
      assign(paste0("hhgrid", i+1),
             load_tab_file_function(data_files[i])) }
  
  hhgrid7 <-
    hhgrid7 %>%
      filter(GCNUM00 == 1 | GHCREL00 >= 11 & GHCREL00 < 15) %>%
      mutate(sex = as.numeric(case_when(GHPSEX00 > 0 ~ GHPSEX00,
                                        GHCSEX00 > 0 ~ GHCSEX00)),
             cm_sex = as.numeric(GHCSEX00),
             age = as.numeric(ifelse(
               GHPAGE00 < 90 & GHPAGE00 >= 0, GHPAGE00, 0)),
             cm_age = as.numeric(GHCAGE00))
  hhgrid6 <- hhgrid6 %>%
    filter(FCNUM00 == 1 | FHCREL00 >= 11 & FHCREL00 < 15) %>%
     mutate(sex = as.numeric(case_when(FHPSEX00 > 0 ~ FHPSEX00,
                                       FHCSEX00 > 0 ~ FHCSEX00)),
            cm_sex = as.numeric(FHCSEX00),
            age = as.numeric(ifelse(
              FHPAGE00 < 90 & FHPAGE00 >= 0, FHPAGE00, 0)),
            cm_age = as.numeric(FHCAGE00))
   hhgrid5 <- hhgrid5 %>%
     filter(ECNUM00 == 1 | ECREL0000 >= 11 & ECREL0000 < 15) %>%
     mutate(sex = as.numeric(case_when(EPSEX0000 > 0 ~ EPSEX0000,
                                       ECSEX0000 > 0 ~ ECSEX0000)),
            cm_sex = as.numeric(ECSEX0000),
            age = as.numeric(ifelse(
              EPAGE0000 < 90 & EPAGE0000 >= 0, EPAGE0000, 0)),
            cm_age = as.numeric(ECAGE0000))
  hhgrid4 <- hhgrid4 %>%
    filter(DCNUM00 == 1 | DHCREL00 >= 11 & DHCREL00 < 15) %>%
     mutate(sex = as.numeric(case_when(DHPSEX00 > 0 ~ DHPSEX00,
                                       DHCSEX00 > 0 ~ DHCSEX00)),
            cm_sex = as.numeric(DHCSEX00),
            age = as.numeric(ifelse(
              DHPAGE00 < 90 & DHPAGE00 >= 0, DHPAGE00, 0)),
            cm_age = as.numeric(DHCAGE00))
   hhgrid3 <- hhgrid3 %>%
     filter(CCNUM00 == 1 | CHCREL00 >= 11 & CHCREL00 < 15) %>%
     mutate(sex = (case_when(CHPSEX00 > 0 ~ CHPSEX00,
                                       CHCSEX00 > 0 ~ CHCSEX00)),
            cm_sex = as.numeric(CHCSEX00),
            age = as.numeric(ifelse(
              CHPAGE00 < 90 & CHPAGE00 >= 0, CHPAGE00, 0)),
            cm_age = as.numeric(CHCAGE00))
  hhgrid2 <- hhgrid2 %>%
    filter(BCNUM00 == 1 | BHCREL00 >= 11 & BHCREL00 < 15) %>%
    mutate(sex = as.numeric(case_when(BHCSEX00 > 0 ~ BHCSEX00,
                                      BHPSEX00 > 0 ~ BHPSEX00)),
           cm_sex = as.numeric(BHCSEX00),
           age = as.numeric(ifelse(
             BHPAGE00 < 90 & BHPAGE00 >= 0, BHPAGE00, 0)),
           cm_age = as.numeric(BHCAGE00))
  
  ss_sibs7 <- hhgrid7 %>%
    ss_bo_sibs() 
   ss_sibs6 <- hhgrid6 %>%
     ss_bo_sibs()
   ss_sibs5 <- hhgrid5 %>%
     ss_bo_sibs()
   ss_sibs4 <- hhgrid4 %>%
     ss_bo_sibs() 
   ss_sibs3 <- hhgrid3 %>%
     ss_bo_sibs() 
  ss_sibs2 <- hhgrid2 %>%
    ss_bo_sibs()
  
   data %>%
     full_join(ss_sibs7, by = c("MCSID")) %>%
     full_join(ss_sibs6, by = c("MCSID"),
               suffix = c("7", "6")) %>%
     full_join(ss_sibs5,
               by = c("MCSID")) %>%
     full_join(ss_sibs4,
               by = c("MCSID"),
               suffix = c("5", "4")) %>%
     full_join(ss_sibs3,
               by = c("MCSID")) %>%
     full_join(ss_sibs2,
               by = c("MCSID"),
               suffix = c("3", "2")) %>%
     full_join(bullying5, 
               by = c("MCSID")) %>%
     full_join(bullying6, 
               by = c("MCSID")) %>%
     mutate(same_sib_gender = 
              ss_ifelse(same_sib_gender7, same_sib_gender6,
                        same_sib_gender5, same_sib_gender4,
                        same_sib_gender3, same_sib_gender2),
            is_oldest_sib = 
              ss_ifelse(is_oldest_sib7, is_oldest_sib6,
                        is_oldest_sib5, is_oldest_sib4,
                        is_oldest_sib3, is_oldest_sib2),
            sib_bully = 
              join_bully_vars(sib_bully6, sib_bully5),
            sib_bully = 
              as.factor(ifelse(
                is.na(sib_bully),
                "No",
                sib_bully)),
            num_sibling = 
              num_siblings_var(CW1_HHNUMWH_6,
                               sib_bully,
                               ADOTHS00,
                               BDOTHS00,
                               CDOTHS00,
                               DDOTHS00,
                               EOTHS00,
                               FDOTHS00,
                               GDOTHS00),
            no_sibling = 
              no_siblings_var(num_sibling))
  }
#number (num) of siblings
num_siblings_var <- function(v1, v2, 
                             h1, h2, 
                             h3, h4, 
                             h5, h6, 
                             h7) {
  as.numeric(case_when(
    h5 > 0 ~ 
      as.numeric(h5),
    h6 > 0 ~ 
      as.numeric(h6),
    h7 > 0 ~ 
      as.numeric(h7),
    (v1 != 1 & v2 != "Yes" & h5 == 0 & h6 == 0 & h7 == 0) ~ 
      as.numeric(0),
    h4 > 0 ~ 
      as.numeric(h4),
    h3 > 0 ~ 
      as.numeric(h3),
    h2 > 0 ~ 
      as.numeric(h2),
    h1 > 0 ~ 
      as.numeric(h1),
    (v1 != 1 & v2 != "Yes") & 
      (h4 == 0 & h3 == 0 & h2 == 0 & h1 == 0) ~ 0,
    (v1 == 1 | v2 == "Yes") ~ 1,
    (v1 != 1 & v2 != "Yes") ~ 0)
    )
}

# number of siblings across all waves (categorical)
no_siblings_var <- function(v1) {
  as.factor(case_when(v1 == 0 ~ "No Siblings",
                      v1 == 1 ~ "One Sibling",
                      v1 >= 2 ~ "Two (+) Siblings"))
}

is.sib <- function(x){
  x >= 11 & x < 15
}

is.sex <- function(x){
  case_when(x > 0) ~ as.numeric(x)
}

is.age <- function(x){
  case_when(x < 90 & x >= 0) ~ x
}

# has at least one same sex sibling or is the oldest sibling
ss_bo_sibs <- function(data) {
  data %>%
    group_by(MCSID) %>%
    summarise(ss_male = sum(sex == 1, na.rm=FALSE),
              ss_female = sum(sex == 2, na.rm=FALSE),
              cm_male = sum(cm_sex == 1, na.rm=FALSE),
              cm_female = sum(cm_sex == 2, na.rm=FALSE),
              oldest_sib = as.numeric(max(age, na.rm=TRUE)),
              cm_age = as.numeric(max(cm_age, na.rm=TRUE))
              ) %>%
    ungroup() %>%
    mutate(same_sib_gender = 
             as.factor(
               ifelse(
                 (ss_male > 1 & cm_male == 1) |
                   (ss_female > 1 & cm_female == 1), 
                 "Yes", "No")
               ),
           is_oldest_sib = 
             as.factor(ifelse(
               cm_age > oldest_sib, "Yes", "No")))
}

ss_ifelse <- function(w7, w6, w5, w4, w3, w2) {
  as.factor(case_when(
    w7 == "Yes" ~ w7,
    w7 == "No" ~ w7,
    w6 == "Yes" ~ w6,
    w6 == "No" ~ w6,
    w5 == "Yes" ~ w5,
    w5 == "No" ~ w5,
    w4 == "Yes" ~ w4,
    w4 == "No" ~ w4,
    w3 == "Yes" ~ w3,
    w3 == "No" ~ w3,
    w2 == "Yes" ~ w2,
    w2 == "No" ~ w2))
}

# sibling bullying wave 5 and 6
sib_bullying5 <- function(wave5_cm_file) {
  wave5 <- load_tab_file_function(wave5_cm_file) 
  wave5 %>%
    filter(ECNUM00 == 1) %>%
    mutate(ECQ54X00 = bully_var(ECQ54X00),
           ECQ55X00 = bully_var(ECQ55X00),
           sib_bully5 = sib_bully_ifelse(ECQ54X00, ECQ55X00))
}

sib_bullying6 <- function(wave6_cm_file) {
  wave6 <- load_tab_file_function(wave6_cm_file) 
  wave6 %>%
    filter(FCNUM00 == 1) %>%
    mutate(FCBULB00 = bully_var(FCBULB00),
           FCBULP00 = bully_var(FCBULP00),
           sib_bully6 = as.factor(sib_bully_ifelse(FCBULB00, FCBULP00)))
  }
sib_bully_ifelse <- function(bully1, bully2) {
  case_when(
    bully1 == "Yes" | bully2 == "Yes" ~ "Yes", 
    is.na(bully1) & is.na(bully2) ~ "No",
    bully1 != "Yes" | bully2 != "Yes" ~ "No")
}
join_bully_vars <- function(bully6, bully5) {
  case_when(
    !is.na(bully6) ~ bully6, 
    !is.na(bully5) ~ bully5)
}
bully_var <- function(bully) {
  case_when(
    bully <= 2 & bully > 0 ~ "Yes",
    bully > 2 | bully <= 0 ~ "No")
}
