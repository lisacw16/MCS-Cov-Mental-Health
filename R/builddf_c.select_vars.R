select_analysis_vars <- function(data){
  data %>%
    filter(!is.na(CW1_COMBWT)) %>%
    filter(residence != "Living with siblings, no parents") %>%
    select(MCSID, CW1_COMBWT, 
           covid_K6, W6_income, W5_income, W4_income, 
           W3_income, W2_income, W1_income, W6_lowincome,
           CW1_PHDE, CW1_PHHO, CW1_PHRF, CW1_PHEE, 
           CW1_PHWO, CW1_PHNE, CW1_COVPER,
           CW1_HHNUMWH_3, CW1_HHNUMWH_6,
           CW1_WEMWBS_1, CW1_WEMWBS_2, CW1_WEMWBS_3, 
           CW1_WEMWBS_4, CW1_WEMWBS_5, CW1_WEMWBS_6, 
           CW1_WEMWBS_7,GDOTHS00, CW1_HHNUM,CW1_CNUM00,
           covid_SWEMWBS,
           region,cov_symp,ethnicity,
           existing_K6,existing_SWEMWBS,
           moved,married,partnered,CW1_HHNUMWH_2,
           CW1_HHNUMWH_4,CW1_HHNUMWH_8, 
           residence,residence2, existing_residence,
           parents17,siblings17,single_7, step_7,
           male,transition,
           no_sibling, num_sibling, same_sib_gender,is_oldest_sib,
           sib_bully,sib_bully6,
           economic_activity, fam_ses_par_ed, 
           fam_ses_overcrowded, par_structure_step,
           par_structure_single,
           conflict
           ) %>%
    rename(Children = CW1_HHNUMWH_2, 
           Grandparent = CW1_HHNUMWH_4, 
           Friend = CW1_HHNUMWH_8) %>%
    mutate(flag_5qs = 
             as.factor(case_when(
               is.na(covid_K6) & 
                 !is.na(CW1_PHDE) & 
                 !is.na(CW1_PHHO) & 
                 !is.na(CW1_PHRF) & 
                 !is.na(CW1_PHEE) & 
                 !is.na(CW1_PHWO) ~ 1,
               is.na(covid_K6) & 
                 !is.na(CW1_PHNE) & 
                 !is.na(CW1_PHHO) & 
                 !is.na(CW1_PHRF) & 
                 !is.na(CW1_PHEE) & 
                 !is.na(CW1_PHWO) ~ 1,
               is.na(covid_K6) & 
                 !is.na(CW1_PHDE) & 
                 !is.na(CW1_PHNE) & 
                 !is.na(CW1_PHRF) & 
                 !is.na(CW1_PHEE) & 
                 !is.na(CW1_PHWO) ~ 1,
               is.na(covid_K6) & 
                 !is.na(CW1_PHDE) & 
                 !is.na(CW1_PHHO) & 
                 !is.na(CW1_PHNE) & 
                 !is.na(CW1_PHEE) & 
                 !is.na(CW1_PHWO) ~ 1,
               is.na(covid_K6) & 
                 !is.na(CW1_PHDE) & 
                 !is.na(CW1_PHHO) & 
                 !is.na(CW1_PHRF) & 
                 !is.na(CW1_PHNE) & 
                 !is.na(CW1_PHWO) ~ 1,
               is.na(covid_K6) & 
                 !is.na(CW1_PHDE) & 
                 !is.na(CW1_PHHO) & 
                 !is.na(CW1_PHRF) & 
                 !is.na(CW1_PHEE) & 
                 !is.na(CW1_PHNE) ~ 1,
               .default = 0))) %>%
    mutate(residence = 
             droplevels(
               relevel(residence,
                       ref = "Living with parents, no siblings")),
           male = relevel(
             male,
             ref = "Female"),
           transition = relevel(
             transition,
             ref = "Stayed with parents, no siblings"),
           residence2 = relevel(
            residence2, 
            ref = "Living with parents, no siblings since before the pandemic"),
           CW1_HHNUM = as.numeric(CW1_HHNUM),
           Children = as.factor(Children),
           Grandparent = as.factor(Grandparent),
           Friend = as.factor(Friend),
           same_sib_gender = 
             relevel(same_sib_gender, ref = "No"),
           sib_bully = relevel(sib_bully, ref = "No"),
           is_oldest_sib = relevel(is_oldest_sib, ref = "No"),
           moved = relevel(moved, ref = "No"),
           region = droplevels(relevel(region, ref = "England")),
           ethnicity = relevel(ethnicity, ref = "White"),
           cov_symp = relevel(
             cov_symp, ref = "No"),
           economic_activity = relevel(
             economic_activity, ref = "In University"),
           partnered = relevel(partnered, ref = "No"),
           married = relevel(married, ref = "No"),
           fam_ses_par_ed	= relevel(
            fam_ses_par_ed, ref = "No"),
           fam_ses_overcrowded = relevel(
             fam_ses_overcrowded, ref = "No"),
           par_structure_step = droplevels(relevel(
             par_structure_step, ref = "No")),
           par_structure_single = droplevels(relevel(
             par_structure_single, ref = "No")),
      conflict = relevel(conflict, ref = "No"))
}
