# Descriptives for each wave are called separately (ran on existing data),and made into a table. For imputed variables (i.e., stepparent, single parent, family income, and well-being), descriptives need updating to include imputed values. The code here provides tables that give correct summary statistics for exclusively imputed values. These are then manually added to the existing descriptive tables

# Wave-specific descriptive corrections:
descriptives_mi.cv.w1 <- function(df){
  ma_data <- filter(df, male == "Male")
  fe_data <-filter(df, male == "Female")
  
  overall <- continuousw1_summary(mice::as.mids(df),"Overall")
  Female <- continuousw1_summary(mice::as.mids(fe_data),"Female")
  Male <- continuousw1_summary(mice::as.mids(ma_data),"Male")
  
  overall_bin <- descriptives_impw1_binary(df,"Overall")
  female_bin <- descriptives_impw1_binary(fe_data,"Female")
  male_bin <- descriptives_impw1_binary(ma_data,"Male")
  
  #----#
  rbind(overall,Female,Male) %>%
    select(-c(statistic,df,p.value)) %>%
    rbind(overall_bin,
          female_bin,
          male_bin) %>%
    reshape(idvar = "term",
            timevar = "dataset",
            direction = "wide") %>%
    select(-c(contains("estimate"), contains("std.error")))
}
descriptives_mi.cv.w2 <- function(df){
  ma_data <- filter(df, Male == "Male")
  fe_data <-filter(df, Male == "Female")
  
  overall <- continuousw2_summary(mice::as.mids(df),"Overall")
  Female <- continuousw2_summary(mice::as.mids(fe_data),"Female")
  Male <- continuousw2_summary(mice::as.mids(ma_data),"Male")
  
  overall_bin <- descriptives_impw2_binary(df,"Overall")
  female_bin <- descriptives_impw2_binary(fe_data,"Female")
  male_bin <- descriptives_impw2_binary(ma_data,"Male")
  
  #----#
  rbind(overall,Female,Male) %>%
    select(-c(statistic,df,p.value)) %>%
    rbind(overall_bin,
          female_bin,
          male_bin) %>%
    reshape(idvar = "term",
            timevar = "dataset",
            direction = "wide") %>%
    select(-c(contains("estimate"), contains("std.error")))
}
descriptives_mi.cv.w3 <- function(df){
  ma_data <- filter(df, 
                    Male == "Male")
  fe_data <-filter(df,
                   Male == "Female")
  
  overall <- continuousw3_summary(mice::as.mids(df),
                                  "Overall")
  Female <- continuousw3_summary(mice::as.mids(fe_data),
                                 "Female")
  Male <- continuousw3_summary(mice::as.mids(ma_data),
                               "Male")
  
  overall_bin <- descriptives_impw3_binary(df,
                                           "Overall")
  female_bin <- descriptives_impw3_binary(fe_data,
                                          "Female")
  male_bin <- descriptives_impw3_binary(ma_data,
                                        "Male")
  
  #----#
  rbind(overall,Female,Male) %>%
    select(-c(statistic,df,p.value)) %>%
    rbind(overall_bin,
          female_bin,
          male_bin) %>%
    reshape(idvar = "term",
            timevar = "dataset",
            direction = "wide")%>%
    select(-c(contains("estimate"), contains("std.error")))
}
# Builder functions for imputation model descriptives
descriptives_impw1_binary <- function(df, string){
  # -- Step -- #
  step_7_wpct <- with(df, 
                      by(df, 
                         .imp, 
                         function(x) c(
                           weights::wpct(x$step_7,
                                         weight = 
                                           x$CW1_COMBWT,
                                         na.rm=TRUE)
                         )))
  step_7_wpct <- round(100*(
    Reduce("+",step_7_wpct)/length(step_7_wpct)),
    digits = 1)
  step_7_wpct <- as.data.frame(step_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = step_7_wpct)
  step_7_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, step_7 == "Yes") %>%
                              summarise(n=n())
                          )))
  step_7_count <- round(Reduce("+",step_7_count)/length(step_7_count),
                        digits = 0)
  step_7_count <- as.data.frame(step_7_count,row.names = 'Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = step_7_count)
  # -- Single -- #
  single_7_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$single_7,
                          weight = x$CW1_COMBWT,
                          na.rm=TRUE)
          )))
  single_7_wpct <- round(100*(
    Reduce("+",single_7_wpct)/length(single_7_wpct)),
    digits = 1)
  single_7_wpct <- as.data.frame(single_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = single_7_wpct)
  single_7_count <- with(df,
                         by(df, 
                            .imp, 
                            function(x) c(
                              filter(x, single_7 == "Yes") %>%
                                summarise(n=n())
                            )))
  single_7_count <- round(
    Reduce("+",single_7_count)/length(single_7_count),
    digits = 0)
  single_7_count <- as.data.frame(single_7_count,
                                  row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = single_7_count)
  # -- Income -- #
  income_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$W6_lowincome,
                          weight = x$CW1_COMBWT,
                          na.rm=TRUE)
          )))
  income_wpct <- round(100*(
    Reduce("+",income_wpct)/length(income_wpct)),
    digits = 1)
  income_wpct <- as.data.frame(income_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = income_wpct)
  income_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, W6_lowincome == "Yes") %>%
                              summarise(n=n())
                          )))
  income_count <- round(
    Reduce("+",income_count)/length(income_count),
    digits = 0)
  income_count <- as.data.frame(income_count,
                                row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = income_count)
  ## -- Education -- ##
  edu_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$fam_ses_par_ed,
                          weight = x$CW1_COMBWT,
                          na.rm=TRUE)
          )))
  edu_wpct <- round(100*(
    Reduce("+",edu_wpct)/length(edu_wpct)),
    digits = 1)
  edu_wpct <- as.data.frame(edu_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = edu_wpct)
  edu_count <- with(df,
                    by(df, 
                       .imp, 
                       function(x) c(
                         filter(x, fam_ses_par_ed == "Yes") %>%
                           summarise(n=n())
                       )))
  edu_count <- round(
    Reduce("+",edu_count)/length(edu_count),
    digits = 0)
  edu_count <- as.data.frame(edu_count,
                             row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = edu_count)
  
  # -- Bind -- #
  step <- step_7_wpct %>%
    full_join(step_7_count, by = c("id")) %>%
    mutate_summary_table.bin("stepparent", string)
  
  single <- single_7_wpct %>%
    full_join(single_7_count, by = c("id")) %>%
    mutate_summary_table.bin("singleparent", string)
  
  income <- income_wpct %>%
    full_join(income_count, by = c("id")) %>%
    mutate_summary_table.bin("income",string)
  
  education <- edu_wpct %>%
    full_join(edu_count, by = c("id")) %>%
    mutate_summary_table.bin("edu",string)
  
  rbind(step,single,income,education)
  
}
continuousw1_summary <- function(data,string){
  covid_K6 <- summary(mice::pool(
    with(data,
         lm(covid_K6 ~ 1, 
            weights = CW1_COMBWT))
  )) %>%
    mutate_summary_table.cont("covid_K6",string)
  
  covid_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(covid_SWEMWBS ~ 1, 
            weights = CW1_COMBWT))
  )) %>%
    mutate_summary_table.cont("covid_SWEMWBS",string)
  
  existing_K6 <- summary(mice::pool(
    with(data,
         lm(existing_K6 ~ 1, 
            weights = CW1_COMBWT))
  )) %>%
    mutate_summary_table.cont("existing_K6",string)
  
  
  existing_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(existing_SWEMWBS ~ 1, 
            weights = CW1_COMBWT))
  )) %>%
    mutate_summary_table.cont("existing_SWEMWBS",string)
  
  rbind(covid_K6,
        existing_K6,
        covid_SWEMWBS,
        existing_SWEMWBS)
}
descriptives_impw2_binary <- function(df, string){
  # -- Step -- #
  step_7_wpct <- with(df, 
                      by(df, 
                         .imp, 
                         function(x) c(
                           weights::wpct(x$W7_steppar,
                                         weight = 
                                           x$CW2_COMBWT,
                                         na.rm=TRUE)
                         )))
  step_7_wpct <- round(100*(
    Reduce("+",step_7_wpct)/length(step_7_wpct)),
    digits = 1)
  step_7_wpct <- as.data.frame(step_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = step_7_wpct)
  step_7_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, W7_steppar == "Yes") %>%
                              summarise(n=n())
                          )))
  step_7_count <- round(Reduce("+",step_7_count)/length(step_7_count),
                        digits = 0)
  step_7_count <- as.data.frame(step_7_count,row.names = 'Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = step_7_count)
  # -- Single -- #
  single_7_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$W7_single_par,
                          weight = x$CW2_COMBWT,
                          na.rm=TRUE)
          )))
  single_7_wpct <- round(100*(
    Reduce("+",single_7_wpct)/length(single_7_wpct)),
    digits = 1)
  single_7_wpct <- as.data.frame(single_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = single_7_wpct)
  single_7_count <- with(df,
                         by(df, 
                            .imp, 
                            function(x) c(
                              filter(x, W7_single_par == "Yes") %>%
                                summarise(n=n())
                            )))
  single_7_count <- round(
    Reduce("+",single_7_count)/length(single_7_count),
    digits = 0)
  single_7_count <- as.data.frame(single_7_count,
                                  row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = single_7_count)
  # -- Income -- #
  income_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$W6_lowincome,
                          weight = x$CW2_COMBWT,
                          na.rm=TRUE)
          )))
  income_wpct <- round(100*(
    Reduce("+",income_wpct)/length(income_wpct)),
    digits = 1)
  income_wpct <- as.data.frame(income_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = income_wpct)
  income_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, W6_lowincome == "Yes") %>%
                              summarise(n=n())
                          )))
  income_count <- round(
    Reduce("+",income_count)/length(income_count),
    digits = 0)
  income_count <- as.data.frame(income_count,
                                row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = income_count)
  ## -- Education -- ##
  edu_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$fam_ses_par_ed,
                          weight = x$CW2_COMBWT,
                          na.rm=TRUE)
          )))
  edu_wpct <- round(100*(
    Reduce("+",edu_wpct)/length(edu_wpct)),
    digits = 1)
  edu_wpct <- as.data.frame(edu_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = edu_wpct)
  edu_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, fam_ses_par_ed == "Yes") %>%
                              summarise(n=n())
                          )))
  edu_count <- round(
    Reduce("+",edu_count)/length(edu_count),
    digits = 0)
  edu_count <- as.data.frame(edu_count,
                                row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = edu_count)
  # -- Bind -- #
  step <- step_7_wpct %>%
    full_join(step_7_count, by = c("id")) %>%
    mutate_summary_table.bin("stepparent", string)
  
  single <- single_7_wpct %>%
    full_join(single_7_count, by = c("id")) %>%
    mutate_summary_table.bin("singleparent", string)
  
  income <- income_wpct %>%
    full_join(income_count, by = c("id")) %>%
    mutate_summary_table.bin("income",string)
  
  education <- edu_wpct %>%
    full_join(edu_count, by = c("id")) %>%
    mutate_summary_table.bin("edu",string)
  
  rbind(step,single,income, education)
}
continuousw2_summary <- function(data,string){
  Covid_K6 <- summary(mice::pool(
    with(data,
         lm(CW2_Covid_K6 ~ 1, 
            weights = CW2_COMBWT))
  )) %>%
    mutate_summary_table.cont("CW2_Covid_K6",string)
  
  Covid_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(CW2_Covid_SWEMWBS ~ 1, 
            weights = CW2_COMBWT))
  )) %>%
    mutate_summary_table.cont("CW2_Covid_SWEMWBS",string)
  
  existing_K6 <- summary(mice::pool(
    with(data,
         lm(existing_K6 ~ 1, 
            weights = CW2_COMBWT))
  )) %>%
    mutate_summary_table.cont("existing_K6",string)
  
  
  existing_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(existing_SWEMWBS ~ 1, 
            weights = CW2_COMBWT))
  )) %>%
    mutate_summary_table.cont("existing_SWEMWBS",string)
  
  rbind(Covid_K6,
        existing_K6,
        Covid_SWEMWBS,
        existing_SWEMWBS)
}
descriptives_impw3_binary <- function(df, string){
  # -- Step -- #
  step_7_wpct <- with(df, 
                      by(df, 
                         .imp, 
                         function(x) c(
                           weights::wpct(x$W7_steppar,
                                         weight = 
                                           x$CW3_COMBWT,
                                         na.rm=TRUE)
                         )))
  step_7_wpct <- round(100*(
    Reduce("+",step_7_wpct)/length(step_7_wpct)),
    digits = 1)
  step_7_wpct <- as.data.frame(step_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = step_7_wpct)
  step_7_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, W7_steppar == "Yes") %>%
                              summarise(n=n())
                          )))
  step_7_count <- round(Reduce("+",step_7_count)/
                          length(step_7_count),
                        digits = 0)
  step_7_count <- as.data.frame(step_7_count,
                                row.names = 'Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = step_7_count)
  # -- Single -- #
  single_7_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$W7_single_par,
                          weight = x$CW3_COMBWT,
                          na.rm=TRUE)
          )))
  single_7_wpct <- round(100*(
    Reduce("+",single_7_wpct)/length(single_7_wpct)),
    digits = 1)
  single_7_wpct <- as.data.frame(single_7_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = single_7_wpct)
  single_7_count <- with(df,
                         by(df, 
                            .imp, 
                            function(x) c(
                              filter(x, 
                                     W7_single_par == "Yes") %>%
                                summarise(n=n())
                            )))
  single_7_count <- round(
    Reduce("+",single_7_count)/length(single_7_count),
    digits = 0)
  single_7_count <- as.data.frame(single_7_count,
                                  row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = single_7_count)
  # -- Income -- #
  income_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$W6_lowincome,
                          weight = x$CW3_COMBWT,
                          na.rm=TRUE)
          )))
  income_wpct <- round(100*(
    Reduce("+",income_wpct)/length(income_wpct)),
    digits = 1)
  income_wpct <- as.data.frame(income_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = income_wpct)
  income_count <- with(df,
                       by(df, 
                          .imp, 
                          function(x) c(
                            filter(x, W6_lowincome == "Yes") %>%
                              summarise(n=n())
                          )))
  income_count <- round(
    Reduce("+",income_count)/length(income_count),
    digits = 0)
  income_count <- as.data.frame(income_count,
                                row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = income_count)
  # -- Education -- #
  edu_wpct <- with(
    df,by(df,
          .imp, 
          function(x) c(
            weights::wpct(x$fam_ses_par_ed,
                          weight = x$CW3_COMBWT,
                          na.rm=TRUE)
          )))
  edu_wpct <- round(100*(
    Reduce("+",edu_wpct)/length(edu_wpct)),
    digits = 1)
  edu_wpct <- as.data.frame(edu_wpct) %>%
    mutate(id = c("No", "Yes")) %>%
    rename(wpct = edu_wpct)
  edu_count <- with(df,
                    by(df, 
                       .imp, 
                       function(x) c(
                         filter(x, fam_ses_par_ed == "Yes") %>%
                           summarise(n=n())
                       )))
  edu_count <- round(
    Reduce("+",edu_count)/length(edu_count),
    digits = 0)
  edu_count <- as.data.frame(edu_count,
                             row.names ='Yes') %>%
    mutate(id = "Yes") %>%
    rename(n = edu_count)
  # -- Bind -- #
  step <- step_7_wpct %>%
    full_join(step_7_count, by = c("id")) %>%
    mutate_summary_table.bin("stepparent", string)
  
  single <- single_7_wpct %>%
    full_join(single_7_count, by = c("id")) %>%
    mutate_summary_table.bin("singleparent", string)
  
  income <- income_wpct %>%
    full_join(income_count, by = c("id")) %>%
    mutate_summary_table.bin("income",string)
  
  education <- edu_wpct %>%
    full_join(edu_count, by = c("id")) %>%
    mutate_summary_table.bin("edu",string)
  
  rbind(step,single,income,education)
  
}
continuousw3_summary <- function(data,string){
  Covid_K6 <- summary(mice::pool(
    with(data,
         lm(CW3_Covid_K6 ~ 1, 
            weights = CW3_COMBWT))
  )) %>%
    mutate_summary_table.cont("CW3_Covid_K6",string)
  
  Covid_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(CW3_Covid_SWEMWBS ~ 1, 
            weights = CW3_COMBWT))
  )) %>%
    mutate_summary_table.cont("CW3_Covid_SWEMWBS",string)
  
  existing_K6 <- summary(mice::pool(
    with(data,
         lm(existing_K6 ~ 1, 
            weights = CW3_COMBWT))
  )) %>%
    mutate_summary_table.cont("existing_K6",string)
  
  
  existing_SWEMWBS <- summary(mice::pool(
    with(data,
         lm(existing_SWEMWBS ~ 1, 
            weights = CW3_COMBWT))
  )) %>%
    mutate_summary_table.cont("existing_SWEMWBS",string)
  
  rbind(Covid_K6,
        existing_K6,
        Covid_SWEMWBS,
        existing_SWEMWBS)
}
# Functions used in to create descriptive tables for all waves
mutate_summary_table.cont <- function(data,string1,string2){
  data %>%
    mutate(term = string1,
           dataset = string2,
           estimate = round(estimate, digits = 2),
           std.error = round(std.error, digits = 2),
           summary = paste(estimate,"(",std.error,")"))
}
mutate_summary_table.bin <- function(data,string,string2){
  data %>%
    filter(id == "Yes") %>%
    select(-id) %>%
    mutate(term = string) %>%
    rename(estimate = n,
           std.error = wpct) %>%
    mutate(summary = paste(estimate,"(",std.error,")"),
           dataset = string2)
}
