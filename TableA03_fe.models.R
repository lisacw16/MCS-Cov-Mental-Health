fixedmodels_1 <- function(data) {
  K61 <- with(data,
              plm::plm(K6 ~ cov_symp+country+residence*Male,
                       weights = weight1,
                       data = data.frame(mget(ls())),
                       index = c("MCSID", "wave"),
                       model = "within"))
  K61 <- mice::pool(K61)

  K62 <- with(data,
              plm::plm(K6 ~ cov_symp+country+residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       weights = weight1,
                       index = c("MCSID", "wave"),
                       model = "within"))
  K62 <- mice::pool(K62)

  WB1 <- with(data,
              plm::plm(SWEMWBS ~ cov_symp+country+
                         residence*Male,
                       data = data.frame(mget(ls())),
                       weights = weight1,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB1 <- mice::pool(WB1)

  WB2 <- with(data,
              plm::plm(SWEMWBS ~ cov_symp+country+
                         residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       weights = weight1,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB2 <- mice::pool(WB2)

  list("K6 Model 1" = K61,
       "K6 Model 2" = K62,
       "WB Model 1" = WB1,
       "WB Model 2" = WB2)
}

fixedmodels_0_3 <- function(data1,data2,data3) {
  K61 <- with(data1,
              plm::plm(K6 ~ cov_symp+country+residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       index = c("MCSID", "wave"),
                       model = "within"))
  K61 <- mice::pool(K61)
  
  WB1 <- with(data1,
              plm::plm(SWEMWBS ~ cov_symp+country+residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       weights = weight1,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB1 <- mice::pool(WB1)
  
  K62 <- with(data2,
              plm::plm(K6 ~ cov_symp+country+residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       weights = weight2,
                       data = data.frame(mget(ls())),
                       index = c("MCSID", "wave"),
                       model = "within"))
  K62 <- mice::pool(K62)
  
  WB2 <- with(data2,
              plm::plm(SWEMWBS ~ cov_symp+country+residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       weights = weight2,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB2 <- mice::pool(WB2)
  
  K63 <- with(data3,
              plm::plm(K6 ~ cov_symp+country+residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       weights = weight3,
                       data = data.frame(mget(ls())),
                       index = c("MCSID", "wave"),
                       model = "within"))
  K63 <- mice::pool(K63)
  
  WB3 <- with(data3,
              plm::plm(SWEMWBS ~ cov_symp+country+
                         residence*Male+
                         economic_activity+
                         fam_ses_overcrowded,
                       data = data.frame(mget(ls())),
                       weights = weight3,
                       index = c("MCSID", "wave"),
                       model = "within"))
  WB3 <- mice::pool(WB3)  
  
  list("K6 Wave 1" = K61,
       "K6 Wave 2" = K62,
       "K6 Wave 3" = K63,
       "WB Wave 1" = WB1,
       "WB Wave 2" = WB2,
       "WB Wave 3" = WB3)
}

modelsummary_femodels <- function(models, string) {
  cm <- c(
    "residenceLeft parental home" = 
      "(Female) Left Parental Home", #1
    "residenceLiving with parents & siblings" =
      "(Female) Living with Parents and Siblings", #2
    "residenceLeft parental home:MaleMale" = 
      "(Male) Left Parental Home", #3
    "residenceLiving with parents & siblings:MaleMale" =
      "(Male) Living with Parents and Siblings" #4
  )

  gof <- tibble::tribble(
    ~raw,            ~clean,    ~fmt,
    "nobs",          "N",       0)

  table <- modelsummary(models,
                        output = "gt",
                        fmt = 2,
                        coef_map = cm,
                        estimate = 
                          "{estimate}{stars} ({std.error})",
                        statistic = NULL,
                        gof_map = gof,
                        stars = c('+' = 0.1, 
                                  '*' = .05, 
                                  '**' = .01)
                        )
  
  table <- table %>%
    cols_align(align="right",
               columns = c(1)) %>%
    tab_row_group(
      label = md(
        "*(Ref: Male, Living with Parents, No Siblings)*"),
      rows = c(4,3)
      ) %>%
    tab_row_group(
      label = md("*(Ref: Female, Living with Parents, No Siblings)*"),
      rows = c(1,2)
      ) %>%
    tab_source_note(source_note = md("*Notes*: All models control for young adult economic activity, overcrowded COVID-19 household, the experience of COVID-19 symptoms, and country of residence (England, Scotland, Wales, or Northern Ireland). Estimates are weighted with survey provided sample design and attrition weights."))

  table %>% gtsave(filename = paste0(string, '.rtf'),
                   path = 'Figures_Tables/')


  return(table)
}