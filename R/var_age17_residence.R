make_age17_living_var <- function(data) {
  data %>%
    mutate(
      GDHTYP00 = as.numeric(GDHTYP00),
      parents17 = as.factor(case_when(
        GDHTYP00 < 0 ~ 0,
        GDHTYP00 == 11 ~ 0,
        GDHTYP00 == 20 ~ 0,
        GDHTYP00 == 23 ~ 0,
        GDHTYP00 == 24 ~ 0,
        GDHTYP00 >= 1 & GDHTYP00 <= 10 ~ 1,
        GDHTYP00 >= 12 ~ 1)),
      siblings17 = case_when(
        GDOTHS00 > 0 ~ as.numeric(GDOTHS00),
        .default = 0
      ),
      residence2 = as.factor(case_when(
        residence == "Left parental home" ~ "Left parental home",
        residence == "Living with parents, no siblings" & 
          parents17 == 1 & siblings17 == 0 ~
          "Living with parents, no siblings since before the pandemic",
        residence == "Living with parents, no siblings" & 
          parents17 == 1 & siblings17 > 0 ~
          "Living with parents, no siblings since the pandemic",
      
        residence == "Living with parents, no siblings" & 
          parents17 == 0 & siblings17 > 0 ~
          "Living with parents, no siblings since the pandemic",
        residence == "Living with parents, no siblings" & 
          parents17 == 0 & siblings17 == 0 ~
          "Living with parents, no siblings since the pandemic",
        
        residence == "Living with parents & siblings" & 
          parents17 == 0 | siblings17 == 0 ~
          "Living with parents & siblings since the pandemic",
        
        residence == "Living with parents & siblings" &
          parents17 == 1 & siblings17 > 0 ~
          "Living with parents & siblings since before the pandemic"))) %>%
    mutate(existing_residence = 
             as.factor(case_when(
               parents17 == 0 & siblings17 <= 0 ~ 
                 "Left parental home",
               parents17 == 1 & siblings17 <= 0 ~ 
                 "Living with parents, no siblings",
               parents17 == 1 & siblings17 > 0 ~ 
                 "Living with parents & siblings",
               parents17 == 0 & siblings17 > 0 ~
                 "Living with siblings, no parents")),
           existing_residence = 
             relevel(existing_residence,
                     ref = 
                       "Living with parents, no siblings"))
    
}
