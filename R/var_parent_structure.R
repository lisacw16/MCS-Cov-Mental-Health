make_family_structure_var <- function(data, file) {
  famder7 <- load_tab_file_function(file)
  data %>%
    full_join(famder7, by = c("MCSID")) %>%
    mutate(
      step_7 = droplevels(relevel(case_when(
        step_conversion(GDHTYP00) != "Missing" ~ 
          step_conversion(GDHTYP00)),
        ref = "No")),
      single_7 = droplevels(relevel(case_when(
        x.DHTYS00_factor(GDHTYS00) != "Missing" ~
          x.DHTYS00_factor(GDHTYS00)), 
        ref = "No")),
      par_structure_single =
        make_par_single_var(
          GDHTYS00,
          FDHTYS00,
          EHTYS00,
          DDHTYS00,
          CDHTYS00,
          BDHTYS00,
          ADHTYS00),
      par_structure_step = 
        make_par_step_var(
          GDHTYP00,
          FDHTYP00,
          EHTYP00,
          DDHTYP00,
          CDHTYP00,
          BDHTYP00)) 
}

make_par_single_var <- function(v1, v2, v3, v4, v5, v6, v7) {
  single_par_structure_ifelse(
    x.DHTYS00_factor(v1),
    x.DHTYS00_factor(v2),
    x.DHTYS00_factor(v3),
    x.DHTYS00_factor(v4),
    x.DHTYS00_factor(v5),
    x.DHTYS00_factor(v6),
    x.DHTYS00_factor(v7))
}

make_par_step_var <-function(v1, v2, v3, v4, v5, v6) {
  step_par_structure_ifelse(
    step_conversion(v1),
    step_conversion(v2),
    step_conversion(v3),
    step_conversion(v4),
    step_conversion(v5),
    step_conversion(v6)
)
}

step_par_structure_ifelse <-function(v1, v2, v3, v4, v5, v6) {
  case_when(
    v1 != "Missing" & !is.na(v1) ~ v1,
    v2 != "Missing" & !is.na(v2) ~ v2,
    v3 != "Missing" & !is.na(v3) ~ v3,
    v4 != "Missing" & !is.na(v4) ~ v4,
    v5 != "Missing" & !is.na(v5) ~ v5,
    v6 != "Missing" & !is.na(v6) ~ v6)
}

step_conversion <- function(var) {
  factor(var, 
         levels = c(-1, 1, 2, 3, 4, 5, 6, 
                    7, 8, 9, 10, 11, 12, 
                    13, 14, 15, 16, 17, 
                    18, 19, 20, 21, 22, 
                    23, 24, 25, 26),
         labels = c("Missing", "No", "Yes", "No", 
                    "No", "Yes", "No", "No", "No",
                    "No", "No", "No", "No", "No", "No", 
                    "No", "No", "No", "No", "Yes", "No", 
                    "No", "Yes","Missing", "No", "Yes", 
                    "Yes"))
}


x.DHTYS00_factor <- function(var) {
  factor(var, 
         levels = c(1, 2, -1),
         labels = c("No", "Yes","Missing"))
}

single_par_structure_ifelse <-function(v1, v2, v3, v4, v5, v6, v7) {
  case_when(
    v1 != "Missing" & !is.na(v1) ~ v1,
    v2 != "Missing" & !is.na(v2) ~ v2,
    v3 != "Missing" & !is.na(v3) ~ v3,
    v4 != "Missing" & !is.na(v4) ~ v4,
    v5 != "Missing" & !is.na(v5) ~ v5,
    v6 != "Missing" & !is.na(v6) ~ v6,
    v7 != "Missing" & !is.na(v7) ~ v7
  )
}
