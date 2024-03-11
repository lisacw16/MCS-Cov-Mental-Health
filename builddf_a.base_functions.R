load_sav_file_function <- function(file){
  haven::read_sav(
    file,
    encoding = NULL,
    user_na = TRUE,
    col_select = NULL,
    skip = 0,
    n_max = Inf,
    .name_repair = "unique"
  )
}

load_tab_file_function <- function(file){
  read.table(
    file,
    header = TRUE,
    fill = TRUE,
    sep = ""
  )
}

check_for_duplicates <- function(data) {
  length(unique(working_data$MCSID)) == nrow(working_data)
} #True indicates that there are no duplicate values

## colSums(is.na(check)) ## 

inspect <- function(data) {
  data %>%
    print(data)
}

ggplot_theme <- function() {
    theme_bw() +
    theme(
      axis.line = element_line(colour = "black"),
      panel.background = element_blank(),
      text = element_text(
        family="Times", 
        size=16),
      legend.title=element_text(size=14), 
      legend.text=element_text(size=13),
      axis.text.x= element_text(colour = "black"),
      axis.text.y= element_text(colour = "black"),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      panel.border = element_blank()
    )
}

gttable_apa <- function(table) {
  table %>%
    tab_options (
      table.border.top.color = "black",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 2,
      column_labels.border.top.color = "black", 
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",  
      table.border.bottom.color = "white",  
      table.background.color = "white",
      table.font.names = "Times New Roman",
      table.font.size = px(26),
      row_group.border.top.color = "white",
      row_group.border.top.width = 1,
      row_group.border.bottom.color = "white",
      footnotes.font.size = px(24)
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = c("top", "bottom"), 
                     color = "white", weight = px(1)),
        cell_fill(color = "white", alpha = NULL)),
      locations = cells_body(columns = everything(), rows = everything()
      )) 
}


convert_to_numeric <- function(var) {
  as.numeric(case_when(as.numeric(var) >= 0 ~ var))
}

add_error_bar <- function(data){
  pd <- position_dodge(0.1) # move them .05 to the left and right
  geom_errorbar(data, aes(ymin=mean-ci, ymax=mean+ci),
                width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd)
}

load_cov_file_function <- function(file) {
  cov_data <- load_sav_file_function(file)
  cov_data %>%
    select(-c(NCDSID, BCSID, NSID, CW1_PNUM00)) %>%
    filter(CW1_CNUM00 == 1)
}

load_w7_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(GCNUM00 == 1)
}

load_w6_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(FCNUM00 == 1)
}

load_w5_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(ECNUM00 == 1)
}

load_w4_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(DCNUM00 == 1)
}

load_w3_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(CCNUM00 == 1)
}

load_w2_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(BCNUM00 == 1)
}

load_w1_filter_cm <- function(file) {
  data <- load_tab_file_function(file)
  data %>%
    filter(ACNUM00 == 1)
}