Figure1_function.K6 <- function(data, var, n) {
  pd <- position_dodge(0.05) # move them .05 to the left and right
  plot <- data %>%
    filter(residence != "Living with siblings, no parents") %>%
    ggplot(aes(x=t, 
               y=k6.mean, 
               group = residence, 
               linetype = residence)) +
    facet_wrap(~male, nrow = 1) +
    labs(y = "Mean Psychological Distress (K6)", 
         x = "", 
         linetype = "Young Adult COVID-19 Living Arrangement") +
    geom_errorbar(
       aes(ymin=k6.mean-(1.96*sqrt(({{var}}/{{n}}))), 
           ymax=k6.mean+(1.96*sqrt(({{var}}/{{n}})))), 
       width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.text = element_text(size = 16),
          text = element_text(size=16,
                              family = "Times"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 16),
          axis.text.x= element_text(colour = "black",
                                    size = 16),
          axis.text.y= element_text(colour = "black",
                                    size = 16),
          axis.line.x.bottom=element_line(color="black"),
          axis.line.y.left=element_line(color="black"))
  
  ggsave("Figures_Tables/01_Figure_K6.pdf", 
         plot = plot,
         height = 6, width = 12,
         dpi = 600)
  
  return(plot)
}

Figure1_function.WB <- function(data, var, n) {
  pd <- position_dodge(0.05) # move them .05 to the left and right
  plot <- data %>%
    filter(residence != "Living with siblings, no parents") %>%
    ggplot(aes(x=t, 
               y=wb.mean, 
               group = residence, 
               linetype = residence)) +
    facet_wrap(~male, nrow = 1) +
    labs(y = "Mean Mental Well-Being (SWEMWBS)", 
         x = "", 
         linetype = "Young Adult COVID-19 Living Arrangement") +
    geom_errorbar(
      aes(ymin=wb.mean-(1.96*sqrt(({{var}}/{{n}}))), 
          ymax=wb.mean+(1.96*sqrt(({{var}}/{{n}})))), 
      width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    scale_linetype_manual(values=c("solid", "dashed", "dotted"))+
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.text = element_text(size = 16),
          text = element_text(size=16,
                              family = "Times"),
          strip.text.x = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x= element_text(colour = "black",
                                    size = 16),
          axis.text.y= element_text(colour = "black",
                                    size = 16),
          axis.line.x.bottom=element_line(color="black"),
          axis.line.y.left=element_line(color="black"))
  
  
  ggsave("Figures_Tables/01_Figure_WB.pdf", 
         plot = plot,
         height = 6, width = 12,
         dpi = 600)
  
  return(plot)
}

figure_df.function <- function(data) {
  cov <-  summarise_cv(data)
  aged_17 <- summarise_17(data)
  
  rbind(cov, aged_17)
}

summarise_cv <- function(data){
  k6 <- data %>%
    filter(!is.na(covid_K6), !is.na(existing_K6)) %>%
    group_by(residence, male) %>%
    summarise(k6.mean = weighted.mean(covid_K6, CW1_COMBWT, 
                                      na.rm = TRUE),
              k6.se = wtd.var(covid_K6, CW1_COMBWT, 
                              na.rm = TRUE),
              k6.n = n()) %>% 
    ungroup()
  
  wb <- data %>%
    filter(!is.na(covid_SWEMWBS), !is.na(existing_SWEMWBS)) %>%
    group_by(residence, male) %>%
    summarise(wb.mean = weighted.mean(covid_SWEMWBS, CW1_COMBWT, 
                                      na.rm = TRUE),
              wb.se = wtd.var(covid_SWEMWBS, CW1_COMBWT, 
                              na.rm = TRUE),
              wb.n = n()) %>%
    ungroup()
  
  k6 %>%
    full_join(wb, by = c("residence", "male")) %>%
    mutate(t = "COVID-19")

}
summarise_17 <- function(data){
  k6 <- data %>%
    filter(!is.na(covid_K6), !is.na(existing_K6)) %>%
    group_by(residence, male) %>%
    summarise(k6.mean = weighted.mean(existing_K6, CW1_COMBWT, 
                                      na.rm = TRUE),
              k6.se = wtd.var(existing_K6, CW1_COMBWT, 
                              na.rm = TRUE),
              k6.n = n()) %>% 
    ungroup()
  
  wb <- data %>%
    filter(!is.na(covid_SWEMWBS), !is.na(existing_SWEMWBS)) %>%
    group_by(residence, male) %>%
    summarise(wb.mean = weighted.mean(existing_SWEMWBS, CW1_COMBWT, 
                                      na.rm = TRUE),
              wb.se = wtd.var(existing_SWEMWBS, CW1_COMBWT, 
                              na.rm = TRUE),
              wb.n = n()) %>%
    ungroup()
  
  k6 %>%
    full_join(wb, by = c("residence", "male")) %>%
    mutate(t = "Aged 17")
}