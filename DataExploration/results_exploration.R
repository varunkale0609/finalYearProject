results_data <- read.csv("newData/results_new.csv")

full_time_results <-  dplyr::case_when(results_data$FTR == "H" ~ 2,
                                       results_data$FTR == "D" ~ 1,
                                       results_data$FTR == "A" ~ 0)

results_data_01 <- results_data %>%
  mutate(FTR = full_time_results)

results_data_02 <- results_data_01 %>% select(-c(Season, DateTime, HomeTeam, AwayTeam, Referee, HTR, FTAG, FTHG, HTHG, HTAG))

after_covid <- results_data_02[7509:7600, ]

M <- cor(results_data_02)

corrplot::corrplot(M, type="upper", order="hclust",
                   col=brewer.pal(n=8, name="RdYlBu"))

c4 = c("green", "blue", "red")
ggplot(results_data, aes(y = FTR, fill =FTR)) +
  geom_bar(width = 0.7,  position = position_stack(reverse = TRUE), stat = "count") +
  ggtitle("Occurences for Home and Away Wins and Draws") +
  xlab("Occurences") + ylab("Full Time Result") + 
  theme(plot.title = element_text(hjust = 0.5),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "Blue")) 




