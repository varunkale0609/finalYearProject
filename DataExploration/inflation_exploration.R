library(ggplot2)
transfer_inflation_new <- read.csv("newData/transfer_inflation_new.csv")

transfer_inflation_new$Season <- seq(from = 2020, to = 2001, by = -1 )


plot(transfer_inflation_new$Season, transfer_inflation_new$Increase_Factor,xlab = "Season", ylab = "Factor to Increase to Todays Money",)

ggplot2::ggplot(data = transfer_inflation_new, aes(Season, Increase_Factor)) + 
  geom_point() +
  geom_smooth(method = glm, se = FALSE) +
  theme(panel.background = element_blank()) +
  xlab("Season") +
  ylab("Factor to Convert to Todays Money")

