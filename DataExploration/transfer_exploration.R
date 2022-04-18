transfer_data_df   <- read.csv("newData/transfer_data_new.csv") 
results_data <- read.csv("newData/results_new.csv")
transfer_inflation <- read.csv("newData/transfer_inflation_new.csv")

transfer_info <- create_transfer_data(transfers_data = transfer_data_df)

adjusted <- adjust_transfers_money(transfer_inflation = transfer_inflation, transfer_data_df = transfer_data_df)$transfers_without_pound


# arsenal spend
west_ham <- adjusted %>% filter(Team == "West Hame") %>%
  mutate(Balance = (Balance/1000000))

west_ham[["sign"]] = ifelse(west_ham[["Balance"]] >= 0, "positive", "negative")

ggplot2::ggplot(data = west_ham, aes(Season, Balance, fill = sign)) + 
  geom_col() +
  ggtitle("West Ham Net Income") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Season") +
  ylab("Net Income (£m)") + ylim(c(-100, 50)) 

# arsenal spend
Liverpool <- adjusted %>% filter(Team == "Liverpool") %>%
  mutate(Balance = (Balance/1000000))

Liverpool[["sign"]] = ifelse(Liverpool[["Balance"]] >= 0, "positive", "negative")

ggplot2::ggplot(data = Liverpool, aes(Season, Balance, fill = sign)) + 
  geom_col() +
  ggtitle("Liverpool Net Income") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Season") +
  ylab("Net Income (£m)") + ylim(c(-150, 50)) 

# total clubs spend 

total <- adjusted %>% group_by(Season) %>%
  mutate(Total_Expenditure = (sum(Expenditure)/1000000) ) %>% select(Season, Total_Expenditure) %>% distinct()

ggplot2::ggplot(data = total, aes(Season, Total_Expenditure)) + 
  geom_col(fill = c("Dark Blue")) +
  ggtitle("Total Clubs Expenditure") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Season") +
  ylab("Expenditure (£m)") 

total <- adjusted %>% group_by(Season) %>%
  mutate(Total_Expenditure = (sum(Balance)/1000000) ) %>% select(Season, Total_Expenditure) %>% distinct()

ggplot2::ggplot(data = total, aes(Season, Total_Expenditure)) + 
  geom_col(fill = c("Red")) +
  ggtitle("Total Clubs Net Income") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  geom_smooth( se = FALSE) +
  xlab("Season") +
  ylab("Net Income (£m)") 

