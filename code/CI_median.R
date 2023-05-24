library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(boot)
library(viridis)
library('plotly')
library('GGally')


setwd("D:/Developing/R Lab/LAB-1")


df <- read_csv('airlane_tidy.csv', show_col_types = FALSE)
df


ggcorr((df %>% filter(Class == 'Eco' | Class == 'Eco Plus'))[c(-1)], method = c("everything", "spearman"), hjust = 1, vjust=0, size=3.5, layout.exp = 3,
        label=TRUE
       , label_size=3.5, label_round = 1, label_alpha = TRUE, palette='PuBuGn') +
  labs(title = 'Eco Type') +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10)),
        axis.text.x=element_blank())


df_tmp <- df %>% filter(Class == "Eco" | Class == "Eco Plus")
correlation <- cor(df_tmp$Satisfaction, df_tmp$Inflight_wifi_service, method = "spearman")


my_function <- function(data, index) {
  cor(data$Satisfaction[index], data$Inflight_wifi_service[index], method = "spearman")
}

bootstrap_result <- boot(data = df %>% filter(Class == "Eco" | Class == "Eco Plus"), statistic = my_function, R = 1000)
median_ci <- boot.ci(bootstrap_result, type = "basic")

#print(median_ci$basic[4:5])

lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

cat("Confidence Interval for the Correlation:", "\n")
cat("Correlation for Eco Type(Satisfaction, Inflight_wifi_service):", correlation, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")






##############################################

##############################################


ggcorr((df %>% filter(Class == 'Business'))[c(-1)], method = c("everything", "spearman"), hjust = 1, vjust=0, size=3.5, layout.exp = 3,
       label=TRUE
  , label_size=3.5, label_round = 1, label_alpha = TRUE, palette='PuBuGn') +
  labs(title = 'Business') +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10)),
        axis.text.x=element_blank())

df_tmp <- df %>% filter(Class == 'Business')
correlation <- cor(df_tmp$Satisfaction, df_tmp$Inflight_entertainment, method = "spearman")


my_function <- function(data, index) {
  cor(data$Satisfaction[index], data$Inflight_entertainment[index], method = "spearman")
}

bootstrap_result <- boot(data = df %>% filter(Class == "Business"), statistic = my_function, R = 1000)
median_ci <- boot.ci(bootstrap_result, type = "basic")

#print(median_ci$basic[4:5])

lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

cat("Confidence Interval for the Correlation:", "\n")
cat("Correlation for Business Type(Satisfaction, Inflight_entertainment):", correlation, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n\n\n")

##############################################

##############################################

correlation <- cor(df_tmp$Satisfaction, df_tmp$Online_boarding, method = "spearman")


my_function <- function(data, index) {
  cor(data$Satisfaction[index], data$Online_boarding[index], method = "spearman")
}

bootstrap_result <- boot(data = df %>% filter(Class == "Business"), statistic = my_function, R = 1000)
median_ci <- boot.ci(bootstrap_result, type = "basic")

#print(median_ci$basic[4:5])

lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

cat("Confidence Interval for the Correlation:", "\n")
cat("Correlation for Business Type(Satisfaction, Online_boarding):", correlation, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")