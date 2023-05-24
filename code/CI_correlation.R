library(tidyverse)
library(dplyr)
library(ggplot2)
library(gplots)
library(reshape2)
library(boot)

df <- read_csv('airline.csv', show_col_types = FALSE)
df


business_travels <- df$Flight_Distance[df$Type_of_Travel == 'Business Travel']
personal_travels <- df$Flight_Distance[df$Type_of_Travel == 'Personal Travel']

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = business_travels, statistic = my_function, R = 100)
business_ci <- boot.ci(bootstrap_result, type = "basic")
medians_mark_1 <- bootstrap_result$t

bootstrap_result <- boot(data = personal_travels, statistic = my_function, R = 100)
personal_ci <- boot.ci(bootstrap_result, type = "basic")
medians_mark_2 <- bootstrap_result$t

info_data <- data.frame(y = c(median(business_travels), median(personal_travels)),
                       x = c('Business', 'Personal'),
                       lower = c(business_ci$basic[4], personal_ci$basic[4]),
                       upper = c(business_ci$basic[5], personal_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Type_of_Travel") +
  ylab("Flight_Distance")



print("Customers from Business Type_of_Travel have bigger Flight_Distance")
wilcox.test(medians_mark_1, medians_mark_2)


mark_1 <- df$Flight_Distance[df$Seat_comfort == 1]
mark_2 <- df$Flight_Distance[df$Seat_comfort == 2]
mark_3 <- df$Flight_Distance[df$Seat_comfort == 3]
mark_4 <- df$Flight_Distance[df$Seat_comfort == 4]
mark_5 <- df$Flight_Distance[df$Seat_comfort == 5]

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = mark_1, statistic = my_function, R = 300)
m1_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_2, statistic = my_function, R = 300)
m2_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_3, statistic = my_function, R = 300)
m3_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_4, statistic = my_function, R = 300)
m4_ci <- boot.ci(bootstrap_result, type = "basic")
medians_mark_1 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_5, statistic = my_function, R = 300)
m5_ci <- boot.ci(bootstrap_result, type = "basic")
medians_mark_2 <- bootstrap_result$t

info_data <- data.frame(y = c(median(mark_1),
                              median(mark_2),
                              median(mark_3),
                              median(mark_4),
                              median(mark_5)),
                        x = c('1', '2', '3', '4', '5'),
                        lower = c(m1_ci$basic[4],
                                  m2_ci$basic[4],
                                  m3_ci$basic[4],
                                  m4_ci$basic[4],
                                  m5_ci$basic[4]),
                        upper = c(m1_ci$basic[5],
                                  m2_ci$basic[5],
                                  m3_ci$basic[5],
                                  m4_ci$basic[5],
                                  m5_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Seat_comfort") +
  ylab("Flight_Distance")

print("Customers give mark 4 for seat comfort on longer Flight_Distance then 5")
wilcox.test(medians_mark_1, medians_mark_2, alternative = "greater")



mark_0 <- df$Arrival_Delay_in_Minutes[df$Satisfaction == 0 & df$Arrival_Delay_in_Minutes != 0]
mark_1 <- df$Arrival_Delay_in_Minutes[df$Satisfaction == 1 & df$Arrival_Delay_in_Minutes != 0]


my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = mark_0, statistic = my_function, R = 300)
m0_ci <- boot.ci(bootstrap_result, type = "basic")
medians_mark_1 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_1, statistic = my_function, R = 300)
m1_ci <- boot.ci(bootstrap_result, type = "basic")
medians_mark_2 <- bootstrap_result$t


info_data <- data.frame(y = c(median(mark_0),
                              median(mark_1)),
                        x = c('0', '1'),
                        lower = c(m0_ci$basic[4],
                                  m1_ci$basic[4]),
                        upper = c(m0_ci$basic[5],
                                  m1_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Satisfaction") +
  ylab("Arrival_Delay_in_Minutes")


print("Customers with heighter Arrival_Delay less satisfacted")
wilcox.test(medians_mark_1, medians_mark_2, alternative = "greater")





mark_1 <- df$Flight_Distance[df$Inflight_wifi_service == 1 & (df$Class == 'Eco' | df$Class == 'Eco Plus')]
mark_2 <- df$Flight_Distance[df$Inflight_wifi_service == 2 & (df$Class == 'Eco' | df$Class == 'Eco Plus')]
mark_3 <- df$Flight_Distance[df$Inflight_wifi_service == 3 & (df$Class == 'Eco' | df$Class == 'Eco Plus')]
mark_4 <- df$Flight_Distance[df$Inflight_wifi_service == 4 & (df$Class == 'Eco' | df$Class == 'Eco Plus')]
mark_5 <- df$Flight_Distance[df$Inflight_wifi_service == 5 & (df$Class == 'Eco' | df$Class == 'Eco Plus')]

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = mark_1, statistic = my_function, R = 300)
m1_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_2, statistic = my_function, R = 300)
m2_ci <- boot.ci(bootstrap_result, type = "basic")
vold_1 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_3, statistic = my_function, R = 300)
m3_ci <- boot.ci(bootstrap_result, type = "basic")
vold_2 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_4, statistic = my_function, R = 300)
m4_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_5, statistic = my_function, R = 300)
m5_ci <- boot.ci(bootstrap_result, type = "basic")

info_data <- data.frame(y = c(median(mark_1),
                              median(mark_2),
                              median(mark_3),
                              median(mark_4),
                              median(mark_5)),
                        x = c('1', '2', '3', '4', '5'),
                        lower = c(m1_ci$basic[4],
                                  m2_ci$basic[4],
                                  m3_ci$basic[4],
                                  m4_ci$basic[4],
                                  m5_ci$basic[4]),
                        upper = c(m1_ci$basic[5],
                                  m2_ci$basic[5],
                                  m3_ci$basic[5],
                                  m4_ci$basic[5],
                                  m5_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Inflight_wifi_service") +
  ylab("Flight_Distance")+
  labs(title = "Eco group")

print("Customers give mark 3 for Inflight_wifi_service on heighter Flight_Distance then 2")
wilcox.test(vold_1, vold_2, alternative = "greater")




mark_1 <- df$Age[df$Online_boarding == 1 & (df$Class == 'Business')]
mark_2 <- df$Age[df$Online_boarding == 2 & (df$Class == 'Business')]
mark_3 <- df$Age[df$Online_boarding == 3 & (df$Class == 'Business')]
mark_4 <- df$Age[df$Online_boarding == 4 & (df$Class == 'Business')]
mark_5 <- df$Age[df$Online_boarding == 5 & (df$Class == 'Business')]

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = mark_1, statistic = my_function, R = 300)
m1_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_2, statistic = my_function, R = 300)
m2_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_3, statistic = my_function, R = 300)
m3_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_4, statistic = my_function, R = 300)
m4_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_5, statistic = my_function, R = 300)
m5_ci <- boot.ci(bootstrap_result, type = "basic")

info_data <- data.frame(y = c(median(mark_1),
                              median(mark_2),
                              median(mark_3),
                              median(mark_4),
                              median(mark_5)),
                        x = c('1', '2', '3', '4', '5'),
                        lower = c(m1_ci$basic[4],
                                  m2_ci$basic[4],
                                  m3_ci$basic[4],
                                  m4_ci$basic[4],
                                  m5_ci$basic[4]),
                        upper = c(m1_ci$basic[5],
                                  m2_ci$basic[5],
                                  m3_ci$basic[5],
                                  m4_ci$basic[5],
                                  m5_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Online_boarding") +
  ylab("Age") +
  labs(title = "Business group")







mark_1 <- df$Age[df$Inflight_entertainment == 1 & (df$Class == 'Business')]
mark_2 <- df$Age[df$Inflight_entertainment == 2 & (df$Class == 'Business')]
mark_3 <- df$Age[df$Inflight_entertainment == 3 & (df$Class == 'Business')]
mark_4 <- df$Age[df$Inflight_entertainment == 4 & (df$Class == 'Business')]
mark_5 <- df$Age[df$Inflight_entertainment == 5 & (df$Class == 'Business')]

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = mark_1, statistic = my_function, R = 300)
m1_ci <- boot.ci(bootstrap_result, type = "basic")
vold_1 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_2, statistic = my_function, R = 300)
m2_ci <- boot.ci(bootstrap_result, type = "basic")
vold_2 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_3, statistic = my_function, R = 300)
m3_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_4, statistic = my_function, R = 300)
m4_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_5, statistic = my_function, R = 300)
m5_ci <- boot.ci(bootstrap_result, type = "basic")

info_data <- data.frame(y = c(median(mark_1),
                              median(mark_2),
                              median(mark_3),
                              median(mark_4),
                              median(mark_5)),
                        x = c('1', '2', '3', '4', '5'),
                        lower = c(m1_ci$basic[4],
                                  m2_ci$basic[4],
                                  m3_ci$basic[4],
                                  m4_ci$basic[4],
                                  m5_ci$basic[4]),
                        upper = c(m1_ci$basic[5],
                                  m2_ci$basic[5],
                                  m3_ci$basic[5],
                                  m4_ci$basic[5],
                                  m5_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Inflight_entertainment") +
  ylab("Age")+
  labs(title = "Business group")

print("Customers who give mark 1 for Inflight_entertainment are younger than customers who give mark 2 (Business group) ")
wilcox.test(vold_1, vold_2, alternative = "greater")




mark_1 <- df$Flight_Distance[df$Inflight_entertainment == 1 & (df$Class == 'Business')]
mark_2 <- df$Flight_Distance[df$Inflight_entertainment == 2 & (df$Class == 'Business')]
mark_3 <- df$Flight_Distance[df$Inflight_entertainment == 3 & (df$Class == 'Business')]
mark_4 <- df$Flight_Distance[df$Inflight_entertainment == 4 & (df$Class == 'Business')]
mark_5 <- df$Flight_Distance[df$Inflight_entertainment == 5 & (df$Class == 'Business')]

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = mark_1, statistic = my_function, R = 300)
m1_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_2, statistic = my_function, R = 300)
m2_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_3, statistic = my_function, R = 300)
m3_ci <- boot.ci(bootstrap_result, type = "basic")

bootstrap_result <- boot(data = mark_4, statistic = my_function, R = 300)
m4_ci <- boot.ci(bootstrap_result, type = "basic")
vold_1 <- bootstrap_result$t

bootstrap_result <- boot(data = mark_5, statistic = my_function, R = 300)
m5_ci <- boot.ci(bootstrap_result, type = "basic")
vold_2 <- bootstrap_result$t

info_data <- data.frame(y = c(median(mark_1),
                              median(mark_2),
                              median(mark_3),
                              median(mark_4),
                              median(mark_5)),
                        x = c('1', '2', '3', '4', '5'),
                        lower = c(m1_ci$basic[4],
                                  m2_ci$basic[4],
                                  m3_ci$basic[4],
                                  m4_ci$basic[4],
                                  m5_ci$basic[4]),
                        upper = c(m1_ci$basic[5],
                                  m2_ci$basic[5],
                                  m3_ci$basic[5],
                                  m4_ci$basic[5],
                                  m5_ci$basic[5]))


ggplot(info_data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  xlab("Inflight_entertainment") +
  ylab("Flight_Distance")+
  labs(title = "Business group")

print("Customers give mark 4 for Inflight_entertainment on heighter Flight_Distance than 5 (Business group)")
wilcox.test(vold_1, vold_2, alternative = "greater")