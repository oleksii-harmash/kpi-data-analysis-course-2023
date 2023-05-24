library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(boot)


df <- read_csv('airline.csv', show_col_types = FALSE)
df


#On the histogram, we can clearly see that most flights have a delay of 0 minutes, but these values make it difficult to see the picture for the flights that had a delay.
#Also, the x-axis suggests that there are some values (perhaps outliers) that narrow the plot a lot.
#Obviously, the distribution resembles an exponential one, so in order to see the full picture of this variable, let's logarithm it.

#*it is known that the logarithm from zero is equal to infinity, so we have two options - either discard the values equal to zero, or raise the x-axis by 1, we will choose the second
df$Departure_Delay_in_Minutes <- log(df$Departure_Delay_in_Minutes + 1)

#Looking at the graph of the logarithmic variable, one can see that despite the zero values (ln(1) == 0),
# the distribution looks normal.
#To better estimate the distribution, we discard the null values



data <- df %>% filter(Departure_Delay_in_Minutes > 0)
data <- data$Departure_Delay_in_Minutes
ggplot() + # filter out values <= 0
  geom_histogram(aes(x = data), bins = 15, color = "black") +
  xlab("Departure Delay (min)") +
  ylab("Num of passengers") +
  ggtitle("ln(delay) > 0") +
  theme(plot.title = element_text(hjust = 0.5))

med <- median(data)

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = data, statistic = my_function, R = 1000)

median_ci <- boot.ci(bootstrap_result, type = "basic")

print(median_ci$basic[4:5])


lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

#
cat("Confidence Interval for the Median (Percentile Method):", "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")


plot(density(data), main = "Density Plot", xlab = "Departure_Delay_in_Minutes")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)

#
plot(density(data), main = "Density Plot", xlim = c(2.4, 3.3), , xlab = "Departure_Delay_in_Minutes")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("bottomright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)





##############################################

##############################################

data <- df$Age
ggplot() + # filter out values <= 0
  geom_histogram(aes(x = data), bins = 15, color = "black") +
  xlab("Age") +
  ylab("Num of passengers") +
  ggtitle("ln(delay) > 0") +
  theme(plot.title = element_text(hjust = 0.5))

med <- median(data)

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = data, statistic = my_function, R = 1000)

median_ci <- boot.ci(bootstrap_result, type = "basic")

print(median_ci$basic[4:5])


lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

#
cat("Confidence Interval for the Median (Percentile Method):", "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")


plot(density(data), main = "Density Plot", xlab = "Age")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)

#
plot(density(data), main = "Density Plot", xlim = c(39.5, 40.7),xlab = "Age")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("bottomright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)



##############################################

##############################################

data <- df$Flight_Distance
ggplot() + # filter out values <= 0
  geom_histogram(aes(x = data), bins = 15, color = "black") +
  xlab("Flight_Distance") +
  ylab("Num of passengers") +
  ggtitle("ln(delay) > 0") +
  theme(plot.title = element_text(hjust = 0.5))

med <- median(data)

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = data, statistic = my_function, R = 1000)

median_ci <- boot.ci(bootstrap_result, type = "basic")

print(median_ci$basic[4:5])


lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

cat("Confidence Interval for the Median (Percentile Method):", "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")


plot(density(data), main = "Density Plot", xlab = "Flight_Distance")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)

#
plot(density(data), main = "Density Plot", xlim = c(838, 852), xlab = "Flight_Distance")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("bottomright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)


##############################################

##############################################


df$Arrival_Delay_in_Minutes <- log(df$Arrival_Delay_in_Minutes + 1)


df<- df %>% filter(Arrival_Delay_in_Minutes != 0)
data <- df$Arrival_Delay_in_Minutes
ggplot() + # filter out values <= 0
  geom_histogram(aes(x = data), bins = 15, color = "black") +
  xlab("Arrival_Delay_in_Minutes") +
  ylab("Num of passengers") +
  ggtitle("ln(delay) > 0") +
  theme(plot.title = element_text(hjust = 0.5))

med <- median(data)

my_function <- function(data, index) {

  median(data[index])
}

bootstrap_result <- boot(data = data, statistic = my_function, R = 1000)

median_ci <- boot.ci(bootstrap_result, type = "basic")

print(median_ci$basic[4:5])


lower_bound <- median_ci$basic[4]
upper_bound <- median_ci$basic[5]

#
cat("Confidence Interval for the Median (Percentile Method):", "\n")
cat("Median:", med, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")


plot(density(data), main = "Density Plot", xlab = "Arrival_Delay_in_Minutes")
abline(v = med, col = "red", lwd = 2, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)

#
plot(density(data), main = "Density Plot", xlim = c(2.4, 3.3), , xlab = "Arrival_Delay_in_Minutes")
abline(v = med, col = "yellow", lwd = 10, lty = 2)
abline(v = lower_bound, col = "blue", lwd = 2, lty = 2)
abline(v = upper_bound, col = "blue", lwd = 2, lty = 2)
legend("bottomright", legend = c("Median", "Lower Bound", "Upper Bound"),
       col = c("red", "blue", "blue"), lwd = 2, lty = 2)




##############################################

##############################################