```{r}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library('gplots')
library(binom)
library('ggridges')
library(RColorBrewer)
library(wesanderson)
library('plotly')
library('ggridges')
library('ggthemes')
library('patchwork')
library('GGally')
library("ggpubr")
library('boot')
```


```{r}
df <- read_csv('airline.csv', show_col_types = FALSE)
target = select(df, "Satisfaction")
df <- subset(df, select = - c(1, 2, 26))
colnames(df)[1] ="id"
df <- cbind(df, C = target)
```

```{r}
View(df)
```


```{r, fig.width=5, fig.height=6}
data <- data.frame()
for (group in c('Underage', 'Middle age', 'Aged')) {
  age_df = df[df$Age_group == group,]
  ci <- binom.confint(nrow(age_df[age_df$Satisfaction == 1,]), nrow(age_df), conf.level=0.95, methods = 'wilson')
  row <- data.frame(group = group, mean = ci$mean, lower_ci = ci$lower, upper_ci = ci$upper)
  data <- rbind(data, row)
}

ggplot(data, aes(x = group, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .9,) +
  labs(x = "Age Group", y = "Proportion") +
  geom_bar(stat = 'identity', width = 0.9, alpha=0.4, position='dodge', fill='#F1BDAD', color=alpha('#F1BDAD', 1))

age_comb <- combn(c('Underage', 'Middle age', 'Aged'), 2)

for (i in (1:3)) {
  print(paste0('Two side test between ',  age_comb[, i][1], ' and ',  age_comb[, i][2]))
  print(prop.test(x = c(nrow(df[df$Age_group == age_comb[, i][1] & df$Satisfaction == 1,]), 
                 nrow(df[df$Age_group == age_comb[, i][2] & df$Satisfaction == 1,])), 
           n = c(nrow(df[df$Age_group == age_comb[, i][1],]),
                 nrow(df[df$Age_group == age_comb[, i][2],])),
           conf.level = 0.95,
           alternative = 'two.sided'))
}
```

```{r}
plot_age_group <- function(feature, estimate, fill) {
  data <- data.frame()
  for (group in c('Underage', 'Middle age', 'Aged')) {
    age_df = df[df$Age_group == group & df[feature] == estimate,]
    ci <- binom.confint(nrow(age_df[age_df$Satisfaction == 1,]), nrow(age_df), conf.level=0.95, methods = 'wilson')
    row <- data.frame(group = group, mean = ci$mean, lower_ci = ci$lower, upper_ci = ci$upper)
    data <- rbind(data, row)
  }
  data$group <- factor(data$group, levels = c('Underage', 'Middle age', 'Aged'))
  fig <- ggplot(data, aes(x = group, y = mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .9) +
    labs(x = "Age Group", y = "Obs. proportion", title = paste0('Service ratings: ', estimate)) +
    theme(plot.title = element_text(size = 11)) +
    geom_bar(stat = 'identity', width = 0.9, alpha=0.2, position='dodge', fill = fill, color = alpha(fill, 0.8))
  print(data)
  return(fig)
}
```

```{r, fig.width=15, fig.height=8}
plot_service <- function(feature, fill, palette) {
  plot_list <- list()
  for (i in 1:5) {
  plot <- plot_age_group(feature, i, fill)
  plot_list[[i]] <- plot
  }
  combined_plot <- plot_list[[1]]
  for (i in 2:4) {
    combined_plot <- combined_plot + plot_list[[i]]
  }
  combined_plot <- combined_plot - plot_list[[5]]
  
  ridgeplot <- ggplot(data=df[df[feature] > 0,], aes_string(x = "Age", y = paste0("factor(", feature, ")"), fill = paste0("factor(", feature, ")"))) + 
    stat_density_ridges(geom = "density_ridges_gradient", rel_min_height = 0.009, scale = 2.5, alpha=0.5) +
    scale_fill_brewer(palette = palette, aesthetics = 'fill') +
    scale_x_continuous(breaks = seq(0, 80, by = 10)) +
    labs(x = "Age", y = paste0(feature)) + 
    guides(fill = guide_legend(title = paste0(feature))) + 
    theme_ridges() +
    theme(plot.title = element_text(hjust = 0.5, size=15, face='italic'),
          axis.title.y = element_text(hjust = 0.5, size=15, margin = margin(r=10)), 
          axis.title.x = element_text(hjust = 0.5, margin = margin(t=20), size=20))
  
  combined_plot <- combined_plot / ridgeplot
  combined_plot <- combined_plot + plot_annotation(title = paste0('CI for observations proportion of ', feature),
                                                   theme = theme(plot.title = element_text(hjust=0.5)))
  combined_plot
}
```


```{r}
hypotest <- function(feature) {
  for (i in (1:5)) {
    for (j in (1:3)) {
      print(paste0('Service rating: ', i, ' Two sided prop.test between ', age_comb[, j][1], ' and ', age_comb[, j][2]))
      print(prop.test(x = c(nrow(df[df$Age_group == age_comb[, j][1] & df[feature] == i & df$Satisfaction == 1,]),
                            nrow(df[df$Age_group == age_comb[, j][2] & df[feature] == i & df$Satisfaction == 1,])),
                      n = c(nrow(df[df$Age_group == age_comb[, j][1] & df[feature] == i,]),
                            nrow(df[df$Age_group == age_comb[, j][2] & df[feature] == i,])), 
                      conf.level = 0.95,
                      alternative = 'two.sided'))
    }
  }
}
```
```{r}
age_comb[, 3][2]
```

```{r, fig.width=17, fig.height=16}
plot_service('Seat_comfort', 'blue', 'Blues')
hypotest('Seat_comfort')
```

```{r, fig.width=17, fig.height=16}
plot_service('Online_boarding', 'red', 'Reds')
hypotest('Online_boarding')
```
```{r, fig.width=17, fig.height=16}
plot_service('Food_and_drink', '#056552', 'BuGn')
hypotest('Food_and_drink')
```

```{r, fig.width=17, fig.height=16}
plot_service('Ease_of_Online_booking', '#B662C7', 'Purples')
hypotest('Ease_of_Online_booking')
```

```{r, fig.width=17, fig.height=16}
plot_service('Inflight_entertainment', '#1CDF33', 'YlGn')
hypotest('Inflight_entertainment')
```
```{r, fig.width=17, fig.height=16}
plot_service('Time_convenient', '#C01266', 'RdPu')
hypotest('Time_convenient')
```
```{r, fig.width=17, fig.height=16}
plot_service('Cleanliness', '#1DA7DB', 'PuBu')
hypotest('Cleanliness')
```