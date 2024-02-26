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
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/7b525194-dfc8-46eb-9b4c-3273bc11dc14)


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
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/8d8d211e-5c78-4b7b-a9ef-aae51413f00b)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/a2f78f69-ee2c-41ee-9dab-e4eb55a7c24a)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/c66ad1a2-0a6f-4bc4-9a81-1b0668f846bf)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/10ea4a6c-2ef8-4406-b06b-0c121ef710fb)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/ed97c2bd-8e63-451b-8a2e-45253e7e003c)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/361a8253-daae-4492-98c6-6d4446cb8ee7)


```{r, fig.width=17, fig.height=16}
plot_service('Online_boarding', 'red', 'Reds')
hypotest('Online_boarding')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1424004e-74a3-4bae-ab0a-16068d91162d)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/05fade30-80fe-4706-a84f-41b0bbb24155)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/eaf9275e-f1ba-45cb-8b41-c8652dbb3ae7)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/c72c9aa6-a9ec-4369-87f7-c81b8f723835)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/b85f0703-42d8-47e0-9247-bd8ffa57bfc8)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/ba07498f-9c00-4f3b-b4b9-9aa7c9a9a124)

```{r, fig.width=17, fig.height=16}
plot_service('Food_and_drink', '#056552', 'BuGn')
hypotest('Food_and_drink')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d702c95b-6753-4a92-a5cf-ed0d4b01f0a9)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/4206fc13-1b7c-421a-9390-77dec903f6a9)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1422c701-0c3b-471f-ba2b-4320a2bc9cf4)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/81e03b00-3755-40fc-b6bd-89477efac959)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/09de0cc6-b7c2-4672-83d5-925763ed763f)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d319984a-b32c-4cb5-96ed-3cedfa3cffa6)

```{r, fig.width=17, fig.height=16}
plot_service('Ease_of_Online_booking', '#B662C7', 'Purples')
hypotest('Ease_of_Online_booking')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/f6361c34-027e-4641-95f6-5878327b1f9b)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1b458113-0ed1-4716-8017-80be426c90b2)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/32ca925e-fa40-49f8-8dec-22ba8b73b278)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9755e56a-a6eb-493c-9331-fb71d68e60aa)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/2a31a880-e834-40a9-8f0c-42629160d1e5)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/85f3f712-1043-4c19-9627-0b90feb06191)

```{r, fig.width=17, fig.height=16}
plot_service('Inflight_entertainment', '#1CDF33', 'YlGn')
hypotest('Inflight_entertainment')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/05500918-2317-46b9-8021-628105c1f4b2)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/2a5526c8-8147-4092-bb65-b87cac7a5ef3)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/8186f710-c9f0-4160-a8da-f36ebb63e887)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/482a66a5-676e-4bc9-909a-acb21fc8e3a9)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/ca2af19b-2002-409b-9fb4-59cf7fa8b014)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/db560d9c-2c5b-420c-8847-3b56a1cec7e1)

```{r, fig.width=17, fig.height=16}
plot_service('Time_convenient', '#C01266', 'RdPu')
hypotest('Time_convenient')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/f8bb4280-ffc9-4809-822e-ec7000b38217)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1b22e234-f67d-4a50-bb7a-0d2a94841aea)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/a5f25707-f85c-4272-9c84-b6e3d29244ef)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/e73d1d01-fcf5-429a-90b0-92094d1f4c7a)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9b318afc-aad4-444d-8bcd-c740bc8392ce)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/a06147fa-1ed3-41c6-8af3-3967dda67e85)

```{r, fig.width=17, fig.height=16}
plot_service('Cleanliness', '#1DA7DB', 'PuBu')
hypotest('Cleanliness')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/5808348a-cc16-49fe-9ea5-117e69288087)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/2412f38a-0f97-45dd-b783-4cd6dbd46b72)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/07a7f513-c0b2-4f03-b23b-df735a30b27c)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/e93c219c-d7d5-4e62-b723-cf520cd928aa)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/f449b218-6261-447f-bb3c-6c857edb7e29)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9ac6a2ca-5fd2-407e-9f90-a05c48e6eb25)
