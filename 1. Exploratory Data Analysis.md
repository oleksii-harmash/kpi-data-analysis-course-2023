```R
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(wesanderson)
library('viridis')a
library('plotly')
library('ggridges')
library('ggthemes')
library('hrbrthemes')
library('patchwork')
library('GGally')
library("lattice")
library("ggpubr")
import_roboto_condensed()
```



```R
test <- read_csv("airline_satisfaction.csv", show_col_types = FALSE)
train <- read_csv("airline_satisfaction2.csv", show_col_types = FALSE)
```




    Concatenation test & train datasets
    df <- rbind(test, train)


## 1. Data Engineering

Dataset overview

    head(df, 3)

![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/83b3ce2f-0034-40a0-a8c4-d734560c10e8)

#### 1.1 Data types & features

```{r}
# видаляємо непотрібний стовпець 'Unnamed: 0'
df <- subset(df, select = - c(1))
# призначимо 'user id' в якості індексу
df <- df[order(df$id),]
df <- df %>% column_to_rownames(., var = 'id')
```

```{r}
dim(df)
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9b267786-6bb3-4bfd-a963-39e1e93c8dd6)

```{r}
str(df)
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/5a95c356-fce5-43c8-851c-b89f9894ffae)

*деякі стовпці мають строковий тип замість числового*

Перейменування стовпців та категорій:

```{r}
# замінимо пробіли на '_' для облегшення виклику функцій
colnames(df) <- gsub(" ", "_", colnames(df))
colnames(df)[colnames(df) == "satisfaction"] <- "Satisfaction"
colnames(df)[colnames(df) == "Departure/Arrival_time_convenient"] <- "Time_convenient"
df$Type_of_Travel <- ifelse(df$Type_of_Travel == "Business travel", "Business Travel", df$Type_of_Travel)
df$Customer_Type <- ifelse(df$Customer_Type == "disloyal Customer", "Disloyal Customer", df$Customer_Type)
```

Змінимо тип змінної *Satisfaction* на числовий шляхом кодування категорій

```{r}
unique(df$Satisfaction)
df$Satisfaction <- ifelse(df$Satisfaction == "neutral or dissatisfied", 0, 1)
df$Satisfaction <- as.integer(df$Satisfaction)
```
```{r}
write.csv(df, "airline_tidy.csv")
```

#### 1.2 Handling missing data

```{r}
df <- read_csv('airline_tidy.csv', show_col_types = FALSE)
```

Знайдемо змінні, що мають відсоток (%) пропущених даних більший за нуль:

```{r}
df_nan <- colSums(is.na(df)) * 100 / nrow(df)
df_nan <- df_nan[df_nan > 0]
df_nan <- df_nan[order(df_nan)]
df_nan
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/a64f02b4-f2b7-4cad-9db2-e44d236d90fa)

```{r}
nrow(df) * df_nan / 100
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d72c70ef-a5c3-4329-b95c-0d7279a8cd14)

Змінна *Arrival_Delay_in_Minutes* має 0.3 % пропущених даних. В цьому випадку маємо два шляхи обробки пропущених даних.

1.  Заповнити N/A дані значеннями моди/медіани відповідного стовпця

2.  Заповнити N/A дані значеннями зі змінної *Departure_Delay_in_Minutes*

Щоб обрати правильний, спочатку знайдемо різницю (в хв.) між змінними *Arrival_Delay_in_Minutes* та *Departure_Delay_in_Minutes*, для цього створимо відповідну змінну *Delay_overtake*, яка міститиме цю різницю.

```{r}
df$Delay_overtake <- df$Departure_Delay_in_Minutes - df$Arrival_Delay_in_Minutes
sort(table(df$Delay_overtake), decreasing = TRUE)[1:5]
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/614704d2-0fbd-4f9b-bfd9-d651c36b01f1)

Приблизно половина рейсів записаних в датасеті має різницю між *Arrival_Delay_in_Minutes* та *Departure_Delay_in_Minutes* в 0 хвилин.

```{r}
median(table(df$Arrival_Delay_in_Minutes))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/75abc6af-3e9e-4ab5-a748-56039c53e930)

В той же час, медіанне значення дорівнює 7 хвилинам, що при заповненні ним пропущених даних може створити не існуючі раніше залежності, що приведе до неправильних результатів аналізу
Отже заповнимо пропущенні значення стовпця *Arrival_Delay_in_Minutes* відповідними значеннями з *Departure_Delay_in_Minutes*

```{r}
df$Arrival_Delay_in_Minutes <- ifelse(is.na(df$Arrival_Delay_in_Minutes), df$Departure_Delay_in_Minutes, df$Arrival_Delay_in_Minutes)
df$Delay_overtake <- df$Arrival_Delay_in_Minutes - df$Departure_Delay_in_Minutes
```

Попередньо перевіримо деякі кореляції між змінними, щоб у випадку коефіцієнта кореляції рівному 1, видалити одну зі змінних, у зв'язку з мультиколінеарністю.

```{r}
print(cor(df$Arrival_Delay_in_Minutes, df$Departure_Delay_in_Minutes))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/533050d7-a448-45e5-8fa0-2d53de421cf0)

Перевіримо категорійні змінні на валідність (кожна змінна з опитування має 5-6 можливих варіантів відповідей)

```{r}
sapply(df, function(x) length(unique(x)))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d3c6ce7b-ceb3-4d23-89bd-5155a47ab212)

Збережемо на даному моменті датасет, приведений до охайного вигляду та без пропущених значень:

```{r}
write.csv(df, "airline_missing.csv")
```

#### 1.3 Outlier detection

```{r}
df <- read_csv('airline_missing.csv', show_col_types = FALSE)
```

Категорійні змінні представлені в даному датасеті фактично не мають можливості мати викиди. Тому проаналізуємо числові змінні.

**Departure Delay in Minutes** **(Arrival Delay in Minutes)**

```{r, fig.height=6, fig.width=12}
ggplot(data = df, aes(x = Departure_Delay_in_Minutes)) +
  geom_histogram(bins = 100, color = "#373737", fill='#53D46B', alpha=1) +
  geom_rug(color = '#53D46B') + 
  labs(x="Departure delay (min)", y="Num of passengers", title="Exponential distribution", subtitle="Most flights were without delays") + 
  scale_x_continuous(breaks = seq(0, 1500, by = 250)) + 
  theme_modern_rc() + 
  theme(axis.title.y = element_text(hjust = 0.5, size=13, margin = margin(r=10)), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(r=15), size=13))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/7b4c4a44-4a60-4fac-a2c3-409667df4654)

На гістограмі ми чітко бачимо що більшість рейсів мають затримку в 0 хвилин, але ці значення заважають побачити картину для рейсів, що мали затримку.\
Крім того вісь *x* підказує, що є деякі значення (можливо викиди), що дуже звужують графік.\
Очевидно розподіл нагадує експоненційний, тому для того, щоб побачити повноцінну картину цієї змінної - логаритмуємо її.

*відомо що логаритм від нуля дорівнює нескінченності тому в нас є два варіанти - або відкинути значення рівні нулеві, або підняти вісь х на 1, виберемо другий*

```{r, fig.height=6, fig.width=10}
df$Departure_Delay_in_Minutes <- log(df$Departure_Delay_in_Minutes)
ggplot(data = df) +
  geom_histogram(aes(x = Departure_Delay_in_Minutes), color = "#373737", fill='#53D46B', binwidth = 0.3) +
  labs(x="Departure delay log", y="Num of passengers log", title="Variable after logarithmization") + 
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=15, face='italic'), 
        axis.title.y = element_text(hjust = 0.5, margin = margin(r=10), size=13), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), size=13))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/6c92aadd-680d-469b-855a-e434e9c16dbc)

Дивлячись на графік логаритмованої змінної видно, що, незважаючи на нульові значення\
*(ln(1) == 0)*, розподіл виглядає нормальним. Але значення що більше 0 та менше 1 псують картину і здається що вони є викидами.
Перевіримо це застосувавши IQR метод для визначення викидів

```{r}
q <- quantile(df$Departure_Delay_in_Minutes[df$Departure_Delay_in_Minutes >= 0], c(0.25, 0.75))
iqr <- IQR(df$Departure_Delay_in_Minutes[df$Departure_Delay_in_Minutes >= 0])
lower_range <- q[1] - (1.5 * iqr)
upper_range <- q[2] + (1.5 * iqr)
```

```{r}
cat('Q1: ', round(q[1], 3), 'Q3: ', round(q[2], 3), '\n')
cat('Lower range: ', round(lower_range, 3), 'Upper range: ', round(upper_range, 3))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/2a08b236-45e7-4bd6-8b51-ecb0c14f3298)

Аналізуючи отримані величини границь зрозуміло що значень затримок рейсів менше за 0 немає, тому сенсу обрізати датасет вище нижньої межи теж не існує.\
Проте верхня межа має значення 6.535. Всі значення що йдуть вище фактично псували нам початковий графік.\
Подивимося кількість значень, що перевищують верхню межу:

```{r}
nrow(df %>% filter(Departure_Delay_in_Minutes > upper_range))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/2d382f17-9dcf-461c-b2d4-1b2a330a4ceb)

Значень, що перевищують верхню межу: 20. Хоча рейси і можуть затримуватися іноді на 26 годин, але таке відбувається дуже рідко.\
Відносно величини датасету ця оцінка статистично незначуща, тому видалимо дані значення для коректності подальшого дослідження і повернемося до початкової системи координат.

```{r}
df[df$Departure_Delay_in_Minutes > upper_range,] <- NA
df <- subset(df, Departure_Delay_in_Minutes <= upper_range)
df$Departure_Delay_in_Minutes <- exp(df$Departure_Delay_in_Minutes)
```

```{r, fig.height=6, fig.width=10}
ggplot(data = df) + # filter out missing values
  geom_histogram(aes(x = Departure_Delay_in_Minutes), bins = 60, color = "#373737", fill='#53D46B') +
  labs(x="Departure delay", y="Num of passengers", title="Distribution after removal of emissions") + 
  scale_x_continuous(breaks = seq(0, 600, by = 100)) + 
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face='italic'), 
        axis.title.y = element_text(hjust = 0.5, margin = margin(r=10), size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), size=12))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/89c58e6e-cc2f-445e-b08f-69f67184c954)

**Age**

```{r, fig.height=6, fig.width=10}
ggplot(data = df %>% filter(!is.na(Age)), aes(x = Age)) +
  geom_histogram(bins = 79, fill = "#53D46B", colour='#373737', alpha=1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) + 
  labs(x="Age", y="Passengers", title="Approximately bimodal distribution") +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face='italic'), 
        axis.title.y = element_text(hjust = 0.5, margin = margin(r=10), size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), size=12)) 
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d5e90c33-ef53-47b7-98b2-c32e64b2f1e2)

Гістограма розподілу віку пасажирів має характерний для цієї змінної приблизно нормальний розподіл.\
Дослідження інших цікавих речей пов'язані з віком представлене у другому розділі - *EDA*.

**Flight Distance**

```{r, fig.height=6, fig.width=10}
ggplot(data = df %>% filter(!is.na(Flight_Distance))) + # filter out missing values
  geom_histogram(aes(x = Flight_Distance), bins = 80, fill = "#53D46B", colour='#373737') +
  labs(x="Flight Distance", y="Passengers", title="Approximately bimodal distribution") +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000)) + 
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face='italic'), 
        axis.title.y = element_text(hjust = 0.5, margin = margin(r=10), size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), size=12)) 
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/3b99bd44-03d3-4c69-8ffd-4fedd06d689b)

За гістограмою розподілу бачимо, що більшість рейсів авіакомпанія проводить на відстань до 1 тис км, в то же час кількість рейсів від 1 до 4 тис км теж достатньо велика.\
Це може свідчити про те, що компанія пропонує різні перельоти, як пасажирські так і бізнес(?).\
Але присутні деякі значення, які виділяються на фоні інших (значення близько 5000 тис км.)

За аналогією зі змінною *Departure Delay* застосуємо IQR метод:

```{r}
df$Flight_Distance <- log(df$Flight_Distance)
```

```{r, fig.width=13, fig.height=5}
p1 <- ggplot(df %>% filter(!is.na(Flight_Distance)), aes(x = Flight_Distance)) +
  geom_histogram(binwidth = 0.1, fill='#53D46B', colour='black') +
  labs(x = 'Flight Distance', y = 'Count') +
  theme_modern_rc() + 
  theme(axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10))) 
p2 <- ggplot(df %>% filter(!is.na(Flight_Distance)), aes(y = Flight_Distance)) +
  geom_boxplot(size = 0.8, fill='white', colour='#8C99A2', outlier.colour = '#53D46B', outlier.size=3, ) +
  labs(x = 'Flight Distance', y = '') +
  theme_modern_rc() +
  coord_flip() + 
  theme(axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10))) 
gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(2, 2))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/070cfa51-6843-4a65-9681-9ec3d94fa3aa)

```{r}
Q <- quantile(df$Flight_Distance, c(0.25, 0.75), na.rm = TRUE)
Q1 <- Q[1]
Q3 <- Q[2]
IQR <- Q3 - Q1
lower_range <- Q1 - (1.5 * IQR)
upper_range <- Q3 + (1.5 * IQR)
```

```{r}
cat(paste('Q1: ', round(Q1, 3), 'Q3: ', round(Q3, 3), '\n'))
cat(paste('Lower range: ', round(lower_range, 3), 'Upper range: ', round(upper_range, 3)))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/3a10527d-280b-4fca-b535-4acfbad50500)

За результатами IQR методу виявлено 11 викидів, кожний з яких менший за нижню границю.

Давайте продивимося ці дані

```{r}
df[df$Flight_Distance < lower_range,][,]
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/8a04a527-d10e-465a-9893-377fde0d9a60)

Викиди мають однакове Flight_Distance у всіх 11 випадках, це свідчить що такий рейс реально був, але слід припустити, що відповідна відстань могла бути

записана неправильно, бо *exp(3.433987)* приблизно дорівнює 30 км, а це дуже мало для рейсу пасажирського літака. Тому видалимо дані викиди.

```{r}
df <- subset(df, Flight_Distance >= lower_range)
df$Flight_Distance <- exp(df$Flight_Distance)
```

Збережемо на даному моменті датасет, приведений до охайного вигляду та без пропущених значень.

```{r}
write.csv(df, "airline_outliers.csv")
```

# 2. Exploratory Data Analysis

```{r}
df <- read_csv('airline_outliers.csv', show_col_types = FALSE)
colnames(df)[2] ="id"
df <- subset(df, select = - c(1))
```

```{r}
t(summary(df[c('Age', 'Flight_Distance', 'Departure_Delay_in_Minutes', 'Arrival_Delay_in_Minutes', 'Delay_overtake')]))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/8b31c39d-d8b7-48f9-bc99-806677e7cbfb)

Підійдемо до розвідкового аналізу з двох боків.

-   Пройдемося по кожній змінній та спробуємо за допомогою візуалізації витягнути цікаву інформацію з даних, або ж нові дослідницькі питання

-   Опрацюємо та проаналізуємо дослідницькі питання, які були складені заздалегідь без будь-якого аналізу

Для загального розуміння дізнаємося яка сумарна кількість задоволених/незадоволених рейсом людей:

```{r, fig.height=6, fig.width=6}
ggplot(df, aes(x = Satisfaction, fill = factor(Satisfaction))) +
  geom_bar(stat='count', width = 1, colour='black') +
  coord_cartesian(ylim=c(0, 80000)) +
  scale_fill_manual("legend", values = c("0" = "#8C99A2", "1" = "#E3EDF5")) +
  geom_text(stat='count', aes(label=..count..), vjust=-1, color='white') +
  labs(x = "Satisfaction", y = "Passengers", title = "The difference is 17 thousand people") +
  guides(fill = guide_legend(title = "Satisfaction")) + 
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        axis.text.x=element_blank()) 
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/ffbb4ca0-3cbc-422e-8822-0d0b320da4f9)

```{r}
cat(sprintf("Dissatisfied: %.2f%%\n", nrow(df[df$Satisfaction == 0,]) * 100 / nrow(df)))
cat(sprintf("Satisfied: %.2f%%\n", nrow(df[df$Satisfaction == 1,]) * 100 / nrow(df)))
cat(sprintf("Difference: %.f\n", round(nrow(df) * (0.565 - 0.434))))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1231253b-f8dd-46ae-a22f-91817c4117ba)

Кількість задоволених рейсом людей менша за незадоволених приблизно на 13 відсотків, що становить аж 17 тисяч людей, але попри те, дані, в силу розміру, фактично зашумлені.

#### 2.1 Gender

Аналізуючи стать пасажирів, варто першим чином подивитися на співвідношення задоволеності відносно гендерних класів.

```{r, fig.height=5, fig.width=8}
ggplot(df, aes(x=Gender, fill=factor(Satisfaction))) +
  geom_bar(aes(y=after_stat(count)/tapply(after_stat(count), ..x.. ,sum)[..x..]), position="dodge", colour='black') + 
  coord_cartesian(ylim=c(0, 0.65)) +
  geom_text(aes( y=after_stat(count)/tapply(after_stat(count), ..x.. ,sum)[..x..], label=scales::percent(after_stat(count)/tapply(after_stat(count), ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5, colour='white') + 
  scale_fill_manual(name = "Satisfaction", values = c("#B1B6AE", "#53D46B"), labels = c("neutral or dissatisfied", "satisfied")) +
  ylab("Percent of passengers") +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(8)) + 
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/6d23591a-8995-45e2-8621-e3a1d09e62a3)

За графіком можна побачити, що кількість жінок та чоловіків задоволених рейсом приблизно однакова, проте жінок, що залишилися незадоволеними рейсом дещо більше за чоловіків. На даному кроці висновки робити зарано, бо така різниця може бути пов'язана з тим, що кількість жінок, що пройшли опитування просто перевищує кількість чоловіків.

```{r}
table(df$Gender)
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9b0f7e82-f288-4758-b633-9a02c2e2b2b9)

#### 2.2 Age

**Дослідницьке питання:** Чи існує залежність задоволеності рейсів від вікових категорій клієнтів?

**Гіпотеза:** Існують вікові категорії, що мають меншу частку задоволеності рейсом за інші

Побудуємо графік щільності розподілу віку пасажирів по двох категоріях: задоволений та незадоволений.

```{r, fig.height=7, fig.width=14}
ggplot(df, aes(x=Age, fill=factor(Satisfaction))) +
  geom_density(alpha=.75, bw = 1.4, color=NA) +
  scale_fill_manual(name='Satisfaction', ,values = c("#ffc8dd", "#bde0fe")) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) + 
  labs(x = "Age", y = "Density of passengers", title = "The growth of satisfaction begins after the age of 37") +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/34c18a6c-9007-4eae-baf8-c4956aeb5a73)

По графіку видно що частка задоволених пасажирів збільшується після умовної межі в 37 років

Також пасажири в межах 18-60 років сумарно мають приблизно однаковий відсоток задоволеності/незадоволеності, в той час як пасажири \< 18 років та \> 60 років здебільшого незадоволені.

Подивимося на конкретні значення:

```{r}
cat('Satisfied (18 < age < 60): ', sum(df$Satisfaction[df$Age > 18 & df$Age < 60] == 1), '\n')
cat('Dissatisfied (18 < age < 60): ', sum(df$Satisfaction[df$Age > 18 & df$Age < 60] == 0), '\n')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1c1f5141-b68c-47c2-a3c0-5a72e1df88e4)

```{r}
cat('Satisfied / amount of passengers: ', round(51163 * 100 / sum(df$Age > 18 & df$Age < 60), 2), '%', '\n')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9b313ab5-efcb-4dfd-82c1-d8db415c92bd)

Візуальні висновки в першому випадку підтвердилися, пасажири від 18 до 60 років мають 48.01 % відсоток задоволеності

```{r}
cat('Satisfied (age < 18 & age > 60): ', sum(df$Satisfaction[df$Age <= 18 | df$Age >= 60] == 1), '\n')
cat('Dissatisfied (age < 18 & age > 60): ', sum(df$Satisfaction[df$Age <= 18 | df$Age >= 60] == 0), '\n')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/ae068ab3-f0ac-46c3-bb2a-a5cbf1e0f34e)

```{r}
cat('Satisfied / amount of passengers: ', round(5256 * 100 / (18023 + 5256), 2), '%', '\n')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/1018fd7f-539f-4f6b-adb3-00cf68ea9d45)

**Висновок:**

На відміну від минулої вікової групи, діти та люди похилого віку мають 22.57% задоволеності. Враховуючи попередній аналіз і висновки про націленість компанії на бізнес-мандрівки можна припустити, що компанія менше уваги приділяє звичайним пасажирським рейсам, якими літають діти та люди похилого віку, бо скоріш за все діти та люди похилого віку не літають на бізнес зустрічі.

**Дослідницьке питання:** Чи існує залежність конкретних факторів задоволеності рейсів від вікових категорій клієнтів?

**Гіпотеза:** Існують вікові категорії, для яких вплив деяких факторів задоволеності менший за інші.

```{r}
categorical_cols <- c('Inflight_wifi_service', 'Departure/Arrival_time_convenient', 'Ease_of_Online_booking',
                      'Gate_location', 'Food_and_drink', 'Online_boarding', 'Seat_comfort',
                      'Inflight_entertainment', 'On-board_service', 'Leg_room_service',
                      'Baggage_handling', 'Checkin_service', 'Inflight_service',
                      'Cleanliness')
df$Age_group <- cut(df$Age, breaks=c(0, 18, 60, Inf), labels=c("Underage", "Middle age", "Aged"))
```

Поділивши пасажирів на три вікові категорії можна переконатися у правильному висновку минулої гіпотези, побудувавши наступну стовпчикову діаграму:

```{r, fig.height=6, fig.width=9}
ggplot(df, aes(fill = factor(Satisfaction), x = Age_group)) +
  geom_bar(stat = "count", position = 'fill', color=FALSE) +
  labs(title = "There is a difference in the level of satisfaction", x = "Age Group") +
  scale_fill_manual(name = "Satisfaction", values = c("#fdded6", "#F1BDAD"), labels = c("neutral or dissatisfied", "satisfied")) +
  ylab('Percent of passengers') +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/e2e98d86-b817-40ef-941a-e0737ecaeb22)

Подивимося на дизбаланс гендерних класів у всіх групових категоріях:

```{r, fig.height=6, fig.width=9}
ggplot(df) +
  geom_bar(aes(x = Age_group, fill = Gender), stat = "count", position='dodge', color='black') +
  labs(title = "There is no imbalance of gender classes", x = "Age Group", y='Passengers') +
  coord_cartesian(ylim=c(0, 60000)) + 
  scale_y_continuous(breaks = seq(0, 60000, by = 10000)) +
  scale_fill_manual(name = "Gender", values = c("#FF7368", "#5690CF"), labels = c("Female", "Male")) +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d43bf3b5-4064-4317-b611-6a26636f87be)

Також за вище наведеним графіком сказати, що в якійсь конкретній віковий категорії присутній дизбаланс гендерних класів - не можна.

Виділимо ті змінні, де можливо помітна різниця між рівнями задоволеності для трьох вікових груп.

```{r, fig.height=8, fig.width=10}
fig1 <- ggplot(aes(x = Age_group, y = Seat_comfort), data = df) + 
stat_summary(fun = "mean", geom = "bar", aes(fill=Age_group), show.legend=FALSE, color=FALSE) +
  scale_y_continuous(breaks = seq(1, 4, by = 0.5)) + 
  coord_cartesian(ylim=c(1, 3.5)) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3)) +
  labs(x = "Age group", y = "Seat comfort") +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
  
fig2 <- ggplot(aes(x = Age_group, y = Leg_room_service), data = df) + 
stat_summary(fun = "mean", geom = "bar", aes(fill=Age_group), show.legend=FALSE, color=FALSE) +
  scale_y_continuous(breaks = seq(1, 4, by = 0.5)) + 
  coord_cartesian(ylim=c(1, 3.5)) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3)) +
  labs(x = "Age group", y = "Leg room service") +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
fig3 <- ggplot(aes(x = Age_group, y = Online_boarding), data = df) + 
stat_summary(fun = "mean", geom = "bar", aes(fill=Age_group), show.legend=FALSE, color=FALSE) +
  coord_cartesian(ylim=c(1, 3.5)) +
  scale_y_continuous(breaks = seq(1, 4, by = 0.5)) + 
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3)) + 
  labs(x = "Age group", y = "Online boarding") +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
fig4 <- ggplot(aes(x = Age_group, y = Time_convenient), data = df) + 
stat_summary(fun = "mean", geom = "bar", aes(fill=Age_group), color=FALSE) +
  scale_y_continuous(breaks = seq(1, 4, by = 0.5)) + 
  coord_cartesian(ylim=c(1, 3.5)) +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3)) + 
    labs(x = "Age group", y = "Time convenient") +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.position = c(-0.2, 1.2))
fig1 + fig2 + fig3 + fig4
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/31534e12-eefb-4258-be60-d5631b6dbfbf)

**Висновок:**

1\. *Seat comfort*: Задоволеність даним фактором найбільша (3.5) у середньої вікової групи, трохи менша у людей похилого віку (3.4), це можна списати на вік та вплив інших факторів самопочуття, проте середня задоволеність дітей значно нижня (3), це **може** свідчити про те, що літаки не достатньо добре облаштовані зручними сидіннями для дітей.

Побудуємо ridgeplot для цього фактору:

```{r, fig.height=7, fig.width=14}
ggplot(data=df %>% filter(Seat_comfort > 0), aes(x=Age, y=factor(Seat_comfort), fill = factor(Seat_comfort))) + 
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = TRUE, quantiles=2, alpha=1, rel_min_height = 0.009, scale = 2.5) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_fill_brewer(palette = "Purples") +
  labs(title = 'There is two ridges on the plot', x = "Seat Comfort", y = "Age") + 
  guides(fill = guide_legend(title = "Seat Comfort")) + 
  theme_ridges() +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/0df57a50-3144-434a-bc99-cf3bc120373c)

За графіком чітко видно два умовних горби для кожного класу змінної Seat Comfort, проте кількість незадоволених (оцінка 1-3) від 20 до 30 років значно більша аніж кількість задоволених (оцінка 4-5).
Також можна побачити як зміщується медіана в бік збільшення віку. Крім того, починаючи від 45 років, кількість частка задоволених помітно збільшується.

2\. *Leg room service*: Простором для ніг задоволена більше середня вікова група та діти ніж люди похилого віку, це може бути пов'язано із впливом інших факторів, таких як стан здоров'я

3\. \_*Online boarding*\_: Даний фактор має найменшу задоволеність серед дітей, проте це можна пояснити тим, що діти не мають можливості придбати квитки на авіарейси, тому скоріш за все під час опитування діти ставили оцінки 0, якщо не знали що відповісти, або такі, які скажуть їм батьки. Проте є деяка різниця між задоволеністю онлайн-реєстрацією на рейс людьми похилого віку і середньої вікової групи, це може бути пов'язане з тим, що старші люди, як правило, менш вдало користуються інтернетом, або проблема в незручному або інтуітивно не зрозумілому інтерфейсомі сайту, на якому клієнти бронюють квитки.

4\. *Departure/Arrival delay*: Тут все теж очевидно, людям середньої вікової групи частіше треба літати з бізнес/робочих цілей на відміну від дітей та людей похилого віку, які з великою вирогідністю літають з туристичною метою.

#### 2.3 Flight distance

```{r, fig.height=7, fig.width=13}
ggplot(df, aes(x = Flight_Distance, fill = factor(Satisfaction))) +
  geom_histogram(binwidth = 50, alpha = 1, color='black') +
  scale_x_continuous(breaks = seq(0, 5000, by = 500)) +
  scale_y_continuous(breaks = seq(0, 6000, by = 1000)) +
  scale_fill_manual(values = c("#B1B6AE", "#53D46B"), name = "Satisfaction") +
  labs(title = 'Long-haul flights are more satisfying', x = "Flight Distance", y = "Num of passengers") + 
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/4ba6061e-28ba-4c3e-a989-f4cf710d67f3)

За графіком можна помітити що авіаперельоти можна поділити по дистанції на три категорії:

-   0-1500 km (візуально кількість незадоволених пасажирів майже вдвічі більша ніж задоволених)

-   1500-2500 km (візуально спостерігається протилежна ситуація, кількість задоволених тепер вдвічі більша ніж незадоволених)

-   2500+ km (на цій ділянці можна помітити велику різницю між кількістю задоволених та незадоволених рейсами)

Подивимося на конкретні числа, щоб дати оцінку ситуації:

```{r}
# create new categorical columns based on int features 'Flight Distance'
flight_haul <- c('Short', 'Medium', 'Long')
df$Flight_haul <- cut(df$Flight_Distance, breaks = c(0, 1500, 2500, Inf), labels = flight_haul)
```

```{r}
for (distance in flight_haul) {
  cat(paste(distance, '-haul flight\n'))
  value_counts <- table(df[df$Flight_haul == distance, 'Satisfaction'])
  cat(paste('Satisfied: ', value_counts[2], '\n'))
  cat(paste('Neutral or dissatisfied: ', value_counts[1], '\n'))
  cat(paste('Satisfied / amount passenger of short haul flight: ', round(value_counts[2] / sum(value_counts), 2), '\n\n'))
}
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/b1bd579f-a7c1-4387-9924-3fa7f7d291ba)

Візуальні оцінки підтвердилися, ми можемо спостерігати що short-haul flights мають лише 34% задоволеності пасажирів на відміну від long-haul flights де цей показник становить 73%.\
Це може означати націленість компанії на авіаперельоти окремої дистанції, тобто компанія більше приділяє уваги клієнтам та перельотам, що мають велику відстань.\
Поглянемо на кількість здійснених перельотів кожної з груп:

```{r}
cat('Num of passenger short haul:', sum(df$Flight_haul == 'Short'), '\n')
cat('Num of passengers medium + long haul:', sum(df$Flight_haul == 'Medium') + sum(df$Flight_haul == 'Long'), '\n')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/0ec31470-aabd-487a-bb60-bdb80653e692)

**Висновок:**

Пасажирів, що літали рейсами короткої дистанції приблизно вдвічі більше ніж довгими, але це ще не означає, що співвідношення кількості рейсів таке ж. Часто перельоти не невеликі відстані можуть мати більшу кількість пасажирських місць ніж на велику відстань, або можливо рейси великих дистанцій мають в середньому більшу кількість вільних пасажирських місць ніж на малу.

Проте, враховуючи всі вище досліджені показники, можна дати оцінку низькій зацікавленості авіакомпанії в підтримці і обслуговуванні рейсів коротких дистанцій.

#### 2.4 Flight distance: type of travel, seat comfort

Проте можливо, що компанія націлена не просто на перельоти великими дистанціями, а конкретним типом польоту: business/personal travel.\

**Дослідницьке питання:** Чи існує залежність між задоволеністю клієнтами рейсом та цілями перельотів?

**Гіпотеза:** Пасажири, що подорожували в персональних цілях залишаються менш задоволеними перельотом.

Побудуємо фацетований графік:

```{r, fig.height=7, fig.width=15}
ggplot(df, aes(x = Flight_Distance, fill = factor(Satisfaction))) +
  geom_histogram(binwidth = 50, alpha = 1, color='black') +
  scale_fill_manual(values = c("#B1B6AE", "#53D46B"), name = "Satisfaction") +
  labs(x = "Flight Distance", y = "Num of passengers") + 
  facet_wrap(~Type_of_Travel, nrow=1) + 
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/98afc42d-ad94-4e13-951d-abe6ae2a974a)

```{r, fig.height=5, fig.width=8}
ggplot(df, aes(x=Type_of_Travel, fill=factor(Satisfaction))) +
  geom_bar(aes(y=after_stat(count)/tapply(after_stat(count), after_stat(x) ,sum)[after_stat(x)]), position="dodge", colour='black') + 
  geom_text(aes( y=after_stat(count)/tapply(after_stat(count), after_stat(x) ,sum)[after_stat(x)], label=scales::percent(after_stat(count)/tapply(after_stat(count), after_stat(x) ,sum)[after_stat(x)]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5, colour='white') + 
  coord_cartesian(ylim=c(0, 1)) +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(6)) + 
  scale_fill_manual(name = "Satisfaction", values = c("#B1B6AE", "#53D46B"), labels = c("neutral or dissatisfied", "satisfied")) +
  ylab("Num of passengers") +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))
ggsave('eda_type.png')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/4ba4a44d-f9fd-4b5a-8318-753b292b19e4)

```{r}
# Calculate satisfaction percentage for personal type
personal <- table(df$Satisfaction[df$Type_of_Travel == "Personal Travel"])
personal_dissatisfaction_pct <- round(personal[1] / sum(personal), 2)
# Calculate satisfaction percentage for business type
business <- table(df$Satisfaction[df$Type_of_Travel == "Business Travel"])
business_satisfaction_pct <- round(business[2] / sum(business), 2)
# Calculate satisfaction percentage for business type (medium and long flights)
business_long <- table(df$Satisfaction[df$Type_of_Travel == "Business Travel" & df$Flight_haul %in% c("Medium", "Long")])
business_long_satisfaction_pct <- round(business_long[2] / sum(business_long), 2)
cat("Personal type % of dissatisfaction:", personal_dissatisfaction_pct, "\n")
cat("Business type % of satisfaction:", business_satisfaction_pct, "\n")
cat("Business type % of satisfaction (medium and long flights):", business_long_satisfaction_pct, "\n")
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/d2c27f08-60fc-42c3-8629-fa410e445030)

**Висновок:**

Дивлячись на два попередні графіки можна переконатись в тому, що авіакомпанії слід звернути увагу на рейси з типом "персональний переліт", бо незалежно від відстані, ті, хто подорожував з особистих причин були майже на 90% (!) незадоволеними.

Проте пасажири, що літали з бізнес причин незалежно від відстані були на 58 % задоволені, а починаючи з 1500 км аж на 74%.

**Дослідницьке питання:** Чи існує різниця впливу комфортабельності сидіння в залежності від дистанції рейсів? В яких випадках можна знехнувати комфортабельністю?

**Гіпотеза:** Комфортабельністю можна нехтувати при невеликих дистанціях рейсів.

```{r, fig.height=8, fig.width=10}
g1 <- ggplot(data = subset(df, Flight_haul == 'Short') %>% filter (Satisfaction == 0 & Seat_comfort > 0), aes(x = Satisfaction, fill = factor(Seat_comfort))) +
  geom_bar(position = 'dodge', color=FALSE) +
  scale_fill_brewer(name= 'Seat comfort', palette = "YlGn") +
  labs(title = 'Short Distance',x = 'Satisfaction', y = 'Passengers') +
  coord_cartesian(ylim=c(0, 15000)) +
  scale_y_continuous(breaks = seq(0, 15000, by = 2500)) +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        axis.text.x=element_blank()) 
g2 <- ggplot(data = subset(df, Flight_haul == 'Medium') %>% filter (Satisfaction == 0), aes(x = Satisfaction, fill = factor(Seat_comfort))) +
  geom_bar(position = 'dodge', color=FALSE) +
  scale_fill_brewer(name= 'Seat comfort', palette = "YlGn") +
  labs(title = 'Medium Distance', x = 'Satisfaction', y = 'Passengers') +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        axis.text.x=element_blank()) 
g3 <- ggplot(data = subset(df, Flight_haul == 'Long') %>% filter (Satisfaction == 0), aes(x = Satisfaction, fill = factor(Seat_comfort))) +
  geom_bar(position = 'dodge', color=FALSE) +
  scale_fill_brewer(name= 'Seat comfort', palette = "YlGn") +
  coord_cartesian(ylim=c(0, 2000)) +
  labs(title = 'Long Distance', x = 'Satisfaction', y = 'Passengers') +
  theme_modern_rc() + 
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10), , size=12), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=12), , size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        axis.text.x=element_blank()) + coord_polar()
g1 + coord_polar()
g2 + coord_polar()
g3 + coord_polar()
ggsave('eda_seat3.png')
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/f89ea125-5e43-41ca-b14f-0d17f8a94c79)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/04e20a51-3b77-4f17-bb5f-8558c99d70d1)
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/0ca7039f-b182-46ee-8741-9f39b8eb8ccf)

Досліджуючи питання комфортабельності сидінь треба слід досліджувати лише ту групу людей, яка залишилася незадоволеною рейсом, щоб визначити вплив комфортабельності сидінь та в подальшому зменшити кількість незадоволених.

Якщо людина залишилася **незадоволеною рейсом**, то слід звернути увагу на рейси тієї дистанції (short, medium, long), на якій частка **задоволених сидіннями** людей **менша**, можливо саме через некомфортне сидіння людина залишилася **незадоволеною рейсом**.

Будемо вважати, що людина задоволена сидінням лише якщо вона поставила оцінку 4 або 5, у всіх інших випадках вона незадоволена сидінням.

```{r}
for (distance in flight_haul) {
  current_haul <- subset(df, Flight_haul == distance & Satisfaction == 0)
  satisfaction_fraction <- round(nrow(subset(current_haul, Seat_comfort > 3)) / nrow(current_haul), 2)
  cat(paste("Fraction of seat satisfaction on", distance, "-haul: ", satisfaction_fraction, "\n"))
}
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/e3f651b9-00ca-4e60-9775-577fba039f56)

Аналізуючи отримані результат, можна прийти до висновку, що чим більша відстань рейсу, тим менше оцінка комфорту сидіння. Тобто на невеликих відстанях навіть ті люди, що залишилися незадоволеними рейсом більше задоволені сидіннями, ніж люди, що подорожували великими відстанями на 10%.

А отже комфорт сидіння має більший вплив на перельоти великих дістанцій.

#### 2.5 Class

**Дослідницьке питання:** Які признаки мають найбільший/найменший вплив на задоволеність клієнтів бізнес/економ класу?

**Гіпотеза:** Існують фактори, які по-різному впливають на людей, подорожуючих різними класами.

Як відомо, обслуговування бізнес класу виходить авіакомпаніям дорожче і можливість втратити клієнта бізнес класу більш негативна, ніж клієнта економ класу. Визначивши як фактори найбільше впливають на клієнтів бізнес класу ми зможемо їх поліпшити. (наприклад за рахунок зниження фінансування і так не важливих факторів для клієнтів економ класу)

Подивимося кількісний склад кожного класу:

```{r}
table(df$Class)
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/93b89e55-6034-4889-8801-79297e8a3546)

Через те, що виділяється лише два великих класи Business та Eco, додамо ще до Eco клас Eco Plus, клієнтів якого лише 7%. Побудуємо відповідно для двох класів матрицю кореляцій, та подивимося кореляцію із цільовою змінною:

```{r, fig.height=8, fig.width=10}
correlations <- cor(df[df$Class == "Business", sapply(df, is.numeric)])
melted_cormat <- melt(correlations)
ggcorr((df %>% filter(Class == 'Business'))[c(-1)], method = c("everything", "pearson"), hjust = 1, vjust=0, size=3.5, layout.exp = 3,  
       low = "black", mid = "#B1B6AE", high = "#53D46B", color='white', label=TRUE,
       label_color='black', label_size=3.5, label_round = 1, label_alpha = TRUE, palette='PuBuGn') +
  theme_modern_rc() + 
  labs(title = 'Business Type') +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10)), 
        axis.text.x=element_blank())
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/25ca0490-5066-4da2-a9f1-b2085ee47187)

```{r}
business_corr <- data.frame(t(cor(df[df$Class == "Business", sapply(df, is.numeric)])["Satisfaction",]))
business_corr <- business_corr[, order(-abs(business_corr))]
business_corr
```

В результаті можна спостерігати, що для клієнтів бізнес класу найбільш корелюючими з цільовою змінною факторами є *Online boarding (0.51), Inflight Entertainment (0.5), On-board Service(0.43)*

В той час найменшим корелюючими є *Gate Location (-0.003), Departure/Arrival time convenient(0.013), Ease of Online booking(0.06)*

```{r, fig.height=10, fig.width=10}
correlations <- cor(df[df$Class == "Eco" | df$Class == "Eco Plus", sapply(df, is.numeric)])
melted_cormat <- melt(correlations)
ggcorr((df %>% filter(Class == 'Eco' | Class == 'Eco Plus'))[c(-1)], method = c("everything", "pearson"), hjust = 1, vjust=0, size=3.5, layout.exp = 3,  
       low = "black", mid = "#B1B6AE", high = "#53D46B", color='white', label=TRUE,
       label_color='black', label_size=3.5, label_round = 1, label_alpha = TRUE, palette='PuBuGn') +
  labs(title = 'Eco Type') +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)), 
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10)), 
        axis.text.x=element_blank())
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/b844f7a7-f4c5-43a2-a5f4-3fc4b429ae97)

```{r}
eco_corr <- data.frame(t(cor(df[df$Class == "Eco" | df$Class == "Eco Plus", sapply(df, is.numeric)])["Satisfaction",]))
eco_corr <- eco_corr[, order(-abs(eco_corr))]
eco_corr
```

Проте різниця впливу факторів все ж існує, а отже гіпотеза підтверджується, можна спостерігати, що для клієнтів економ класу найбільш корелюючими з цільовою змінною факторами є *Inflight wifi service (0.47), Online_boarding (0.31), Ease_of_Online_booking (0.21), Food_and_drink*

В той час найменшим корелюючими є *Gate Location, Departure/Arrival time convenient, Ease of Online booking*

#### 2.6 Correlation

```{r, fig.height=10, fig.width=10}
correlations <- cor(df[sapply(df, is.numeric)])
melted_cormat <- melt(correlations)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='black', linetype=3) +
  scale_fill_viridis(discrete=FALSE, direction=1) +
  guides(fill = guide_colourbar(title='', barwidth = 1, barheight = 15)) +
  labs(title = '', x = '', y = '') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  theme_modern_rc() +
  theme(plot.title = element_text(hjust = 0.5, size=13, face='italic'),
        axis.title.y = element_text(hjust = 0.5, margin = margin(r = 10)),
        axis.title.x = element_text(hjust = 0.5, margin = margin(t=10)),
        axis.text.x=element_blank())
```
![image](https://github.com/oleksii-harmash/pet-data-analysis-R-2023/assets/72203364/9b0610c2-fd18-4c22-b081-3441aa3fe459)
