## Load Packages----
library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(ggpubr)
library(viridis)
library(patchwork)
## Load Data----

getwd()
library(readxl)
birds <- read_excel("Bird_Thermoregulation_R.xlsx")
View(Bird_Thermoregulation_R)

head(birds)
str(birds)

## Convert variables to factors----
library(tidyverse)
birds <- birds %>%
  mutate(
    Species = as.factor(Species),
    Day_period = as.factor(Day_period),
    Location = as.factor(Location),
    Microhabitat = as.factor(Microhabitat),
    Substrate = as.factor(Substrate),
    Sun_exposure = as.factor(Sun_exposure),
    Behaviour = as.factor(Behaviour),
    Body_condition = as.factor(Body_condition),
    Movement = as.factor(Movement)
  )
str(birds)
levels(birds$Species)
levels(birds$Behaviour)
levels(birds$Movement)
levels(birds$Day_period)
colSums(is.na(birds))

## Descriptive summary ----
summary(birds)
table(birds$Species)
table(birds$Behaviour)
table(birds$Day_period)
table(birds$Sun_exposure)


birds %>%
  group_by(Species) %>%
  summarise(
    Mean_Temp = mean(Temperature_C),
    SD_Temp = sd(Temperature_C),
    Median_Temp = median(Temperature_C),
    Min_Temp = min(Temperature_C),
    Max_Temp = max(Temperature_C)
  )


table(birds$Species, birds$Behaviour)

## Figures ----
Figure 1. Temperature Distribution Across Bird Species
ggplot(birds, aes(x = Species, y = Temperature_C, fill = Species)) +
  geom_boxplot(width = 0.7, alpha = 0.8) +
  labs(
    title = "Temperature Distribution Across Bird Species",
    x = "Species",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "none"
  )


Figure 2. Behavioural Frequencies Across Species
ggplot(birds, aes(x = Behaviour, fill = Species)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Behavioural Frequencies Across Species",
    x = "Behaviour",
    y = "Frequency"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

Figure 3. Distribution of Ambient Temperature
ggplot(birds, aes(x = Temperature_C)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Distribution of Ambient Temperature",
    x = "Temperature (°C)",
    y = "Frequency"
  ) +
  theme_classic(base_size = 14)

Figure 4. Q-Q Plot of Temperature Data
ggplot(birds, aes(sample = Temperature_C)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of Temperature Data"
  ) +
  theme_classic(base_size = 14)


Figure 5. Behavioural Composition Across Species
ggplot(as.data.frame(behaviour_table),
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Behavioural Composition Across Species",
    x = "Species",
    y = "Proportion",
    fill = "Behaviour"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )


Figure 6. Movement Activity Across Day Periods
ggplot(as.data.frame(movement_table),
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Movement Activity Across Day Periods",
    x = "Day Period",
    y = "Proportion",
    fill = "Movement"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

Figure 7. Movement Activity Under Different Sun Exposure Conditions
ggplot(as.data.frame(sun_move_table),
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Movement Activity Under Different Sun Exposure Conditions",
    x = "Sun Exposure",
    y = "Proportion",
    fill = "Movement"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


Figure 8. Temperature Distribution Across Movement Categories
ggplot(birds, aes(x = Movement, y = Temperature_C, fill = Movement)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  labs(
    title = "Temperature Distribution Across Movement Categories",
    x = "Movement Activity",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


Figure 9. 
ggboxplot(
  birds,
  x = "Movement",
  y = "Temperature_C",
  fill = "Movement"
) +
  stat_compare_means(method = "kruskal.test") +
  labs(
    title = "Ambient Temperature Across Movement Categories",
    x = "Movement Activity",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


Figure 10. Temperature Across Body Condition Categories
ggplot(
  birds,
  aes(
    x = Body_condition,
    y = Temperature_C,
    fill = Body_condition
  )
) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Temperature Across Body Condition Categories",
    x = "Body Condition",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "none"
  )

Figure 11. Ambient Temperature Across Body Condition Categories
ggboxplot(
  birds,
  x = "Body_condition",
  y = "Temperature_C",
  fill = "Body_condition"
) +
  stat_compare_means(method = "t.test") +
  labs(
    title = "Ambient Temperature Across Body Condition Categories",
    x = "Body Condition",
    y = "Temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "none"
  )
view(birds)
Figure 12. Behavioural Composition Across Day Periods
ggplot(
  as.data.frame(behaviour_day_table),
  aes(x = Var1, y = Freq, fill = Var2)
) +
  geom_bar(
    stat = "identity",
    position = "fill"
  ) +
  labs(
    title = "Behavioural Composition Across Day Periods",
    x = "Day Period",
    y = "Proportion",
    fill = "Behaviour"
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

Figure 12. Sun Exposure Across Bird Species
ggplot(
  as.data.frame(species_sun_table),
  aes(
    x = Var1,
    y = Freq,
    fill = Var2
  )
) +
  geom_bar(
    stat = "identity",
    position = "fill"
  ) +
  labs(
    title = "Sun Exposure Across Bird Species",
    x = "Species",
    y = "Proportion",
    fill = "Sun Exposure"
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )


Figure 13. Behavioural Categories Across Sun Exposure Conditions
ggplot(
  as.data.frame(behaviour_sun_table),
  aes(
    x = Var1,
    y = Freq,
    fill = Var2
  )
) +
  geom_bar(
    stat = "identity",
    position = "fill"
  ) +
  labs(
    title = "Behavioural Categories Across Sun Exposure Conditions",
    x = "Behaviour",
    y = "Proportion",
    fill = "Sun Exposure"
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

Figure 14. Day Period Distribution Across Bird Species
ggplot(
  as.data.frame(species_day_table),
  aes(
    x = Var1,
    y = Freq,
    fill = Var2
  )
) +
  geom_bar(
    stat = "identity",
    position = "fill"
  ) +
  labs(
    title = "Day Period Distribution Across Bird Species",
    x = "Species",
    y = "Proportion",
    fill = "Day Period"
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )
# Supplementary figure
birds_numeric <- birds %>%
  mutate(
    Species_num = as.numeric(Species),
    Day_num = as.numeric(Day_period),
    Sun_num = as.numeric(Sun_exposure),
    Behaviour_num = as.numeric(Behaviour),
    Movement_num = as.numeric(Movement),
    Body_num = as.numeric(Body_condition)
  )

# Correlation Matrix
cor_matrix <- cor(
  birds_numeric %>%
    select(
      Temperature_C,
      Species_num,
      Day_num,
      Sun_num,
      Behaviour_num,
      Movement_num,
      Body_num
    ),
  method = "spearman"
)

cor_matrix
#Heatmap Figure
library(corrplot)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)
## Shapiro-Wilk Normality test----
shapiro.test(birds$Temperature_C)

birds %>%
  group_by(Species) %>%
  shapiro_test(Temperature_C)

## Homogeneity----
library(car)
leveneTest(Temperature_C ~ Species, data = birds)
## One-way ANOVA----
anova_temp <- aov(Temperature_C ~ Species, data = birds)

summary(anova_temp)

library(effectsize)
eta_squared(anova_temp)

par(mfrow = c(2,2))
plot(anova_temp)
par(mfrow = c(1,1))

## Chi-square test (Not applied)----
behaviour_table <- table(birds$Species, birds$Behaviour)

chisq.test(behaviour_table)$expected



chisq_behaviour <- chisq.test(behaviour_table)

chisq_behaviour


cramers_v(behaviour_table)

chisq_behaviour$stdres
## Fisher exact test----
fisher.test(behaviour_table)

## Day_period x movememt----
movement_table <- table(birds$Day_period, birds$Movement)

movement_table

chisq.test(movement_table)$expected


chisq_movement <- chisq.test(movement_table)

chisq_movement


cramers_v(movement_table)

chisq_movement$stdres

## Sun exposure x movemenyt----
sun_move_table <- table(birds$Sun_exposure, birds$Movement)

sun_move_table


chisq.test(sun_move_table)$expected


chisq_sun_move <- chisq.test(sun_move_table)

chisq_sun_move


cramers_v(sun_move_table)



chisq_sun_move$stdres



## Temperature summary by movement----
birds %>%
  group_by(Movement) %>%
  summarise(
    Mean = mean(Temperature_C),
    SD = sd(Temperature_C),
    Median = median(Temperature_C),
    Min = min(Temperature_C),
    Max = max(Temperature_C)
  )

## Normality within movement categories
birds %>%
  group_by(Movement) %>%
  shapiro_test(Temperature_C)
## Homogeneity
leveneTest(Temperature_C ~ Movement, data = birds)

## Kruskal wallis test
kruskal.test(Temperature_C ~ Movement, data = birds)

kruskal_effsize(birds, Temperature_C ~ Movement)

## Dunn post hoc test
dunn_results <- dunn_test(
  birds,
  Temperature_C ~ Movement,
  p.adjust.method = "bonferroni"
)

dunn_results


dunn_results %>%
  mutate(
    Significance = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )


## Temperature summary by body condition----
birds %>%
  group_by(Body_condition) %>%
  summarise(
    `Mean ± SD` = paste0(
      round(mean(Temperature_C), 2),
      " ± ",
      round(sd(Temperature_C), 2)
    ),
    Median = round(median(Temperature_C), 2),
    Min = min(Temperature_C),
    Max = max(Temperature_C)
  )



## Normality
birds %>%
  group_by(Body_condition) %>%
  shapiro_test(Temperature_C)

## Homogeneity
leveneTest(Temperature_C ~ Body_condition, data = birds)

## Independent sample t-test
t_test_body <- t.test(
  Temperature_C ~ Body_condition,
  data = birds,
  var.equal = TRUE
)

t_test_body




## Effect size (Coden's d)
cohens_d(
  Temperature_C ~ Body_condition,
  data = birds,
  pooled_sd = TRUE
)
## Behaviour x Day period----
behaviour_day_table <- table(
  birds$Day_period,
  birds$Behaviour
)

behaviour_day_table


## Chi square (Not applied)
chisq.test(behaviour_day_table)$expected


chisq_behaviour_day <- chisq.test(
  behaviour_day_table
)

chisq_behaviour_day


cramers_v(behaviour_day_table)



chisq_behaviour_day$stdres


## Fisher exact test
fisher.test(behaviour_day_table)

## Model----
library(nnet)
birds$Movement <- relevel(
  birds$Movement,
  ref = "Low"
)
# Fit Multinomial logistic regression model

movement_model <- multinom(
  Movement ~ Temperature_C +
    Day_period +
    Sun_exposure,
  data = birds
)

summary(movement_model)


# Model significance
z_values <- summary(movement_model)$coefficients /
  summary(movement_model)$standard.errors

p_values <- 2 * (1 - pnorm(abs(z_values)))

p_values

# Odd ratios
exp(coef(movement_model))
# Model Diagnostics
library(car)

vif_model <- lm(
  Temperature_C ~ Day_period + Sun_exposure,
  data = birds
)

vif(vif_model)

## Species x Sun exposure----
# Contingency table
species_sun_table <- table(
  birds$Species,
  birds$Sun_exposure
)

species_sun_table

# Chi-square test
chisq.test(species_sun_table)$expected


chisq_species_sun <- chisq.test(
  species_sun_table
)

chisq_species_sun


cramers_v(species_sun_table)


chisq_species_sun$stdres

## Behaviour x Sun exposure----
behaviour_sun_table <- table(
  birds$Behaviour,
  birds$Sun_exposure
)

behaviour_sun_table


# Chi-square test
chisq.test(behaviour_sun_table)$expected

chisq_behaviour_sun <- chisq.test(
  behaviour_sun_table
)

chisq_behaviour_sun


cramers_v(behaviour_sun_table)
chisq_behaviour_sun$stdres

## Species x Day period----
# Contingency table
species_day_table <- table(
  birds$Species,
  birds$Day_period
)

species_day_table

# Chi-square test
chisq.test(species_day_table)$expected


chisq_species_day <- chisq.test(
  species_day_table
)

chisq_species_day


cramers_v(species_day_table)

chisq_species_day$stdres
