library(readxl)
library(ggplot2)
library(dplyr)

data <- read_excel("MWHC.xlsx")

# Factor as categorical
data$Treatment <- as.factor(data$Treatment)

#Mean,sd,se
treatment_means <- data %>%
  group_by(Treatment) %>%
  summarize(Mean_MWHC = mean(MWHC, na.rm = TRUE))

regression_model <- lm(MWHC ~ as.numeric(Treatment), data = data)

separate_means_model <- lm(MWHC ~ Treatment, data = data)

#ANOVA lack of fit
lack_of_fit_table <- anova(regression_model, separate_means_model)
print("ANOVA Lack-of-Fit Table:")
print(lack_of_fit_table)

p_value <- summary(regression_model)$coefficients[2, 4]

#Plot
ggplot(data, aes(x = as.numeric(Treatment), y = MWHC)) +
  geom_point(aes(color = Treatment), size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") + 
  geom_point(data = treatment_means, aes(x = as.numeric(Treatment), y = Mean_MWHC), 
             color = "red", size = 4, shape = 17) + 
  labs(
    title = "Maximum Water Holding Capacity of Digestate",
    x = "Treatment (w/w)",
    y = "MWHC (%)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = 1:length(levels(data$Treatment)), 
    labels = levels(data$Treatment)
  ) +
  theme(legend.position = "none") + 
  annotate("text", x = max(as.numeric(data$Treatment)) - 0.5, y = max(data$MWHC), 
           label = paste("p-value =", round(p_value, 4)), size = 5, color = "black")
