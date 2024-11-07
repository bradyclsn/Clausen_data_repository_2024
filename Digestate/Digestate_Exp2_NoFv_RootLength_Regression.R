library(readxl)
library(ggplot2)
library(dplyr)
data <- read_excel("Digestate_Exp2_NoFv_RootLength_Regression.xlsx")
# Convert treatment to factor, so it is categorical 
data$Treatment <- as.factor(data$Treatment)
#mean,sd,se
treatment_means <- data %>%
  group_by(Treatment) %>%
  summarize(Mean_Root_Length = mean(`Root Length`, na.rm = TRUE))
regression_model <- lm(`Root Length` ~ as.numeric(Treatment), data = data)
separate_means_model <- lm(`Root Length` ~ Treatment, data = data)
#ANOVA lack of fit
lack_of_fit_table <- anova(regression_model, separate_means_model)
print("ANOVA Lack-of-Fit Table:")
print(lack_of_fit_table)
#P-value for graph
p_value <- summary(regression_model)$coefficients[2, 4]
#Plot
ggplot(data, aes(x = as.numeric(Treatment), y = `Root Length`)) +
  geom_point(aes(color = Treatment), size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") + 
  geom_point(data = treatment_means, aes(x = as.numeric(Treatment), y = Mean_Root_Length), 
             color = "red", size = 4, shape = 17) + 
  labs(
    title = "Dry Digestate + No Fv - Exp 2", 
    x = "Treatment (w/w)",                   
    y = "Root Length (cm)"                   
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = 1:length(levels(data$Treatment)), 
    labels = levels(data$Treatment)
  ) +
  theme(legend.position = "none") + 
  annotate("text", x = max(as.numeric(data$Treatment)) - 0.5, y = max(data$`Root Length`), 
           label = paste("p-value =", round(p_value, 4)), size = 5, color = "black")
