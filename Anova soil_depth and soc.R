# Title: Anova soil_depth and soc.R
# Author: Sumir Shrestha
# Date: [2024-11-01]
# Description: This script performs ANOVA on soil organic carbon (soc) data across different soil depths, checks assumptions, and conducts post-hoc tests.

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(car)        # For Levene's test
library(pastecs)    # For descriptive statistics
library(multcomp)   # For multiple comparisons
library(agricolae)  # For LSD test

# Create the data frame
soil_depth <- gl(3,4, labels = c('0-20','20-30','30-40'))
soc <- c(5,4,6,7,3,4,3,4,3,3,2,3)
data <- data.frame(soil_depth, soc)

# Display the first few rows and structure of the data
head(data)
str(data)

#ploting the graphics
summary(data)
plot(data, main = "Box plot of soil organic carbon")
hist(data$soc, main = "Histogram of Soil organic carbon")
boxplot(data$soc, main = "Box plot of soil organic carbon")

# Create Hypothesis
# Assuming we want to test if there is a significant difference in the average soil organic carbon among the three levels of depth
# Null Hypothesis (H0): The mean soc is the same across all depths
# Alternative Hypothesis (Ha): The mean soc is different among at least one depth.

anova_model <- aov(soc ~ soil_depth, data=data)
summary(anova_model)

#--- Levene's Test for homogeneity of variances ---
library(sjstats)
a_result <- anova_stats(anova_model)  # Store detailed ANOVA results as a data frame
View(a_result)

# levene test
leveneTest(data$soc, data$soil_depth)

#  Perform LSD test
lsd_result <- LSD.test(anova_model, "soil_depth", p.adj = "none")
lsd_result

# Visualize the LSD test results
plot(lsd_result)

df<-lsd_result$groups

library(tibble)
df <- tibble::rownames_to_column(df, "Depth")
ggplot(df) +
  aes(x = Depth, y = soc, fill = groups) +
  geom_col() +scale_fill_hue(direction = 1) +
  labs(x = "Depth",y = "soc",
       title = "Fisher least square difference Data Visualization",
       subtitle = "source: www.shresthasumir.com.np",
       fill = "LSD Group") +theme_minimal()

# Plotting the bar chart with labels
Plot <- ggplot(df) +
  aes(x = Depth, y = soc, fill = groups) +
  geom_col() +
  geom_text(data = df, aes(label = groups, y = soc),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Depth", y = "soc",
    title = "Fisher least square difference Data Visualization",
    subtitle = "source: www.shresthasumir.com.np",
    fill = "LSD Group"
  ) +
  theme_minimal()

# Display the plot
print(Plot)

#--- Post Hoc Tests ---
# Pairwise t-tests with Bonferroni and Benjamini-Hochberg adjustments
pairwise.t.test(data$soc, data$soil_depth, p.adjust.method = "bonferroni")
pairwise.t.test(data$soc, data$soil_depth, p.adjust.method = "BH")

# Tukey's HSD test
postHocs <- TukeyHSD(anova_model)  # Base package for Tukey's HSD test
view(postHocs)

#copyright disclaimer
# This code is a work of fiction and is not intended to represent any real-world data or models.
# The models and metrics used are for illustrative purposes only and should not be used for actual scientific analysis.

