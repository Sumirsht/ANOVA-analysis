# ANOVA-analysis
🔬 ANOVA on Soil Organic Carbon (SOC) by Soil Depth
This R script performs analysis of variance (ANOVA) to investigate whether soil organic carbon (SOC) significantly differs across three soil depth classes: 0–20 cm, 20–30 cm, and 30–40 cm. It includes assumption checks, post-hoc comparisons, and visualizations to interpret the results.

📋 Features
Performs one-way ANOVA on SOC values across soil depths.
Tests ANOVA assumptions including Levene's test for homogeneity of variances.
Conducts post-hoc tests:
Fisher's Least Significant Difference (LSD)
Tukey’s HSD
Pairwise t-tests with multiple comparison corrections (Bonferroni, BH).

📈 Visualizes:
Boxplots and histograms of SOC distribution.
LSD group comparison via grouped bar charts.
Clean, commented R code ideal for teaching, demonstration, or extension.

📦 Dependencies
tidyverse
car
pastecs
multcomp
agricolae
sjstats
tibble

👨‍💻 Author
Developed by Sumir Shrestha
https://www.linkedin.com/in/sumir-shrestha456/
