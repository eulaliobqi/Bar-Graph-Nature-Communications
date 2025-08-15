############################################################
# qRT-PCR Analysis Script
# Barplot with ANOVA + Dunnett test (Nature style formatting)
# Author: Eulalio Gutemberg
# Date: 2025
############################################################
# Install required packages
install.packages(c(
  "readxl",
  "tidyverse",
  "ggplot2",
  "ggpubr",
  "multcompView",
  "multcomp",
  "ggsignif"
))

# ========================
# 1) Load packages
# ========================
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(multcompView)
library(multcomp)
library(ggsignif)

# ========================
# 2) Input data
# ========================
# Set your input file and sheet name
file_path <- "data/paper_review.xlsx"
data <- read_excel(file_path, sheet = "fig1e1")

# Define the order of factor levels (modify as needed)
data$name <- factor(data$name, levels = c("Col0", "L1", "L3", "limyb32"))

# ========================
# 3) Functions
# ========================

# Add significance based on Dunnett test
add_significance <- function(anova_model, data) {
  dunnett_test <- glht(anova_model, linfct = mcp(name = "Dunnett"))
  dunnett_summary <- summary(dunnett_test)
  pvalues <- dunnett_summary$test$pvalues
  
  sig_levels <- ifelse(pvalues < 0.05, "*", "")
  names(sig_levels) <- levels(data$name)[-1] # exclude control (Col0)
  
  significance_data <- data.frame(
    name = names(sig_levels),
    significance = sig_levels
  )
  
  # Add empty significance for control
  significance_data <- rbind(
    data.frame(name = "Col0", significance = ""),
    significance_data
  )
  
  return(significance_data)
}

# ========================
# 4) Statistics
# ========================
anova <- aov(value ~ name, data = data)
summary(anova)

# Dunnett test
dunnett_test <- glht(anova, linfct = mcp(name = "Dunnett"))
summary(dunnett_test)

# Collect summary statistics
significance <- add_significance(anova, data)

summary_stats <- data %>%
  group_by(name) %>%
  summarize(Mean = mean(value),
            SE = sd(value) / sqrt(n())) %>%
  left_join(significance, by = "name") %>%
  mutate(Letters = significance)

# ========================
# 5) Labels
# ========================
# Customize labels for x-axis
new_labels <- c("Col-0", "LIMYB-32-L1", "LIMYB-32-L3", expression(italic("limyb-32")))

# ========================
# 6) Plot
# ========================
fig1e1 <- ggplot(data, aes(x = name, y = value, fill = name)) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.8,
           position = position_dodge(0.4), width = 0.3) +
  geom_errorbar(stat = "summary", fun.data = mean_se,
                width = 0.2, color = "black", size = 0.3) +
  geom_jitter(width = 0.1, size = 0.3, alpha = 1, color = "black") +
  labs(x = " ", y = "mRNA/Ubq", title = "PsbP") +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(0, 6, by = 0.5),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(values = c('#34A74BFF', '#34A74BFF', '#34A74BFF', '#34A74BFF')) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, colour = "black"),
    axis.text.y = element_text(size = 9, colour = "black"),
    axis.title.x = element_text(size = 9, colour = "black"),
    axis.title.y = element_text(size = 9, colour = "black"),
    plot.title = element_text(size = 10, hjust = 0.5,
                              margin = margin(t = 5, b = -12),
                              face = "italic"),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    legend.position = "none"
  ) +
  geom_text(data = summary_stats,
            aes(x = name, y = Mean + SE, label = Letters),
            vjust = -0.6, size = 5)

# Print figure
print(fig1e1)

# ========================
# 7) Save plot
# ========================
ggsave("output/fig1e1.tiff", plot = fig1e1,
       width = 7, height = 7, dpi = 800, units = "cm")
