############################################################
# qRT-PCR Analysis Script
# Barplot with Student's t-test (Nature style formatting)
# Author: Eulalio Gutemberg
# Date: 2025
############################################################

# ========================
# 1) Load packages
# ========================
library(readxl)
library(ggplot2)
library(dplyr)

# ========================
# 2) Input data
# ========================
# Set your input file and sheet name
file_path <- "data/paper_review.xlsx"
data <- read_xlsx(file_path, sheet = "fig1j")

# Define order of factors
data$name <- factor(data$name, levels = c("pro UBQ", "pro UBQ + 35S:T474D"))
data$time <- factor(data$time, levels = c("0h", "3h", "8h", "16h", "24h", "48h", "72h"))

# ========================
# 3) Summary statistics
# ========================
summary_data <- data %>%
  group_by(name, time) %>%
  summarise(
    mean_value = mean(value),
    se_value = sd(value) / sqrt(n()),
    .groups = "drop"
  )

# ========================
# 4) t-test
# ========================
t_test_results <- data %>%
  group_by(time) %>%
  summarise(
    p_value = t.test(value ~ name)$p.value,
    .groups = "drop"
  )

# Merge test results with summary
summary_data <- summary_data %>%
  left_join(t_test_results, by = "time") %>%
  group_by(time) %>%
  mutate(is_min = mean_value == min(mean_value),
         asterisk = ifelse(p_value < 0.05 & is_min, "*", ""))

# Ensure factor levels are consistent
summary_data$time <- factor(summary_data$time,
                            levels = c("0h", "3h", "8h", "16h", "24h", "48h", "72h"))

# ========================
# 5) Plot
# ========================
fig1j <- ggplot(summary_data, aes(x = time, y = mean_value, fill = name)) +
  geom_bar(stat = "identity", position = position_dodge(0.55), width = 0.53) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                position = position_dodge(0.55), width = 0.4, size = 0.4) +
  geom_jitter(data = data, aes(x = time, y = value, fill = name),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.55),
              alpha = 1, color = "black", size = 0.2) +
  scale_fill_manual(values = c("#574AE2", "#00A5CF")) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1),
                     expand = expansion(mult = c(0, 0.05))) +
  theme_classic() +
  labs(x = "", y = "Luciferase activity F/R", title = "") +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.35, "cm"),
    legend.margin = margin(t = -10, b = -21),
    text = element_text(family = "Arial", size = 7),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(size = 7, colour = "black"),
    axis.title.x = element_text(size = 7, colour = "black"),
    axis.title.y = element_text(size = 7, colour = "black"),
    plot.title = element_text(size = 7, hjust = 0.5),
    legend.text = element_text(size = 7),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    legend.justification = "center"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  # Significance bars
  geom_segment(data = summary_data %>% filter(p_value < 0.05),
               aes(x = as.numeric(time) - 0.25,
                   xend = as.numeric(time) + 0.25,
                   y = max(mean_value) + max(se_value) + 0.003,
                   yend = max(mean_value) + max(se_value) + 0.003),
               inherit.aes = FALSE, color = "black") +
  # Asterisks above bars
  geom_text(data = summary_data %>% filter(p_value < 0.05),
            aes(x = as.numeric(time),
                y = max(mean_value) + max(se_value) + 0.05,
                label = "*"),
            inherit.aes = FALSE, color = "black", size = 5, vjust = 1.5)

# Print figure
print(fig1j)

# ========================
# 6) Save figure
# ========================
ggsave("output/fig1j.tiff", plot = fig1j,
       width = 7.5, height = 7.5, dpi = 400, units = "cm")
