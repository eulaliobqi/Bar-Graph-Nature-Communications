###############################################################
# Script genérico para análise estatística e gráficos de barras
# Teste ANOVA + Dunnett
# Saída no estilo publicação científica (Nature-like)
# Autor: [Seu Nome]
# Data: [AAAA-MM-DD]
###############################################################

# Instalar pacotes necessários (se ainda não estiverem instalados)
packages <- c(
  "readxl", 
  "ggplot2", 
  "dplyr", 
  "multcomp", 
  "multcompView", 
  "openxlsx", 
  "report"
)

installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Carregar pacotes
library(readxl)
library(ggplot2)
library(dplyr)
library(multcomp)
library(multcompView)
library(openxlsx)
library(report)

###############################################################
# Carregar os dados
###############################################################

# Substitua pelo caminho/arquivo correto
file_path <- "paper_review.xlsx"
data <- read_excel(file_path, sheet = "fig2b")

# Ajustar fatores
data$name <- factor(data$name, 
                    levels = c("Col-0", "35:ST474D-6", "nik1-1", "nik2-1", "nik1-1/nik2-1"))
data$time <- factor(data$time, 
                    levels = c("H2O", "InDNA", "UnDNA", "InRNA", "UnRNA"))

###############################################################
# Estatísticas descritivas
###############################################################
summary_data <- data %>%
  group_by(name, time) %>%
  summarize(
    mean_value = mean(value),
    sd_value = sd(value),
    se_value = sd(value) / sqrt(n()),
    .groups = "drop"
  )

###############################################################
# Função para ANOVA + teste de Dunnett
###############################################################
add_significance <- function(anova_model, group_name) {
  dunnett_test <- glht(anova_model, linfct = mcp(time = "Dunnett"))
  dunnett_summary <- summary(dunnett_test)
  print(paste("Resumo do teste de Dunnett para o grupo:", group_name))
  print(dunnett_summary)
  
  pvalues <- dunnett_summary$test$pvalues
  sig_levels <- ifelse(pvalues < 0.05, "*", "")
  time_levels <- levels(data$time)[-1] # exclui referência "H2O"
  
  significance_data <- data.frame(
    name = group_name,
    time = time_levels,
    significance = sig_levels
  )
  
  # Adicionar referência "H2O" sem significância
  significance_data <- rbind(
    data.frame(name = group_name, time = "H2O", significance = ""), 
    significance_data
  )
  
  return(significance_data)
}

###############################################################
# Rodar ANOVA + Dunnett para cada grupo
###############################################################
groups <- levels(data$name)
significance_list <- list()

for (grp in groups) {
  data_sub <- data %>% filter(name == grp)
  anova_model <- aov(value ~ time, data = data_sub)
  sig_data <- add_significance(anova_model, grp)
  significance_list[[grp]] <- sig_data
}

# Unir todos os resultados
significance_data <- do.call(rbind, significance_list)

# Juntar ao resumo
summary_data <- summary_data %>%
  left_join(significance_data, by = c("name", "time"))

###############################################################
# Gráfico
###############################################################
fig2b <- ggplot(summary_data, aes(x = name, y = mean_value, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(0.73), width = 0.71) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                position = position_dodge(0.73), width = 0.4, size = 0.4) +
  geom_jitter(data = data, aes(x = name, y = value, fill = time),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.73), 
              alpha = 1, color = "black", size = 0.3) +
  geom_text(aes(label = significance, y = mean_value + se_value + 0.5), 
            position = position_dodge(0.73), vjust = 0.5, size = 6) +
  scale_fill_manual(values = c('#009392', '#39b185', '#9ccb86', '#e9e29c', '#eeb479')) +
  scale_y_continuous(breaks = seq(0, 14, by = 2), expand = expansion(mult = c(0, 0.08))) +
  scale_x_discrete(labels = c("Col-0", 
                              "NIK1-T474D-6", 
                              expression(italic("nik1-1")), 
                              expression(italic("nik2-1")), 
                              expression(italic("nik1-1/nik2-1")))) +
  theme_classic() +
  labs(x = "", y = "mRNA/Actin", title = expression(italic("PsbP"))) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.35, "cm"),
    legend.margin = margin(t = -7, b = -20),
    text = element_text(family = "Arial", size = 9),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1, size = 9, colour = "black"),
    axis.text.y = element_text(size = 9, colour = "black"),
    axis.title.x = element_text(size = 9, colour = "black"),
    axis.title.y = element_text(size = 9, colour = "black"),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.text = element_text(size = 8),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    legend.justification = "center"
  ) +
  guides(fill = guide_legend(nrow = 1))

print(fig2b)

###############################################################
# Salvar gráfico
###############################################################
ggsave("fig2b_tiff.tiff", plot = fig2b, width = 9.5, height = 6.2, dpi = 800, unit = "cm")
