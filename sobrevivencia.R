# Instalar e carregar os pacotes necessários
pacman::p_load("survival", "tidyverse", "knitr", "survminer","ggfortify")

# Carregar o conjunto de dados
veteran

# Estimar a função de sobrevivência usando Kaplan-Meier por tipo de célula
kaplanMeier <- survfit(Surv(time, status) ~ celltype, data = veteran)
summary(kaplanMeier)

# Estimar a função de risco acumulado por tipo de célula
funcaoRiscoNelson <- survfit(Surv(time, status) ~ celltype, data = veteran, type = "fh")
summary(funcaoRiscoNelson)

# Teste de log-rank para comparar grupos por tipo de célula
survdiff(Surv(time, status) ~ celltype, data = veteran)

# Modelo de Cox Proporcional Hazards por tipo de célula
coxPorTipoCelula <- coxph(Surv(time, status) ~ celltype + age + karno + diagtime + prior, data = veteran)
summary(coxPorTipoCelula)

# Plot Kaplan-Meier para tipos de célula usando ggsurvplot
ggsurvplot(fit = kaplanMeier, 
           data = veteran,
           title = "Sobrevivência Geral",
           subtitle = "Estratificado por Tipo de Célula",
           font.title = c(22, "bold", "black"),
           ggtheme = theme_minimal() + 
             theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
             theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")),
           censor = TRUE, 
           censor.shape = "|",
           censor.size = 5,
           conf.int = TRUE, 
           surv.median.line = "hv", 
           xlab = "Dias", 
           ylab = "Probabilidade de Sobrevivência",
           font.x = c(18, "bold"), 
           font.y = c(18, "bold"), 
           font.xtickslab = c(14, "plain"), 
           font.ytickslab = c(14, "plain"),
           legend.title = "Tipos de Célula",
           legend.labs = c("Escamosa", "Pequenas Células", "Adenocarcinoma", "Grandes Células"), 
           surv.plot.height = 0.85, 
           risk.table = TRUE, 
           risk.table.height = 0.25, 
           risk.table.fontsize = 3.0)



# Agrupar os dados por tipo de célula
gruposPorCelula <- veteran %>%
  group_by(celltype) %>%
  summarise(
    media_idade = mean(age),
    mediana_tempo = median(time),
    min_tempo = min(time),
    max_tempo = max(time),
  )
gruposPorCelula


# Gráfico de dispersão: Karnofsky vs. tempo de sobrevivência
ggplot(veteran, aes(x = karno, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relação entre Karnofsky e Tempo de Sobrevivência",
       x = "Índice de Karnofsky",
       y = "Tempo de Sobrevivência (dias)")

# Verificar os níveis presentes no conjunto de dados
levels(veteran$celltype)

# Histograma da frequência dos tipos de célula com escala manual corrigida
ggplot(veteran, aes(x = factor(celltype), fill = factor(celltype))) +
  geom_bar() +
  labs(title = "Distribuição dos Tipos de Célula",
       x = "Tipo de Célula",
       y = "Frequência") +
  scale_fill_manual(values = c("squamous" = "red", "smallcell" = "blue", "adeno" = "green", "large" = "purple"))



library(ggplot2)

# Dados do teste de log-rank
resultadoLogRank <- survdiff(Surv(time, status) ~ celltype, data = veteran)

# Criar um data frame com os resultados
dadosLogRank <- data.frame(
  celltype = levels(veteran$celltype),
  observed = resultadoLogRank$obs,
  expected = resultadoLogRank$exp
)

# Gráfico de barras
ggplot(dadosLogRank, aes(x = celltype, fill = factor(celltype))) +
  geom_bar(aes(y = observed), stat = "identity", position = "dodge", alpha = 0.5) +
  geom_bar(aes(y = expected), stat = "identity", position = "dodge", color = "black", alpha = 0.5) +
  geom_text(aes(label = sprintf("(O - E)^2 / E\n%.2f", (observed - expected)^2 / expected), y = observed), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Resultados do Teste de Log-Rank por Tipo de Célula",
       x = "Tipo de Célula",
       y = "Número de Observações") +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal() +
  theme(legend.position = "none")


################################################################################################################################################
#########IDENTICO **NÃO USAR POIS O DE KM É ESTATISTICAMENTE O MESMO RESULTADO####################################
###################################### ESTATISTICAMENTE IGUAL AO PRIMEIRO GRÁFICO DO TESTE DE Kaplan-Meier, NÃO USAR ######################################
# Calcular a estimativa de Nelson-Aalen por tipo de célula
modeloNelsonTipoCelula <- survfit(Surv(time, status) ~ celltype, data = veteran)

# Extrair as estimativas de Nelson-Aalen para cada tipo de célula
dadosNelsonTipoCelula <- fortify(modeloNelsonTipoCelula, data = veteran)

# Plot Nelson-Aalen para tipos de célula
ggplot(dadosNelsonTipoCelula, aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Sobrevivência Geral (Nelson-Aalen)",
       x = "Dias",
       y = "Probabilidade de Sobrevivência",
       color = "Tipo de Célula") +
  theme_minimal() +
  theme(legend.position = "bottom")
###################################### ESTATISTICAMENTE IGUAL AO PRIMEIRO GRÁFICO DO TESTE DE Kaplan-Meier, NÃO USAR ######################################
################################################################################################################################################################################################################################################################################################################################################################################################################################################