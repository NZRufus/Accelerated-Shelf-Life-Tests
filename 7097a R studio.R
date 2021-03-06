install.packages("datarium")
install.packages("rstatix")
library(datarium)
library(tidyverse)
library(ggpubr)
library(rstatix)
set.seed(123)
setwd("Z:/Tech Development/Tech Development projects/EM-089 Application Of Arrhenius Equation - for HbA1c products/July")
df <- read.csv("ASLT_7097a.csv")
df$�..Day <- as.factor(df$�..Day)
df$Temp <- as.factor(df$Temp)
df$Length <- as.factor(df$Length)
df %>% sample_n_by(Temp, Length, �..Day)
df %>%
  group_by(Temp, Length, �..Day) %>%
  get_summary_stats(Conc, type = "mean_sd")
bxp <- ggboxplot(
  df, x="�..Day", y="Conc",
  color = "Temp", palette = "jco", facet.by = "Length"
)
bxp
model <- lm(Conc ~ Temp*Length*�..Day, data = df)
ggqqplot(residuals(model))
shapiro.test(residuals(model))
df %>%
  group_by(Temp, Length, �..Day) %>%
  shapiro_test(Conc)
df %>%
  group_by( �..Day, Temp, Length) %>%
  shapiro_test(Conc)
ggqqplot(df,"Conc", ggtheme = theme_bw()) +
           facet_grid(Temp + Length ~ �..Day, labeller = "label_both")
df %>% levene_test(Conc ~ Temp*Length*�..Day)
res.aov <- df %>% anova_test(Conc ~ �..Day*Temp*Length)
res.aov
library(emmeans)
pwc <- df %>%
  group_by(Length, Temp) %>%
  emmeans_test(Conc ~ �..Day, p.adjust.method = "holm") %>%
  select(-df,-statistic,-p)
pwc %>% filter(Length == "0", Temp == "0")
pwc
pwc <- df %>%
  group_by(�..Day, Temp) %>%
  emmeans_test(Conc ~ Length, p.adjust.method = "holm") %>%
  select(-df,-statistic,-p)
pwc
pwc <- df %>%
  group_by(Length, �..Day ) %>%
  emmeans_test(Conc ~ Temp, p.adjust.method = "holm") %>%
  select(-df,-statistic,-p)
pwc
model <- lm(Conc ~ �..Day * Length, data = df)
df %>%
  group_by(�..Day) %>%
  anova_test(Conc ~ Temp, error = model)
pwc1 <- df %>%
  group_by(Temp) %>%
  emmeans_test(Conc ~ �..Day, p.adjust.method = "bonferroni")
pwc1
pwc2 <- df %>%
  group_by(Length) %>%
  emmeans_test(Conc ~ �..Day, p.adjust.method = "bonferroni")
pwc2
pwc3 <- df %>%
  group_by(�..Day) %>%
  emmeans_test(Conc ~ Length, p.adjust.method = "bonferroni")
pwc3
pwc4 <- df %>%
  group_by(�..Day) %>%
  emmeans_test(Conc ~ Temp, p.adjust.method = "bonferroni")
pwc4
res.aov
summary(df)
bxp1 <- ggboxplot(
  df, x="Temp", y="Conc",
  color = "�..Day", palette = "jco", facet.by = "Length")
bxp1
bxp2 <- ggboxplot(
  df, x="�..Day", y="Conc",
  color = "Temp", palette = "jco", facet.by = "Length")
bxp2
bxp3 <- ggviolin(df, x="�..Day", y="Conc",
                    color = "Temp", palette = "jco", facet.by = "Length")
bxp3

bxp4 <- ggviolin(df, x="�..Day", y="Conc",
                 color = "Length", palette = "jco", facet.by = "Temp")
bxp4
bxp5 <- ggscatter(df, x="�..Day", y="Conc",
                  color = "Length", palette = "jco", facet.by = "Temp")
bxp5
bxp6 <- ggboxplot(
  df, x="�..Day", y="Conc",
  color = "Length", palette = "jco", facet.by = "Temp")
bxp6
v1 <- ggplot(df, aes((�..Day),Conc ))
pwc1a <- df %>%
group_by(Temp) %>%
emmeans_test(Conc ~ �..Day, p.adjust.method = "bonferroni")
pwc1a
pwc2a <- df %>%
group_by(Length) %>%
emmeans_test(Conc ~ �..Day, p.adjust.method = "bonferroni")
pwc2a
pwc3a <- df %>%
  group_by(Length) %>%
  emmeans_test(Conc ~ Temp, p.adjust.method = "bonferroni")
pwc3a
pwc4a <- df %>%
  group_by(Temp) %>%
  emmeans_test(Conc ~ Length, p.adjust.method = "bonferroni")
pwc4a
