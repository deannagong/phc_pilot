# Load packages

library(psych)
library(readxl)
library(stringr)
library(lavaan)
library(car)
library(dplyr)
library(boot)
library(ggplot2)

# Data cleaning

df <- read_excel("C:/Users/Deanna/Documents/衛生所/衛生所人員_sas.xlsx")

## Map
likert_map <- c("從未" = 1, "很少" = 2, "偶爾" = 3, "常常" = 4, "總是" = 5,
                "非常不同意" = 1, "不同意" = 2, "普通"  = 3,  "同意"  = 4,  "非常同意" = 5,
                "極不好" = 1, "不好" = 2, "中等程度好" = 3, "好" = 4, "極好" = 5,
                "通常不可以" = 1, "有時不可以" = 2, "可以" = 3,
                "通常不正確" = 1, "有時不正確" = 2, "正確" = 3,
                "通常不合理" = 1, "有時不合理" = 2, "合理" = 3,
                "通常不明確" = 1, "有時不明確" = 2, "明確" = 3,
                "從未或幾乎從未" = 1, "不常" = 2, "有時候" = 3,
                "非常不享受" = 1, "不享受" = 2, "還算享受" =3, "很享受" = 4, "非常享受" = 5,
                "非常不滿意" = 1, "不滿意" = 2, "還算滿意" = 3, "很滿意" = 4, "非常滿意" = 5,
                "沒有" = 1, "不清楚" = 2, "有" = 3,
                "不需要" = 1, "不確定" = 2, "需要" = 3,
                "不需增加人力" = 0, "需要增加人力" = 1, "是" = 1, "否" = 0,
                "為醫師/藥師/醫檢師等特定醫事人員，不適用" = NA,
                "主管職不適用" = NA, "沒意見" = 2)

cols <- c("ot_weekend", "ot_weekday", "KPI_Stress", "Job_Burnout", "fatigue_01", "fatigue_02", 
          "fatigue_03", "fatigue_04", "fatigue_05", "talk_boss", "boss_flex", "cowork_help",
          "Workload_Affects_Quality", "life_quality", "life_enjoy", "health_sat",
          "Authority_Understanding", "Opinion_Expression", "Competency_Feeling", "Add_Staff",
          "Regular_Training", "Staff_Training", "Workload_Reasonable", "Responsibility_Clarity",
          "TransformToHealthCenter", 'Health_Center_1', 'Health_Center_2', 'Health_Center_3',
          'Health_Center_4', 'Health_Center_5', 'Health_Center_6', 'Health_Center_7', 
          "Rotation_Affects_Service", "Personnel_Rotation_Affects_Servi", "Public_Satisfaction",
          "Understand_CommunityNeeds", "TailoredServices", "CompetitionWithInstitutions","CooperationWithInstitutions",
          "ResourceIntegration", "BuildCommunityTrust", "CareSystem", "home_visit", "Hire_PublicHealth_Specialist"
)

for (col in cols) {
  unmatched <- setdiff(unique(df[[col]]), names(likert_map))
  if (length(unmatched) > 0) {
    cat("Unmatched in", col, ":", unmatched, "\n")
  }
}

## Convert
df[cols] <- lapply(df[cols], function(x) str_trim(as.character(x)))
df[cols] <- lapply(df[cols], function(x) likert_map[x])
df[,cols]

## Area regrouping
df <- df %>%
  mutate(
    area = factor(area,
                  levels = c("Medically underserved",
                             "Major cities",
                             "Indigenous/outlying islands",
                             "Other urban areas")),
    majorcity = ifelse(area == "Major cities", 1, 0),
    underserved = ifelse(area == "Medically underserved", 1, 0),
    otherurban = ifelse(area == "Other urban areas", 1, 0)
  )

# Test associations with area

df$area <- factor(df$area, levels = c(
  "Major cities",
  "Other urban areas",
  "Indigenous/outlying islands",
  "Medically underserved"
))

## Levene test

HC_levene <- list()

for (v in paste0("Health_Center_", 1:7)) {
  test <- leveneTest(as.formula(paste(v, "~ area")), data = df)
  HC_levene[[v]] <- test
}

HC_levene # all > 0.05, safe to proceed

## Kruskal-Wallis test

HC_kruskal <- list()

for (v in paste0("Health_Center_", 1:7)) {
  test <- kruskal.test(as.formula(paste(v, "~ area")), data = df)
  HC_kruskal[[v]] <- test
}

HC_kruskal # Health_Center_2,6,7 have significant associations with area

## Health_Center_2
boxplot(Health_Center_2 ~ area, data=df, xlab = "Area", ylab = "Level of Agreement", main = "Responsible for Long-term Care Services",
        col=c("lightblue", "lavender", "lightgreen", "lightsalmon"))
text(x = 4.4, y = 1, labels = paste0("p = ", signif(kruskal.test(Health_Center_2 ~ area, data=df)$p.value, 2), "**"))

## Health_Center_6
boxplot(Health_Center_6 ~ area, data=df, xlab = "Area", ylab = "Level of Agreement", main = "Responsible for Nutrition Promotion Services",
        col=c("lightblue", "lavender", "lightgreen", "lightsalmon"))
text(x = 4.4, y = 1, labels = paste0("p = ", signif(kruskal.test(Health_Center_6 ~ area, data=df)$p.value, 1), "**"))

## Health_Center_7
boxplot(Health_Center_7 ~ area, data=df, xlab = "Area", ylab = "Level of Agreement", main = "Need to Open Outpatient Services",
        col=c("lightblue", "lavender", "lightgreen", "lightsalmon"))
text(x = 4.4, y = 1, labels = paste0("p = ", signif(kruskal.test(Health_Center_7 ~ area, data=df)$p.value, 1), "**"))

## Check for TransformToHealthCenter

leveneTest(TransformToHealthCenter ~ area, data = df) # safe to proceed

kruskal.test(TransformToHealthCenter ~ area, data = df) # no significant association with area

boxplot(TransformToHealthCenter ~ area, data=df, xlab = "Area", ylab = "Level of Agreement", main = "Need to Transform into Community Health Center",
        col=c("lightblue", "lavender", "lightgreen", "lightsalmon"))
mtext("(sole focus on disease prevention & health promotion services)", side = 3, line = 0.5, cex = 0.8)
text(x = 4.4, y = 1, labels = paste0("p = ", signif(kruskal.test(TransformToHealthCenter ~ area, data=df)$p.value, 3)))

# EFA

test_fa <- df[, c("fatigue_01", "fatigue_02", "fatigue_03", "fatigue_04","fatigue_05", "Job_Burnout", "KPI_Stress", # stress-related
                  'Responsibility_Clarity', 'Workload_Reasonable', 'Authority_Understanding', 'Opinion_Expression', 'cowork_help', 'boss_flex', 'talk_boss', # working environment
                  'Health_Center_1', 'Health_Center_2', 'Health_Center_3', 'Health_Center_4', 'Health_Center_5', 'Health_Center_6', 'Health_Center_7', # PHC functions
                  "CareSystem", "Understand_CommunityNeeds", "TailoredServices", "ResourceIntegration", "BuildCommunityTrust", "CooperationWithInstitutions", "CompetitionWithInstitutions" # external relations
                  )]
fa.parallel(test_fa, fa="fa", fm="pa") # 5 factors suggested
fa_test <- fa(r = test_fa, nfactors = 5, fm = "pa", rotate = "promax", scores = "Bartlett")
print(fa_test$loadings, cutoff = 0.3)

## Remove variables that load to >1 factors
test_fa <- subset(test_fa, select = -c(KPI_Stress, Job_Burnout, Health_Center_1))
fa.parallel(test_fa, fa="fa", fm="pa") # 4 factors suggested now, aligns with our theory
fa_test <- fa(r = test_fa, nfactors = 4, fm = "pa", rotate = "promax", scores = "Bartlett")
print(fa_test$loadings, cutoff = 0.3)

## Remove variable that doesn't load to any factor
test_fa <- subset(test_fa, select = -c(CompetitionWithInstitutions))
fa_test <- fa(r = test_fa, nfactors = 4, fm = "pa", rotate = "promax", scores = "Bartlett")
print(fa_test$loadings, cutoff = 0.3)

## Remove variable that doesn't load to any factor
test_fa <- subset(test_fa, select = -c(cowork_help))
fa_test <- fa(r = test_fa, nfactors = 4, fm = "pa", rotate = "promax", scores = "Bartlett")
print(fa_test$loadings, cutoff = 0.3)

## Remove variable with lowest loading
test_fa <- subset(test_fa, select = -c(talk_boss))
fa_test <- fa(r = test_fa, nfactors = 4, fm = "pa", rotate = "promax", scores = "Bartlett")
print(fa_test$loadings, cutoff = 0.3)

## Remove variable that load to >1 factors
test_fa <- subset(test_fa, select = -c(BuildCommunityTrust))
fa_test <- fa(r = test_fa, nfactors = 4, fm = "pa", rotate = "promax", scores = "Bartlett")
print(fa_test$loadings, cutoff = 0.3)

## CLEAN!

## Test alpha for each factor
psych::alpha(df[, c("fatigue_01", "fatigue_02", "fatigue_03", "fatigue_04","fatigue_05")]) # no need for alteration
psych::alpha(df[, c('Responsibility_Clarity', 'Workload_Reasonable', 'Authority_Understanding', 'Opinion_Expression', 'boss_flex')]) # dropping boss_flex would improve alpha, but keep for now
psych::alpha(df[, c('Health_Center_2', 'Health_Center_3', 'Health_Center_4', 'Health_Center_5', 'Health_Center_6', 'Health_Center_7')]) # no need for alteration
psych::alpha(df[, c("Understand_CommunityNeeds", "CareSystem", "TailoredServices", "ResourceIntegration", "CooperationWithInstitutions")]) # no need for alteration

## Test alpha for all variables
pred <- df[, c("fatigue_01", "fatigue_02", "fatigue_03", "fatigue_04","fatigue_05",
               'Responsibility_Clarity', 'Workload_Reasonable', 'Authority_Understanding', 'Opinion_Expression', 'boss_flex',
               'Health_Center_2', 'Health_Center_3', 'Health_Center_4', 'Health_Center_5',
               'Health_Center_6', 'Health_Center_7', 'CareSystem', "CooperationWithInstitutions", 
               "Understand_CommunityNeeds", "TailoredServices", "ResourceIntegration")]
psych::alpha(pred, check.keys=TRUE)$total$raw_alpha

## Bootstrap confidence interval (95%)
set.seed(123)  # for reproducibility
boot_alpha <- function(data, indices) {
  d <- data[indices, ]   # resample rows
  return(psych::alpha(d, check.keys=TRUE)$total$raw_alpha)
}
boot_res <- boot(data = pred, statistic = boot_alpha, R = 1000) # 1000 samples
boot.ci(boot_res, type = "perc")

# CFA

cfa_model1 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression + CooperationWithInstitutions
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration + boss_flex
'
fit_cfa1 <- cfa_fit_plot(cfa_model1, data = df) # poor SRMR (> 0.08), try dropping boss_flex

cfa_model2 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression + CooperationWithInstitutions
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration
'
fit_cfa2 <- cfa_fit_plot(cfa_model2, data = df) # SRMR is still poor (> 0.08), drop CooperationWithInstitutions (R-squared = 0.000)

cfa_model3 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration
'
fit_cfa3 <- cfa_fit_plot(cfa_model3, data = df) # good!

## Other way of writing

# latent <- list(
#   Working_Environment = c("Responsibility_Clarity", "Workload_Reasonable", "Authority_Understanding", "Opinion_Expression"),
#   Fatigue = paste0("fatigue_0", 1:5),
#   PHC_task = paste0("Health_Center_", 2:6),
#   Community = c("Understand_CommunityNeeds", "CareSystem", "TailoredServices", "ResourceIntegration")
# )
# 
# cfa.model <- write_lavaan(latent = latent)
# cat(cfa.model)
# 
# fit.cfa <- cfa_fit_plot(cfa.model, df)

fit_cfa <- fit_cfa3
nice_fit(fit_cfa, nice_table = TRUE) # this is for convenience of glancing at the GOF indicators, guidelines can be loosened for smaller sample size
AVE(fit_cfa) # > 0.36 = acceptable

# SEM

DV <- "TransformToHealthCenter"
IV <- "Community"

sem_test1 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration
  
  # Test potential relationships broadly
  Fatigue ~ Working_Environment
  TransformToHealthCenter ~ PHC_task + Community + Fatigue + Working_Environment
'
sem_fit1 <- sem(sem_test1, data = df, std.lv = TRUE, estimator = "WLSMV" , ordered = TRUE)
summary(sem_fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
### TransformToHealthCenter is directly associated with Community, seems like Working_Environment & Fatigue
### don't directly influence TransformToHealthCenter. Maybe they influence PHC_task?

sem_test2 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration
  
  Fatigue ~ Working_Environment
  PHC_task ~ Working_Environment + Fatigue
  TransformToHealthCenter ~ PHC_task + Community
'
sem_fit2 <- sem(sem_test2, data = df, std.lv = TRUE, estimator = "WLSMV" , ordered = TRUE)
summary(sem_fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
### SRMR is very poor (> 0.08), check modindices for unaddressed covariances

modindices(sem_fit2, sort. = T)
### seems like we need to incorporate a covariance between Community & PHC_task

sem_test3 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration
  
  Fatigue ~ Working_Environment
  PHC_task ~ Working_Environment + Fatigue
  TransformToHealthCenter ~ PHC_task + Community
  
  PHC_task ~~ Community
'
sem_fit3 <- sem(sem_test3, data = df, std.lv = TRUE, estimator = "WLSMV" , ordered = TRUE)
summary(sem_fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
### SRMR is getting better (0.083), check modindices for unaddressed covariances

modindices(sem_fit3, sort. = T)
### seems like we need to incorporate a covariance between Community & Fatigue

sem_test4 <- '
  Working_Environment =~ Responsibility_Clarity + Workload_Reasonable + Authority_Understanding + Opinion_Expression
  Fatigue =~ fatigue_01 + fatigue_02 + fatigue_03 + fatigue_04 + fatigue_05
  PHC_task =~ Health_Center_2 + Health_Center_3 + Health_Center_4 + Health_Center_5 + Health_Center_6 + Health_Center_7
  Community =~ Understand_CommunityNeeds + CareSystem + TailoredServices + ResourceIntegration
  
  Fatigue ~ Working_Environment
  PHC_task ~ Working_Environment + Fatigue
  TransformToHealthCenter ~ PHC_task + Community
  
  PHC_task ~~ Community
  Community ~~ Fatigue
'
sem_fit4 <- sem(sem_test4, data = df, std.lv = TRUE, estimator = "WLSMV" , ordered = TRUE)
summary(sem_fit4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
### good! found the final model.

fit_sem <- sem_fit4

nice_fit(fit_sem, nice_table = TRUE) # this is for convenience of glancing at the GOF indicators, guidelines can be loosened for smaller sample size

## Draw the plot

nice_lavaanPlot(fit_sem) # method 1

lavaanPlot(
  model = fit_sem,
  coefs = TRUE,      # show path coefficients
  stand = TRUE,      # standardized
  stars = "regress"  # significance stars on regression paths
) # method 2
