#-----------------------------------------------------------
#🟣 PHASE 1 — PRÉPARATION DES DONNÉES
-----------------------------------------------------------#
  
#🔵 1.1 Importation des données & nettoyage des noms#
# Packages
library(tidyverse)
library(naniar)
library(readxl)
library(janitor)
library(lubridate)
library(VIM)
library(GGally)
# Importation du dataset initial
df_initial <- read_excel("C:/Users/ASUS/Downloads/Projet Stat/DATA.xlsx", na = "")
view(df_initial)
# Nettoyage des noms de colonnes
names(df_initial) <- df_initial %>% names() %>% 
  tolower() %>% 
  gsub(" ", "_", .) %>% 
  gsub("-", "_", .)
# Aperçu général
glimpse(df_initial)
summary(df_initial)
df_initial %>% tabyl(sex)

#==>Le jeu de données comprend 2 000 adultes âgés de 19 à 64 ans, majoritairement des hommes. Les variables anthropométriques et de performance physique présentent une forte variabilité, ce qui rend l’échantillon adapté aux analyses statistiques et aux modèles de régression.



#🔵 1.2 Conversion des types & extraction temporelle#

#Conversion des types
names(df_initial)
df_initial <- df_initial %>%
  mutate(
    participant_id = as.integer(participant_id),
    sex = as.factor(sex),
    age = as.integer(age),
    measurement_date = as.Date(measurement_date),
    bmi = as.numeric(bmi),
    percent_body_fat = as.numeric(percent_body_fat),
    hand_grip_strength_kg = as.numeric(hand_grip_strength_kg),
    sit_and_reach_cm = as.numeric(sit_and_reach_cm),
    sit_ups_count = as.integer(sit_ups_count),
    vo2_estimate_ml_per_kg_min = as.numeric(vo2_estimate_ml_per_kg_min),
    measurement_year = year(measurement_date),
    measurement_month = month(measurement_date)
  )
#==>La conversion garantit des analyses statistiques valides.
names(df_initial)
# Extraction temporelle 
df_initial <- df_initial %>%
  mutate(
    measurement_year = year(measurement_date),
    measurement_month = month(measurement_date)
  )

#==>L’extraction de l’année et du mois à partir de la date de mesure permet d’introduire une dimension temporelle dans l’analyse. Cette transformation facilite l’étude de l’évolution des performances physiques dans le temps ainsi que la détection d’éventuelles variations saisonnières ou interannuelles. Elle permet également d’explorer la stabilité des mesures selon les périodes de collecte.


#🔵 1.3 Valeurs manquantes#
gg_miss_var(df_initial) +
  labs(title = "Pourcentage de valeurs manquantes")
mean(is.na(df_initial)) * 100

#==>Le jeu de données ne présente aucune valeur manquante, ce qui garantit l’intégrité de l’échantillon et la fiabilité des analyses statistiques ultérieures.


#🔵 1.4 Détection des outliers#

#Méthode univariée — Boxplots
numeric_desc <- df_initial %>% select(where(is.numeric))
numeric_desc %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = "#E64B35") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplots des variables numériques",
       x = "Variable", y = "Valeur")

#==>L'analyse des boxplots révèle la présence de valeurs aberrantes (outliers) pour plusieurs variables, notamment vo2_estimate_ml_per_kg_min et sit_and_reach_cm qui présentent des outliers extrêmes. La variable BMI montre une distribution relativement symétrique avec peu d'outliers, tandis que age et percent_body_fat présentent une asymétrie vers les valeurs élevées. Les variables sit_ups_count et hand_grip_strength_kg affichent quelques valeurs extrêmes mais restent globalement bien distribuées. Ces observations suggèrent la nécessité de vérifier les outliers avant l'analyse statistique et d'envisager des tests non-paramétriques pour les variables présentant de fortes asymétries ou de nombreux outliers.


#🔵 1.5 Remplacement automatique des outliers par la moyenne#

replace_outliers_with_mean <- function(x) {
  if (!is.numeric(x)) return(x)
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  mean_val <- mean(x, na.rm = TRUE)
  x[x < lower | x > upper] <- mean_val
  return(x)
}
df_cleaned <- df_initial %>%
  mutate(across(where(is.numeric), replace_outliers_with_mean))
#Verification#
# Liste des variables à exclure
vars_to_exclude <- c(
  "participant_id", 
  "age",
  "measurement_year",
  "measurement_month"
)
# 1) Sélection des variables numériques sauf celles à exclure
num_vars_initial <- df_initial %>%
  select(where(is.numeric)) %>%
  select(-all_of(vars_to_exclude))
num_vars_cleaned <- df_cleaned %>%
  select(where(is.numeric)) %>%
  select(-all_of(vars_to_exclude))
# 2) Fusion avant/après au format long
df_compare_all <- bind_rows(
  num_vars_initial %>% mutate(version = "before"),
  num_vars_cleaned %>% mutate(version = "after")
) %>%
  pivot_longer(
    cols = -version,
    names_to = "variable",
    values_to = "value"
  )
# 3) Visualisation finale
ggplot(df_compare_all, aes(x = version, y = value, fill = version)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("before" = "#E64B35", "after" = "#4DBBD5")) +
  labs(
    title = "Comparaison avant/après remplacement des outliers",
    x = "Version",
    y = "Valeur"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
#==>Outliers réglés


#🔵 1.6 Encodage du sexe#

df_cleaned <- df_cleaned %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female"))

#==>L’encodage de la variable sex en valeurs numériques a été réalisé afin de permettre son intégration dans les modèles statistiques et les régressions linéaires. Ce codage est conforme à celui utilisé dans l’article de référence, ce qui assure la comparabilité des résultats. La conversion en facteur garantit en outre une interprétation correcte lors des analyses graphiques et statistiques.



#🔵 1.7 Nettoyage final (âges et BMI réalistes)#

df_cleaned <- df_cleaned %>%
  filter(bmi > 10, bmi < 60, age > 18, age < 65)
# INTERPRÉTATION :
# Le filtrage final élimine uniquement les valeurs physiologiquement irréalistes 
# ou non pertinentes pour une population adulte. Les BMI < 10 ou > 60 sont retirés 
# car ils correspondent à des erreurs ou à des situations cliniques extrêmes. 
# De même, seules les personnes âgées de 19 à 64 ans sont conservées afin d’exclure 
# les adolescents en croissance et les seniors dont les performances diminuent fortement. 
# Ce nettoyage assure un échantillon homogène et aligné sur la méthodologie de l’article scientifique.
#==>⭐ 3. Pourquoi on n’a pas supprimé ces valeurs lors de la détection des outliers ?
#Parce que :
#✔️ La méthode IQR repère les valeurs extrêmes, mais ne doit PAS supprimer des données valides
#→ Exemple : une femme très flexible ou quelqu’un très fort en grip → extrême mais réel.






#-----------------------------------------------------------
#  🟣 PHASE 2 — ANALYSE DESCRIPTIVE
#-----------------------------------------------------------

# PACKAGES
library(psych)
library(kableExtra)

# 🔵 2.1 Tableau descriptif des variables numériques#
numeric_desc <- df_cleaned %>% select(
  age, bmi, percent_body_fat,
  hand_grip_strength_kg, sit_and_reach_cm,
  sit_ups_count, vo2_estimate_ml_per_kg_min
)
describe_numeric <- psych::describe(numeric_desc)
kable(describe_numeric, caption = "Statistiques descriptives des variables numériques") %>%
  kable_styling(full_width = FALSE)

#==>vars: Numéro d’ordre de la variable (index). Purement indicatif.
#n: Taille de l’échantillon, c’est-à-dire le nombre d’observations disponibles pour cette variable.
#mean: Moyenne arithmétique : valeur centrale moyenne des données.
#sd: Écart-type : mesure de la dispersion autour de la moyenne.
#median: Médiane : valeur centrale séparant l’échantillon en deux sous-groupes de même taille.
#trimmed: Moyenne tronquée (souvent à 10%) : moyenne calculée après exclusion des valeurs extrêmes.
#mad: Median Absolute Deviation : dispersion robuste basée sur la médiane (moins sensible aux outliers).
#min: Valeur minimale observée.
#max: Valeur maximale observée.
#range: Étendue : max – min.
#skew: Coefficient d’asymétrie : <0 asymétrie gauche, >0 asymétrie droite, ≈0 distribution symétrique.
#kurtosis: Coefficient d’aplatissement : >0 distribution plus pointue, <0 distribution aplatie.
#se: Erreur standard de la moyenne : précision de l’estimation de la moyenne (sd / √n).
##Les statistiques descriptives montrent que les 7 variables numériques sont mesurées sur un échantillon large (n = 2000), garantissant une bonne stabilité des estimations. Les moyennes et les médianes sont très proches, indiquant des distributions globalement symétriques et peu influencées par des valeurs extrêmes. La dispersion varie selon les variables : le BMI et le nombre de sit-ups présentent une variabilité faible, tandis que l’âge, la force de préhension et le pourcentage de masse grasse montrent des variations plus importantes entre individus. Les coefficients d’asymétrie (skew) et d’aplatissement (kurtosis) sont proches de zéro, suggérant des distributions proches de la normalité. Quelques valeurs extrêmes sont observées, notamment pour la flexibilité (sit_and_reach_cm), ce qui nécessite potentiellement une vérification lors de la préparation des données. Globalement, les variables sont de bonne qualité statistique pour une modélisation ultérieure.


#🔵 2.2 Répartition du sexe#
df_cleaned %>% 
  tabyl(sex) %>%
  adorn_pct_formatting() %>%
  kable(caption = "Répartition du sexe (effectifs et pourcentages)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

#==>L’échantillon est majoritairement masculin (~60%), ce qui peut influencer certaines analyses.


#🔵 2.3 Histogrammes#

numeric_desc %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "#4DBBD5", color = "white") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  labs(title = "Histogrammes des variables numériques",
       x = "Valeur", y = "Fréquence")

#==>Distributions globalement unimodales. Certaines variables (sit-ups, percent fat) sont légèrement asymétriques.


#🔵 2.4 Statistiques par sexe#

table_sex_stats <- df_cleaned %>%
  group_by(sex) %>%
  summarise(
    mean_hgs = mean(hand_grip_strength_kg),
    sd_hgs = sd(hand_grip_strength_kg),
    mean_vo2 = mean(vo2_estimate_ml_per_kg_min),
    sd_vo2 = sd(vo2_estimate_ml_per_kg_min),
    mean_bmi = mean(bmi),
    sd_bmi = sd(bmi),
    .groups = "drop"
  )
kable(table_sex_stats,
      caption = "Comparaison descriptive des performances par sexe",
      digits = 2) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

#==>Les hommes ont des performances physiques supérieures (HGS, VO2), comme attendu biologiquement.


#🔵 2.5 Matrice de corrélation#

vars_cor <- df_cleaned %>% 
  select(
    age, bmi, percent_body_fat,
    hand_grip_strength_kg,
    vo2_estimate_ml_per_kg_min,
    sit_ups_count, sit_and_reach_cm
  )
cor_mat <- cor(vars_cor, use = "pairwise.complete.obs", method = "pearson")
cor_long <- as.data.frame(as.table(cor_mat))
colnames(cor_long) <- c("Var1", "Var2", "Correlation")
ggplot(cor_long, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Matrice de corrélation",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_fixed()

## Interprétation de la matrice de corrélation complète
# La matrice de corrélation met en évidence plusieurs relations importantes
# entre les variables anthropométriques et les performances physiques.

# Pourcentage de masse grasse et force de préhension :
# Une corrélation négative modérée (r = -0.53) est observée,
# indiquant qu’une augmentation du pourcentage de masse grasse
# est associée à une diminution significative de la force musculaire.
# Ceci est cohérent sur le plan physiologique : un excès de masse adipeuse
# est souvent associé à une altération de la performance musculaire.

# Pourcentage de masse grasse et VO₂ estimé :
# Une corrélation négative modérée (r = -0.40) montre qu’un taux
# de masse grasse plus élevé est lié à une capacité cardio-respiratoire
# plus faible, ce qui reflète une diminution de l’endurance aérobie.

# BMI et force de préhension :
# Une corrélation positive modérée (r = 0.34) suggère que les individus
# ayant un BMI plus élevé présentent en moyenne une force musculaire
# légèrement plus importante, pouvant s’expliquer par un effet combiné
# de la masse musculaire et de la masse grasse.

# Force de préhension et VO₂ :
# Une corrélation positive faible à modérée (r = 0.28) indique
# une association physiologique cohérente entre la condition musculaire
# et la capacité aérobie.

# Âge et performances physiques :
# L’âge est négativement corrélé avec le VO₂ estimé (r = -0.28),
# le nombre de sit-ups (r = -0.14) et la souplesse (r = -0.06),
# traduisant une diminution progressive des performances physiques
# avec l’avancée en âge.

# Relations entre sit-ups et souplesse :
# La corrélation est faible positive (r = 0.07), indiquant que
# ces deux composantes de la condition physique représentent
# des capacités physiques distinctes.

# BMI et pourcentage de masse grasse :
# Une corrélation positive faible (r = 0.16) confirme que le BMI
# est un indicateur partiel du tissu adipeux, sans en être
# un reflet parfaitement fidèle.

# Enfin, aucune corrélation forte (|r| > 0.8) n’est observée entre les variables.
# Ceci indique l’absence de multicolinéarité sévère susceptible
# de perturber l’estimation des modèles de régression.


#🔵 2.6 Densité HGS par sexe#

ggplot(df_cleaned, aes(x = hand_grip_strength_kg, fill = sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
    values = c(
      "Male" = "#4DBBD5",    # Bleu clair
      "Female" = "#E64B35"   # Rouge corail
    )
  ) +
  labs(
    title = "Distribution de la force de préhension par sexe",
    x = "Force de préhension (kg)",
    y = "Densité",
    fill = "Sexe"
  ) +
  theme_minimal()

#==>La distribution de la force de préhension diffère nettement selon le sexe. La courbe associée aux hommes est globalement décalée vers des valeurs plus élevées, indiquant une force de préhension moyenne supérieure à celle des femmes. À l’inverse, la distribution des femmes est centrée sur des valeurs plus faibles et présente une dispersion légèrement plus réduite. Le chevauchement partiel entre les deux distributions montre toutefois qu’il existe une variabilité intra-groupe. Ces résultats sont cohérents avec les différences physiologiques liées à la masse musculaire et confirment que le sexe constitue un facteur explicatif important de la force de préhension.


#🔵 2.7 Relation Âge – Force#

ggplot(df_cleaned, aes(age, hand_grip_strength_kg, color = sex)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(
    values = c(
      "Male"   = "#4DBBD5",  
      "Female" = "#E64B35"    
    )
  ) +
  labs(
    title = "Relation âge – force de préhension (comme dans l’article)",
    x = "Âge",
    y = "Force de préhension (kg)",
    color = "Sexe"
  ) +
  theme_minimal()

#==>Relation âge – force de préhension par sexe)
#Le nuage de points met en évidence une relation globale faiblement négative entre l’âge et la force de préhension, traduisant une légère diminution de la force musculaire avec l’avancée en âge. À tout âge, les hommes présentent une force de préhension nettement supérieure à celle des femmes, ce qui est cohérent avec les différences physiologiques liées à la masse musculaire. Les droites de régression montrent une pente légèrement décroissante pour les deux sexes, indiquant que le déclin de la force avec l’âge concerne aussi bien les hommes que les femmes. La dispersion des points autour des droites traduit toutefois une variabilité individuelle importante.








#-----------------------------------------------------------
#🟣 PHASE 3 — TEST DE NORMALITÉ 
#-----------------------------------------------------------#


# 🔵 3.1 Test de Shapiro-Wilk pour chaque variable numérique

numeric_vars_to_test <- df_cleaned %>% 
  select(age, bmi, percent_body_fat, hand_grip_strength_kg,
         sit_and_reach_cm, sit_ups_count, vo2_estimate_ml_per_kg_min)

# Fonction pour appliquer Shapiro + stocker résultats
shapiro_results <- lapply(numeric_vars_to_test, shapiro.test)

# Convertir en tableau lisible
shapiro_table <- tibble(
  variable = names(shapiro_results),
  W = sapply(shapiro_results, function(x) round(x$statistic, 4)),
  p_value = sapply(shapiro_results, function(x) round(x$p.value, 4))
)

kable(shapiro_table, caption = "Résultats du test de Shapiro–Wilk") %>%
  kable_styling(full_width = FALSE)

# ==>Le test de Shapiro–Wilk appliqué aux 7 variables numériques
# montre que pour toutes les variables, la p-value < 0.05,
# ce qui conduit à rejeter l’hypothèse de normalité.

# 👉 Cependant, Shapiro–Wilk est très sensible lorsqu'un échantillon est large (n = 2000).
# Même de légères déviations par rapport à la normale entraînent une p-value très faible,
# ce qui peut conduire à des conclusions trop strictes.

# ⭐ Conclusion :
# Le test de Shapiro–Wilk suggère que les variables ne suivent pas une distribution normale,
# mais, en raison de la taille importante de l’échantillon, il est nécessaire de compléter
# l’évaluation de normalité avec des méthodes visuelles (QQ-plots) et un test plus robuste.

#🔵 3.2 Les QQ-plots#
numeric_vars_to_test %>%
  pivot_longer(everything()) %>%
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "QQ-plots des variables numériques")
# ==>L’inspection des QQ-plots apporte une vision qualitative de la normalité.
# Elle révèle que certaines variables (BMI, sit_and_reach_cm, vo2_estimate)
# suivent globalement une droite, ce qui est compatible avec une distribution normale.

# À l’inverse, d’autres variables (age, percent_body_fat, hand_grip_strength_kg, sit_ups_count)
# présentent des déviations importantes :
#   - asymétrie marquée pour age,
#   - paliers pour percent_body_fat (valeurs répétées),
#   - dispersion élevée dans hand_grip_strength_kg,
#   - distribution discrète en « marches » pour sit_ups_count.

# ⭐ Conclusion :
# Les QQ-plots confirment que certaines variables semblent proches de la normale,
# contrairement à ce qu’indique Shapiro–Wilk.  
# Pour trancher définitivement, nous appliquons un test plus robuste
# adapté aux grands échantillons : l’Anderson–Darling test (AD-test).

#🔵 3.3 Test Anderson–Darling#
# Charger le package
library(nortest)

# Sélection des variables numériques à tester
vars_to_test <- df_cleaned %>% 
  select(age, bmi, percent_body_fat, hand_grip_strength_kg,
         sit_and_reach_cm, sit_ups_count, vo2_estimate_ml_per_kg_min)

# Appliquer AD test à chaque variable
ad_results <- lapply(vars_to_test, ad.test)

# Construire tableau final
ad_table <- tibble(
  variable = names(ad_results),
  statistic = sapply(ad_results, function(x) round(x$statistic, 4)),
  p_value   = sapply(ad_results, function(x) round(x$p.value, 4)),
  normality = ifelse(
    sapply(ad_results, function(x) x$p.value) > 0.05,
    "Normale",
    "Non normale"
  )
)

# Affichage formaté comme le tableau Shapiro
kable(ad_table, caption = "Résultats du test Anderson–Darling") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# ==>Le test AD, plus puissant que Shapiro et mieux adapté aux grands n,
# confirme que seules quelques variables suivent réellement une loi normale.

# Résultats :
#   ✔ Variables normales : BMI, Sit-and-reach, VO2-estimate
#   ❌ Variables non normales : Age, Percent body fat, Hand grip strength, Sit-ups

# ⭐ Conclusion finale :
# Le test AD confirme que seules trois variables (bmi, sit_and_reach_cm, vo2_estimate_ml_per_kg_min)
# peuvent être considérées comme normalement distribuées.
# Les autres variables présentent des déviations significatives par rapport à la normale.

# 👉 Conséquence méthodologique :
# - Tests paramétriques possibles pour les variables normales.
# - Tests non paramétriques nécessaires pour les variables non normales.




#-----------------------------------------------------------
#🟣 PHASE 4 — TESTS D'HYPOTHESES
#-----------------------------------------------------------#

#-----------------------------------------------------------
#🟣 PHASE 4.1 — TESTS PARAMETRIQUES
#-----------------------------------------------------------#

#🔵 4.1.1 Test F de Fisher – Homogénéité des variances (Homme vs Femme)
#👉 Ce test sert uniquement à décider quel type de t-test utiliser :
#      -variances égales → t-test classique (Student)
#      -variances inégales → t-test de Welch

#🔹 Hypothèses du Test F (ajouter capture discussion farah+sirine)

#-----------------------------------------------------------
# 🔵 Test F de Fisher – Homogénéité des variances (Homme vs Femme)
#-----------------------------------------------------------

# 1. Test F pour BMI
test_bmi <- var.test(bmi ~ sex, data = df_cleaned)

# 2. Test F pour la souplesse (Sit and Reach)
test_sit <- var.test(sit_and_reach_cm ~ sex, data = df_cleaned)

# 3. Test F pour VO2 estimé
test_vo2 <- var.test(vo2_estimate_ml_per_kg_min ~ sex, data = df_cleaned)

# Tableau récapitulatif CORRIGÉ
fisher_table <- tibble(
  Variable = c("BMI", "Souplesse (Sit & Reach)", "VO2 estimé"),
  
  F_statistic = c(
    round(as.numeric(test_bmi$statistic), 4),
    round(as.numeric(test_sit$statistic), 4),
    round(as.numeric(test_vo2$statistic), 4)
  ),
  
  p_value = c(
    round(test_bmi$p.value, 4),
    round(test_sit$p.value, 4),
    round(test_vo2$p.value, 4)
  ),
  
  Variances = c(
    ifelse(test_bmi$p.value  > 0.05, "Égales", "Inégales"),
    ifelse(test_sit$p.value  > 0.05, "Égales", "Inégales"),
    ifelse(test_vo2$p.value  > 0.05, "Égales", "Inégales")
  )
)

# Affichage
kable(
  fisher_table,
  caption = "Test F de Fisher – Comparaison des variances Homme/Femme"
) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


#INTERPRETATION:
#==>Le test F de Fisher a été utilisé afin de vérifier l’égalité des variances entre les hommes et les femmes pour les variables à distribution normale. Les résultats montrent que les variances du BMI diffèrent significativement entre les sexes (p < 0.001), tandis que celles de la souplesse et du VO₂ estimé peuvent être considérées comme homogènes (p > 0.05). En conséquence, un t-test de Welch a été retenu pour le BMI, alors qu’un t-test classique a été appliqué pour la souplesse et le VO₂.
#==>Le test F de Fisher est conçu uniquement pour comparer les variances de deux groupes. Dans cette étude, la variable qualitative « sexe » ne comporte que deux modalités (homme et femme)


#-----------------------------------------------------------
library(dplyr)
library(knitr)
library(kableExtra)

#🔵 4.1.4 : Test t à 1 échantillon (comparaison à la valeur théorique)

#🔹 Hypothèses du Test t
#H₀ : μ = μ₀ (la moyenne observée est égale à la valeur théorique)

#H₁ : μ ≠ μ₀ (la moyenne observée est différente de la valeur théorique)

### --- 1. Valeurs de référence issues de l’article ---
ref_values <- list(
  bmi = 22.8,
  sit = 17.56,
  vo2 = 37.3
)

### --- 2. Tests t à 1 échantillon ---
#🔹 1. BMI

#H₀ : μ_BMI = 22

#H₁ : μ_BMI ≠ 22

test_bmi_1 <- t.test(df_cleaned$bmi, mu = ref_values$bmi)
#🔹 2. Souplesse (Sit & Reach)

#H₀ : μ_souplesse = 28

#H₁ : μ_souplesse ≠ 28
test_sit_1 <- t.test(df_cleaned$sit_and_reach_cm, mu = ref_values$sit)
#🔹 3. VO2 estimé

#H₀ : μ_VO2 = 42

#H₁ : μ_VO2 ≠ 42
test_vo2_1 <- t.test(df_cleaned$vo2_estimate_ml_per_kg_min, mu = ref_values$vo2)

### --- 3. Construction du tableau final ---
ttest1_table <- tibble(
  Variable = c("BMI", "Souplesse (Sit & Reach)", "VO2 estimé"),
  
  `Valeur théorique (µ₀)` = c(
    ref_values$bmi,
    ref_values$sit,
    ref_values$vo2
  ),
  
  `Moyenne observée` = c(
    round(mean(df_cleaned$bmi, na.rm = TRUE), 2),
    round(mean(df_cleaned$sit_and_reach_cm, na.rm = TRUE), 2),
    round(mean(df_cleaned$vo2_estimate_ml_per_kg_min, na.rm = TRUE), 2)
  ),
  
  `t statistic` = c(
    round(test_bmi_1$statistic, 4),
    round(test_sit_1$statistic, 4),
    round(test_vo2_1$statistic, 4)
  ),
  
  `p-value` = c(
    round(test_bmi_1$p.value, 4),
    round(test_sit_1$p.value, 4),
    round(test_vo2_1$p.value, 4)
  ),
  
  `Conclusion` = c(
    ifelse(test_bmi_1$p.value > 0.05, "≃ Égale à µ₀", "≠ Différente de µ₀"),
    ifelse(test_sit_1$p.value > 0.05, "≃ Égale à µ₀", "≠ Différente de µ₀"),
    ifelse(test_vo2_1$p.value > 0.05, "≃ Égale à µ₀", "≠ Différente de µ₀")
  )
)

### --- 4. Affichage kable propre ---
kable(
  ttest1_table,
  caption = "Test t à 1 échantillon – Comparaison des moyennes avec les valeurs de référence de l’article"
) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

#➡️ Le BMI moyen de la population étudiée est significativement plus élevé que celui rapporté dans l’article.
#   Cela suggère que les participants sont globalement plus corpulents que la population de référence.
#➡️ La souplesse moyenne dans notre échantillon est très significativement plus faible que la valeur de référence.
#   L’écart est très important, ce qui montre que la population étudiée présente une mobilité nettement réduite.
#➡️ Le VO₂ max estimé est significativement inférieur à celui de l’article.
#   Cela reflète une capacité cardiovasculaire légèrement plus faible, mais l’écart reste moins important que pour la souplesse.

#➡️ Les trois tests montrent que les moyennes de notre échantillon diffèrent de manière significative des valeurs de référence issues de la littérature.
#La population étudiée semble :
#plus lourde (BMI plus élevé),
#moins flexible (écart très marqué),
#moins endurante (VO₂ plus faible).
#Ces résultats suggèrent un profil global de condition physique moins favorable par rapport à la population théorique utilisée comme référence.