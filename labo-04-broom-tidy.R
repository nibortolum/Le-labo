# labo-04-broom-tidy.R
# Série : R Méconnu #4 — `broom::tidy()` : extraire les résultats de n'importe quel modèle
# Le Labo — lebiostatisticien.fr
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(palmerpenguins)




# Le problème avec summary() ----

# Un modèle linéaire simple : masse corporelle ~ bec + espèce
mod <- lm(body_mass_g ~ bill_length_mm + species, data = penguins)
summary(mod)

# summary() est lisible à l'écran, mais impossible à manipuler programmatiquement
# summary(mod) renvoie une liste imbriquée — un objet des années 90
# Pour extraire la p-value de bill_length_mm, il faut faire ça :
summary(mod)$coefficients["bill_length_mm", "Pr(>|t|)"]
# Fragile, illisible, et ça casse si tu changes de modèle




# tidy() — les coefficients en tibble ----

# install.packages("broom")
library(broom)

# Un tibble avec une ligne par terme : term, estimate, std.error, statistic, p.value
# Tidy, filtrable, joinable
tidy(mod)

# Avec les intervalles de confiance : conf.low et conf.high — prêt pour un forest plot
tidy(mod, conf.int = TRUE, conf.level = 0.95)




# glance() — les métriques globales du modèle ----

# Une seule ligne : R², R² ajusté, AIC, BIC, sigma, df, p-value globale
# Tout ce que tu vois en bas du summary(), dans un tibble
glance(mod)




# augment() — les données enrichies ----

# Le jeu de données original + .fitted, .resid, .hat, .cooksd
augment(mod)

# Directement exploitable pour les graphiques de diagnostic
augment(mod) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Valeurs prédites", y = "Résidus")




# Comparer plusieurs modèles ----

# Définir 5 spécifications de modèles
mod1 <- lm(body_mass_g ~ bill_length_mm, data = penguins)
mod2 <- lm(body_mass_g ~ bill_length_mm + species, data = penguins)
mod3 <- lm(body_mass_g ~ bill_length_mm * species, data = penguins)
mod4 <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm + species, data = penguins)
mod5 <- lm(body_mass_g ~ bill_length_mm * species + flipper_length_mm, data = penguins)




# Tableau comparatif des métriques ----

# Stocker les modèles dans une liste nommée
modeles <- list(
  "bill seul" = mod1,
  "bill + espèce" = mod2,
  "bill × espèce" = mod3,
  "bill + flipper + espèce" = mod4,
  "bill × espèce + flipper" = mod5
)

# Cinq modèles, un seul tableau — on voit immédiatement le meilleur AIC / R² ajusté
map_dfr(modeles, glance, .id = "modele") |>
  select(modele, r.squared, adj.r.squared, AIC, BIC)




# Forest plot des coefficients ----

# Visualiser la stabilité du coefficient de bill_length_mm selon la spécification
# En 10 lignes : graphique publication-ready impossible à faire avec summary()
map_dfr(modeles, \(m) tidy(m, conf.int = TRUE), .id = "modele") |>
  filter(term == "bill_length_mm") |>
  ggplot(aes(x = estimate, y = modele, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Effet de bill_length_mm (g)", y = NULL,
       title = "Stabilité du coefficient selon le modèle")




# broom fonctionne avec plus de 100 types de modèles ----

# GLM (régression logistique)
tidy(glm(species == "Adelie" ~ bill_length_mm, data = penguins, family = binomial))

# Test de Student
tidy(t.test(body_mass_g ~ sex, data = penguins |> drop_na(sex)))

# ANOVA
tidy(aov(body_mass_g ~ species, data = penguins))

# Test de corrélation
tidy(cor.test(penguins$bill_length_mm, penguins$body_mass_g, use = "complete.obs"))
