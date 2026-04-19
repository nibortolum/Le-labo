# labo-08-pivot-longer.R
# Série : R Méconnu #8
# tidyr::pivot_longer() : passer du format large au format long
# ============================================================
# Ce script contient tout le code présenté dans la vidéo.
# Chaque section correspond à une partie de la vidéo.
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(palmerpenguins)


# Partie 1 : Format large vs format long ----
# Le format large : une ligne par site, une colonne par espèce.
# C'est lisible sur le terrain, mais ggplot2 et dplyr n'en veulent pas.
# Le tidyverse attend le format long : une ligne par observation (site × espèce).

# Format large — typique d'un relevé de terrain
abondances_large <- tibble(
  site = c("Forêt A", "Prairie B", "Marais C", "Forêt D"),
  mesange = c(12, 3, 0, 8),
  pinson = c(5, 15, 2, 7),
  merle = c(8, 6, 4, 10),
  rouge_gorge = c(3, 2, 1, 5)
)

abondances_large

# Comment faire un barplot par espèce ? Chaque espèce est une colonne...
# On ne peut pas mapper "espèce" sur l'axe X en format large.

# Le format long cible :
# site       | espece       | abondance
# Forêt A    | mesange      | 12
# Forêt A    | pinson       | 5
# Forêt A    | merle        | 8
# ...
# Une ligne par observation (site x espèce), trois colonnes.


# Partie 2a : pivot_longer() — la transformation de base ----
# Trois arguments essentiels :
#   cols       — quelles colonnes pivoter
#   names_to   — nom de la colonne qui recevra les anciens noms de colonnes
#   values_to  — nom de la colonne qui recevra les valeurs
# Résultat : 4 lignes × 5 colonnes → 16 lignes × 3 colonnes.

abondances_long <- abondances_large |>
  pivot_longer(
    cols = mesange:rouge_gorge,
    names_to = "espece",
    values_to = "abondance"
  )

abondances_long


# Partie 2b : Sélection des colonnes avec tidyselect ----
# cols = -site : toutes les colonnes sauf site (le plus pratique).
# Toutes les sélections tidyselect fonctionnent.

# Par exclusion (plus pratique quand il y a beaucoup d'espèces)
abondances_large |>
  pivot_longer(cols = -site, names_to = "espece", values_to = "abondance")

# Par combinaison de sélecteurs
abondances_large |>
  pivot_longer(cols = everything() & !site, names_to = "espece", values_to = "abondance")


# Partie 2c : Le graphique rendu possible par le format long ----
# Impossible à faire directement en format large.
# En format long, 4 lignes de ggplot2 suffisent.

abondances_long |>
  ggplot(aes(x = fct_reorder(espece, abondance, .fun = sum, .desc = TRUE),
             y = abondance, fill = site)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Abondance", fill = "Site")


# Partie 3a : pivot_wider() — le chemin inverse ----
# Repasser en format large : pour un tableau dans un article,
# ou pour certaines fonctions qui attendent une matrice.
# Arguments inversés : names_from et values_from.

abondances_long |>
  pivot_wider(
    names_from = espece,
    values_from = abondance
  )


# Partie 3b : pivot_wider() — tableau croisé avec valeurs manquantes ----
# values_fill = 0 remplace les combinaisons absentes par 0 (pas NA).

# Comptage espèce × île en format long
comptage <- penguins |> count(species, island)

# Passage en format large pour un tableau
comptage |>
  pivot_wider(names_from = island, values_from = n, values_fill = 0)


# Partie 4 : Cas avancé — plusieurs colonnes de valeurs (.value) ----
# Mesures répétées dans le temps, stockées en colonnes nommées variable_temps.
# On veut une colonne "temps", une colonne "taille", une colonne "poids".

mesures <- tibble(
  individu = c("A", "B", "C"),
  taille_t0 = c(10.2, 11.5, 9.8),
  taille_t1 = c(12.1, 13.0, 11.5),
  taille_t2 = c(14.5, 14.8, 13.2),
  poids_t0 = c(25, 28, 22),
  poids_t1 = c(30, 33, 27),
  poids_t2 = c(35, 38, 32)
)

mesures

# Le ".value" dans names_to est spécial :
# - la partie AVANT le séparateur (taille, poids) → noms de colonnes
# - la partie APRÈS le séparateur (t0, t1, t2) → colonne "temps"
# Résultat : individu | temps | taille | poids

mesures |>
  pivot_longer(
    cols = -individu,
    names_to = c(".value", "temps"),
    names_sep = "_"
  )

# C'est le format qu'attendent lme4::lmer() et les autres
# fonctions d'analyse longitudinale.
