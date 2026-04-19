# labo-07-forcats.R
# Série : R Méconnu #7
# forcats::fct_reorder() : des graphiques ordonnés sans galère

# ============================================================
# Ce script contient tout le code présenté dans la vidéo.
# Chaque section correspond à une partie de la vidéo.
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(palmerpenguins)


# Partie 1 : Le problème — ordre alphabétique par défaut ----
# Par défaut, R ordonne les facteurs alphabétiquement.
# L'axe X est en ordre alphabétique : Adelie, Chinstrap, Gentoo.
# Ce n'est presque jamais l'ordre souhaité.

# Comptage par espèce
comptage <- penguins |>
  count(species) |>
  drop_na()

comptage

# Barplot naïf — ordre alphabétique sur l'axe X
ggplot(comptage, aes(x = species, y = n)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Effectif")


# Partie 2a : fct_reorder() — trier par valeur ----
# fct_reorder(species, n) réordonne les niveaux du facteur species
# selon les valeurs de n. Par défaut : croissant.

# Trier les espèces par effectif total
comptage_total <- penguins |> count(species)

ggplot(comptage_total, aes(x = fct_reorder(species, n), y = n)) +
  geom_col() +
  labs(x = NULL, y = "Effectif") 


# Partie 2b : fct_reorder() — ordre décroissant ----
# .desc = TRUE inverse le tri : la plus grande valeur en premier.

ggplot(comptage_total, aes(x = fct_reorder(species, n, .desc = TRUE), y = n)) +
  geom_col() +
  labs(x = NULL, y = "Effectif")


# Partie 2c : fct_reorder() — avec une fonction d'agrégation ----
# .fun = mean applique la moyenne de body_mass_g pour chaque espèce,
# puis ordonne les espèces selon ce résultat.
# On peut passer median, max, min, ou n'importe quelle fonction.

ggplot(penguins |> drop_na(),
       aes(x = fct_reorder(species, body_mass_g, .fun = mean), y = body_mass_g)) +
  geom_boxplot() +
  labs(x = NULL, y = "Masse corporelle (g)")


# Partie 3a : fct_infreq() — trier par fréquence ----
# [3:00 – 4:45]
# fct_infreq() ordonne selon le nombre d'occurrences dans les données.
# Pas besoin de calculer le comptage soi-même.

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar() +
  labs(x = NULL, y = "Effectif")


# Partie 3b : fct_rev() — inverser l'ordre ----
# Utile avec coord_flip() pour que le plus fréquent soit en haut.
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar() +
  coord_flip() +
  labs(x = NULL, y = "Effectif")

ggplot(penguins, aes(x = fct_rev(fct_infreq(species)))) +
  geom_bar() +
  coord_flip() +
  labs(x = NULL, y = "Effectif")


# Partie 3c : fct_lump_n() — regrouper les petites catégories ----
# Garder les n catégories les plus fréquentes, regrouper le reste dans "Other".
# other_level = "Autre" permet de changer le label du groupe résiduel.

penguins |>
  mutate(espece_top = fct_lump_n(species, n = 2)) |>
  count(espece_top)


# Partie 3d : fct_relevel() — ordre manuel ----
# Utile quand on veut contrôler la catégorie de référence dans une régression.
# Le premier niveau du facteur est la référence dans lm(), glm(), etc.

penguins |>
  mutate(species = fct_relevel(species, "Gentoo"))
