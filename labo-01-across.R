# labo-01-across.R
# Série : R Méconnu #1 — `across()` : appliquer une fonction à 50 colonnes en une ligne
# Le Labo — lebiostatisticien.fr
# ============================================================


# Chargement des packages ----

# On charge le tidyverse et palmerpenguins pour les données d'exemple
library(tidyverse)

# install.packages("palmerpenguins")
library(palmerpenguins)

# Aperçu du jeu de données : 4 variables numériques principales
glimpse(penguins)




# Le problème : l'approche naïve ----

# Calculer la moyenne de chaque variable numérique ligne par ligne
# Ça passe pour 4 colonnes, mais pas pour 50 ou 200
penguins |>
  summarise(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    mean_body_mass = mean(body_mass_g, na.rm = TRUE)
  )




# across() — syntaxe de base ----

# Une seule ligne pour toutes les colonnes numériques
# where(is.numeric) sélectionne automatiquement toutes les colonnes numériques
penguins |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))




# across() — syntaxe lambda moderne (R 4.1+ / dplyr 1.1+) ----

# La syntaxe avec arguments directs est dépréciée
# \(x) est le raccourci pour function(x) — chaque colonne est passée comme x
penguins |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))




# Sélectionner les colonnes autrement ----

# Par nom explicite
penguins |>
  summarise(across(c(bill_length_mm, bill_depth_mm), \(x) mean(x, na.rm = TRUE)))

# Par pattern : starts_with() fonctionne exactement comme dans select()
penguins |>
  summarise(across(starts_with("bill"), \(x) mean(x, na.rm = TRUE)))

# Par position (moins recommandé mais possible)
penguins |>
  summarise(across(3:6, \(x) mean(x, na.rm = TRUE)))




# Plusieurs fonctions d'un coup ----

# Passer une liste nommée pour calculer plusieurs statistiques
# Les colonnes résultantes sont nommées automatiquement : bill_length_mm_moy, etc.
penguins |>
  summarise(across(
    where(is.numeric),
    list(
      moy = \(x) mean(x, na.rm = TRUE),
      et = \(x) sd(x, na.rm = TRUE)
    )
  ))

# Contrôler le nommage avec .names : {.fn} = nom de la fonction, {.col} = nom de la colonne
# Résultat : moy_bill_length_mm, et_bill_length_mm, etc.
penguins |>
  summarise(across(
    where(is.numeric),
    list(
      moy = \(x) mean(x, na.rm = TRUE),
      et = \(x) sd(x, na.rm = TRUE)
    ),
    .names = "{.fn}_{.col}"
  ))




# across() dans mutate() ----

# Centrer-réduire toutes les variables numériques
penguins |>
  mutate(across(where(is.numeric), scale))

# Arrondir à 1 décimale
penguins |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

# Log-transformer les colonnes qui contiennent "mm"
penguins |>
  mutate(across(contains("mm"), log))




# across() dans filter() — if_all() et if_any() ----

# Dans filter(), on utilise if_all() et if_any() au lieu de across()
# Garder les lignes où AUCUNE variable numérique n'est NA
penguins |>
  filter(if_all(where(is.numeric), \(x) !is.na(x)))

# Garder les lignes où AU MOINS UNE variable dépasse 2 écarts-types
penguins |>
  filter(if_any(where(is.numeric), \(x) abs(x - mean(x, na.rm = TRUE)) > 2 * sd(x, na.rm = TRUE)))




# Par groupe avec group_by() ----

# Le vrai pouvoir : combiné avec group_by()
# 3 espèces × 4 variables → un tableau 3 lignes × 5 colonnes. En deux lignes.
penguins |>
  group_by(species) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# Avec .by (syntaxe moderne dplyr 1.1+) — le groupement ne persiste pas
penguins |>
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .by = species
  )
