# labo-03-janitor.R
# Série : R Méconnu #3 — `janitor::clean_names()` : nettoyer tes noms de colonnes en 1 sec
# Le Labo — lebiostatisticien.fr
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(readxl)




# Simulation d'un Excel de terrain typique ----

# Colonnes avec espaces, accents, parenthèses, slashs, tirets — R déteste ça
terrain <- tibble(
  `Espèce (nom latin)` = c("Parus major", "Cyanistes caeruleus"),
  `Poids (g) - Balance A` = c(18.2, 11.5),
  `Date / Heure` = c("2026-03-01 08:30", "2026-03-01 09:15"),
  `N° échantillon` = c(1, 2),
  `Commentaires terrain (optionnel)` = c("RAS", "plumage abîmé")
)

# Afficher les noms de colonnes — le problème est visible
names(terrain)




# Le problème : backticks obligatoires partout ----

# Pour accéder à une colonne avec un nom sale, on doit mettre des backticks
terrain$`Poids (g) - Balance A`

# Pareil dans le tidyverse — pénible et source d'erreurs
terrain |> select(`Poids (g) - Balance A`)




# clean_names() — la solution ----

# install.packages("janitor")
library(janitor)

# Un seul appel : tout est converti en snake_case propre
terrain |> clean_names()

# Ce que clean_names() fait :
# - Convertit en minuscules
# - Espaces, tirets, slashs → underscores
# - Supprime les accents
# - Supprime les parenthèses et caractères spéciaux
# - Garantit des noms uniques (numérote si collision)
terrain |> clean_names() |> names()




# Le réflexe à prendre : clean_names() juste après l'import ----

# Pattern standard avec Excel
donnees <- read_excel("mon_fichier.xlsx") |>
  clean_names()

# Ou avec un CSV
donnees <- read_csv("mon_fichier.csv") |>
  clean_names()




# tabyl() — le table() en mieux ----

library(palmerpenguins)

# table() classique — sortie en matrice, pas tidy, pas de pourcentages
table(penguins$species, penguins$sex)

# tabyl() : tableau croisé propre, avec pourcentages ET effectifs en 3 lignes
penguins |>
  tabyl(species, sex) |>
  adorn_percentages("row") |>
  adorn_pct_formatting() |>
  adorn_ns()




# remove_empty() — virer les lignes et colonnes vides ----

# Fréquent dans les Excel : des lignes et colonnes entièrement vides
donnees_sales <- tibble(
  a = c(1, NA, 3),
  b = c(NA, NA, NA),  # Colonne entièrement vide
  c = c(4, NA, 6)
)

# La colonne b disparaît, les lignes entièrement NA aussi
donnees_sales |>
  remove_empty(c("rows", "cols"))




# get_dupes() — trouver les doublons ----

# Trouver les lignes dupliquées sur une combinaison de colonnes
# Indispensable pour le contrôle qualité des données de terrain
penguins |>
  get_dupes(species, island, bill_length_mm)
