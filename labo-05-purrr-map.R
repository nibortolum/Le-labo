# labo-05-purrr-map.R
# Série : R Méconnu #5 — `purrr::map()` : la boucle for en mieux
# Le Labo — lebiostatisticien.fr
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(palmerpenguins)




# Le problème avec les boucles for ----

# Ajuster un modèle par espèce avec une boucle for :
# verbeux, indices à gérer, nommage après coup, et une nouvelle boucle pour chaque extraction
especes <- unique(penguins$species)
resultats <- list()

for (i in seq_along(especes)) {
  donnees_sp <- penguins |> filter(species == especes[i])
  resultats[[i]] <- lm(body_mass_g ~ bill_length_mm, data = donnees_sp)
}
names(resultats) <- especes




# map() — le concept ----

# map(.x, .f) : applique .f à chaque élément de .x, retourne une liste
# Exemple simple avant d'attaquer les modèles
nombres <- list(1:10, 11:20, 21:30)
map(nombres, mean)




# Le modèle par espèce, version map() ----

# split(~species) coupe le dataframe en liste de 3 dataframes nommés
# map(\(df) ...) applique lm() à chaque sous-groupe
# Résultat : une liste nommée avec 3 modèles — en 3 lignes au lieu de 7
modeles <- penguins |>
  drop_na(body_mass_g, bill_length_mm) |>
  split(~species) |>
  map(\(df) lm(body_mass_g ~ bill_length_mm, data = df))




# Extraire les résultats ----

# Les R² de chaque modèle (version base)
map(modeles, \(m) summary(m)$r.squared)

# Mieux : avec broom, tout dans un seul dataframe
# map_dfr() combine en row-bind, .id = "species" ajoute le nom de chaque élément
library(broom)
map_dfr(modeles, glance, .id = "species")




# Les variantes de map() — contrôler le type de sortie ----

# map()     → liste (type universel)
# map_dbl() → vecteur numeric
# map_chr() → vecteur character
# map_lgl() → vecteur logical
# map_dfr() → dataframe (row-bind)

# Extraire le R² comme vecteur numérique nommé
map_dbl(modeles, \(m) summary(m)$r.squared)
# Adelie   Chinstrap  Gentoo
# 0.153    0.427      0.414




# map2() — deux inputs en parallèle ----

# map2(.x, .y, .f) itère sur deux listes simultanément
# Ici : enregistrer un graphique de diagnostic pour chaque modèle
fichiers <- c("adelie.pdf", "chinstrap.pdf", "gentoo.pdf")

map2(modeles, fichiers, \(mod, f) {
  p <- augment(mod) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point()
  ggsave(f, p, width = 12, height = 8, units = "cm")
})




# imap() — itérer avec le nom de l'élément ----

# imap() passe l'élément ET son nom à la fonction
# Pratique pour le logging ou les titres de graphiques
imap(modeles, \(mod, nom) {
  cat("Espèce:", nom, "- R² =", round(summary(mod)$r.squared, 3), "\n")
})




# Le workflow complet en un seul pipeline ----

# split → modèle → extraction des coefficients → tableau tidy
# Tout en une chaîne lisible, sans variable intermédiaire
resultats <- penguins |>
  drop_na(body_mass_g, bill_length_mm) |>
  split(~species) |>
  map(\(df) lm(body_mass_g ~ bill_length_mm, data = df)) |>
  map_dfr(\(m) tidy(m, conf.int = TRUE), .id = "species")

resultats

# Forest plot : effet du bec sur la masse selon l'espèce
resultats |>
  filter(term == "bill_length_mm") |>
  ggplot(aes(x = estimate, y = species, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Effet du bec sur la masse (g/mm)", y = NULL)




# Alternative : nest() + map() dans un tibble ----

# Pour rester dans un seul objet tibble tout au long du workflow
# nest() crée une colonne data avec un dataframe par espèce
# unnest() déplie les résultats à la fin
penguins |>
  drop_na(body_mass_g, bill_length_mm) |>
  nest(.by = species) |>
  mutate(
    modele = map(data, \(df) lm(body_mass_g ~ bill_length_mm, data = df)),
    resultats = map(modele, \(m) tidy(m, conf.int = TRUE))
  ) |>
  unnest(resultats)
