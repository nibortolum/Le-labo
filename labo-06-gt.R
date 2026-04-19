# labo-06-gt.R
# Série : R Méconnu #6
# gt::gt() : des tableaux publication-ready en R

# ============================================================
# Ce script contient tout le code présenté dans la vidéo.
# Chaque section correspond à une partie de la vidéo.
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(palmerpenguins)
library(gt)
library(broom)  # utilisé en Partie 4


# Partie 1 : De tibble à tableau ----
# On commence par calculer des stats descriptives classiques.
# C'est un tibble lisible dans la console, mais pas présentable.

# Stats descriptives classiques
stats <- penguins |>
  drop_na() |>
  summarise(
    .by = species,
    n = n(),
    masse_moy = mean(body_mass_g),
    masse_et = sd(body_mass_g),
    bec_moy = mean(bill_length_mm),
    bec_et = sd(bill_length_mm)
  )

stats

# gt() transforme le tibble en un objet tableau formaté
stats |> gt()


# Partie 2a : Arrondir et renommer ----
# fmt_number() contrôle les décimales.
# cols_label() renomme les en-têtes.

stats |>
  gt() |>
  fmt_number(columns = c(masse_moy, masse_et), decimals = 0) |>
  fmt_number(columns = c(bec_moy, bec_et), decimals = 1) |>
  cols_label(
    species = "Espèce",
    n = "N",
    masse_moy = "Masse moyenne",
    masse_et = "ET",
    bec_moy = "Longueur moyenne du bec",
    bec_et = "ET"
  )


# Partie 2b : Grouper les colonnes avec des spanners ----
# Les spanners ajoutent un en-tête de groupe au-dessus de plusieurs colonnes.
# Exactement comme dans les tableaux d'articles scientifiques.

stats |>
  gt() |>
  fmt_number(columns = c(masse_moy, masse_et), decimals = 0) |>
  fmt_number(columns = c(bec_moy, bec_et), decimals = 1) |>
  cols_label(
    species = "Espèce",
    n = "N",
    masse_moy = "Moyenne",
    masse_et = "ET",
    bec_moy = "Moyenne",
    bec_et = "ET"
  ) |>
  tab_spanner(label = "Masse corporelle (g)", columns = c(masse_moy, masse_et)) |>
  tab_spanner(label = "Longueur du bec (mm)", columns = c(bec_moy, bec_et))


# Partie 2c : Titre et notes ----
# tab_header() ajoute un titre et un sous-titre.
# tab_footnote() et tab_source_note() gèrent les notes de bas de tableau.

stats |>
  gt() |>
  fmt_number(columns = c(masse_moy, masse_et), decimals = 0) |>
  fmt_number(columns = c(bec_moy, bec_et), decimals = 1) |>
  cols_label(
    species = "Espèce", n = "N",
    masse_moy = "Moyenne", masse_et = "ET",
    bec_moy = "Moyenne", bec_et = "ET"
  ) |>
  tab_spanner(label = "Masse corporelle (g)", columns = c(masse_moy, masse_et)) |>
  tab_spanner(label = "Longueur du bec (mm)", columns = c(bec_moy, bec_et)) |>
  tab_header(
    title = "Tableau 1. Morphométrie des manchots de l'archipel Palmer",
    subtitle = "Données Palmer Station LTER, 2007-2009"
  ) |>
  tab_footnote("ET = écart-type") |>
  tab_source_note("Source : palmerpenguins (Horst et al., 2020)")


# Partie 3a : Mise en forme conditionnelle ----
# tab_style() applique un style à des cellules conditionnellement.
# Ici : mettre en gras les n > 100.

stats |>
  gt() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = n, rows = n > 100)
  )


# Partie 3b : Thèmes prédéfinis ----
# opt_stylize() applique un des 6 thèmes prédéfinis de gt.
# tab_options() permet un contrôle granulaire du style.

# Sobre, adapté aux articles
stats |>
  gt() |>
  opt_stylize(style = 3)  # Essaie 1 à 6

# Ou le style minimal (bordures horizontales uniquement)
stats |>
  gt() |>
  tab_options(
    table.border.top.style = "hidden",
    column_labels.border.bottom.width = px(2),
    table_body.border.bottom.width = px(5)
  )


# Partie 3c : Exporter le tableau ----
# gtsave() exporte dans le format déduit de l'extension.
# .docx produit un tableau Word formaté sans passer par Excel.

tableau <- stats |> gt()  # Ton tableau formaté

# En HTML (pour Quarto, rapports web)
gtsave(tableau, "tableau1.html")

# En PNG (pour présentations, Word)
gtsave(tableau, "tableau1.png")

# En LaTeX (pour les articles)
gtsave(tableau, "tableau1.tex")

# En Word (directement)
gtsave(tableau, "tableau1.docx")


# Partie 4 : Tableau de résultats de modèle ----
# broom::tidy() extrait les coefficients d'un modèle sous forme de tibble.
# Combiné à gt(), on obtient un tableau publication-ready en une pipeline.

# Deux modèles à comparer
mod1 <- lm(body_mass_g ~ bill_length_mm + species, data = penguins)
mod2 <- lm(body_mass_g ~ bill_length_mm * species, data = penguins)

bind_rows(
  tidy(mod1, conf.int = TRUE) |> mutate(modele = "Additif"),
  tidy(mod2, conf.int = TRUE) |> mutate(modele = "Interaction")
) |>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
  select(modele, term, estimate, conf.low, conf.high, p.value) |>
  gt(groupname_col = "modele") |>
  fmt_number(columns = c(estimate, conf.low, conf.high), decimals = 1) |>
  fmt_scientific(columns = p.value, decimals = 1) |>
  cols_label(
    term = "Terme",
    estimate = "β",
    conf.low = "IC 2.5%",
    conf.high = "IC 97.5%",
    p.value = "p"
  ) |>
  tab_header(title = "Tableau 2. Coefficients des modèles linéaires")
