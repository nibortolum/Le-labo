# labo-02-case-when.R
# Série : R Méconnu #2 — `case_when()` : en finir avec les ifelse() imbriqués
# Le Labo — lebiostatisticien.fr
# ============================================================


# Chargement des packages ----

library(tidyverse)
library(palmerpenguins)




# Le cauchemar des ifelse() imbriqués ----

# Classer les manchots par taille de masse corporelle
# Ça marche, mais essaie de lire ça avec 7 catégories : cauchemar
penguins |>
  mutate(
    taille = ifelse(body_mass_g < 3500, "petit",
              ifelse(body_mass_g < 4500, "moyen",
                ifelse(body_mass_g < 5500, "grand", "très grand")))
  )




# case_when() — syntaxe de base ----

# Chaque ligne : une condition ~ un résultat
# Évalué dans l'ordre — la première condition vraie gagne
# .default attrape tout le reste (y compris les NA si pas gérés)
penguins |>
  mutate(
    taille = case_when(
      body_mass_g < 3500 ~ "petit",
      body_mass_g < 4500 ~ "moyen",
      body_mass_g < 5500 ~ "grand",
      .default = "très grand"
    )
  )




# Gérer les NA explicitement ----

# Mettre is.na() EN PREMIER — sinon les NA tombent dans .default et deviennent "très grand"
# NA_character_ et pas juste NA : le type doit correspondre aux autres branches (character)
penguins |>
  mutate(
    taille = case_when(
      is.na(body_mass_g) ~ NA_character_,
      body_mass_g < 3500 ~ "petit",
      body_mass_g < 4500 ~ "moyen",
      body_mass_g < 5500 ~ "grand",
      .default = "très grand"
    )
  )




# Recoder des stades de développement ----

# Cas typique en entomologie : mesures continues → stades discrets
insectes <- tibble(
  id = 1:100,
  longueur_mm = runif(100, 1, 25)
)

insectes |>
  mutate(
    stade = case_when(
      longueur_mm < 3  ~ "larve L1",
      longueur_mm < 7  ~ "larve L2",
      longueur_mm < 12 ~ "larve L3",
      longueur_mm < 18 ~ "nymphe",
      .default = "adulte"
    )
  )




# Conditions multiples — combiner plusieurs variables ----

# On peut combiner plusieurs colonnes dans une condition avec & et |
# L'ordre compte : "Gentoo costaud" doit être testé AVANT "Gentoo standard"
penguins |>
  mutate(
    profil = case_when(
      species == "Gentoo" & body_mass_g > 5000 ~ "Gentoo costaud",
      species == "Gentoo" ~ "Gentoo standard",
      bill_length_mm > 50 ~ "Bec long (non-Gentoo)",
      .default = "Autre"
    )
  )




# Recoder des facteurs — version case_when() ----

# Recodage simple des noms d'espèces en français
penguins |>
  mutate(
    espece_fr = case_when(
      species == "Adelie"    ~ "Manchot Adélie",
      species == "Chinstrap" ~ "Manchot à jugulaire",
      species == "Gentoo"    ~ "Manchot papou"
    )
  )




# case_match() — recodage un-pour-un (dplyr 1.1+) ----

# case_match() est case_when() pour les cas d'égalité pure
# Pas de <, pas de >, juste == implicite — plus concis pour du recodage simple
penguins |>
  mutate(
    espece_fr = case_match(
      species,
      "Adelie"    ~ "Manchot Adélie",
      "Chinstrap" ~ "Manchot à jugulaire",
      "Gentoo"    ~ "Manchot papou"
    )
  )
