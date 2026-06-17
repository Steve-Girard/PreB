# Script R corrigé : appliquer mappages de normalisation orthographique et produire un log
# Usage :
# 1) placer ce script dans le même dossier que votre CSV d'entrée
# 2) modifier `input_csv` et `output_prefix` si nécessaire
# 3) exécuter en dry-run (apply_changes <- FALSE) puis, si OK, activer apply_changes <- TRUE
#
# Sorties :
# - _normalized.csv : CSV normalisé (forme_canonique modifiée)
# - _changes_log.csv : log détaillé des modifications
# - _summary.txt : résumé des opérations

# ---------- Chargement robuste des dépendances ----------
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("stringi", quietly = TRUE)) install.packages("stringi")

library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(stringi)

# ---------- Paramètres utilisateur ----------
input_csv <- "FLH_annotations_v1.csv"   # fichier d'entrée
output_prefix <- "FLH_annotations_v1"   # préfixe pour fichiers de sortie

# Par défaut en test : dry-run (ne pas écraser les fichiers)
apply_changes <- TRUE

# ---------- Table de mappages (corrigée) ----------
# Remarques :
# - pas d'apostrophe typographique ; pas de mapping " " -> "" dangereux
# - placer les remplacements spécifiques avant les généraux
mappings <- tribble(
  ~archaic, ~normalized, ~rule,
  "ç", "z", "remplacer ç par z",
  "ss", "s", "réduire double s non phonologique",
  "ph", "f", "remplacer ph par f",
  "th", "t", "remplacer th par t",
  "ch", "tx", "ch -> tx pour affricate palatale",
  "y", "j", "y -> j pour semi-voyelle /j/",
  "v", "b", "v -> b selon réalisme phonologique",
  "ae", "e", "ae -> e simplification diphtongue",
  "í", "i", "supprimer diacritiques non phonologiques",
  "ì", "i", "supprimer diacritiques non phonologiques",
  "î", "i", "supprimer diacritiques non phonologiques",
  "ó", "o", "supprimer diacritiques non phonologiques",
  "ò", "o", "supprimer diacritiques non phonologiques",
  "œ", "e", "simplifier ligature oe -> e",
  "'", "", "retirer apostrophe non phonologique"
)

# ---------- Fonctions utilitaires ----------
safe_read_csv <- function(path) {
  if (!file.exists(path)) stop("Fichier d'entrée introuvable : ", path)
  readr::read_csv(path, col_types = cols(.default = col_character()))
}

# Appliquer mappages sur une chaîne en respectant l'ordre
apply_mappings_to_string <- function(s, mappings) {
  if (is.na(s)) return(NA_character_)
  if (s == "") return(s)
  # Appliquer chaque mappage séquentiellement (échappement regex)
  for (i in seq_len(nrow(mappings))) {
    a <- mappings$archaic[i]
    n <- mappings$normalized[i]
    a_esc <- stringr::str_replace_all(a, "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\\\1")
    s <- stringr::str_replace_all(s, a_esc, n)
  }
  return(s)
}

# Enregistrer log d'une modification (ajoute en fin)
record_change <- function(log_df, id, field, before, after, rule) {
  new <- tibble(
    id = as.character(id),
    field = as.character(field),
    before = as.character(before),
    after = as.character(after),
    rule_applied = as.character(rule),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  bind_rows(log_df, new)
}

# Écriture protégée
safe_write_csv <- function(df, path) {
  tryCatch({
    readr::write_csv(df, path)
    TRUE
  }, error = function(e) {
    message("Échec écriture ", path, " : ", e$message)
    FALSE
  })
}

safe_write_lines <- function(lines, path) {
  tryCatch({
    readr::write_lines(lines, path)
    TRUE
  }, error = function(e) {
    message("Échec écriture ", path, " : ", e$message)
    FALSE
  })
}

# ---------- Lecture du CSV d'entrée ----------
df <- safe_read_csv(input_csv)

# Vérifier présence des champs essentiels
required_fields <- c("id", "orthographe_originale", "forme_canonique", "annotateur", "confidence")
missing_fields <- setdiff(required_fields, names(df))
if (length(missing_fields) > 0) stop("Champs obligatoires manquants dans le CSV d'entrée : ", paste(missing_fields, collapse = ", "))

# Préparer log
changes_log <- tibble(id = character(), field = character(), before = character(), after = character(), rule_applied = character(), timestamp = character())

# ---------- Application des mappages ----------
df_norm <- df %>% mutate(.row = row_number())
df_norm$forme_canonique <- ifelse(is.na(df_norm$forme_canonique) | df_norm$forme_canonique == "", df_norm$orthographe_originale, df_norm$forme_canonique)

for (i in seq_len(nrow(df_norm))) {
  row_id <- df_norm$id[i]
  before_form <- df_norm$forme_canonique[i]

  # Normaliser diacritiques en ASCII (prétraitement sûr)
  if (!is.na(before_form) && before_form != "") {
    before_norm <- stringi::stri_trans_general(before_form, "Latin-ASCII")
  } else {
    before_norm <- before_form
  }

  after_form <- apply_mappings_to_string(before_norm, mappings)
  # Nettoyage supplémentaire : normaliser espaces multiples et trim
  if (!is.na(after_form)) after_form <- stringr::str_squish(after_form)

  # Si changement (tenir compte de NA)
  changed <- !(identical(before_form, after_form) || (is.na(before_form) && is.na(after_form)))
  if (changed) {
    # Trouver règles appliquées (détecter sur la version avant-normalisation des diacritiques)
    applied_rules <- c()
    tmp <- before_norm
    for (j in seq_len(nrow(mappings))) {
      a <- mappings$archaic[j]
      n <- mappings$normalized[j]
      a_esc <- stringr::str_replace_all(a, "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\\\1")
      if (!is.na(tmp) && stringr::str_detect(tmp, a_esc)) {
        applied_rules <- c(applied_rules, paste0(a, "→", n, " (", mappings$rule[j], ")"))
        tmp <- stringr::str_replace_all(tmp, a_esc, n)
      }
    }
    rule_text <- if (length(applied_rules) > 0) paste(applied_rules, collapse = " ; ") else ""
    changes_log <- record_change(changes_log, row_id, "forme_canonique", before_form, after_form, rule_text)
    df_norm$forme_canonique[i] <- after_form
  }
}

# ---------- Rapport de modifications et validations ----------
n_changes <- nrow(changes_log)
n_rows <- nrow(df_norm)
summary_lines <- c(
  paste0("Input file: ", input_csv),
  paste0("Rows processed: ", n_rows),
  paste0("Total modifications: ", n_changes),
  paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
)

if (n_changes > 0) {
  rule_counts <- changes_log %>% separate_rows(rule_applied, sep = " ; ") %>% count(rule_applied, sort = TRUE)
} else {
  rule_counts <- tibble(rule_applied = character(), n = integer())
}

# ---------- Écriture des fichiers de sortie ----------
if (apply_changes) {
  normalized_path <- paste0(output_prefix, "_normalized.csv")
  log_path <- paste0(output_prefix, "_changes_log.csv")
  summary_path <- paste0(output_prefix, "_summary.txt")

  safe_write_csv(df_norm %>% select(-.row), normalized_path)
  safe_write_csv(changes_log, log_path)
  safe_write_lines(c(summary_lines, "", "Rule counts:", capture.output(print(rule_counts))), summary_path)

  message("Normalisation appliquée. Fichiers écrits :")
  message(" - Normalized CSV: ", normalized_path)
  message(" - Changes log: ", log_path)
  message(" - Summary: ", summary_path)
} else {
  # Dry-run : n'écrit que le log et le résumé (suffixe dryrun)
  log_path <- paste0(output_prefix, "_changes_log_dryrun.csv")
  summary_path <- paste0(output_prefix, "_summary_dryrun.txt")

  safe_write_csv(changes_log, log_path)
  safe_write_lines(c(summary_lines, "", "Rule counts:", capture.output(print(rule_counts))), summary_path)

  message("Dry-run terminé. Aucun changement appliqué. Fichiers écrits :")
  message(" - Changes log (dry-run): ", log_path)
  message(" - Summary (dry-run): ", summary_path)
}

# ---------- Contrôles post-traitement ----------
dups <- df_norm %>% count(id) %>% filter(n > 1)
if (nrow(dups) > 0) {
  warning("Doublons d'id détectés après traitement : ", paste(dups$id, collapse = ", "))
}
# Fin du script
