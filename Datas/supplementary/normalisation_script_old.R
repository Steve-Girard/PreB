# Script R : appliquer mappages de normalisation orthographique et produire un
# log # Usage : # 1) placer ce script dans le même dossier que votre CSV
# d'entrée (ex: FLH_annotations_v1.csv) # 2) modifier `input_csv` et
# `output_prefix` si nécessaire # 3) exécuter dans R ou RStudio # # Sorties : #
# - _normalized.csv : CSV normalisé (forme_canonique modifiée) # -
# _changes_log.csv : log détaillé des modifications # - _summary.txt : résumé
# des opérations # # Dépendances : tidyverse # Installer si nécessaire :
# install.packages("tidyverse") library(tidyverse) # ---------- Paramètres
# utilisateur ---------- input_csv <- "FLH_annotations_v1.csv" # fichier
# d'entrée (doit contenir les champs du schéma) output_prefix <-
# "FLH_annotations_v1" # préfixe pour fichiers de sortie apply_changes <- TRUE #
# TRUE = appliquer et sauvegarder; FALSE = dry-run (log seulement) #
# ------------------------------------------------ # ---------- Table de
# mappages (exemples fournis) ---------- # Vous pouvez remplacer cette table par
# la lecture d'un CSV de mappages si vous préférez. mappings <- tribble(
# ~archaic, ~normalized, ~rule, "ç", "z", "remplacer ç par z", "ss", "s",
# "réduire double s non phonologique", "ph", "f", "remplacer ph par f", "th",
# "t", "remplacer th par t", "ch", "tx", "ch -> tx pour affricate palatale",
# "y", "j", "y -> j pour semi-voyelle /j/", "v", "b", "v -> b selon réalisme
# phonologique", "ae", "e", "ae -> e simplification diphtongue", "í", "i",
# "supprimer diacritiques non phonologiques", "ì", "i", "supprimer diacritiques
# non phonologiques", "î", "i", "supprimer diacritiques non phonologiques", "ó",
# "o", "supprimer diacritiques non phonologiques", "ò", "o", "supprimer
# diacritiques non phonologiques", "œ", "e", "simplifier ligature œ -> e", "’",
# "", "retirer apostrophe non phonologique", " ", "", "retirer espaces internes
# irréguliers (après vérification)" ) # Note : l'ordre des mappages peut être
# important. Les remplacements plus spécifiques doivent venir avant les plus
# généraux. # Si vous avez un fichier CSV de mappages, remplacez la table
# ci‑dessous par : # mappings <- read_csv("mappings.csv", col_types =
# cols(archaic = col_character(), normalized = col_character(), rule =
# col_character())) # ---------- Fonctions utilitaires ---------- safe_read_csv
# <- function(path) { if (!file.exists(path)) stop("Fichier d'entrée introuvable
# : ", path) read_csv(path, col_types = cols(.default = col_character())) } #
# Appliquer mappages sur une chaîne en respectant l'ordre
# apply_mappings_to_string <- function(s, mappings) { original <- s if (is.na(s)
# || s == "") return(s) # Appliquer chaque mappage séquentiellement for (i in
# seq_len(nrow(mappings))) { a <- mappings$archaic[i] n <-
# mappings$normalized[i] # échappement pour regex si nécessaire a_esc <-
# stringr::str_replace_all(a, "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\\\1") s <-
# stringr::str_replace_all(s, a_esc, n) } return(s) } # Enregistrer log d'une
# modification record_change <- function(log_df, id, field, before, after, rule)
# { tibble( id = id, field = field, before = before, after = after, rule_applied
# = rule, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S") ) %>%
# bind_rows(log_df) } # ---------- Lecture du CSV d'entrée ---------- df <-
# safe_read_csv(input_csv) # Vérifier présence des champs essentiels
# required_fields <- c("id", "orthographe_originale", "forme_canonique",
# "annotateur", "confidence") missing_fields <- setdiff(required_fields,
# names(df)) if (length(missing_fields) > 0) stop("Champs obligatoires manquants
# dans le CSV d'entrée : ", paste(missing_fields, collapse = ", ")) # Préparer
# changes_log <- tibble(id = character(), field = character(), before =
# character(), after = character(), rule_applied = character(), timestamp =
# character()) # ---------- Application des mappages ---------- # Nous
# appliquons les mappages sur le champ forme_canonique. # Optionnel : appliquer
# aussi sur orthographe_originale dans une colonne séparée si vous le souhaitez.
# df_norm <- df %>% mutate(.row = row_number()) for (i in
# seq_len(nrow(df_norm))) { row_id <- df_norm$id[i] before_form <-
# df_norm$forme_canonique[i] if (is.na(before_form)) before_form <- ""
# after_form <- apply_mappings_to_string(before_form, mappings) # Nettoyage
# supplémentaire : normaliser espaces multiples et trim after_form <-
# stringr::str_squish(after_form) # Si changement, enregistrer dans log if
# (!identical(before_form, after_form)) { # Trouver règles appliquées (liste des
# mappages qui ont modifié la chaîne) applied_rules <- c() tmp <- before_form
# for (j in seq_len(nrow(mappings))) { a <- mappings$archaic[j] n <-
# mappings$normalized[j] a_esc <- stringr::str_replace_all(a,
# "([\\^$.|?*+()\\[\\]{}\\\\])", "\\\\\\1") if (stringr::str_detect(tmp, a_esc))
# { applied_rules <- c(applied_rules, paste0(a, "→", n, " (", mappings$rule[j],
# ")")) tmp <- stringr::str_replace_all(tmp, a_esc, n) } } rule_text <-
# paste(applied_rules, collapse = " ; ") changes_log <-
# record_change(changes_log, row_id, "forme_canonique", before_form, after_form,
# rule_text) df_norm$forme_canonique[i] <- after_form } } # ---------- Rapport
# de modifications et validations ---------- n_changes <- nrow(changes_log)
# n_rows <- nrow(df_norm) summary_lines <- c( paste0("Input file: ", input_csv),
# paste0("Rows processed: ", n_rows), paste0("Total modifications: ",
# n_changes), paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")) ) #
# Détails par règle (comptage) if (n_changes > 0) { rule_counts <- changes_log
# %>% separate_rows(rule_applied, sep = " ; ") %>% count(rule_applied, sort =
# TRUE) } else { rule_counts <- tibble(rule_applied = character(), n =
# integer()) } # ---------- Écriture des fichiers de sortie ---------- if
# (apply_changes) { normalized_path <- paste0(output_prefix, "_normalized.csv")
# write_csv(df_norm %>% select(-.row), normalized_path) log_path <-
# paste0(output_prefix, "_changes_log.csv") write_csv(changes_log, log_path)
# summary_path <- paste0(output_prefix, "_summary.txt")
# write_lines(c(summary_lines, "", "Rule counts:",
# capture.output(print(rule_counts))), summary_path) message("Normalisation
# appliquée. Fichiers écrits :") message(" - Normalized CSV: ", normalized_path)
# message(" - Changes log: ", log_path) message(" - Summary: ", summary_path) }
# else { # Dry-run : n'écrit que le log et le résumé log_path <-
# paste0(output_prefix, "_changes_log_dryrun.csv") write_csv(changes_log,
# log_path) summary_path <- paste0(output_prefix, "_summary_dryrun.txt")
# write_lines(c(summary_lines, "", "Rule counts:",
# capture.output(print(rule_counts))), summary_path) message("Dry-run terminé.
# Aucun changement appliqué. Fichiers écrits :") message(" - Changes log
# (dry-run): ", log_path) message(" - Summary (dry-run): ", summary_path) } #
# ---------- Contrôles post-traitement ---------- # Vérifier unicité des id dups
# <- df_norm %>% count(id) %>% filter(n > 1) if (nrow(dups) > 0) {
# warning("Doublons d'id détectés après traitement : ", paste(dups$id, collapse
# = ", ")) } # Fin du script