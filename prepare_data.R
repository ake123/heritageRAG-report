# Copy the study results this website needs into data/.
#
# Run from this folder after every new study run, then re-render:
#
#   Rscript prepare_data.R <final-results-folder> <pilot-results-folder> [sami-sensitivity-folder]
#   quarto render
#
# Example (website folder next to the heritageRAG package):
#   Rscript prepare_data.R ../heritageRAG/results/final ../heritageRAG/results/batch-pilot \
#     ../heritageRAG/results/sami-sensitivity
#
# The third folder is optional: the output of scripts/run_sami_sensitivity.R,
# shown on the Extra tests page. The Annif-versus-catalogue files written by
# scripts/annif_vs_catalogue.R into the final folder are copied with it.
#
# Use this script rather than copying the results folders into data/ by
# hand: it extracts the AI answers, leaves out the files that would unblind
# the checkers, and puts everything where the pages look for it.
#
# What is copied: the batch tables, the paper figures, each question's tables,
# the AI answers, and the annotation *scores*. What is NOT copied: the
# annotators' sheets and the KEY files (they would reveal which search produced
# which item), and the large study.rds files.

args <- commandArgs(trailingOnly = TRUE)
final_dir <- if (length(args) >= 1) args[[1]] else "../heritageRAG/results/final"
pilot_dir <- if (length(args) >= 2) args[[2]] else "../heritageRAG/results/batch-pilot"
sami_dir <- if (length(args) >= 3) args[[3]] else "../heritageRAG/results/sami-sensitivity"

# A zip unpacked into a folder of the same name gives final/final/...; use
# the folder that actually holds the batch files.
find_batch_dir <- function(from, marker = "batch_metrics.csv") {
  if (!dir.exists(from) || file.exists(file.path(from, marker))) return(from)
  subs <- list.dirs(from, recursive = FALSE)
  subs <- subs[file.exists(file.path(subs, marker))]
  if (length(subs) == 1L) {
    message("Using ", subs, " (the batch files are one folder down)")
    return(subs)
  }
  from
}

copy_run <- function(from, to) {
  from <- find_batch_dir(from)
  if (!dir.exists(from)) {
    message("Skipping (not found): ", from)
    return(invisible(FALSE))
  }
  if (!file.exists(file.path(from, "batch_metrics.csv"))) {
    stop(from, " has no batch_metrics.csv. Give the folder written by run_study_batch.R ",
         "(it holds batch_metrics.csv and one folder per question).", call. = FALSE)
  }
  unlink(to, recursive = TRUE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  top <- list.files(from, pattern = "^(batch_.*|questions_used|run_info|failures|annif_models|annif_vs_catalogue_.*)\\.csv$|^sessionInfo\\.txt$",
                    full.names = TRUE)
  file.copy(top, to)
  if (dir.exists(file.path(from, "paper"))) {
    dir.create(file.path(to, "paper"), showWarnings = FALSE)
    file.copy(list.files(file.path(from, "paper"), full.names = TRUE), file.path(to, "paper"))
  }
  questions <- list.dirs(from, recursive = FALSE, full.names = FALSE)
  questions <- questions[file.exists(file.path(from, questions, "query_plan.csv"))]
  answers <- list()
  for (q in questions) {
    dir.create(file.path(to, "questions", q), recursive = TRUE, showWarnings = FALSE)
    files <- list.files(file.path(from, q), pattern = "\\.(csv|md)$", full.names = TRUE)
    file.copy(files, file.path(to, "questions", q))
    rds <- file.path(from, q, "study.rds")
    if (file.exists(rds)) {
      s <- readRDS(rds)
      for (cond in names(s$runs)) {
        g <- s$runs[[cond]]$generation
        answers[[length(answers) + 1]] <- data.frame(
          question_id = q, condition = cond, query = s$runs[[cond]]$query,
          provider = if (is.null(g)) NA_character_ else g$provider,
          answer = if (is.null(g)) NA_character_ else paste(g$answer, collapse = "\n"),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (length(answers)) utils::write.csv(do.call(rbind, answers), file.path(to, "answers.csv"), row.names = FALSE)
  ann <- file.path(from, "annotation")
  if (dir.exists(ann)) {
    dir.create(file.path(to, "annotation"), showWarnings = FALSE)
    # Item-level scores list the search behind each sentence, so they are left out.
    scores <- list.files(ann, pattern = "^scores_.*\\.csv$", full.names = TRUE)
    file.copy(scores[basename(scores) != "scores_claims_items.csv"], file.path(to, "annotation"))
    # Progress only: how many items exist and how many each annotator has filled in.
    count_done <- function(files, column) {
      do.call(rbind, lapply(files, function(f) {
        x <- utils::read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))
        data.frame(sheet = basename(f), items = nrow(x), done = sum(!is.na(x[[column]])))
      }))
    }
    rel <- list.files(ann, "^relevance_annotator_\\d+\\.csv$", full.names = TRUE)
    cla <- list.files(ann, "^claims_annotator_\\d+\\.csv$", full.names = TRUE)
    status <- rbind(
      if (length(rel)) cbind(task = "relevance", count_done(rel, "relevance_0_1_2")),
      if (length(cla)) cbind(task = "claims", count_done(cla, "support")))
    if (!is.null(status)) utils::write.csv(status, file.path(to, "annotation", "status.csv"), row.names = FALSE)
  }
  if (!length(questions)) stop("No question folders (with query_plan.csv) found in ", from, call. = FALSE)
  message("Copied ", length(questions), " questions from ", from, " to ", to)
  invisible(TRUE)
}

# The Sámi sensitivity test: the comparison tables and the run information of
# its two batches (no records, no answers).
copy_sensitivity <- function(from, to) {
  from <- find_batch_dir(from, "sensitivity_summary.csv")
  if (!dir.exists(from)) {
    message("Skipping the Sámi test (not found): ", from)
    return(invisible(FALSE))
  }
  unlink(to, recursive = TRUE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(from, pattern = "^sensitivity_.*\\.csv$", full.names = TRUE), to)
  info <- file.path(from, "sensitivity_info.csv")
  runs <- if (file.exists(info)) unlist(utils::read.csv(info, stringsAsFactors = FALSE)[1, c("base_dir", "alt_dir")]) else c("base", "with-se")
  for (i in seq_along(runs)) {
    sub <- file.path(to, c("base", "with-se")[i])
    dir.create(sub, showWarnings = FALSE)
    file.copy(list.files(file.path(from, runs[[i]]), pattern = "^(run_info|annif_models|batch_metrics|failures)\\.csv$", full.names = TRUE), sub)
  }
  message("Copied the Sámi test from ", from, " to ", to)
  invisible(TRUE)
}

copy_run(final_dir, "data/final")
copy_run(pilot_dir, "data/batch-pilot")
copy_sensitivity(sami_dir, "data/sami-sensitivity")
