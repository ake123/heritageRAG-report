# Shared set-up for every page of the report.
# All numbers on the website are calculated here from the files in data/,
# so re-running prepare_data.R and re-rendering updates the whole site.

suppressPackageStartupMessages(library(knitr))
knitr::opts_chunk$set(dev = "png", dpi = 150, fig.align = "left")

FINAL <- "data/final"
PILOT <- "data/batch-pilot"

# Path of a result file. prepare_data.R puts each question's files in
# data/final/questions/<id>/; a results folder copied into data/ as it is has
# them in data/final/<id>/. Both layouts work.
data_path <- function(file, run = FINAL) {
  path <- file.path(run, file)
  if (!file.exists(path) && grepl("^questions/", file)) {
    alt <- file.path(run, sub("^questions/", "", file))
    if (file.exists(alt)) return(alt)
  }
  path
}

rd <- function(file, run = FINAL) {
  path <- data_path(file, run)
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE,
                  na.strings = c("NA"))
}

# The AI answers, from answers.csv (written by prepare_data.R) or, for a
# results folder copied as it is, straight from each question's study.rds.
read_answers <- function(run = FINAL) {
  a <- rd("answers.csv", run)
  if (!is.null(a)) return(a)
  ids <- list.dirs(run, recursive = FALSE, full.names = FALSE)
  rows <- lapply(ids, function(q) {
    f <- file.path(run, q, "study.rds")
    if (!file.exists(f)) return(NULL)
    s <- readRDS(f)
    do.call(rbind, lapply(names(s$runs), function(cond) {
      g <- s$runs[[cond]]$generation
      data.frame(question_id = q, condition = cond, query = s$runs[[cond]]$query,
                 provider = if (is.null(g)) NA_character_ else g$provider,
                 answer = if (is.null(g)) NA_character_ else paste(g$answer, collapse = "\n"),
                 stringsAsFactors = FALSE)
    }))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows)) do.call(rbind, rows) else NULL
}

# Checking progress, from annotation/status.csv or counted from the sheets.
read_status <- function(run = FINAL) {
  st <- rd("annotation/status.csv", run)
  if (!is.null(st)) return(st)
  ann <- file.path(run, "annotation")
  count <- function(pattern, column, task) {
    files <- list.files(ann, pattern, full.names = TRUE)
    if (!length(files)) return(NULL)
    do.call(rbind, lapply(files, function(f) {
      x <- utils::read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      data.frame(task = task, sheet = basename(f), items = nrow(x), done = sum(!is.na(x[[column]])))
    }))
  }
  rbind(count("^relevance_annotator_\\d+\\.csv$", "relevance_0_1_2", "relevance"),
        count("^claims_annotator_\\d+\\.csv$", "support", "claims"))
}

# ---- The three searches ----------------------------------------------------
conds <- c("A_direct", "B_annif", "C_finto")
cond_short <- c(A_direct = "A: own words", B_annif = "B: + Annif", C_finto = "C: + YSO terms")
cond_long <- c(A_direct = "Search A: your own words",
               B_annif = "Search B: your words + the indexer's suggestions",
               C_finto = "Search C: B + official thesaurus terms in Finnish, Swedish and English")
cond_col <- c(A_direct = "#3A5A78", B_annif = "#2E8B7A", C_finto = "#8A5A9E")

# ---- Formatting helpers ----------------------------------------------------
pct <- function(x, digits = 0) ifelse(is.na(x) | !is.finite(x), "-", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
num <- function(x, digits = 0) ifelse(is.na(x) | !is.finite(x), "-", formatC(x, format = "f", digits = digits, big.mark = " "))
times <- function(x) ifelse(is.na(x) | !is.finite(x), "-", paste0(formatC(x, format = "f", digits = ifelse(x >= 10, 0, 1)), "x"))
yr <- function(x) ifelse(is.na(x) | !is.finite(x), "-", as.character(round(x)))
number_word <- function(n) if (n >= 0 && n <= 10) c("No", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")[n + 1] else as.character(n)
provider_name <- function(p) { m <- c(openai = "OpenAI", anthropic = "Anthropic"); ifelse(p %in% names(m), m[p], p) }
# Put a blank line before list items so Markdown shows them as a list.
md_fix <- function(x) gsub("\n(?=- |\\* |\\d+\\. )", "\n\n", x, perl = TRUE)
tbl <- function(df, col.names = names(df), align = NULL, ...) {
  knitr::kable(df, col.names = col.names, align = align, row.names = FALSE, ...)
}
esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
callout <- function(type, title, body) {
  cat(sprintf("\n::: {.callout-%s}\n## %s\n\n%s\n:::\n\n", type, title, paste(body, collapse = "\n")))
}

# ---- Data ----------------------------------------------------------------
run_info <- rd("run_info.csv")
pilot_info <- rd("run_info.csv", PILOT)
questions <- rd("questions_used.csv")
metrics <- rd("batch_metrics.csv")
overlap <- rd("batch_overlap.csv")
queries <- rd("batch_queries.csv")
annif <- rd("batch_annif_mapping.csv")
vis <- rd("batch_visibility_shift.csv")
temporal <- rd("batch_temporal.csv")
themes <- rd("batch_themes.csv")
trace <- rd("batch_traceability.csv")
answers <- read_answers()
failures <- rd("failures.csv")
pilot_metrics <- rd("batch_metrics.csv", PILOT)

short_names <- c(
  "women-advertising" = "Women in advertising (asked in English)",
  "women-advertising-fi" = "Women in advertising (asked in Finnish)",
  "emigrant-letters" = "Emigrant letters",
  "civil-war-camps" = "Civil War prison camps",
  "temperance-posters" = "Temperance posters",
  "sami-handicrafts" = "Sámi handicrafts",
  "karelian-evacuees" = "Karelian evacuees",
  "working-class-housing" = "Working-class housing",
  "finland-swedish-theatre" = "Swedish-language theatre",
  "womens-suffrage" = "Women's suffrage",
  "sawmill-workers" = "Sawmill workers",
  "childrens-book-illustration" = "Children's book illustration",
  "amateur-radio" = "Amateur radio clubs")
qname <- function(id) ifelse(id %in% names(short_names), short_names[id], id)
qids <- questions$id
n_q <- length(qids)
if (!n_q || !any(file.exists(vapply(qids, function(q) data_path(file.path("questions", q, "retrieved_records.csv")), character(1))))) {
  stop("No study results found in ", FINAL, ". Copy them in with\n",
       "  Rscript prepare_data.R <results>/final <results>/batch-pilot [<results>/sami-sensitivity]\n",
       "where <results>/final is the folder that holds batch_metrics.csv and one folder per question.",
       call. = FALSE)
}

# Every retrieved record of every question in one table.
records_all <- do.call(rbind, lapply(qids, function(q) {
  x <- rd(file.path("questions", q, "retrieved_records.csv"))
  if (is.null(x) || !nrow(x)) return(NULL)
  keep <- intersect(c("condition", "evidence", "record_id", "title", "year", "language", "institution",
                      "material_type", "introduced_by", "matched_terms", "source_url"), names(x))
  cbind(question_id = q, x[, keep])
}))
records_all$language[is.na(records_all$language)] <- ""

# Search size: matching records in the whole Finna index, relative to search A.
idx <- reshape(metrics[, c("question_id", "condition", "index_matches")], idvar = "question_id",
               timevar = "condition", direction = "wide")
names(idx) <- sub("index_matches.", "", names(idx), fixed = TRUE)
idx <- idx[match(qids, idx$question_id), ]
# Questions where no search found anything to keep.
recs <- reshape(metrics[, c("question_id", "condition", "records")], idvar = "question_id",
                timevar = "condition", direction = "wide")
names(recs) <- sub("records.", "", names(recs), fixed = TRUE)
empty_q <- recs$question_id[recs$A_direct == 0 & recs$B_annif == 0 & recs$C_finto == 0]
# Questions left out of the averages by hand, with the reason (edit
# exclude.csv; delete a row once a re-run has removed its cause).
excluded <- if (file.exists("exclude.csv")) utils::read.csv("exclude.csv", stringsAsFactors = FALSE, encoding = "UTF-8") else
  data.frame(question_id = character(), reason = character())
excluded <- excluded[excluded$question_id %in% setdiff(qids, empty_q), , drop = FALSE]
usable_q <- setdiff(qids, c(empty_q, excluded$question_id))
left_out_text <- function() {
  parts <- c(if (length(empty_q)) paste0(paste(qname(empty_q), collapse = " and "), ", which found no records in any search"),
             if (nrow(excluded)) paste0(qname(excluded$question_id), " (", excluded$reason, ")"))
  if (!length(parts)) return("")
  paste0("*Left out of the averages:* ", paste(parts, collapse = "; "), ". See [Question by question](questions.qmd).")
}

# Ratios only where search A matched something and the question found records.
ok <- idx$A_direct > 0 & idx$question_id %in% usable_q
idx$ratio_B <- ifelse(ok, idx$B_annif / idx$A_direct, NA)
idx$ratio_C <- ifelse(ok, idx$C_finto / idx$A_direct, NA)

# Annif suggestions
fits_levels <- c("same as your word", "added to your word", "only part of your word (not used)", "did not fit your words")
annif$fits <- ifelse(annif$mapping == "unanchored", "did not fit your words",
              ifelse(annif$mapping == "anchor_concept", "same as your word",
              ifelse(annif$mapping == "partial_phrase", "only part of your word (not used)", "added to your word")))

# How each key word was matched in the thesaurus, in plain words.
method_names <- c(lookup = "exact name", annif = "Annif's concept", search_exact = "search, same name",
                  search_qualifier = "search, name with a bracket", search_altlabel = "search, other name",
                  search_annif = "search, confirmed by Annif", search = "search, first hit", none = "no match")
method_plain <- function(m) {
  out <- ifelse(m %in% names(method_names), method_names[m], m)
  out <- ifelse(grepl("^ambiguous", m), "several meanings, none chosen", out)
  ifelse(grepl("^error", m), "thesaurus not reachable", out)
}

# ---- Software versions and the Annif model behind the run --------------------
data_ver <- if (!is.null(run_info$heritageRAG)) as.character(run_info$heritageRAG[1]) else NA_character_
newer_than <- function(v, ver = data_ver) !is.na(ver) && utils::compareVersion(ver, v) >= 0
annif_models <- rd("annif_models.csv")
annif_pkg_ver <- if (!is.null(run_info$annif) && !is.na(run_info$annif[1])) as.character(run_info$annif[1]) else NA_character_
annif_model_text <- function(models = annif_models) {
  if (is.null(models) || !nrow(models)) {
    return("not recorded in this run (heritageRAG records it from version 0.3.0)")
  }
  paste(vapply(seq_len(nrow(models)), function(i) {
    m <- models[i, ]
    bits <- c(if (!is.na(m$backend) && nzchar(m$backend)) paste0("backend *", m$backend, "*"),
              if (!is.na(m$model_modified) && nzchar(m$model_modified)) paste0("model last changed ", substr(m$model_modified, 1, 10)))
    paste0("`", m$project_id, "`", if (length(bits)) paste0(" (", paste(bits, collapse = ", "), ")") else "")
  }, character(1)), collapse = "; ")
}
# Status line for a problem fixed in a given package version.
fix_status <- function(version = "0.3.0") {
  if (newer_than(version)) {
    sprintf("<span class=\"status status-fixed\">Fixed</span> This run used heritageRAG %s, which includes the fix.", data_ver)
  } else {
    sprintf("<span class=\"status status-pending\">Fixed in heritageRAG %s</span> This run used %s, so the numbers here still show the problem. Re-run the study to update them.",
            version, ifelse(is.na(data_ver), "an older version", data_ver))
  }
}

# ---- Is a difference bigger than chance? (heritageRAG 0.3.0 and later) -------
has_noise <- !is.null(vis) && "shift_beyond_noise" %in% names(vis)
noise_sentence <- function(v, what, to = "C_finto") {
  if (!has_noise) {
    return("*This run was made before heritageRAG 0.3.0, so it does not yet say which differences are bigger than chance with 60 records. Re-running adds that.*")
  }
  v <- v[v$to == to & !is.na(v$shift_beyond_noise), , drop = FALSE]
  sprintf("In **%s of %s** questions the difference in %s between A and %s is bigger than chance alone would give with this many records.",
          sum(v$shift_beyond_noise %in% TRUE), nrow(v), what, substr(to, 1, 1))
}

# ---- Extra tests (filled in once their results are copied into data/) ---------
SAMI <- "data/sami-sensitivity"
sami_info <- rd("sensitivity_info.csv", SAMI)
sami_q <- rd("sensitivity_by_question.csv", SAMI)
sami_lang <- rd("sensitivity_languages.csv", SAMI)
sami_terms <- rd("sensitivity_terms.csv", SAMI)
sami_summary <- rd("sensitivity_summary.csv", SAMI)
sami_run <- rd("run_info.csv", file.path(SAMI, "with-se"))
cat_records <- rd("annif_vs_catalogue_records.csv")
cat_by <- rd("annif_vs_catalogue_by_condition.csv")
cat_summary <- rd("annif_vs_catalogue_summary.csv")
have_sami <- !is.null(sami_q) && nrow(sami_q) > 0
have_cat <- !is.null(cat_summary) && nrow(cat_summary) > 0
sami_codes <- c("sme", "smn", "sms", "sma", "smj", "sjd", "sje", "sju", "smi")

# Overlap with search A
ov_A <- overlap[overlap$condition_a == "A_direct", ]

# Languages among records whose language is known
rec_use <- records_all[records_all$question_id %in% usable_q, ]
lang_known <- rec_use[nzchar(rec_use$language), ]
lang_known_share <- nrow(lang_known) / nrow(rec_use)

# Anchors that the national thesaurus could not match
anchor_tbl <- do.call(rbind, lapply(qids, function(q) {
  x <- rd(file.path("questions", q, "anchor_concepts.csv"))
  if (is.null(x)) return(NULL)
  data.frame(question_id = q, anchor = x$anchor, uri = x$uri, label = x$prefLabel, method = x$match_method)
}))
unmatched <- anchor_tbl[is.na(anchor_tbl$uri) | !nzchar(anchor_tbl$uri), ]

diagnostics_for <- function(q) {
  md <- data_path(file.path("questions", q, "narratives.md"))
  if (!file.exists(md)) return(character())
  lines <- readLines(md, warn = FALSE, encoding = "UTF-8")
  start <- grep("^## Diagnostics", lines)
  if (!length(start)) return(character())
  d <- sub("^- ", "", lines[(start + 1):length(lines)])
  d[nzchar(trimws(d)) & d != "none"]
}

# Hand-written notes (edit notes.csv to change them)
notes <- if (file.exists("notes.csv")) utils::read.csv("notes.csv", stringsAsFactors = FALSE, encoding = "UTF-8") else data.frame(question_id = character(), note = character())

# ---- Charts --------------------------------------------------------------
chart_par <- function(...) graphics::par(family = "sans", las = 1, bty = "n", cex = 0.95, col.axis = "#33454F",
                                         col.lab = "#33454F", fg = "#55636B", ...)

# Dots per question for the three searches; zeros drawn as open circles at the left edge.
dot_chart <- function(values, xlab, log = FALSE, xlim = NULL, order_ids = usable_q, show = conds) {
  ids <- rev(order_ids)
  v <- values[match(ids, values$question_id), conds]
  chart_par(mar = c(4.2, 15, 2.2, 1))
  vv <- unlist(v); vv <- vv[is.finite(vv)]
  if (log) {
    pos <- vv[vv > 0]
    xlim <- xlim %||% c(min(1, pos), max(pos) * 1.3)
  } else xlim <- xlim %||% c(0, max(vv, 1) * 1.05)
  graphics::plot(NA, xlim = xlim, ylim = c(0.5, length(ids) + 0.5), log = if (log) "x" else "",
                 yaxt = "n", xlab = xlab, ylab = "")
  graphics::abline(h = seq_along(ids), col = "#E3E8EA", lwd = 1)
  graphics::axis(2, at = seq_along(ids), labels = qname(ids), tick = FALSE, cex.axis = 0.85)
  off <- c(-0.15, 0, 0.15)
  for (j in seq_along(conds)) {
    if (!conds[j] %in% show) next
    x <- v[[conds[j]]]
    zero <- !is.na(x) & x <= 0 & log
    graphics::points(ifelse(zero, xlim[1], x), seq_along(ids) + off[j], pch = ifelse(zero, 1, 19),
                     col = cond_col[conds[j]], cex = 1.15)
  }
  graphics::legend("top", inset = c(0, -0.09), xpd = TRUE, horiz = TRUE, bty = "n", pch = 19,
                   col = cond_col[show], legend = cond_short[show], cex = 0.85)
}

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# ---- Category comparison charts (as in the Research Lab's Visibility tab) ----
dim_label <- c(institution = "Institution", language = "Language", material = "Material type",
               decade = "Decade", subject = "Subject word")
lang_full <- c(fin = "Finnish", swe = "Swedish", eng = "English", smi = "Sámi languages", sme = "Northern Sámi",
               smn = "Inari Sámi", sms = "Skolt Sámi", sma = "Southern Sámi", smj = "Lule Sámi",
               nor = "Norwegian", deu = "German", fra = "French", rus = "Russian", spa = "Spanish", est = "Estonian",
               lat = "Latin", dan = "Danish", ita = "Italian", mul = "Several languages", zxx = "No linguistic content")

vis_q <- function(q) rd(file.path("questions", q, "visibility_by_condition.csv"))
n_kept <- function(q) {
  m <- metrics[metrics$question_id == q, ]
  stats::setNames(m$records[match(conds, m$condition)], conds)
}

# Long table (value, condition, records) -> wide counts for one dimension.
category_counts <- function(v, dim) {
  v <- v[v$dimension == dim, , drop = FALSE]
  if (!nrow(v)) return(NULL)
  if (dim == "subject") v$value <- sub("[.]+$", "", v$value)
  if (dim == "language") v$value <- ifelse(v$value %in% names(lang_full), lang_full[v$value], v$value)
  v$value[v$value == "(unknown)"] <- "(not given)"
  agg <- stats::aggregate(records ~ value + condition, data = v, FUN = max)
  w <- reshape(agg, idvar = "value", timevar = "condition", direction = "wide")
  names(w) <- sub("records.", "", names(w), fixed = TRUE)
  for (c in conds) if (!c %in% names(w)) w[[c]] <- 0
  w[is.na(w)] <- 0
  w[, c("value", conds)]
}

# Draw grouped horizontal bars: one group per category, one bar per search.
# Labels of categories that only B or C show are purple; those C drops are grey.
draw_group_bars <- function(w, n, top = 10, title = NULL, chronological = FALSE) {
  share <- w
  for (c in conds) share[[c]] <- if (n[[c]] > 0) w[[c]] / n[[c]] else 0
  share$max <- apply(share[, conds], 1, max)
  keep <- order(-share$max, share$value)[seq_len(min(top, nrow(share)))]
  if (chronological) keep <- keep[order(w$value[keep])]   # decades read top to bottom in time order
  w <- w[keep, ]; share <- share[keep, ]
  k <- nrow(w)
  a_has <- n[["A_direct"]] > 0
  status <- if (!a_has) rep("normal", k) else
            ifelse(w$A_direct == 0 & (w$B_annif > 0 | w$C_finto > 0), "new",
            ifelse(w$A_direct > 0 & w$C_finto == 0, "gone", "normal"))
  xmax <- max(share$max, 0.05) * 1.18
  chart_par(mar = c(3.6, 15.5, 3.2, 1.2))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, xmax), ylim = c(0.45, k + 0.55), xaxs = "i")
  ticks <- pretty(c(0, xmax * 0.95), n = 5)
  graphics::abline(v = ticks, col = "#E3E8EA")
  off <- c(A_direct = 0.27, B_annif = 0, C_finto = -0.27)
  for (i in seq_len(k)) {
    y <- k - i + 1
    for (c in conds) {
      yy <- y + off[[c]]
      if (n[[c]] > 0) {
        graphics::rect(0, yy - 0.115, share[[c]][i], yy + 0.115, col = cond_col[[c]], border = NA)
        if (w[[c]][i] > 0) graphics::text(share[[c]][i], yy, w[[c]][i], pos = 4, cex = 0.62, col = "#55636B", offset = 0.25)
      }
    }
  }
  lab <- ifelse(nchar(w$value) > 40, paste0(substr(w$value, 1, 39), "..."), w$value)
  graphics::text(-xmax * 0.015, rev(seq_len(k)), lab, pos = 2, xpd = TRUE, cex = 0.82,
                 col = c(normal = "#1D2B33", new = "#7A3F94", gone = "#9AA5AB")[status],
                 font = ifelse(status == "new", 2, 1))
  graphics::axis(1, at = ticks, labels = paste0(round(100 * ticks), "%"), cex.axis = 0.8, lwd = 0, lwd.ticks = 1)
  graphics::mtext("Share of each search's kept records (number = record count)", side = 1, line = 2.3, cex = 0.72, col = "#55636B")
  leg <- paste0(substr(cond_short, 1, 1), " (", ifelse(n > 0, paste(n, "records"), "none"), ")")
  usr <- graphics::par("usr")
  line_h <- graphics::par("csi") * diff(usr[3:4]) / graphics::par("pin")[2]  # one text line in y units
  graphics::legend(x = mean(usr[1:2]), y = usr[4] + 1.35 * line_h, xjust = 0.5, yjust = 0.5,
                   xpd = NA, horiz = TRUE, bty = "n", fill = cond_col, border = NA, legend = leg, cex = 0.78)
  if (!is.null(title)) graphics::title(main = title, adj = 0, cex.main = 0.9, line = 2.2, font.main = 1)
  invisible(status)
}

# Save one comparison chart as a PNG sized to its number of categories.
save_group_chart <- function(w, n, file, top = 10, ...) {
  if (is.null(w) || !nrow(w) || sum(n) == 0) return(NULL)
  k <- min(top, nrow(w))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(file, width = 6.6, height = 1.3 + 0.42 * k, units = "in", res = 160)
  on.exit(grDevices::dev.off())
  draw_group_bars(w, n, top = top, ...)
  file
}

# One or two plain sentences on what changed between two searches.
change_note <- function(w, n, dim) {
  if (is.null(w) || !nrow(w)) return("Nothing recorded for this dimension.")
  from <- if (n[["A_direct"]] > 0) "A_direct" else "B_annif"
  fl <- substr(from, 1, 1)
  if (n[[from]] == 0 || n[["C_finto"]] == 0) return("Too few records to compare.")
  known <- w[w$value != "(not given)", , drop = FALSE]
  sh_from <- known[[from]] / n[[from]]; sh_c <- known$C_finto / n[["C_finto"]]
  out <- character()
  if (from == "B_annif") out <- c(out, "Search A found nothing, so this compares B with C.")
  new <- known[known[[from]] == 0 & known$C_finto > 0, ]
  new <- head(new[order(-new$C_finto), ], 3)
  if (nrow(new)) out <- c(out, sprintf("**New in C:** %s.", paste0(new$value, " (", new$C_finto, ")", collapse = ", ")))
  gone <- known[known[[from]] > 0 & known$C_finto == 0, ]
  gone <- head(gone[order(-gone[[from]]), ], 3)
  if (nrow(gone)) out <- c(out, sprintf("**Missing from C:** %s.", paste0(gone$value, " (", gone[[from]], " in ", fl, ")", collapse = ", ")))
  both <- which(known[[from]] > 0 & known$C_finto > 0)
  if (length(both)) {
    d <- sh_c[both] - sh_from[both]
    i <- both[which.max(abs(d))]
    if (abs(sh_c[i] - sh_from[i]) >= 0.1) out <- c(out, sprintf("**Biggest change in share:** %s, from %s in %s to %s in C.",
                                                              known$value[i], pct(sh_from[i]), fl, pct(sh_c[i])))
  }
  ng <- w[w$value == "(not given)", , drop = FALSE]
  if (dim == "language" && nrow(ng)) out <- c(out, sprintf("No language is given for %s of %s's records and %s of C's.",
                                                          pct(ng[[from]] / n[[from]]), fl, pct(ng$C_finto / n[["C_finto"]])))
  if (!length(out)) out <- "The three searches show much the same mix."
  paste(out, collapse = " ")
}

# Counts summed over all questions that found records (for the overview charts).
pooled_counts <- function(dim, ids = usable_q) {
  v <- do.call(rbind, lapply(ids, function(q) { x <- vis_q(q); if (is.null(x)) NULL else x[x$dimension == dim, ] }))
  if (is.null(v) || !nrow(v)) return(NULL)
  v$value[v$value == "(unknown)"] <- "(not given)"
  agg <- stats::aggregate(records ~ value + condition, data = v, FUN = sum)
  w <- reshape(agg, idvar = "value", timevar = "condition", direction = "wide")
  names(w) <- sub("records.", "", names(w), fixed = TRUE)
  for (c in conds) if (!c %in% names(w)) w[[c]] <- 0
  w[is.na(w)] <- 0
  w[, c("value", conds)]
}
pooled_n <- function(ids = usable_q) {
  m <- metrics[metrics$question_id %in% ids, ]
  stats::setNames(as.numeric(tapply(m$records, factor(m$condition, conds), sum)), conds)
}
