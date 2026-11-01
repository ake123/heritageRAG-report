# HeritageRAG study report (Quarto website and book)

A plain-language website that reports the HeritageRAG study from its result files.
Every number and chart is calculated from the files in `data/`, so a new study run only
needs two commands to update the whole site.

## Open it

The same pages are built two ways, so you can compare them:

- **Website** in `_site/`: open `_site/index.html`. Pages are reached from the menu bar.
- **Book** in `_book/`: open `_book/index.html`. Numbered chapters in three parts, a sidebar
  with all chapters, previous/next links at the bottom of each chapter, and search.

## Website or book

The shared settings are in `_quarto.yml`; `_quarto-website.yml` and `_quarto-book.yml` hold
what differs (Quarto *profiles*). The page files are the same for both.

```bash
quarto render                    # website -> _site/  (the default)
quarto render --profile book     # book    -> _book/
quarto preview --profile book    # book, live while editing
```

To publish only one of them on GitHub Pages:

```bash
quarto publish gh-pages                   # the website
quarto publish gh-pages --profile book    # or the book instead (replaces the website)
```

To publish both, with the book at `.../heritageRAG-report/book/`, build the website first
(rendering the website empties `_site/`), then the book into it, then publish without
rendering again:

```bash
quarto render
quarto render --profile book --output-dir _site/book
quarto publish gh-pages --no-render
```

## Rebuild or update it

You need [Quarto](https://quarto.org) (1.4 or newer) and R with the packages `knitr` and
`rmarkdown`. Nothing else: the charts use base R.

Keep this folder next to the `heritageRAG` package folder, then:

```bash
# 0. (optional) redo the analysis and paper figures with the latest heritageRAG,
#    without new searches; run from the heritageRAG folder:
#    Rscript scripts/refresh_analysis.R results/final --exclude=amateur-radio
# 1. copy the latest results into data/ (the third folder, the Sámi test, is optional)
Rscript prepare_data.R ../heritageRAG/results/final ../heritageRAG/results/batch-pilot ../heritageRAG/results/sami-sensitivity
# 2. build the website into _site/ (add --profile book for the book in _book/)
quarto render
# or, to watch it while editing:
quarto preview
```

Always copy results with `prepare_data.R`, not by copying the results folders into `data/`
by hand: the script extracts the AI answers, leaves out the files that would unblind the
checkers, and puts each question's files where the pages look for them. (The pages can also
read a results folder copied as it is, but then the checkers' sheets and keys sit in `data/`.)
If a zip was unpacked into a folder of the same name (`final/final/`), the script finds the
inner folder itself.

## What is where

| File | What it is |
|---|---|
| `index.qmd` ... `problems.qmd` | The eleven pages (`paper-figures.qmd` explains the figures in `data/final/paper/`; `compare.qmd` holds the A/B/C comparison charts for every question; `extra-tests.qmd` fills in once the Sámi test and the Annif-versus-catalogue test have been run) |
| `figs/compare/` | The comparison charts, recreated on every render |
| `_common.R` | Loads the data and holds the shared names, colours, charts and table helpers |
| `notes.csv` | Short hand-written notes per question (the only text that is not recalculated: check it after a new run) |
| `exclude.csv` | Questions left out of the averages by hand, with the reason shown on the pages. Delete a row once a re-run has removed its cause (the amateur-radio row after re-running with heritageRAG 0.3.1) |
| `prepare_data.R` | Copies what the site needs from the results folders into `data/` |
| `styles.scss`, `_quarto.yml`, `_quarto-website.yml`, `_quarto-book.yml` | Look, shared settings, and the website and book settings |
| `data/` | The copied results (the website's only input) |
| `_site/`, `_book/` | The built website and book |

## Keeping the checking blind

`prepare_data.R` deliberately does **not** copy the checkers' sheets, the `KEY_*` files or
item-level scores, because they reveal which search produced each item. The pages
*Question by question* and *AI answers* still show which search found what, so checkers
should not read them until they have finished.

## Sharing it

`_site/` is a static website, so any static host works: `quarto publish gh-pages` (GitHub
Pages), `quarto publish quarto-pub`, Netlify, or the university's web space.
