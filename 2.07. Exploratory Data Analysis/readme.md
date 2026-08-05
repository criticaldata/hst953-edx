Workshop files for HST.953x Section 2.07: Exploratory Data Analysis.

## Files

- `eda_workshop_student.Rmd` — the workshop; knit to HTML to answer the edX questions
- `eda_workshop_answerkey.Rmd` — completed version
- `mimicbook_dplyr_fix.R` — compatibility shim, see below

## mimicbook_dplyr_fix.R

The workshop depends on [MIMICbook](https://github.com/jraffa/MIMICbook), which
has not been updated since January 2018. Its `plot_prop_by_level()`,
`plot_odds_by_level()` and `plot_OR_by_level()` functions use `mutate_()`,
`group_by_()`, `rename_()` and `group_indices()` — deprecated in dplyr 0.7.0 and
defunct in current versions — along with `aes_string()`, deprecated in
ggplot2 3.0. Knitting fails with:

    `mutate_()` was deprecated in dplyr 0.7.0 and is now defunct.

Both Rmd files `source()` this shim immediately after `library(MIMICbook)`. It
defines corrected versions of the three functions in the global environment,
where they take precedence over the package's. Behaviour is otherwise
unchanged, so results still match the answer key and the edX questions.

**Downgrading dplyr is not necessary.**

If MIMICbook is fixed upstream (see
https://github.com/jraffa/MIMICbook/issues/1), the `source()` lines and this
file can both be deleted.

## Data

Both files read `../data/aline_full_cohort_data.csv`, falling back to the copy
on raw.githubusercontent.com if that path is unavailable. The URL originally in
the workshop pointed at a GitHub `blob` page, which returns HTML rather than
CSV.
