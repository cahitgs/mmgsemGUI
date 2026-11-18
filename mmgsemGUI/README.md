# mmgsemGUI

A point-and-click interface for Mixture Multigroup Structural Equation Modeling
(MMG-SEM) with the [`mmgsem`](https://github.com/AndresFPA/mmgsem) R package
(Perez Alonso, Vermunt, Rosseel, Zhao & De Roover, 2026,
<https://doi.org/10.1080/10705511.2026.2617170>).

All estimation is done by `mmgsem` and `lavaan`. This package adds the
interface, background computation, diagnostics, graphics and an R script that
reproduces every analysis.

## Install and start

```r
install.packages("pak")
pak::pak("cahitgs/mmgsemGUI/mmgsemGUI")   # also installs mmgsem at the tested version
mmgsemGUI::run_app()
```

From a copy of this repository, `pak::local_install("mmgsemGUI")` installs
the package from its folder.

The interface opens in the browser but runs entirely on your computer: no data
leave it and no internet connection is needed.

## The steps

1. **Data** — a file with one row per respondent, one column per item and a
   grouping variable (.csv, .txt, .xlsx, .sav, .rds), or the tutorial example.
2. **Measurement model** — define the factors by clicking or in lavaan syntax,
   release loadings that differ between groups, and estimate a multigroup CFA
   with (partial) metric invariance; optionally with FIML, measurement blocks
   or ordinal items. Two checks support that decision: a
   comparison with the configural model and a score-test screen of the equal
   loadings.
3. **Structural model** — choose the regressions between the factors; the
   path diagram updates as you click.
4. **Number of clusters** — models for a range of cluster numbers are
   estimated in parallel and compared on six criteria, with scree plots and
   the share of random starts that reached the best solution.
5. **Clusters and relations** — path diagrams per cluster, coefficients,
   cluster membership and classification certainty.
6. **Hypothesis tests** — quick (naive) or corrected standard errors, z tests
   against zero, Wald tests across clusters and pairwise comparisons.
7. **Describe the clusters** — compare a group-level variable across clusters.
8. **R script and files** — the script of the analysis, result tables, and a
   project file that keeps everything.

## Does it give the same results as `mmgsem`?

Yes, by construction and by test. The interface calls `mmgsem::mmgsem()` and
`mmgsem::compute_se()`; it estimates the models for different numbers of
clusters in separate R processes and assembles them into the object that
`mmgsem::ModelSelection()` returns.

`tests/testthat` checks, among other things, that

* the model-selection table equals `ModelSelection()` and Output Box 1 of the
  tutorial, and each criterion selects the model reported there;
* the coefficients and clusters equal Output Box 2, and the five-cluster
  solution of Appendix F equals Output Box 7;
* computed from the standard errors of the tutorial (`standard_errors_deprecated`
  and `five_K_se` in mmgsem), the Wald statistics equal Output Box 5, the
  pairwise comparisons equal Output Boxes 6 and 8, and the z tests equal the
  output of `summary()`;
* the uncorrected standard errors of the current mmgsem agree with lavaan's for
  the equivalent multigroup path model (see below);
* an analysis run through the interface functions equals `mmgsem` called
  directly;
* the score tests behind "Screen the equal loadings" equal the univariate
  tests of `lavaan::lavTestScore()`. They are computed with one matrix
  factorisation instead of one per constraint: about 20 seconds instead of
  more than an hour for the tutorial example.

Run them with `NOT_CRAN=true Rscript -e 'testthat::test_local()'`.

Two deliberate differences from calling `lavaan::cfa()` with its defaults:

* The measurement model is estimated with `se = "none"`. MMG-SEM uses only its
  estimates, which are unchanged; for the tutorial example this takes 8
  instead of 28 seconds (8-core laptop). The exported script uses the default
  call.
* Unless FIML is chosen, rows with missing values on the analysis variables
  are removed before estimation, and the number of removed rows is reported.

## Standard errors of the tutorial example

Output Boxes 4 to 6 of the tutorial were produced with `compute_se()` as it
was in August 2025. The function was revised in October 2026 (mmgsem commit
`8f4386e`, "Fixed issues related to indexing of the parameter vector"), and
what it returns since then differs for the regressions of F3 on F2:

| Cluster | Output Box 4 (uncorrected) | Corrected, earlier version | Corrected, revised version |
|---|---|---|---|
| 2 | 0.093 | 0.094 | 0.026 |
| 3 | 0.039 | 0.039 | 0.027 |
| 4 | 0.035 | 0.035 | 0.027 |

The other standard errors agree within 0.003, and no conclusion about a
coefficient being different from zero changes. The Wald statistics become
69.5, 104.6, 79.6 and 81.2 (Box 5: 64.7, 96.0, 48.8, 75.8), and 12 instead of
11 of the 24 pairwise comparisons are significant after the Bonferroni
correction (in Output Box 6, F3 ~ F2 between clusters 2 and 4 is not).

Since 17 November 2026 mmgsem stores the values of the revised function as
`standard_errors` and keeps the earlier ones as `standard_errors_deprecated`;
`five_K_se`, for the five-cluster solution of Appendix F, is unchanged. The
corrected standard errors that the interface offers for the example without
waiting (`data-raw/tutorial_se.R`; 124 minutes on the development computer)
are identical to the new `standard_errors`. For every other analysis the
interface computes the standard errors with `compute_se()` itself.

That the difference comes from the change in mmgsem, and not from this
interface, was checked by replication: mmgsem as it was on 29 August 2025
(commit `0df43d1`), run on the tutorial example, gives the standard errors
printed in Output Box 4 for all sixteen coefficients (0.093 for F3 ~ F2 in
cluster 2); the revised version gives 0.026 for that coefficient.

The uncorrected standard errors of the revised version were also compared
with a computation that uses no code of mmgsem. With the groups assigned to
their clusters, Step 2 is a multigroup path model on the factor covariance
matrices of Step 1 in which the regressions are equal within a cluster.
lavaan's analytic standard errors for that model agree with
`compute_se(naive = TRUE)` within 0.001 for all sixteen coefficients (0.026
for F3 ~ F2 in cluster 2); the standard errors of Output Box 4 differ from
them by up to 0.067. This comparison says nothing about the correction for
stepwise estimation, because the path model takes the factor covariance
matrices as given. In the example the correction adds between 0.2% and 2.3%
to the uncorrected standard errors.

Confidence intervals can differ from those printed by `summary()` by 0.001:
`summary()` computes them from the estimate rounded to three decimals, the
interface from the estimate itself.

## The printed tutorial code and the current package

The exported script follows the code of the tutorial, with these differences,
because the code as printed in the article does not run, or does not do what
is intended, with the current version of mmgsem (the authors' script at
<https://github.com/AndresFPA/Tutorial-mmgsem> is correct where noted):

* `group.partial = c("F1 =~ v2", ...)` is printed with a lower-case `v`
  (main text and Appendices A and B). The items are called `V2`, ..., and
  lavaan ignores a name it does not find without a warning, so the printed
  call estimates full instead of partial metric invariance (1216 instead of
  1308 parameters). The authors' script and Output Box 1 use `V2`. In the
  interface released loadings are chosen from the model, so this cannot happen.
* `plot(ModelSel = ...)` and `summary(model = ...)` fail; the arguments are
  now `x` and `object`. The exported script passes the object unnamed.
* Appendix A assigns the fourth block to `S1_model.F3` a second time, so
  `S1_model.F4` does not exist.
* Appendix B needs `ordered = TRUE` in the call of `mmgsem()` (the authors have
  added this correction to their tutorial repository); Table E1 lists this
  argument as `ordinal`.
* Appendix D and Section 4.2.4 describe the measurement model that `mmgsem()`
  estimates itself as fully metric invariant. Without `group.equal = "loadings"`
  in the call it is a configural model: the loadings differ between groups.
  The interface always estimates the measurement model with equal loadings
  (except the released ones) and hands it to `mmgsem()`.
* Output Box 4 shows uncorrected standard errors of the earlier version of
  `compute_se()`; Output Boxes 5, 6 and 8 are based on the corrected ones of
  that version, which mmgsem keeps as `standard_errors_deprecated` (four
  clusters) and `five_K_se` (five clusters).

## Ways to estimate the measurement model

Besides the default (multigroup CFA on complete rows), the three alternatives
of the tutorial are available in the measurement-model step:

* **Missing answers with FIML** (Section 4.2.1). Incomplete rows are kept and
  `missing = "fiml"` is passed to `lavaan::cfa()` and to `mmgsem()`, which then
  takes its sample statistics from the incomplete data in the same way.
* **Measurement blocks** (Appendix A). Each factor is estimated in its own
  multigroup CFA; `mmgsem()` receives the list of models and fits.
* **Ordinal items** (Appendix B). Categorical CFA with the Wu and Estabrook
  (2016) identification, thresholds and loadings equal across groups. `mmgsem()`
  is called with `ordered = TRUE` (and the same constraints); without
  `ordered = TRUE` it neither uses polychoric correlations nor rescales the
  factors to the marker-variable scale. The result equals letting `mmgsem()`
  estimate Step 1 itself (tested).

Ordinal items cannot be combined with the other two (mmgsem does not support
it), and the two invariance checks are not available for ordinal items.

## Safeguards

* A measurement model that did not converge keeps the next steps closed; an
  inadmissible solution (for example a negative variance) is flagged.
* `mmgsem()` returns a result whenever the EM algorithm stops. A solution whose
  best start stopped at the iteration limit is marked as not converged, a
  cluster without groups is reported, and warnings raised during estimation
  are shown with the solution they belong to.
* Known limit: `mmgsem()` also stops when the log-likelihood decreases, and its
  output does not say so; that case cannot be detected from outside.

## Not included yet

* Bayesian (blavaan) and multilevel (Mplus) measurement models.

## Windows installer

`installer/build_installer.R` (in the folder above this package) produces
`mmgsemGUI-setup-<version>.exe`: one file that installs R, the packages and
the application for the current user, without administrator rights. People
who use the installed application do not need R, and it uses only the packages
it ships with. Building it needs Inno Setup 6 on the building computer:

```
Rscript installer/build_installer.R
```

The installed application opens in its own window and stops when that window
is closed.

## Versions

Developed and tested with R 4.6.1, lavaan 0.7-2 and mmgsem 0.0.1 (GitHub commit
`f3b2622`). `mmgsem` is under development; if its interface changes, the tests
above show where.
