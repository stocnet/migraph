# Contributing

Contributions to `migraph`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

Please note that the `migraph` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

## Git

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The GitHub page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

### Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
Examples for documentation are also most welcome.

Issues that belong to another package in the family should be transferred there
rather than fixed here: `gh issue transfer <number> stocnet/<package>`.
See the division of labour below.

### Cloning

Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull

This command allows you to `pull` changes from the remote repository to your local repository.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose main or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check,
  and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries, then pkgdown site deploy.
- The PR metadata job requires that each PR into `main` bumps the `Version:` field in
  `DESCRIPTION` by the appropriate increment, names that new version in the PR title,
  and itemizes its changes in the PR description under `##` subsection titles matching
  the `NEWS.md` conventions below.
- Development dependencies are declared in `DESCRIPTION` under `Config/Needs/build`,
  `Config/Needs/check`, and `Config/Needs/website` rather than `Suggests` —
  the workflows install them via `needs:` in `setup-r-dependencies`.

## Package architecture

### Project overview

`migraph` is an R package (part of the [stocnet](https://github.com/stocnet) ecosystem)
providing the *inferential layer* for network analysis:
conditional uniform graph (CUG) and quadratic assignment procedure (QAP) tests of network
statistics, multiple regression QAP (MRQAP) for network data, and diffusion models.
It is also a software companion to
*Multimodal Political Networks* (Knoke, Diani, Hollway, and Christopoulos 2021),
whose datasets (prefixed `mpn_`) are bundled here.
Because it builds on `{manynet}`, every function accepts matrices, edgelists,
`{igraph}`, `{network}`, or `{tidygraph}` objects, and one-mode or two-mode networks alike.
Division of labour to keep in mind when adding functions:

- `{manynet}`: network classes/coercion (`as_*()`), making and manipulating networks,
  and network-level logical tests (e.g. `is_directed()`, `is_twomode()`).
- `{netrics}`: everything analytic — marks, measures, memberships, motifs —
  at the node, tie, and network level.
- `{autograph}`: drawing graphs and plotting analytic, modelling, or diagnostic results,
  along with deep (often institutional) theming. *All* plot methods should live there.
- `{migraph}` (this package): testing and modelling, e.g. CUG/QAP/MRQAP and diffusion models,
  and the `mpn_*` datasets.

### Style

In terms of style, we are aiming for pleasant predictability in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`test_*()`, `over_*()`, etc.) should share
argument order and naming, so that behaviour is guessable across the family.

When writing documentation or NEWS items, prefer breaking lines at punctuation.

Make it clear when you are referring to functions by adding backticks and parentheses,
e.g. `a_function()`, and arguments by adding an equals sign, e.g. `argument=`.
Argument values or variables can be in double quotation marks, e.g. "value".

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root
(or via `Rscript -e`).

- Load package for interactive development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "model_tests")`
  (matches `test-model_tests.R`), or `testthat::test_file("tests/testthat/test-model_regression.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Code coverage: `covr::package_coverage()`
- Rebuild `README.md` from `README.Rmd`: `devtools::build_readme()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.
Roxygen is configured with `markdown = TRUE`;
`NAMESPACE` and all `man/*.Rd` files are generated — never hand-edit them.
Likewise `README.md` is generated from `README.Rmd` — edit the `.Rmd` and re-knit.

### File organization

All exported functions live directly in `R/`, grouped by theme rather than one file per function:

| File | Contains |
|---|---|
| `model_tests.R` | the test family: `test_random()` (CUG), `test_permutation()` (QAP), `test_configuration()`, `test_distribution()`, `test_fit()`, `test_gof()` |
| `model_regression.R` | `net_regression()` (alias `network_reg()`), the MRQAP implementation, returning `netlm`/`netlogit` objects |
| `model_predict.R`, `model_distrib.R`, `model_diffusion.R` | prediction, distribution comparison, and diffusion simulation (`play_diffusions()`) |
| `class_models.R` | `{broom}`-style S3 methods (`tidy.*`, `glance.*`, `predict.*`, `print.*`, `summary.*`) for `netlm`, `netlogit`, `ergm`, and `sienaFit`, so results are comparable across model types |
| `class_makes.R` | internal `make_*()` constructors and `print()`/`summary()` methods for the lightweight result classes (`network_measures`, `diffs_model`, `over_memb`) |
| `measure_over.R` | `over_time()`, `over_waves()`, `over_membership()` — applying a measure across longitudinal slices or group partitions |
| `tutorial_run.R` | `run_tute()`/`extract_tute()`, `{learnr}` wrappers that search *all* installed stocnet packages for a tutorial by name or fuzzy title match |
| `data_mpn.R` | documentation for the `mpn_*` datasets from *Multimodal Political Networks* |
| `migraph-package.R` | package-level doc, the `thisRequires()` helper, global variables, and shims silencing R CMD check's unused-import notes |
| `migraph-defunct.R` | retired function stubs kept for a deprecation cycle |

### Function body conventions

Test functions consistently:

1. Compute the observed statistic by applying the user-supplied `FUN` to `.data`.
2. Generate `times` random, configuration-preserving, or permuted networks
   via the corresponding `{manynet}` generator (`generate_random()`,
   `generate_configuration()`, etc.), rebinding node attributes with
   `manynet::bind_node_attributes()` where the statistic needs them.
3. Recompute `FUN` over each simulated network.
4. Return a `network_test` object recording the test type, observed value,
   simulated distribution, one- and two-tailed p-values,
   and the network's properties (`is_directed()`, `is_complex()`).

Properties of the dependent network — modes, directedness, loops — must always be
respected in permutations and analysis;
one-mode and two-mode cases are branched on explicitly rather than projected away.
All `manynet`, `igraph`, `furrr`, and `future` calls use explicit `::` namespacing
(with per-file `@importFrom` roxygen tags for NAMESPACE generation).
Plot methods belong in `{autograph}`, not here.

### Parallelism

Every simulation-heavy function (`test_random()`, `test_configuration()`,
`test_permutation()`, `net_regression()`, `play_diffusions()`, …) takes:

- `times` — the number of simulations (default `1000`; 1,000–10,000 for publication).
- `strategy` — a `{future}` plan name (default `"sequential"`;
  `"multisession"`/`"multicore"` for multiple cores),
  set with `future::plan(strategy)` and restored via `on.exit()`.
- `verbose` — passed to `{furrr}`'s `.progress` to report progress.

Simulations are mapped with `furrr::future_map*()` using
`furrr::furrr_options(seed = TRUE)` for reproducible parallel RNG.
Follow this same signature when adding new simulation-based functions.

### Cross-package dependency

`migraph` `Depends` on pinned minimum versions of `{manynet}`, `{autograph}`, and
`{netrics}` (see `DESCRIPTION`).
CRAN and reverse-dependency issues are commonly resolved by bumping these minimum
versions rather than by changing `migraph`'s own code — see `cran-comments.md`.
`ergm` and `learnr` are heavier dependencies used by only a few functions;
guard such code paths with the `thisRequires()` helper in
[R/migraph-package.R](../R/migraph-package.R), which prompts to install on first use.

### Datasets

The `mpn_*` datasets accompany *Multimodal Political Networks*.
They are the part of this package that will remain after the inferential
functions move, so their conventions matter.

**Naming.** Lower case throughout, with no capitals:
`mpn_evs_ita`, not `mpn_IT`.
Group by source, then narrow: `mpn_evs_*`, `mpn_cow_*`, `mpn_elite_*`.
Use the ISO three-letter code for a country.

**Class.** Every dataset is a `stocnet` object.
Build one as a list with the slots `info`, `nodes`, `ties`, `changes`, `globals`,
and `missings` where there are unobserved dyads.

| Slot | Holds |
|---|---|
| `nodes$label` | the node name |
| `nodes$mode` | a **character** name for each set of nodes, two or more values |
| `ties$layer` | a character name for each tie type |
| `ties$time` | the observation time |
| `missings` | unobserved dyads, kept out of `ties` |
| `info` | provenance, written with `manynet::add_info()` |

Two traps when converting.
`manynet::as_stocnet()` writes `nodes$mode` as the strings `"FALSE"` and `"TRUE"`,
so rewrite it with real names.
It also drops the `grand` graph attribute, so restore the title with `add_info()`.

**Provenance.** Record what the source states, and nothing more.
`add_info()` recognises `name`, `modes`, `layers`, `directed`, `source`, `method`,
`location`, `date`, `boundary`, `observation`, `update`, and `doi`.
Do not invent a `method`, `boundary`, or `doi` that cannot be checked against
the documentation or the cited source.

**Missing data.** Record an unobserved dyad in `missings`, never as a zero tie
and never as a source's numeric code.
`mpn_cow_trade` once stored the Correlates of War code `-9` as a negative weight,
which made the network read as signed.

### Tests

This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

Tests in `tests/testthat/` mirror the `R/` files
(e.g. `test-model_tests.R`, `test-model_regression.R`, `test-measure_over.R`),
plus `test-tutorials_*.R` files that render `{migraph}`'s `{learnr}` tutorials and
evaluate their purled code, so tutorial code that errors or raises a
deprecation warning fails the suite.
[tests/testthat/helper-functions.R](../tests/testthat/helper-functions.R) provides
`find_pkg_tutorial_paths()`, `check_tute_rendering()`, and `check_tute_functions()`
for this, alongside the `top3()`/`bot3()`/`top5()`/`bot5()` helpers that pull rounded
top/bottom N values from a result for terse reference vectors in assertions.

`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`);
`Config/testthat/start-first` prioritises `tutorials_manynet`, since it is slow.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
function overview (`pkgdown/_pkgdown.yml` `reference:` titles).
Lead with `## Package` (package-wide/website/infrastructure changes),
then the function families in overview order:
`## Parallelism` (the `over_*()` functions), `## Simulating` (`play_*()` and diffusion),
`## Modelling` (the `test_*()` family, `net_regression()`, `predict()`,
and the `tidy()`/`glance()` methods).
Put `## Tutorials` and `## Data` near the end, so that they usually close the list.
Each heading appears at most once per version.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closing #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.
Cluster related changes (e.g. several fixes to the same function, or sub-points of one
feature) as indented sub-bullets under a lead bullet, to improve readability.
