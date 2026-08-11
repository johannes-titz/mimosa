# mimosa 0.5.9000

## Models and statistical output

- Dichotomous dependent variables are now detected automatically and fitted
  with `lme4::glmer(..., family = binomial())`. Numeric responses with more
  than two observed values continue to use Gaussian `lme4::lmer()` models.
  Mimosa explicitly reports which response value is modeled as the event.
- The output table now displays the empirical fixed-effect variance, each
  random-effect variance component, and the residual variance. Random-effect
  variances and correlations are placed next to their corresponding terms.
- Marginal and conditional R-squared and the intraclass correlation coefficient
  (ICC) now have detailed hover explanations showing the variance components
  and formulas used in their calculation.
- The variance calculations are regression tested against published Gaussian
  mixed-model examples from Nakagawa and Schielzeth (2013) and Johnson (2014).
  The Johnson two-level Orange example is available in the example selector,
  and the BeetlesBody data used for the published benchmark are included as a
  documented package data set.
- Singular fits, model-fitting errors, and cases with too few repeated groups
  now produce more useful diagnostics instead of failing silently.

## Interface and reproducibility

- A new **R analysis code** panel below the model display provides a complete
  copyable script for the current analysis. It includes reproducible data
  loading, the correct `lmer()` or `glmer()` call, binomial event recoding when
  needed, and `sjPlot::tab_model(model)`.
- Generated formulas and R code now quote non-syntactic variable names safely,
  wrap long lines for easier reading, and place the copy button below the code.
- The right-hand model/code panel and the save-output area were rearranged to
  avoid excess vertical whitespace.
- The installed Mimosa version is now shown in the application header.
- The example selector now includes additional two-level data sets from
  `mlmRev`, with descriptions of their nesting structure. The app remains
  intentionally limited to two-level models.
- Uploaded data handling and character-encoding fallbacks are more robust, and
  unsupported dependent variables are excluded. Dependent variables may be
  numeric or dichotomous factors, characters, or logical values.
- Grouping-variable detection now reports its reasoning, uses more robust
  repeated-measure thresholds, and warns when a proposed grouping variable has
  too many groups without repetition. `explain_find_id()` is exported for
  inspecting these diagnostics outside the app.

## Testing, deployment, and maintenance

- The automated suite now contains 266 tests with 97.7% line coverage. New
  headless `shiny::testServer()` tests exercise Gaussian and binomial models,
  generated R code, downloads, uploads, reactive error states, and output
  diagnostics on GitHub Actions; the browser-dependent `shinytest2` test is
  skipped on CI.
- GitHub Actions now performs multi-platform R CMD checks, generates and
  uploads Cobertura coverage, and uses current supported action versions.
- A Shinylive/webR build can run Mimosa entirely in the browser and is built
  and deployed to GitHub Pages automatically.
- Package metadata, generated documentation, styling, examples, the tutorial
  video links, and dependency declarations were refreshed. `insight` is now an
  explicit dependency for model variance calculations.

# v0.5.1

- improve uploading of files; if it does not succeed, an error is displayed
- improve error display in general: get rid of shinyalert, use modalDialog and showModal, req data in file upload, do not sanitize errors anymore

# v0.5.0

- use shinytest2 for tests
- allow to select examples from menu
- add popular2 as example data set
- add hox poular2 as test
- test loading of datasets without gui
- add github actions check and coverage with badges
- improve Dockerfile
- get rid of plyr as dependency
- some helper functions are now exported because they are useful even without the gui
- add port and host as parameters for run_app
- simplify link for school example in help
- remove safari check as it seems to work fine now
- clean up interface code

# v0.4.0

- upstream problem with p-values for unstandardized coefficients was fixed
- update tests for unstandardized coefficients (see last bullet point)
- add JOSS doi and citation to readme and shiny page

# v0.3.0
- this is the release for JOSS
- fix typos
- add Maria Reichert as contributor
- add acknowledgements
- add non-reactive mode
- add busy spinner and notifcation
- filter independent variables
- copy-edit paper

# v0.2.0

- finding the grouping variable now works more reliably (mimosa now also checks for the number of average levels, not only for the number of level-2 variables)
- documentation was added to non-exported functions, which should make extensions easier
- in Safari all file endings are shown, while in the other browsers only .sav and .csv are shown
- a help box at start of mimosa shows some useful links
- the readme now includes a proper introduction and example
- a summary paper was added
- several new tests were added: identifying the grouping variable, Exam data estimates from mlmRev vignette, formula display

# v0.1.1

- fix problem with interaction formula in model display
- fix problem with NA variable names in model display
- make R package structure runable at shinyapps.io
- add test for model display

# v0.1.0

First Release
