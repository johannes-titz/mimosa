
[![Build
Status](https://travis-ci.org/johannes-titz/mimosa.svg?branch=master)](https://travis-ci.org/johannes-titz/mimosa)

Mimosa, the mixed models special agent is a shiny app for 2-level mixed
models.

# Installation

No need to install the app, just got to www.mimosa.icu and use it there.

If you really want to use it locally, open R and install the
dependencies:

``` r
install.packages(c("dplyr", "mlmRev", "lme4", "shiny", "shinyalert",
                   "shinydashboard", "shinyjs", "sjPlot"),
                 repos = "http://cran.us.r-project.org")
```

And now run the app:

``` r
shiny::runGitHub("mimosa", "johannes-titz")
```

Yes, it is that easy\!

<!-- For an introduction to mimosa please check out [@Titz2020]. -->

# Contributing

Contributions of any kind are very welcome\! I will sincerely consider
every suggestion on how to improve the internal code and the user
interface. Even minor things, such as suggestions for better wording or
improving grammar in any part of the package are considered as valuable
contributions. If you find any bugs, please use the issue tracker at:

<https://github.com/johannes-titz/mimosa/issues>

You can also drop me an e-mail and discuss suggestions at: johannes at
titz.science or johannes.titz at gmail.com

If you want to make a pull request, please run all tests in the
directory *tests* and check that there are no problems. Please also
follow the code style described here:
<http://r-pkgs.had.co.nz/r.html#style>

<!-- # References -->
