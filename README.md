
[![Build
Status](https://travis-ci.org/johannes-titz/mimosa.svg?branch=master)](https://travis-ci.org/johannes-titz/mimosa)

Mimosa, the mixed models special agent is a shiny app for 2-level mixed
models.

# Installation

No need to install the app, just go to www.mimosa.icu and use it there.
An example data file is loaded when you go to www.mimosa.icu/example.

If you really want to use it locally, pull the development version from
github:

``` r
devtools::install_github("johannes-titz/mimosa")
```

And now run the app:

``` r
shiny::run_app()
```

Yes, it is that easy–at least under GNU/Linux\!

If you have any problems installing mimosa, check that your R version is
up to date (currently 3.6.1). If you are using Windows, enable TLS 1.2
in the Internet Options Advanced tab (see
<https://github.com/r-lib/remotes/issues/130#issuecomment-423830669>).

If it still does not work drop me an e-mail at johannes at titz.science
or at johannes.titz at gmail.com.

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

If you want to make a pull request, please check that you can build the
mimosa package without any errors, warnings and notes. Please also
follow the code style described here:
<http://r-pkgs.had.co.nz/r.html#style>. If I like the pull request, I
will run additional tests locally and if there are no problems, I will
accept the request.

<!-- # References -->
