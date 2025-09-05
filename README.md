
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{CRimpact}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/AuroreAA/CRimpact/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AuroreAA/CRimpact/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

You can install the development version of `{CRimpact}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
CRimpact::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-09-05 17:28:41 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading CRimpact
#> Warning: remplacement de l'importation précédente 'DT::dataTableOutput' par
#> 'shiny::dataTableOutput' lors du chargement de 'CRimpact'
#> Warning: remplacement de l'importation précédente 'DT::renderDataTable' par
#> 'shiny::renderDataTable' lors du chargement de 'CRimpact'
#> ── R CMD check results ──────────────────────────────── CRimpact 0.0.0.9000 ────
#> Duration: 1m 16.4s
#> 
#> ❯ checking tests ...
#>   See below...
#> 
#> ❯ checking top-level files ... NOTE
#>   File
#>     LICENSE
#>   is not mentioned in the DESCRIPTION file.
#> 
#> ❯ checking package subdirectories ... NOTE
#>   Problems with news in 'NEWS.md':
#>   No news entries found.
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   bias_plot: no visible binding for global variable 'contamination'
#>   maxbias_plot: no visible binding for global variable 'contamination'
#>   mod_scale_server : <anonymous>: no visible binding for global variable
#>     'y'
#>   Undefined global functions or variables:
#>     contamination y
#> 
#> ❯ checking Rd contents ... NOTE
#>   Argument items with no description in Rd file 'get_custom_distribution.Rd':
#>     'session'
#> 
#> ── Test failures ───────────────────────────────────────────────── testthat ────
#> 
#> > # This file is part of the standard setup for testthat.
#> > # It is recommended that you do not modify it.
#> > #
#> > # Where should you do additional test configuration?
#> > # Learn more about the roles of various files in:
#> > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#> > # * https://testthat.r-lib.org/articles/special-files.html
#> > 
#> > library(testthat)
#> > library(CRimpact)
#> Warning messages:
#> 1: replacing previous import 'DT::dataTableOutput' by 'shiny::dataTableOutput' when loading 'CRimpact' 
#> 2: replacing previous import 'DT::renderDataTable' by 'shiny::renderDataTable' when loading 'CRimpact' 
#> > 
#> > test_check("CRimpact")
#> Loading required package: shiny
#> [ FAIL 1 | WARN 0 | SKIP 1 | PASS 108 ]
#> 
#> ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
#> • rlang_is_interactive() is not TRUE (1): 'test-golem-recommended.R:72:5'
#> 
#> ══ Failed tests ════════════════════════════════════════════════════════════════
#> ── Error ('test-mod_sidebar_bias.R:1:1'): (code run outside of `test_that()`) ──
#> <evalError/missingArgError/error/condition>
#> Error in `module(childScope$input, childScope$output, childScope, ...)`: argument "tab_name" is missing, with no default
#> Backtrace:
#>      ▆
#>   1. ├─shiny::testServer(...) at test-mod_sidebar_bias.R:1:1
#>   2. │ ├─shiny:::withMockContext(session, rlang::exec(app, !!!args))
#>   3. │ │ ├─shiny::isolate(...)
#>   4. │ │ │ ├─shiny::..stacktraceoff..(...)
#>   5. │ │ │ └─ctx$run(...)
#>   6. │ │ │   ├─promises::with_promise_domain(...)
#>   7. │ │ │   │ └─domain$wrapSync(expr)
#>   8. │ │ │   ├─shiny::withReactiveDomain(...)
#>   9. │ │ │   │ └─promises::with_promise_domain(...)
#>  10. │ │ │   │   └─domain$wrapSync(expr)
#>  11. │ │ │   │     └─base::force(expr)
#>  12. │ │ │   ├─shiny::captureStackTraces(...)
#>  13. │ │ │   │ └─promises::with_promise_domain(...)
#>  14. │ │ │   │   └─domain$wrapSync(expr)
#>  15. │ │ │   │     └─base::withCallingHandlers(expr, error = doCaptureStack)
#>  16. │ │ │   └─env$runWith(self, func)
#>  17. │ │ │     └─shiny (local) contextFunc()
#>  18. │ │ │       └─shiny::..stacktraceon..(expr)
#>  19. │ │ ├─shiny::withReactiveDomain(...)
#>  20. │ │ │ └─promises::with_promise_domain(...)
#>  21. │ │ │   └─domain$wrapSync(expr)
#>  22. │ │ │     └─base::force(expr)
#>  23. │ │ └─withr::with_options(...)
#>  24. │ │   └─base::force(code)
#>  25. │ ├─rlang::exec(app, !!!args)
#>  26. │ └─CRimpact (local) `<fn>`(id = "proxy1")
#>  27. │   └─shiny::moduleServer(...)
#>  28. │     ├─session$setReturned(callModule(module, id, session = session))
#>  29. │     └─shiny::callModule(module, id, session = session)
#>  30. │       ├─shiny::withReactiveDomain(...)
#>  31. │       │ └─promises::with_promise_domain(...)
#>  32. │       │   └─domain$wrapSync(expr)
#>  33. │       │     └─base::force(expr)
#>  34. │       └─CRimpact (local) module(childScope$input, childScope$output, childScope, ...)
#>  35. │         └─tab_name %in% c("crimpact")
#>  36. └─shiny (local) `<fn>`(`<evalErrr>`)
#> 
#> [ FAIL 1 | WARN 0 | SKIP 1 | PASS 108 ]
#> Error: Test failures
#> Execution halted
#> 
#> 1 error ✖ | 0 warnings ✔ | 4 notes ✖
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error: Failure in `C:/Users/a.archimbaud/AppData/Local/Temp/RtmpIH4pzP/R_LIBS60f84812463/CRimpact/CRimpact-tests/testthat.Rout.fail`
#> :force(expr)
#>  34. │       └─CRimpact (local) module(childScope$input, childScope$output, childScope, ...)
#>  35. │         └─tab_name %in% c("crimpact")
#>  36. └─shiny (local) `<fn>`(`<evalErrr>`)
#> 
#> [ FAIL 1 | WARN 0 | SKIP 1 | PASS 108 ]
#> Erreur : Test failures
#> Exécution arrêtée
```
