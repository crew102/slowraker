slowraker
================

> A slow version of the Rapid Automatic Keyword Extraction (RAKE) algorithm

[![Linux Build Status](https://travis-ci.org/crew102/slowraker.svg?branch=master)](https://travis-ci.org/crew102/slowraker) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/2ycx1m182va333ye?svg=true)](https://ci.appveyor.com/project/crew102/slowraker)

Installation
------------

You can get the stable version from CRAN:

``` r
install.packages("slowraker")
```

Or the development version from GitHub:

``` r
if (!require(devtools)) install.packages("devtools")

devtools::install_github("crew102/slowraker")
```

Basic usage
-----------

There is one main function in the `slowraker` package, `slowrake()`. `slowrake()` will extract keywords from a vector of documents using the RAKE algorithm. RAKE doesn't require any training data, so it's super easy to use:

``` r
library(slowraker)

data("dog_pubs")
rakelist <- slowrake(txt = dog_pubs$abstract[1:5])
```

`slowrake()` outputs a list of data frames. Each data frame contains the keywords that were extracted for an element of `txt`:

``` r
rakelist
#> 
#> # A rakelist containing 5 data frames:
#>  $ :'data.frame':    61 obs. of  4 variables:
#>   ..$ keyword:"assistance dog identification tags" ...
#>   ..$ freq   :1 1 ...
#>   ..$ score  :11 ...
#>   ..$ stem   :"assist dog identif tag" ...
#>  $ :'data.frame':    90 obs. of  4 variables:
#>   ..$ keyword:"current dog suitability assessments focus" ...
#>   ..$ freq   :1 1 ...
#>   ..$ score  :21 ...
#>   ..$ stem   :"current dog suitabl assess focu" ...
#> #...With 3 more data frames.
```

You can bind these data frames together using `rbind_rakelist()`:

``` r
rakedf <- rbind_rakelist(rakelist = rakelist, doc_id = dog_pubs$doi[1:5])
head(rakedf, 5)
#>                         doc_id                            keyword freq score
#> 1 10.1371/journal.pone.0132820 assistance dog identification tags    1  10.8
#> 2 10.1371/journal.pone.0132820          animal control facilities    1   9.0
#> 3 10.1371/journal.pone.0132820          emotional support animals    1   9.0
#> 4 10.1371/journal.pone.0132820                   small body sizes    1   9.0
#> 5 10.1371/journal.pone.0132820       seemingly inappropriate dogs    1   7.9
#>                       stem
#> 1   assist dog identif tag
#> 2       anim control facil
#> 3        emot support anim
#> 4          small bodi size
#> 5 seemingli inappropri dog
```

Learning more
-------------

-   To learn about how the RAKE algorithm works as well as the basics of `slowrake()`, check out the "Getting started" vignette (`vignette("getting-started")`). Frequently asked questions are answered in the FAQs vignette (`vignette("faqs")`).
-   All documentation is also on the package's [website](https://crew102.github.io/slowraker/index.html)
