slowraker
================

> A slow version of the Rapid Automatic Keyword Extraction (RAKE) algorithm

[![Linux Build Status](https://travis-ci.org/crew102/slowraker.svg?branch=master)](https://travis-ci.org/crew102/slowraker) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/2ycx1m182va333ye?svg=true)](https://ci.appveyor.com/project/crew102/slowraker)

Installation
------------

``` r
if (!require(devtools)) install.packages("devtools")

devtools::install_github("crew102/slowraker")
```

Basic usage
-----------

There is one main function in the `slowraker` package, `slowrake()`. `slowrake()` will extract keywords from a vector of text using the RAKE algorithm. RAKE doesn't require any training data, so it's super easy to use:

``` r
library(slowraker)

data("dog_pubs")
rakelist <- slowrake(txt = dog_pubs$title[1:5])
```

`slowrake()` outputs a list of data frames, with each data frame containing the keywords that were extracted from a document:

``` r
rakelist
#> # A rakelist containing 5 data frames:
#>  $ :'data.frame':    4 obs. of  4 variables:
#>   ..$ keyword:"assistance dogs" ...
#>   ..$ freq   :1 1 ...
#>   ..$ score  :4 4 ...
#>   ..$ stem   :"assist dog" ...
#>  $ :'data.frame':    6 obs. of  4 variables:
#>   ..$ keyword:"guide dog owners perspectives" ...
#>   ..$ freq   :1 1 ...
#>   ..$ score  :13 ...
#>   ..$ stem   :"guid dog owner perspect" ...
#> #...With 3 more data frames.
```

We can bind these data frames together using `rbind_rakelist()`:

``` r
rakedf <- rbind_rakelist(rakelist = rakelist, doc_id = dog_pubs$doi[1:5])
head(rakedf, 5)
#>                         doc_id                       keyword freq    score                    stem
#> 1 10.1371/journal.pone.0132820               assistance dogs    1  4.00000              assist dog
#> 2 10.1371/journal.pone.0132820           identification tags    1  4.00000             identif tag
#> 3 10.1371/journal.pone.0132820                    california    1  1.00000              california
#> 4 10.1371/journal.pone.0132820                 registrations    1  1.00000                 registr
#> 5 10.1371/journal.pone.0176018 guide dog owners perspectives    1 13.33333 guid dog owner perspect
```
