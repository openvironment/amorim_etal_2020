
# amorim\_etal\_2020

Data, R scripts and other files from the paper “Amorim et al., 2020”

# System info

    macOS Catalina - Version 10.15.1 (19B88)
    
    MacBook Pro 2017
    Processor: 2,3 GHz Dual-Core Intel Core i5
    Memory: 8 GB 2133 MHz LPDDR3

# R and packages versions used

``` r
devtools::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.6.1 (2019-07-05)
    ##  os       macOS Catalina 10.15.1      
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       America/Araguaina           
    ##  date     2020-04-05                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package      * version    date       lib source                              
    ##  assertthat     0.2.1      2019-03-21 [1] CRAN (R 3.6.0)                      
    ##  backports      1.1.5      2019-10-02 [1] CRAN (R 3.6.0)                      
    ##  broom          0.5.4      2020-01-27 [1] CRAN (R 3.6.0)                      
    ##  callr          3.3.2      2019-09-22 [1] CRAN (R 3.6.0)                      
    ##  caret        * 6.0-84     2019-08-13 [1] Github (topepo/caret@c96a978)       
    ##  cellranger     1.1.0      2016-07-27 [1] CRAN (R 3.6.0)                      
    ##  class          7.3-15     2019-01-01 [1] CRAN (R 3.6.1)                      
    ##  cli            2.0.1      2020-01-08 [1] CRAN (R 3.6.0)                      
    ##  codetools      0.2-16     2018-12-24 [1] CRAN (R 3.6.1)                      
    ##  colorspace     1.4-1      2019-03-18 [1] CRAN (R 3.6.0)                      
    ##  crayon         1.3.4      2017-09-16 [1] CRAN (R 3.6.0)                      
    ##  data.table     1.12.2     2019-04-07 [1] CRAN (R 3.6.0)                      
    ##  desc           1.2.0      2018-05-01 [1] CRAN (R 3.6.0)                      
    ##  devtools       2.2.1      2019-09-24 [1] CRAN (R 3.6.0)                      
    ##  dials          0.0.4      2019-12-02 [1] CRAN (R 3.6.0)                      
    ##  DiceDesign     1.8-1      2019-07-31 [1] CRAN (R 3.6.0)                      
    ##  digest         0.6.22     2019-10-21 [1] CRAN (R 3.6.0)                      
    ##  dplyr        * 0.8.4      2020-01-31 [1] CRAN (R 3.6.0)                      
    ##  ellipsis       0.3.0      2019-09-20 [1] CRAN (R 3.6.0)                      
    ##  evaluate       0.14       2019-05-28 [1] CRAN (R 3.6.0)                      
    ##  fansi          0.4.0      2018-10-05 [1] CRAN (R 3.6.0)                      
    ##  forcats      * 0.4.0      2019-02-17 [1] CRAN (R 3.6.0)                      
    ##  foreach        1.4.7      2019-07-27 [1] CRAN (R 3.6.0)                      
    ##  fs             1.3.1      2019-05-06 [1] CRAN (R 3.6.0)                      
    ##  generics       0.0.2      2018-11-29 [1] CRAN (R 3.6.0)                      
    ##  ggplot2      * 3.2.1      2019-08-10 [1] CRAN (R 3.6.0)                      
    ##  glue           1.3.1      2019-03-12 [1] CRAN (R 3.6.0)                      
    ##  gower          0.2.1      2019-05-14 [1] CRAN (R 3.6.0)                      
    ##  GPfit          1.0-8      2019-02-08 [1] CRAN (R 3.6.0)                      
    ##  gtable         0.3.0      2019-03-25 [1] CRAN (R 3.6.0)                      
    ##  haven          2.1.1      2019-07-04 [1] CRAN (R 3.6.0)                      
    ##  hms            0.5.2      2019-10-30 [1] CRAN (R 3.6.0)                      
    ##  htmltools      0.4.0      2019-10-04 [1] CRAN (R 3.6.0)                      
    ##  httr           1.4.1      2019-08-05 [1] CRAN (R 3.6.0)                      
    ##  ipred          0.9-9      2019-04-28 [1] CRAN (R 3.6.0)                      
    ##  iterators      1.0.12     2019-07-26 [1] CRAN (R 3.6.0)                      
    ##  jsonlite       1.6        2018-12-07 [1] CRAN (R 3.6.0)                      
    ##  knitr          1.28       2020-02-06 [1] CRAN (R 3.6.0)                      
    ##  lattice      * 0.20-38    2018-11-04 [1] CRAN (R 3.6.1)                      
    ##  lava           1.6.6      2019-08-01 [1] CRAN (R 3.6.0)                      
    ##  lazyeval       0.2.2      2019-03-15 [1] CRAN (R 3.6.0)                      
    ##  lhs            1.0.1      2019-02-03 [1] CRAN (R 3.6.0)                      
    ##  lifecycle      0.1.0      2019-08-01 [1] CRAN (R 3.6.0)                      
    ##  lubridate      1.7.4      2018-04-11 [1] CRAN (R 3.6.0)                      
    ##  magrittr       1.5        2014-11-22 [1] CRAN (R 3.6.0)                      
    ##  MASS           7.3-51.4   2019-03-31 [1] CRAN (R 3.6.1)                      
    ##  Matrix         1.2-17     2019-03-22 [1] CRAN (R 3.6.1)                      
    ##  memoise        1.1.0      2017-04-21 [1] CRAN (R 3.6.0)                      
    ##  mgcv         * 1.8-29     2019-09-20 [1] CRAN (R 3.6.0)                      
    ##  ModelMetrics   1.2.2      2018-11-03 [1] CRAN (R 3.6.0)                      
    ##  modelr         0.1.5      2019-08-08 [1] CRAN (R 3.6.0)                      
    ##  munsell        0.5.0      2018-06-12 [1] CRAN (R 3.6.0)                      
    ##  nlme         * 3.1-141    2019-08-01 [1] CRAN (R 3.6.0)                      
    ##  nnet           7.3-12     2016-02-02 [1] CRAN (R 3.6.1)                      
    ##  parsnip        0.0.5      2020-01-07 [1] CRAN (R 3.6.0)                      
    ##  patchwork    * 0.0.1      2019-08-13 [1] Github (thomasp85/patchwork@fd7958b)
    ##  pillar         1.4.3      2019-12-20 [1] CRAN (R 3.6.0)                      
    ##  pkgbuild       1.0.5      2019-08-26 [1] CRAN (R 3.6.0)                      
    ##  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 3.6.0)                      
    ##  pkgload        1.0.2      2018-10-29 [1] CRAN (R 3.6.0)                      
    ##  plyr           1.8.4      2016-06-08 [1] CRAN (R 3.6.0)                      
    ##  prettyunits    1.0.2      2015-07-13 [1] CRAN (R 3.6.0)                      
    ##  processx       3.4.1      2019-07-18 [1] CRAN (R 3.6.0)                      
    ##  prodlim        2018.04.18 2018-04-18 [1] CRAN (R 3.6.0)                      
    ##  ps             1.3.0      2018-12-21 [1] CRAN (R 3.6.0)                      
    ##  purrr        * 0.3.3      2019-10-18 [1] CRAN (R 3.6.0)                      
    ##  R6             2.4.1      2019-11-12 [1] CRAN (R 3.6.0)                      
    ##  Rcpp           1.0.3      2019-11-08 [1] CRAN (R 3.6.0)                      
    ##  readr        * 1.3.1      2018-12-21 [1] CRAN (R 3.6.0)                      
    ##  readxl         1.3.1      2019-03-13 [1] CRAN (R 3.6.0)                      
    ##  recipes      * 0.1.9      2020-01-07 [1] CRAN (R 3.6.0)                      
    ##  remotes        2.1.0      2019-06-24 [1] CRAN (R 3.6.0)                      
    ##  reshape2       1.4.3      2017-12-11 [1] CRAN (R 3.6.0)                      
    ##  rlang          0.4.5      2020-03-01 [1] CRAN (R 3.6.0)                      
    ##  rmarkdown      1.16       2019-10-01 [1] CRAN (R 3.6.0)                      
    ##  rpart          4.1-15     2019-04-12 [1] CRAN (R 3.6.1)                      
    ##  rprojroot      1.3-2      2018-01-03 [1] CRAN (R 3.6.0)                      
    ##  rstudioapi     0.11       2020-02-07 [1] CRAN (R 3.6.0)                      
    ##  rvest          0.3.5      2019-11-08 [1] CRAN (R 3.6.0)                      
    ##  scales         1.0.0      2018-08-09 [1] CRAN (R 3.6.0)                      
    ##  sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 3.6.0)                      
    ##  stringi        1.4.3      2019-03-12 [1] CRAN (R 3.6.0)                      
    ##  stringr      * 1.4.0      2019-02-10 [1] CRAN (R 3.6.0)                      
    ##  survival       2.44-1.1   2019-04-01 [1] CRAN (R 3.6.1)                      
    ##  testthat       2.2.1      2019-07-25 [1] CRAN (R 3.6.0)                      
    ##  tibble       * 2.1.3      2019-06-06 [1] CRAN (R 3.6.0)                      
    ##  tidyr        * 1.0.2      2020-01-24 [1] CRAN (R 3.6.0)                      
    ##  tidyselect     0.2.5      2018-10-11 [1] CRAN (R 3.6.0)                      
    ##  tidyverse    * 1.2.1      2017-11-14 [1] CRAN (R 3.6.0)                      
    ##  timeDate       3043.102   2018-02-21 [1] CRAN (R 3.6.0)                      
    ##  usethis        1.5.1      2019-07-04 [1] CRAN (R 3.6.0)                      
    ##  vctrs          0.2.4      2020-03-10 [1] CRAN (R 3.6.0)                      
    ##  withr          2.1.2      2018-03-15 [1] CRAN (R 3.6.0)                      
    ##  workflows      0.1.0      2019-12-30 [1] CRAN (R 3.6.0)                      
    ##  xfun           0.10       2019-10-01 [1] CRAN (R 3.6.0)                      
    ##  xml2           1.2.2      2019-08-09 [1] CRAN (R 3.6.0)                      
    ##  yaml           2.2.0      2018-07-25 [1] CRAN (R 3.6.0)                      
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
