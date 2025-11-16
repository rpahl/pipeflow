---
title: "pipeflow benchmarks"
output:
  md_document:
    variant: gfm   # GitHub-friendly markdown
    preserve_yaml: true
---

Package version: pipeflow 0.2.3.9000

## Long linear pipelines

Benchmark results for long linear pipelines (in ms):

| expr    |  min | mean | median |  max | median/n |
|:--------|-----:|-----:|-------:|-----:|---------:|
| n = 32  |  187 |  196 |    192 |  221 |        6 |
| n = 64  |  631 |  655 |    659 |  670 |       10 |
| n = 128 | 2356 | 2422 |   2432 | 2477 |       19 |

## Session info

    # R version 4.5.1 (2025-06-13 ucrt)
    # Platform: x86_64-w64-mingw32/x64
    # Running under: Windows 10 x64 (build 19045)
    # 
    # Matrix products: default
    #   LAPACK version 3.12.1
    # 
    # locale:
    # [1] LC_COLLATE=English_Germany.utf8  LC_CTYPE=English_Germany.utf8   
    # [3] LC_MONETARY=English_Germany.utf8 LC_NUMERIC=C                    
    # [5] LC_TIME=English_Germany.utf8    
    # 
    # time zone: Europe/Berlin
    # tzcode source: internal
    # 
    # attached base packages:
    # [1] stats     graphics  grDevices utils     datasets  methods   base     
    # 
    # other attached packages:
    # [1] pipeflow_0.2.3.9000 testthat_3.2.3     
    # 
    # loaded via a namespace (and not attached):
    #  [1] generics_0.1.4       digest_0.6.37        magrittr_2.0.4      
    #  [4] evaluate_1.0.5       grid_4.5.1           RColorBrewer_1.1-3  
    #  [7] pkgload_1.4.1        fastmap_1.2.0        jsonlite_2.0.0      
    # [10] rprojroot_2.1.1      pkgbuild_1.4.8       sessioninfo_1.2.3   
    # [13] brio_1.1.5           urlchecker_1.0.1     promises_1.3.3      
    # [16] purrr_1.1.0          scales_1.4.0         microbenchmark_1.5.0
    # [19] cli_3.6.5            shiny_1.11.1         rlang_1.1.6         
    # [22] crayon_1.5.3         ellipsis_0.3.2       withr_3.0.2         
    # [25] remotes_2.5.0        cachem_1.1.0         yaml_2.3.10         
    # [28] devtools_2.4.5       tools_4.5.1          memoise_2.0.1       
    # [31] dplyr_1.1.4          ggplot2_4.0.0        httpuv_1.6.16       
    # [34] vctrs_0.6.5          R6_2.6.1             mime_0.13           
    # [37] lifecycle_1.0.4      fs_1.6.6             htmlwidgets_1.6.4   
    # [40] usethis_3.2.1        miniUI_0.1.2         pkgconfig_2.0.3     
    # [43] desc_1.4.3           pillar_1.11.1        later_1.4.4         
    # [46] gtable_0.3.6         data.table_1.17.8    glue_1.8.0          
    # [49] profvis_0.4.0        Rcpp_1.1.0           lgr_0.5.0           
    # [52] xfun_0.53            tibble_3.3.0         tidyselect_1.2.1    
    # [55] rstudioapi_0.17.1    knitr_1.50           farver_2.1.2        
    # [58] xtable_1.8-4         htmltools_0.5.8.1    rmarkdown_2.30      
    # [61] compiler_4.5.1       S7_0.2.0
