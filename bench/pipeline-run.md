---
title: "pipeflow benchmarks"
output:
  md_document:
    variant: gfm   # GitHub-friendly markdown
    preserve_yaml: true
---

Package version: pipeflow 0.2.3.9005

## Long linear pipelines

Benchmark results for long linear pipelines (in ms):

| expr    | min | mean | median | max | median/n |
|:--------|----:|-----:|-------:|----:|---------:|
| n = 32  |  42 |   55 |     54 |  70 |        2 |
| n = 64  |  90 |  102 |    101 | 124 |        2 |
| n = 128 | 170 |  196 |    193 | 239 |        2 |

## Session info

    # R version 4.5.2 (2025-10-31 ucrt)
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
    # [1] pipeflow_0.2.3.9005 testthat_3.3.2     
    # 
    # loaded via a namespace (and not attached):
    #  [1] crayon_1.5.3         vctrs_0.6.5          microbenchmark_1.5.0
    #  [4] cli_3.6.5            knitr_1.51           rlang_1.1.6         
    #  [7] xfun_0.55            otel_0.2.0           purrr_1.2.0         
    # [10] pkgload_1.4.1        jsonlite_2.0.0       data.table_1.18.0   
    # [13] glue_1.8.0           rprojroot_2.1.1      htmltools_0.5.9     
    # [16] pkgbuild_1.4.8       brio_1.1.5           rmarkdown_2.30      
    # [19] evaluate_1.0.5       ellipsis_0.3.2       fastmap_1.2.0       
    # [22] yaml_2.3.12          lifecycle_1.0.4      memoise_2.0.1       
    # [25] compiler_4.5.2       fs_1.6.6             sessioninfo_1.2.3   
    # [28] Rcpp_1.1.1           rstudioapi_0.18.0    digest_0.6.39       
    # [31] R6_2.6.1             usethis_3.2.1        magrittr_2.0.4      
    # [34] withr_3.0.2          tools_4.5.2          devtools_2.4.6      
    # [37] lgr_0.5.2            desc_1.4.3           cachem_1.1.0        
    # [40] remotes_2.5.0
