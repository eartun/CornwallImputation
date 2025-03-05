# Cornwall Petrophysical Dataset Imputation

The repository was created for the peer-reviewed publcation:


Input Files:

data.xlx: Main data file which includes measured petrophysical characteristics, including missing data, for 277 samples from different plutons
R Session Information: 
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: Asia/Dubai
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] yardstick_1.2.0    workflowsets_1.0.1 workflows_1.1.3    tune_1.1.2         rsample_1.2.0      recipes_1.0.8      parsnip_1.1.1     
 [8] modeldata_1.2.0    infer_1.0.5        dials_1.2.0        scales_1.3.0       broom_1.0.5        tidymodels_1.1.1   RColorBrewer_1.1-3
[15] vip_0.4.1          ggpubr_0.6.0       mice_3.16.0        lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4       
[22] purrr_1.0.2        readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       ggplot2_3.5.1      tidyverse_2.0.0    VIM_6.2.2         
[29] colorspace_2.1-0   readxl_1.4.3      

loaded via a namespace (and not attached):
 [1] rlang_1.1.2         magrittr_2.0.3      furrr_0.3.1         e1071_1.7-13        compiler_4.3.2      vctrs_0.6.4         lhs_1.1.6          
 [8] pkgconfig_2.0.3     shape_1.4.6         backports_1.4.1     utf8_1.2.4          prodlim_2023.08.28  tzdb_0.4.0          nloptr_2.0.3       
[15] glmnet_4.1-8        jomo_2.7-6          pan_1.9             parallel_4.3.2      R6_2.5.1            stringi_1.8.2       vcd_1.4-12         
[22] ranger_0.16.0       parallelly_1.36.0   car_3.1-2           boot_1.3-28.1       rpart_4.1.21        lmtest_0.9-40       cellranger_1.1.0   
[29] Rcpp_1.0.11         iterators_1.0.14    future.apply_1.11.0 zoo_1.8-12          Matrix_1.6-1.1      splines_4.3.2       nnet_7.3-19        
[36] timechange_0.2.0    tidyselect_1.2.0    rstudioapi_0.15.0   abind_1.4-5         timeDate_4022.108   codetools_0.2-19    listenv_0.9.0      
[43] lattice_0.21-9      withr_2.5.2         future_1.33.0       survival_3.5-7      proxy_0.4-27        pillar_1.9.0        carData_3.0-5      
[50] foreach_1.5.2       generics_0.1.3      sp_2.1-1            hms_1.1.3           munsell_0.5.0       laeken_0.5.3        minqa_1.2.6        
[57] globals_0.16.2      class_7.3-22        glue_1.6.2          tools_4.3.2         robustbase_0.99-2   data.table_1.14.8   lme4_1.1-35.1      
[64] gower_1.0.1         ggsignif_0.6.4      ipred_0.9-14        nlme_3.1-163        cli_3.6.1           DiceDesign_1.9      fansi_1.0.5        
[71] lava_1.7.3          gtable_0.3.4        DEoptimR_1.1-3      GPfit_1.0-8         rstatix_0.7.2       digest_0.6.33       lifecycle_1.0.4    
[78] hardhat_1.3.0       mitml_0.4-5         MASS_7.3-60    
