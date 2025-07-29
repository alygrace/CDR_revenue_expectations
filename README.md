# Replication package for: "A global analysis of expected revenues from carbon dioxide removal"

This repository contains the R scripts used to generate the figures included in the manuscript submission. 
Data used in Figure 1 cannot be shared due to participant privacy constraints. Data supporting Figures 2, 4, and 5 will be made available upon reasonable request. Data for Figure 3 are available from the following sources: historical EU ETS prices from the World Bank Carbon Pricing Dashboard (https://carbonpricingdashboard.worldbank.org/), EU ETS forecasts from the Ariadne Project (https://ariadneprojekt.de/), and cost data from the literature cited. 

Each script is designed to create a specific figure. Additionally, there is a dedicated script for generating Figure 1 and Table 2 from the Supplementary Information (SI).

Note: Please set the working directory to the root of this repository before running any scripts.

### License
This replication package is licensed under the MIT License. 

### Dependencies
The script relies on several R packages, which are all loaded at the beginning of the script. You can install these packages using:
````
install.packages(c("tidyverse", "dplyr", "stringr", "patchwork", "ggtext", "cowplot", "rnaturalearthdata", "rnaturalearthhires", "rnaturalearth", "sf", "scales", "svglite", "ggrepel", "knitr", "maps"))
````

Package versions used and other information available through R's sessionInfo() function are displayed below. RStudio v2023.12.1 was used.

````
R version 4.3.3 (2024-02-29 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: Europe/Zurich
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] knitr_1.45                    ggrepel_0.9.5                 scales_1.4.0                 
 [4] sf_1.0-15                     rnaturalearth_1.0.1           rnaturalearthhires_1.0.0.9000
 [7] rnaturalearthdata_1.0.0       renv_1.1.5                    maps_3.4.2                   
[10] patchwork_1.2.0               pacman_0.5.1                  svglite_2.1.3                
[13] cowplot_1.1.3                 ggtext_0.1.2                  lubridate_1.9.3              
[16] forcats_1.0.0                 stringr_1.5.1                 dplyr_1.1.4                  
[19] purrr_1.0.2                   readr_2.1.5                   tidyr_1.3.1                  
[22] tibble_3.2.1                  ggplot2_3.5.0                 tidyverse_2.0.0              

loaded via a namespace (and not attached):
 [1] gtable_0.3.4       xfun_0.42          tzdb_0.4.0         vctrs_0.6.5       
 [5] tools_4.3.3        generics_0.1.3     parallel_4.3.3     proxy_0.4-27      
 [9] fansi_1.0.6        pkgconfig_2.0.3    KernSmooth_2.23-22 RColorBrewer_1.1-3
[13] lifecycle_1.0.4    compiler_4.3.3     farver_2.1.1       terra_1.7-71      
[17] codetools_0.2-19   class_7.3-22       pillar_1.9.0       crayon_1.5.2      
[21] classInt_0.4-10    tidyselect_1.2.0   stringi_1.8.3      grid_4.3.3        
[25] cli_3.6.2          magrittr_2.0.3     utf8_1.2.4         e1071_1.7-14      
[29] withr_3.0.0        bit64_4.0.5        timechange_0.3.0   httr_1.4.7        
[33] bit_4.0.5          hms_1.1.3          rlang_1.1.3        gridtext_0.1.5    
[37] Rcpp_1.0.12        glue_1.7.0         DBI_1.2.2          xml2_1.3.6        
[41] rstudioapi_0.15.0  vroom_1.6.5        jsonlite_1.8.8     R6_2.5.1          
[45] systemfonts_1.0.5  units_0.8-5  
````

### Installing R and RStudio
Before running the script, you will need to have R (and, optionally, RStudio) installed on your system.

Installing R
Visit the Comprehensive R Archive Network (CRAN) at https://cran.r-project.org and choose the version appropriate for your operating system, then follow the instructions there for installation.

Installing RStudio Desktop
Visit the download page for RStudio Desktop at https://posit.co/download/rstudio-desktop and download the free RStudio Desktop installer for your operating system, then follow the installation instructions.
