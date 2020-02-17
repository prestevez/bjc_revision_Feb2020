---
title: "Extortion in Mexico: Who pays and why?"
author: "Patricio R. Estevez Soto"
email: "patricio.estevez.14@ucl.ac.uk"
date: "05/05/2018"
output:
  md_document:
    variant: "markdown"
pandoc_args: "--smart"
---



# Introduction

This document contains the script and results for a research project on extortion against businesses in Mexico.

Previous research has found that extortion against businesses is acutely concentrated on a few businesses who experience more than half of all extortion incidents in the country as repeat extortion victimizations. That project found that the rate at which businesses suffer extortion is mostly determined by their characteristics---such as their age, whether they are a restaurant, the number of corruption incidents they have suffered, and their size---and to a lesser extent, to the characteristics of the state where they are located---which include the state homicide rate and other unmeasured between-state differences.

Nonetheless, that research did not consider that most extortion victims do not comply with extortion demands. Thus this project aims to explore the distribution of successful extortion incidents, and identify the incident, victim and area characteristics associated with a higher likelihood of complying with extortion.

The incident-level characteristics that will be explored are:

- month
- time of day
- number of offenders
- victim's relationship to the offender
- use of a weapon
- type of weapon
- use of violence
- whether the incident was reported to the authorities (MP + other)
- type of extortion
- what was requested
- retaliation against business for not complying
- whether the victim complied with the extortion

The victim-level characteristics are:
- business type
- business age
- business size
- Victim of corruption
- number of corruption victimizations, repeat corruption victim
- number of extortion victimizations
- repeat extortion victim

Area-level characteristics:
- State
- State homicides
- State population (control)
Add more stuff here

# Set up, data input and pre-process

## Session info

We first check details of the session and system, and for reproducibility, we set the random seed.


```r
starttime <- proc.time()
date()
```

```
[1] "Mon Feb 17 11:50:13 2020"
```

```r
sessionInfo()
```

```
R version 3.6.2 (2019-12-12)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 10 (buster)

Matrix products: default
BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C              LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] arsenal_3.3.0     lmtest_0.9-37     zoo_1.8-7         sandwich_2.5-1    downloader_0.4    forcats_0.4.0    
 [7] stringr_1.4.0     dplyr_0.8.4       purrr_0.3.3       readr_1.3.1       tidyr_1.0.2       tibble_2.1.3     
[13] ggplot2_3.2.1     tidyverse_1.3.0   victim_0.0.0.9000

loaded via a namespace (and not attached):
 [1] httr_1.4.1         splines_3.6.2      jsonlite_1.6.1     carData_3.0-3      modelr_0.1.5      
 [6] assertthat_0.2.1   highr_0.8          cellranger_1.1.0   yaml_2.2.1         pillar_1.4.3      
[11] backports_1.1.5    lattice_0.20-38    glue_1.3.1         digest_0.6.24      rvest_0.3.5       
[16] colorspace_1.4-1   Matrix_1.2-18      htmltools_0.4.0    plyr_1.8.5         pkgconfig_2.0.3   
[21] broom_0.5.4        haven_2.2.0        scales_1.1.0       openxlsx_4.1.4     rio_0.5.16        
[26] generics_0.0.2     car_3.0-6          withr_2.1.2        lazyeval_0.2.2     cli_2.0.1         
[31] survival_3.1-8     magrittr_1.5       crayon_1.3.4       readxl_1.3.1       evaluate_0.14     
[36] fs_1.3.1           fansi_0.4.1        nlme_3.1-142       MASS_7.3-51.4      xml2_1.2.2        
[41] foreign_0.8-72     class_7.3-15       tools_3.6.2        data.table_1.12.8  hms_0.5.3         
[46] lifecycle_0.1.0    munsell_0.5.0      reprex_0.3.0       zip_2.0.4          compiler_3.6.2    
[51] e1071_1.7-3        rlang_0.4.4        classInt_0.4-2     grid_3.6.2         rstudioapi_0.11   
[56] rmarkdown_2.1      gtable_0.3.0       codetools_0.2-16   abind_1.4-5        DBI_1.1.0         
[61] curl_4.3           reshape2_1.4.3     R6_2.4.1           lubridate_1.7.4    knitr_1.28        
[66] utf8_1.1.4         KernSmooth_2.23-16 stringi_1.4.5      Rcpp_1.0.3         vctrs_0.2.2       
[71] dbplyr_1.4.2       tidyselect_1.0.0   xfun_0.12         
```

```r
set.seed(42)
options(scipen=0)
```

## Load packages and functions

Next we load the packages that we will use.


```r
#library(Cairo)
#library(glmmTMB)
library(victim)
library(tidyverse)
library(downloader)
library(sandwich)
library(lmtest)
library(arsenal)

read.dbf <- foreign::read.dbf

kable <- knitr::kable
melt <- reshape2::melt
select <- dplyr::select
Anova <- car::Anova

sessionInfo()
```

```
R version 3.6.2 (2019-12-12)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 10 (buster)

Matrix products: default
BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C              LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] arsenal_3.3.0     lmtest_0.9-37     zoo_1.8-7         sandwich_2.5-1    downloader_0.4    forcats_0.4.0    
 [7] stringr_1.4.0     dplyr_0.8.4       purrr_0.3.3       readr_1.3.1       tidyr_1.0.2       tibble_2.1.3     
[13] ggplot2_3.2.1     tidyverse_1.3.0   victim_0.0.0.9000

loaded via a namespace (and not attached):
 [1] httr_1.4.1         splines_3.6.2      jsonlite_1.6.1     carData_3.0-3      modelr_0.1.5      
 [6] assertthat_0.2.1   highr_0.8          cellranger_1.1.0   yaml_2.2.1         pillar_1.4.3      
[11] backports_1.1.5    lattice_0.20-38    glue_1.3.1         digest_0.6.24      rvest_0.3.5       
[16] colorspace_1.4-1   Matrix_1.2-18      htmltools_0.4.0    plyr_1.8.5         pkgconfig_2.0.3   
[21] broom_0.5.4        haven_2.2.0        scales_1.1.0       openxlsx_4.1.4     rio_0.5.16        
[26] generics_0.0.2     car_3.0-6          withr_2.1.2        lazyeval_0.2.2     cli_2.0.1         
[31] survival_3.1-8     magrittr_1.5       crayon_1.3.4       readxl_1.3.1       evaluate_0.14     
[36] fs_1.3.1           fansi_0.4.1        nlme_3.1-142       MASS_7.3-51.4      xml2_1.2.2        
[41] foreign_0.8-72     class_7.3-15       tools_3.6.2        data.table_1.12.8  hms_0.5.3         
[46] lifecycle_0.1.0    munsell_0.5.0      reprex_0.3.0       zip_2.0.4          compiler_3.6.2    
[51] e1071_1.7-3        rlang_0.4.4        classInt_0.4-2     grid_3.6.2         rstudioapi_0.11   
[56] rmarkdown_2.1      gtable_0.3.0       codetools_0.2-16   abind_1.4-5        DBI_1.1.0         
[61] curl_4.3           reshape2_1.4.3     R6_2.4.1           lubridate_1.7.4    knitr_1.28        
[66] utf8_1.1.4         KernSmooth_2.23-16 stringi_1.4.5      Rcpp_1.0.3         vctrs_0.2.2       
[71] dbplyr_1.4.2       tidyselect_1.0.0   xfun_0.12         
```


Load custom functions


```r
mylog <- function(x, center = FALSE){
    if(min(x) <= 0) {tlog <- log1p}
    else {tlog <- log}

    logx <- tlog(x)

    if(isTRUE(center))
    {
        logx <- logx - tlog(mean(x))
    }

    if(is.numeric(center)){
        logx <- logx - tlog(center)
    }

    return(logx)

}

vif <- function(form, data, family = "binomial"){
    if(!is_formula(form)) {form <- formula(form)}

    lmmod <- glm(form, data = data,  family = family)

    vifs <- car::vif(lmmod)

    return(vifs)
}

add_stars <- function(pval, alpha = c(0.001, 0.01, 0.05))
{
    ifelse(pval < alpha[1], "***", ifelse(pval < alpha[2], "**",
        ifelse(pval < alpha[3], "*", "")))
}

summaryCL <- function(m, clusterid, conf.level = 0.95, boots = FALSE, R = 400, ...){
    if(isFALSE(boots))
    {
        robust_vcov <- sandwich::vcovCL(m, cluster = clusterid)
    } else
    {
        robust_vcov <- sandwich::vcovBS(m, cluster = clusterid, R, ...)
    }

    rob_coef <- lmtest::coeftest(m, vcov. = robust_vcov)
    rob_confint <- lmtest::coefci(m, level = conf.level, vcov. = robust_vcov)

    results <- cbind(rob_coef, rob_confint)
    results <- data.frame(results)

    names(results) <- c("estimate", "SE", "z.value", "p.value", "ci.low", "ci.high")

    results$sig <- add_stars(results[,4])

    waldt <- lmtest::waldtest(m, vcov = robust_vcov, test = "Chisq")

    results_list <- list(coefs = results, clusterid = clusterid, wald = waldt, vcov = robust_vcov)

    class(results_list) <- "summaryCL"

    return(results_list)

}


print.summaryCL <- function(summaryCL.object){
    print(summaryCL.object[-4])
}

vcov.summaryCL <- function(summaryCL.object){
    summaryCL.object$vcov
}


# predictive accuracy function

accuracy <- function(mod){

    nm <- mod$model

    # Naive Guess
    t1 <- table(nm[,1])
    pt1 <- prop.table(t1)
    ng <- round(max(pt1) * 100, 3)

    # Predictive accuracy
    nm$predicted <- round(predict(mod,type = "response"))

    confusion <- table(nm[,1], nm$predicted)

    predaccu <- round(sum(diag(confusion)) / sum(confusion) * 100, 3)

    return(list(accuracy = predaccu, naive = ng))

}

mysummary <- function(m){
    print(summary(m))
    print(car::vif(m))
    print(logLik(m))
    print(waldtest(m, test = "Chisq"))
    print(accuracy(m))
    print(car::Anova(m))
}

mkbin <- function(x,n = 0) ifelse(x > n, 1, 0)
```

## Load data

We first load and arrange the area and victim level data


```r
### Change if not in testing settings
#params <- list(test = TRUE)
if(params$test){
    download("https://raw.githubusercontent.com/prestevez/datahouse/master/enve2014cuest_ciega_2014.dbf",
                      destfile = "enve2014cuest_ciega_2014.dbf", mode = "wb")
    download("https://raw.githubusercontent.com/prestevez/datahouse/master/enve2014delitos_ciega_2014.dbf",
                 destfile = "enve2014delitos_ciega_2014.dbf", mode = "wb")
}
```

```
Error in eval(expr, envir, enclos): object 'params' not found
```

```r
list.files()
```

```
[1] "BJC_compliance_revision2020.html" "BJC_compliance_revision2020.md"   "BJC_compliance_revision2020.Rmd" 
[4] "cache"                            "enve2014cuest_ciega_2014.dbf"     "enve2014delitos_ciega_2014.dbf"  
```



```r
cat_entidades <- read.csv("https://raw.githubusercontent.com/prestevez/datahouse/master/cat_entidades.csv", head=TRUE)
state_level_data <- read.csv("https://raw.githubusercontent.com/prestevez/datahouse/master/state_level_data_2013.csv", header=TRUE)
state_level_data <- merge(state_level_data,
                          cat_entidades, by="CVE_ENT", all.x=TRUE)
scode <- read.csv("https://raw.githubusercontent.com/prestevez/datahouse/master/secode.csv", head=TRUE)
scode$Code <- scode$Code*10000


enve_all <- read.dbf("enve2014cuest_ciega_2014.dbf")



# Prepare data for analysis
# Selecting only the relevant variables
enve_test <- data.frame(extortions=as.integer(as.character(enve_all$P26_10)))
enve_test$extortion_victim <- enve_all$P25_10
enve_test$extortions[enve_test$extortion_victim == 2] <- 0
summary(enve_test$extortions)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5808  0.0000 33.0000      50 
```

```r
enve_test$extortions[is.na(enve_test$extortions)] <- 0

summary(enve_test$extortions)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.5692  0.0000 33.0000 
```

```r
table(enve_test$extortions)
```

```

   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   19   20   21   22   23   25 
2203  109   46   32   25   19    8    5    4    5    4    2    5    4    6    5    2    4    2    1    2    1 
  27   29   32   33 
   1    1    3    1 
```

```r
enve_test$rep_extortion_victim <- mkbin(enve_test$extortions, 1)

enve_test$rep_extortions <- enve_test$extortions
enve_test$rep_extortions[enve_test$rep_extortions > 0] <- enve_test$rep_extortions[enve_test$rep_extortions > 0] - 1

summary(enve_test$rep_extortions)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.0000  0.0000  0.4504  0.0000 32.0000 
```

```r
table(enve_test$rep_extortions)
```

```

   0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   18   19   20   21   22   24   26 
2312   46   32   25   19    8    5    4    5    4    2    5    4    6    5    2    4    2    1    2    1    1 
  28   31   32 
   1    3    1 
```

```r
enve_test$CVE_UNICA <- as.integer(as.character(enve_all$ID_CONSECU))

enve_test$bribes <- as.integer(as.character(enve_all$P33))
summary(enve_test$bribes)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.486   3.000  16.000    2218 
```

```r
# 4 bribe cats
enve_test$bribe1 <- enve_all$P29_1
enve_test$bribe2 <- enve_all$P30_1
enve_test$bribe3 <- enve_all$P31_1
enve_test$bribe4 <- enve_all$P32_1

enve_test$bribes[with(enve_test,
                        bribe1 == 2 &
                        bribe2 == 2 &
                        bribe3 == 2 &
                        bribe4 == 2)] <- 0

summary(enve_test$bribes)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2861  0.0000 16.0000      50 
```

```r
enve_test$bribes[is.na(enve_test$bribes)] <- 0

enve_test$bribe_victim <- mkbin(enve_test$bribes, 0)

table(enve_test$bribe_victim)
```

```

   0    1 
2218  282 
```

```r
enve_test$rep_bribe <- mkbin(enve_test$bribes, 1)

table(enve_test$rep_bribe)
```

```

   0    1 
2353  147 
```

```r
enve_test$bribe_cats <- factor(enve_test$bribes)
levels(enve_test$bribe_cats) <- c(0, 1, 2, rep("3+",
                                            length(levels(enve_test$bribe_cats)) - 3))
summary(enve_test$bribe_cats)
```

```
   0    1    2   3+ 
2218  135   54   93 
```

```r
enve_test$CVE_ENT <- as.integer(as.character(enve_all$CVE_ENT))

enve_test$size <- enve_all$ID_ESTRATO
levels(enve_test$size) <- c("Large", "Medium", "Small", "Micro")

enve_test$sector <- enve_all$SECTOR_FIN

# subsector
enve_test$tempsub <- as.integer(as.character(enve_all$P1_1B))
enve_test$subsector <- cut(enve_test$tempsub, scode$Code, right=FALSE)
levels(enve_test$subsector) <- scode$Sector
enve_test$subsector <- droplevels(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)
```

```
 [1] "Retail"         "Mining"         "Utilities"      "Construction"   "Manufacturing"  "Wholesale"     
 [7] "Transport"      "Media"          "Finance"        "Real estate"    "Prof. services" "Corporate"     
[13] "Maintenance"    "Education"      "Health"         "Leisure"        "HotelsRestBar"  "Other"         
```

```r
enve_test$subsector_safe <- enve_test$subsector

enve_test$subsector <- as.character(enve_test$subsector)

# Must remember to exclude utilities from analysis

enve_test$subsector[enve_test$subsector %in%
                      c("Mining",
                        "Construction")] <- "Other industry"

# Must remember to exclude Corporate from analysis

enve_test$subsector[enve_test$subsector %in%
                      c("Media",
                        "Maintenance",
                        "Other",
                        "Finance",
                        "Health",
                        "Leisure",
                        "Education",
                        "Prof. services",
                        "Real estate")] <- "Other serv."


enve_test$subsector <- as.factor(enve_test$subsector)
enve_test$subsector <- relevel(enve_test$subsector, ref="Retail")
levels(enve_test$subsector)
```

```
[1] "Retail"         "Corporate"      "HotelsRestBar"  "Manufacturing"  "Other industry" "Other serv."   
[7] "Transport"      "Utilities"      "Wholesale"     
```

```r
summary(enve_test$subsector)
```

```
        Retail      Corporate  HotelsRestBar  Manufacturing Other industry    Other serv.      Transport 
            82             50            367            484            386            843            135 
     Utilities      Wholesale 
            38            115 
```

```r
enve_test$years <- 2013 - as.numeric(as.character(enve_all$P3))
summary(enve_test$years)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   10.00   21.00   20.94   31.00   43.00 
```

```r
intyears <- classInt::classIntervals(enve_test$years, 5, style="quantile")
enve_test$yearsquant <- cut(enve_test$years, intyears$brks, right=TRUE,
                            include.lowest = TRUE)

enve_test <- merge(enve_test, state_level_data, by="CVE_ENT", all.x=TRUE)

length(enve_test$extortions[is.na(enve_test$extortions)])
```

```
[1] 0
```

```r
length(enve_test$bribes[is.na(enve_test$bribes)])
```

```
[1] 0
```

```r
## enve_test$extortions[is.na(enve_test$extortions)] <- 0
## enve_test$bribes[is.na(enve_test$bribes)] <- 0

summary(enve_test)
```

```
    CVE_ENT        extortions      extortion_victim rep_extortion_victim rep_extortions      CVE_UNICA     
 Min.   : 1.00   Min.   : 0.0000   1: 297           Min.   :0.0000       Min.   : 0.0000   Min.   :    60  
 1st Qu.: 9.00   1st Qu.: 0.0000   2:2153           1st Qu.:0.0000       1st Qu.: 0.0000   1st Qu.: 10220  
 Median :16.00   Median : 0.0000   9:  50           Median :0.0000       Median : 0.0000   Median : 19849  
 Mean   :16.56   Mean   : 0.5692                    Mean   :0.0752       Mean   : 0.4504   Mean   : 32087  
 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.:0.0000       3rd Qu.: 0.0000   3rd Qu.: 53126  
 Max.   :32.00   Max.   :33.0000                    Max.   :1.0000       Max.   :32.0000   Max.   :100000  
                                                                                                           
     bribes        bribe1   bribe2   bribe3   bribe4    bribe_victim      rep_bribe      bribe_cats
 Min.   : 0.0000   1: 282   1: 282   1: 282   1: 282   Min.   :0.0000   Min.   :0.0000   0 :2218   
 1st Qu.: 0.0000   2:2168   2:2168   2:2168   2:2168   1st Qu.:0.0000   1st Qu.:0.0000   1 : 135   
 Median : 0.0000   9:  50   9:  50   9:  50   9:  50   Median :0.0000   Median :0.0000   2 :  54   
 Mean   : 0.2804                                       Mean   :0.1128   Mean   :0.0588   3+:  93   
 3rd Qu.: 0.0000                                       3rd Qu.:0.0000   3rd Qu.:0.0000             
 Max.   :16.0000                                       Max.   :1.0000   Max.   :1.0000             
                                                                                                   
     size     sector     tempsub                subsector         subsector_safe     years         yearsquant 
 Large :589   C:809   Min.   :212410   Other serv.   :843   Manufacturing:484    Min.   : 0.00   [0,8]  :552  
 Medium:633   I:825   1st Qu.:361710   Manufacturing :484   Health       :391    1st Qu.:10.00   (8,16] :454  
 Small :618   S:866   Median :511160   Other industry:386   HotelsRestBar:367    Median :21.00   (16,25]:507  
 Micro :660           Mean   :511102   HotelsRestBar :367   Construction :356    Mean   :20.94   (25,34]:525  
                      3rd Qu.:658810   Transport     :135   Maintenance  :206    3rd Qu.:31.00   (34,43]:462  
                      Max.   :812910   Wholesale     :115   Transport    :135    Max.   :43.00                
                                       (Other)       :170   (Other)      :561                                 
       X               N        denuncias_homs     poblacion           tasahom       bribes_preval  
 Min.   : 1.00   Min.   : 534   Min.   :  39.0   Min.   :  698295   Min.   : 1.938   Min.   :16.93  
 1st Qu.: 9.00   1st Qu.: 745   1st Qu.: 151.0   1st Qu.: 1728429   1st Qu.: 7.611   1st Qu.:32.06  
 Median :16.00   Median : 838   Median : 536.0   Median : 2890108   Median :11.806   Median :42.25  
 Mean   :16.56   Mean   : 875   Mean   : 581.6   Mean   : 3811708   Mean   :15.974   Mean   :46.32  
 3rd Qu.:24.00   3rd Qu.: 961   3rd Qu.: 755.5   3rd Qu.: 4941059   3rd Qu.:20.165   3rd Qu.:56.95  
 Max.   :32.00   Max.   :1657   Max.   :2087.0   Max.   :16364210   Max.   :59.225   Max.   :94.14  
                                                                                                    
  bribes_inci      bribes_conc     bribes_abvic   bribes_abincs     Incidence        Prevalence    
 Min.   : 31.42   Min.   :1.318   Min.   : 14.0   Min.   : 26.0   Min.   : 45.67   Min.   : 23.85  
 1st Qu.: 72.48   1st Qu.:1.693   1st Qu.: 27.0   1st Qu.: 54.0   1st Qu.: 78.41   1st Qu.: 56.50  
 Median : 89.49   Median :1.895   Median : 34.0   Median : 82.0   Median :116.80   Median : 67.17  
 Mean   :111.30   Mean   :2.345   Mean   : 39.9   Mean   : 94.9   Mean   :131.86   Mean   : 80.30  
 3rd Qu.:125.46   3rd Qu.:2.485   3rd Qu.: 53.0   3rd Qu.:108.0   3rd Qu.:172.60   3rd Qu.: 99.25  
 Max.   :420.16   Max.   :5.797   Max.   :101.0   Max.   :371.0   Max.   :290.12   Max.   :190.69  
                                                                                                   
 Concentration       defun          defun_arma      ind_general     ind_derecho       Derecho     
 Min.   :1.163   Min.   :  46.0   Min.   :   8.0   Min.   :25.26   Min.   :21.37   Min.   :21.37  
 1st Qu.:1.340   1st Qu.: 214.0   1st Qu.: 111.0   1st Qu.:42.55   1st Qu.:50.09   1st Qu.:50.09  
 Median :1.606   Median : 638.0   Median : 380.0   Median :49.02   Median :55.53   Median :55.53  
 Mean   :1.658   Mean   : 718.6   Mean   : 449.1   Mean   :47.81   Mean   :54.42   Mean   :54.42  
 3rd Qu.:1.820   3rd Qu.: 832.0   3rd Qu.: 526.0   3rd Qu.:52.67   3rd Qu.:62.15   3rd Qu.:62.15  
 Max.   :2.709   Max.   :3265.0   Max.   :2049.0   Max.   :67.85   Max.   :78.40   Max.   :78.40  
                                                                                                  
    General                  ENTIDAD          AÑO           drogas             do             armas       
 Min.   :25.26   HIDALGO         : 100   Min.   :2013   Min.   :  37.0   Min.   :  0.00   Min.   :  31.0  
 1st Qu.:42.55   GUANAJUATO      :  95   1st Qu.:2013   1st Qu.: 108.0   1st Qu.: 15.00   1st Qu.: 240.0  
 Median :49.02   JALISCO         :  95   Median :2013   Median : 187.0   Median : 28.00   Median : 455.0  
 Mean   :47.81   NUEVO LEON      :  87   Mean   :2013   Mean   : 548.9   Mean   : 39.64   Mean   : 575.1  
 3rd Qu.:52.67   CIUDAD DE MEXICO:  85   3rd Qu.:2013   3rd Qu.: 591.0   3rd Qu.: 53.00   3rd Qu.: 874.0  
 Max.   :67.85   PUEBLA          :  84   Max.   :2013   Max.   :3738.0   Max.   :174.00   Max.   :1632.0  
                 (Other)         :1954                                                                    
 ab_ext_preval      ab_ext_inc                NOM_ENT        NOM_ABR    
 Min.   : 19.00   Min.   : 29.0   HIDALGO         : 100   HGO.   : 100  
 1st Qu.: 45.00   1st Qu.: 63.0   GUANAJUATO      :  95   GTO.   :  95  
 Median : 59.00   Median :103.0   JALISCO         :  95   JAL.   :  95  
 Mean   : 69.97   Mean   :115.7   NUEVO LEON      :  87   NL     :  87  
 3rd Qu.: 83.00   3rd Qu.:144.0   DISTRITO FEDERAL:  85   DF     :  85  
 Max.   :180.00   Max.   :286.0   PUEBLA          :  84   PUE.   :  84  
                                  (Other)         :1954   (Other):1954  
```

```r
# Exclude Corporate and Utilitites

nrow(enve_test) # should be 28179
```

```
[1] 2500
```

```r
enve_test %>%
    filter(subsector != "Utilities") %>%
    filter(subsector != "Corporate") -> enve_test_2

enve_test_2$subsector <- droplevels(enve_test_2$subsector)

levels(enve_test_2$subsector)
```

```
[1] "Retail"         "HotelsRestBar"  "Manufacturing"  "Other industry" "Other serv."    "Transport"     
[7] "Wholesale"     
```

```r
enve_test_2$subsector_safe <- droplevels(enve_test_2$subsector_safe)
levels(enve_test_2$subsector_safe)
```

```
 [1] "Retail"         "Mining"         "Construction"   "Manufacturing"  "Wholesale"      "Transport"     
 [7] "Media"          "Finance"        "Real estate"    "Prof. services" "Maintenance"    "Education"     
[13] "Health"         "Leisure"        "HotelsRestBar"  "Other"         
```

```r
nrow(enve_test_2) # should be 28161
```

```
[1] 2412
```

```r
enve_test <- enve_test_2
nrow(enve_test)
```

```
[1] 2412
```

```r
summary(enve_test)
```

```
    CVE_ENT        extortions      extortion_victim rep_extortion_victim rep_extortions      CVE_UNICA     
 Min.   : 1.00   Min.   : 0.0000   1: 285           Min.   :0.00000      Min.   : 0.0000   Min.   :    60  
 1st Qu.: 9.00   1st Qu.: 0.0000   2:2079           1st Qu.:0.00000      1st Qu.: 0.0000   1st Qu.: 10225  
 Median :16.00   Median : 0.0000   9:  48           Median :0.00000      Median : 0.0000   Median : 19849  
 Mean   :16.47   Mean   : 0.5688                    Mean   :0.07504      Mean   : 0.4507   Mean   : 32109  
 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.:0.00000      3rd Qu.: 0.0000   3rd Qu.: 53297  
 Max.   :32.00   Max.   :33.0000                    Max.   :1.00000      Max.   :32.0000   Max.   :100000  
                                                                                                           
     bribes        bribe1   bribe2   bribe3   bribe4    bribe_victim      rep_bribe       bribe_cats
 Min.   : 0.0000   1: 274   1: 274   1: 274   1: 274   Min.   :0.0000   Min.   :0.00000   0 :2138   
 1st Qu.: 0.0000   2:2090   2:2090   2:2090   2:2090   1st Qu.:0.0000   1st Qu.:0.00000   1 : 131   
 Median : 0.0000   9:  48   9:  48   9:  48   9:  48   Median :0.0000   Median :0.00000   2 :  54   
 Mean   : 0.2786                                       Mean   :0.1136   Mean   :0.05929   3+:  89   
 3rd Qu.: 0.0000                                       3rd Qu.:0.0000   3rd Qu.:0.00000             
 Max.   :13.0000                                       Max.   :1.0000   Max.   :1.00000             
                                                                                                    
     size     sector     tempsub                subsector         subsector_safe     years        yearsquant 
 Large :568   C:776   Min.   :212410   Retail        : 82   Manufacturing:484    Min.   : 0.0   [0,8]  :539  
 Medium:611   I:792   1st Qu.:365310   HotelsRestBar :367   Health       :391    1st Qu.:10.0   (8,16] :433  
 Small :597   S:844   Median :510610   Manufacturing :484   HotelsRestBar:367    Median :21.0   (16,25]:487  
 Micro :636           Mean   :514704   Other industry:386   Construction :356    Mean   :20.9   (25,34]:510  
                      3rd Qu.:665310   Other serv.   :843   Maintenance  :206    3rd Qu.:31.0   (34,43]:443  
                      Max.   :812910   Transport     :135   Transport    :135    Max.   :43.0                
                                       Wholesale     :115   (Other)      :473                                
       X               N        denuncias_homs     poblacion           tasahom       bribes_preval  
 Min.   : 1.00   Min.   : 534   Min.   :  39.0   Min.   :  698295   Min.   : 1.938   Min.   :16.93  
 1st Qu.: 9.00   1st Qu.: 721   1st Qu.: 151.0   1st Qu.: 1728429   1st Qu.: 7.797   1st Qu.:32.06  
 Median :16.00   Median : 838   Median : 536.0   Median : 2890108   Median :11.806   Median :42.25  
 Mean   :16.47   Mean   : 873   Mean   : 583.3   Mean   : 3812229   Mean   :16.031   Mean   :46.27  
 3rd Qu.:24.00   3rd Qu.: 961   3rd Qu.: 775.0   3rd Qu.: 4941059   3rd Qu.:20.165   3rd Qu.:56.95  
 Max.   :32.00   Max.   :1657   Max.   :2087.0   Max.   :16364210   Max.   :59.225   Max.   :94.14  
                                                                                                    
  bribes_inci      bribes_conc     bribes_abvic    bribes_abincs      Incidence        Prevalence    
 Min.   : 31.42   Min.   :1.318   Min.   : 14.00   Min.   : 26.00   Min.   : 45.67   Min.   : 23.85  
 1st Qu.: 72.48   1st Qu.:1.693   1st Qu.: 27.00   1st Qu.: 48.00   1st Qu.: 83.42   1st Qu.: 56.50  
 Median : 89.49   Median :1.895   Median : 34.00   Median : 74.00   Median :116.80   Median : 67.17  
 Mean   :111.18   Mean   :2.342   Mean   : 39.75   Mean   : 94.57   Mean   :131.88   Mean   : 80.34  
 3rd Qu.:125.46   3rd Qu.:2.485   3rd Qu.: 53.00   3rd Qu.:108.00   3rd Qu.:172.60   3rd Qu.: 99.25  
 Max.   :420.16   Max.   :5.797   Max.   :101.00   Max.   :371.00   Max.   :290.12   Max.   :190.69  
                                                                                                     
 Concentration       defun          defun_arma      ind_general     ind_derecho       Derecho     
 Min.   :1.163   Min.   :  46.0   Min.   :   8.0   Min.   :25.26   Min.   :21.37   Min.   :21.37  
 1st Qu.:1.340   1st Qu.: 214.0   1st Qu.: 111.0   1st Qu.:42.55   1st Qu.:50.09   1st Qu.:50.09  
 Median :1.606   Median : 638.0   Median : 380.0   Median :49.02   Median :55.53   Median :55.53  
 Mean   :1.658   Mean   : 721.4   Mean   : 451.1   Mean   :47.83   Mean   :54.38   Mean   :54.38  
 3rd Qu.:1.820   3rd Qu.: 832.0   3rd Qu.: 526.0   3rd Qu.:52.67   3rd Qu.:62.15   3rd Qu.:62.15  
 Max.   :2.709   Max.   :3265.0   Max.   :2049.0   Max.   :67.85   Max.   :78.40   Max.   :78.40  
                                                                                                  
    General                  ENTIDAD          AÑO           drogas             do             armas       
 Min.   :25.26   HIDALGO         :  98   Min.   :2013   Min.   :  37.0   Min.   :  0.00   Min.   :  31.0  
 1st Qu.:42.55   GUANAJUATO      :  93   1st Qu.:2013   1st Qu.: 108.0   1st Qu.: 15.00   1st Qu.: 240.0  
 Median :49.02   JALISCO         :  93   Median :2013   Median : 187.0   Median : 28.00   Median : 455.0  
 Mean   :47.83   CIUDAD DE MEXICO:  82   Mean   :2013   Mean   : 550.9   Mean   : 39.69   Mean   : 575.8  
 3rd Qu.:52.67   NUEVO LEON      :  82   3rd Qu.:2013   3rd Qu.: 591.0   3rd Qu.: 53.00   3rd Qu.: 874.0  
 Max.   :67.85   COAHUILA        :  80   Max.   :2013   Max.   :3738.0   Max.   :174.00   Max.   :1632.0  
                 (Other)         :1884                                                                    
 ab_ext_preval      ab_ext_inc                    NOM_ENT        NOM_ABR    
 Min.   : 19.00   Min.   : 29.0   HIDALGO             :  98   HGO.   :  98  
 1st Qu.: 45.00   1st Qu.: 63.0   GUANAJUATO          :  93   GTO.   :  93  
 Median : 59.00   Median :103.0   JALISCO             :  93   JAL.   :  93  
 Mean   : 69.83   Mean   :115.4   DISTRITO FEDERAL    :  82   DF     :  82  
 3rd Qu.: 83.00   3rd Qu.:144.0   NUEVO LEON          :  82   NL     :  82  
 Max.   :180.00   Max.   :286.0   COAHUILA DE ZARAGOZA:  80   COAH.  :  80  
                                  (Other)             :1884   (Other):1884  
```

Next we load incident-level data:


```r
enve_incidents_all <- read.dbf("enve2014delitos_ciega_2014.dbf")

# Selecting only those relevant for extortion (code 10)

enve_incidents_all$delito <- as.integer(as.character(enve_incidents_all$ID_DELITO))

enve_incidents <- enve_incidents_all[enve_incidents_all$delito == 10,]

# Selecting those relevant for our study

incident_df <- data.frame(CVE_UNICA=as.integer(as.character(enve_incidents$ID_CONSECU)))

incident_df$month <- as.numeric(as.character(enve_incidents$M1_1))

incident_df$delito <- enve_incidents$delito

incident_df$n_offenders <- enve_incidents$M1_8
summary(incident_df$n_offenders)
```

```
   1    2    3    4    5    6    9 NA's 
2394  750  154   34   23   34  199 3067 
```

```r
levels(incident_df$n_offenders) <- c(1:6, "DK/DA")
summary(incident_df$n_offenders)
```

```
    1     2     3     4     5     6 DK/DA  NA's 
 2394   750   154    34    23    34   199  3067 
```

```r
incident_df$n_offenders_NA <-  incident_df$n_offenders
incident_df$n_offenders_NA[is.na(incident_df$n_offenders)] <- "DK/DA"
summary(incident_df$n_offenders_NA)
```

```
    1     2     3     4     5     6 DK/DA 
 2394   750   154    34    23    34  3266 
```

```r
incident_df$n_offenders_old <- incident_df$n_offenders_NA
levels(incident_df$n_offenders_NA) <- c(1:3, "4+", "4+", "4+", "DK/DA")
summary(incident_df$n_offenders_NA)
```

```
    1     2     3    4+ DK/DA 
 2394   750   154    91  3266 
```

```r
incident_df$uk_n_offenders <- incident_df$n_offenders_NA

levels(incident_df$uk_n_offenders)[1:4] <- "known"

incident_df$n_offenders_num <- enve_incidents$M1_8
summary(incident_df$n_offenders_num)
```

```
   1    2    3    4    5    6    9 NA's 
2394  750  154   34   23   34  199 3067 
```

```r
levels(incident_df$n_offenders_num) <- c(1:6, 0)
summary(incident_df$n_offenders_num)
```

```
   1    2    3    4    5    6    0 NA's 
2394  750  154   34   23   34  199 3067 
```

```r
incident_df$n_offenders_num[is.na(incident_df$n_offenders_num)] <- 0
incident_df$n_offenders_num <- as.numeric(as.character(incident_df$n_offenders_num))
table(incident_df$n_offenders_num)
```

```

   0    1    2    3    4    5    6 
3266 2394  750  154   34   23   34 
```

```r
incident_df$n_offenders_num <- incident_df$n_offenders_num - 1


## Data imputation for missing variables?

incident_df$rel_offenders <- enve_incidents$M1_11
levels(incident_df$rel_offenders) <- c("Known", "Known",
                                       "Known", "Known",
                                       "Total stranger", "DK/DA")
incident_df$rel_offenders <- relevel(incident_df$rel_offenders, ref="Total stranger")
summary(incident_df$rel_offenders)
```

```
Total stranger          Known          DK/DA           NA's 
          3178            175            235           3067 
```

```r
incident_df$rel_offenders_NA <- incident_df$rel_offenders
incident_df$rel_offenders_NA[is.na(incident_df$rel_offenders_NA)] <- "DK/DA"
summary(incident_df$rel_offenders_NA)
```

```
Total stranger          Known          DK/DA 
          3178            175           3302 
```

```r
incident_df$had_weapon <- enve_incidents$M1_13
levels(incident_df$had_weapon) <- c("Yes", "No", "DK/DA")
incident_df$had_weapon <- relevel(incident_df$had_weapon, ref="No")
summary(incident_df$had_weapon)
```

```
   No   Yes DK/DA  NA's 
 1502   185  1901  3067 
```

```r
incident_df$had_weapon_NA <- incident_df$had_weapon
incident_df$had_weapon_NA[is.na(incident_df$had_weapon_NA)] <- "DK/DA"
summary(incident_df$had_weapon_NA)
```

```
   No   Yes DK/DA 
 1502   185  4968 
```

```r
incident_df$extortion_type <- as.character(enve_incidents$M5_1)
incident_df$extortion_type <- as.factor(incident_df$extortion_type)
levels(incident_df$extortion_type) <- c("Remote", "Remote", "Street",
                                        "Premises", "Cobro de piso", "Other")
levels(incident_df$extortion_type)
```

```
[1] "Remote"        "Street"        "Premises"      "Cobro de piso" "Other"        
```

```r
summary(incident_df$extortion_type)
```

```
       Remote        Street      Premises Cobro de piso         Other 
         6245           207            52            31           120 
```

```r
incident_df$extortion_type_bin <- as.character(enve_incidents$M5_1)
incident_df$extortion_type_bin <- as.factor(incident_df$extortion_type_bin)
levels(incident_df$extortion_type_bin) <- c("Remote", "Remote", "In person",
                                        "In person", "In person", "Other")
levels(incident_df$extortion_type_bin)
```

```
[1] "Remote"    "In person" "Other"    
```

```r
summary(incident_df$extortion_type_bin)
```

```
   Remote In person     Other 
     6245       290       120 
```

```r
# Extortion bin w/o Cobro de piso
incident_df$extortion_type_wocp <- as.character(enve_incidents$M5_1)
incident_df$extortion_type_wocp <- as.factor(incident_df$extortion_type_wocp)
levels(incident_df$extortion_type_wocp) <- c("Remote", "Remote", "In person",
                                        "In person", "Other", "Other")
levels(incident_df$extortion_type_wocp)
```

```
[1] "Remote"    "In person" "Other"    
```

```r
summary(incident_df$extortion_type_wocp)
```

```
   Remote In person     Other 
     6245       259       151 
```

```r
incident_df$complied <- enve_incidents$M5_3
levels(incident_df$complied) <-  c("Yes", "No", "DK/DA")
incident_df$complied <- relevel(incident_df$complied, ref="No")
summary(incident_df$complied)
```

```
   No   Yes DK/DA  NA's 
 4767   370    29  1489 
```

```r
incident_df$complied_bin <- enve_incidents$M5_3
levels(incident_df$complied_bin) <-  c("Yes", "No", NA)
incident_df$complied_bin <- relevel(incident_df$complied_bin, ref="No")
summary(incident_df$complied_bin)
```

```
  No  Yes NA's 
4767  370 1518 
```

```r
incident_df$complied_bin_NA <- incident_df$complied_bin
incident_df$complied_bin_NA[is.na(incident_df$complied_bin_NA)] <- "No"
summary(incident_df$complied_bin_NA)
```

```
  No  Yes 
6285  370 
```

```r
incident_df <- subset(incident_df, extortion_type != "Other")
incident_df$extortion_type <- droplevels(incident_df$extortion_type)

# Remember to subset out "other" obs from extortion_type_wocp
incident_df$extortion_type_bin <- droplevels(incident_df$extortion_type_bin)

summary(incident_df)
```

```
   CVE_UNICA         month           delito    n_offenders   n_offenders_NA n_offenders_old uk_n_offenders
 Min.   :   24   Min.   : 1.00   Min.   :10   1      :2351   1    :2351     1    :2351      known:3302    
 1st Qu.: 7168   1st Qu.: 4.00   1st Qu.:10   2      : 728   2    : 728     2    : 728      DK/DA:3233    
 Median :11759   Median : 7.00   Median :10   DK/DA  : 194   3    : 147     3    : 147                    
 Mean   :11948   Mean   :10.34   Mean   :10   3      : 147   4+   :  76     4    :  30                    
 3rd Qu.:17414   3rd Qu.:11.00   3rd Qu.:10   4      :  30   DK/DA:3233     5    :  23                    
 Max.   :23385   Max.   :99.00   Max.   :10   (Other):  46                  6    :  23                    
                                              NA's   :3039                  DK/DA:3233                    
 n_offenders_num          rel_offenders        rel_offenders_NA had_weapon   had_weapon_NA       extortion_type
 Min.   :-1.0000   Total stranger:3121   Total stranger:3121    No   :1455   No   :1455    Remote       :6245  
 1st Qu.:-1.0000   Known         : 142   Known         : 142    Yes  : 149   Yes  : 149    Street       : 207  
 Median : 0.0000   DK/DA         : 233   DK/DA         :3272    DK/DA:1892   DK/DA:4931    Premises     :  52  
 Mean   :-0.2929   NA's          :3039                          NA's :3039                 Cobro de piso:  31  
 3rd Qu.: 0.0000                                                                                               
 Max.   : 5.0000                                                                                               
                                                                                                               
 extortion_type_bin extortion_type_wocp  complied    complied_bin complied_bin_NA
 Remote   :6245     Remote   :6245      No   :4714   No  :4714    No :6198       
 In person: 290     In person: 259      Yes  : 337   Yes : 337    Yes: 337       
                    Other    :  31      DK/DA:   9   NA's:1484                   
                                        NA's :1475                               
                                                                                 
                                                                                 
                                                                                 
```

Next we merge both incident-level and victim-level tables.


```r
enve_incvic <- merge(incident_df, enve_test, by="CVE_UNICA")

nrow(enve_incvic)
```

```
[1] 1522
```

```r
nrow(incident_df)
```

```
[1] 6535
```


# Models



```r
## datasets with no NA values first.

nacols <- colnames(enve_incvic)[colSums(is.na(enve_incvic)) > 0]

nonacols <- names(enve_incvic)[!(names(enve_incvic) %in% nacols)]

enve_incvic %>%
    select(nonacols) -> enve_nona

summary(enve_nona)
```

```
   CVE_UNICA         month           delito   n_offenders_NA n_offenders_old uk_n_offenders n_offenders_num  
 Min.   :   60   Min.   : 1.00   Min.   :10   1    :550      1    :550       known:759      Min.   :-1.0000  
 1st Qu.: 7449   1st Qu.: 4.00   1st Qu.:10   2    :153      2    :153       DK/DA:763      1st Qu.:-1.0000  
 Median :11818   Median : 7.00   Median :10   3    : 35      3    : 35                      Median :-1.0000  
 Mean   :12072   Mean   :10.47   Mean   :10   4+   : 21      4    :  7                      Mean   :-0.3009  
 3rd Qu.:17592   3rd Qu.:11.00   3rd Qu.:10   DK/DA:763      5    :  9                      3rd Qu.: 0.0000  
 Max.   :23355   Max.   :99.00   Max.   :10                  6    :  5                      Max.   : 5.0000  
                                                             DK/DA:763                                       
       rel_offenders_NA had_weapon_NA       extortion_type extortion_type_bin extortion_type_wocp
 Total stranger:724     No   : 352    Remote       :1437   Remote   :1437     Remote   :1437     
 Known         : 31     Yes  :  36    Street       :  62   In person:  85     In person:  78     
 DK/DA         :767     DK/DA:1134    Premises     :  16                      Other    :   7     
                                      Cobro de piso:   7                                         
                                                                                                 
                                                                                                 
                                                                                                 
 complied_bin_NA    CVE_ENT        extortions      extortion_victim rep_extortion_victim rep_extortions   
 No :1434        Min.   : 1.00   Min.   : 0.0000   1: 170           Min.   :0.00000      Min.   : 0.0000  
 Yes:  88        1st Qu.: 9.00   1st Qu.: 0.0000   2:1326           1st Qu.:0.00000      1st Qu.: 0.0000  
                 Median :16.00   Median : 0.0000   9:  26           Median :0.00000      Median : 0.0000  
                 Mean   :16.35   Mean   : 0.5986                    Mean   :0.06636      Mean   : 0.4869  
                 3rd Qu.:24.00   3rd Qu.: 0.0000                    3rd Qu.:0.00000      3rd Qu.: 0.0000  
                 Max.   :32.00   Max.   :33.0000                    Max.   :1.00000      Max.   :32.0000  
                                                                                                          
     bribes        bribe1   bribe2   bribe3   bribe4    bribe_victim      rep_bribe       bribe_cats
 Min.   : 0.0000   1: 181   1: 181   1: 181   1: 181   Min.   :0.0000   Min.   :0.00000   0 :1341   
 1st Qu.: 0.0000   2:1310   2:1310   2:1310   2:1310   1st Qu.:0.0000   1st Qu.:0.00000   1 :  84   
 Median : 0.0000   9:  31   9:  31   9:  31   9:  31   Median :0.0000   Median :0.00000   2 :  30   
 Mean   : 0.3154                                       Mean   :0.1189   Mean   :0.06373   3+:  67   
 3rd Qu.: 0.0000                                       3rd Qu.:0.0000   3rd Qu.:0.00000             
 Max.   :13.0000                                       Max.   :1.0000   Max.   :1.00000             
                                                                                                    
     size     sector     tempsub                subsector         subsector_safe     years         yearsquant 
 Large :373   C:491   Min.   :212410   Retail        : 45   Manufacturing:336    Min.   : 0.00   [0,8]  :359  
 Medium:369   I:478   1st Qu.:369135   HotelsRestBar :233   Health       :265    1st Qu.: 9.00   (8,16] :296  
 Small :353   S:553   Median :517110   Manufacturing :336   HotelsRestBar:233    Median :20.00   (16,25]:297  
 Micro :427           Mean   :518096   Other industry:219   Construction :198    Mean   :20.31   (25,34]:306  
                      3rd Qu.:671810   Other serv.   :547   Maintenance  :126    3rd Qu.:31.00   (34,43]:264  
                      Max.   :812910   Transport     : 80   Transport    : 80    Max.   :43.00                
                                       Wholesale     : 62   (Other)      :284                                 
       X               N          denuncias_homs     poblacion           tasahom       bribes_preval  
 Min.   : 1.00   Min.   : 534.0   Min.   :  39.0   Min.   :  698295   Min.   : 1.938   Min.   :16.93  
 1st Qu.: 9.00   1st Qu.: 745.0   1st Qu.: 151.0   1st Qu.: 1874188   1st Qu.: 7.797   1st Qu.:31.50  
 Median :16.00   Median : 838.0   Median : 536.0   Median : 2932313   Median :12.814   Median :42.20  
 Mean   :16.35   Mean   : 876.9   Mean   : 598.5   Mean   : 3964424   Mean   :15.903   Mean   :45.69  
 3rd Qu.:24.00   3rd Qu.: 961.0   3rd Qu.: 775.0   3rd Qu.: 4941059   3rd Qu.:20.165   3rd Qu.:56.95  
 Max.   :32.00   Max.   :1657.0   Max.   :2087.0   Max.   :16364210   Max.   :59.225   Max.   :94.14  
                                                                                                      
  bribes_inci      bribes_conc     bribes_abvic    bribes_abincs     Incidence        Prevalence    
 Min.   : 31.42   Min.   :1.318   Min.   : 14.00   Min.   : 26.0   Min.   : 45.67   Min.   : 23.85  
 1st Qu.: 64.99   1st Qu.:1.693   1st Qu.: 27.00   1st Qu.: 48.0   1st Qu.: 83.42   1st Qu.: 56.50  
 Median : 88.77   Median :1.882   Median : 34.00   Median : 74.0   Median :116.80   Median : 67.17  
 Mean   :106.72   Mean   :2.287   Mean   : 39.34   Mean   : 90.9   Mean   :131.75   Mean   : 80.67  
 3rd Qu.:125.46   3rd Qu.:2.263   3rd Qu.: 53.00   3rd Qu.:108.0   3rd Qu.:172.60   3rd Qu.: 99.25  
 Max.   :420.16   Max.   :5.797   Max.   :101.00   Max.   :371.0   Max.   :290.12   Max.   :190.69  
                                                                                                    
 Concentration       defun        defun_arma      ind_general     ind_derecho       Derecho     
 Min.   :1.163   Min.   :  46   Min.   :   8.0   Min.   :25.26   Min.   :21.37   Min.   :21.37  
 1st Qu.:1.340   1st Qu.: 214   1st Qu.: 111.0   1st Qu.:42.55   1st Qu.:49.83   1st Qu.:49.83  
 Median :1.589   Median : 648   Median : 402.0   Median :49.02   Median :55.50   Median :55.50  
 Mean   :1.643   Mean   : 755   Mean   : 470.7   Mean   :47.90   Mean   :54.12   Mean   :54.12  
 3rd Qu.:1.820   3rd Qu.: 832   3rd Qu.: 526.0   3rd Qu.:53.60   3rd Qu.:62.15   3rd Qu.:62.15  
 Max.   :2.709   Max.   :3265   Max.   :2049.0   Max.   :67.85   Max.   :78.40   Max.   :78.40  
                                                                                                
    General            ENTIDAD          AÑO           drogas             do             armas       
 Min.   :25.26   JALISCO   :  74   Min.   :2013   Min.   :  37.0   Min.   :  0.00   Min.   :  31.0  
 1st Qu.:42.55   OAXACA    :  65   1st Qu.:2013   1st Qu.: 108.0   1st Qu.: 20.00   1st Qu.: 257.0  
 Median :49.02   GUANAJUATO:  63   Median :2013   Median : 241.0   Median : 29.00   Median : 468.0  
 Mean   :47.90   CHIHUAHUA :  62   Mean   :2013   Mean   : 574.5   Mean   : 40.77   Mean   : 607.6  
 3rd Qu.:53.60   HIDALGO   :  59   3rd Qu.:2013   3rd Qu.: 591.0   3rd Qu.: 53.00   3rd Qu.: 932.0  
 Max.   :67.85   QUERETARO :  58   Max.   :2013   Max.   :3738.0   Max.   :174.00   Max.   :1632.0  
                 (Other)   :1141                                                                    
 ab_ext_preval      ab_ext_inc          NOM_ENT        NOM_ABR    
 Min.   : 19.00   Min.   : 29.0   JALISCO   :  74   JAL.   :  74  
 1st Qu.: 49.00   1st Qu.: 64.0   OAXACA    :  65   OAX.   :  65  
 Median : 59.00   Median :103.0   GUANAJUATO:  63   GTO.   :  63  
 Mean   : 70.27   Mean   :115.7   CHIHUAHUA :  62   CHIH.  :  62  
 3rd Qu.: 83.00   3rd Qu.:144.0   HIDALGO   :  59   HGO.   :  59  
 Max.   :180.00   Max.   :286.0   QUERETARO :  58   QRO.   :  58  
                                  (Other)   :1141   (Other):1141  
```

```r
nrow(enve_nona)
```

```
[1] 1522
```

```r
nrow(enve_nona[complete.cases(enve_nona),])
```

```
[1] 1522
```

```r
nrow(enve_nona[complete.cases(enve_incvic),])
```

```
[1] 604
```

```r
## Data structure

"number of incidents"
```

```
[1] "number of incidents"
```

```r
nrow(enve_nona)
```

```
[1] 1522
```

```r
"incidents per business"
```

```
[1] "incidents per business"
```

```r
table(table(enve_nona$CVE_UNICA))
```

```

   1    2    3    4    5 
1017  124   48   12   13 
```

```r
enve_nona %>%
    count(CVE_UNICA) %>%
    summarise(min(n),
              max(n),
              mean(n),
              n())
```

```
# A tibble: 1 x 4
  `min(n)` `max(n)` `mean(n)` `n()`
     <int>    <int>     <dbl> <int>
1        1        5      1.25  1214
```

```r
"number of incidents per state"
```

```
[1] "number of incidents per state"
```

```r
enve_nona %>%
    count(CVE_ENT) %>%
    summarise(min(n),
              mean(n),
              max(n),
              n())
```

```
# A tibble: 1 x 4
  `min(n)` `mean(n)` `max(n)` `n()`
     <int>     <dbl>    <int> <int>
1       30      47.6       74    32
```

```r
"number of businesses per state"
```

```
[1] "number of businesses per state"
```

```r
enve_nona %>%
    count(CVE_ENT, CVE_UNICA) %>%
    count(CVE_ENT) %>%
    summarise(min(n),
              mean(n),
              max(n))
```

```
# A tibble: 1 x 3
  `min(n)` `mean(n)` `max(n)`
     <int>     <dbl>    <int>
1       26      37.9       49
```


```r
mean_bribes <- mean(state_level_data$bribes_abvic)
mean_bribes
```

```
[1] 40.125
```

```r
mean_armas <- mean(state_level_data$armas)
mean_armas
```

```
[1] 559.625
```

```r
mean_drogas <- mean(state_level_data$drogas)
mean_drogas
```

```
[1] 526.9062
```

```r
mean_poblacion <- mean(state_level_data$poblacion)
mean_poblacion
```

```
[1] 3699845
```

```r
mean_N <- mean(state_level_data$N)
mean_N
```

```
[1] 880.5938
```

```r
mean_General <- mean(state_level_data$General)
mean_General
```

```
[1] 47.66523
```

```r
mean_Derecho <- mean(state_level_data$Derecho)
mean_Derecho
```

```
[1] 54.38953
```


First the canonical model with the state level variables identified in JQC manuscript.

# EDA


```r
my_controls <- tableby.control(numeric.test = "kwt", cat.test = "chisq")


my_labels <- list(
  extortion_type = "Extortion type",
  extortion_type_bin = "Extortion type (binary)",
  extortion_type_wocp = "Extortion type (Other: piso)",
  n_offenders_NA = "Num. offenders",
  n_offenders_old = "Num. offenders (all cat.)",
  had_weapon_NA = "Had weapon",
  extortions = "Extortion concentration",
  bribes = "Corruption incidence",
  subsector = "Business type",
  subsector_safe = "Business type (all cat.)",
  size = "Business size",
  yearsquant = "Age (quintiles)",
  years = "Age"
)

#
#table_two <- tableby(continent ~ .,
#  data = gapminder,
#  control = my_controls
#)
#
#summary(table_two,
#  labelTranslations = my_labels,
#  title = "Summary Statistic of Gapminder Data"
#)
#


desc_formula <-  ~ extortion_type +
                        extortion_type_bin +
                        extortion_type_wocp +
                        n_offenders_NA +
                        n_offenders_old +
                        had_weapon_NA +
                        extortions +
                        bribes +
                        subsector +
                        subsector_safe +
                        size +
                        yearsquant +
                        years


desc_table <- tableby(desc_formula, data  = enve_nona,
                control = my_controls)

summary(desc_table,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 | Overall (N=1522) |
|:--------------------------------|:----------------:|
|**Extortion type**               |                  |
|&nbsp;&nbsp;&nbsp;Remote         |   1437 (94.4%)   |
|&nbsp;&nbsp;&nbsp;Street         |    62 (4.1%)     |
|&nbsp;&nbsp;&nbsp;Premises       |    16 (1.1%)     |
|&nbsp;&nbsp;&nbsp;Cobro de piso  |     7 (0.5%)     |
|**Extortion type (binary)**      |                  |
|&nbsp;&nbsp;&nbsp;Remote         |   1437 (94.4%)   |
|&nbsp;&nbsp;&nbsp;In person      |    85 (5.6%)     |
|**Extortion type (Other: piso)** |                  |
|&nbsp;&nbsp;&nbsp;Remote         |   1437 (94.4%)   |
|&nbsp;&nbsp;&nbsp;In person      |    78 (5.1%)     |
|&nbsp;&nbsp;&nbsp;Other          |     7 (0.5%)     |
|**Num. offenders**               |                  |
|&nbsp;&nbsp;&nbsp;1              |   550 (36.1%)    |
|&nbsp;&nbsp;&nbsp;2              |   153 (10.1%)    |
|&nbsp;&nbsp;&nbsp;3              |    35 (2.3%)     |
|&nbsp;&nbsp;&nbsp;4+             |    21 (1.4%)     |
|&nbsp;&nbsp;&nbsp;DK/DA          |   763 (50.1%)    |
|**Num. offenders (all cat.)**    |                  |
|&nbsp;&nbsp;&nbsp;1              |   550 (36.1%)    |
|&nbsp;&nbsp;&nbsp;2              |   153 (10.1%)    |
|&nbsp;&nbsp;&nbsp;3              |    35 (2.3%)     |
|&nbsp;&nbsp;&nbsp;4              |     7 (0.5%)     |
|&nbsp;&nbsp;&nbsp;5              |     9 (0.6%)     |
|&nbsp;&nbsp;&nbsp;6              |     5 (0.3%)     |
|&nbsp;&nbsp;&nbsp;DK/DA          |   763 (50.1%)    |
|**Had weapon**                   |                  |
|&nbsp;&nbsp;&nbsp;No             |   352 (23.1%)    |
|&nbsp;&nbsp;&nbsp;Yes            |    36 (2.4%)     |
|&nbsp;&nbsp;&nbsp;DK/DA          |   1134 (74.5%)   |
|**Extortion concentration**      |                  |
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.599 (2.949)   |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 33.000  |
|**Corruption incidence**         |                  |
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.315 (1.210)   |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 13.000  |
|**Business type**                |                  |
|&nbsp;&nbsp;&nbsp;Retail         |    45 (3.0%)     |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   233 (15.3%)    |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   336 (22.1%)    |
|&nbsp;&nbsp;&nbsp;Other industry |   219 (14.4%)    |
|&nbsp;&nbsp;&nbsp;Other serv.    |   547 (35.9%)    |
|&nbsp;&nbsp;&nbsp;Transport      |    80 (5.3%)     |
|&nbsp;&nbsp;&nbsp;Wholesale      |    62 (4.1%)     |
|**Business type (all cat.)**     |                  |
|&nbsp;&nbsp;&nbsp;Retail         |    45 (3.0%)     |
|&nbsp;&nbsp;&nbsp;Mining         |    21 (1.4%)     |
|&nbsp;&nbsp;&nbsp;Construction   |   198 (13.0%)    |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   336 (22.1%)    |
|&nbsp;&nbsp;&nbsp;Wholesale      |    62 (4.1%)     |
|&nbsp;&nbsp;&nbsp;Transport      |    80 (5.3%)     |
|&nbsp;&nbsp;&nbsp;Media          |    28 (1.8%)     |
|&nbsp;&nbsp;&nbsp;Finance        |    21 (1.4%)     |
|&nbsp;&nbsp;&nbsp;Real estate    |    26 (1.7%)     |
|&nbsp;&nbsp;&nbsp;Prof. services |    23 (1.5%)     |
|&nbsp;&nbsp;&nbsp;Maintenance    |    126 (8.3%)    |
|&nbsp;&nbsp;&nbsp;Education      |    25 (1.6%)     |
|&nbsp;&nbsp;&nbsp;Health         |   265 (17.4%)    |
|&nbsp;&nbsp;&nbsp;Leisure        |    25 (1.6%)     |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   233 (15.3%)    |
|&nbsp;&nbsp;&nbsp;Other          |     8 (0.5%)     |
|**Business size**                |                  |
|&nbsp;&nbsp;&nbsp;Large          |   373 (24.5%)    |
|&nbsp;&nbsp;&nbsp;Medium         |   369 (24.2%)    |
|&nbsp;&nbsp;&nbsp;Small          |   353 (23.2%)    |
|&nbsp;&nbsp;&nbsp;Micro          |   427 (28.1%)    |
|**Age (quintiles)**              |                  |
|&nbsp;&nbsp;&nbsp;[0,8]          |   359 (23.6%)    |
|&nbsp;&nbsp;&nbsp;(8,16]         |   296 (19.4%)    |
|&nbsp;&nbsp;&nbsp;(16,25]        |   297 (19.5%)    |
|&nbsp;&nbsp;&nbsp;(25,34]        |   306 (20.1%)    |
|&nbsp;&nbsp;&nbsp;(34,43]        |   264 (17.3%)    |
|**Age**                          |                  |
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 20.309 (12.682)  |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 43.000  |
```

```r
desc_formula2 <-  update(desc_formula, complied_bin_NA ~ .)


desc_table2 <- tableby(desc_formula2, data  = enve_nona,
                        control = my_controls)

summary(desc_table2,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |   No (N=1434)   |   Yes (N=88)    | Total (N=1522)  |    p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|----------:|
|**Extortion type**               |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;Remote         |  1384 (96.5%)   |   53 (60.2%)    |  1437 (94.4%)   |           |
|&nbsp;&nbsp;&nbsp;Street         |    44 (3.1%)    |   18 (20.5%)    |    62 (4.1%)    |           |
|&nbsp;&nbsp;&nbsp;Premises       |    3 (0.2%)     |   13 (14.8%)    |    16 (1.1%)    |           |
|&nbsp;&nbsp;&nbsp;Cobro de piso  |    3 (0.2%)     |    4 (4.5%)     |    7 (0.5%)     |           |
|**Extortion type (binary)**      |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;Remote         |  1384 (96.5%)   |   53 (60.2%)    |  1437 (94.4%)   |           |
|&nbsp;&nbsp;&nbsp;In person      |    50 (3.5%)    |   35 (39.8%)    |    85 (5.6%)    |           |
|**Extortion type (Other: piso)** |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;Remote         |  1384 (96.5%)   |   53 (60.2%)    |  1437 (94.4%)   |           |
|&nbsp;&nbsp;&nbsp;In person      |    47 (3.3%)    |   31 (35.2%)    |    78 (5.1%)    |           |
|&nbsp;&nbsp;&nbsp;Other          |    3 (0.2%)     |    4 (4.5%)     |    7 (0.5%)     |           |
|**Num. offenders**               |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   525 (36.6%)   |   25 (28.4%)    |   550 (36.1%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   134 (9.3%)    |   19 (21.6%)    |   153 (10.1%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    31 (2.2%)    |    4 (4.5%)     |    35 (2.3%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    7 (0.5%)     |   14 (15.9%)    |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   737 (51.4%)   |   26 (29.5%)    |   763 (50.1%)   |           |
|**Num. offenders (all cat.)**    |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   525 (36.6%)   |   25 (28.4%)    |   550 (36.1%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   134 (9.3%)    |   19 (21.6%)    |   153 (10.1%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    31 (2.2%)    |    4 (4.5%)     |    35 (2.3%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    3 (0.2%)     |    4 (4.5%)     |    7 (0.5%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    1 (0.1%)     |    8 (9.1%)     |    9 (0.6%)     |           |
|&nbsp;&nbsp;&nbsp;6              |    3 (0.2%)     |    2 (2.3%)     |    5 (0.3%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   737 (51.4%)   |   26 (29.5%)    |   763 (50.1%)   |           |
|**Had weapon**                   |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;No             |   327 (22.8%)   |   25 (28.4%)    |   352 (23.1%)   |           |
|&nbsp;&nbsp;&nbsp;Yes            |    17 (1.2%)    |   19 (21.6%)    |    36 (2.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |  1090 (76.0%)   |   44 (50.0%)    |  1134 (74.5%)   |           |
|**Extortion concentration**      |                 |                 |                 |   0.269^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.600 (2.984)  |  0.568 (2.323)  |  0.599 (2.949)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 33.000  | 0.000 - 20.000  | 0.000 - 33.000  |           |
|**Corruption incidence**         |                 |                 |                 |   0.005^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.294 (1.187)  |  0.670 (1.506)  |  0.315 (1.210)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 13.000  |  0.000 - 7.000  | 0.000 - 13.000  |           |
|**Business type**                |                 |                 |                 |   0.757^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    43 (3.0%)    |    2 (2.3%)     |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   225 (15.7%)   |    8 (9.1%)     |   233 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   314 (21.9%)   |   22 (25.0%)    |   336 (22.1%)   |           |
|&nbsp;&nbsp;&nbsp;Other industry |   206 (14.4%)   |   13 (14.8%)    |   219 (14.4%)   |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   512 (35.7%)   |   35 (39.8%)    |   547 (35.9%)   |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |    4 (4.5%)     |    80 (5.3%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    58 (4.0%)    |    4 (4.5%)     |    62 (4.1%)    |           |
|**Business type (all cat.)**     |                 |                 |                 |   0.101^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    43 (3.0%)    |    2 (2.3%)     |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;Mining         |    20 (1.4%)    |    1 (1.1%)     |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Construction   |   186 (13.0%)   |   12 (13.6%)    |   198 (13.0%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   314 (21.9%)   |   22 (25.0%)    |   336 (22.1%)   |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    58 (4.0%)    |    4 (4.5%)     |    62 (4.1%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |    4 (4.5%)     |    80 (5.3%)    |           |
|&nbsp;&nbsp;&nbsp;Media          |    25 (1.7%)    |    3 (3.4%)     |    28 (1.8%)    |           |
|&nbsp;&nbsp;&nbsp;Finance        |    16 (1.1%)    |    5 (5.7%)     |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    24 (1.7%)    |    2 (2.3%)     |    26 (1.7%)    |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    23 (1.6%)    |    0 (0.0%)     |    23 (1.5%)    |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |   118 (8.2%)    |    8 (9.1%)     |   126 (8.3%)    |           |
|&nbsp;&nbsp;&nbsp;Education      |    25 (1.7%)    |    0 (0.0%)     |    25 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;Health         |   248 (17.3%)   |   17 (19.3%)    |   265 (17.4%)   |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    25 (1.7%)    |    0 (0.0%)     |    25 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   225 (15.7%)   |    8 (9.1%)     |   233 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Other          |    8 (0.6%)     |    0 (0.0%)     |    8 (0.5%)     |           |
|**Business size**                |                 |                 |                 |   0.005^1^|
|&nbsp;&nbsp;&nbsp;Large          |   354 (24.7%)   |   19 (21.6%)    |   373 (24.5%)   |           |
|&nbsp;&nbsp;&nbsp;Medium         |   351 (24.5%)   |   18 (20.5%)    |   369 (24.2%)   |           |
|&nbsp;&nbsp;&nbsp;Small          |   319 (22.2%)   |   34 (38.6%)    |   353 (23.2%)   |           |
|&nbsp;&nbsp;&nbsp;Micro          |   410 (28.6%)   |   17 (19.3%)    |   427 (28.1%)   |           |
|**Age (quintiles)**              |                 |                 |                 |   0.118^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   339 (23.6%)   |   20 (22.7%)    |   359 (23.6%)   |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |   281 (19.6%)   |   15 (17.0%)    |   296 (19.4%)   |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |   278 (19.4%)   |   19 (21.6%)    |   297 (19.5%)   |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |   295 (20.6%)   |   11 (12.5%)    |   306 (20.1%)   |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |   241 (16.8%)   |   23 (26.1%)    |   264 (17.3%)   |           |
|**Age**                          |                 |                 |                 |   0.430^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 20.241 (12.609) | 21.420 (13.854) | 20.309 (12.682) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  | 0.000 - 43.000  | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
desc_formula3 <-  update(desc_formula, extortion_type ~ . -
                                        extortion_type -
                                        extortion_type_bin -
                                        extortion_type_wocp)


desc_table3 <- tableby(desc_formula3, data  = enve_nona,
                        control = my_controls)

summary(desc_table3,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 | Remote (N=1437) |  Street (N=62)  | Premises (N=16) | Cobro de piso (N=7) | Total (N=1522)  |    p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|:-------------------:|:---------------:|----------:|
|**Num. offenders**               |                 |                 |                 |                     |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   517 (36.0%)   |   26 (41.9%)    |    3 (18.8%)    |      4 (57.1%)      |   550 (36.1%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   135 (9.4%)    |   13 (21.0%)    |    3 (18.8%)    |      2 (28.6%)      |   153 (10.1%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    23 (1.6%)    |   10 (16.1%)    |    1 (6.2%)     |      1 (14.3%)      |    35 (2.3%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    5 (0.3%)     |    9 (14.5%)    |    7 (43.8%)    |      0 (0.0%)       |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   757 (52.7%)   |    4 (6.5%)     |    2 (12.5%)    |      0 (0.0%)       |   763 (50.1%)   |           |
|**Num. offenders (all cat.)**    |                 |                 |                 |                     |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   517 (36.0%)   |   26 (41.9%)    |    3 (18.8%)    |      4 (57.1%)      |   550 (36.1%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   135 (9.4%)    |   13 (21.0%)    |    3 (18.8%)    |      2 (28.6%)      |   153 (10.1%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    23 (1.6%)    |   10 (16.1%)    |    1 (6.2%)     |      1 (14.3%)      |    35 (2.3%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    3 (0.2%)     |    3 (4.8%)     |    1 (6.2%)     |      0 (0.0%)       |    7 (0.5%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    1 (0.1%)     |    3 (4.8%)     |    5 (31.2%)    |      0 (0.0%)       |    9 (0.6%)     |           |
|&nbsp;&nbsp;&nbsp;6              |    1 (0.1%)     |    3 (4.8%)     |    1 (6.2%)     |      0 (0.0%)       |    5 (0.3%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   757 (52.7%)   |    4 (6.5%)     |    2 (12.5%)    |      0 (0.0%)       |   763 (50.1%)   |           |
|**Had weapon**                   |                 |                 |                 |                     |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;No             |   307 (21.4%)   |   36 (58.1%)    |    4 (25.0%)    |      5 (71.4%)      |   352 (23.1%)   |           |
|&nbsp;&nbsp;&nbsp;Yes            |    4 (0.3%)     |   20 (32.3%)    |   10 (62.5%)    |      2 (28.6%)      |    36 (2.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |  1126 (78.4%)   |    6 (9.7%)     |    2 (12.5%)    |      0 (0.0%)       |  1134 (74.5%)   |           |
|**Extortion concentration**      |                 |                 |                 |                     |                 |   0.013^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.604 (2.946)  |  0.468 (3.429)  |  0.125 (0.500)  |    1.714 (2.138)    |  0.599 (2.949)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 33.000  | 0.000 - 27.000  |  0.000 - 2.000  |    0.000 - 4.000    | 0.000 - 33.000  |           |
|**Corruption incidence**         |                 |                 |                 |                     |                 |   0.069^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.313 (1.223)  |  0.177 (0.666)  |  0.938 (1.436)  |    0.571 (1.512)    |  0.315 (1.210)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 13.000  |  0.000 - 4.000  |  0.000 - 3.000  |    0.000 - 4.000    | 0.000 - 13.000  |           |
|**Business type**                |                 |                 |                 |                     |                 |   0.221^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    44 (3.1%)    |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   221 (15.4%)   |   10 (16.1%)    |    1 (6.2%)     |      1 (14.3%)      |   233 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   318 (22.1%)   |   13 (21.0%)    |    2 (12.5%)    |      3 (42.9%)      |   336 (22.1%)   |           |
|&nbsp;&nbsp;&nbsp;Other industry |   205 (14.3%)   |    8 (12.9%)    |    6 (37.5%)    |      0 (0.0%)       |   219 (14.4%)   |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   514 (35.8%)   |   25 (40.3%)    |    7 (43.8%)    |      1 (14.3%)      |   547 (35.9%)   |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |    2 (3.2%)     |    0 (0.0%)     |      2 (28.6%)      |    80 (5.3%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    59 (4.1%)    |    3 (4.8%)     |    0 (0.0%)     |      0 (0.0%)       |    62 (4.1%)    |           |
|**Business type (all cat.)**     |                 |                 |                 |                     |                 |   0.603^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    44 (3.1%)    |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;Mining         |    20 (1.4%)    |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Construction   |   185 (12.9%)   |    7 (11.3%)    |    6 (37.5%)    |      0 (0.0%)       |   198 (13.0%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   318 (22.1%)   |   13 (21.0%)    |    2 (12.5%)    |      3 (42.9%)      |   336 (22.1%)   |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    59 (4.1%)    |    3 (4.8%)     |    0 (0.0%)     |      0 (0.0%)       |    62 (4.1%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |    2 (3.2%)     |    0 (0.0%)     |      2 (28.6%)      |    80 (5.3%)    |           |
|&nbsp;&nbsp;&nbsp;Media          |    28 (1.9%)    |    0 (0.0%)     |    0 (0.0%)     |      0 (0.0%)       |    28 (1.8%)    |           |
|&nbsp;&nbsp;&nbsp;Finance        |    19 (1.3%)    |    2 (3.2%)     |    0 (0.0%)     |      0 (0.0%)       |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    25 (1.7%)    |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    26 (1.7%)    |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    22 (1.5%)    |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    23 (1.5%)    |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |   120 (8.4%)    |    5 (8.1%)     |    1 (6.2%)     |      0 (0.0%)       |   126 (8.3%)    |           |
|&nbsp;&nbsp;&nbsp;Education      |    23 (1.6%)    |    1 (1.6%)     |    0 (0.0%)     |      1 (14.3%)      |    25 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;Health         |   246 (17.1%)   |   13 (21.0%)    |    6 (37.5%)    |      0 (0.0%)       |   265 (17.4%)   |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    24 (1.7%)    |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    25 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   221 (15.4%)   |   10 (16.1%)    |    1 (6.2%)     |      1 (14.3%)      |   233 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Other          |    7 (0.5%)     |    1 (1.6%)     |    0 (0.0%)     |      0 (0.0%)       |    8 (0.5%)     |           |
|**Business size**                |                 |                 |                 |                     |                 |   0.131^1^|
|&nbsp;&nbsp;&nbsp;Large          |   357 (24.8%)   |   12 (19.4%)    |    2 (12.5%)    |      2 (28.6%)      |   373 (24.5%)   |           |
|&nbsp;&nbsp;&nbsp;Medium         |   345 (24.0%)   |   19 (30.6%)    |    3 (18.8%)    |      2 (28.6%)      |   369 (24.2%)   |           |
|&nbsp;&nbsp;&nbsp;Small          |   324 (22.5%)   |   21 (33.9%)    |    5 (31.2%)    |      3 (42.9%)      |   353 (23.2%)   |           |
|&nbsp;&nbsp;&nbsp;Micro          |   411 (28.6%)   |   10 (16.1%)    |    6 (37.5%)    |      0 (0.0%)       |   427 (28.1%)   |           |
|**Age (quintiles)**              |                 |                 |                 |                     |                 |   0.004^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   324 (22.5%)   |   23 (37.1%)    |    6 (37.5%)    |      6 (85.7%)      |   359 (23.6%)   |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |   288 (20.0%)   |    8 (12.9%)    |    0 (0.0%)     |      0 (0.0%)       |   296 (19.4%)   |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |   283 (19.7%)   |   11 (17.7%)    |    3 (18.8%)    |      0 (0.0%)       |   297 (19.5%)   |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |   294 (20.5%)   |    9 (14.5%)    |    3 (18.8%)    |      0 (0.0%)       |   306 (20.1%)   |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |   248 (17.3%)   |   11 (17.7%)    |    4 (25.0%)    |      1 (14.3%)      |   264 (17.3%)   |           |
|**Age**                          |                 |                 |                 |                     |                 |   0.041^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 20.443 (12.573) | 18.355 (13.860) | 20.938 (15.631) |   8.714 (13.099)    | 20.309 (12.682) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  | 0.000 - 43.000  | 0.000 - 42.000  |   2.000 - 38.000    | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
desc_formula4 <-  update(desc_formula3, extortion_type_bin ~ .)


desc_table4 <- tableby(desc_formula4, data  = enve_nona,
                        control = my_controls)

summary(desc_table4,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 | Remote (N=1437) | In person (N=85) | Total (N=1522)  |    p value|
|:--------------------------------|:---------------:|:----------------:|:---------------:|----------:|
|**Num. offenders**               |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   517 (36.0%)   |    33 (38.8%)    |   550 (36.1%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   135 (9.4%)    |    18 (21.2%)    |   153 (10.1%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    23 (1.6%)    |    12 (14.1%)    |    35 (2.3%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    5 (0.3%)     |    16 (18.8%)    |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   757 (52.7%)   |     6 (7.1%)     |   763 (50.1%)   |           |
|**Num. offenders (all cat.)**    |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   517 (36.0%)   |    33 (38.8%)    |   550 (36.1%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   135 (9.4%)    |    18 (21.2%)    |   153 (10.1%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    23 (1.6%)    |    12 (14.1%)    |    35 (2.3%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    3 (0.2%)     |     4 (4.7%)     |    7 (0.5%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    1 (0.1%)     |     8 (9.4%)     |    9 (0.6%)     |           |
|&nbsp;&nbsp;&nbsp;6              |    1 (0.1%)     |     4 (4.7%)     |    5 (0.3%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   757 (52.7%)   |     6 (7.1%)     |   763 (50.1%)   |           |
|**Had weapon**                   |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;No             |   307 (21.4%)   |    45 (52.9%)    |   352 (23.1%)   |           |
|&nbsp;&nbsp;&nbsp;Yes            |    4 (0.3%)     |    32 (37.6%)    |    36 (2.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |  1126 (78.4%)   |     8 (9.4%)     |  1134 (74.5%)   |           |
|**Extortion concentration**      |                 |                  |                 |   0.395^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.604 (2.946)  |  0.506 (3.010)   |  0.599 (2.949)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 33.000  |  0.000 - 27.000  | 0.000 - 33.000  |           |
|**Corruption incidence**         |                 |                  |                 |   0.473^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.313 (1.223)  |  0.353 (0.972)   |  0.315 (1.210)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 13.000  |  0.000 - 4.000   | 0.000 - 13.000  |           |
|**Business type**                |                 |                  |                 |   0.947^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    44 (3.1%)    |     1 (1.2%)     |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   221 (15.4%)   |    12 (14.1%)    |   233 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   318 (22.1%)   |    18 (21.2%)    |   336 (22.1%)   |           |
|&nbsp;&nbsp;&nbsp;Other industry |   205 (14.3%)   |    14 (16.5%)    |   219 (14.4%)   |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   514 (35.8%)   |    33 (38.8%)    |   547 (35.9%)   |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |     4 (4.7%)     |    80 (5.3%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    59 (4.1%)    |     3 (3.5%)     |    62 (4.1%)    |           |
|**Business type (all cat.)**     |                 |                  |                 |   0.967^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    44 (3.1%)    |     1 (1.2%)     |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;Mining         |    20 (1.4%)    |     1 (1.2%)     |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Construction   |   185 (12.9%)   |    13 (15.3%)    |   198 (13.0%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   318 (22.1%)   |    18 (21.2%)    |   336 (22.1%)   |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    59 (4.1%)    |     3 (3.5%)     |    62 (4.1%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |     4 (4.7%)     |    80 (5.3%)    |           |
|&nbsp;&nbsp;&nbsp;Media          |    28 (1.9%)    |     0 (0.0%)     |    28 (1.8%)    |           |
|&nbsp;&nbsp;&nbsp;Finance        |    19 (1.3%)    |     2 (2.4%)     |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    25 (1.7%)    |     1 (1.2%)     |    26 (1.7%)    |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    22 (1.5%)    |     1 (1.2%)     |    23 (1.5%)    |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |   120 (8.4%)    |     6 (7.1%)     |   126 (8.3%)    |           |
|&nbsp;&nbsp;&nbsp;Education      |    23 (1.6%)    |     2 (2.4%)     |    25 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;Health         |   246 (17.1%)   |    19 (22.4%)    |   265 (17.4%)   |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    24 (1.7%)    |     1 (1.2%)     |    25 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   221 (15.4%)   |    12 (14.1%)    |   233 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Other          |    7 (0.5%)     |     1 (1.2%)     |    8 (0.5%)     |           |
|**Business size**                |                 |                  |                 |   0.027^1^|
|&nbsp;&nbsp;&nbsp;Large          |   357 (24.8%)   |    16 (18.8%)    |   373 (24.5%)   |           |
|&nbsp;&nbsp;&nbsp;Medium         |   345 (24.0%)   |    24 (28.2%)    |   369 (24.2%)   |           |
|&nbsp;&nbsp;&nbsp;Small          |   324 (22.5%)   |    29 (34.1%)    |   353 (23.2%)   |           |
|&nbsp;&nbsp;&nbsp;Micro          |   411 (28.6%)   |    16 (18.8%)    |   427 (28.1%)   |           |
|**Age (quintiles)**              |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   324 (22.5%)   |    35 (41.2%)    |   359 (23.6%)   |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |   288 (20.0%)   |     8 (9.4%)     |   296 (19.4%)   |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |   283 (19.7%)   |    14 (16.5%)    |   297 (19.5%)   |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |   294 (20.5%)   |    12 (14.1%)    |   306 (20.1%)   |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |   248 (17.3%)   |    16 (18.8%)    |   264 (17.3%)   |           |
|**Age**                          |                 |                  |                 |   0.066^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 20.443 (12.573) | 18.047 (14.294)  | 20.309 (12.682) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  |  0.000 - 43.000  | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
desc_formula5 <-  update(desc_formula3, extortion_type_wocp ~ .)


desc_table5 <- tableby(desc_formula5,
                        data = subset(enve_nona, extortion_type_wocp != "Other"),
                        control = my_controls)

summary(desc_table5,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 | Remote (N=1437) | In person (N=78) | Total (N=1515)  |    p value|
|:--------------------------------|:---------------:|:----------------:|:---------------:|----------:|
|**Num. offenders**               |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   517 (36.0%)   |    29 (37.2%)    |   546 (36.0%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   135 (9.4%)    |    16 (20.5%)    |   151 (10.0%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    23 (1.6%)    |    11 (14.1%)    |    34 (2.2%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    5 (0.3%)     |    16 (20.5%)    |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   757 (52.7%)   |     6 (7.7%)     |   763 (50.4%)   |           |
|**Num. offenders (all cat.)**    |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   517 (36.0%)   |    29 (37.2%)    |   546 (36.0%)   |           |
|&nbsp;&nbsp;&nbsp;2              |   135 (9.4%)    |    16 (20.5%)    |   151 (10.0%)   |           |
|&nbsp;&nbsp;&nbsp;3              |    23 (1.6%)    |    11 (14.1%)    |    34 (2.2%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    3 (0.2%)     |     4 (5.1%)     |    7 (0.5%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    1 (0.1%)     |    8 (10.3%)     |    9 (0.6%)     |           |
|&nbsp;&nbsp;&nbsp;6              |    1 (0.1%)     |     4 (5.1%)     |    5 (0.3%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |   757 (52.7%)   |     6 (7.7%)     |   763 (50.4%)   |           |
|**Had weapon**                   |                 |                  |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;No             |   307 (21.4%)   |    40 (51.3%)    |   347 (22.9%)   |           |
|&nbsp;&nbsp;&nbsp;Yes            |    4 (0.3%)     |    30 (38.5%)    |    34 (2.2%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |  1126 (78.4%)   |    8 (10.3%)     |  1134 (74.9%)   |           |
|**Extortion concentration**      |                 |                  |                 |   0.087^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.604 (2.946)  |  0.397 (3.064)   |  0.593 (2.952)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 33.000  |  0.000 - 27.000  | 0.000 - 33.000  |           |
|**Corruption incidence**         |                 |                  |                 |   0.506^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.313 (1.223)  |  0.333 (0.921)   |  0.314 (1.209)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 13.000  |  0.000 - 4.000   | 0.000 - 13.000  |           |
|**Business type**                |                 |                  |                 |   0.745^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    44 (3.1%)    |     1 (1.3%)     |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   221 (15.4%)   |    11 (14.1%)    |   232 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   318 (22.1%)   |    15 (19.2%)    |   333 (22.0%)   |           |
|&nbsp;&nbsp;&nbsp;Other industry |   205 (14.3%)   |    14 (17.9%)    |   219 (14.5%)   |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   514 (35.8%)   |    32 (41.0%)    |   546 (36.0%)   |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |     2 (2.6%)     |    78 (5.1%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    59 (4.1%)    |     3 (3.8%)     |    62 (4.1%)    |           |
|**Business type (all cat.)**     |                 |                  |                 |   0.889^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    44 (3.1%)    |     1 (1.3%)     |    45 (3.0%)    |           |
|&nbsp;&nbsp;&nbsp;Mining         |    20 (1.4%)    |     1 (1.3%)     |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Construction   |   185 (12.9%)   |    13 (16.7%)    |   198 (13.1%)   |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   318 (22.1%)   |    15 (19.2%)    |   333 (22.0%)   |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    59 (4.1%)    |     3 (3.8%)     |    62 (4.1%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    76 (5.3%)    |     2 (2.6%)     |    78 (5.1%)    |           |
|&nbsp;&nbsp;&nbsp;Media          |    28 (1.9%)    |     0 (0.0%)     |    28 (1.8%)    |           |
|&nbsp;&nbsp;&nbsp;Finance        |    19 (1.3%)    |     2 (2.6%)     |    21 (1.4%)    |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    25 (1.7%)    |     1 (1.3%)     |    26 (1.7%)    |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    22 (1.5%)    |     1 (1.3%)     |    23 (1.5%)    |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |   120 (8.4%)    |     6 (7.7%)     |   126 (8.3%)    |           |
|&nbsp;&nbsp;&nbsp;Education      |    23 (1.6%)    |     1 (1.3%)     |    24 (1.6%)    |           |
|&nbsp;&nbsp;&nbsp;Health         |   246 (17.1%)   |    19 (24.4%)    |   265 (17.5%)   |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    24 (1.7%)    |     1 (1.3%)     |    25 (1.7%)    |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   221 (15.4%)   |    11 (14.1%)    |   232 (15.3%)   |           |
|&nbsp;&nbsp;&nbsp;Other          |    7 (0.5%)     |     1 (1.3%)     |    8 (0.5%)     |           |
|**Business size**                |                 |                  |                 |   0.060^1^|
|&nbsp;&nbsp;&nbsp;Large          |   357 (24.8%)   |    14 (17.9%)    |   371 (24.5%)   |           |
|&nbsp;&nbsp;&nbsp;Medium         |   345 (24.0%)   |    22 (28.2%)    |   367 (24.2%)   |           |
|&nbsp;&nbsp;&nbsp;Small          |   324 (22.5%)   |    26 (33.3%)    |   350 (23.1%)   |           |
|&nbsp;&nbsp;&nbsp;Micro          |   411 (28.6%)   |    16 (20.5%)    |   427 (28.2%)   |           |
|**Age (quintiles)**              |                 |                  |                 |   0.020^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   324 (22.5%)   |    29 (37.2%)    |   353 (23.3%)   |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |   288 (20.0%)   |    8 (10.3%)     |   296 (19.5%)   |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |   283 (19.7%)   |    14 (17.9%)    |   297 (19.6%)   |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |   294 (20.5%)   |    12 (15.4%)    |   306 (20.2%)   |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |   248 (17.3%)   |    15 (19.2%)    |   263 (17.4%)   |           |
|**Age**                          |                 |                  |                 |   0.240^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 20.443 (12.573) | 18.885 (14.173)  | 20.362 (12.660) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  |  0.000 - 43.000  | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
## Extortion type and include compliance

desc_formula6 <-  update(desc_formula3, complied_bin_NA ~ .)


# EDA for remote extortions
desc_table6 <- enve_nona %>%
  filter(extortion_type == "Remote") %>%
  tableby(desc_formula6,
          data = .,
          control = my_controls)

summary(desc_table6,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |   No (N=1384)   |   Yes (N=53)    | Total (N=1437)  |  p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|--------:|
|**Num. offenders**               |                 |                 |                 | 0.015^1^|
|&nbsp;&nbsp;&nbsp;1              |   499 (36.1%)   |   18 (34.0%)    |   517 (36.0%)   |         |
|&nbsp;&nbsp;&nbsp;2              |   127 (9.2%)    |    8 (15.1%)    |   135 (9.4%)    |         |
|&nbsp;&nbsp;&nbsp;3              |    20 (1.4%)    |    3 (5.7%)     |    23 (1.6%)    |         |
|&nbsp;&nbsp;&nbsp;4+             |    4 (0.3%)     |    1 (1.9%)     |    5 (0.3%)     |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |   734 (53.0%)   |   23 (43.4%)    |   757 (52.7%)   |         |
|**Num. offenders (all cat.)**    |                 |                 |                 | 0.014^1^|
|&nbsp;&nbsp;&nbsp;1              |   499 (36.1%)   |   18 (34.0%)    |   517 (36.0%)   |         |
|&nbsp;&nbsp;&nbsp;2              |   127 (9.2%)    |    8 (15.1%)    |   135 (9.4%)    |         |
|&nbsp;&nbsp;&nbsp;3              |    20 (1.4%)    |    3 (5.7%)     |    23 (1.6%)    |         |
|&nbsp;&nbsp;&nbsp;4              |    2 (0.1%)     |    1 (1.9%)     |    3 (0.2%)     |         |
|&nbsp;&nbsp;&nbsp;5              |    1 (0.1%)     |    0 (0.0%)     |    1 (0.1%)     |         |
|&nbsp;&nbsp;&nbsp;6              |    1 (0.1%)     |    0 (0.0%)     |    1 (0.1%)     |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |   734 (53.0%)   |   23 (43.4%)    |   757 (52.7%)   |         |
|**Had weapon**                   |                 |                 |                 | 0.791^1^|
|&nbsp;&nbsp;&nbsp;No             |   294 (21.2%)   |   13 (24.5%)    |   307 (21.4%)   |         |
|&nbsp;&nbsp;&nbsp;Yes            |    4 (0.3%)     |    0 (0.0%)     |    4 (0.3%)     |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |  1086 (78.5%)   |   40 (75.5%)    |  1126 (78.4%)   |         |
|**Extortion concentration**      |                 |                 |                 | 0.391^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.602 (2.951)  |  0.660 (2.848)  |  0.604 (2.946)  |         |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 33.000  | 0.000 - 20.000  | 0.000 - 33.000  |         |
|**Corruption incidence**         |                 |                 |                 | 0.026^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.298 (1.202)  |  0.698 (1.648)  |  0.313 (1.223)  |         |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 13.000  |  0.000 - 7.000  | 0.000 - 13.000  |         |
|**Business type**                |                 |                 |                 | 0.548^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    42 (3.0%)    |    2 (3.8%)     |    44 (3.1%)    |         |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   215 (15.5%)   |    6 (11.3%)    |   221 (15.4%)   |         |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   307 (22.2%)   |   11 (20.8%)    |   318 (22.1%)   |         |
|&nbsp;&nbsp;&nbsp;Other industry |   201 (14.5%)   |    4 (7.5%)     |   205 (14.3%)   |         |
|&nbsp;&nbsp;&nbsp;Other serv.    |   491 (35.5%)   |   23 (43.4%)    |   514 (35.8%)   |         |
|&nbsp;&nbsp;&nbsp;Transport      |    73 (5.3%)    |    3 (5.7%)     |    76 (5.3%)    |         |
|&nbsp;&nbsp;&nbsp;Wholesale      |    55 (4.0%)    |    4 (7.5%)     |    59 (4.1%)    |         |
|**Business type (all cat.)**     |                 |                 |                 | 0.131^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    42 (3.0%)    |    2 (3.8%)     |    44 (3.1%)    |         |
|&nbsp;&nbsp;&nbsp;Mining         |    19 (1.4%)    |    1 (1.9%)     |    20 (1.4%)    |         |
|&nbsp;&nbsp;&nbsp;Construction   |   182 (13.2%)   |    3 (5.7%)     |   185 (12.9%)   |         |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   307 (22.2%)   |   11 (20.8%)    |   318 (22.1%)   |         |
|&nbsp;&nbsp;&nbsp;Wholesale      |    55 (4.0%)    |    4 (7.5%)     |    59 (4.1%)    |         |
|&nbsp;&nbsp;&nbsp;Transport      |    73 (5.3%)    |    3 (5.7%)     |    76 (5.3%)    |         |
|&nbsp;&nbsp;&nbsp;Media          |    25 (1.8%)    |    3 (5.7%)     |    28 (1.9%)    |         |
|&nbsp;&nbsp;&nbsp;Finance        |    16 (1.2%)    |    3 (5.7%)     |    19 (1.3%)    |         |
|&nbsp;&nbsp;&nbsp;Real estate    |    23 (1.7%)    |    2 (3.8%)     |    25 (1.7%)    |         |
|&nbsp;&nbsp;&nbsp;Prof. services |    22 (1.6%)    |    0 (0.0%)     |    22 (1.5%)    |         |
|&nbsp;&nbsp;&nbsp;Maintenance    |   114 (8.2%)    |    6 (11.3%)    |   120 (8.4%)    |         |
|&nbsp;&nbsp;&nbsp;Education      |    23 (1.7%)    |    0 (0.0%)     |    23 (1.6%)    |         |
|&nbsp;&nbsp;&nbsp;Health         |   237 (17.1%)   |    9 (17.0%)    |   246 (17.1%)   |         |
|&nbsp;&nbsp;&nbsp;Leisure        |    24 (1.7%)    |    0 (0.0%)     |    24 (1.7%)    |         |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   215 (15.5%)   |    6 (11.3%)    |   221 (15.4%)   |         |
|&nbsp;&nbsp;&nbsp;Other          |    7 (0.5%)     |    0 (0.0%)     |    7 (0.5%)     |         |
|**Business size**                |                 |                 |                 | 0.198^1^|
|&nbsp;&nbsp;&nbsp;Large          |   347 (25.1%)   |   10 (18.9%)    |   357 (24.8%)   |         |
|&nbsp;&nbsp;&nbsp;Medium         |   332 (24.0%)   |   13 (24.5%)    |   345 (24.0%)   |         |
|&nbsp;&nbsp;&nbsp;Small          |   306 (22.1%)   |   18 (34.0%)    |   324 (22.5%)   |         |
|&nbsp;&nbsp;&nbsp;Micro          |   399 (28.8%)   |   12 (22.6%)    |   411 (28.6%)   |         |
|**Age (quintiles)**              |                 |                 |                 | 0.137^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   318 (23.0%)   |    6 (11.3%)    |   324 (22.5%)   |         |
|&nbsp;&nbsp;&nbsp;(8,16]         |   276 (19.9%)   |   12 (22.6%)    |   288 (20.0%)   |         |
|&nbsp;&nbsp;&nbsp;(16,25]        |   269 (19.4%)   |   14 (26.4%)    |   283 (19.7%)   |         |
|&nbsp;&nbsp;&nbsp;(25,34]        |   286 (20.7%)   |    8 (15.1%)    |   294 (20.5%)   |         |
|&nbsp;&nbsp;&nbsp;(34,43]        |   235 (17.0%)   |   13 (24.5%)    |   248 (17.3%)   |         |
|**Age**                          |                 |                 |                 | 0.150^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 20.350 (12.571) | 22.849 (12.524) | 20.443 (12.573) |         |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  | 2.000 - 43.000  | 0.000 - 43.000  |         |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
# EDA for street extortions
desc_table7 <- enve_nona %>%
  filter(extortion_type == "Street") %>%
  tableby(desc_formula6,
          data = .,
          control = my_controls)

summary(desc_table7,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |    No (N=44)    |   Yes (N=18)    |  Total (N=62)   |    p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|----------:|
|**Num. offenders**               |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   24 (54.5%)    |    2 (11.1%)    |   26 (41.9%)    |           |
|&nbsp;&nbsp;&nbsp;2              |    5 (11.4%)    |    8 (44.4%)    |   13 (21.0%)    |           |
|&nbsp;&nbsp;&nbsp;3              |   10 (22.7%)    |    0 (0.0%)     |   10 (16.1%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    3 (6.8%)     |    6 (33.3%)    |    9 (14.5%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    2 (4.5%)     |    2 (11.1%)    |    4 (6.5%)     |           |
|**Num. offenders (all cat.)**    |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   24 (54.5%)    |    2 (11.1%)    |   26 (41.9%)    |           |
|&nbsp;&nbsp;&nbsp;2              |    5 (11.4%)    |    8 (44.4%)    |   13 (21.0%)    |           |
|&nbsp;&nbsp;&nbsp;3              |   10 (22.7%)    |    0 (0.0%)     |   10 (16.1%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    1 (2.3%)     |    2 (11.1%)    |    3 (4.8%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    0 (0.0%)     |    3 (16.7%)    |    3 (4.8%)     |           |
|&nbsp;&nbsp;&nbsp;6              |    2 (4.5%)     |    1 (5.6%)     |    3 (4.8%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    2 (4.5%)     |    2 (11.1%)    |    4 (6.5%)     |           |
|**Had weapon**                   |                 |                 |                 |   0.040^1^|
|&nbsp;&nbsp;&nbsp;No             |   30 (68.2%)    |    6 (33.3%)    |   36 (58.1%)    |           |
|&nbsp;&nbsp;&nbsp;Yes            |   11 (25.0%)    |    9 (50.0%)    |   20 (32.3%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    3 (6.8%)     |    3 (16.7%)    |    6 (9.7%)     |           |
|**Extortion concentration**      |                 |                 |                 |   0.884^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.636 (4.070)  |  0.056 (0.236)  |  0.468 (3.429)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 27.000  |  0.000 - 1.000  | 0.000 - 27.000  |           |
|**Corruption incidence**         |                 |                 |                 |   0.717^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.091 (0.291)  |  0.389 (1.145)  |  0.177 (0.666)  |           |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 1.000  |  0.000 - 4.000  |  0.000 - 4.000  |           |
|**Business type**                |                 |                 |                 |   0.427^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |    9 (20.5%)    |    1 (5.6%)     |   10 (16.1%)    |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    7 (15.9%)    |    6 (33.3%)    |   13 (21.0%)    |           |
|&nbsp;&nbsp;&nbsp;Other industry |    5 (11.4%)    |    3 (16.7%)    |    8 (12.9%)    |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   18 (40.9%)    |    7 (38.9%)    |   25 (40.3%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    1 (2.3%)     |    1 (5.6%)     |    2 (3.2%)     |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    3 (6.8%)     |    0 (0.0%)     |    3 (4.8%)     |           |
|**Business type (all cat.)**     |                 |                 |                 |           |
|&nbsp;&nbsp;&nbsp;Retail         |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;Mining         |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;Construction   |    4 (9.1%)     |    3 (16.7%)    |    7 (11.3%)    |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    7 (15.9%)    |    6 (33.3%)    |   13 (21.0%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    3 (6.8%)     |    0 (0.0%)     |    3 (4.8%)     |           |
|&nbsp;&nbsp;&nbsp;Transport      |    1 (2.3%)     |    1 (5.6%)     |    2 (3.2%)     |           |
|&nbsp;&nbsp;&nbsp;Media          |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |           |
|&nbsp;&nbsp;&nbsp;Finance        |    0 (0.0%)     |    2 (11.1%)    |    2 (3.2%)     |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |    4 (9.1%)     |    1 (5.6%)     |    5 (8.1%)     |           |
|&nbsp;&nbsp;&nbsp;Education      |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;Health         |    9 (20.5%)    |    4 (22.2%)    |   13 (21.0%)    |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |    9 (20.5%)    |    1 (5.6%)     |   10 (16.1%)    |           |
|&nbsp;&nbsp;&nbsp;Other          |    1 (2.3%)     |    0 (0.0%)     |    1 (1.6%)     |           |
|**Business size**                |                 |                 |                 |   0.079^1^|
|&nbsp;&nbsp;&nbsp;Large          |    6 (13.6%)    |    6 (33.3%)    |   12 (19.4%)    |           |
|&nbsp;&nbsp;&nbsp;Medium         |   16 (36.4%)    |    3 (16.7%)    |   19 (30.6%)    |           |
|&nbsp;&nbsp;&nbsp;Small          |   13 (29.5%)    |    8 (44.4%)    |   21 (33.9%)    |           |
|&nbsp;&nbsp;&nbsp;Micro          |    9 (20.5%)    |    1 (5.6%)     |   10 (16.1%)    |           |
|**Age (quintiles)**              |                 |                 |                 |   0.639^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   18 (40.9%)    |    5 (27.8%)    |   23 (37.1%)    |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |    5 (11.4%)    |    3 (16.7%)    |    8 (12.9%)    |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |    8 (18.2%)    |    3 (16.7%)    |   11 (17.7%)    |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |    7 (15.9%)    |    2 (11.1%)    |    9 (14.5%)    |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |    6 (13.6%)    |    5 (27.8%)    |   11 (17.7%)    |           |
|**Age**                          |                 |                 |                 |   0.360^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 17.295 (13.624) | 20.944 (14.485) | 18.355 (13.860) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  | 0.000 - 42.000  | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
# EDA for Premises extortions
desc_table8 <- enve_nona %>%
  filter(extortion_type == "Premises") %>%
  tableby(desc_formula6,
          data = .,
          control = my_controls)

summary(desc_table8,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |    No (N=3)     |   Yes (N=13)    |  Total (N=16)   |  p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|--------:|
|**Num. offenders**               |                 |                 |                 | 0.080^1^|
|&nbsp;&nbsp;&nbsp;1              |    0 (0.0%)     |    3 (23.1%)    |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;2              |    1 (33.3%)    |    2 (15.4%)    |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;3              |    1 (33.3%)    |    0 (0.0%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;4+             |    0 (0.0%)     |    7 (53.8%)    |    7 (43.8%)    |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |    1 (33.3%)    |    1 (7.7%)     |    2 (12.5%)    |         |
|**Num. offenders (all cat.)**    |                 |                 |                 | 0.214^1^|
|&nbsp;&nbsp;&nbsp;1              |    0 (0.0%)     |    3 (23.1%)    |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;2              |    1 (33.3%)    |    2 (15.4%)    |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;3              |    1 (33.3%)    |    0 (0.0%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;4              |    0 (0.0%)     |    1 (7.7%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;5              |    0 (0.0%)     |    5 (38.5%)    |    5 (31.2%)    |         |
|&nbsp;&nbsp;&nbsp;6              |    0 (0.0%)     |    1 (7.7%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |    1 (33.3%)    |    1 (7.7%)     |    2 (12.5%)    |         |
|**Had weapon**                   |                 |                 |                 | 0.389^1^|
|&nbsp;&nbsp;&nbsp;No             |    1 (33.3%)    |    3 (23.1%)    |    4 (25.0%)    |         |
|&nbsp;&nbsp;&nbsp;Yes            |    1 (33.3%)    |    9 (69.2%)    |   10 (62.5%)    |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |    1 (33.3%)    |    1 (7.7%)     |    2 (12.5%)    |         |
|**Extortion concentration**      |                 |                 |                 | 0.631^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.000 (0.000)  |  0.154 (0.555)  |  0.125 (0.500)  |         |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 0.000  |  0.000 - 2.000  |  0.000 - 2.000  |         |
|**Corruption incidence**         |                 |                 |                 | 0.210^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.000 (0.000)  |  1.154 (1.519)  |  0.938 (1.436)  |         |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 0.000  |  0.000 - 3.000  |  0.000 - 3.000  |         |
|**Business type**                |                 |                 |                 |         |
|&nbsp;&nbsp;&nbsp;Retail         |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |    1 (33.3%)    |    0 (0.0%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    0 (0.0%)     |    2 (15.4%)    |    2 (12.5%)    |         |
|&nbsp;&nbsp;&nbsp;Other industry |    0 (0.0%)     |    6 (46.2%)    |    6 (37.5%)    |         |
|&nbsp;&nbsp;&nbsp;Other serv.    |    2 (66.7%)    |    5 (38.5%)    |    7 (43.8%)    |         |
|&nbsp;&nbsp;&nbsp;Transport      |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Wholesale      |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|**Business type (all cat.)**     |                 |                 |                 |         |
|&nbsp;&nbsp;&nbsp;Retail         |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Mining         |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Construction   |    0 (0.0%)     |    6 (46.2%)    |    6 (37.5%)    |         |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    0 (0.0%)     |    2 (15.4%)    |    2 (12.5%)    |         |
|&nbsp;&nbsp;&nbsp;Wholesale      |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Transport      |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Media          |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Finance        |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Real estate    |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Prof. services |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Maintenance    |    0 (0.0%)     |    1 (7.7%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;Education      |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;Health         |    2 (66.7%)    |    4 (30.8%)    |    6 (37.5%)    |         |
|&nbsp;&nbsp;&nbsp;Leisure        |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |    1 (33.3%)    |    0 (0.0%)     |    1 (6.2%)     |         |
|&nbsp;&nbsp;&nbsp;Other          |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|**Business size**                |                 |                 |                 | 0.412^1^|
|&nbsp;&nbsp;&nbsp;Large          |    0 (0.0%)     |    2 (15.4%)    |    2 (12.5%)    |         |
|&nbsp;&nbsp;&nbsp;Medium         |    1 (33.3%)    |    2 (15.4%)    |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;Small          |    0 (0.0%)     |    5 (38.5%)    |    5 (31.2%)    |         |
|&nbsp;&nbsp;&nbsp;Micro          |    2 (66.7%)    |    4 (30.8%)    |    6 (37.5%)    |         |
|**Age (quintiles)**              |                 |                 |                 |         |
|&nbsp;&nbsp;&nbsp;[0,8]          |    0 (0.0%)     |    6 (46.2%)    |    6 (37.5%)    |         |
|&nbsp;&nbsp;&nbsp;(8,16]         |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|&nbsp;&nbsp;&nbsp;(16,25]        |    1 (33.3%)    |    2 (15.4%)    |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;(25,34]        |    2 (66.7%)    |    1 (7.7%)     |    3 (18.8%)    |         |
|&nbsp;&nbsp;&nbsp;(34,43]        |    0 (0.0%)     |    4 (30.8%)    |    4 (25.0%)    |         |
|**Age**                          |                 |                 |                 | 0.584^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 27.333 (7.371)  | 19.462 (16.845) | 20.938 (15.631) |         |
|&nbsp;&nbsp;&nbsp;Range          | 19.000 - 33.000 | 0.000 - 42.000  | 0.000 - 42.000  |         |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
# EDA for Cobro de piso extortions
desc_table9 <- enve_nona %>%
  filter(extortion_type == "Cobro de piso") %>%
  tableby(desc_formula6,
          data = .,
          control = my_controls)

summary(desc_table9,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |   No (N=3)    |    Yes (N=4)    |  Total (N=7)   |  p value|
|:--------------------------------|:-------------:|:---------------:|:--------------:|--------:|
|**Num. offenders**               |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;1              |   2 (66.7%)   |    2 (50.0%)    |   4 (57.1%)    |         |
|&nbsp;&nbsp;&nbsp;2              |   1 (33.3%)   |    1 (25.0%)    |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;3              |   0 (0.0%)    |    1 (25.0%)    |   1 (14.3%)    |         |
|&nbsp;&nbsp;&nbsp;4+             |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|**Num. offenders (all cat.)**    |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;1              |   2 (66.7%)   |    2 (50.0%)    |   4 (57.1%)    |         |
|&nbsp;&nbsp;&nbsp;2              |   1 (33.3%)   |    1 (25.0%)    |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;3              |   0 (0.0%)    |    1 (25.0%)    |   1 (14.3%)    |         |
|&nbsp;&nbsp;&nbsp;4              |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;5              |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;6              |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|**Had weapon**                   |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;No             |   2 (66.7%)   |    3 (75.0%)    |   5 (71.4%)    |         |
|&nbsp;&nbsp;&nbsp;Yes            |   1 (33.3%)   |    1 (25.0%)    |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;DK/DA          |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|**Extortion concentration**      |               |                 |                | 0.066^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 0.000 (0.000) |  3.000 (2.000)  | 1.714 (2.138)  |         |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 0.000 |  0.000 - 4.000  | 0.000 - 4.000  |         |
|**Corruption incidence**         |               |                 |                | 0.248^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 1.333 (2.309) |  0.000 (0.000)  | 0.571 (1.512)  |         |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 4.000 |  0.000 - 0.000  | 0.000 - 4.000  |         |
|**Business type**                |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;Retail         |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   0 (0.0%)    |    1 (25.0%)    |   1 (14.3%)    |         |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   0 (0.0%)    |    3 (75.0%)    |   3 (42.9%)    |         |
|&nbsp;&nbsp;&nbsp;Other industry |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Other serv.    |   1 (33.3%)   |    0 (0.0%)     |   1 (14.3%)    |         |
|&nbsp;&nbsp;&nbsp;Transport      |   2 (66.7%)   |    0 (0.0%)     |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;Wholesale      |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|**Business type (all cat.)**     |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;Retail         |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Mining         |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Construction   |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Manufacturing  |   0 (0.0%)    |    3 (75.0%)    |   3 (42.9%)    |         |
|&nbsp;&nbsp;&nbsp;Wholesale      |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Transport      |   2 (66.7%)   |    0 (0.0%)     |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;Media          |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Finance        |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Real estate    |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Prof. services |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Maintenance    |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Education      |   1 (33.3%)   |    0 (0.0%)     |   1 (14.3%)    |         |
|&nbsp;&nbsp;&nbsp;Health         |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;Leisure        |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   0 (0.0%)    |    1 (25.0%)    |   1 (14.3%)    |         |
|&nbsp;&nbsp;&nbsp;Other          |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|**Business size**                |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;Large          |   1 (33.3%)   |    1 (25.0%)    |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;Medium         |   2 (66.7%)   |    0 (0.0%)     |   2 (28.6%)    |         |
|&nbsp;&nbsp;&nbsp;Small          |   0 (0.0%)    |    3 (75.0%)    |   3 (42.9%)    |         |
|&nbsp;&nbsp;&nbsp;Micro          |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|**Age (quintiles)**              |               |                 |                |         |
|&nbsp;&nbsp;&nbsp;[0,8]          |  3 (100.0%)   |    3 (75.0%)    |   6 (85.7%)    |         |
|&nbsp;&nbsp;&nbsp;(8,16]         |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;(16,25]        |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;(25,34]        |   0 (0.0%)    |    0 (0.0%)     |    0 (0.0%)    |         |
|&nbsp;&nbsp;&nbsp;(34,43]        |   0 (0.0%)    |    1 (25.0%)    |   1 (14.3%)    |         |
|**Age**                          |               |                 |                | 0.271^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 5.667 (2.082) | 11.000 (18.000) | 8.714 (13.099) |         |
|&nbsp;&nbsp;&nbsp;Range          | 4.000 - 8.000 | 2.000 - 38.000  | 2.000 - 38.000 |         |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
# EDA for In person extortions
desc_table10 <- enve_nona %>%
  filter(extortion_type_bin == "In person") %>%
  tableby(desc_formula6,
          data = .,
          control = my_controls)

summary(desc_table10,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |    No (N=50)    |   Yes (N=35)    |  Total (N=85)   |    p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|----------:|
|**Num. offenders**               |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   26 (52.0%)    |    7 (20.0%)    |   33 (38.8%)    |           |
|&nbsp;&nbsp;&nbsp;2              |    7 (14.0%)    |   11 (31.4%)    |   18 (21.2%)    |           |
|&nbsp;&nbsp;&nbsp;3              |   11 (22.0%)    |    1 (2.9%)     |   12 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    3 (6.0%)     |   13 (37.1%)    |   16 (18.8%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    3 (6.0%)     |    3 (8.6%)     |    6 (7.1%)     |           |
|**Num. offenders (all cat.)**    |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   26 (52.0%)    |    7 (20.0%)    |   33 (38.8%)    |           |
|&nbsp;&nbsp;&nbsp;2              |    7 (14.0%)    |   11 (31.4%)    |   18 (21.2%)    |           |
|&nbsp;&nbsp;&nbsp;3              |   11 (22.0%)    |    1 (2.9%)     |   12 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    1 (2.0%)     |    3 (8.6%)     |    4 (4.7%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    0 (0.0%)     |    8 (22.9%)    |    8 (9.4%)     |           |
|&nbsp;&nbsp;&nbsp;6              |    2 (4.0%)     |    2 (5.7%)     |    4 (4.7%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    3 (6.0%)     |    3 (8.6%)     |    6 (7.1%)     |           |
|**Had weapon**                   |                 |                 |                 |   0.014^1^|
|&nbsp;&nbsp;&nbsp;No             |   33 (66.0%)    |   12 (34.3%)    |   45 (52.9%)    |           |
|&nbsp;&nbsp;&nbsp;Yes            |   13 (26.0%)    |   19 (54.3%)    |   32 (37.6%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    4 (8.0%)     |    4 (11.4%)    |    8 (9.4%)     |           |
|**Extortion concentration**      |                 |                 |                 |   0.094^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.560 (3.818)  |  0.429 (1.170)  |  0.506 (3.010)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 27.000  |  0.000 - 4.000  | 0.000 - 27.000  |           |
|**Corruption incidence**         |                 |                 |                 |   0.146^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.160 (0.618)  |  0.629 (1.285)  |  0.353 (0.972)  |           |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 4.000  |  0.000 - 4.000  |  0.000 - 4.000  |           |
|**Business type**                |                 |                 |                 |   0.051^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   10 (20.0%)    |    2 (5.7%)     |   12 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    7 (14.0%)    |   11 (31.4%)    |   18 (21.2%)    |           |
|&nbsp;&nbsp;&nbsp;Other industry |    5 (10.0%)    |    9 (25.7%)    |   14 (16.5%)    |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   21 (42.0%)    |   12 (34.3%)    |   33 (38.8%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    3 (6.0%)     |    1 (2.9%)     |    4 (4.7%)     |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    3 (6.0%)     |    0 (0.0%)     |    3 (3.5%)     |           |
|**Business type (all cat.)**     |                 |                 |                 |           |
|&nbsp;&nbsp;&nbsp;Retail         |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|&nbsp;&nbsp;&nbsp;Mining         |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|&nbsp;&nbsp;&nbsp;Construction   |    4 (8.0%)     |    9 (25.7%)    |   13 (15.3%)    |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    7 (14.0%)    |   11 (31.4%)    |   18 (21.2%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    3 (6.0%)     |    0 (0.0%)     |    3 (3.5%)     |           |
|&nbsp;&nbsp;&nbsp;Transport      |    3 (6.0%)     |    1 (2.9%)     |    4 (4.7%)     |           |
|&nbsp;&nbsp;&nbsp;Media          |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |           |
|&nbsp;&nbsp;&nbsp;Finance        |    0 (0.0%)     |    2 (5.7%)     |    2 (2.4%)     |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |    4 (8.0%)     |    2 (5.7%)     |    6 (7.1%)     |           |
|&nbsp;&nbsp;&nbsp;Education      |    2 (4.0%)     |    0 (0.0%)     |    2 (2.4%)     |           |
|&nbsp;&nbsp;&nbsp;Health         |   11 (22.0%)    |    8 (22.9%)    |   19 (22.4%)    |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   10 (20.0%)    |    2 (5.7%)     |   12 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;Other          |    1 (2.0%)     |    0 (0.0%)     |    1 (1.2%)     |           |
|**Business size**                |                 |                 |                 |   0.035^1^|
|&nbsp;&nbsp;&nbsp;Large          |    7 (14.0%)    |    9 (25.7%)    |   16 (18.8%)    |           |
|&nbsp;&nbsp;&nbsp;Medium         |   19 (38.0%)    |    5 (14.3%)    |   24 (28.2%)    |           |
|&nbsp;&nbsp;&nbsp;Small          |   13 (26.0%)    |   16 (45.7%)    |   29 (34.1%)    |           |
|&nbsp;&nbsp;&nbsp;Micro          |   11 (22.0%)    |    5 (14.3%)    |   16 (18.8%)    |           |
|**Age (quintiles)**              |                 |                 |                 |   0.338^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   21 (42.0%)    |   14 (40.0%)    |   35 (41.2%)    |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |    5 (10.0%)    |    3 (8.6%)     |    8 (9.4%)     |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |    9 (18.0%)    |    5 (14.3%)    |   14 (16.5%)    |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |    9 (18.0%)    |    3 (8.6%)     |   12 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |    6 (12.0%)    |   10 (28.6%)    |   16 (18.8%)    |           |
|**Age**                          |                 |                 |                 |   0.813^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 17.200 (13.406) | 19.257 (15.595) | 18.047 (14.294) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  | 0.000 - 42.000  | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```

```r
# EDA for In person wocp extortions
desc_table11 <- enve_nona %>%
  filter(extortion_type_wocp == "In person") %>%
  tableby(desc_formula6,
          data = .,
          control = my_controls)

summary(desc_table11,
            labelTranslations = my_labels,
            pfootnote = TRUE)
```

```


|                                 |    No (N=47)    |   Yes (N=31)    |  Total (N=78)   |    p value|
|:--------------------------------|:---------------:|:---------------:|:---------------:|----------:|
|**Num. offenders**               |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   24 (51.1%)    |    5 (16.1%)    |   29 (37.2%)    |           |
|&nbsp;&nbsp;&nbsp;2              |    6 (12.8%)    |   10 (32.3%)    |   16 (20.5%)    |           |
|&nbsp;&nbsp;&nbsp;3              |   11 (23.4%)    |    0 (0.0%)     |   11 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;4+             |    3 (6.4%)     |   13 (41.9%)    |   16 (20.5%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    3 (6.4%)     |    3 (9.7%)     |    6 (7.7%)     |           |
|**Num. offenders (all cat.)**    |                 |                 |                 | < 0.001^1^|
|&nbsp;&nbsp;&nbsp;1              |   24 (51.1%)    |    5 (16.1%)    |   29 (37.2%)    |           |
|&nbsp;&nbsp;&nbsp;2              |    6 (12.8%)    |   10 (32.3%)    |   16 (20.5%)    |           |
|&nbsp;&nbsp;&nbsp;3              |   11 (23.4%)    |    0 (0.0%)     |   11 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;4              |    1 (2.1%)     |    3 (9.7%)     |    4 (5.1%)     |           |
|&nbsp;&nbsp;&nbsp;5              |    0 (0.0%)     |    8 (25.8%)    |    8 (10.3%)    |           |
|&nbsp;&nbsp;&nbsp;6              |    2 (4.3%)     |    2 (6.5%)     |    4 (5.1%)     |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    3 (6.4%)     |    3 (9.7%)     |    6 (7.7%)     |           |
|**Had weapon**                   |                 |                 |                 |   0.005^1^|
|&nbsp;&nbsp;&nbsp;No             |   31 (66.0%)    |    9 (29.0%)    |   40 (51.3%)    |           |
|&nbsp;&nbsp;&nbsp;Yes            |   12 (25.5%)    |   18 (58.1%)    |   30 (38.5%)    |           |
|&nbsp;&nbsp;&nbsp;DK/DA          |    4 (8.5%)     |    4 (12.9%)    |    8 (10.3%)    |           |
|**Extortion concentration**      |                 |                 |                 |   0.679^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.596 (3.938)  |  0.097 (0.396)  |  0.397 (3.064)  |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 27.000  |  0.000 - 2.000  | 0.000 - 27.000  |           |
|**Corruption incidence**         |                 |                 |                 |   0.049^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      |  0.085 (0.282)  |  0.710 (1.346)  |  0.333 (0.921)  |           |
|&nbsp;&nbsp;&nbsp;Range          |  0.000 - 1.000  |  0.000 - 4.000  |  0.000 - 4.000  |           |
|**Business type**                |                 |                 |                 |   0.067^1^|
|&nbsp;&nbsp;&nbsp;Retail         |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   10 (21.3%)    |    1 (3.2%)     |   11 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    7 (14.9%)    |    8 (25.8%)    |   15 (19.2%)    |           |
|&nbsp;&nbsp;&nbsp;Other industry |    5 (10.6%)    |    9 (29.0%)    |   14 (17.9%)    |           |
|&nbsp;&nbsp;&nbsp;Other serv.    |   20 (42.6%)    |   12 (38.7%)    |   32 (41.0%)    |           |
|&nbsp;&nbsp;&nbsp;Transport      |    1 (2.1%)     |    1 (3.2%)     |    2 (2.6%)     |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    3 (6.4%)     |    0 (0.0%)     |    3 (3.8%)     |           |
|**Business type (all cat.)**     |                 |                 |                 |           |
|&nbsp;&nbsp;&nbsp;Retail         |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;Mining         |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;Construction   |    4 (8.5%)     |    9 (29.0%)    |   13 (16.7%)    |           |
|&nbsp;&nbsp;&nbsp;Manufacturing  |    7 (14.9%)    |    8 (25.8%)    |   15 (19.2%)    |           |
|&nbsp;&nbsp;&nbsp;Wholesale      |    3 (6.4%)     |    0 (0.0%)     |    3 (3.8%)     |           |
|&nbsp;&nbsp;&nbsp;Transport      |    1 (2.1%)     |    1 (3.2%)     |    2 (2.6%)     |           |
|&nbsp;&nbsp;&nbsp;Media          |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |           |
|&nbsp;&nbsp;&nbsp;Finance        |    0 (0.0%)     |    2 (6.5%)     |    2 (2.6%)     |           |
|&nbsp;&nbsp;&nbsp;Real estate    |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;Prof. services |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;Maintenance    |    4 (8.5%)     |    2 (6.5%)     |    6 (7.7%)     |           |
|&nbsp;&nbsp;&nbsp;Education      |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;Health         |   11 (23.4%)    |    8 (25.8%)    |   19 (24.4%)    |           |
|&nbsp;&nbsp;&nbsp;Leisure        |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|&nbsp;&nbsp;&nbsp;HotelsRestBar  |   10 (21.3%)    |    1 (3.2%)     |   11 (14.1%)    |           |
|&nbsp;&nbsp;&nbsp;Other          |    1 (2.1%)     |    0 (0.0%)     |    1 (1.3%)     |           |
|**Business size**                |                 |                 |                 |   0.109^1^|
|&nbsp;&nbsp;&nbsp;Large          |    6 (12.8%)    |    8 (25.8%)    |   14 (17.9%)    |           |
|&nbsp;&nbsp;&nbsp;Medium         |   17 (36.2%)    |    5 (16.1%)    |   22 (28.2%)    |           |
|&nbsp;&nbsp;&nbsp;Small          |   13 (27.7%)    |   13 (41.9%)    |   26 (33.3%)    |           |
|&nbsp;&nbsp;&nbsp;Micro          |   11 (23.4%)    |    5 (16.1%)    |   16 (20.5%)    |           |
|**Age (quintiles)**              |                 |                 |                 |   0.432^1^|
|&nbsp;&nbsp;&nbsp;[0,8]          |   18 (38.3%)    |   11 (35.5%)    |   29 (37.2%)    |           |
|&nbsp;&nbsp;&nbsp;(8,16]         |    5 (10.6%)    |    3 (9.7%)     |    8 (10.3%)    |           |
|&nbsp;&nbsp;&nbsp;(16,25]        |    9 (19.1%)    |    5 (16.1%)    |   14 (17.9%)    |           |
|&nbsp;&nbsp;&nbsp;(25,34]        |    9 (19.1%)    |    3 (9.7%)     |   12 (15.4%)    |           |
|&nbsp;&nbsp;&nbsp;(34,43]        |    6 (12.8%)    |    9 (29.0%)    |   15 (19.2%)    |           |
|**Age**                          |                 |                 |                 |   0.627^2^|
|&nbsp;&nbsp;&nbsp;Mean (SD)      | 17.936 (13.491) | 20.323 (15.263) | 18.885 (14.173) |           |
|&nbsp;&nbsp;&nbsp;Range          | 0.000 - 43.000  | 0.000 - 42.000  | 0.000 - 43.000  |           |
1. Pearson's Chi-squared test
2. Kruskal-Wallis rank sum test
```





Fit the models




```r
compliance_formula <- complied_bin_NA ~ extortion_type +
                                        n_offenders_NA +
                                        had_weapon_NA +
                                        mylog(extortions) +
                                        mylog(bribes) +
                                        subsector +
                                        size +
                                        yearsquant +
                                        mylog(bribes_abvic, TRUE) +
                                        mylog(armas, TRUE) +
                                        mylog(drogas, TRUE) +
                                        mylog(poblacion, TRUE) +
                                        mylog(N, TRUE) +
                                        scale(General, scale = FALSE) +
                                        scale(Derecho, scale = FALSE)

m1 <- glm(formula = compliance_formula, data = enve_nona, family = binomial())  

summary(m1)
```

```

Call:
glm(formula = compliance_formula, family = binomial(), data = enve_nona)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8338  -0.3051  -0.2185  -0.1562   3.1424  

Coefficients:
                              Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   -4.15071    0.89875  -4.618 3.87e-06 ***
extortion_typeStreet           2.25721    0.47682   4.734 2.20e-06 ***
extortion_typePremises         4.78565    0.80112   5.974 2.32e-09 ***
extortion_typeCobro de piso    3.75758    0.93783   4.007 6.16e-05 ***
n_offenders_NA2                1.11838    0.36951   3.027 0.002473 ** 
n_offenders_NA3                0.59274    0.68435   0.866 0.386414    
n_offenders_NA4+               2.81415    0.81513   3.452 0.000556 ***
n_offenders_NADK/DA           -0.13816    0.34905  -0.396 0.692245    
had_weapon_NAYes              -0.39686    0.67306  -0.590 0.555432    
had_weapon_NADK/DA             0.25936    0.37823   0.686 0.492885    
mylog(extortions)              0.24603    0.21314   1.154 0.248375    
mylog(bribes)                  0.56462    0.24381   2.316 0.020569 *  
subsectorHotelsRestBar        -0.72783    0.86155  -0.845 0.398229    
subsectorManufacturing         0.02265    0.80795   0.028 0.977634    
subsectorOther industry       -0.59301    0.86528  -0.685 0.493133    
subsectorOther serv.           0.10468    0.78703   0.133 0.894193    
subsectorTransport            -0.12103    0.96559  -0.125 0.900250    
subsectorWholesale             0.41082    0.93970   0.437 0.661978    
sizeMedium                    -0.21363    0.38216  -0.559 0.576162    
sizeSmall                      0.44117    0.34967   1.262 0.207064    
sizeMicro                     -0.45746    0.39284  -1.164 0.244227    
yearsquant(8,16]               0.70740    0.44593   1.586 0.112661    
yearsquant(16,25]              0.99203    0.43044   2.305 0.021183 *  
yearsquant(25,34]              0.18938    0.49868   0.380 0.704114    
yearsquant(34,43]              1.14460    0.42468   2.695 0.007034 ** 
mylog(bribes_abvic, TRUE)      0.46051    0.41557   1.108 0.267801    
mylog(armas, TRUE)            -0.66542    0.25800  -2.579 0.009904 ** 
mylog(drogas, TRUE)            0.43029    0.19718   2.182 0.029090 *  
mylog(poblacion, TRUE)         0.43403    0.27909   1.555 0.119913    
mylog(N, TRUE)                 0.44573    0.69664   0.640 0.522290    
scale(General, scale = FALSE) -0.01962    0.01689  -1.162 0.245413    
scale(Derecho, scale = FALSE) -0.01754    0.01364  -1.286 0.198536    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 672.49  on 1521  degrees of freedom
Residual deviance: 490.17  on 1490  degrees of freedom
AIC: 554.17

Number of Fisher Scoring iterations: 6
```

```r
vif(m1, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
extortion_type                2.632461  3        1.175061
n_offenders_NA                2.831341  4        1.138935
had_weapon_NA                 3.572640  2        1.374825
mylog(extortions)             1.073765  1        1.036226
mylog(bribes)                 1.129062  1        1.062573
subsector                     1.410753  6        1.029092
size                          1.210455  3        1.032345
yearsquant                    1.514865  4        1.053287
mylog(bribes_abvic, TRUE)     1.762293  1        1.327514
mylog(armas, TRUE)            4.569498  1        2.137638
mylog(drogas, TRUE)           2.838783  1        1.684869
mylog(poblacion, TRUE)        2.385129  1        1.544386
mylog(N, TRUE)                1.775233  1        1.332379
scale(General, scale = FALSE) 1.331628  1        1.153962
scale(Derecho, scale = FALSE) 1.953600  1        1.397713
```

```r
logLik(m1)
```

```
'log Lik.' -245.0866 (df=32)
```

```r
m1_step <- step(m1, direction = "both")
```

```
Start:  AIC=554.17
complied_bin_NA ~ extortion_type + n_offenders_NA + had_weapon_NA + 
    mylog(extortions) + mylog(bribes) + subsector + size + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + mylog(N, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- subsector                      6   496.58 548.58
- had_weapon_NA                  2   491.06 551.06
- mylog(N, TRUE)                 1   490.58 552.58
- mylog(extortions)              1   491.36 553.36
- mylog(bribes_abvic, TRUE)      1   491.40 553.40
- scale(General, scale = FALSE)  1   491.53 553.53
- scale(Derecho, scale = FALSE)  1   491.82 553.82
<none>                               490.17 554.17
- mylog(poblacion, TRUE)         1   492.56 554.56
- size                           3   496.72 554.72
- mylog(bribes)                  1   494.87 556.87
- mylog(drogas, TRUE)            1   495.01 557.01
- yearsquant                     4   501.39 557.39
- mylog(armas, TRUE)             1   496.71 558.71
- n_offenders_NA                 4   508.55 564.55
- extortion_type                 3   544.27 602.27

Step:  AIC=548.58
complied_bin_NA ~ extortion_type + n_offenders_NA + had_weapon_NA + 
    mylog(extortions) + mylog(bribes) + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- had_weapon_NA                  2   497.35 545.35
- mylog(N, TRUE)                 1   496.87 546.87
- mylog(extortions)              1   497.55 547.55
- scale(General, scale = FALSE)  1   497.79 547.79
- scale(Derecho, scale = FALSE)  1   497.85 547.85
- mylog(bribes_abvic, TRUE)      1   498.14 548.14
- mylog(poblacion, TRUE)         1   498.40 548.40
- size                           3   502.44 548.44
<none>                               496.58 548.58
- yearsquant                     4   506.51 550.51
- mylog(bribes)                  1   501.53 551.53
- mylog(drogas, TRUE)            1   501.64 551.64
- mylog(armas, TRUE)             1   502.54 552.54
+ subsector                      6   490.17 554.17
- n_offenders_NA                 4   514.06 558.06
- extortion_type                 3   550.56 596.56

Step:  AIC=545.35
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(extortions) + 
    mylog(bribes) + size + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- mylog(N, TRUE)                 1   497.60 543.60
- mylog(extortions)              1   498.41 544.41
- scale(General, scale = FALSE)  1   498.56 544.56
- scale(Derecho, scale = FALSE)  1   498.69 544.69
- mylog(bribes_abvic, TRUE)      1   498.87 544.87
- mylog(poblacion, TRUE)         1   499.15 545.15
<none>                               497.35 545.35
- size                           3   503.38 545.38
- yearsquant                     4   507.16 547.16
- mylog(drogas, TRUE)            1   502.35 548.35
- mylog(bribes)                  1   502.52 548.52
+ had_weapon_NA                  2   496.58 548.58
- mylog(armas, TRUE)             1   503.34 549.34
+ subsector                      6   491.06 551.06
- n_offenders_NA                 4   516.64 556.64
- extortion_type                 3   562.32 604.32

Step:  AIC=543.6
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(extortions) + 
    mylog(bribes) + size + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- scale(Derecho, scale = FALSE)  1   498.69 542.69
- mylog(extortions)              1   498.71 542.71
- scale(General, scale = FALSE)  1   498.80 542.80
- mylog(poblacion, TRUE)         1   499.33 543.33
- size                           3   503.59 543.59
<none>                               497.60 543.60
- mylog(bribes_abvic, TRUE)      1   499.93 543.93
+ mylog(N, TRUE)                 1   497.35 545.35
- yearsquant                     4   507.47 545.47
- mylog(drogas, TRUE)            1   502.53 546.53
- mylog(bribes)                  1   502.68 546.68
+ had_weapon_NA                  2   496.87 546.87
- mylog(armas, TRUE)             1   503.42 547.42
+ subsector                      6   491.41 549.41
- n_offenders_NA                 4   516.86 554.86
- extortion_type                 3   562.36 602.36

Step:  AIC=542.69
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(extortions) + 
    mylog(bribes) + size + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + scale(General, scale = FALSE)

                                Df Deviance    AIC
- scale(General, scale = FALSE)  1   499.69 541.69
- mylog(extortions)              1   499.75 541.75
- mylog(poblacion, TRUE)         1   500.19 542.19
<none>                               498.69 542.69
- size                           3   504.72 542.72
+ scale(Derecho, scale = FALSE)  1   497.60 543.60
- mylog(bribes_abvic, TRUE)      1   502.35 544.35
- yearsquant                     4   508.57 544.57
+ mylog(N, TRUE)                 1   498.69 544.69
- mylog(drogas, TRUE)            1   502.80 544.80
- mylog(armas, TRUE)             1   503.43 545.43
- mylog(bribes)                  1   503.52 545.52
+ had_weapon_NA                  2   497.85 545.85
+ subsector                      6   492.78 548.78
- n_offenders_NA                 4   517.83 553.83
- extortion_type                 3   563.49 601.49

Step:  AIC=541.69
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(extortions) + 
    mylog(bribes) + size + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE)

                                Df Deviance    AIC
- mylog(extortions)              1   500.64 540.64
- mylog(poblacion, TRUE)         1   501.15 541.15
- size                           3   505.55 541.55
<none>                               499.69 541.69
+ scale(General, scale = FALSE)  1   498.69 542.69
+ scale(Derecho, scale = FALSE)  1   498.80 542.80
- yearsquant                     4   509.14 543.14
- mylog(drogas, TRUE)            1   503.25 543.25
+ mylog(N, TRUE)                 1   499.69 543.69
- mylog(armas, TRUE)             1   504.12 544.12
+ had_weapon_NA                  2   498.87 544.87
- mylog(bribes)                  1   504.94 544.94
- mylog(bribes_abvic, TRUE)      1   505.76 545.76
+ subsector                      6   493.88 547.88
- n_offenders_NA                 4   518.89 552.89
- extortion_type                 3   564.04 600.04

Step:  AIC=540.64
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE)

                                Df Deviance    AIC
- mylog(poblacion, TRUE)         1   502.02 540.02
<none>                               500.64 540.64
- size                           3   506.89 540.89
+ mylog(extortions)              1   499.69 541.69
+ scale(General, scale = FALSE)  1   499.75 541.75
+ scale(Derecho, scale = FALSE)  1   499.78 541.78
- yearsquant                     4   509.89 541.89
- mylog(drogas, TRUE)            1   504.03 542.03
+ mylog(N, TRUE)                 1   500.64 542.64
- mylog(armas, TRUE)             1   504.93 542.93
- mylog(bribes)                  1   505.58 543.58
+ had_weapon_NA                  2   499.74 543.74
- mylog(bribes_abvic, TRUE)      1   506.47 544.47
+ subsector                      6   495.05 547.05
- n_offenders_NA                 4   520.14 552.14
- extortion_type                 3   565.05 599.05

Step:  AIC=540.02
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE)

                                Df Deviance    AIC
- size                           3   507.80 539.80
<none>                               502.02 540.02
+ mylog(poblacion, TRUE)         1   500.64 540.64
- mylog(armas, TRUE)             1   504.94 540.94
+ mylog(extortions)              1   501.15 541.15
+ scale(General, scale = FALSE)  1   501.17 541.17
+ scale(Derecho, scale = FALSE)  1   501.25 541.25
- yearsquant                     4   511.26 541.26
- mylog(drogas, TRUE)            1   505.55 541.55
+ mylog(N, TRUE)                 1   502.02 542.02
- mylog(bribes)                  1   506.88 542.88
+ had_weapon_NA                  2   501.14 543.14
- mylog(bribes_abvic, TRUE)      1   509.52 545.52
+ subsector                      6   496.82 546.82
- n_offenders_NA                 4   521.07 551.07
- extortion_type                 3   566.19 598.19

Step:  AIC=539.8
complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE)

                                Df Deviance    AIC
<none>                               507.80 539.80
+ size                           3   502.02 540.02
- mylog(armas, TRUE)             1   510.46 540.46
+ mylog(extortions)              1   506.55 540.55
+ mylog(poblacion, TRUE)         1   506.89 540.89
+ scale(Derecho, scale = FALSE)  1   506.90 540.90
- mylog(drogas, TRUE)            1   511.04 541.04
+ scale(General, scale = FALSE)  1   507.12 541.12
- yearsquant                     4   517.15 541.15
+ mylog(N, TRUE)                 1   507.79 541.79
+ had_weapon_NA                  2   506.71 542.71
- mylog(bribes)                  1   513.45 543.45
- mylog(bribes_abvic, TRUE)      1   516.13 546.13
+ subsector                      6   503.11 547.11
- n_offenders_NA                 4   528.37 552.37
- extortion_type                 3   572.64 598.64
```

```r
summary(m1_step)
```

```

Call:
glm(formula = complied_bin_NA ~ extortion_type + n_offenders_NA + 
    mylog(bribes) + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE), family = binomial(), 
    data = enve_nona)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8655  -0.3052  -0.2337  -0.1768   3.0715  

Coefficients:
                            Estimate Std. Error z value Pr(>|z|)    
(Intercept)                 -3.95899    0.40181  -9.853  < 2e-16 ***
extortion_typeStreet         2.08094    0.39459   5.274 1.34e-07 ***
extortion_typePremises       4.26734    0.75057   5.685 1.30e-08 ***
extortion_typeCobro de piso  3.77606    0.86269   4.377 1.20e-05 ***
n_offenders_NA2              1.04516    0.35842   2.916 0.003545 ** 
n_offenders_NA3              0.36447    0.67116   0.543 0.587104    
n_offenders_NA4+             2.40309    0.66763   3.599 0.000319 ***
n_offenders_NADK/DA         -0.06268    0.31264  -0.200 0.841110    
mylog(bribes)                0.61141    0.23576   2.593 0.009503 ** 
yearsquant(8,16]             0.58039    0.43156   1.345 0.178670    
yearsquant(16,25]            0.86919    0.41314   2.104 0.035390 *  
yearsquant(25,34]            0.07932    0.47558   0.167 0.867532    
yearsquant(34,43]            0.96524    0.41002   2.354 0.018566 *  
mylog(bribes_abvic, TRUE)    0.88004    0.30912   2.847 0.004415 ** 
mylog(armas, TRUE)          -0.28641    0.17288  -1.657 0.097580 .  
mylog(drogas, TRUE)          0.30326    0.16735   1.812 0.069971 .  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 672.49  on 1521  degrees of freedom
Residual deviance: 507.80  on 1506  degrees of freedom
AIC: 539.8

Number of Fisher Scoring iterations: 6
```

```r
logLik(m1_step)
```

```
'log Lik.' -253.8986 (df=16)
```

```r
waldtest(m1_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1   1506                          
2   1521 -15 122.18  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_step, m1, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ extortion_type + n_offenders_NA + had_weapon_NA + 
    mylog(extortions) + mylog(bribes) + subsector + size + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + mylog(N, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)
1   1506                     
2   1490 16 16.696     0.4055
```

```r
car::vif(m1_step)
```

```
                              GVIF Df GVIF^(1/(2*Df))
extortion_type            1.547564  3        1.075494
n_offenders_NA            1.453348  4        1.047843
mylog(bribes)             1.061394  1        1.030240
yearsquant                1.272774  4        1.030609
mylog(bribes_abvic, TRUE) 1.123640  1        1.060019
mylog(armas, TRUE)        2.133032  1        1.460490
mylog(drogas, TRUE)       2.130036  1        1.459464
```

```r
lrtest(m1_step, m1)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ extortion_type + n_offenders_NA + had_weapon_NA + 
    mylog(extortions) + mylog(bribes) + subsector + size + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + mylog(N, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  16 -253.90                     
2  32 -245.09 16 17.624     0.3464
```

```r
Anova(m1)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
extortion_type                  54.094  3  1.071e-11 ***
n_offenders_NA                  18.372  4   0.001044 ** 
had_weapon_NA                    0.887  2   0.641679    
mylog(extortions)                1.184  1   0.276622    
mylog(bribes)                    4.693  1   0.030286 *  
subsector                        6.409  6   0.378959    
size                             6.550  3   0.087715 .  
yearsquant                      11.213  4   0.024274 *  
mylog(bribes_abvic, TRUE)        1.230  1   0.267438    
mylog(armas, TRUE)               6.537  1   0.010566 *  
mylog(drogas, TRUE)              4.833  1   0.027926 *  
mylog(poblacion, TRUE)           2.390  1   0.122131    
mylog(N, TRUE)                   0.407  1   0.523589    
scale(General, scale = FALSE)    1.355  1   0.244352    
scale(Derecho, scale = FALSE)    1.643  1   0.199925    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_step)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)    
extortion_type              64.844  3  5.418e-14 ***
n_offenders_NA              20.574  4  0.0003846 ***
mylog(bribes)                5.655  1  0.0174013 *  
yearsquant                   9.354  4  0.0528406 .  
mylog(bribes_abvic, TRUE)    8.335  1  0.0038886 ** 
mylog(armas, TRUE)           2.662  1  0.1027877    
mylog(drogas, TRUE)          3.246  1  0.0715982 .  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1)
```

```
$accuracy
[1] 95.401

$naive
[1] 94.218
```

```r
accuracy(m1_step)
```

```
$accuracy
[1] 95.335

$naive
[1] 94.218
```

Use Robust Clustered Errors



```r
# m1, m1step, m1_inc,  m1_bus, m1_state, m1_inc_bus

#### Asymptotic Cluster SE
## Individual-level clustering

cluster_form <- ~ CVE_UNICA + CVE_ENT

m1CL <- summaryCL(m1, cluster_form)
m1CL
```

```
$coefs
                                 estimate         SE     z.value      p.value       ci.low     ci.high sig
(Intercept)                   -4.15070781 0.86320305 -4.80849532 1.520706e-06 -5.842554696 -2.45886093 ***
extortion_typeStreet           2.25721080 0.42383534  5.32567856 1.005769e-07  1.426508791  3.08791281 ***
extortion_typePremises         4.78564653 0.67451538  7.09494051 1.294071e-12  3.463620667  6.10767239 ***
extortion_typeCobro de piso    3.75758447 1.03134071  3.64339780 2.690625e-04  1.736193826  5.77897512 ***
n_offenders_NA2                1.11838314 0.29598658  3.77849275 1.577804e-04  0.538260105  1.69850617 ***
n_offenders_NA3                0.59274404 1.11597865  0.53114282 5.953198e-01 -1.594533911  2.78002199    
n_offenders_NA4+               2.81414726 0.79354665  3.54629090 3.906947e-04  1.258824407  4.36947011 ***
n_offenders_NADK/DA           -0.13815768 0.24127229 -0.57262144 5.669010e-01 -0.611042675  0.33472731    
had_weapon_NAYes              -0.39686070 0.43385254 -0.91473638 3.603300e-01 -1.247196062  0.45347466    
had_weapon_NADK/DA             0.25936427 0.35923011  0.72200036 4.702943e-01 -0.444713808  0.96344235    
mylog(extortions)              0.24603115 0.21376379  1.15094868 2.497533e-01 -0.172938175  0.66500047    
mylog(bribes)                  0.56462145 0.23941347  2.35835294 1.835623e-02  0.095379680  1.03386323   *
subsectorHotelsRestBar        -0.72782943 0.95384803 -0.76304548 4.454363e-01 -2.597337205  1.14167835    
subsectorManufacturing         0.02265058 0.83862677  0.02700913 9.784525e-01 -1.621027689  1.66632885    
subsectorOther industry       -0.59300842 0.99123641 -0.59825125 5.496723e-01 -2.535796079  1.34977924    
subsectorOther serv.           0.10467553 0.76817709  0.13626485 8.916119e-01 -1.400923888  1.61027495    
subsectorTransport            -0.12103243 1.13748574 -0.10640347 9.152622e-01 -2.350463521  2.10839865    
subsectorWholesale             0.41082035 1.32576998  0.30987302 7.566575e-01 -2.187641058  3.00928176    
sizeMedium                    -0.21362575 0.42896946 -0.49799756 6.184858e-01 -1.054390442  0.62713895    
sizeSmall                      0.44117270 0.35362994  1.24755473 2.121942e-01 -0.251929241  1.13427464    
sizeMicro                     -0.45746053 0.37120503 -1.23236617 2.178123e-01 -1.185009020  0.27008797    
yearsquant(8,16]               0.70740101 0.40619016  1.74155132 8.158699e-02 -0.088717073  1.50351909    
yearsquant(16,25]              0.99203475 0.36560640  2.71339546 6.659758e-03  0.275459369  1.70861012  **
yearsquant(25,34]              0.18938337 0.47108819  0.40201255 6.876748e-01 -0.733932526  1.11269926    
yearsquant(34,43]              1.14459979 0.32746742  3.49530886 4.735135e-04  0.502775430  1.78642414 ***
mylog(bribes_abvic, TRUE)      0.46051239 0.36773382  1.25229817 2.104612e-01 -0.260232653  1.18125743    
mylog(armas, TRUE)            -0.66541545 0.29597216 -2.24823661 2.456111e-02 -1.245510230 -0.08532067   *
mylog(drogas, TRUE)            0.43029150 0.23582151  1.82464909 6.805400e-02 -0.031910162  0.89249317    
mylog(poblacion, TRUE)         0.43402661 0.22487788  1.93005475 5.360005e-02 -0.006725926  0.87477916    
mylog(N, TRUE)                 0.44572508 0.65188376  0.68374933 4.941335e-01 -0.831943601  1.72339377    
scale(General, scale = FALSE) -0.01962314 0.01780726 -1.10197422 2.704729e-01 -0.054524732  0.01527845    
scale(Derecho, scale = FALSE) -0.01753502 0.01698769 -1.03221913 3.019695e-01 -0.050830274  0.01576024    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ extortion_type + n_offenders_NA + had_weapon_NA + 
    mylog(extortions) + mylog(bribes) + subsector + size + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + mylog(N, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1   1490                          
2   1521 -31 2618.2  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m1_stepCL <- summaryCL(m1_step, cluster_form)
m1_stepCL
```

```
$coefs
                               estimate        SE     z.value      p.value      ci.low     ci.high sig
(Intercept)                 -3.95898533 0.3389787 -11.6791566 1.629038e-31 -4.62337137 -3.29459928 ***
extortion_typeStreet         2.08093925 0.4048317   5.1402578 2.743618e-07  1.28748372  2.87439478 ***
extortion_typePremises       4.26734429 0.6664959   6.4026562 1.526968e-10  2.96103627  5.57365231 ***
extortion_typeCobro de piso  3.77605547 1.2736826   2.9646753 3.030025e-03  1.27968339  6.27242755  **
n_offenders_NA2              1.04516350 0.3289875   3.1769098 1.488533e-03  0.40035991  1.68996708  **
n_offenders_NA3              0.36446722 1.0877311   0.3350711 7.375715e-01 -1.76744650  2.49638093    
n_offenders_NA4+             2.40309274 0.6065452   3.9619350 7.434477e-05  1.21428596  3.59189951 ***
n_offenders_NADK/DA         -0.06267523 0.2521501  -0.2485632 8.036987e-01 -0.55688035  0.43152989    
mylog(bribes)                0.61140781 0.2333644   2.6199699 8.793754e-03  0.15402190  1.06879371  **
yearsquant(8,16]             0.58038966 0.3817012   1.5205340 1.283768e-01 -0.16773094  1.32851027    
yearsquant(16,25]            0.86918576 0.3314853   2.6220944 8.739123e-03  0.21948650  1.51888503  **
yearsquant(25,34]            0.07932399 0.4479628   0.1770772 8.594478e-01 -0.79866692  0.95731489    
yearsquant(34,43]            0.96524095 0.3215251   3.0020706 2.681500e-03  0.33506340  1.59541851  **
mylog(bribes_abvic, TRUE)    0.88003712 0.2479591   3.5491214 3.865189e-04  0.39404612  1.36602812 ***
mylog(armas, TRUE)          -0.28641077 0.1729538  -1.6559955 9.772273e-02 -0.62539406  0.05257252    
mylog(drogas, TRUE)          0.30325950 0.1959007   1.5480269 1.216158e-01 -0.08069874  0.68721773    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1   1506                          
2   1521 -15 479.53  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_step, m1, test = "Chisq", vcov = vcov(m1CL))
```

```
Wald test

Model 1: complied_bin_NA ~ extortion_type + n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ extortion_type + n_offenders_NA + had_weapon_NA + 
    mylog(extortions) + mylog(bribes) + subsector + size + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + mylog(N, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)    
1   1506                         
2   1490 16 46.478   8.21e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



# Per extortion type

Fit the same models as above (m1 and a reduced m1) fur subsets per extortion type

- Remote



```r
compliance_formula2 <- complied_bin_NA ~ n_offenders_NA +
                                        had_weapon_NA +
                                        mylog(extortions) +
                                        mylog(bribes) +
                                        subsector +
                                        size +
                                        yearsquant +
                                        mylog(bribes_abvic, TRUE) +
                                        mylog(armas, TRUE) +
                                        mylog(drogas, TRUE) +
                                        mylog(poblacion, TRUE) +
                                        mylog(N, TRUE) +
                                        scale(General, scale = FALSE) +
                                        scale(Derecho, scale = FALSE)

enve_remote <- filter(enve_nona, extortion_type == "Remote")

m1_remote <- glm(formula = compliance_formula2, 
                 data = enve_remote, 
                 family = binomial())  

summary(m1_remote)
```

```

Call:
glm(formula = compliance_formula2, family = binomial(), data = enve_remote)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.9653  -0.2958  -0.2183  -0.1585   3.0440  

Coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                    -4.027228   0.918480  -4.385 1.16e-05 ***
n_offenders_NA2                 0.654889   0.455863   1.437   0.1508    
n_offenders_NA3                 1.742817   0.721494   2.416   0.0157 *  
n_offenders_NA4+                2.477217   1.367226   1.812   0.0700 .  
n_offenders_NADK/DA            -0.200731   0.359232  -0.559   0.5763    
had_weapon_NAYes              -13.791422 660.249273  -0.021   0.9833    
had_weapon_NADK/DA              0.119246   0.387388   0.308   0.7582    
mylog(extortions)               0.181270   0.243541   0.744   0.4567    
mylog(bribes)                   0.631930   0.256789   2.461   0.0139 *  
subsectorHotelsRestBar         -0.929901   0.863163  -1.077   0.2813    
subsectorManufacturing         -0.581393   0.817703  -0.711   0.4771    
subsectorOther industry        -1.174952   0.906920  -1.296   0.1951    
subsectorOther serv.           -0.281057   0.781716  -0.360   0.7192    
subsectorTransport             -0.283133   0.968096  -0.292   0.7699    
subsectorWholesale              0.249352   0.925634   0.269   0.7876    
sizeMedium                      0.304953   0.444224   0.686   0.4924    
sizeSmall                       0.799599   0.419180   1.908   0.0565 .  
sizeMicro                       0.014644   0.452136   0.032   0.9742    
yearsquant(8,16]                0.806348   0.524052   1.539   0.1239    
yearsquant(16,25]               1.109459   0.510709   2.172   0.0298 *  
yearsquant(25,34]               0.504110   0.568685   0.886   0.3754    
yearsquant(34,43]               1.135858   0.524600   2.165   0.0304 *  
mylog(bribes_abvic, TRUE)       0.628500   0.465064   1.351   0.1766    
mylog(armas, TRUE)             -0.655859   0.275961  -2.377   0.0175 *  
mylog(drogas, TRUE)             0.456229   0.231515   1.971   0.0488 *  
mylog(poblacion, TRUE)          0.264022   0.309925   0.852   0.3943    
mylog(N, TRUE)                  0.243259   0.773984   0.314   0.7533    
scale(General, scale = FALSE)  -0.017760   0.019254  -0.922   0.3563    
scale(Derecho, scale = FALSE)  -0.008624   0.015476  -0.557   0.5774    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 453.82  on 1436  degrees of freedom
Residual deviance: 407.74  on 1408  degrees of freedom
AIC: 465.74

Number of Fisher Scoring iterations: 14
```

```r
vif(m1_remote, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                2.659193  4        1.130040
had_weapon_NA                 2.311560  2        1.233037
mylog(extortions)             1.035702  1        1.017695
mylog(bribes)                 1.095687  1        1.046750
subsector                     1.303174  6        1.022312
size                          1.162708  3        1.025444
yearsquant                    1.262117  4        1.029526
mylog(bribes_abvic, TRUE)     1.785250  1        1.336132
mylog(armas, TRUE)            4.511796  1        2.124099
mylog(drogas, TRUE)           2.866046  1        1.692940
mylog(poblacion, TRUE)        2.381907  1        1.543343
mylog(N, TRUE)                1.788203  1        1.337237
scale(General, scale = FALSE) 1.319719  1        1.148790
scale(Derecho, scale = FALSE) 1.901729  1        1.379032
```

```r
logLik(m1_remote)
```

```
'log Lik.' -203.8691 (df=29)
```

```r
m1_remote_step <- step(m1_remote, direction = "both")
```

```
Start:  AIC=465.74
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- subsector                      6   414.03 460.03
- had_weapon_NA                  2   409.00 463.00
- mylog(N, TRUE)                 1   407.84 463.84
- scale(Derecho, scale = FALSE)  1   408.05 464.05
- mylog(extortions)              1   408.24 464.24
- mylog(poblacion, TRUE)         1   408.45 464.45
- scale(General, scale = FALSE)  1   408.59 464.59
- size                           3   412.88 464.88
- yearsquant                     4   415.07 465.07
- mylog(bribes_abvic, TRUE)      1   409.59 465.59
<none>                               407.74 465.74
- n_offenders_NA                 4   417.29 467.29
- mylog(drogas, TRUE)            1   411.68 467.68
- mylog(bribes)                  1   412.89 468.89
- mylog(armas, TRUE)             1   413.34 469.34

Step:  AIC=460.03
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + size + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- had_weapon_NA                  2   415.10 457.10
- mylog(N, TRUE)                 1   414.11 458.11
- scale(Derecho, scale = FALSE)  1   414.17 458.17
- mylog(extortions)              1   414.49 458.49
- mylog(poblacion, TRUE)         1   414.59 458.59
- size                           3   418.60 458.60
- yearsquant                     4   420.73 458.73
- scale(General, scale = FALSE)  1   414.78 458.78
- mylog(bribes_abvic, TRUE)      1   416.02 460.02
<none>                               414.03 460.03
- n_offenders_NA                 4   423.64 461.64
- mylog(drogas, TRUE)            1   418.07 462.07
- mylog(bribes)                  1   419.07 463.07
- mylog(armas, TRUE)             1   419.46 463.46
+ subsector                      6   407.74 465.74

Step:  AIC=457.1
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- mylog(N, TRUE)                 1   415.15 455.15
- scale(Derecho, scale = FALSE)  1   415.26 455.26
- mylog(extortions)              1   415.59 455.59
- size                           3   419.60 455.60
- mylog(poblacion, TRUE)         1   415.61 455.61
- scale(General, scale = FALSE)  1   415.90 455.90
- yearsquant                     4   422.04 456.04
- mylog(bribes_abvic, TRUE)      1   417.05 457.05
<none>                               415.10 457.10
- n_offenders_NA                 4   424.74 458.74
- mylog(drogas, TRUE)            1   419.04 459.04
+ had_weapon_NA                  2   414.03 460.03
- mylog(armas, TRUE)             1   420.37 460.37
- mylog(bribes)                  1   420.38 460.38
+ subsector                      6   409.00 463.00

Step:  AIC=455.15
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- scale(Derecho, scale = FALSE)  1   415.27 453.27
- size                           3   419.61 453.61
- mylog(extortions)              1   415.64 453.64
- mylog(poblacion, TRUE)         1   415.65 453.65
- scale(General, scale = FALSE)  1   415.95 453.95
- yearsquant                     4   422.08 454.08
<none>                               415.15 455.15
- mylog(bribes_abvic, TRUE)      1   417.82 455.82
- n_offenders_NA                 4   424.77 456.77
+ mylog(N, TRUE)                 1   415.10 457.10
- mylog(drogas, TRUE)            1   419.45 457.45
+ had_weapon_NA                  2   414.11 458.11
- mylog(bribes)                  1   420.38 458.38
- mylog(armas, TRUE)             1   420.69 458.69
+ subsector                      6   409.04 461.04

Step:  AIC=453.27
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(General, 
    scale = FALSE)

                                Df Deviance    AIC
- size                           3   419.69 451.69
- mylog(poblacion, TRUE)         1   415.73 451.73
- mylog(extortions)              1   415.75 451.75
- scale(General, scale = FALSE)  1   416.03 452.03
- yearsquant                     4   422.21 452.21
<none>                               415.27 453.27
- mylog(bribes_abvic, TRUE)      1   418.42 454.42
- n_offenders_NA                 4   424.95 454.95
+ scale(Derecho, scale = FALSE)  1   415.15 455.15
+ mylog(N, TRUE)                 1   415.26 455.26
- mylog(drogas, TRUE)            1   419.50 455.50
+ had_weapon_NA                  2   414.18 456.18
- mylog(bribes)                  1   420.45 456.45
- mylog(armas, TRUE)             1   421.19 457.19
+ subsector                      6   409.33 459.33

Step:  AIC=451.69
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(General, 
    scale = FALSE)

                                Df Deviance    AIC
- mylog(poblacion, TRUE)         1   420.02 450.02
- mylog(extortions)              1   420.19 450.19
- scale(General, scale = FALSE)  1   420.53 450.53
- yearsquant                     4   426.68 450.68
<none>                               419.69 451.69
+ size                           3   415.27 453.27
- mylog(bribes_abvic, TRUE)      1   423.30 453.30
- n_offenders_NA                 4   429.50 453.50
+ scale(Derecho, scale = FALSE)  1   419.61 453.61
+ mylog(N, TRUE)                 1   419.69 453.69
- mylog(drogas, TRUE)            1   423.87 453.87
+ had_weapon_NA                  2   418.68 454.68
- mylog(armas, TRUE)             1   425.22 455.22
- mylog(bribes)                  1   425.37 455.37
+ subsector                      6   414.25 458.25

Step:  AIC=450.02
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE) + scale(General, scale = FALSE)

                                Df Deviance    AIC
- mylog(extortions)              1   420.53 448.53
- scale(General, scale = FALSE)  1   420.92 448.92
- yearsquant                     4   426.94 448.94
<none>                               420.02 450.02
+ mylog(poblacion, TRUE)         1   419.69 451.69
+ size                           3   415.73 451.73
- n_offenders_NA                 4   429.76 451.76
+ scale(Derecho, scale = FALSE)  1   419.96 451.96
+ mylog(N, TRUE)                 1   420.02 452.02
- mylog(bribes_abvic, TRUE)      1   424.29 452.29
- mylog(drogas, TRUE)            1   424.43 452.43
+ had_weapon_NA                  2   419.07 453.07
- mylog(bribes)                  1   425.61 453.61
- mylog(armas, TRUE)             1   426.00 454.00
+ subsector                      6   414.65 456.65

Step:  AIC=448.53
complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + scale(General, scale = FALSE)

                                Df Deviance    AIC
- yearsquant                     4   427.22 447.22
- scale(General, scale = FALSE)  1   421.37 447.37
<none>                               420.53 448.53
+ mylog(extortions)              1   420.02 450.02
+ mylog(poblacion, TRUE)         1   420.19 450.19
+ size                           3   416.23 450.23
- n_offenders_NA                 4   430.31 450.31
+ scale(Derecho, scale = FALSE)  1   420.47 450.47
+ mylog(N, TRUE)                 1   420.53 450.53
- mylog(drogas, TRUE)            1   424.87 450.87
- mylog(bribes_abvic, TRUE)      1   424.88 450.88
+ had_weapon_NA                  2   419.57 451.57
- mylog(bribes)                  1   426.00 452.00
- mylog(armas, TRUE)             1   426.49 452.49
+ subsector                      6   415.20 455.20

Step:  AIC=447.22
complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + scale(General, 
    scale = FALSE)

                                Df Deviance    AIC
- scale(General, scale = FALSE)  1   427.63 445.63
<none>                               427.22 447.22
- n_offenders_NA                 4   436.44 448.44
+ yearsquant                     4   420.53 448.53
+ size                           3   422.91 448.91
+ mylog(extortions)              1   426.94 448.94
+ mylog(poblacion, TRUE)         1   426.96 448.96
+ scale(Derecho, scale = FALSE)  1   427.13 449.13
+ mylog(N, TRUE)                 1   427.22 449.22
- mylog(drogas, TRUE)            1   431.65 449.65
+ had_weapon_NA                  2   426.07 450.07
- mylog(bribes_abvic, TRUE)      1   432.61 450.61
- mylog(armas, TRUE)             1   433.22 451.22
- mylog(bribes)                  1   433.64 451.64
+ subsector                      6   422.16 454.16

Step:  AIC=445.63
complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)

                                Df Deviance    AIC
<none>                               427.63 445.63
- n_offenders_NA                 4   436.68 446.68
+ scale(General, scale = FALSE)  1   427.22 447.22
+ size                           3   423.30 447.30
+ mylog(poblacion, TRUE)         1   427.33 447.33
+ mylog(extortions)              1   427.37 447.37
+ yearsquant                     4   421.37 447.37
+ scale(Derecho, scale = FALSE)  1   427.56 447.56
+ mylog(N, TRUE)                 1   427.62 447.62
- mylog(drogas, TRUE)            1   431.80 447.80
+ had_weapon_NA                  2   426.46 448.46
- mylog(armas, TRUE)             1   433.47 449.47
- mylog(bribes)                  1   434.49 450.49
- mylog(bribes_abvic, TRUE)      1   435.57 451.57
+ subsector                      6   422.65 452.65
```

```r
summary(m1_remote_step)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE), family = binomial(), data = enve_remote)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.8124  -0.2999  -0.2335  -0.2052   3.1144  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)                -3.3926     0.2737 -12.397  < 2e-16 ***
n_offenders_NA2             0.7136     0.4452   1.603  0.10895    
n_offenders_NA3             1.6006     0.6838   2.341  0.01924 *  
n_offenders_NA4+            1.6394     1.1950   1.372  0.17011    
n_offenders_NADK/DA        -0.1408     0.3251  -0.433  0.66497    
mylog(bribes)               0.7164     0.2445   2.929  0.00340 ** 
mylog(bribes_abvic, TRUE)   0.9824     0.3551   2.767  0.00566 ** 
mylog(armas, TRUE)         -0.4672     0.1901  -2.458  0.01397 *  
mylog(drogas, TRUE)         0.4051     0.1955   2.072  0.03830 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 453.82  on 1436  degrees of freedom
Residual deviance: 427.63  on 1428  degrees of freedom
AIC: 445.63

Number of Fisher Scoring iterations: 6
```

```r
logLik(m1_remote_step)
```

```
'log Lik.' -213.813 (df=9)
```

```r
waldtest(m1_remote_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df Df  Chisq Pr(>Chisq)    
1   1428                         
2   1436 -8 27.958  0.0004823 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_remote_step, m1_remote, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)
1   1428                     
2   1408 20 17.526     0.6186
```

```r
car::vif(m1_remote_step)
```

```
                              GVIF Df GVIF^(1/(2*Df))
n_offenders_NA            1.081384  4        1.009828
mylog(bribes)             1.052114  1        1.025726
mylog(bribes_abvic, TRUE) 1.067749  1        1.033320
mylog(armas, TRUE)        2.282903  1        1.510928
mylog(drogas, TRUE)       2.296460  1        1.515408
```

```r
lrtest(m1_remote_step, m1_remote)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   9 -213.81                     
2  29 -203.87 20 19.888      0.465
```

```r
Anova(m1_remote)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)  
n_offenders_NA                  9.5567  4    0.04860 *
had_weapon_NA                   1.2585  2    0.53300  
mylog(extortions)               0.5009  1    0.47912  
mylog(bribes)                   5.1529  1    0.02321 *
subsector                       6.2919  6    0.39130  
size                            5.1387  3    0.16192  
yearsquant                      7.3357  4    0.11918  
mylog(bribes_abvic, TRUE)       1.8494  1    0.17385  
mylog(armas, TRUE)              5.6030  1    0.01793 *
mylog(drogas, TRUE)             3.9409  1    0.04712 *
mylog(poblacion, TRUE)          0.7153  1    0.39770  
mylog(N, TRUE)                  0.0984  1    0.75375  
scale(General, scale = FALSE)   0.8551  1    0.35512  
scale(Derecho, scale = FALSE)   0.3079  1    0.57895  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_remote_step)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)   
n_offenders_NA              9.0529  4   0.059791 . 
mylog(bribes)               6.8666  1   0.008782 **
mylog(bribes_abvic, TRUE)   7.9392  1   0.004838 **
mylog(armas, TRUE)          5.8442  1   0.015628 * 
mylog(drogas, TRUE)         4.1694  1   0.041161 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_remote)
```

```
$accuracy
[1] 96.451

$naive
[1] 96.312
```

```r
accuracy(m1_remote_step)
```

```
$accuracy
[1] 96.312

$naive
[1] 96.312
```

```r
# Robust SE

m1_remoteCL <- summaryCL(m1_remote, cluster_form)
m1_remoteCL
```

```
$coefs
                                   estimate         SE     z.value      p.value       ci.low      ci.high sig
(Intercept)                    -4.027227919 0.84717503  -4.7537141 1.997134e-06  -5.68766047  -2.36679537 ***
n_offenders_NA2                 0.654888704 0.43286368   1.5129214 1.302996e-01  -0.19350851   1.50328592    
n_offenders_NA3                 1.742817118 0.97078221   1.7952710 7.261054e-02  -0.15988105   3.64551529    
n_offenders_NA4+                2.477216632 1.06404274   2.3281176 1.990586e-02   0.39173118   4.56270209   *
n_offenders_NADK/DA            -0.200730874 0.25642481  -0.7828060 4.337411e-01  -0.70331426   0.30185251    
had_weapon_NAYes              -13.791421506 0.95274048 -14.4755280 1.729922e-47 -15.65875853 -11.92408448 ***
had_weapon_NADK/DA              0.119246126 0.38799021   0.3073431 7.585822e-01  -0.64120072   0.87969297    
mylog(extortions)               0.181270326 0.25806928   0.7024095 4.824238e-01  -0.32453618   0.68707683    
mylog(bribes)                   0.631929935 0.23884187   2.6458088 8.149586e-03   0.16380847   1.10005140  **
subsectorHotelsRestBar         -0.929900514 0.85704567  -1.0850070 2.779186e-01  -2.60967917   0.74987814    
subsectorManufacturing         -0.581393105 0.78885086  -0.7370127 4.611146e-01  -2.12751238   0.96472617    
subsectorOther industry        -1.174952227 1.03024000  -1.1404646 2.540928e-01  -3.19418552   0.84428106    
subsectorOther serv.           -0.281056636 0.72694122  -0.3866291 6.990308e-01  -1.70583525   1.14372198    
subsectorTransport             -0.283133466 1.00977239  -0.2803934 7.791757e-01  -2.26225098   1.69598405    
subsectorWholesale              0.249351885 1.17181210   0.2127917 8.314894e-01  -2.04735763   2.54606140    
sizeMedium                      0.304952594 0.51488717   0.5922707 5.536693e-01  -0.70420771   1.31411290    
sizeSmall                       0.799599072 0.42594618   1.8772303 6.048654e-02  -0.03524011   1.63443825    
sizeMicro                       0.014644067 0.42562298   0.0344062 9.725532e-01  -0.81956164   0.84884978    
yearsquant(8,16]                0.806347782 0.47333307   1.7035526 8.846470e-02  -0.12136798   1.73406355    
yearsquant(16,25]               1.109458674 0.48767650   2.2749890 2.290659e-02   0.15363031   2.06528704   *
yearsquant(25,34]               0.504109743 0.53053533   0.9501907 3.420154e-01  -0.53572040   1.54393988    
yearsquant(34,43]               1.135857658 0.47172541   2.4078789 1.604550e-02   0.21129285   2.06042247   *
mylog(bribes_abvic, TRUE)       0.628500253 0.44656042   1.4074249 1.593014e-01  -0.24674209   1.50374259    
mylog(armas, TRUE)             -0.655859124 0.30943083  -2.1195662 3.404265e-02  -1.26233241  -0.04938583   *
mylog(drogas, TRUE)             0.456228723 0.30629961   1.4894852 1.363597e-01  -0.14410747   1.05656492    
mylog(poblacion, TRUE)          0.264021749 0.22640659   1.1661399 2.435579e-01  -0.17972702   0.70777051    
mylog(N, TRUE)                  0.243259231 0.78072242   0.3115822 7.553580e-01  -1.28692860   1.77344706    
scale(General, scale = FALSE)  -0.017759772 0.01867414  -0.9510357 3.415862e-01  -0.05436041   0.01884087    
scale(Derecho, scale = FALSE)  -0.008624065 0.01997772  -0.4316841 6.659710e-01  -0.04777968   0.03053155    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df Chisq Pr(>Chisq)    
1   1408                         
2   1436 -28 10118  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m1_remote_stepCL <- summaryCL(m1_remote_step, cluster_form)
m1_remote_stepCL
```

```
$coefs
                            estimate        SE     z.value      p.value      ci.low    ci.high sig
(Intercept)               -3.3926056 0.2929001 -11.5828066 5.036952e-31 -3.96667934 -2.8185319 ***
n_offenders_NA2            0.7136165 0.4006028   1.7813566 7.485422e-02 -0.07155064  1.4987836    
n_offenders_NA3            1.6006116 0.9179598   1.7436620 8.121804e-02 -0.19855652  3.3997798    
n_offenders_NA4+           1.6393983 1.3334047   1.2294829 2.188908e-01 -0.97402692  4.2528235    
n_offenders_NADK/DA       -0.1407906 0.2677084  -0.5259105 5.989504e-01 -0.66548938  0.3839081    
mylog(bribes)              0.7163638 0.2428683   2.9495971 3.181885e-03  0.24035056  1.1923769  **
mylog(bribes_abvic, TRUE)  0.9824085 0.3086217   3.1832122 1.456508e-03  0.37752101  1.5872960  **
mylog(armas, TRUE)        -0.4671909 0.1850922  -2.5240990 1.159953e-02 -0.82996488 -0.1044170   *
mylog(drogas, TRUE)        0.4050545 0.2298111   1.7625542 7.797570e-02 -0.04536696  0.8554760    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df Df  Chisq Pr(>Chisq)    
1   1428                         
2   1436 -8 34.629  3.123e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_remote_step, m1_remote, test = "Chisq", vcov = vcov(m1_remoteCL))
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)    
1   1428                         
2   1408 20 2484.2  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# using m1_step terms

m1_remote_step2 <- update(m1_step, 
                             . ~ . - extortion_type,
                             data = enve_remote)


summary(m1_remote_step2)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE), family = binomial(), data = enve_remote)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.8971  -0.2924  -0.2318  -0.1788   3.0717  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
(Intercept)                -3.9955     0.4740  -8.428  < 2e-16 ***
n_offenders_NA2             0.6922     0.4465   1.550  0.12111    
n_offenders_NA3             1.6214     0.6879   2.357  0.01842 *  
n_offenders_NA4+            1.7132     1.2247   1.399  0.16186    
n_offenders_NADK/DA        -0.1855     0.3275  -0.566  0.57117    
mylog(bribes)               0.6760     0.2465   2.742  0.00610 ** 
yearsquant(8,16]            0.6663     0.5125   1.300  0.19359    
yearsquant(16,25]           0.9817     0.4998   1.964  0.04953 *  
yearsquant(25,34]           0.3290     0.5565   0.591  0.55433    
yearsquant(34,43]           0.9762     0.5105   1.912  0.05586 .  
mylog(bribes_abvic, TRUE)   0.9217     0.3507   2.628  0.00859 ** 
mylog(armas, TRUE)         -0.4577     0.1889  -2.423  0.01538 *  
mylog(drogas, TRUE)         0.3972     0.1978   2.008  0.04464 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 453.82  on 1436  degrees of freedom
Residual deviance: 421.37  on 1424  degrees of freedom
AIC: 447.37

Number of Fisher Scoring iterations: 6
```

```r
logLik(m1_remote_step2)
```

```
'log Lik.' -210.6875 (df=13)
```

```r
waldtest(m1_remote_step2, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1   1424                          
2   1436 -12 32.915   0.000998 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_remote_step2, m1_remote, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)
1   1424                     
2   1408 16 12.424     0.7143
```

```r
car::vif(m1_remote_step2)
```

```
                              GVIF Df GVIF^(1/(2*Df))
n_offenders_NA            1.125211  4        1.014856
mylog(bribes)             1.068374  1        1.033622
yearsquant                1.081826  4        1.009880
mylog(bribes_abvic, TRUE) 1.065854  1        1.032402
mylog(armas, TRUE)        2.270601  1        1.506852
mylog(drogas, TRUE)       2.293660  1        1.514483
```

```r
lrtest(m1_remote_step2, m1_remote)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  13 -210.69                     
2  29 -203.87 16 13.637     0.6258
```

```r
Anova(m1_remote)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)  
n_offenders_NA                  9.5567  4    0.04860 *
had_weapon_NA                   1.2585  2    0.53300  
mylog(extortions)               0.5009  1    0.47912  
mylog(bribes)                   5.1529  1    0.02321 *
subsector                       6.2919  6    0.39130  
size                            5.1387  3    0.16192  
yearsquant                      7.3357  4    0.11918  
mylog(bribes_abvic, TRUE)       1.8494  1    0.17385  
mylog(armas, TRUE)              5.6030  1    0.01793 *
mylog(drogas, TRUE)             3.9409  1    0.04712 *
mylog(poblacion, TRUE)          0.7153  1    0.39770  
mylog(N, TRUE)                  0.0984  1    0.75375  
scale(General, scale = FALSE)   0.8551  1    0.35512  
scale(Derecho, scale = FALSE)   0.3079  1    0.57895  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_remote_step2)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)   
n_offenders_NA              9.4719  4   0.050328 . 
mylog(bribes)               6.1354  1   0.013250 * 
yearsquant                  6.2510  4   0.181174   
mylog(bribes_abvic, TRUE)   7.1518  1   0.007489 **
mylog(armas, TRUE)          5.6952  1   0.017012 * 
mylog(drogas, TRUE)         3.9132  1   0.047909 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_remote)
```

```
$accuracy
[1] 96.451

$naive
[1] 96.312
```

```r
accuracy(m1_remote_step2)
```

```
$accuracy
[1] 96.312

$naive
[1] 96.312
```

```r
# Robust SE



m1_remote_step2CL <- summaryCL(m1_remote_step2, cluster_form)
m1_remote_step2CL
```

```
$coefs
                            estimate        SE    z.value      p.value      ci.low    ci.high sig
(Intercept)               -3.9954507 0.4715876 -8.4723403 2.405113e-17 -4.91974544 -3.0711560 ***
n_offenders_NA2            0.6921949 0.4073986  1.6990607 8.930774e-02 -0.10629162  1.4906814    
n_offenders_NA3            1.6214011 0.9185350  1.7652033 7.752961e-02 -0.17889452  3.4216967    
n_offenders_NA4+           1.7131795 1.1872538  1.4429766 1.490271e-01 -0.61379529  4.0401542    
n_offenders_NADK/DA       -0.1854744 0.2739939 -0.6769289 4.984511e-01 -0.72249262  0.3515438    
mylog(bribes)              0.6760038 0.2413083  2.8014112 5.087964e-03  0.20304822  1.1489595  **
yearsquant(8,16]           0.6662725 0.4527462  1.4716246 1.411223e-01 -0.22109383  1.5536388    
yearsquant(16,25]          0.9816575 0.4583015  2.1419471 3.219774e-02  0.08340311  1.8799120   *
yearsquant(25,34]          0.3290497 0.5267729  0.6246520 5.321995e-01 -0.70340611  1.3615056    
yearsquant(34,43]          0.9761737 0.4575238  2.1336019 3.287538e-02  0.07944349  1.8729039   *
mylog(bribes_abvic, TRUE)  0.9217143 0.3052333  3.0197046 2.530214e-03  0.32346807  1.5199604  **
mylog(armas, TRUE)        -0.4576634 0.1840252 -2.4869609 1.288396e-02 -0.81834617 -0.0969807   *
mylog(drogas, TRUE)        0.3972076 0.2361650  1.6819073 9.258682e-02 -0.06566726  0.8600826    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1   1424                          
2   1436 -12 77.564    1.2e-11 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_remote_step2, m1_remote, test = "Chisq", vcov = vcov(m1_remoteCL))
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)    
1   1424                         
2   1408 16 774.24  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


- premises



```r
enve_premises <- filter(enve_nona, extortion_type == "Premises")

m1_premises <- glm(formula = compliance_formula2, 
                 data = enve_premises, 
                 family = binomial())  

summary(m1_premises)
```

```

Call:
glm(formula = compliance_formula2, family = binomial(), data = enve_premises)

Deviance Residuals: 
         1           2           3           4           5           6           7           8           9  
-3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06  
        10          11          12          13          14          15          16  
 3.971e-06   3.971e-06  -3.971e-06  -3.971e-06   3.971e-06   3.971e-06   3.971e-06  

Coefficients: (13 not defined because of singularities)
                                Estimate Std. Error z value Pr(>|z|)
(Intercept)                    7.670e+01  1.036e+06       0        1
n_offenders_NA2               -5.113e+01  3.055e+05       0        1
n_offenders_NA3               -5.113e+01  3.055e+05       0        1
n_offenders_NA4+              -5.113e+01  4.320e+05       0        1
n_offenders_NADK/DA           -5.113e+01  4.320e+05       0        1
had_weapon_NAYes                      NA         NA      NA       NA
had_weapon_NADK/DA                    NA         NA      NA       NA
mylog(extortions)              9.062e-08  4.816e+05       0        1
mylog(bribes)                  1.490e-07  5.214e+05       0        1
subsectorManufacturing        -2.101e-06  6.831e+05       0        1
subsectorOther industry               NA         NA      NA       NA
subsectorOther serv.           5.113e+01  4.320e+05       0        1
sizeMedium                            NA         NA      NA       NA
sizeSmall                             NA         NA      NA       NA
sizeMicro                             NA         NA      NA       NA
yearsquant(16,25]             -5.113e+01  6.831e+05       0        1
yearsquant(25,34]             -1.023e+02  7.482e+05       0        1
yearsquant(34,43]             -5.113e+01  5.291e+05       0        1
mylog(bribes_abvic, TRUE)             NA         NA      NA       NA
mylog(armas, TRUE)                    NA         NA      NA       NA
mylog(drogas, TRUE)                   NA         NA      NA       NA
mylog(poblacion, TRUE)                NA         NA      NA       NA
mylog(N, TRUE)                        NA         NA      NA       NA
scale(General, scale = FALSE)         NA         NA      NA       NA
scale(Derecho, scale = FALSE)         NA         NA      NA       NA

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1.5442e+01  on 15  degrees of freedom
Residual deviance: 2.5232e-10  on  4  degrees of freedom
AIC: 24

Number of Fisher Scoring iterations: 24
```

```r
vif(m1_premises, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                2.659193  4        1.130040
had_weapon_NA                 2.311560  2        1.233037
mylog(extortions)             1.035702  1        1.017695
mylog(bribes)                 1.095687  1        1.046750
subsector                     1.303174  6        1.022312
size                          1.162708  3        1.025444
yearsquant                    1.262117  4        1.029526
mylog(bribes_abvic, TRUE)     1.785250  1        1.336132
mylog(armas, TRUE)            4.511796  1        2.124099
mylog(drogas, TRUE)           2.866046  1        1.692940
mylog(poblacion, TRUE)        2.381907  1        1.543343
mylog(N, TRUE)                1.788203  1        1.337237
scale(General, scale = FALSE) 1.319719  1        1.148790
scale(Derecho, scale = FALSE) 1.901729  1        1.379032
```

```r
logLik(m1_premises)
```

```
'log Lik.' -1.261586e-10 (df=12)
```

```r
m1_premises_step <- step(m1_premises, direction = "both")
```

```
Start:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE)


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE)


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE)


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE)


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant


Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + yearsquant
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    subsector + yearsquant
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=24
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + subsector + 
    yearsquant
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                    Df Deviance    AIC
- subsector          3   0.0000 18.000
- n_offenders_NA     4   3.8191 19.819
- yearsquant         3   3.8191 21.819
- mylog(extortions)  1   0.0000 22.000
<none>                   0.0000 24.000
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=18
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + yearsquant
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- mylog(extortions)              1   0.0000 16.000
- n_offenders_NA                 4   6.5917 16.592
<none>                               0.0000 18.000
- yearsquant                     3   6.5917 18.592
+ mylog(poblacion, TRUE)         1   0.0000 20.000
+ mylog(armas, TRUE)             1   0.0000 20.000
+ scale(Derecho, scale = FALSE)  1   0.0000 20.000
+ mylog(drogas, TRUE)            1   0.0000 20.000
+ mylog(N, TRUE)                 1   0.0000 20.000
+ mylog(bribes_abvic, TRUE)      1   0.0000 20.000
+ scale(General, scale = FALSE)  1   0.0000 20.000
+ mylog(bribes)                  1   0.0000 20.000
+ subsector                      3   0.0000 24.000
+ size                           3   0.0000 24.000
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=16
complied_bin_NA ~ n_offenders_NA + yearsquant
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- n_offenders_NA                 4   7.6382 15.638
<none>                               0.0000 16.000
- yearsquant                     3   6.5917 16.592
+ mylog(N, TRUE)                 1   0.0000 18.000
+ scale(General, scale = FALSE)  1   0.0000 18.000
+ mylog(bribes_abvic, TRUE)      1   0.0000 18.000
+ mylog(bribes)                  1   0.0000 18.000
+ mylog(poblacion, TRUE)         1   0.0000 18.000
+ mylog(drogas, TRUE)            1   0.0000 18.000
+ scale(Derecho, scale = FALSE)  1   0.0000 18.000
+ mylog(armas, TRUE)             1   0.0000 18.000
+ mylog(extortions)              1   0.0000 18.000
+ subsector                      3   0.0000 22.000
+ size                           3   0.0000 22.000

Step:  AIC=15.64
complied_bin_NA ~ yearsquant
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
+ mylog(armas, TRUE)             1   2.7726 12.773
+ scale(General, scale = FALSE)  1   2.7726 12.773
+ mylog(poblacion, TRUE)         1   2.7726 12.773
+ mylog(drogas, TRUE)            1   2.7726 12.773
+ had_weapon_NA                  2   2.7726 14.773
<none>                               7.6382 15.638
+ n_offenders_NA                 4   0.0000 16.000
+ mylog(bribes_abvic, TRUE)      1   6.0860 16.086
+ scale(Derecho, scale = FALSE)  1   6.5247 16.525
+ mylog(extortions)              1   6.5917 16.592
- yearsquant                     3  15.4425 17.442
+ mylog(N, TRUE)                 1   7.6199 17.620
+ mylog(bribes)                  1   7.6382 17.638
+ size                           3   3.8191 17.819
+ subsector                      3   3.8191 17.819
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=12.77
complied_bin_NA ~ yearsquant + mylog(armas, TRUE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
<none>                               2.7726 12.773
+ mylog(poblacion, TRUE)         1   2.7726 14.773
+ mylog(bribes_abvic, TRUE)      1   2.7726 14.773
+ scale(General, scale = FALSE)  1   2.7726 14.773
+ mylog(extortions)              1   2.7726 14.773
+ mylog(N, TRUE)                 1   2.7726 14.773
+ mylog(drogas, TRUE)            1   2.7726 14.773
+ scale(Derecho, scale = FALSE)  1   2.7726 14.773
+ mylog(bribes)                  1   2.7726 14.773
- mylog(armas, TRUE)             1   7.6382 15.638
+ had_weapon_NA                  2   2.7726 16.773
+ n_offenders_NA                 4   0.0000 18.000
+ size                           3   2.7726 18.773
+ subsector                      3   2.7726 18.773
- yearsquant                     3  15.4225 19.422
```

```r
summary(m1_premises_step)
```

```

Call:
glm(formula = complied_bin_NA ~ yearsquant + mylog(armas, TRUE), 
    family = binomial(), data = enve_premises)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.17741   0.00000   0.00001   0.00001   1.17741  

Coefficients:
                   Estimate Std. Error z value Pr(>|z|)
(Intercept)           31.44   31668.51   0.001    0.999
yearsquant(16,25]    -43.82   35731.48  -0.001    0.999
yearsquant(25,34]   -101.18   55283.76  -0.002    0.999
yearsquant(34,43]     25.05   50914.40   0.000    1.000
mylog(armas, TRUE)  -103.05   60298.86  -0.002    0.999

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 15.4425  on 15  degrees of freedom
Residual deviance:  2.7726  on 11  degrees of freedom
AIC: 12.773

Number of Fisher Scoring iterations: 22
```

```r
logLik(m1_premises_step)
```

```
'log Lik.' -1.386294 (df=5)
```

```r
waldtest(m1_premises_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ yearsquant + mylog(armas, TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df Df Chisq Pr(>Chisq)
1     11                    
2     15 -4     0          1
```

```r
waldtest(m1_premises_step, m1_premises, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ yearsquant + mylog(armas, TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1     11                    
2      4  7                 
```

```r
car::vif(m1_premises_step)
```

```
                       GVIF Df GVIF^(1/(2*Df))
yearsquant         13.50319  3        1.543143
mylog(armas, TRUE) 13.50319  1        3.674668
```

```r
lrtest(m1_premises_step, m1_premises)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ yearsquant + mylog(armas, TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   5 -1.3863                     
2  12  0.0000  7 2.7726     0.9052
```

```r
Anova(m1_premises)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)
n_offenders_NA                  2.7726  2       0.25
had_weapon_NA                           0           
mylog(extortions)                       0           
mylog(bribes)                           0           
subsector                               0           
size                                    0           
yearsquant                              0           
mylog(bribes_abvic, TRUE)               0           
mylog(armas, TRUE)                      0           
mylog(drogas, TRUE)                     0           
mylog(poblacion, TRUE)                  0           
mylog(N, TRUE)                          0           
scale(General, scale = FALSE)           0           
scale(Derecho, scale = FALSE)           0           
```

```r
Anova(m1_premises_step)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                   LR Chisq Df Pr(>Chisq)   
yearsquant          12.6499  3   0.005458 **
mylog(armas, TRUE)   4.8656  1   0.027398 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_premises)
```

```
$accuracy
[1] 100

$naive
[1] 81.25
```

```r
accuracy(m1_premises_step)
```

```
$accuracy
[1] 93.75

$naive
[1] 81.25
```

```r
# Robust SE

m1_premisesCL <- summaryCL(m1_premises, cluster_form)
```

```
Error in waldtest.lm(m, vcov = robust_vcov, test = "Chisq"): there are aliased coefficients in the model
```

```r
m1_premisesCL
```

```
Error in eval(expr, envir, enclos): object 'm1_premisesCL' not found
```

```r
m1_premises_stepCL <- summaryCL(m1_premises_step, cluster_form)
m1_premises_stepCL
```

```
$coefs
                     estimate       SE   z.value       p.value     ci.low   ci.high sig
(Intercept)          31.43524 1.298515  24.20861 1.805441e-129   28.89020  33.98029 ***
yearsquant(16,25]   -43.82013 2.138071 -20.49517  2.377735e-93  -48.01067 -39.62959 ***
yearsquant(25,34]  -101.17950 3.708544 -27.28281 6.785189e-164 -108.44811 -93.91089 ***
yearsquant(34,43]    25.05010 1.325109  18.90417  1.053767e-79   22.45293  27.64726 ***
mylog(armas, TRUE) -103.05008 3.804501 -27.08636 1.425614e-161 -110.50676 -95.59339 ***

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ yearsquant + mylog(armas, TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df Df      Chisq Pr(>Chisq)    
1     11                             
2     15 -4 1089086050  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_premises_step, m1_premises, test = "Chisq", vcov = vcov(m1_premisesCL))
```

```
Error in vcov(m1_premisesCL): object 'm1_premisesCL' not found
```

```r
# using m1_step terms

m1_premises_step2 <- update(m1_step, 
                             . ~ . - extortion_type,
                             data = enve_premises)


summary(m1_premises_step2)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE), family = binomial(), data = enve_premises)

Deviance Residuals: 
         1           2           3           4           5           6           7           8           9  
-3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06   3.971e-06  
        10          11          12          13          14          15          16  
 3.971e-06   3.971e-06  -3.971e-06  -3.971e-06   3.971e-06   3.971e-06   3.971e-06  

Coefficients:
                           Estimate Std. Error z value Pr(>|z|)
(Intercept)                  -33.73  577613.49       0        1
n_offenders_NA2               35.44  329978.60       0        1
n_offenders_NA3              -51.13  305470.19       0        1
n_offenders_NA4+              35.44  430673.66       0        1
n_offenders_NADK/DA           35.44  494174.06       0        1
mylog(bribes)                 15.17  325406.81       0        1
yearsquant(16,25]            -30.11  645768.03       0        1
yearsquant(25,34]             10.24  488267.77       0        1
yearsquant(34,43]             51.83  452023.99       0        1
mylog(bribes_abvic, TRUE)     27.89  556230.55       0        1
mylog(armas, TRUE)           -29.58  365075.91       0        1
mylog(drogas, TRUE)          -14.66  141821.82       0        1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1.5442e+01  on 15  degrees of freedom
Residual deviance: 2.5232e-10  on  4  degrees of freedom
AIC: 24

Number of Fisher Scoring iterations: 24
```

```r
logLik(m1_premises_step2)
```

```
'log Lik.' -1.261586e-10 (df=12)
```

```r
waldtest(m1_premises_step2, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df Chisq Pr(>Chisq)
1      4                     
2     15 -11     0          1
```

```r
waldtest(m1_premises_step2, m1_premises, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1      4                    
2      4  0                 
```

```r
car::vif(m1_premises_step2)
```

```
                                GVIF Df GVIF^(1/(2*Df))
n_offenders_NA            100.239222  4        1.778811
mylog(bribes)              14.993349  1        3.872125
yearsquant                297.472562  3        2.583694
mylog(bribes_abvic, TRUE)   8.855696  1        2.975852
mylog(armas, TRUE)         13.259983  1        3.641426
mylog(drogas, TRUE)         9.211358  1        3.035022
```

```r
lrtest(m1_premises_step2, m1_premises)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df      LogLik Df Chisq Pr(>Chisq)
1  12 -1.2616e-10                    
2  12 -1.2616e-10  0     0          1
```

```r
Anova(m1_premises)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)
n_offenders_NA                  2.7726  2       0.25
had_weapon_NA                           0           
mylog(extortions)                       0           
mylog(bribes)                           0           
subsector                               0           
size                                    0           
yearsquant                              0           
mylog(bribes_abvic, TRUE)               0           
mylog(armas, TRUE)                      0           
mylog(drogas, TRUE)                     0           
mylog(poblacion, TRUE)                  0           
mylog(N, TRUE)                          0           
scale(General, scale = FALSE)           0           
scale(Derecho, scale = FALSE)           0           
```

```r
Anova(m1_premises_step2)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)
n_offenders_NA              2.7726  4     0.5966
mylog(bribes)               0.0000  1     1.0000
yearsquant                  0.0000  3     1.0000
mylog(bribes_abvic, TRUE)   0.0000  1     1.0000
mylog(armas, TRUE)          0.0000  1     1.0000
mylog(drogas, TRUE)         0.0000  1     1.0000
```

```r
accuracy(m1_premises)
```

```
$accuracy
[1] 100

$naive
[1] 81.25
```

```r
accuracy(m1_premises_step2)
```

```
$accuracy
[1] 100

$naive
[1] 81.25
```

```r
# Robust SE



m1_premises_step2CL <- summaryCL(m1_premises_step2, cluster_form)
```

```
Error in solve.default(vc[ovar, ovar]): system is computationally singular: reciprocal condition number = 6.69422e-18
```

```r
m1_premises_step2CL
```

```
Error in eval(expr, envir, enclos): object 'm1_premises_step2CL' not found
```

```r
waldtest(m1_premises_step2, m1_premises, test = "Chisq", vcov = vcov(m1_premisesCL))
```

```
Error in vcov(m1_premisesCL): object 'm1_premisesCL' not found
```

- street




```r
enve_street <- filter(enve_nona, extortion_type == "Street")

m1_street <- glm(formula = compliance_formula2, 
                 data = enve_street, 
                 family = binomial())  
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(m1_street)
```

```

Call:
glm(formula = compliance_formula2, family = binomial(), data = enve_street)

Deviance Residuals: 
       Min          1Q      Median          3Q         Max  
-2.712e-05  -2.409e-06  -2.110e-08   2.110e-08   2.816e-05  

Coefficients:
                                Estimate Std. Error z value Pr(>|z|)
(Intercept)                    1.651e+02  9.162e+05   0.000        1
n_offenders_NA2                1.988e+02  3.732e+05   0.001        1
n_offenders_NA3               -3.933e+01  1.763e+05   0.000        1
n_offenders_NA4+               1.926e+02  7.639e+05   0.000        1
n_offenders_NADK/DA            2.654e+02  4.745e+07   0.000        1
had_weapon_NAYes              -4.075e+01  1.705e+05   0.000        1
had_weapon_NADK/DA            -7.112e+00  4.746e+07   0.000        1
mylog(extortions)             -1.949e+01  4.304e+05   0.000        1
mylog(bribes)                 -4.242e+01  4.131e+05   0.000        1
subsectorHotelsRestBar        -2.354e+02  1.094e+06   0.000        1
subsectorManufacturing        -2.969e+02  5.053e+05  -0.001        1
subsectorOther industry       -1.777e+02  1.040e+06   0.000        1
subsectorOther serv.          -3.293e+02  6.977e+05   0.000        1
subsectorTransport            -3.631e+02  4.746e+07   0.000        1
subsectorWholesale            -4.707e+02  7.703e+05  -0.001        1
sizeMedium                    -2.566e+01  7.332e+05   0.000        1
sizeSmall                     -4.040e+01  9.271e+05   0.000        1
sizeMicro                     -3.137e+01  1.527e+06   0.000        1
yearsquant(8,16]               2.537e+01  4.755e+05   0.000        1
yearsquant(16,25]             -6.409e+01  5.168e+05   0.000        1
yearsquant(25,34]             -9.484e+01  1.872e+05  -0.001        1
yearsquant(34,43]              1.320e+01  3.729e+05   0.000        1
mylog(bribes_abvic, TRUE)      6.754e+01  7.968e+05   0.000        1
mylog(armas, TRUE)             1.052e+02  2.627e+05   0.000        1
mylog(drogas, TRUE)           -7.990e+01  1.312e+05  -0.001        1
mylog(poblacion, TRUE)        -3.497e+01  3.039e+05   0.000        1
mylog(N, TRUE)                -3.210e+02  2.115e+06   0.000        1
scale(General, scale = FALSE)  6.397e+00  4.165e+04   0.000        1
scale(Derecho, scale = FALSE) -3.328e+00  2.742e+04   0.000        1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 7.4703e+01  on 61  degrees of freedom
Residual deviance: 5.9415e-09  on 33  degrees of freedom
AIC: 58

Number of Fisher Scoring iterations: 25
```

```r
vif(m1_street, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                2.659193  4        1.130040
had_weapon_NA                 2.311560  2        1.233037
mylog(extortions)             1.035702  1        1.017695
mylog(bribes)                 1.095687  1        1.046750
subsector                     1.303174  6        1.022312
size                          1.162708  3        1.025444
yearsquant                    1.262117  4        1.029526
mylog(bribes_abvic, TRUE)     1.785250  1        1.336132
mylog(armas, TRUE)            4.511796  1        2.124099
mylog(drogas, TRUE)           2.866046  1        1.692940
mylog(poblacion, TRUE)        2.381907  1        1.543343
mylog(N, TRUE)                1.788203  1        1.337237
scale(General, scale = FALSE) 1.319719  1        1.148790
scale(Derecho, scale = FALSE) 1.901729  1        1.379032
```

```r
logLik(m1_street)
```

```
'log Lik.' -2.970769e-09 (df=29)
```

```r
m1_street_step <- step(m1_street, direction = "both")
```

```
Start:  AIC=58
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- yearsquant                     4     0.00  50.00
- subsector                      6     4.50  50.50
- size                           3     0.00  52.00
- had_weapon_NA                  2     0.00  54.00
- mylog(extortions)              1     0.00  56.00
- mylog(bribes)                  1     0.00  56.00
- mylog(poblacion, TRUE)         1     0.00  56.00
- scale(Derecho, scale = FALSE)  1     0.00  56.00
- mylog(drogas, TRUE)            1     0.00  56.00
<none>                                 0.00  58.00
- n_offenders_NA                 4    38.08  88.08
- scale(General, scale = FALSE)  1   288.35 344.35
- mylog(bribes_abvic, TRUE)      1   432.52 488.52
- mylog(N, TRUE)                 1   432.52 488.52
- mylog(armas, TRUE)             1   576.70 632.70
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=50
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- had_weapon_NA                  2     0.00  46.00
- mylog(extortions)              1     0.00  48.00
- mylog(poblacion, TRUE)         1     0.00  48.00
- mylog(armas, TRUE)             1     0.00  48.00
- mylog(bribes)                  1     0.00  48.00
- mylog(N, TRUE)                 1     0.00  48.00
- scale(Derecho, scale = FALSE)  1     0.00  48.00
- mylog(drogas, TRUE)            1     0.00  48.00
- mylog(bribes_abvic, TRUE)      1     0.00  48.00
- scale(General, scale = FALSE)  1     0.00  48.00
<none>                                 0.00  50.00
+ yearsquant                     4     0.00  58.00
- n_offenders_NA                 4    47.23  89.23
- size                           3   432.52 476.52
- subsector                      6   720.87 758.87
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=46
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    subsector + size + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- subsector                      6    4.499 38.499
- size                           3    0.000 40.000
- mylog(extortions)              1    0.000 44.000
- mylog(armas, TRUE)             1    0.000 44.000
- mylog(poblacion, TRUE)         1    0.000 44.000
- mylog(bribes)                  1    0.000 44.000
- mylog(N, TRUE)                 1    0.000 44.000
- mylog(bribes_abvic, TRUE)      1    0.000 44.000
- mylog(drogas, TRUE)            1    0.000 44.000
- scale(General, scale = FALSE)  1    0.000 44.000
<none>                                0.000 46.000
+ had_weapon_NA                  2    0.000 50.000
+ yearsquant                     4    0.000 54.000
- scale(Derecho, scale = FALSE)  1   21.735 65.735
- n_offenders_NA                 4   48.867 86.867
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=38.5
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    size + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + mylog(N, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- mylog(extortions)              1     4.50  36.50
- mylog(poblacion, TRUE)         1     4.50  36.50
- mylog(bribes)                  1     4.50  36.50
- mylog(N, TRUE)                 1     4.50  36.50
<none>                                 4.50  38.50
+ subsector                      6     0.00  46.00
+ yearsquant                     4     4.50  46.50
- size                           3    25.52  53.52
- mylog(armas, TRUE)             1    21.70  53.70
- mylog(bribes_abvic, TRUE)      1    22.33  54.33
- mylog(drogas, TRUE)            1    24.62  56.62
- scale(General, scale = FALSE)  1    26.22  58.22
- scale(Derecho, scale = FALSE)  1    29.88  61.88
- n_offenders_NA                 4    53.33  79.33
+ had_weapon_NA                  2   720.87 758.87
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=36.5
complied_bin_NA ~ n_offenders_NA + mylog(bribes) + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- mylog(poblacion, TRUE)         1    4.499 34.499
- mylog(bribes)                  1    4.499 34.499
- mylog(N, TRUE)                 1    4.499 34.499
<none>                                4.499 36.499
+ mylog(extortions)              1    4.499 38.499
+ had_weapon_NA                  2    4.499 40.499
+ subsector                      6    0.000 44.000
+ yearsquant                     4    4.499 44.499
- size                           3   25.724 51.724
- mylog(armas, TRUE)             1   22.561 52.561
- mylog(bribes_abvic, TRUE)      1   22.680 52.680
- mylog(drogas, TRUE)            1   25.075 55.075
- scale(General, scale = FALSE)  1   26.572 56.572
- scale(Derecho, scale = FALSE)  1   30.868 60.868
- n_offenders_NA                 4   53.339 77.339
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=34.5
complied_bin_NA ~ n_offenders_NA + mylog(bribes) + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
- mylog(bribes)                  1    4.499 32.499
<none>                                4.499 34.499
+ mylog(poblacion, TRUE)         1    4.499 36.499
+ mylog(extortions)              1    4.499 36.499
+ had_weapon_NA                  2    4.499 38.499
+ subsector                      6    0.000 42.000
+ yearsquant                     4    4.499 42.499
- mylog(N, TRUE)                 1   17.783 45.783
- size                           3   28.186 52.186
- mylog(bribes_abvic, TRUE)      1   25.072 53.072
- mylog(drogas, TRUE)            1   27.493 55.493
- scale(General, scale = FALSE)  1   28.574 56.574
- mylog(armas, TRUE)             1   30.229 58.229
- scale(Derecho, scale = FALSE)  1   30.874 58.874
- n_offenders_NA                 4   54.319 76.319
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=32.5
complied_bin_NA ~ n_offenders_NA + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance    AIC
<none>                                4.499 32.499
+ mylog(bribes)                  1    4.499 34.499
+ mylog(poblacion, TRUE)         1    4.499 34.499
+ mylog(extortions)              1    4.499 34.499
+ had_weapon_NA                  2    4.499 36.499
+ subsector                      6    0.000 40.000
+ yearsquant                     4    4.499 40.499
- mylog(N, TRUE)                 1   19.413 45.413
- size                           3   28.665 50.665
- mylog(bribes_abvic, TRUE)      1   25.148 51.148
- mylog(drogas, TRUE)            1   27.944 53.944
- scale(General, scale = FALSE)  1   28.984 54.984
- mylog(armas, TRUE)             1   30.229 56.229
- scale(Derecho, scale = FALSE)  1   31.469 57.469
- n_offenders_NA                 4   55.050 75.050
```

```r
summary(m1_street_step)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE), 
    family = binomial(), data = enve_street)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7585   0.0000   0.0000   0.0000   1.6651  

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)
(Intercept)                    -2616.29  245433.70  -0.011    0.991
n_offenders_NA2                 2724.19  257189.28   0.011    0.992
n_offenders_NA3                  431.03   84816.09   0.005    0.996
n_offenders_NA4+                2973.77  278254.05   0.011    0.991
n_offenders_NADK/DA             2938.72  275690.49   0.011    0.991
sizeMedium                      -108.09   11920.67  -0.009    0.993
sizeSmall                       -287.15   26248.90  -0.011    0.991
sizeMicro                       2179.57  204333.72   0.011    0.991
mylog(bribes_abvic, TRUE)       1573.15  143927.54   0.011    0.991
mylog(armas, TRUE)              1031.57   96898.57   0.011    0.992
mylog(drogas, TRUE)             -923.35   85288.12  -0.011    0.991
mylog(N, TRUE)                  -621.90   96761.85  -0.006    0.995
scale(General, scale = FALSE)    120.13   11074.46   0.011    0.991
scale(Derecho, scale = FALSE)    -30.95    2857.84  -0.011    0.991

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 74.7026  on 61  degrees of freedom
Residual deviance:  4.4987  on 48  degrees of freedom
AIC: 32.499

Number of Fisher Scoring iterations: 25
```

```r
logLik(m1_street_step)
```

```
'log Lik.' -2.249341 (df=14)
```

```r
waldtest(m1_street_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df Chisq Pr(>Chisq)
1     48                     
2     61 -13 1e-04          1
```

```r
waldtest(m1_street_step, m1_street, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1     48                    
2     33 15     0          1
```

```r
car::vif(m1_street_step)
```

```
                                     GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                347347.8199  4        4.927148
size                           14180.1983  3        4.919795
mylog(bribes_abvic, TRUE)       4124.4448  1       64.221841
mylog(armas, TRUE)              1702.1316  1       41.256898
mylog(drogas, TRUE)             2519.4551  1       50.194174
mylog(N, TRUE)                   445.1172  1       21.097800
scale(General, scale = FALSE)   5794.4066  1       76.121000
scale(Derecho, scale = FALSE)   3594.0151  1       59.950105
```

```r
lrtest(m1_street_step, m1_street)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)
1  14 -2.2493                     
2  29  0.0000 15 4.4987     0.9956
```

```r
Anova(m1_street)
```

```
Warning: glm.fit: algorithm did not converge

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                   38.08  4  1.081e-07 ***
had_weapon_NA                     0.00  2     1.0000    
mylog(extortions)                 0.00  1     1.0000    
mylog(bribes)                     0.00  1     1.0000    
subsector                         4.50  6     0.6095    
size                              0.00  3     1.0000    
yearsquant                        0.00  4     1.0000    
mylog(bribes_abvic, TRUE)       432.52  1  < 2.2e-16 ***
mylog(armas, TRUE)              576.70  1  < 2.2e-16 ***
mylog(drogas, TRUE)               0.00  1     0.9999    
mylog(poblacion, TRUE)            0.00  1     1.0000    
mylog(N, TRUE)                  432.52  1  < 2.2e-16 ***
scale(General, scale = FALSE)   288.35  1  < 2.2e-16 ***
scale(Derecho, scale = FALSE)     0.00  1     1.0000    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_street_step)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                  50.551  4  2.770e-10 ***
size                            24.166  3  2.306e-05 ***
mylog(bribes_abvic, TRUE)       20.649  1  5.515e-06 ***
mylog(armas, TRUE)              25.731  1  3.925e-07 ***
mylog(drogas, TRUE)             23.445  1  1.285e-06 ***
mylog(N, TRUE)                  14.914  1  0.0001125 ***
scale(General, scale = FALSE)   24.485  1  7.489e-07 ***
scale(Derecho, scale = FALSE)   26.971  1  2.066e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_street)
```

```
$accuracy
[1] 100

$naive
[1] 70.968
```

```r
accuracy(m1_street_step)
```

```
$accuracy
[1] 98.387

$naive
[1] 70.968
```

```r
# Robust SE

m1_streetCL <- summaryCL(m1_street, cluster_form)
```

```
Error in solve.default(vc[ovar, ovar]): system is computationally singular: reciprocal condition number = 3.77514e-18
```

```r
m1_streetCL
```

```
Error in eval(expr, envir, enclos): object 'm1_streetCL' not found
```

```r
m1_street_stepCL <- summaryCL(m1_street_step, cluster_form)
```

```
Warning in sqrt(diag(se)): NaNs produced
```

```
Warning in sqrt(diag(se)): NaNs produced
```

```r
m1_street_stepCL
```

```
$coefs
                                 estimate  SE z.value p.value ci.low ci.high sig
(Intercept)                   -2616.28742 NaN     NaN     NaN    NaN     NaN  NA
n_offenders_NA2                2724.18616 NaN     NaN     NaN    NaN     NaN  NA
n_offenders_NA3                 431.02559 NaN     NaN     NaN    NaN     NaN  NA
n_offenders_NA4+               2973.77087 NaN     NaN     NaN    NaN     NaN  NA
n_offenders_NADK/DA            2938.72155 NaN     NaN     NaN    NaN     NaN  NA
sizeMedium                     -108.08766 NaN     NaN     NaN    NaN     NaN  NA
sizeSmall                      -287.15068 NaN     NaN     NaN    NaN     NaN  NA
sizeMicro                      2179.57405 NaN     NaN     NaN    NaN     NaN  NA
mylog(bribes_abvic, TRUE)      1573.15059 NaN     NaN     NaN    NaN     NaN  NA
mylog(armas, TRUE)             1031.56957 NaN     NaN     NaN    NaN     NaN  NA
mylog(drogas, TRUE)            -923.35481 NaN     NaN     NaN    NaN     NaN  NA
mylog(N, TRUE)                 -621.89721 NaN     NaN     NaN    NaN     NaN  NA
scale(General, scale = FALSE)   120.12745 NaN     NaN     NaN    NaN     NaN  NA
scale(Derecho, scale = FALSE)   -30.95207 NaN     NaN     NaN    NaN     NaN  NA

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + size + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1     48                          
2     61 -13 5429.4  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_street_step, m1_street, test = "Chisq", vcov = vcov(m1_streetCL))
```

```
Error in vcov(m1_streetCL): object 'm1_streetCL' not found
```

```r
# using m1_step terms

m1_street_step2 <- update(m1_step, 
                             . ~ . - extortion_type,
                             data = enve_street)


summary(m1_street_step2)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE), family = binomial(), data = enve_street)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.81458  -0.34857  -0.03179   0.29229   2.30693  

Coefficients:
                           Estimate Std. Error z value Pr(>|z|)   
(Intercept)                 -3.6017     1.4005  -2.572  0.01012 * 
n_offenders_NA2              4.9415     1.8392   2.687  0.00722 **
n_offenders_NA3            -15.0784  3057.5977  -0.005  0.99607   
n_offenders_NA4+             6.0656     2.3299   2.603  0.00923 **
n_offenders_NADK/DA          3.8662     1.7785   2.174  0.02971 * 
mylog(bribes)               -1.2754     1.3218  -0.965  0.33460   
yearsquant(8,16]            -0.8910     1.7646  -0.505  0.61362   
yearsquant(16,25]           -0.1431     1.4272  -0.100  0.92013   
yearsquant(25,34]           -2.7096     1.6998  -1.594  0.11092   
yearsquant(34,43]            0.1038     1.5593   0.067  0.94692   
mylog(bribes_abvic, TRUE)    1.6914     1.4408   1.174  0.24042   
mylog(armas, TRUE)           2.6345     1.3034   2.021  0.04324 * 
mylog(drogas, TRUE)         -0.7264     0.6806  -1.067  0.28584   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 74.703  on 61  degrees of freedom
Residual deviance: 33.794  on 49  degrees of freedom
AIC: 59.794

Number of Fisher Scoring iterations: 18
```

```r
logLik(m1_street_step2)
```

```
'log Lik.' -16.89689 (df=13)
```

```r
waldtest(m1_street_step2, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)
1     49                      
2     61 -12 11.667     0.4728
```

```r
waldtest(m1_street_step2, m1_street, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1     49                    
2     33 16     0          1
```

```r
car::vif(m1_street_step2)
```

```
                              GVIF Df GVIF^(1/(2*Df))
n_offenders_NA            4.883406  4        1.219243
mylog(bribes)             1.583423  1        1.258341
yearsquant                4.510886  4        1.207210
mylog(bribes_abvic, TRUE) 2.293650  1        1.514480
mylog(armas, TRUE)        3.411983  1        1.847155
mylog(drogas, TRUE)       3.657410  1        1.912436
```

```r
lrtest(m1_street_step2, m1_street)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)   
1  13 -16.897                        
2  29   0.000 16 33.794   0.005791 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_street)
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: algorithm did not converge
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                   38.08  4  1.081e-07 ***
had_weapon_NA                     0.00  2     1.0000    
mylog(extortions)                 0.00  1     1.0000    
mylog(bribes)                     0.00  1     1.0000    
subsector                         4.50  6     0.6095    
size                              0.00  3     1.0000    
yearsquant                        0.00  4     1.0000    
mylog(bribes_abvic, TRUE)       432.52  1  < 2.2e-16 ***
mylog(armas, TRUE)              576.70  1  < 2.2e-16 ***
mylog(drogas, TRUE)               0.00  1     0.9999    
mylog(poblacion, TRUE)            0.00  1     1.0000    
mylog(N, TRUE)                  432.52  1  < 2.2e-16 ***
scale(General, scale = FALSE)   288.35  1  < 2.2e-16 ***
scale(Derecho, scale = FALSE)     0.00  1     1.0000    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_street_step2)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)    
n_offenders_NA             23.5377  4  9.885e-05 ***
mylog(bribes)               0.8512  1    0.35621    
yearsquant                  3.8288  4    0.42968    
mylog(bribes_abvic, TRUE)   1.4773  1    0.22419    
mylog(armas, TRUE)          6.2131  1    0.01268 *  
mylog(drogas, TRUE)         1.2462  1    0.26428    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_street)
```

```
$accuracy
[1] 100

$naive
[1] 70.968
```

```r
accuracy(m1_street_step2)
```

```
$accuracy
[1] 88.71

$naive
[1] 70.968
```

```r
# Robust SE



m1_street_step2CL <- summaryCL(m1_street_step2, cluster_form)
m1_street_step2CL
```

```
$coefs
                             estimate        SE     z.value      p.value      ci.low      ci.high sig
(Intercept)                -3.6017035 0.9861527 -3.65227751 2.599248e-04  -5.5345274  -1.66887963 ***
n_offenders_NA2             4.9414509 1.7196214  2.87356912 4.058623e-03   1.5710549   8.31184694  **
n_offenders_NA3           -15.0783780 1.5739510 -9.57995398 9.708819e-22 -18.1632652 -11.99349072 ***
n_offenders_NA4+            6.0655650 1.8941658  3.20223552 1.363655e-03   2.3530682   9.77806166  **
n_offenders_NADK/DA         3.8662414 1.0161316  3.80486283 1.418828e-04   1.8746600   5.85782273 ***
mylog(bribes)              -1.2753544 1.1798282 -1.08096619 2.797122e-01  -3.5877753   1.03706641    
yearsquant(8,16]           -0.8909902 1.4642611 -0.60849135 5.428616e-01  -3.7608893   1.97890881    
yearsquant(16,25]          -0.1431129 1.3160175 -0.10874694 9.134032e-01  -2.7224598   2.43623405    
yearsquant(25,34]          -2.7096181 1.3531727 -2.00241849 4.523974e-02  -5.3617879  -0.05744828   *
yearsquant(34,43]           0.1038234 1.2322324  0.08425637 9.328526e-01  -2.3113077   2.51895453    
mylog(bribes_abvic, TRUE)   1.6914374 0.9397645  1.79985245 7.188394e-02  -0.1504671   3.53334183    
mylog(armas, TRUE)          2.6345312 1.0724579  2.45653578 1.402838e-02   0.5325523   4.73651001   *
mylog(drogas, TRUE)        -0.7264125 0.5260458 -1.38089213 1.673121e-01  -1.7574434   0.30461832    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1     49                          
2     61 -12 2127.4  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_street_step2, m1_street, test = "Chisq", vcov = vcov(m1_streetCL))
```

```
Error in vcov(m1_streetCL): object 'm1_streetCL' not found
```

- cobro de piso




```r
enve_piso <- filter(enve_nona, extortion_type == "Cobro de piso")

m1_piso <- glm(formula = compliance_formula2, 
                 data = enve_piso, 
                 family = binomial())  

summary(m1_piso)
```

```

Call:
glm(formula = compliance_formula2, family = binomial(), data = enve_piso)

Deviance Residuals: 
         1           2           3           4           5           6           7  
-6.547e-06  -6.547e-06   6.547e-06   6.547e-06   6.547e-06   6.547e-06  -6.547e-06  

Coefficients: (13 not defined because of singularities)
                                Estimate Std. Error z value Pr(>|z|)
(Intercept)                   -2.457e+01  1.310e+05       0        1
n_offenders_NA2                2.794e-09  1.605e+05       0        1
n_offenders_NA3                4.913e+01  2.451e+05       0        1
had_weapon_NAYes              -2.794e-09  2.451e+05       0        1
mylog(extortions)              3.053e+01  9.970e+04       0        1
mylog(bribes)                 -1.523e-14  1.151e+05       0        1
subsectorManufacturing                NA         NA      NA       NA
subsectorOther serv.                  NA         NA      NA       NA
subsectorTransport                    NA         NA      NA       NA
sizeMedium                            NA         NA      NA       NA
sizeSmall                             NA         NA      NA       NA
yearsquant(34,43]                     NA         NA      NA       NA
mylog(bribes_abvic, TRUE)             NA         NA      NA       NA
mylog(armas, TRUE)                    NA         NA      NA       NA
mylog(drogas, TRUE)                   NA         NA      NA       NA
mylog(poblacion, TRUE)                NA         NA      NA       NA
mylog(N, TRUE)                        NA         NA      NA       NA
scale(General, scale = FALSE)         NA         NA      NA       NA
scale(Derecho, scale = FALSE)         NA         NA      NA       NA

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 9.5607e+00  on 6  degrees of freedom
Residual deviance: 3.0007e-10  on 1  degrees of freedom
AIC: 12

Number of Fisher Scoring iterations: 23
```

```r
vif(m1_piso, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                2.659193  4        1.130040
had_weapon_NA                 2.311560  2        1.233037
mylog(extortions)             1.035702  1        1.017695
mylog(bribes)                 1.095687  1        1.046750
subsector                     1.303174  6        1.022312
size                          1.162708  3        1.025444
yearsquant                    1.262117  4        1.029526
mylog(bribes_abvic, TRUE)     1.785250  1        1.336132
mylog(armas, TRUE)            4.511796  1        2.124099
mylog(drogas, TRUE)           2.866046  1        1.692940
mylog(poblacion, TRUE)        2.381907  1        1.543343
mylog(N, TRUE)                1.788203  1        1.337237
scale(General, scale = FALSE) 1.319719  1        1.148790
scale(Derecho, scale = FALSE) 1.901729  1        1.379032
```

```r
logLik(m1_piso)
```

```
'log Lik.' -1.500338e-10 (df=6)
```

```r
m1_piso_step <- step(m1_piso, direction = "both")
```

```
Start:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE)


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector


Step:  AIC=12
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes)

                    Df Deviance    AIC
- had_weapon_NA      1   0.0000 10.000
- mylog(bribes)      1   0.0000 10.000
- n_offenders_NA     2   2.7726 10.773
<none>                   0.0000 12.000
- mylog(extortions)  1   3.8191 13.819

Step:  AIC=10
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes)

                                Df Deviance     AIC
- mylog(bribes)                  1   0.0000  8.0000
- n_offenders_NA                 2   3.8191  9.8191
<none>                               0.0000 10.0000
+ had_weapon_NA                  1   0.0000 12.0000
+ size                           1   0.0000 12.0000
+ mylog(bribes_abvic, TRUE)      1   0.0000 12.0000
+ mylog(armas, TRUE)             1   0.0000 12.0000
+ mylog(drogas, TRUE)            1   0.0000 12.0000
+ mylog(poblacion, TRUE)         1   0.0000 12.0000
+ mylog(N, TRUE)                 1   0.0000 12.0000
+ scale(General, scale = FALSE)  1   0.0000 12.0000
+ scale(Derecho, scale = FALSE)  1   0.0000 12.0000
- mylog(extortions)              1   6.5917 14.5917

Step:  AIC=8
complied_bin_NA ~ n_offenders_NA + mylog(extortions)

                                Df Deviance     AIC
<none>                               0.0000  8.0000
- n_offenders_NA                 2   4.4987  8.4987
+ had_weapon_NA                  1   0.0000 10.0000
+ mylog(bribes)                  1   0.0000 10.0000
+ subsector                      1   0.0000 10.0000
+ size                           1   0.0000 10.0000
+ mylog(bribes_abvic, TRUE)      1   0.0000 10.0000
+ mylog(armas, TRUE)             1   0.0000 10.0000
+ mylog(drogas, TRUE)            1   0.0000 10.0000
+ mylog(poblacion, TRUE)         1   0.0000 10.0000
+ mylog(N, TRUE)                 1   0.0000 10.0000
+ scale(General, scale = FALSE)  1   0.0000 10.0000
+ scale(Derecho, scale = FALSE)  1   0.0000 10.0000
- mylog(extortions)              1   8.3178 14.3178
```

```r
summary(m1_piso_step)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(extortions), 
    family = binomial(), data = enve_piso)

Deviance Residuals: 
         1           2           3           4           5           6           7  
-6.547e-06  -6.547e-06   6.547e-06   6.547e-06   6.547e-06   6.547e-06  -6.547e-06  

Coefficients:
                    Estimate Std. Error z value Pr(>|z|)
(Intercept)       -2.457e+01  8.457e+04       0        1
n_offenders_NA2    2.322e-12  1.135e+05       0        1
n_offenders_NA3    4.913e+01  1.559e+05       0        1
mylog(extortions)  3.053e+01  6.646e+04       0        1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 9.5607e+00  on 6  degrees of freedom
Residual deviance: 3.0007e-10  on 3  degrees of freedom
AIC: 8

Number of Fisher Scoring iterations: 23
```

```r
logLik(m1_piso_step)
```

```
'log Lik.' -1.500338e-10 (df=4)
```

```r
waldtest(m1_piso_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions)
Model 2: complied_bin_NA ~ 1
  Res.Df Df Chisq Pr(>Chisq)
1      3                    
2      6 -3     0          1
```

```r
waldtest(m1_piso_step, m1_piso, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1      3                    
2      1  2     0          1
```

```r
car::vif(m1_piso_step)
```

```
                      GVIF Df GVIF^(1/(2*Df))
n_offenders_NA    1.142857  2        1.033946
mylog(extortions) 1.142857  1        1.069045
```

```r
lrtest(m1_piso_step, m1_piso)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df      LogLik Df Chisq Pr(>Chisq)
1   4 -1.5003e-10                    
2   6 -1.5003e-10  2     0          1
```

```r
Anova(m1_piso)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)
n_offenders_NA                       0  1          1
had_weapon_NA                           0           
mylog(extortions)                       0           
mylog(bribes)                           0           
subsector                               0           
size                                    0           
yearsquant                              0           
mylog(bribes_abvic, TRUE)               0           
mylog(armas, TRUE)                      0           
mylog(drogas, TRUE)                     0           
mylog(poblacion, TRUE)                  0           
mylog(N, TRUE)                          0           
scale(General, scale = FALSE)           0           
scale(Derecho, scale = FALSE)           0           
```

```r
Anova(m1_piso_step)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                  LR Chisq Df Pr(>Chisq)   
n_offenders_NA      4.4987  2   0.105469   
mylog(extortions)   8.3178  1   0.003926 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_piso)
```

```
$accuracy
[1] 100

$naive
[1] 57.143
```

```r
accuracy(m1_piso_step)
```

```
$accuracy
[1] 100

$naive
[1] 57.143
```

```r
# Robust SE

m1_pisoCL <- summaryCL(m1_piso, cluster_form)
```

```
Warning in sqrt(diag(se)): NaNs produced

Warning in sqrt(diag(se)): NaNs produced
```

```
Error in waldtest.lm(m, vcov = robust_vcov, test = "Chisq"): there are aliased coefficients in the model
```

```r
m1_pisoCL
```

```
Error in eval(expr, envir, enclos): object 'm1_pisoCL' not found
```

```r
m1_piso_stepCL <- summaryCL(m1_piso_step, cluster_form)
m1_piso_stepCL
```

```
$coefs
                       estimate        SE       z.value       p.value     ci.low    ci.high sig
(Intercept)       -2.456607e+01 0.6846532 -3.588104e+01 6.036361e-282 -25.907964 -23.224173 ***
n_offenders_NA2    2.321855e-12 0.6846536  3.391285e-12  1.000000e+00  -1.341896   1.341896    
n_offenders_NA3    4.913214e+01 1.3110106  3.747654e+01 2.221126e-307  46.562605  51.701672 ***
mylog(extortions)  3.052751e+01 0.8021397  3.805760e+01  0.000000e+00  28.955349  32.099679 ***

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions)
Model 2: complied_bin_NA ~ 1
  Res.Df Df  Chisq Pr(>Chisq)    
1      3                         
2      6 -3 2317.4  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_piso_step, m1_piso, test = "Chisq", vcov = vcov(m1_pisoCL))
```

```
Error in vcov(m1_pisoCL): object 'm1_pisoCL' not found
```

```r
# using m1_step terms

m1_piso_step2 <- update(m1_step, 
                             . ~ . - extortion_type,
                             data = enve_piso)


summary(m1_piso_step2)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE), family = binomial(), data = enve_piso)

Deviance Residuals: 
         1           2           3           4           5           6           7  
-6.547e-06  -6.547e-06   6.547e-06   6.547e-06   6.547e-06   6.547e-06  -6.547e-06  

Coefficients: (2 not defined because of singularities)
                            Estimate Std. Error z value Pr(>|z|)
(Intercept)                7.494e+00  7.566e+04       0        1
n_offenders_NA2            5.284e-10  1.605e+05       0        1
n_offenders_NA3            3.499e+01  1.582e+05       0        1
mylog(bribes)             -3.157e+01  1.010e+05       0        1
yearsquant(34,43]                 NA         NA      NA       NA
mylog(bribes_abvic, TRUE) -6.507e+01  2.123e+05       0        1
mylog(armas, TRUE)         1.498e+01  7.941e+04       0        1
mylog(drogas, TRUE)               NA         NA      NA       NA

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 9.5607e+00  on 6  degrees of freedom
Residual deviance: 3.0007e-10  on 1  degrees of freedom
AIC: 12

Number of Fisher Scoring iterations: 23
```

```r
logLik(m1_piso_step2)
```

```
'log Lik.' -1.500338e-10 (df=6)
```

```r
waldtest(m1_piso_step2, test = "Chisq")
```

```
Error in waldtest.lm(m1_piso_step2, test = "Chisq"): there are aliased coefficients in the model
```

```r
waldtest(m1_piso_step2, m1_piso, test = "Chisq")
```

```
Error in waldtest.lm(m1_piso_step2, m1_piso, test = "Chisq"): there are aliased coefficients in the model
```

```r
car::vif(m1_piso_step2)
```

```
Error in vif.default(m1_piso_step2): there are aliased coefficients in the model
```

```r
lrtest(m1_piso_step2, m1_piso)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df      LogLik Df Chisq Pr(>Chisq)
1   6 -1.5003e-10                    
2   6 -1.5003e-10  0     0          1
```

```r
Anova(m1_piso)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)
n_offenders_NA                       0  1          1
had_weapon_NA                           0           
mylog(extortions)                       0           
mylog(bribes)                           0           
subsector                               0           
size                                    0           
yearsquant                              0           
mylog(bribes_abvic, TRUE)               0           
mylog(armas, TRUE)                      0           
mylog(drogas, TRUE)                     0           
mylog(poblacion, TRUE)                  0           
mylog(N, TRUE)                          0           
scale(General, scale = FALSE)           0           
scale(Derecho, scale = FALSE)           0           
```

```r
Anova(m1_piso_step2)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)
n_offenders_NA                   0  1          1
mylog(bribes)                       0           
yearsquant                          0           
mylog(bribes_abvic, TRUE)           0           
mylog(armas, TRUE)                  0           
mylog(drogas, TRUE)                 0           
```

```r
accuracy(m1_piso)
```

```
$accuracy
[1] 100

$naive
[1] 57.143
```

```r
accuracy(m1_piso_step2)
```

```
$accuracy
[1] 100

$naive
[1] 57.143
```

```r
# Robust SE



m1_piso_step2CL <- summaryCL(m1_piso_step2, cluster_form)
```

```
Error in waldtest.lm(m, vcov = robust_vcov, test = "Chisq"): there are aliased coefficients in the model
```

```r
m1_piso_step2CL
```

```
Error in eval(expr, envir, enclos): object 'm1_piso_step2CL' not found
```

```r
waldtest(m1_piso_step2, m1_piso, test = "Chisq", vcov = vcov(m1_pisoCL))
```

```
Error in waldtest.lm(m1_piso_step2, m1_piso, test = "Chisq", vcov = vcov(m1_pisoCL)): there are aliased coefficients in the model
```


- Inperson (agg)


```r
enve_inperson <- filter(enve_nona, extortion_type_bin == "In person")

m1_inperson <- glm(formula = compliance_formula2, 
                 data = enve_inperson, 
                 family = binomial())  

summary(m1_inperson)
```

```

Call:
glm(formula = compliance_formula2, family = binomial(), data = enve_inperson)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.95742  -0.44997  -0.00547   0.38242   2.97703  

Coefficients:
                                Estimate Std. Error z value Pr(>|z|)  
(Intercept)                     -6.37061 3956.18278  -0.002   0.9987  
n_offenders_NA2                  3.79359    1.70196   2.229   0.0258 *
n_offenders_NA3                  0.66524    1.88669   0.353   0.7244  
n_offenders_NA4+                 6.05489    2.47340   2.448   0.0144 *
n_offenders_NADK/DA              1.34021   10.15002   0.132   0.8950  
had_weapon_NAYes                -0.04543    1.32659  -0.034   0.9727  
had_weapon_NADK/DA               4.06283   10.18346   0.399   0.6899  
mylog(extortions)                2.61353    1.24155   2.105   0.0353 *
mylog(bribes)                   -0.00304    1.13230  -0.003   0.9979  
subsectorHotelsRestBar           1.18959 3956.18496   0.000   0.9998  
subsectorManufacturing           7.30779 3956.18234   0.002   0.9985  
subsectorOther industry          6.75749 3956.18259   0.002   0.9986  
subsectorOther serv.             5.86287 3956.18287   0.001   0.9988  
subsectorTransport               2.97702 3956.18390   0.001   0.9994  
subsectorWholesale             -11.43167 4568.20582  -0.003   0.9980  
sizeMedium                      -3.31628    1.59943  -2.073   0.0381 *
sizeSmall                       -4.58812    2.09038  -2.195   0.0282 *
sizeMicro                        0.57378    1.53148   0.375   0.7079  
yearsquant(8,16]                -0.40203    1.77549  -0.226   0.8209  
yearsquant(16,25]               -2.02988    1.94205  -1.045   0.2959  
yearsquant(25,34]               -2.77916    1.53447  -1.811   0.0701 .
yearsquant(34,43]                0.75155    1.19643   0.628   0.5299  
mylog(bribes_abvic, TRUE)        2.61867    1.92078   1.363   0.1728  
mylog(armas, TRUE)              -0.77678    1.44932  -0.536   0.5920  
mylog(drogas, TRUE)             -0.65560    0.72609  -0.903   0.3666  
mylog(poblacion, TRUE)           3.20970    1.71051   1.876   0.0606 .
mylog(N, TRUE)                   1.24250    3.62948   0.342   0.7321  
scale(General, scale = FALSE)    0.04325    0.08021   0.539   0.5898  
scale(Derecho, scale = FALSE)   -0.15316    0.06513  -2.352   0.0187 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 115.17  on 84  degrees of freedom
Residual deviance:  52.16  on 56  degrees of freedom
AIC: 110.16

Number of Fisher Scoring iterations: 16
```

```r
vif(m1_inperson, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                2.659193  4        1.130040
had_weapon_NA                 2.311560  2        1.233037
mylog(extortions)             1.035702  1        1.017695
mylog(bribes)                 1.095687  1        1.046750
subsector                     1.303174  6        1.022312
size                          1.162708  3        1.025444
yearsquant                    1.262117  4        1.029526
mylog(bribes_abvic, TRUE)     1.785250  1        1.336132
mylog(armas, TRUE)            4.511796  1        2.124099
mylog(drogas, TRUE)           2.866046  1        1.692940
mylog(poblacion, TRUE)        2.381907  1        1.543343
mylog(N, TRUE)                1.788203  1        1.337237
scale(General, scale = FALSE) 1.319719  1        1.148790
scale(Derecho, scale = FALSE) 1.901729  1        1.379032
```

```r
logLik(m1_inperson)
```

```
'log Lik.' -26.07992 (df=29)
```

```r
m1_inperson_step <- step(m1_inperson, direction = "both")
```

```
Start:  AIC=110.16
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- had_weapon_NA                  2   52.527 106.53
- mylog(bribes)                  1   52.160 108.16
- mylog(N, TRUE)                 1   52.276 108.28
- scale(General, scale = FALSE)  1   52.455 108.45
- mylog(armas, TRUE)             1   52.457 108.46
- yearsquant                     4   58.609 108.61
- mylog(drogas, TRUE)            1   53.024 109.02
- subsector                      6   63.971 109.97
<none>                               52.160 110.16
- mylog(bribes_abvic, TRUE)      1   54.176 110.18
- size                           3   59.992 111.99
- mylog(poblacion, TRUE)         1   56.840 112.84
- mylog(extortions)              1   58.106 114.11
- n_offenders_NA                 4   64.795 114.80
- scale(Derecho, scale = FALSE)  1   59.841 115.84

Step:  AIC=106.53
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes) + 
    subsector + size + yearsquant + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- mylog(bribes)                  1   52.538 104.54
- mylog(N, TRUE)                 1   52.632 104.63
- mylog(armas, TRUE)             1   52.760 104.76
- scale(General, scale = FALSE)  1   52.796 104.80
- mylog(drogas, TRUE)            1   53.494 105.49
- yearsquant                     4   59.849 105.85
- subsector                      6   64.364 106.36
<none>                               52.527 106.53
- mylog(bribes_abvic, TRUE)      1   54.559 106.56
- size                           3   60.599 108.60
- mylog(poblacion, TRUE)         1   57.428 109.43
+ had_weapon_NA                  2   52.160 110.16
- mylog(extortions)              1   58.565 110.56
- scale(Derecho, scale = FALSE)  1   60.127 112.13
- n_offenders_NA                 4   75.128 121.13

Step:  AIC=104.54
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + subsector + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + mylog(N, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- mylog(N, TRUE)                 1   52.652 102.65
- mylog(armas, TRUE)             1   52.769 102.77
- scale(General, scale = FALSE)  1   52.807 102.81
- mylog(drogas, TRUE)            1   53.494 103.49
- yearsquant                     4   59.932 103.93
- subsector                      6   64.365 104.36
<none>                               52.538 104.54
- mylog(bribes_abvic, TRUE)      1   54.568 104.57
+ mylog(bribes)                  1   52.527 106.53
- size                           3   60.751 106.75
- mylog(poblacion, TRUE)         1   57.470 107.47
+ had_weapon_NA                  2   52.160 108.16
- mylog(extortions)              1   58.839 108.84
- scale(Derecho, scale = FALSE)  1   60.443 110.44
- n_offenders_NA                 4   75.781 119.78

Step:  AIC=102.65
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + subsector + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)

                                Df Deviance    AIC
- mylog(armas, TRUE)             1   52.843 100.84
- scale(General, scale = FALSE)  1   52.994 100.99
- mylog(drogas, TRUE)            1   54.044 102.04
<none>                               52.652 102.65
- subsector                      6   64.725 102.72
- yearsquant                     4   60.778 102.78
- mylog(bribes_abvic, TRUE)      1   54.976 102.98
+ mylog(N, TRUE)                 1   52.538 104.54
+ mylog(bribes)                  1   52.632 104.63
- mylog(poblacion, TRUE)         1   57.472 105.47
- size                           3   61.566 105.57
+ had_weapon_NA                  2   52.278 106.28
- mylog(extortions)              1   59.400 107.40
- scale(Derecho, scale = FALSE)  1   61.685 109.69
- n_offenders_NA                 4   76.239 118.24

Step:  AIC=100.84
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + subsector + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)

                                Df Deviance     AIC
- scale(General, scale = FALSE)  1   53.423  99.423
<none>                               52.843 100.843
- yearsquant                     4   60.861 100.861
- subsector                      6   65.035 101.035
- mylog(bribes_abvic, TRUE)      1   55.408 101.408
- mylog(drogas, TRUE)            1   56.047 102.047
+ mylog(armas, TRUE)             1   52.652 102.652
+ mylog(N, TRUE)                 1   52.769 102.769
+ mylog(bribes)                  1   52.828 102.828
- size                           3   61.805 103.805
+ had_weapon_NA                  2   52.554 104.554
- mylog(extortions)              1   59.419 105.419
- mylog(poblacion, TRUE)         1   59.977 105.977
- scale(Derecho, scale = FALSE)  1   62.064 108.064
- n_offenders_NA                 4   76.469 116.469

Step:  AIC=99.42
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + subsector + 
    size + yearsquant + mylog(bribes_abvic, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)

                                Df Deviance     AIC
- yearsquant                     4   60.861  98.861
- subsector                      6   65.054  99.054
<none>                               53.423  99.423
- mylog(bribes_abvic, TRUE)      1   55.464  99.464
- mylog(drogas, TRUE)            1   56.077 100.077
+ scale(General, scale = FALSE)  1   52.843 100.843
+ mylog(armas, TRUE)             1   52.994 100.994
+ mylog(N, TRUE)                 1   53.248 101.248
+ mylog(bribes)                  1   53.410 101.410
- size                           3   61.823 101.823
+ had_weapon_NA                  2   53.201 103.201
- mylog(extortions)              1   59.420 103.420
- mylog(poblacion, TRUE)         1   60.048 104.048
- scale(Derecho, scale = FALSE)  1   62.333 106.333
- n_offenders_NA                 4   76.674 114.674

Step:  AIC=98.86
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + subsector + 
    size + mylog(bribes_abvic, TRUE) + mylog(drogas, TRUE) + 
    mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)

                                Df Deviance     AIC
- subsector                      6   71.533  97.533
- mylog(bribes_abvic, TRUE)      1   61.808  97.808
- mylog(drogas, TRUE)            1   62.361  98.361
<none>                               60.861  98.861
+ yearsquant                     4   53.423  99.423
- mylog(extortions)              1   63.544  99.544
+ mylog(N, TRUE)                 1   60.077 100.077
- size                           3   68.677 100.677
+ mylog(bribes)                  1   60.761 100.761
+ mylog(armas, TRUE)             1   60.795 100.795
+ scale(General, scale = FALSE)  1   60.861 100.861
- mylog(poblacion, TRUE)         1   65.068 101.068
+ had_weapon_NA                  2   60.165 102.165
- scale(Derecho, scale = FALSE)  1   69.260 105.260
- n_offenders_NA                 4   83.153 113.153

Step:  AIC=97.53
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + size + 
    mylog(bribes_abvic, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)

                                Df Deviance     AIC
- size                           3   75.482  95.482
- mylog(bribes_abvic, TRUE)      1   71.607  95.607
- mylog(drogas, TRUE)            1   71.840  95.840
<none>                               71.533  97.533
+ mylog(N, TRUE)                 1   69.944  97.944
- mylog(extortions)              1   74.399  98.399
+ subsector                      6   60.861  98.861
+ yearsquant                     4   65.054  99.054
+ mylog(bribes)                  1   71.446  99.446
+ mylog(armas, TRUE)             1   71.485  99.485
+ scale(General, scale = FALSE)  1   71.530  99.530
- scale(Derecho, scale = FALSE)  1   76.111 100.111
+ had_weapon_NA                  2   70.878 100.878
- mylog(poblacion, TRUE)         1   77.380 101.380
- n_offenders_NA                 4   94.742 112.742

Step:  AIC=95.48
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(bribes_abvic, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(Derecho, 
    scale = FALSE)

                                Df Deviance     AIC
- mylog(bribes_abvic, TRUE)      1   75.482  93.482
- mylog(drogas, TRUE)            1   75.608  93.608
<none>                               75.482  95.482
- mylog(extortions)              1   77.798  95.798
+ yearsquant                     4   68.361  96.361
+ scale(General, scale = FALSE)  1   75.387  97.387
+ mylog(N, TRUE)                 1   75.430  97.430
+ mylog(armas, TRUE)             1   75.478  97.478
+ mylog(bribes)                  1   75.482  97.482
+ size                           3   71.533  97.533
- scale(Derecho, scale = FALSE)  1   79.924  97.924
- mylog(poblacion, TRUE)         1   80.493  98.493
+ had_weapon_NA                  2   75.460  99.460
+ subsector                      6   68.677 100.677
- n_offenders_NA                 4  100.959 112.959

Step:  AIC=93.48
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)

                                Df Deviance     AIC
- mylog(drogas, TRUE)            1   75.610  91.610
<none>                               75.482  93.482
- mylog(extortions)              1   77.885  93.885
+ yearsquant                     4   68.636  94.636
+ scale(General, scale = FALSE)  1   75.402  95.402
+ mylog(N, TRUE)                 1   75.430  95.430
+ mylog(armas, TRUE)             1   75.478  95.478
+ mylog(bribes_abvic, TRUE)      1   75.482  95.482
+ mylog(bribes)                  1   75.482  95.482
+ size                           3   71.607  95.607
- mylog(poblacion, TRUE)         1   80.540  96.540
- scale(Derecho, scale = FALSE)  1   80.953  96.953
+ had_weapon_NA                  2   75.460  97.460
+ subsector                      6   68.729  98.729
- n_offenders_NA                 4  101.779 111.779

Step:  AIC=91.61
complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)

                                Df Deviance     AIC
<none>                               75.610  91.610
- mylog(extortions)              1   77.962  91.962
+ yearsquant                     4   68.640  92.640
+ mylog(N, TRUE)                 1   75.450  93.450
+ mylog(drogas, TRUE)            1   75.482  93.482
+ scale(General, scale = FALSE)  1   75.492  93.492
+ mylog(armas, TRUE)             1   75.583  93.583
+ mylog(bribes_abvic, TRUE)      1   75.608  93.608
+ mylog(bribes)                  1   75.609  93.609
+ size                           3   71.953  93.953
- scale(Derecho, scale = FALSE)  1   81.055  95.055
- mylog(poblacion, TRUE)         1   81.433  95.433
+ had_weapon_NA                  2   75.581  95.581
+ subsector                      6   69.153  97.153
- n_offenders_NA                 4  102.649 110.649
```

```r
summary(m1_inperson_step)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(extortions) + 
    mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE), family = binomial(), 
    data = enve_inperson)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8585  -0.6600  -0.2624   0.6060   2.3521  

Coefficients:
                              Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   -1.76040    0.57080  -3.084 0.002042 ** 
n_offenders_NA2                2.49118    0.84528   2.947 0.003207 ** 
n_offenders_NA3               -0.36069    1.21466  -0.297 0.766505    
n_offenders_NA4+               3.34052    0.89958   3.713 0.000204 ***
n_offenders_NADK/DA            1.63054    1.00526   1.622 0.104801    
mylog(extortions)              0.86467    0.60704   1.424 0.154328    
mylog(poblacion, TRUE)         1.26064    0.56689   2.224 0.026162 *  
scale(Derecho, scale = FALSE) -0.05663    0.02581  -2.194 0.028211 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 115.17  on 84  degrees of freedom
Residual deviance:  75.61  on 77  degrees of freedom
AIC: 91.61

Number of Fisher Scoring iterations: 5
```

```r
logLik(m1_inperson_step)
```

```
'log Lik.' -37.80493 (df=8)
```

```r
waldtest(m1_inperson_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df Df  Chisq Pr(>Chisq)   
1     77                        
2     84 -7 21.803   0.002747 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_inperson_step, m1_inperson, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)
1     77                     
2     56 21 12.086     0.9373
```

```r
car::vif(m1_inperson_step)
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                1.441396  4        1.046762
mylog(extortions)             1.150894  1        1.072797
mylog(poblacion, TRUE)        1.124405  1        1.060379
scale(Derecho, scale = FALSE) 1.388998  1        1.178558
```

```r
lrtest(m1_inperson_step, m1_inperson)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df Chisq Pr(>Chisq)
1   8 -37.805                    
2  29 -26.080 21 23.45     0.3205
```

```r
Anova(m1_inperson)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)   
n_offenders_NA                 12.6347  4    0.01321 * 
had_weapon_NA                   0.3667  2    0.83247   
mylog(extortions)               5.9459  1    0.01475 * 
mylog(bribes)                   0.0000  1    0.99786   
subsector                      11.8109  6    0.06632 . 
size                            7.8324  3    0.04960 * 
yearsquant                      6.4493  4    0.16801   
mylog(bribes_abvic, TRUE)       2.0162  1    0.15563   
mylog(armas, TRUE)              0.2975  1    0.58547   
mylog(drogas, TRUE)             0.8639  1    0.35265   
mylog(poblacion, TRUE)          4.6806  1    0.03051 * 
mylog(N, TRUE)                  0.1166  1    0.73277   
scale(General, scale = FALSE)   0.2952  1    0.58690   
scale(Derecho, scale = FALSE)   7.6811  1    0.00558 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_inperson_step)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                 27.0387  4  1.952e-05 ***
mylog(extortions)               2.3525  1    0.12508    
mylog(poblacion, TRUE)          5.8230  1    0.01582 *  
scale(Derecho, scale = FALSE)   5.4450  1    0.01962 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_inperson)
```

```
$accuracy
[1] 85.882

$naive
[1] 58.824
```

```r
accuracy(m1_inperson_step)
```

```
$accuracy
[1] 78.824

$naive
[1] 58.824
```

```r
# Robust SE

m1_inpersonCL <- summaryCL(m1_inperson, cluster_form)
m1_inpersonCL
```

```
$coefs
                                   estimate         SE      z.value      p.value      ci.low     ci.high sig
(Intercept)                    -6.370612607 4.10422274 -1.552209276 1.206122e-01 -14.4147414  1.67351615    
n_offenders_NA2                 3.793595320 2.33460748  1.624939249 1.041755e-01  -0.7821513  8.36934189    
n_offenders_NA3                 0.665244544 2.89895854  0.229477081 8.184981e-01  -5.0166098  6.34709887    
n_offenders_NA4+                6.054890629 2.75354374  2.198944780 2.788185e-02   0.6580441 11.45173719   *
n_offenders_NADK/DA             1.340212856 1.81338015  0.739068891 4.598652e-01  -2.2139469  4.89437264    
had_weapon_NAYes               -0.045425914 1.19262161 -0.038089126 9.696166e-01  -2.3829213  2.29206948    
had_weapon_NADK/DA              4.062826874 3.19956551  1.269805808 2.041538e-01  -2.2082063 10.33386005    
mylog(extortions)               2.613530706 1.59264750  1.640997588 1.007979e-01  -0.5080010  5.73506245    
mylog(bribes)                  -0.003039969 0.73142407 -0.004156233 9.966838e-01  -1.4366048  1.43052486    
subsectorHotelsRestBar          1.189595093 6.46853492  0.183904873 8.540881e-01 -11.4885004 13.86769057    
subsectorManufacturing          7.307790071 4.05452438  1.802379115 7.148578e-02  -0.6389317 15.25451184    
subsectorOther industry         6.757495348 4.25683160  1.587447186 1.124114e-01  -1.5857413 15.10073198    
subsectorOther serv.            5.862864667 4.90648212  1.194922254 2.321174e-01  -3.7536636 15.47939290    
subsectorTransport              2.977020031 4.31151614  0.690481013 4.898917e-01  -5.4733963 11.42743638    
subsectorWholesale            -11.431667371 4.78178848 -2.390667724 1.681777e-02 -20.8038006 -2.05953418   *
sizeMedium                     -3.316280516 1.41679960 -2.340684249 1.924844e-02  -6.0931567 -0.53940432   *
sizeSmall                      -4.588124463 2.07083846 -2.215587816 2.671974e-02  -8.6468933 -0.52935566   *
sizeMicro                       0.573777726 1.26939359  0.452009316 6.512623e-01  -1.9141880  3.06174344    
yearsquant(8,16]               -0.402031377 2.08458957 -0.192858768 8.470696e-01  -4.4877519  3.68368910    
yearsquant(16,25]              -2.029877304 1.72787407 -1.174783128 2.400815e-01  -5.4164482  1.35669364    
yearsquant(25,34]              -2.779159618 2.24632788 -1.237201230 2.160124e-01  -7.1818814  1.62356213    
yearsquant(34,43]               0.751550511 1.48215446  0.507066253 6.121083e-01  -2.1534189  3.65651988    
mylog(bribes_abvic, TRUE)       2.618670368 2.92643085  0.894834187 3.708757e-01  -3.1170287  8.35436944    
mylog(armas, TRUE)             -0.776781219 1.05969074 -0.733026335 4.635424e-01  -2.8537369  1.30017447    
mylog(drogas, TRUE)            -0.655597424 0.45568319 -1.438713217 1.502318e-01  -1.5487201  0.23752521    
mylog(poblacion, TRUE)          3.209697593 1.54042325  2.083646553 3.719233e-02   0.1905235  6.22887168   *
mylog(N, TRUE)                  1.242501638 2.60510198  0.476949328 6.333982e-01  -3.8634044  6.34840769    
scale(General, scale = FALSE)   0.043245942 0.12094715  0.357560665 7.206721e-01  -0.1938061  0.28029799    
scale(Derecho, scale = FALSE)  -0.153158563 0.03780592 -4.051179783 5.096003e-05  -0.2272568 -0.07906033 ***

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df Chisq Pr(>Chisq)    
1     56                         
2     84 -28 45060  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m1_inperson_stepCL <- summaryCL(m1_inperson_step, cluster_form)
m1_inperson_stepCL
```

```
$coefs
                                 estimate         SE    z.value     p.value      ci.low     ci.high sig
(Intercept)                   -1.76039516 0.70591869 -2.4937648 0.012639627 -3.14397036 -0.37681996   *
n_offenders_NA2                2.49118096 1.00969376  2.4672639 0.013614998  0.51221755  4.47014436   *
n_offenders_NA3               -0.36069333 1.47663839 -0.2442665 0.807024407 -3.25485138  2.53346473    
n_offenders_NA4+               3.34051988 1.11000729  3.0094576 0.002617146  1.16494557  5.51609418  **
n_offenders_NADK/DA            1.63053761 0.98535696  1.6547685 0.097971475 -0.30072654  3.56180177    
mylog(extortions)              0.86466790 1.03449004  0.8358398 0.403245061 -1.16289533  2.89223112    
mylog(poblacion, TRUE)         1.26064073 0.60494609  2.0838894 0.037170233  0.07496817  2.44631328   *
scale(Derecho, scale = FALSE) -0.05663114 0.02065163 -2.7422120 0.006102693 -0.09710758 -0.01615469  **

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df Df  Chisq Pr(>Chisq)    
1     77                         
2     84 -7 24.715  0.0008521 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_inperson_step, m1_inperson, test = "Chisq", vcov = vcov(m1_inpersonCL))
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(extortions) + mylog(poblacion, 
    TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)    
1     77                         
2     56 21 308.78  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# using m1_step terms

m1_inperson_step2 <- update(m1_step, 
                             . ~ . - extortion_type,
                             data = enve_inperson)


summary(m1_inperson_step2)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE), family = binomial(), data = enve_inperson)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3522  -0.7255  -0.2703   0.6054   2.6255  

Coefficients:
                          Estimate Std. Error z value Pr(>|z|)   
(Intercept)                -1.3810     0.6551  -2.108  0.03501 * 
n_offenders_NA2             2.0331     0.7935   2.562  0.01040 * 
n_offenders_NA3            -0.9321     1.2373  -0.753  0.45125   
n_offenders_NA4+            3.3709     1.0545   3.197  0.00139 **
n_offenders_NADK/DA         1.3829     1.0442   1.324  0.18538   
mylog(bribes)              -0.1957     0.8139  -0.240  0.80999   
yearsquant(8,16]           -0.7267     1.2142  -0.599  0.54950   
yearsquant(16,25]           0.2115     0.8723   0.242  0.80842   
yearsquant(25,34]          -1.3844     1.0397  -1.332  0.18301   
yearsquant(34,43]           1.2332     0.8807   1.400  0.16140   
mylog(bribes_abvic, TRUE)   0.6394     0.8098   0.790  0.42976   
mylog(armas, TRUE)          0.9960     0.6428   1.549  0.12127   
mylog(drogas, TRUE)        -0.1286     0.4114  -0.313  0.75453   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 115.174  on 84  degrees of freedom
Residual deviance:  75.806  on 72  degrees of freedom
AIC: 101.81

Number of Fisher Scoring iterations: 5
```

```r
logLik(m1_inperson_step2)
```

```
'log Lik.' -37.90286 (df=13)
```

```r
waldtest(m1_inperson_step2, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)  
1     72                        
2     84 -12 21.933    0.03828 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_inperson_step2, m1_inperson, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1     72                    
2     56 16 10.58     0.8346
```

```r
car::vif(m1_inperson_step2)
```

```
                              GVIF Df GVIF^(1/(2*Df))
n_offenders_NA            2.058574  4        1.094450
mylog(bribes)             1.510020  1        1.228829
yearsquant                1.943846  4        1.086633
mylog(bribes_abvic, TRUE) 1.419190  1        1.191298
mylog(armas, TRUE)        2.524980  1        1.589018
mylog(drogas, TRUE)       2.564279  1        1.601337
```

```r
lrtest(m1_inperson_step2, m1_inperson)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)  
1  13 -37.903                       
2  29 -26.080 16 23.646    0.09753 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_inperson)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)   
n_offenders_NA                 12.6347  4    0.01321 * 
had_weapon_NA                   0.3667  2    0.83247   
mylog(extortions)               5.9459  1    0.01475 * 
mylog(bribes)                   0.0000  1    0.99786   
subsector                      11.8109  6    0.06632 . 
size                            7.8324  3    0.04960 * 
yearsquant                      6.4493  4    0.16801   
mylog(bribes_abvic, TRUE)       2.0162  1    0.15563   
mylog(armas, TRUE)              0.2975  1    0.58547   
mylog(drogas, TRUE)             0.8639  1    0.35265   
mylog(poblacion, TRUE)          4.6806  1    0.03051 * 
mylog(N, TRUE)                  0.1166  1    0.73277   
scale(General, scale = FALSE)   0.2952  1    0.58690   
scale(Derecho, scale = FALSE)   7.6811  1    0.00558 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_inperson_step2)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)    
n_offenders_NA             20.9864  4  0.0003186 ***
mylog(bribes)               0.0578  1  0.8100155    
yearsquant                  6.5959  4  0.1588500    
mylog(bribes_abvic, TRUE)   0.6342  1  0.4258127    
mylog(armas, TRUE)          2.9386  1  0.0864836 .  
mylog(drogas, TRUE)         0.0984  1  0.7537427    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_inperson)
```

```
$accuracy
[1] 85.882

$naive
[1] 58.824
```

```r
accuracy(m1_inperson_step2)
```

```
$accuracy
[1] 83.529

$naive
[1] 58.824
```

```r
# Robust SE



m1_inperson_step2CL <- summaryCL(m1_inperson_step2, cluster_form)
m1_inperson_step2CL
```

```
$coefs
                            estimate        SE    z.value     p.value      ci.low   ci.high sig
(Intercept)               -1.3809818 0.8187454 -1.6867049 0.091660112 -2.98569330 0.2237296    
n_offenders_NA2            2.0331273 1.0299774  1.9739534 0.048387044  0.01440876 4.0518458   *
n_offenders_NA3           -0.9320898 1.4957057 -0.6231773 0.533168008 -3.86361908 1.9994394    
n_offenders_NA4+           3.3709327 1.2589799  2.6775112 0.007417137  0.90337754 5.8384879  **
n_offenders_NADK/DA        1.3829242 0.8343921  1.6574034 0.097437946 -0.25245415 3.0183026    
mylog(bribes)             -0.1956901 0.6109381 -0.3203109 0.748732676 -1.39310678 1.0017265    
yearsquant(8,16]          -0.7267133 0.9193372 -0.7904753 0.429250271 -2.52858097 1.0751544    
yearsquant(16,25]          0.2115007 0.9608735  0.2201130 0.825783166 -1.67177664 2.0947781    
yearsquant(25,34]         -1.3844233 0.9587508 -1.4439866 0.148742747 -3.26354032 0.4946937    
yearsquant(34,43]          1.2332296 0.8086814  1.5249882 0.127262030 -0.35175677 2.8182159    
mylog(bribes_abvic, TRUE)  0.6394489 0.7690733  0.8314537 0.405717370 -0.86790717 2.1468049    
mylog(armas, TRUE)         0.9960286 0.6227829  1.5993190 0.109749730 -0.22460350 2.2166606    
mylog(drogas, TRUE)       -0.1286307 0.2705373 -0.4754637 0.634456504 -0.65887411 0.4016128    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1     72                          
2     84 -12 45.895  7.233e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_inperson_step2, m1_inperson, test = "Chisq", vcov = vcov(m1_inpersonCL))
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)    
1     72                        
2     56 16 323.7  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

- Inperson (agg) wocp



```r
enve_inperson_wocp <- filter(enve_nona, extortion_type_wocp == "In person")

m1_inperson_wocp <- glm(formula = compliance_formula2, 
                 data = enve_inperson_wocp, 
                 family = binomial())  
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(m1_inperson_wocp)
```

```

Call:
glm(formula = compliance_formula2, family = binomial(), data = enve_inperson_wocp)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.11502  -0.26532  -0.00001   0.29476   2.27032  

Coefficients:
                                Estimate Std. Error z value Pr(>|z|)   
(Intercept)                   -1.091e+01  1.075e+04  -0.001  0.99919   
n_offenders_NA2                5.957e+00  2.114e+00   2.818  0.00483 **
n_offenders_NA3               -2.008e+01  2.199e+03  -0.009  0.99271   
n_offenders_NA4+               8.416e+00  3.059e+00   2.752  0.00593 **
n_offenders_NADK/DA            3.585e+00  1.761e+01   0.204  0.83863   
had_weapon_NAYes              -1.829e+00  1.718e+00  -1.064  0.28712   
had_weapon_NADK/DA             3.991e+00  1.761e+01   0.227  0.82070   
mylog(extortions)              8.740e-01  1.540e+00   0.568  0.57029   
mylog(bribes)                  9.399e-01  1.525e+00   0.616  0.53776   
subsectorHotelsRestBar         1.443e+00  1.075e+04   0.000  0.99989   
subsectorManufacturing         8.062e+00  1.075e+04   0.001  0.99940   
subsectorOther industry        9.870e+00  1.075e+04   0.001  0.99927   
subsectorOther serv.           7.108e+00  1.075e+04   0.001  0.99947   
subsectorTransport             4.948e+00  1.075e+04   0.000  0.99963   
subsectorWholesale            -1.413e+01  1.242e+04  -0.001  0.99909   
sizeMedium                    -1.513e+00  1.986e+00  -0.762  0.44620   
sizeSmall                     -4.868e+00  2.275e+00  -2.140  0.03239 * 
sizeMicro                      3.254e+00  2.194e+00   1.483  0.13813   
yearsquant(8,16]              -4.526e-01  1.722e+00  -0.263  0.79266   
yearsquant(16,25]             -2.901e-01  2.174e+00  -0.133  0.89383   
yearsquant(25,34]             -1.518e+00  1.686e+00  -0.900  0.36813   
yearsquant(34,43]              2.009e+00  1.562e+00   1.286  0.19837   
mylog(bribes_abvic, TRUE)      1.912e+00  2.234e+00   0.856  0.39194   
mylog(armas, TRUE)             2.508e-01  1.878e+00   0.134  0.89376   
mylog(drogas, TRUE)           -1.815e+00  1.058e+00  -1.716  0.08616 . 
mylog(poblacion, TRUE)         1.774e+00  2.060e+00   0.861  0.38918   
mylog(N, TRUE)                -2.768e+00  5.742e+00  -0.482  0.62983   
scale(General, scale = FALSE)  1.037e-01  9.358e-02   1.108  0.26800   
scale(Derecho, scale = FALSE) -2.360e-01  9.750e-02  -2.420  0.01551 * 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 104.825  on 77  degrees of freedom
Residual deviance:  36.949  on 49  degrees of freedom
AIC: 94.949

Number of Fisher Scoring iterations: 18
```

```r
vif(m1_inperson_wocp, data = enve_nona) 
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                2.659193  4        1.130040
had_weapon_NA                 2.311560  2        1.233037
mylog(extortions)             1.035702  1        1.017695
mylog(bribes)                 1.095687  1        1.046750
subsector                     1.303174  6        1.022312
size                          1.162708  3        1.025444
yearsquant                    1.262117  4        1.029526
mylog(bribes_abvic, TRUE)     1.785250  1        1.336132
mylog(armas, TRUE)            4.511796  1        2.124099
mylog(drogas, TRUE)           2.866046  1        1.692940
mylog(poblacion, TRUE)        2.381907  1        1.543343
mylog(N, TRUE)                1.788203  1        1.337237
scale(General, scale = FALSE) 1.319719  1        1.148790
scale(Derecho, scale = FALSE) 1.901729  1        1.379032
```

```r
logLik(m1_inperson_wocp)
```

```
'log Lik.' -18.47429 (df=29)
```

```r
m1_inperson_wocp_step <- step(m1_inperson_wocp, direction = "both")
```

```
Start:  AIC=94.95
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- yearsquant                     4   41.140  91.140
- had_weapon_NA                  2   38.573  92.573
- mylog(armas, TRUE)             1   36.967  92.967
- mylog(N, TRUE)                 1   37.200  93.200
- mylog(extortions)              1   37.241  93.241
- mylog(bribes)                  1   37.343  93.343
- mylog(bribes_abvic, TRUE)      1   37.678  93.678
- mylog(poblacion, TRUE)         1   37.770  93.770
- scale(General, scale = FALSE)  1   38.204  94.204
<none>                               36.949  94.949
- size                           3   44.415  96.415
- mylog(drogas, TRUE)            1   41.137  97.137
- subsector                      6   52.941  98.941
- scale(Derecho, scale = FALSE)  1   45.308 101.308
- n_offenders_NA                 4   57.206 107.206
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=91.14
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- mylog(N, TRUE)                 1   41.142  89.142
- mylog(extortions)              1   41.143  89.143
- mylog(bribes)                  1   41.283  89.283
- mylog(armas, TRUE)             1   41.324  89.324
- mylog(bribes_abvic, TRUE)      1   41.528  89.528
- mylog(poblacion, TRUE)         1   41.576  89.576
- had_weapon_NA                  2   43.730  89.730
- scale(General, scale = FALSE)  1   42.582  90.582
<none>                               41.140  91.140
- mylog(drogas, TRUE)            1   44.749  92.749
- size                           3   48.863  92.863
- subsector                      6   56.641  94.641
+ yearsquant                     4   36.949  94.949
- scale(Derecho, scale = FALSE)  1   50.165  98.165
- n_offenders_NA                 4   66.859 108.859
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=89.14
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + mylog(bribes_abvic, TRUE) + 
    mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + scale(General, scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- mylog(extortions)              1   41.144  87.144
- mylog(bribes)                  1   41.284  87.284
- mylog(armas, TRUE)             1   41.324  87.324
- mylog(bribes_abvic, TRUE)      1   41.535  87.535
- mylog(poblacion, TRUE)         1   41.588  87.588
- had_weapon_NA                  2   43.857  87.857
- scale(General, scale = FALSE)  1   42.585  88.585
<none>                               41.142  89.142
+ mylog(N, TRUE)                 1   41.140  91.140
- mylog(drogas, TRUE)            1   45.968  91.968
- subsector                      6   56.852  92.852
+ yearsquant                     4   37.200  93.200
- size                           3   51.681  93.681
- scale(Derecho, scale = FALSE)  1   51.529  97.529
- n_offenders_NA                 4   69.530 109.530
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=87.14
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(bribes) + 
    subsector + size + mylog(bribes_abvic, TRUE) + mylog(armas, 
    TRUE) + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- mylog(bribes)                  1   41.321  85.321
- mylog(armas, TRUE)             1   41.325  85.325
- mylog(bribes_abvic, TRUE)      1   41.559  85.559
- mylog(poblacion, TRUE)         1   41.761  85.761
- had_weapon_NA                  2   43.862  85.862
- scale(General, scale = FALSE)  1   42.590  86.590
<none>                               41.144  87.144
+ mylog(extortions)              1   41.142  89.142
+ mylog(N, TRUE)                 1   41.143  89.143
- mylog(drogas, TRUE)            1   45.991  89.991
- subsector                      6   56.945  90.945
+ yearsquant                     4   37.464  91.464
- size                           3   53.279  93.279
- scale(Derecho, scale = FALSE)  1   53.019  97.019
- n_offenders_NA                 4   69.588 107.588
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=85.32
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + subsector + 
    size + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- mylog(armas, TRUE)             1   41.549  83.549
- mylog(bribes_abvic, TRUE)      1   41.742  83.742
- mylog(poblacion, TRUE)         1   41.962  83.962
- had_weapon_NA                  2   44.101  84.101
- scale(General, scale = FALSE)  1   42.734  84.734
<none>                               41.321  85.321
+ mylog(bribes)                  1   41.144  87.144
+ mylog(extortions)              1   41.284  87.284
+ mylog(N, TRUE)                 1   41.321  87.321
- mylog(drogas, TRUE)            1   46.298  88.298
- subsector                      6   57.628  89.628
+ yearsquant                     4   37.779  89.779
- size                           3   53.282  91.282
- scale(Derecho, scale = FALSE)  1   53.043  95.043
- n_offenders_NA                 4   72.963 108.963
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=83.55
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + subsector + 
    size + mylog(bribes_abvic, TRUE) + mylog(drogas, TRUE) + 
    mylog(poblacion, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- mylog(bribes_abvic, TRUE)      1   41.790  81.790
- had_weapon_NA                  2   44.301  82.301
- scale(General, scale = FALSE)  1   42.741  82.741
<none>                               41.549  83.549
+ mylog(armas, TRUE)             1   41.321  85.321
+ mylog(bribes)                  1   41.325  85.325
- mylog(poblacion, TRUE)         1   45.457  85.457
+ mylog(extortions)              1   41.544  85.544
+ mylog(N, TRUE)                 1   41.549  85.549
- mylog(drogas, TRUE)            1   47.542  87.542
+ yearsquant                     4   37.913  87.913
- subsector                      6   58.077  88.077
- size                           3   53.287  89.287
- scale(Derecho, scale = FALSE)  1   56.386  96.386
- n_offenders_NA                 4   72.966 106.966
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=81.79
complied_bin_NA ~ n_offenders_NA + had_weapon_NA + subsector + 
    size + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(General, 
    scale = FALSE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- had_weapon_NA                  2   44.585  80.585
- scale(General, scale = FALSE)  1   42.761  80.761
<none>                               41.790  81.790
+ mylog(bribes_abvic, TRUE)      1   41.549  83.549
+ mylog(bribes)                  1   41.597  83.597
+ mylog(armas, TRUE)             1   41.742  83.742
+ mylog(extortions)              1   41.748  83.748
+ mylog(N, TRUE)                 1   41.789  83.789
- mylog(poblacion, TRUE)         1   46.264  84.264
- mylog(drogas, TRUE)            1   47.601  85.601
- subsector                      6   58.086  86.086
+ yearsquant                     4   38.365  86.365
- size                           3   53.445  87.445
- scale(Derecho, scale = FALSE)  1   59.590  97.590
- n_offenders_NA                 4   73.182 105.182
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```

Step:  AIC=80.58
complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
- scale(General, scale = FALSE)  1   45.046  79.046
<none>                               44.585  80.585
+ had_weapon_NA                  2   41.790  81.790
+ mylog(bribes_abvic, TRUE)      1   44.301  82.301
+ mylog(bribes)                  1   44.331  82.331
+ mylog(N, TRUE)                 1   44.481  82.481
+ mylog(armas, TRUE)             1   44.553  82.553
+ mylog(extortions)              1   44.559  82.559
- mylog(poblacion, TRUE)         1   48.755  82.755
- subsector                      6   59.302  83.302
- size                           3   54.044  84.044
- mylog(drogas, TRUE)            1   50.139  84.139
+ yearsquant                     4   40.825  84.825
- scale(Derecho, scale = FALSE)  1   60.784  94.784
- n_offenders_NA                 4   77.298 105.298

Step:  AIC=79.05
complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
                                Df Deviance     AIC
<none>                               45.046  79.046
+ scale(General, scale = FALSE)  1   44.585  80.585
+ had_weapon_NA                  2   42.761  80.761
+ mylog(bribes)                  1   44.833  80.833
- mylog(poblacion, TRUE)         1   48.973  80.973
+ mylog(N, TRUE)                 1   44.994  80.994
+ mylog(extortions)              1   45.027  81.027
+ mylog(bribes_abvic, TRUE)      1   45.038  81.038
+ mylog(armas, TRUE)             1   45.046  81.046
- subsector                      6   59.618  81.618
- size                           3   54.152  82.152
- mylog(drogas, TRUE)            1   50.190  82.190
+ yearsquant                     4   41.201  83.201
- scale(Derecho, scale = FALSE)  1   60.788  92.788
- n_offenders_NA                 4   77.484 103.484
```

```r
summary(m1_inperson_wocp_step)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + subsector + 
    size + mylog(drogas, TRUE) + mylog(poblacion, TRUE) + scale(Derecho, 
    scale = FALSE), family = binomial(), data = enve_inperson_wocp)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.97390  -0.43716  -0.00006   0.39438   2.37915  

Coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   -1.260e+01  1.075e+04  -0.001 0.999065    
n_offenders_NA2                4.745e+00  1.650e+00   2.876 0.004033 ** 
n_offenders_NA3               -1.839e+01  2.340e+03  -0.008 0.993727    
n_offenders_NA4+               5.814e+00  1.692e+00   3.437 0.000589 ***
n_offenders_NADK/DA            5.404e+00  2.194e+00   2.463 0.013769 *  
subsectorHotelsRestBar         6.663e+00  1.075e+04   0.001 0.999506    
subsectorManufacturing         1.055e+01  1.075e+04   0.001 0.999217    
subsectorOther industry        1.256e+01  1.075e+04   0.001 0.999068    
subsectorOther serv.           1.008e+01  1.075e+04   0.001 0.999252    
subsectorTransport             5.977e+00  1.075e+04   0.001 0.999557    
subsectorWholesale            -1.044e+01  1.242e+04  -0.001 0.999329    
sizeMedium                    -2.169e+00  1.289e+00  -1.683 0.092351 .  
sizeSmall                     -3.495e+00  1.642e+00  -2.129 0.033273 *  
sizeMicro                      1.632e+00  1.430e+00   1.141 0.253750    
mylog(drogas, TRUE)           -1.145e+00  5.627e-01  -2.035 0.041864 *  
mylog(poblacion, TRUE)         1.628e+00  9.141e-01   1.781 0.074905 .  
scale(Derecho, scale = FALSE) -2.067e-01  6.879e-02  -3.005 0.002658 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 104.825  on 77  degrees of freedom
Residual deviance:  45.046  on 61  degrees of freedom
AIC: 79.046

Number of Fisher Scoring iterations: 18
```

```r
logLik(m1_inperson_wocp_step)
```

```
'log Lik.' -22.52278 (df=17)
```

```r
waldtest(m1_inperson_wocp_step, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)
1     61                      
2     77 -16 14.887     0.5329
```

```r
waldtest(m1_inperson_wocp_step, m1_inperson_wocp, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)
1     61                    
2     49 12 5.933     0.9194
```

```r
car::vif(m1_inperson_wocp_step)
```

```
                                  GVIF Df GVIF^(1/(2*Df))
n_offenders_NA                6.885377  4        1.272744
subsector                     5.728666  6        1.156568
size                          7.253979  3        1.391328
mylog(drogas, TRUE)           3.413100  1        1.847458
mylog(poblacion, TRUE)        2.158038  1        1.469026
scale(Derecho, scale = FALSE) 4.593944  1        2.143349
```

```r
lrtest(m1_inperson_wocp_step, m1_inperson_wocp)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df Chisq Pr(>Chisq)
1  17 -22.523                    
2  29 -18.474 12 8.097     0.7775
```

```r
Anova(m1_inperson_wocp)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                 20.2577  4  0.0004442 ***
had_weapon_NA                   1.6248  2  0.4437910    
mylog(extortions)               0.2922  1  0.5888200    
mylog(bribes)                   0.3948  1  0.5298052    
subsector                      15.9922  6  0.0137958 *  
size                            7.4666  3  0.0584224 .  
yearsquant                      4.1915  4  0.3807072    
mylog(bribes_abvic, TRUE)       0.7290  1  0.3931971    
mylog(armas, TRUE)              0.0181  1  0.8930403    
mylog(drogas, TRUE)             4.1880  1  0.0407106 *  
mylog(poblacion, TRUE)          0.8211  1  0.3648472    
mylog(N, TRUE)                  0.2514  1  0.6160610    
scale(General, scale = FALSE)   1.2555  1  0.2625084    
scale(Derecho, scale = FALSE)   8.3593  1  0.0038373 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_inperson_wocp_step)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                  32.439  4  1.556e-06 ***
subsector                       14.573  6    0.02385 *  
size                             9.106  3    0.02791 *  
mylog(drogas, TRUE)              5.144  1    0.02333 *  
mylog(poblacion, TRUE)           3.928  1    0.04749 *  
scale(Derecho, scale = FALSE)   15.742  1  7.259e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_inperson_wocp)
```

```
$accuracy
[1] 92.308

$naive
[1] 60.256
```

```r
accuracy(m1_inperson_wocp_step)
```

```
$accuracy
[1] 83.333

$naive
[1] 60.256
```

```r
# Robust SE

m1_inperson_wocpCL <- summaryCL(m1_inperson_wocp, cluster_form)
```

```
Warning in sqrt(diag(se)): NaNs produced
```

```
Warning in sqrt(diag(se)): NaNs produced
```

```r
m1_inperson_wocpCL
```

```
$coefs
                                 estimate         SE    z.value      p.value       ci.low      ci.high  sig
(Intercept)                   -10.9078992        NaN        NaN          NaN          NaN          NaN <NA>
n_offenders_NA2                 5.9565123 2.35075312  2.5338741 1.128093e-02   1.34912088  10.56390377    *
n_offenders_NA3               -20.0847470 3.41174437 -5.8869437 3.934026e-09 -26.77164308 -13.39785092  ***
n_offenders_NA4+                8.4159802 2.81142726  2.9934903 2.758063e-03   2.90568400  13.92627634   **
n_offenders_NADK/DA             3.5853645 3.01622553  1.1886924 2.345607e-01  -2.32632895   9.49705786     
had_weapon_NAYes               -1.8292112 1.21159035 -1.5097604 1.311046e-01  -4.20388461   0.54546227     
had_weapon_NADK/DA              3.9913294 2.22276010  1.7956636 7.254803e-02  -0.36520032   8.34785915     
mylog(extortions)               0.8740022 0.89673925  0.9746447 3.297365e-01  -0.88357446   2.63157879     
mylog(bribes)                   0.9399310 1.16073699  0.8097709 4.180719e-01  -1.33507168   3.21493369     
subsectorHotelsRestBar          1.4430079        NaN        NaN          NaN          NaN          NaN <NA>
subsectorManufacturing          8.0618885        NaN        NaN          NaN          NaN          NaN <NA>
subsectorOther industry         9.8700296        NaN        NaN          NaN          NaN          NaN <NA>
subsectorOther serv.            7.1084191        NaN        NaN          NaN          NaN          NaN <NA>
subsectorTransport              4.9482957        NaN        NaN          NaN          NaN          NaN <NA>
subsectorWholesale            -14.1304872        NaN        NaN          NaN          NaN          NaN <NA>
sizeMedium                     -1.5130129 2.01289028 -0.7516619 4.522544e-01  -5.45820535   2.43217955     
sizeSmall                      -4.8675884 1.52605553 -3.1896535 1.424435e-03  -7.85860226  -1.87657452   **
sizeMicro                       3.2535749 1.93666943  1.6799846 9.296030e-02  -0.54222744   7.04937721     
yearsquant(8,16]               -0.4525645 1.66309967 -0.2721211 7.855289e-01  -3.71217999   2.80705092     
yearsquant(16,25]              -0.2900741 1.77686979 -0.1632501 8.703216e-01  -3.77267489   3.19252670     
yearsquant(25,34]              -1.5176799 1.74042346 -0.8720176 3.831988e-01  -4.92884715   1.89348743     
yearsquant(34,43]               2.0085568 1.91220061  1.0503902 2.935387e-01  -1.73928750   5.75640116     
mylog(bribes_abvic, TRUE)       1.9124560 2.71723710  0.7038237 4.815425e-01  -3.41323087   7.23814284     
mylog(armas, TRUE)              0.2508392 0.91629617  0.2737534 7.842741e-01  -1.54506831   2.04674668     
mylog(drogas, TRUE)            -1.8150962 0.91295211 -1.9881614 4.679384e-02  -3.60444944  -0.02574292    *
mylog(poblacion, TRUE)          1.7735170 0.90636381  1.9567385 5.037822e-02  -0.00292342   3.54995744     
mylog(N, TRUE)                 -2.7675229 3.40884168 -0.8118661 4.168685e-01  -9.44872986   3.91368400     
scale(General, scale = FALSE)   0.1036543 0.11499884  0.9013505 3.674020e-01  -0.12173933   0.32904784     
scale(Derecho, scale = FALSE)  -0.2359681 0.07088015 -3.3291148 8.712249e-04  -0.37489068  -0.09704561  ***

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df   Chisq Pr(>Chisq)    
1     49                           
2     77 -28 7963277  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
m1_inperson_wocp_stepCL <- summaryCL(m1_inperson_wocp_step, cluster_form)
```

```
Warning in sqrt(diag(se)): NaNs produced

Warning in sqrt(diag(se)): NaNs produced
```

```r
m1_inperson_wocp_stepCL
```

```
$coefs
                                 estimate         SE    z.value      p.value       ci.low      ci.high  sig
(Intercept)                   -12.5998599        NaN        NaN          NaN          NaN          NaN <NA>
n_offenders_NA2                 4.7446974 1.84374423   2.573403 1.007038e-02   1.13102507   8.35836965    *
n_offenders_NA3               -18.3948691 1.68499381 -10.916876 9.573174e-28 -21.69739624 -15.09234190  ***
n_offenders_NA4+                5.8137566 1.79981469   3.230197 1.237048e-03   2.28618460   9.34132856   **
n_offenders_NADK/DA             5.4044539 2.72958193   1.979957 4.770840e-02   0.05457161  10.75433615    *
subsectorHotelsRestBar          6.6633919        NaN        NaN          NaN          NaN          NaN <NA>
subsectorManufacturing         10.5491505        NaN        NaN          NaN          NaN          NaN <NA>
subsectorOther industry        12.5603335        NaN        NaN          NaN          NaN          NaN <NA>
subsectorOther serv.           10.0823850        NaN        NaN          NaN          NaN          NaN <NA>
subsectorTransport              5.9770705        NaN        NaN          NaN          NaN          NaN <NA>
subsectorWholesale            -10.4442339        NaN        NaN          NaN          NaN          NaN <NA>
sizeMedium                     -2.1691522 1.46578995  -1.479852 1.389127e-01  -5.04204775   0.70374326     
sizeSmall                      -3.4951282 1.44784955  -2.414013 1.577788e-02  -6.33286118  -0.65739522    *
sizeMicro                       1.6316037 1.43341142   1.138266 2.550094e-01  -1.17783109   4.44103842     
mylog(drogas, TRUE)            -1.1449726 0.55773820  -2.052885 4.008370e-02  -2.23811938  -0.05182581    *
mylog(poblacion, TRUE)          1.6280285 0.70174092   2.319985 2.034168e-02   0.25264159   3.00341543    *
scale(Derecho, scale = FALSE)  -0.2067119 0.06036311  -3.424475 6.159895e-04  -0.32502147  -0.08840242  ***

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1     61                          
2     77 -16 723.33  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_inperson_wocp_step, m1_inperson_wocp, test = "Chisq", vcov = vcov(m1_inperson_wocpCL))
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + subsector + size + mylog(drogas, 
    TRUE) + mylog(poblacion, TRUE) + scale(Derecho, scale = FALSE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df Chisq Pr(>Chisq)  
1     61                      
2     49 12 21.15    0.04823 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# using m1_step terms

m1_inperson_wocp_step2 <- update(m1_step, 
                             . ~ . - extortion_type,
                             data = enve_inperson_wocp)


summary(m1_inperson_wocp_step2)
```

```

Call:
glm(formula = complied_bin_NA ~ n_offenders_NA + mylog(bribes) + 
    yearsquant + mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + 
    mylog(drogas, TRUE), family = binomial(), data = enve_inperson_wocp)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.42334  -0.61685  -0.00015   0.59336   2.58390  

Coefficients:
                           Estimate Std. Error z value Pr(>|z|)   
(Intercept)                 -2.0235     0.8149  -2.483  0.01302 * 
n_offenders_NA2              2.3068     0.8992   2.566  0.01030 * 
n_offenders_NA3            -16.4277  1886.3479  -0.009  0.99305   
n_offenders_NA4+             3.4629     1.1313   3.061  0.00221 **
n_offenders_NADK/DA          1.7043     1.0803   1.578  0.11464   
mylog(bribes)                0.4270     1.0095   0.423  0.67233   
yearsquant(8,16]            -0.1727     1.2492  -0.138  0.89003   
yearsquant(16,25]            0.6144     0.9535   0.644  0.51935   
yearsquant(25,34]           -0.9165     1.0910  -0.840  0.40088   
yearsquant(34,43]            1.3483     1.0053   1.341  0.17985   
mylog(bribes_abvic, TRUE)    0.8167     0.8751   0.933  0.35064   
mylog(armas, TRUE)           0.9389     0.6781   1.385  0.16614   
mylog(drogas, TRUE)         -0.2615     0.4108  -0.637  0.52431   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 104.83  on 77  degrees of freedom
Residual deviance:  61.26  on 65  degrees of freedom
AIC: 87.26

Number of Fisher Scoring iterations: 17
```

```r
logLik(m1_inperson_wocp_step2)
```

```
'log Lik.' -30.63023 (df=13)
```

```r
waldtest(m1_inperson_wocp_step2, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)
1     65                      
2     77 -12 17.606     0.1282
```

```r
waldtest(m1_inperson_wocp_step2, m1_inperson_wocp, test = "Chisq")
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)
1     65                     
2     49 16 9.8396     0.8749
```

```r
car::vif(m1_inperson_wocp_step2)
```

```
                              GVIF Df GVIF^(1/(2*Df))
n_offenders_NA            1.954480  4        1.087374
mylog(bribes)             1.467827  1        1.211539
yearsquant                1.863051  4        1.080882
mylog(bribes_abvic, TRUE) 1.501127  1        1.225205
mylog(armas, TRUE)        2.148492  1        1.465773
mylog(drogas, TRUE)       2.260689  1        1.503559
```

```r
lrtest(m1_inperson_wocp_step2, m1_inperson_wocp)
```

```
Likelihood ratio test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  #Df  LogLik Df  Chisq Pr(>Chisq)  
1  13 -30.630                       
2  29 -18.474 16 24.312    0.08291 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_inperson_wocp)
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                              LR Chisq Df Pr(>Chisq)    
n_offenders_NA                 20.2577  4  0.0004442 ***
had_weapon_NA                   1.6248  2  0.4437910    
mylog(extortions)               0.2922  1  0.5888200    
mylog(bribes)                   0.3948  1  0.5298052    
subsector                      15.9922  6  0.0137958 *  
size                            7.4666  3  0.0584224 .  
yearsquant                      4.1915  4  0.3807072    
mylog(bribes_abvic, TRUE)       0.7290  1  0.3931971    
mylog(armas, TRUE)              0.0181  1  0.8930403    
mylog(drogas, TRUE)             4.1880  1  0.0407106 *  
mylog(poblacion, TRUE)          0.8211  1  0.3648472    
mylog(N, TRUE)                  0.2514  1  0.6160610    
scale(General, scale = FALSE)   1.2555  1  0.2625084    
scale(Derecho, scale = FALSE)   8.3593  1  0.0038373 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
Anova(m1_inperson_wocp_step2)
```

```
Analysis of Deviance Table (Type II tests)

Response: complied_bin_NA
                          LR Chisq Df Pr(>Chisq)    
n_offenders_NA             21.8395  4  0.0002157 ***
mylog(bribes)               0.1863  1  0.6660332    
yearsquant                  4.8043  4  0.3079714    
mylog(bribes_abvic, TRUE)   0.9007  1  0.3425866    
mylog(armas, TRUE)          2.3118  1  0.1283985    
mylog(drogas, TRUE)         0.4130  1  0.5204394    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
accuracy(m1_inperson_wocp)
```

```
$accuracy
[1] 92.308

$naive
[1] 60.256
```

```r
accuracy(m1_inperson_wocp_step2)
```

```
$accuracy
[1] 83.333

$naive
[1] 60.256
```

```r
# Robust SE



m1_inperson_wocp_step2CL <- summaryCL(m1_inperson_wocp_step2, cluster_form)
m1_inperson_wocp_step2CL
```

```
$coefs
                             estimate        SE     z.value      p.value      ci.low     ci.high sig
(Intercept)                -2.0235375 0.5851285  -3.4582789 5.436383e-04  -3.1703682  -0.8767067 ***
n_offenders_NA2             2.3067946 0.9479630   2.4334226 1.495683e-02   0.4488212   4.1647679   *
n_offenders_NA3           -16.4277023 1.0503189 -15.6406798 3.846002e-55 -18.4862896 -14.3691150 ***
n_offenders_NA4+            3.4628744 1.3119230   2.6395409 8.301841e-03   0.8915526   6.0341962  **
n_offenders_NADK/DA         1.7043417 1.0043341   1.6969867 8.969917e-02  -0.2641170   3.6728003    
mylog(bribes)               0.4269556 0.9181566   0.4650139 6.419215e-01  -1.3725983   2.2265095    
yearsquant(8,16]           -0.1727207 0.9338619  -0.1849532 8.532657e-01  -2.0030564   1.6576149    
yearsquant(16,25]           0.6143911 0.9154982   0.6711003 5.021567e-01  -1.1799525   2.4087347    
yearsquant(25,34]          -0.9164871 0.9769402  -0.9381199 3.481828e-01  -2.8312548   0.9982806    
yearsquant(34,43]           1.3483099 0.9189981   1.4671519 1.423347e-01  -0.4528934   3.1495131    
mylog(bribes_abvic, TRUE)   0.8167283 0.6590493   1.2392522 2.152521e-01  -0.4749846   2.1084413    
mylog(armas, TRUE)          0.9389440 0.6777772   1.3853285 1.659521e-01  -0.3894749   2.2673629    
mylog(drogas, TRUE)        -0.2615472 0.2490624  -1.0501271 2.936597e-01  -0.7497005   0.2266062    

$clusterid
~CVE_UNICA + CVE_ENT

$wald
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ 1
  Res.Df  Df  Chisq Pr(>Chisq)    
1     65                          
2     77 -12 1442.9  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
waldtest(m1_inperson_wocp_step2, m1_inperson_wocp, test = "Chisq", vcov = vcov(m1_inperson_wocpCL))
```

```
Wald test

Model 1: complied_bin_NA ~ n_offenders_NA + mylog(bribes) + yearsquant + 
    mylog(bribes_abvic, TRUE) + mylog(armas, TRUE) + mylog(drogas, 
    TRUE)
Model 2: complied_bin_NA ~ n_offenders_NA + had_weapon_NA + mylog(extortions) + 
    mylog(bribes) + subsector + size + yearsquant + mylog(bribes_abvic, 
    TRUE) + mylog(armas, TRUE) + mylog(drogas, TRUE) + mylog(poblacion, 
    TRUE) + mylog(N, TRUE) + scale(General, scale = FALSE) + 
    scale(Derecho, scale = FALSE)
  Res.Df Df  Chisq Pr(>Chisq)    
1     65                         
2     49 16 250.57  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# Benchmark stats


```r
endtime <- proc.time()
time <- endtime - starttime
time
```

```
   user  system elapsed 
 22.497  54.180  13.017 
```

```r
print(paste("the script took", round(time[3]/60,2),
              "minutes to run.", sep=" "))
```

```
[1] "the script took 0.22 minutes to run."
```
