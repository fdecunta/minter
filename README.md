
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minter <img src="man/figures/minter_logo.png" align="right" alt="" width="150" />

<!-- badges: start -->
<!-- badges: end -->

minter provdies helper functions to conduct **m**eta-analysis of
**inter**actions using data from 2x2 factorial experiments.

## Installation

You can install the development version of minter from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("fdecunta/minter", force = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(minter)

data(fake_data)

# Compute the effect sizes
fake_data <- inter_effsize(
  effsize = "lnrr",
  colnames = c("Herb", "Fert"),
  data = fake_data,
  Ctrl_mean = C_mean,
  Ctrl_sd = C_sd,
  Ctrl_n = C_n,
  A_mean = Herb_mean,
  A_sd = Herb_sd,
  A_n = Herb_n,
  B_mean = Fert_mean,
  B_sd = Fert_sd,
  B_n = Fert_n,
  AB_mean = HxF_mean,
  AB_sd = HxF_sd,
  AB_n = HxF_n
)

summary(fake_data)
#>     Study               C_mean            C_sd            C_n       
#>  Length:9           Min.   : 8.198   Min.   :1.265   Min.   :20.00  
#>  Class :character   1st Qu.: 8.744   1st Qu.:1.321   1st Qu.:23.00  
#>  Mode  :character   Median : 9.022   Median :1.565   Median :25.00  
#>                     Mean   : 9.518   Mean   :1.592   Mean   :24.67  
#>                     3rd Qu.:10.413   3rd Qu.:1.902   3rd Qu.:27.00  
#>                     Max.   :11.327   Max.   :1.986   Max.   :28.00  
#>    Herb_mean        Herb_sd          Herb_n        Fert_mean     
#>  Min.   :10.11   Min.   :1.058   Min.   :20.00   Min.   : 9.076  
#>  1st Qu.:10.88   1st Qu.:1.428   1st Qu.:24.00   1st Qu.:10.155  
#>  Median :12.90   Median :1.542   Median :27.00   Median :11.082  
#>  Mean   :12.49   Mean   :1.522   Mean   :25.78   Mean   :11.032  
#>  3rd Qu.:13.05   3rd Qu.:1.618   3rd Qu.:27.00   3rd Qu.:11.471  
#>  Max.   :15.53   Max.   :1.916   Max.   :29.00   Max.   :13.889  
#>     Fert_sd          Fert_n         HxF_mean         HxF_sd     
#>  Min.   :1.006   Min.   :23.00   Min.   :10.80   Min.   :1.008  
#>  1st Qu.:1.072   1st Qu.:26.00   1st Qu.:11.08   1st Qu.:1.390  
#>  Median :1.503   Median :28.00   Median :12.94   Median :1.465  
#>  Mean   :1.457   Mean   :27.67   Mean   :13.54   Mean   :1.421  
#>  3rd Qu.:1.770   3rd Qu.:29.00   3rd Qu.:15.54   3rd Qu.:1.492  
#>  Max.   :1.972   Max.   :30.00   Max.   :18.20   Max.   :1.713  
#>      HxF_n       EffectSize_ID Herb_simple_lnRR Herb_simple_lnRR_var
#>  Min.   :20.00   Min.   :1     Min.   :0.1283   Min.   :0.0009205   
#>  1st Qu.:22.00   1st Qu.:3     1st Qu.:0.1446   1st Qu.:0.0016026   
#>  Median :23.00   Median :5     Median :0.2515   Median :0.0019407   
#>  Mean   :23.78   Mean   :5     Mean   :0.2683   Mean   :0.0018350   
#>  3rd Qu.:25.00   3rd Qu.:7     3rd Qu.:0.3512   3rd Qu.:0.0021480   
#>  Max.   :28.00   Max.   :9     Max.   :0.5429   Max.   :0.0025133   
#>  Fert_simple_lnRR  Fert_simple_lnRR_var Herb_overall_lnRR Herb_overall_lnRR_var
#>  Min.   :-0.2216   Min.   :0.0009554    Min.   :0.0334    Min.   :0.0004653    
#>  1st Qu.: 0.1338   1st Qu.:0.0013764    1st Qu.:0.1615    1st Qu.:0.0006822    
#>  Median : 0.1496   Median :0.0017009    Median :0.1987    Median :0.0007442    
#>  Mean   : 0.1467   Mean   :0.0018761    Mean   :0.2313    Mean   :0.0007373    
#>  3rd Qu.: 0.2710   3rd Qu.:0.0025882    3rd Qu.:0.2813    3rd Qu.:0.0008385    
#>  Max.   : 0.3137   Max.   :0.0027368    Max.   :0.4363    Max.   :0.0009038    
#>  Fert_overall_lnRR  Fert_overall_lnRR_var Herb_x_Fert_lnRR  
#>  Min.   :-0.18431   Min.   :0.0005495     Min.   :-0.39215  
#>  1st Qu.: 0.06604   1st Qu.:0.0006826     1st Qu.:-0.20258  
#>  Median : 0.10348   Median :0.0007098     Median :-0.11527  
#>  Mean   : 0.10676   Mean   :0.0007046     Mean   :-0.07350  
#>  3rd Qu.: 0.16821   3rd Qu.:0.0007439     3rd Qu.: 0.06885  
#>  Max.   : 0.39171   Max.   :0.0009038     Max.   : 0.14200  
#>  Herb_x_Fert_lnRR_var
#>  Min.   :0.002219    
#>  1st Qu.:0.002803    
#>  Median :0.003077    
#>  Mean   :0.003016    
#>  3rd Qu.:0.003455    
#>  Max.   :0.003838
```
