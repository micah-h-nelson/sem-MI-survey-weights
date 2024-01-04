SEM Multiple Imputation and Complex Survey Design Weights
================
Micah Nelson
2024-1-4

This file contains code for estimating a structural equation model that
includes complex survey design weights and multiple imputation of
missing data.

This example code is a partial replication of a paper by Joshua Zingher
(<https://doi.org/10.1016/j.electstud.2020.102119>) which examines the
relationships between white voters’ support for President Trump in 2016,
racial resentment, and authoritarianism.

Here, I use 2020 ANES data and include the full sample to simplify the
use of the weights.

Load in necessary packages and read in the data

``` r
library(haven)
library(lavaan)
library(semTools)
library(Amelia)
library(tidyverse)
library(lavaan.survey)
library(mitools)

data <- read_dta("anes_timeseries_2020_stata_20210719.dta")
```

Prepare the relevant variables

``` r
data <- data |> 
  mutate(age = ifelse(V201507x == -9, NA, V201507x),
         male = case_match(V201600, 
                           2 ~ 0,
                           1 ~ 1),
         democrat = case_when(V201231x %in% 1:3 ~ 1,
                              V201231x %in% 4:7 ~ 0),
         ideology = ifelse(V201200 == -9 | V201200 == -8 | V201200 == 99, NA, V201200),
         south = ifelse(V203003 == 3, 1, 0),
         trump.vote = case_match(V202073,
                                 1 ~ 0,
                                 2 ~ 1,
                                 3 ~ 0,
                                 4 ~ 0,
                                 5 ~ 0),
         auth1 = case_match(V202266,
                            1 ~ 0,
                            2 ~ 1),
         auth2 = case_match(V202267,
                            1 ~ 0,
                            2 ~ 1),
         auth3 = case_match(V202268,
                            1 ~ 0,
                            2 ~ 1),
         auth4 = case_match(V202269,
                            1 ~ 0,
                            2 ~ 1),
         resent1 = case_match(V202300,
                              1 ~ 5,
                              2 ~ 4,
                              3 ~ 3,
                              4 ~ 2,
                              5 ~ 1),
         resent2 = case_match(V202301, 
                              1 ~ 5,
                              2 ~ 4,
                              3 ~ 3,
                              4 ~ 2,
                              5 ~ 1),
         resent3 = case_match(V202302, 
                              5 ~ 5,
                              4 ~ 4,
                              3 ~ 3,
                              2 ~ 2,
                              1 ~ 1),
         resent4 = case_match(V202303, 
                              1 ~ 5,
                              2 ~ 4,
                              3 ~ 3,
                              4 ~ 2,
                              5 ~ 1),
         college = case_when(V201510 %in% 1:5 ~ 0,
                             V201510 %in% 6:8 ~ 1),
         weight = ifelse(is.na(V200010b), 0, V200010b),
         psu = V200010c,
         strata = V200010d
         ) |> 
  filter(
    V200004 == 3
  ) |> 
  select(age, male, democrat, ideology, south, trump.vote, 
         auth1, auth2, auth3, auth4, resent1, resent2,
         resent3, resent4, college, weight, psu, strata 
         )
```

Define the model structure

``` r
model <- '
#Define racial resentment and authoritarianism variables
resent =~ resent1 + resent2 + resent3 + resent4

authoritarian =~ auth1 + auth2 + auth3 + auth4

#Regressions
resent ~ college + age + male + democrat + ideology + south

authoritarian ~ college + age + male + democrat + ideology + south

trump.vote ~ college + resent + authoritarian + college + age + male + democrat + ideology + south

#Correlate the residuals of the latent variables
resent ~~ authoritarian
'
```

Estimate the model without imputation or weights to make sure it works

``` r
fit <- sem(model = model, data = data, ordered = c("auth1", "auth2", "auth3", "auth4", "trump.vote"))

summary(fit, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 56 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        42
    ## 
    ##                                                   Used       Total
    ##   Number of observations                          4847        7453
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                               504.774     717.681
    ##   Degrees of freedom                                61          61
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.712
    ##   Shift parameter                                            8.975
    ##     simple second-order correction                                
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              9094.614    5757.825
    ##   Degrees of freedom                                36          36
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.583
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.951       0.885
    ##   Tucker-Lewis Index (TLI)                       0.971       0.932
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.039       0.047
    ##   90 Percent confidence interval - lower         0.036       0.044
    ##   90 Percent confidence interval - upper         0.042       0.050
    ##   P-value H_0: RMSEA <= 0.050                    1.000       0.934
    ##   P-value H_0: RMSEA >= 0.080                    0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.046       0.046
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Delta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   resent =~                                           
    ##     resent1           1.000                           
    ##     resent2          -0.950    0.026  -36.900    0.000
    ##     resent3           0.893    0.023   38.969    0.000
    ##     resent4           0.893    0.022   41.190    0.000
    ##   authoritarian =~                                    
    ##     auth1             1.000                           
    ##     auth2             1.088    0.040   27.236    0.000
    ##     auth3            -0.896    0.035  -25.719    0.000
    ##     auth4             0.775    0.035   22.227    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   resent ~                                            
    ##     college          -0.326    0.028  -11.719    0.000
    ##     age               0.005    0.001    5.942    0.000
    ##     male             -0.013    0.027   -0.493    0.622
    ##     democrat         -0.887    0.040  -22.295    0.000
    ##     ideology          0.313    0.013   23.855    0.000
    ##     south             0.045    0.028    1.603    0.109
    ##   authoritarian ~                                     
    ##     college          -0.472    0.030  -15.543    0.000
    ##     age               0.003    0.001    3.222    0.001
    ##     male             -0.028    0.028   -1.012    0.312
    ##     democrat         -0.024    0.041   -0.598    0.550
    ##     ideology          0.231    0.014   16.933    0.000
    ##     south             0.185    0.030    6.235    0.000
    ##   trump.vote ~                                        
    ##     college           0.001    0.058    0.024    0.981
    ##     resent            0.622    0.032   19.297    0.000
    ##     authoritarian     0.185    0.047    3.905    0.000
    ##     age              -0.001    0.002   -0.470    0.639
    ##     male             -0.209    0.054   -3.843    0.000
    ##     democrat         -1.385    0.072  -19.238    0.000
    ##     ideology          0.308    0.026   11.980    0.000
    ##     south            -0.039    0.058   -0.676    0.499
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .resent ~~                                           
    ##    .authoritarian     0.169    0.014   12.404    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .resent1           1.874    0.092   20.460    0.000
    ##    .resent2           4.044    0.093   43.574    0.000
    ##    .resent3           1.946    0.086   22.674    0.000
    ##    .resent4           1.765    0.086   20.548    0.000
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     auth1|t1          0.967    0.118    8.185    0.000
    ##     auth2|t1          0.592    0.110    5.392    0.000
    ##     auth3|t1         -1.276    0.110  -11.565    0.000
    ##     auth4|t1          1.116    0.115    9.715    0.000
    ##     trump.vote|t1     1.805    0.151   11.967    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .resent1           0.628    0.016   39.188    0.000
    ##    .resent2           0.722    0.017   42.230    0.000
    ##    .resent3           0.612    0.015   41.559    0.000
    ##    .resent4           0.631    0.015   42.969    0.000
    ##    .auth1             0.470                           
    ##    .auth2             0.372                           
    ##    .auth3             0.575                           
    ##    .auth4             0.681                           
    ##    .trump.vote        0.680                           
    ##    .resent            0.681    0.031   21.870    0.000
    ##    .authoritarian     0.530    0.028   18.763    0.000

Impute data with amelia and store imputations For the sake of keeping
this example simple, I do not include auxiliary variables for the
imputation

``` r
data.amelia <- amelia(x = data, m = 20, ords = c("male", "democrat", "ideology", "south", "trump.vote",
                                             "auth1", "auth2", "auth3", "auth4", "resent1", "resent2",
                                             "resent3", "resent4", "college"))

imputations <- data.amelia$imputations

dataimplist <- imputationList(data.amelia$imputations)
```

Create survey design object and estimate original model with survey
weights and imputed data

``` r
design.mi <- svydesign(ids = ~psu, weights = ~weight, strata = ~strata, nest = TRUE, data = dataimplist)

fit.survey.mi <- lavaan.survey(lavaan.fit = fit, survey.design = design.mi)

summary(fit.survey.mi, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 79 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        47
    ## 
    ##   Number of observations                          7453
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                              1961.113     809.371
    ##   Degrees of freedom                                61          61
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  2.423
    ##     Satorra-Bentler correction                                    
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             32412.194   16622.037
    ##   Degrees of freedom                                90          90
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.950
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.941       0.955
    ##   Tucker-Lewis Index (TLI)                       0.913       0.933
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.944
    ##   Robust Tucker-Lewis Index (TLI)                            0.917
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -62803.138  -62803.138
    ##   Loglikelihood unrestricted model (H1)     -61822.581  -61822.581
    ##                                                                   
    ##   Akaike (AIC)                              125700.275  125700.275
    ##   Bayesian (BIC)                            126025.345  126025.345
    ##   Sample-size adjusted Bayesian (SABIC)     125875.989  125875.989
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.065       0.041
    ##   90 Percent confidence interval - lower         0.062       0.039
    ##   90 Percent confidence interval - upper         0.067       0.042
    ##   P-value H_0: RMSEA <= 0.050                    0.000       1.000
    ##   P-value H_0: RMSEA >= 0.080                    0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                               0.063
    ##   90 Percent confidence interval - lower                     0.059
    ##   90 Percent confidence interval - upper                     0.067
    ##   P-value H_0: Robust RMSEA <= 0.050                         0.000
    ##   P-value H_0: Robust RMSEA >= 0.080                         0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.031       0.031
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   resent =~                                           
    ##     resent1           1.000                           
    ##     resent2          -1.016    0.019  -52.739    0.000
    ##     resent3           1.011    0.017   59.803    0.000
    ##     resent4           0.897    0.013   69.145    0.000
    ##   authoritarian =~                                    
    ##     auth1             1.000                           
    ##     auth2             1.166    0.037   31.404    0.000
    ##     auth3            -1.044    0.035  -30.065    0.000
    ##     auth4             0.645    0.035   18.208    0.000
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   resent ~                                            
    ##     college          -0.336    0.039   -8.513    0.000
    ##     age               0.007    0.001    7.140    0.000
    ##     male              0.014    0.038    0.372    0.710
    ##     democrat         -0.808    0.049  -16.602    0.000
    ##     ideology          0.251    0.015   16.644    0.000
    ##     south            -0.004    0.045   -0.089    0.929
    ##   authoritarian ~                                     
    ##     college          -0.186    0.011  -16.554    0.000
    ##     age               0.002    0.000    4.685    0.000
    ##     male             -0.020    0.011   -1.855    0.064
    ##     democrat         -0.012    0.015   -0.819    0.413
    ##     ideology          0.055    0.005   11.469    0.000
    ##     south             0.074    0.011    6.707    0.000
    ##   trump.vote ~                                        
    ##     college           0.008    0.015    0.539    0.590
    ##     resent            0.125    0.010   12.889    0.000
    ##     authoritarian     0.014    0.035    0.403    0.687
    ##     age               0.000    0.000    0.031    0.975
    ##     male             -0.029    0.013   -2.218    0.027
    ##     democrat         -0.417    0.023  -18.160    0.000
    ##     ideology          0.046    0.007    6.732    0.000
    ##     south             0.002    0.014    0.120    0.905
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .resent ~~                                           
    ##    .authoritarian     0.050    0.005    9.791    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .resent1           2.228    0.097   22.906    0.000
    ##    .resent2           4.114    0.098   41.811    0.000
    ##    .resent3           2.038    0.099   20.558    0.000
    ##    .resent4           1.860    0.089   20.972    0.000
    ##    .auth1             0.435    0.030   14.288    0.000
    ##    .auth2             0.309    0.032    9.665    0.000
    ##    .auth3             0.831    0.030   27.350    0.000
    ##    .auth4             0.117    0.022    5.420    0.000
    ##    .trump.vote        0.346    0.039    8.907    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .resent1           0.819    0.031   26.100    0.000
    ##    .resent2           0.752    0.030   24.726    0.000
    ##    .resent3           0.591    0.027   22.159    0.000
    ##    .resent4           0.794    0.023   34.885    0.000
    ##    .auth1             0.133    0.004   29.693    0.000
    ##    .auth2             0.130    0.004   32.096    0.000
    ##    .auth3             0.160    0.004   39.638    0.000
    ##    .auth4             0.170    0.003   50.652    0.000
    ##    .trump.vote        0.108    0.003   31.270    0.000
    ##    .resent            0.669    0.027   24.632    0.000
    ##    .authoritarian     0.056    0.003   19.832    0.000
