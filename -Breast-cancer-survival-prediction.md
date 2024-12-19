Breast cancer survival prediction
================
Tong Su
2024-12-19

``` r
# Load dataset
data <- read.csv("./data/Project_2_data.csv")
```

# 1. Descriptive Summary

``` r
summary_stats <- data |> 
  summarise_all(list(mean = ~mean(as.numeric(.), na.rm = TRUE), 
                     median = ~median(as.numeric(.), na.rm = TRUE), 
                     count = ~sum(!is.na(.))))
```

    ## Warning: There were 22 warnings in `summarise()`.
    ## The first warning was:
    ## ℹ In argument: `Race_mean = (structure(function (..., .x = ..1, .y = ..2, . =
    ##   ..1) ...`.
    ## Caused by warning in `mean()`:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 21 remaining warnings.

``` r
summary_stats
```

    ##   Age_mean Race_mean Marital.Status_mean T.Stage_mean N.Stage_mean
    ## 1 53.97217       NaN                 NaN          NaN          NaN
    ##   X6th.Stage_mean differentiate_mean Grade_mean A.Stage_mean Tumor.Size_mean
    ## 1             NaN                NaN   2.141823          NaN        30.47366
    ##   Estrogen.Status_mean Progesterone.Status_mean Regional.Node.Examined_mean
    ## 1                  NaN                      NaN                    14.35711
    ##   Reginol.Node.Positive_mean Survival.Months_mean Status_mean Age_median
    ## 1                   4.158052             71.29796         NaN         54
    ##   Race_median Marital.Status_median T.Stage_median N.Stage_median
    ## 1          NA                    NA             NA             NA
    ##   X6th.Stage_median differentiate_median Grade_median A.Stage_median
    ## 1                NA                   NA            2             NA
    ##   Tumor.Size_median Estrogen.Status_median Progesterone.Status_median
    ## 1                25                     NA                         NA
    ##   Regional.Node.Examined_median Reginol.Node.Positive_median
    ## 1                            14                            2
    ##   Survival.Months_median Status_median Age_count Race_count
    ## 1                     73            NA      4024       4024
    ##   Marital.Status_count T.Stage_count N.Stage_count X6th.Stage_count
    ## 1                 4024          4024          4024             4024
    ##   differentiate_count Grade_count A.Stage_count Tumor.Size_count
    ## 1                4024        4024          4024             4024
    ##   Estrogen.Status_count Progesterone.Status_count Regional.Node.Examined_count
    ## 1                  4024                      4024                         4024
    ##   Reginol.Node.Positive_count Survival.Months_count Status_count
    ## 1                        4024                  4024         4024

# 2. Data Visualization

``` r
# Distribution of continuous variables
numeric_vars <- data |> select_if(is.numeric)

# Histogram for continuous variables
for (col in names(numeric_vars)) {
  ggplot(data, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    ggtitle(paste("Distribution of", col)) +
    theme_minimal()
}
```

    ## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ## ℹ Please use tidy evaluation idioms with `aes()`.
    ## ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
# Correlation heatmap
cor_matrix <- cor(numeric_vars, use = "complete.obs")
ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2() + 
  ggtitle("Correlation Heatmap") + 
  theme_minimal()
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# 3. Model Building: Survival Analysis

``` r
# Convert Status to binary (Dead = 1, Alive = 0)
data$Status <- ifelse(data$Status == "Dead", 1, 0)

# Build Cox Proportional Hazards model
cox_model <- coxph(Surv(Survival.Months, Status) ~ ., data = data)
summary(cox_model)
```

    ## Call:
    ## coxph(formula = Surv(Survival.Months, Status) ~ ., data = data)
    ## 
    ##   n= 4024, number of events= 616 
    ## 
    ##                                         coef exp(coef)  se(coef)      z
    ## Age                                 0.020463  1.020673  0.004873  4.200
    ## RaceOther                          -0.736866  0.478612  0.214042 -3.443
    ## RaceWhite                          -0.370655  0.690282  0.129776 -2.856
    ## Marital.StatusMarried              -0.203376  0.815971  0.119228 -1.706
    ## Marital.StatusSeparated             0.391784  1.479619  0.290182  1.350
    ## Marital.StatusSingle               -0.030024  0.970422  0.146084 -0.206
    ## Marital.StatusWidowed              -0.030182  0.970269  0.181574 -0.166
    ## T.StageT2                           0.201182  1.222847  0.160641  1.252
    ## T.StageT3                           0.354265  1.425133  0.253364  1.398
    ## T.StageT4                           0.637921  1.892542  0.313356  2.036
    ## N.StageN2                           0.568511  1.765636  0.203140  2.799
    ## N.StageN3                           0.756433  2.130663  0.240152  3.150
    ## X6th.StageIIB                       0.269771  1.309664  0.201799  1.337
    ## X6th.StageIIIA                      0.027973  1.028368  0.254824  0.110
    ## X6th.StageIIIB                      0.202537  1.224505  0.395992  0.511
    ## X6th.StageIIIC                            NA        NA  0.000000     NA
    ## differentiatePoorly differentiated  0.351803  1.421629  0.089308  3.939
    ## differentiateUndifferentiated       1.104912  3.018960  0.349828  3.158
    ## differentiateWell differentiated   -0.449352  0.638042  0.171180 -2.625
    ## Grade1                                    NA        NA  0.000000     NA
    ## Grade2                                    NA        NA  0.000000     NA
    ## Grade3                                    NA        NA  0.000000     NA
    ## A.StageRegional                    -0.150448  0.860322  0.194018 -0.775
    ## Tumor.Size                          0.001680  1.001681  0.003161  0.531
    ## Estrogen.StatusPositive            -0.648532  0.522813  0.135622 -4.782
    ## Progesterone.StatusPositive        -0.486487  0.614782  0.106949 -4.549
    ## Regional.Node.Examined             -0.033013  0.967526  0.006492 -5.085
    ## Reginol.Node.Positive               0.058490  1.060234  0.011353  5.152
    ##                                    Pr(>|z|)    
    ## Age                                2.67e-05 ***
    ## RaceOther                          0.000576 ***
    ## RaceWhite                          0.004289 ** 
    ## Marital.StatusMarried              0.088051 .  
    ## Marital.StatusSeparated            0.176973    
    ## Marital.StatusSingle               0.837162    
    ## Marital.StatusWidowed              0.867980    
    ## T.StageT2                          0.210435    
    ## T.StageT3                          0.162038    
    ## T.StageT4                          0.041773 *  
    ## N.StageN2                          0.005132 ** 
    ## N.StageN3                          0.001634 ** 
    ## X6th.StageIIB                      0.181279    
    ## X6th.StageIIIA                     0.912589    
    ## X6th.StageIIIB                     0.609025    
    ## X6th.StageIIIC                           NA    
    ## differentiatePoorly differentiated 8.18e-05 ***
    ## differentiateUndifferentiated      0.001586 ** 
    ## differentiateWell differentiated   0.008664 ** 
    ## Grade1                                   NA    
    ## Grade2                                   NA    
    ## Grade3                                   NA    
    ## A.StageRegional                    0.438084    
    ## Tumor.Size                         0.595099    
    ## Estrogen.StatusPositive            1.74e-06 ***
    ## Progesterone.StatusPositive        5.40e-06 ***
    ## Regional.Node.Examined             3.67e-07 ***
    ## Reginol.Node.Positive              2.58e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                                    exp(coef) exp(-coef) lower .95 upper .95
    ## Age                                   1.0207     0.9797    1.0110    1.0305
    ## RaceOther                             0.4786     2.0894    0.3146    0.7281
    ## RaceWhite                             0.6903     1.4487    0.5353    0.8902
    ## Marital.StatusMarried                 0.8160     1.2255    0.6459    1.0308
    ## Marital.StatusSeparated               1.4796     0.6758    0.8378    2.6131
    ## Marital.StatusSingle                  0.9704     1.0305    0.7288    1.2921
    ## Marital.StatusWidowed                 0.9703     1.0306    0.6797    1.3850
    ## T.StageT2                             1.2228     0.8178    0.8926    1.6754
    ## T.StageT3                             1.4251     0.7017    0.8673    2.3416
    ## T.StageT4                             1.8925     0.5284    1.0240    3.4976
    ## N.StageN2                             1.7656     0.5664    1.1857    2.6291
    ## N.StageN3                             2.1307     0.4693    1.3308    3.4114
    ## X6th.StageIIB                         1.3097     0.7636    0.8818    1.9451
    ## X6th.StageIIIA                        1.0284     0.9724    0.6241    1.6946
    ## X6th.StageIIIB                        1.2245     0.8167    0.5635    2.6609
    ## X6th.StageIIIC                            NA         NA        NA        NA
    ## differentiatePoorly differentiated    1.4216     0.7034    1.1933    1.6936
    ## differentiateUndifferentiated         3.0190     0.3312    1.5208    5.9928
    ## differentiateWell differentiated      0.6380     1.5673    0.4562    0.8924
    ## Grade1                                    NA         NA        NA        NA
    ## Grade2                                    NA         NA        NA        NA
    ## Grade3                                    NA         NA        NA        NA
    ## A.StageRegional                       0.8603     1.1624    0.5882    1.2584
    ## Tumor.Size                            1.0017     0.9983    0.9955    1.0079
    ## Estrogen.StatusPositive               0.5228     1.9127    0.4008    0.6820
    ## Progesterone.StatusPositive           0.6148     1.6266    0.4985    0.7582
    ## Regional.Node.Examined                0.9675     1.0336    0.9553    0.9799
    ## Reginol.Node.Positive                 1.0602     0.9432    1.0369    1.0841
    ## 
    ## Concordance= 0.744  (se = 0.011 )
    ## Likelihood ratio test= 501.6  on 24 df,   p=<2e-16
    ## Wald test            = 570.2  on 24 df,   p=<2e-16
    ## Score (logrank) test = 665  on 24 df,   p=<2e-16

``` r
# Save model summary
tidy_cox <- broom::tidy(cox_model)
write.csv(tidy_cox, "cox_model_summary.csv")

# Evaluate Model Performance
# Concordance index
c_index <- summary(cox_model)$concordance
cat("Concordance Index:", c_index, "\n")
```

    ## Concordance Index: 0.7437448 0.01069854

# 4. Fairness Analysis
