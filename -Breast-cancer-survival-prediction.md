Breast cancer survival prediction
================
Tong Su, Minghe Wang, Kai Tan, Yifei Chen
2024-12-19

``` r
# Load dataset
data <- read.csv("./data/Project_2_data.csv")

# Clean dataset
data_cleaned <- data %>%
  mutate(
    Race = factor(Race),
    Marital.Status = factor(trimws(Marital.Status)),
    T.Stage = factor(T.Stage),
    N.Stage = factor(N.Stage),
    X6th.Stage = factor(X6th.Stage),
    differentiate = factor(differentiate),
    Grade = factor(trimws(Grade)),  # Trim spaces before factoring
    A.Stage = factor(A.Stage),
    Estrogen.Status = factor(Estrogen.Status),
    Progesterone.Status = factor(Progesterone.Status),
    Status = ifelse(Status == "Dead", 1, 0)
  ) %>% 
  mutate(
    T.Stage = as.numeric(factor(T.Stage, levels = c("T1", "T2", "T3", "T4"), ordered = TRUE)),
    N.Stage = as.numeric(factor(N.Stage, levels = c("N1", "N2", "N3"), ordered = TRUE)),
    X6th.Stage = as.numeric(factor(X6th.Stage, levels = c("IIA", "IIB", "IIIA", "IIIB", "IIIC"), ordered = TRUE)),
    Grade = as.numeric(factor(Grade, levels = c("1", "2", "3", "anaplastic; Grade IV"), ordered = TRUE)),
    A.Stage = as.numeric(factor(A.Stage, levels = c("Regional", "Distant"), ordered = TRUE))
  ) %>% 
  select(-differentiate)
```

`Grade` and `differentiate` are redundant variables, so we drop
`differentiate`.

# 1. Descriptive Summary

``` r
# summary_stats for numeric predictors
summary_numeric <- data_cleaned |> 
  select_if(is.numeric) |>
  summarise_all(list(
    mean = ~mean(.x, na.rm = TRUE), 
    median = ~median(.x, na.rm = TRUE), 
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    iqr = ~IQR(.x, na.rm = TRUE),
    count = ~sum(!is.na(.x))
  ))

summary_numeric_long <- summary_numeric |> 
  pivot_longer(
    cols = everything(),        
    names_to = c("Variable", "Statistic"), 
    names_sep = "_",            
    values_to = "Value"         
  )
summary_numeric_wide <- summary_numeric_long |> 
  pivot_wider(
    names_from = Statistic,    
    values_from = Value         
  )
summary_numeric_wide
```

    ## # A tibble: 11 × 8
    ##    Variable                 mean median     sd   min   max   iqr count
    ##    <chr>                   <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 Age                    54.0       54  8.96     30    69    14  4024
    ##  2 T.Stage                 1.78       2  0.766     1     4     1  4024
    ##  3 N.Stage                 1.44       1  0.693     1     3     1  4024
    ##  4 X6th.Stage              2.32       2  1.27      1     5     2  4024
    ##  5 Grade                   2.15       2  0.638     1     4     1  4024
    ##  6 A.Stage                 1.02       1  0.149     1     2     0  4024
    ##  7 Tumor.Size             30.5       25 21.1       1   140    22  4024
    ##  8 Regional.Node.Examined 14.4       14  8.10      1    61    10  4024
    ##  9 Reginol.Node.Positive   4.16       2  5.11      1    46     4  4024
    ## 10 Survival.Months        71.3       73 22.9       1   107    34  4024
    ## 11 Status                  0.153      0  0.360     0     1     0  4024

``` r
# Missing data
missing_percentage <- data_cleaned |> 
  summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "missing_pct_{.col}"))
missing_percentage
```

    ##   missing_pct_Age missing_pct_Race missing_pct_Marital.Status
    ## 1               0                0                          0
    ##   missing_pct_T.Stage missing_pct_N.Stage missing_pct_X6th.Stage
    ## 1                   0                   0                      0
    ##   missing_pct_Grade missing_pct_A.Stage missing_pct_Tumor.Size
    ## 1                 0                   0                      0
    ##   missing_pct_Estrogen.Status missing_pct_Progesterone.Status
    ## 1                           0                               0
    ##   missing_pct_Regional.Node.Examined missing_pct_Reginol.Node.Positive
    ## 1                                  0                                 0
    ##   missing_pct_Survival.Months missing_pct_Status
    ## 1                           0                  0

# 2. Data Visualization

``` r
# Distribution of continuous variables
numeric_vars <- data_cleaned |> select_if(is.numeric)

# Histogram for continuous variables
for (col in names(numeric_vars)) {
  ggplot(data_cleaned, aes_string(x = col)) +
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
# Scatter plot for continuous variables
predictors = c("Age", "Tumor.Size", "Regional.Node.Examined", "Reginol.Node.Positive")
sorted_data = melt(data_cleaned, id.vars = "Survival.Months", measure.vars = predictors)
ggplot(sorted_data, aes(x = value, y = Survival.Months)) +
  geom_point(alpha = 1) +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~variable, scales = "free_x") +  
  labs(title = "Scatter Plots of Predictors vs Survival Time",
       x = "Predictor Value",
       y = "Survival Time (Months)") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Correlation heatmap with correlation coefficient
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "number", type = "full", tl.col = "black", tl.srt = 45, title = "Correlation Heatmap")
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# Bar Plot for categorical variables
categorical_vars <- names(data_cleaned)[sapply(data_cleaned, is.factor) | sapply(data_cleaned, is.character)]
if (length(categorical_vars) > 0) {
  for (var in categorical_vars) {
    p <- ggplot(data_cleaned, aes_string(x = var, fill = var)) +
      geom_bar() +
      labs(
        title = paste("Distribution of", var),
        x = var,
        y = "Count"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")  
    
    print(p)  
  }
} else {
  message("No categorical variables found in the dataset.")
}
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
#show pairs with strong correlation
for (i in 1:(nrow(cor_matrix) - 1)) {
  for (j in (i + 1):ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) > 0.7) {
      cat("Pairs with strong correlation:", rownames(cor_matrix)[i], "~", colnames(cor_matrix)[j], 
          "with correlation =", cor_matrix[i, j], "\n")
    }
  }
}
```

    ## Pairs with strong correlation: T.Stage ~ Tumor.Size with correlation = 0.8091755 
    ## Pairs with strong correlation: N.Stage ~ X6th.Stage with correlation = 0.8818801 
    ## Pairs with strong correlation: N.Stage ~ Reginol.Node.Positive with correlation = 0.8380733 
    ## Pairs with strong correlation: X6th.Stage ~ Reginol.Node.Positive with correlation = 0.7739631

``` r
#show pairs with weak correlation
for (i in 1:(nrow(cor_matrix) - 1)) {
  for (j in (i + 1):ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) > 0.2 & abs(cor_matrix[i, j]) < 0.4) {
      cat("Pairs with weak correlation:", rownames(cor_matrix)[i], "~", colnames(cor_matrix)[j], 
          "with correlation =", cor_matrix[i, j], "\n")
    }
  }
}
```

    ## Pairs with weak correlation: T.Stage ~ N.Stage with correlation = 0.2770145 
    ## Pairs with weak correlation: T.Stage ~ A.Stage with correlation = 0.2211235 
    ## Pairs with weak correlation: T.Stage ~ Reginol.Node.Positive with correlation = 0.2430749 
    ## Pairs with weak correlation: N.Stage ~ A.Stage with correlation = 0.2605729 
    ## Pairs with weak correlation: N.Stage ~ Tumor.Size with correlation = 0.2779047 
    ## Pairs with weak correlation: N.Stage ~ Regional.Node.Examined with correlation = 0.3282761 
    ## Pairs with weak correlation: N.Stage ~ Status with correlation = 0.2557719 
    ## Pairs with weak correlation: X6th.Stage ~ A.Stage with correlation = 0.2919618 
    ## Pairs with weak correlation: X6th.Stage ~ Regional.Node.Examined with correlation = 0.3172187 
    ## Pairs with weak correlation: X6th.Stage ~ Status with correlation = 0.2576359 
    ## Pairs with weak correlation: A.Stage ~ Reginol.Node.Positive with correlation = 0.2328489 
    ## Pairs with weak correlation: Tumor.Size ~ Reginol.Node.Positive with correlation = 0.2423217 
    ## Pairs with weak correlation: Reginol.Node.Positive ~ Status with correlation = 0.2566381

# 3. Model Building: Survival Analysis

``` r
# Build Cox Proportional Hazards model
cox_model <- coxph(Surv(Survival.Months, Status) ~ ., data = data_cleaned)
summary(cox_model)
```

    ## Call:
    ## coxph(formula = Surv(Survival.Months, Status) ~ ., data = data_cleaned)
    ## 
    ##   n= 4024, number of events= 616 
    ## 
    ##                                   coef  exp(coef)   se(coef)      z Pr(>|z|)
    ## Age                          0.0203770  1.0205860  0.0048613  4.192 2.77e-05
    ## RaceOther                   -0.7368944  0.4785979  0.2134615 -3.452 0.000556
    ## RaceWhite                   -0.3744974  0.6876348  0.1293944 -2.894 0.003801
    ## Marital.StatusMarried       -0.1991172  0.8194539  0.1190045 -1.673 0.094290
    ## Marital.StatusSeparated      0.4384181  1.5502529  0.2878537  1.523 0.127744
    ## Marital.StatusSingle        -0.0111130  0.9889485  0.1456421 -0.076 0.939177
    ## Marital.StatusWidowed       -0.0380960  0.9626205  0.1810814 -0.210 0.833371
    ## T.Stage                      0.2826774  1.3266771  0.0927377  3.048 0.002303
    ## N.Stage                      0.3475721  1.4156264  0.1554043  2.237 0.025315
    ## X6th.Stage                  -0.0038128  0.9961945  0.1001211 -0.038 0.969622
    ## Grade                        0.3943812  1.4834659  0.0684734  5.760 8.43e-09
    ## A.Stage                      0.1219854  1.1297376  0.1861550  0.655 0.512282
    ## Tumor.Size                  -0.0002408  0.9997592  0.0026804 -0.090 0.928422
    ## Estrogen.StatusPositive     -0.6544763  0.5197142  0.1348804 -4.852 1.22e-06
    ## Progesterone.StatusPositive -0.4701143  0.6249308  0.1064462 -4.416 1.00e-05
    ## Regional.Node.Examined      -0.0323493  0.9681683  0.0064298 -5.031 4.88e-07
    ## Reginol.Node.Positive        0.0535131  1.0549708  0.0109855  4.871 1.11e-06
    ##                                
    ## Age                         ***
    ## RaceOther                   ***
    ## RaceWhite                   ** 
    ## Marital.StatusMarried       .  
    ## Marital.StatusSeparated        
    ## Marital.StatusSingle           
    ## Marital.StatusWidowed          
    ## T.Stage                     ** 
    ## N.Stage                     *  
    ## X6th.Stage                     
    ## Grade                       ***
    ## A.Stage                        
    ## Tumor.Size                     
    ## Estrogen.StatusPositive     ***
    ## Progesterone.StatusPositive ***
    ## Regional.Node.Examined      ***
    ## Reginol.Node.Positive       ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                             exp(coef) exp(-coef) lower .95 upper .95
    ## Age                            1.0206     0.9798    1.0109    1.0304
    ## RaceOther                      0.4786     2.0894    0.3150    0.7272
    ## RaceWhite                      0.6876     1.4543    0.5336    0.8861
    ## Marital.StatusMarried          0.8195     1.2203    0.6490    1.0347
    ## Marital.StatusSeparated        1.5503     0.6451    0.8818    2.7254
    ## Marital.StatusSingle           0.9889     1.0112    0.7434    1.3157
    ## Marital.StatusWidowed          0.9626     1.0388    0.6750    1.3728
    ## T.Stage                        1.3267     0.7538    1.1062    1.5911
    ## N.Stage                        1.4156     0.7064    1.0439    1.9197
    ## X6th.Stage                     0.9962     1.0038    0.8187    1.2122
    ## Grade                          1.4835     0.6741    1.2972    1.6965
    ## A.Stage                        1.1297     0.8852    0.7844    1.6272
    ## Tumor.Size                     0.9998     1.0002    0.9945    1.0050
    ## Estrogen.StatusPositive        0.5197     1.9241    0.3990    0.6770
    ## Progesterone.StatusPositive    0.6249     1.6002    0.5073    0.7699
    ## Regional.Node.Examined         0.9682     1.0329    0.9560    0.9804
    ## Reginol.Node.Positive          1.0550     0.9479    1.0325    1.0779
    ## 
    ## Concordance= 0.743  (se = 0.011 )
    ## Likelihood ratio test= 493.1  on 17 df,   p=<2e-16
    ## Wald test            = 575.7  on 17 df,   p=<2e-16
    ## Score (logrank) test = 648.5  on 17 df,   p=<2e-16

``` r
# Save model summary
tidy_cox <- broom::tidy(cox_model)
write.csv(tidy_cox, "cox_model_summary.csv")

# Evaluate Model Performance
# Concordance index
c_index <- summary(cox_model)$concordance
cat("Concordance Index:", c_index, "\n")
```

    ## Concordance Index: 0.7434161 0.01067736

## Model Diagnositics

``` r
cox_zph <- cox.zph(cox_model)
print(cox_zph)
```

    ##                           chisq df       p
    ## Age                     0.10103  1   0.751
    ## Race                    0.99407  2   0.608
    ## Marital.Status          2.52774  4   0.640
    ## T.Stage                 0.00271  1   0.959
    ## N.Stage                 0.79775  1   0.372
    ## X6th.Stage              0.35537  1   0.551
    ## Grade                   1.88411  1   0.170
    ## A.Stage                 5.52961  1   0.019
    ## Tumor.Size              0.97769  1   0.323
    ## Estrogen.Status        29.60558  1 5.3e-08
    ## Progesterone.Status    32.46468  1 1.2e-08
    ## Regional.Node.Examined  0.01050  1   0.918
    ## Reginol.Node.Positive   0.02145  1   0.884
    ## GLOBAL                 55.54616 17 5.6e-06

``` r
# # Alternatively, using ggplot2 for enhanced visualization
# ggcoxzph(cox_zph)


# Fit Cox model with stratification on violating covariates
cox_model_strat <- coxph(Surv(Survival.Months, Status) ~ Age + Race + Marital.Status + T.Stage + 
                          N.Stage + X6th.Stage + Grade + Tumor.Size + 
                          Regional.Node.Examined + Reginol.Node.Positive + 
                          strata(A.Stage) + strata(Estrogen.Status) + 
                          strata(Progesterone.Status), 
                        data = data_cleaned)

summary(cox_model_strat)
```

    ## Call:
    ## coxph(formula = Surv(Survival.Months, Status) ~ Age + Race + 
    ##     Marital.Status + T.Stage + N.Stage + X6th.Stage + Grade + 
    ##     Tumor.Size + Regional.Node.Examined + Reginol.Node.Positive + 
    ##     strata(A.Stage) + strata(Estrogen.Status) + strata(Progesterone.Status), 
    ##     data = data_cleaned)
    ## 
    ##   n= 4024, number of events= 616 
    ## 
    ##                               coef  exp(coef)   se(coef)      z Pr(>|z|)    
    ## Age                      0.0202369  1.0204430  0.0048649  4.160 3.19e-05 ***
    ## RaceOther               -0.7467612  0.4738990  0.2133503 -3.500 0.000465 ***
    ## RaceWhite               -0.3745104  0.6876258  0.1291795 -2.899 0.003742 ** 
    ## Marital.StatusMarried   -0.1927006  0.8247289  0.1191009 -1.618 0.105671    
    ## Marital.StatusSeparated  0.3660085  1.4419674  0.2906047  1.259 0.207860    
    ## Marital.StatusSingle    -0.0164929  0.9836424  0.1458465 -0.113 0.909964    
    ## Marital.StatusWidowed   -0.0400914  0.9607017  0.1817133 -0.221 0.825381    
    ## T.Stage                  0.2857357  1.3307407  0.0923058  3.096 0.001965 ** 
    ## N.Stage                  0.3453182  1.4124393  0.1571581  2.197 0.028001 *  
    ## X6th.Stage               0.0076886  1.0077182  0.1009082  0.076 0.939265    
    ## Grade                    0.3942280  1.4832388  0.0687292  5.736 9.70e-09 ***
    ## Tumor.Size              -0.0008377  0.9991627  0.0026999 -0.310 0.756370    
    ## Regional.Node.Examined  -0.0315417  0.9689505  0.0064293 -4.906 9.30e-07 ***
    ## Reginol.Node.Positive    0.0507808  1.0520923  0.0110575  4.592 4.38e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                         exp(coef) exp(-coef) lower .95 upper .95
    ## Age                        1.0204     0.9800    1.0108    1.0302
    ## RaceOther                  0.4739     2.1102    0.3119    0.7199
    ## RaceWhite                  0.6876     1.4543    0.5338    0.8857
    ## Marital.StatusMarried      0.8247     1.2125    0.6530    1.0416
    ## Marital.StatusSeparated    1.4420     0.6935    0.8158    2.5487
    ## Marital.StatusSingle       0.9836     1.0166    0.7391    1.3091
    ## Marital.StatusWidowed      0.9607     1.0409    0.6728    1.3717
    ## T.Stage                    1.3307     0.7515    1.1105    1.5946
    ## N.Stage                    1.4124     0.7080    1.0380    1.9220
    ## X6th.Stage                 1.0077     0.9923    0.8269    1.2281
    ## Grade                      1.4832     0.6742    1.2963    1.6971
    ## Tumor.Size                 0.9992     1.0008    0.9939    1.0045
    ## Regional.Node.Examined     0.9690     1.0320    0.9568    0.9812
    ## Reginol.Node.Positive      1.0521     0.9505    1.0295    1.0751
    ## 
    ## Concordance= 0.704  (se = 0.014 )
    ## Likelihood ratio test= 309.6  on 14 df,   p=<2e-16
    ## Wald test            = 339  on 14 df,   p=<2e-16
    ## Score (logrank) test = 364  on 14 df,   p=<2e-16

``` r
cox_zph_strat <- cox.zph(cox_model_strat)
print(cox_zph_strat)
```

    ##                         chisq df    p
    ## Age                     0.355  1 0.55
    ## Race                    0.844  2 0.66
    ## Marital.Status          4.525  4 0.34
    ## T.Stage                 0.562  1 0.45
    ## N.Stage                 0.315  1 0.57
    ## X6th.Stage              0.928  1 0.34
    ## Grade                   0.236  1 0.63
    ## Tumor.Size              0.277  1 0.60
    ## Regional.Node.Examined  0.443  1 0.51
    ## Reginol.Node.Positive   1.552  1 0.21
    ## GLOBAL                 11.635 14 0.64

# 4. Model Building: Logistic Regression

## Varibales selection

``` r
# Building model
logistic_full <- glm(Status ~ . + T.Stage*Tumor.Size + N.Stage*Reginol.Node.Positive, family = binomial, data = data_cleaned)
logistic_null <- glm(Status ~ 1, family = binomial, data = data_cleaned)

# Bootstrap for Logistic Full Model
boot_fn <- function(data, indices) {
  data_boot <- data[indices, ]
  model <- glm(Status ~ ., family = binomial, data = data_boot)
  return(coef(model))
}

set.seed(123)
boot_results <- boot(data_cleaned, boot_fn, R = 1000)
print(boot_results)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = data_cleaned, statistic = boot_fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##          original        bias    std. error
    ## t1*  -0.259066449 -0.0227561038 0.699388362
    ## t2*   0.028075230  0.0004690450 0.006883036
    ## t3*  -0.901437769  0.0028532134 0.309933785
    ## t4*  -0.451197887  0.0101063001 0.175442128
    ## t5*  -0.164186058  0.0016872655 0.177191245
    ## t6*   0.492752378 -0.0277453624 0.574478560
    ## t7*  -0.089658048 -0.0012152272 0.213382784
    ## t8*   0.025760972  0.0088698189 0.264622751
    ## t9*   0.515058360  0.0048517373 0.151837474
    ## t10*  0.449343261 -0.0076262876 0.207672753
    ## t11* -0.104976069  0.0033060705 0.138856498
    ## t12*  0.499042094  0.0075432259 0.097524059
    ## t13* -0.139475584 -0.0098305139 0.306651939
    ## t14* -0.005517452 -0.0001860039 0.004011227
    ## t15* -0.400592712 -0.0079133814 0.210107129
    ## t16* -0.501816924  0.0042759038 0.149878995
    ## t17* -0.030913004 -0.0003498618 0.008132438
    ## t18*  0.075223432  0.0014949438 0.017196375
    ## t19* -0.061397624 -0.0005325745 0.003074691

``` r
# backward selection
backward_model <- step(logistic_full, direction = "backward")
```

    ## Start:  AIC=2279.16
    ## Status ~ Age + Race + Marital.Status + T.Stage + N.Stage + X6th.Stage + 
    ##     Grade + A.Stage + Tumor.Size + Estrogen.Status + Progesterone.Status + 
    ##     Regional.Node.Examined + Reginol.Node.Positive + Survival.Months + 
    ##     T.Stage * Tumor.Size + N.Stage * Reginol.Node.Positive
    ## 
    ##                                 Df Deviance    AIC
    ## - Marital.Status                 4   2240.3 2274.3
    ## - T.Stage:Tumor.Size             1   2237.2 2277.2
    ## - A.Stage                        1   2237.3 2277.3
    ## - X6th.Stage                     1   2237.5 2277.5
    ## - N.Stage:Reginol.Node.Positive  1   2237.9 2277.9
    ## <none>                               2237.2 2279.2
    ## - Estrogen.Status                1   2240.3 2280.3
    ## - Race                           2   2247.3 2285.3
    ## - Progesterone.Status            1   2247.8 2287.8
    ## - Regional.Node.Examined         1   2253.1 2293.1
    ## - Age                            1   2256.2 2296.2
    ## - Grade                          1   2266.4 2306.4
    ## - Survival.Months                1   2956.1 2996.1
    ## 
    ## Step:  AIC=2274.32
    ## Status ~ Age + Race + T.Stage + N.Stage + X6th.Stage + Grade + 
    ##     A.Stage + Tumor.Size + Estrogen.Status + Progesterone.Status + 
    ##     Regional.Node.Examined + Reginol.Node.Positive + Survival.Months + 
    ##     T.Stage:Tumor.Size + N.Stage:Reginol.Node.Positive
    ## 
    ##                                 Df Deviance    AIC
    ## - T.Stage:Tumor.Size             1   2240.3 2272.3
    ## - A.Stage                        1   2240.5 2272.5
    ## - X6th.Stage                     1   2240.6 2272.6
    ## - N.Stage:Reginol.Node.Positive  1   2241.0 2273.0
    ## <none>                               2240.3 2274.3
    ## - Estrogen.Status                1   2243.5 2275.5
    ## - Race                           2   2251.7 2281.7
    ## - Progesterone.Status            1   2251.5 2283.5
    ## - Regional.Node.Examined         1   2256.2 2288.2
    ## - Age                            1   2261.6 2293.6
    ## - Grade                          1   2269.4 2301.4
    ## - Survival.Months                1   2964.2 2996.2
    ## 
    ## Step:  AIC=2272.32
    ## Status ~ Age + Race + T.Stage + N.Stage + X6th.Stage + Grade + 
    ##     A.Stage + Tumor.Size + Estrogen.Status + Progesterone.Status + 
    ##     Regional.Node.Examined + Reginol.Node.Positive + Survival.Months + 
    ##     N.Stage:Reginol.Node.Positive
    ## 
    ##                                 Df Deviance    AIC
    ## - A.Stage                        1   2240.5 2270.5
    ## - X6th.Stage                     1   2240.6 2270.6
    ## - N.Stage:Reginol.Node.Positive  1   2241.0 2271.0
    ## - Tumor.Size                     1   2242.1 2272.1
    ## <none>                               2240.3 2272.3
    ## - Estrogen.Status                1   2243.5 2273.5
    ## - Race                           2   2251.7 2279.7
    ## - Progesterone.Status            1   2251.5 2281.5
    ## - T.Stage                        1   2251.5 2281.5
    ## - Regional.Node.Examined         1   2256.3 2286.3
    ## - Age                            1   2261.6 2291.6
    ## - Grade                          1   2269.5 2299.5
    ## - Survival.Months                1   2965.9 2995.9
    ## 
    ## Step:  AIC=2270.48
    ## Status ~ Age + Race + T.Stage + N.Stage + X6th.Stage + Grade + 
    ##     Tumor.Size + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    ##     Reginol.Node.Positive + Survival.Months + N.Stage:Reginol.Node.Positive
    ## 
    ##                                 Df Deviance    AIC
    ## - X6th.Stage                     1   2240.8 2268.8
    ## - N.Stage:Reginol.Node.Positive  1   2241.2 2269.2
    ## - Tumor.Size                     1   2242.2 2270.2
    ## <none>                               2240.5 2270.5
    ## - Estrogen.Status                1   2243.6 2271.6
    ## - Race                           2   2251.9 2277.9
    ## - T.Stage                        1   2251.5 2279.5
    ## - Progesterone.Status            1   2251.7 2279.7
    ## - Regional.Node.Examined         1   2256.3 2284.3
    ## - Age                            1   2262.0 2290.0
    ## - Grade                          1   2269.9 2297.9
    ## - Survival.Months                1   2965.9 2993.9
    ## 
    ## Step:  AIC=2268.83
    ## Status ~ Age + Race + T.Stage + N.Stage + Grade + Tumor.Size + 
    ##     Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    ##     Reginol.Node.Positive + Survival.Months + N.Stage:Reginol.Node.Positive
    ## 
    ##                                 Df Deviance    AIC
    ## - N.Stage:Reginol.Node.Positive  1   2241.8 2267.8
    ## - Tumor.Size                     1   2242.4 2268.4
    ## <none>                               2240.8 2268.8
    ## - Estrogen.Status                1   2243.9 2269.9
    ## - Race                           2   2252.4 2276.4
    ## - Progesterone.Status            1   2251.9 2277.9
    ## - T.Stage                        1   2255.3 2281.3
    ## - Regional.Node.Examined         1   2256.7 2282.7
    ## - Age                            1   2262.2 2288.2
    ## - Grade                          1   2270.1 2296.1
    ## - Survival.Months                1   2966.0 2992.0
    ## 
    ## Step:  AIC=2267.77
    ## Status ~ Age + Race + T.Stage + N.Stage + Grade + Tumor.Size + 
    ##     Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    ##     Reginol.Node.Positive + Survival.Months
    ## 
    ##                          Df Deviance    AIC
    ## - Tumor.Size              1   2243.2 2267.2
    ## <none>                        2241.8 2267.8
    ## - Estrogen.Status         1   2244.8 2268.8
    ## - N.Stage                 1   2247.9 2271.9
    ## - Race                    2   2253.4 2275.4
    ## - Progesterone.Status     1   2252.8 2276.8
    ## - T.Stage                 1   2256.8 2280.8
    ## - Regional.Node.Examined  1   2257.4 2281.4
    ## - Reginol.Node.Positive   1   2260.8 2284.8
    ## - Age                     1   2263.1 2287.1
    ## - Grade                   1   2271.2 2295.2
    ## - Survival.Months         1   2966.7 2990.7
    ## 
    ## Step:  AIC=2267.23
    ## Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + 
    ##     Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + 
    ##     Survival.Months
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        2243.2 2267.2
    ## - Estrogen.Status         1   2246.2 2268.2
    ## - N.Stage                 1   2248.9 2270.9
    ## - Race                    2   2254.7 2274.7
    ## - Progesterone.Status     1   2254.2 2276.2
    ## - Regional.Node.Examined  1   2258.8 2280.8
    ## - Reginol.Node.Positive   1   2262.3 2284.3
    ## - T.Stage                 1   2264.6 2286.6
    ## - Age                     1   2265.2 2287.2
    ## - Grade                   1   2273.0 2295.0
    ## - Survival.Months         1   2967.1 2989.1

``` r
# forward selection
forward_model <- step(logistic_null, scope = list(lower = logistic_full, upper = logistic_null), direction = "forward")
```

    ## Start:  AIC=3446.68
    ## Status ~ 1

``` r
# stepwise selection
stepwise <- step(logistic_null, scope = list(lower = logistic_full, upper = logistic_null), direction = "both")
```

    ## Start:  AIC=3446.68
    ## Status ~ 1

``` r
# examine if the same model was build
formula(backward_model)
```

    ## Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + 
    ##     Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + 
    ##     Survival.Months

``` r
formula(forward_model)
```

    ## Status ~ 1

``` r
formula(stepwise)
```

    ## Status ~ 1

``` r
# check if there is a close call
summary(backward_model)
```

    ## 
    ## Call:
    ## glm(formula = Status ~ Age + Race + T.Stage + N.Stage + Grade + 
    ##     Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    ##     Reginol.Node.Positive + Survival.Months, family = binomial, 
    ##     data = data_cleaned)
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.422654   0.537693  -0.786 0.431838    
    ## Age                          0.029137   0.006287   4.634 3.58e-06 ***
    ## RaceOther                   -0.937676   0.286598  -3.272 0.001069 ** 
    ## RaceWhite                   -0.474423   0.185175  -2.562 0.010406 *  
    ## T.Stage                      0.332547   0.071744   4.635 3.57e-06 ***
    ## N.Stage                      0.297392   0.124393   2.391 0.016814 *  
    ## Grade                        0.500656   0.092751   5.398 6.74e-08 ***
    ## Estrogen.StatusPositive     -0.389678   0.224930  -1.732 0.083196 .  
    ## Progesterone.StatusPositive -0.509437   0.151281  -3.367 0.000759 ***
    ## Regional.Node.Examined      -0.030766   0.007996  -3.848 0.000119 ***
    ## Reginol.Node.Positive        0.073295   0.016811   4.360 1.30e-05 ***
    ## Survival.Months             -0.061348   0.002743 -22.364  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 2243.2  on 4012  degrees of freedom
    ## AIC: 2267.2
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
summary(forward_model)
```

    ## 
    ## Call:
    ## glm(formula = Status ~ 1, family = binomial, data = data_cleaned)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.71063    0.04378  -39.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 3444.7  on 4023  degrees of freedom
    ## AIC: 3446.7
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
summary(stepwise)
```

    ## 
    ## Call:
    ## glm(formula = Status ~ 1, family = binomial, data = data_cleaned)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.71063    0.04378  -39.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 3444.7  on 4023  degrees of freedom
    ## AIC: 3446.7
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
# results
results <- data.frame(
Model = c("forward", "backward", "stepwise"),
AIC = c(AIC(forward_model), AIC(backward_model), AIC(stepwise)), 
BIC = c(BIC(forward_model), BIC(backward_model), BIC(stepwise))
) 
results
```

    ##      Model      AIC      BIC
    ## 1  forward 3446.683 3452.983
    ## 2 backward 2267.233 2342.833
    ## 3 stepwise 3446.683 3452.983

``` r
#compare AIC with and without Estrogen.StatusPosive
Estrogen.Status_included_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months, family = binomial, data = data_cleaned)

Estrogen.Status_excluded_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months, family = binomial, data = data_cleaned) 

result1 <- data.frame(
Model = c("with Estrogen.StatusPosive", "without Estrogen.StatusPosive"),
AIC = c(AIC(Estrogen.Status_included_backward), AIC(Estrogen.Status_excluded_backward)), 
BIC = c(BIC(Estrogen.Status_included_backward), BIC(Estrogen.Status_excluded_backward))
) 
result1
```

    ##                           Model      AIC      BIC
    ## 1    with Estrogen.StatusPosive 2267.233 2342.833
    ## 2 without Estrogen.StatusPosive 2268.216 2337.516

``` r
#compare AIC with and without N.Stage*Regional.Node.Positive
Interaction_included_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months + N.Stage*Reginol.Node.Positive, family = binomial, data = data_cleaned)

Interaction_excluded_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months, family = binomial, data = data_cleaned)

result2 <- data.frame(
Model = c("with N.Stage*Regional.Node.Positive", "without N.Stage*Regional.Node.Positive"),
AIC = c(AIC(Interaction_included_backward), AIC(Interaction_excluded_backward)), 
BIC = c(BIC(Interaction_included_backward), BIC(Interaction_excluded_backward))
) 
result2
```

    ##                                    Model      AIC      BIC
    ## 1    with N.Stage*Regional.Node.Positive 2268.356 2350.256
    ## 2 without N.Stage*Regional.Node.Positive 2267.233 2342.833

``` r
#compare AIC with and without N.Stage
Nstage_included_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months, family = binomial, data = data_cleaned)

Nstage_excluded_backward = glm(Status ~ Age + Race + T.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months, family = binomial, data = data_cleaned)

result3 <- data.frame(
Model = c("with N.Stage", "without N.Stage"),
AIC = c(AIC(Nstage_included_backward), AIC(Nstage_excluded_backward)), 
BIC = c(BIC(Nstage_included_backward), BIC(Nstage_excluded_backward))
) 
result3
```

    ##             Model      AIC      BIC
    ## 1    with N.Stage 2267.233 2342.833
    ## 2 without N.Stage 2270.923 2340.224

``` r
#compare AIC with and without Regional.Node.Positive
RNP_included_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + Survival.Months, family = binomial, data = data_cleaned)

RNP_excluded_backward = glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Survival.Months, family = binomial, data = data_cleaned)

result4 <- data.frame(
Model = c("with Regional.Node.Positive", "without Regional.Node.Positive"),
AIC = c(AIC(RNP_included_backward), AIC(RNP_excluded_backward)), 
BIC = c(BIC(RNP_included_backward), BIC(RNP_excluded_backward))
) 
result4
```

    ##                            Model      AIC      BIC
    ## 1    with Regional.Node.Positive 2267.233 2342.833
    ## 2 without Regional.Node.Positive 2284.273 2353.574

## try using lasso for model selection and test the model

``` r
predictors <- model.matrix(Status ~ ., data = data_cleaned)[, -1]
patients_status <- as.numeric(data_cleaned$Status)
lasso_model <- cv.glmnet(
  predictors, 
  patients_status, 
  alpha = 1,            
  family = "binomial"   
)

# best lambda
best_lambda <- lasso_model$lambda.min
best_lambda
```

    ## [1] 0.0006459749

``` r
# select variables
lasso_coefs <- coef(lasso_model, s = best_lambda)
lasso_coefs_matrix <- as.matrix(lasso_coefs)
selected_dummies <- rownames(lasso_coefs_matrix)[lasso_coefs_matrix != 0][-1] 

selected_predictors <- unique(gsub("(.*)\\..*", "\\1", selected_dummies))
selected_predictors
```

    ##  [1] "Age"           "RaceOther"     "RaceWhite"     "Marital"      
    ##  [5] "T"             "N"             "Grade"         "A"            
    ##  [9] "Tumor"         "Estrogen"      "Progesterone"  "Regional.Node"
    ## [13] "Reginol.Node"  "Survival"

``` r
#logistics regression
logistics_test2 <- glm(Status ~ Age + Race + Marital.Status + T.Stage + N.Stage + Grade + Tumor.Size + Estrogen.Status + Progesterone.Status + Regional.Node.Examined + Survival.Months, data = data_cleaned, family = binomial)
summary(logistics_test2)
```

    ## 
    ## Call:
    ## glm(formula = Status ~ Age + Race + Marital.Status + T.Stage + 
    ##     N.Stage + Grade + Tumor.Size + Estrogen.Status + Progesterone.Status + 
    ##     Regional.Node.Examined + Survival.Months, family = binomial, 
    ##     data = data_cleaned)
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.8296671  0.5584710  -1.486  0.13738    
    ## Age                          0.0289454  0.0064405   4.494 6.98e-06 ***
    ## RaceOther                   -0.8879879  0.2895050  -3.067  0.00216 ** 
    ## RaceWhite                   -0.4237485  0.1889672  -2.242  0.02493 *  
    ## Marital.StatusMarried       -0.1784395  0.1650737  -1.081  0.27971    
    ## Marital.StatusSeparated      0.5193406  0.4841274   1.073  0.28339    
    ## Marital.StatusSingle        -0.0958285  0.2049063  -0.468  0.64002    
    ## Marital.StatusWidowed       -0.0008013  0.2573882  -0.003  0.99752    
    ## T.Stage                      0.4375839  0.1114225   3.927 8.59e-05 ***
    ## N.Stage                      0.7233206  0.0788680   9.171  < 2e-16 ***
    ## Grade                        0.4848063  0.0923886   5.247 1.54e-07 ***
    ## Tumor.Size                  -0.0048483  0.0039248  -1.235  0.21672    
    ## Estrogen.StatusPositive     -0.3822399  0.2239074  -1.707  0.08780 .  
    ## Progesterone.StatusPositive -0.4855005  0.1511848  -3.211  0.00132 ** 
    ## Regional.Node.Examined      -0.0189581  0.0072624  -2.610  0.00904 ** 
    ## Survival.Months             -0.0614349  0.0027441 -22.388  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 2257.3  on 4008  degrees of freedom
    ## AIC: 2289.3
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
#diagosis
std_residuals <- rstandard(logistics_test2)
leverage <- hatvalues(logistics_test2)
cooks_distance <- cooks.distance(logistics_test2)
plot_data <- data.frame(Leverage = leverage, Residuals = std_residuals)

ggplot(plot_data, aes(x = leverage, y = std_residuals)) +
  geom_point(aes(color = cooks_distance > 4 / nrow(data_cleaned))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2 * mean(leverage), linetype = "dashed", color = "blue") +
  labs(
    title = "Residuals vs Leverage with Cook's Distance",
    x = "Leverage (Hat Values)",
    y = "Standardized Residuals (Deviance)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("black", "purple"), labels = c("Not Influential", "Influential"))
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Likelihood Ratio Test

``` r
#Regional.Node.Examined * Reginol.Node.Positive (significant, lrt suggests they should be included)
interaction_model <- glm(Status ~ Regional.Node.Examined * Reginol.Node.Positive, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ Regional.Node.Examined + Reginol.Node.Positive, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ Regional.Node.Examined + Reginol.Node.Positive
    ## Model 2: Status ~ Regional.Node.Examined * Reginol.Node.Positive
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1      4021     3208.3                          
    ## 2      4020     3190.1  1   18.196 1.993e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#A.Stage * Reginol.Node.Positive (significant, lrt suggests they should be included)
interaction_model <- glm(Status ~ A.Stage * Reginol.Node.Positive, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ A.Stage + Reginol.Node.Positive, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ A.Stage + Reginol.Node.Positive
    ## Model 2: Status ~ A.Stage * Reginol.Node.Positive
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1      4021     3232.8                          
    ## 2      4020     3218.7  1   14.064 0.0001767 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#T.Stage * Tumor.Size
interaction_model <- glm(Status ~ T.Stage * Tumor.Size, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ T.Stage + Tumor.Size, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ T.Stage + Tumor.Size
    ## Model 2: Status ~ T.Stage * Tumor.Size
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3352.6                     
    ## 2      4020     3350.2  1   2.3433   0.1258

``` r
#N.Stage * X6th.Stage
interaction_model <- glm(Status ~ N.Stage*X6th.Stage, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ N.Stage + X6th.Stage, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ N.Stage + X6th.Stage
    ## Model 2: Status ~ N.Stage * X6th.Stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3193.9                     
    ## 2      4020     3193.5  1  0.35755   0.5499

``` r
#N.Stage * Reginol.Node.Positive
interaction_model <- glm(Status ~ N.Stage*Reginol.Node.Positive, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ N.Stage + Reginol.Node.Positive, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ N.Stage + Reginol.Node.Positive
    ## Model 2: Status ~ N.Stage * Reginol.Node.Positive
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3203.3                     
    ## 2      4020     3202.1  1   1.1307   0.2876

``` r
#X6th.Stage ~ Reginol.Node.Positive
interaction_model <- glm(Status ~ X6th.Stage*Reginol.Node.Positive, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ X6th.Stage + Reginol.Node.Positive, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ X6th.Stage + Reginol.Node.Positive
    ## Model 2: Status ~ X6th.Stage * Reginol.Node.Positive
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3186.9                     
    ## 2      4020     3185.1  1   1.7275   0.1887

``` r
#T.Stage ~ X6th.Stage
interaction_model <- glm(Status ~ T.Stage*X6th.Stage, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ T.Stage + X6th.Stage, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ T.Stage + X6th.Stage
    ## Model 2: Status ~ T.Stage * X6th.Stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3196.0                     
    ## 2      4020     3193.3  1    2.655   0.1032

``` r
#T.Stage ~ N.Stage
interaction_model <- glm(Status ~ T.Stage * N.Stage, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ T.Stage + N.Stage, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ T.Stage + N.Stage
    ## Model 2: Status ~ T.Stage * N.Stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3183.0                     
    ## 2      4020     3182.7  1  0.27976   0.5969

``` r
#T.Stage ~ A.Stage
interaction_model <- glm(Status ~ T.Stage * A.Stage, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ T.Stage + A.Stage, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ T.Stage + A.Stage
    ## Model 2: Status ~ T.Stage * A.Stage
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1      4021     3344.0                       
    ## 2      4020     3340.7  1   3.2792  0.07016 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#A.Stage * Reginol.Node.Positive !!!
interaction_model <- glm(Status ~ A.Stage * Reginol.Node.Positive, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ A.Stage + Reginol.Node.Positive, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ A.Stage + Reginol.Node.Positive
    ## Model 2: Status ~ A.Stage * Reginol.Node.Positive
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1      4021     3232.8                          
    ## 2      4020     3218.7  1   14.064 0.0001767 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Tumor.Size ~ Reginol.Node.Positive
interaction_model <- glm(Status ~ X6th.Stage * Regional.Node.Examined, data = data_cleaned, family = binomial)
base_model <- glm(Status ~ X6th.Stage + Regional.Node.Examined, data = data_cleaned, family = binomial)
lrt <- anova(base_model, interaction_model, test = "LRT")
print(lrt)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Status ~ X6th.Stage + Regional.Node.Examined
    ## Model 2: Status ~ X6th.Stage * Regional.Node.Examined
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1      4021     3185.2                     
    ## 2      4020     3184.7  1  0.47797   0.4893

## logistic regression

``` r
# with Reginol.Node.Positive and Regional.Node.Examined * Reginol.Node.Positive
logistics <- glm(Status ~ Age + Race + T.Stage + N.Stage + Grade + 
    Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    Reginol.Node.Positive + Regional.Node.Examined * 
    Reginol.Node.Positive, family = binomial, data = data_cleaned)
summary(logistics)
```

    ## 
    ## Call:
    ## glm(formula = Status ~ Age + Race + T.Stage + N.Stage + Grade + 
    ##     Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    ##     Reginol.Node.Positive + Regional.Node.Examined * Reginol.Node.Positive, 
    ##     family = binomial, data = data_cleaned)
    ## 
    ## Coefficients:
    ##                                                Estimate Std. Error z value
    ## (Intercept)                                  -3.4216072  0.4503808  -7.597
    ## Age                                           0.0247414  0.0054301   4.556
    ## RaceOther                                    -0.9990226  0.2453012  -4.073
    ## RaceWhite                                    -0.5652089  0.1582853  -3.571
    ## T.Stage                                       0.3174751  0.0608692   5.216
    ## N.Stage                                       0.2806696  0.1253506   2.239
    ## Grade                                         0.4463125  0.0787679   5.666
    ## Estrogen.StatusPositive                      -0.7517817  0.1764558  -4.260
    ## Progesterone.StatusPositive                  -0.5795957  0.1271259  -4.559
    ## Regional.Node.Examined                       -0.0313358  0.0083278  -3.763
    ## Reginol.Node.Positive                         0.1022236  0.0314179   3.254
    ## Regional.Node.Examined:Reginol.Node.Positive -0.0008627  0.0008702  -0.991
    ##                                              Pr(>|z|)    
    ## (Intercept)                                  3.03e-14 ***
    ## Age                                          5.20e-06 ***
    ## RaceOther                                    4.65e-05 ***
    ## RaceWhite                                    0.000356 ***
    ## T.Stage                                      1.83e-07 ***
    ## N.Stage                                      0.025151 *  
    ## Grade                                        1.46e-08 ***
    ## Estrogen.StatusPositive                      2.04e-05 ***
    ## Progesterone.StatusPositive                  5.13e-06 ***
    ## Regional.Node.Examined                       0.000168 ***
    ## Reginol.Node.Positive                        0.001139 ** 
    ## Regional.Node.Examined:Reginol.Node.Positive 0.321458    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 2966.1  on 4012  degrees of freedom
    ## AIC: 2990.1
    ## 
    ## Number of Fisher Scoring iterations: 5

# cv for glm

``` r
#cv for glm
data_cleaned$Status <- factor(data_cleaned$Status, levels = c(0, 1))
control <- trainControl(method = "cv", number = 10)
logistic_cv_model <- train(
  Status ~Age + Race + T.Stage + N.Stage + Grade + 
    Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    Reginol.Node.Positive + Regional.Node.Examined * 
    Reginol.Node.Positive,
  data = data_cleaned,
  method = "glm",
  family = binomial,
  trControl = control
)
logistic_cv_model
```

    ## Generalized Linear Model 
    ## 
    ## 4024 samples
    ##    9 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 3621, 3621, 3621, 3622, 3622, 3622, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa   
    ##   0.8571131  0.196932

\#GAM

``` r
#GAM using predictors from auto procedure
logistics_gam_auto <- gam(Status ~ Age + Race + T.Stage + N.Stage + Grade + 
    Estrogen.Status + Progesterone.Status + Regional.Node.Examined + 
    Reginol.Node.Positive + Regional.Node.Examined * 
    Reginol.Node.Positive,
    data = data_cleaned,
    family = binomial)
summary(logistics_gam_auto)
```

    ## 
    ## Family: binomial 
    ## Link function: logit 
    ## 
    ## Formula:
    ## Status ~ Age + Race + T.Stage + N.Stage + Grade + Estrogen.Status + 
    ##     Progesterone.Status + Regional.Node.Examined + Reginol.Node.Positive + 
    ##     Regional.Node.Examined * Reginol.Node.Positive
    ## 
    ## Parametric coefficients:
    ##                                                Estimate Std. Error z value
    ## (Intercept)                                  -3.4216072  0.4503812  -7.597
    ## Age                                           0.0247414  0.0054301   4.556
    ## RaceOther                                    -0.9990226  0.2453016  -4.073
    ## RaceWhite                                    -0.5652089  0.1582854  -3.571
    ## T.Stage                                       0.3174751  0.0608692   5.216
    ## N.Stage                                       0.2806696  0.1253507   2.239
    ## Grade                                         0.4463125  0.0787679   5.666
    ## Estrogen.StatusPositive                      -0.7517817  0.1764559  -4.260
    ## Progesterone.StatusPositive                  -0.5795957  0.1271260  -4.559
    ## Regional.Node.Examined                       -0.0313358  0.0083278  -3.763
    ## Reginol.Node.Positive                         0.1022236  0.0314179   3.254
    ## Regional.Node.Examined:Reginol.Node.Positive -0.0008627  0.0008702  -0.991
    ##                                              Pr(>|z|)    
    ## (Intercept)                                  3.03e-14 ***
    ## Age                                          5.20e-06 ***
    ## RaceOther                                    4.65e-05 ***
    ## RaceWhite                                    0.000356 ***
    ## T.Stage                                      1.83e-07 ***
    ## N.Stage                                      0.025151 *  
    ## Grade                                        1.46e-08 ***
    ## Estrogen.StatusPositive                      2.04e-05 ***
    ## Progesterone.StatusPositive                  5.13e-06 ***
    ## Regional.Node.Examined                       0.000168 ***
    ## Reginol.Node.Positive                        0.001139 ** 
    ## Regional.Node.Examined:Reginol.Node.Positive 0.321459    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## R-sq.(adj) =  0.139   Deviance explained = 13.9%
    ## UBRE = -0.25694  Scale est. = 1         n = 4024

``` r
AIC(logistics_gam_auto, logistics)
```

    ##                    df      AIC
    ## logistics_gam_auto 12 2990.083
    ## logistics          12 2990.083

``` r
#Gam using predictors from LASSO
logistics_gam_lasso <- gam(Status ~ s(Age) + Race + Marital.Status + T.Stage + N.Stage + Grade + s(Tumor.Size) + Estrogen.Status + 
    Progesterone.Status + s(Regional.Node.Examined), data = data_cleaned, family = binomial)

summary(logistics_gam_lasso)
```

    ## 
    ## Family: binomial 
    ## Link function: logit 
    ## 
    ## Formula:
    ## Status ~ s(Age) + Race + Marital.Status + T.Stage + N.Stage + 
    ##     Grade + s(Tumor.Size) + Estrogen.Status + Progesterone.Status + 
    ##     s(Regional.Node.Examined)
    ## 
    ## Parametric coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -2.91346    0.36954  -7.884 3.17e-15 ***
    ## RaceOther                   -0.91390    0.24730  -3.696 0.000219 ***
    ## RaceWhite                   -0.49450    0.16218  -3.049 0.002295 ** 
    ## Marital.StatusMarried       -0.23157    0.14130  -1.639 0.101235    
    ## Marital.StatusSeparated      0.76931    0.37959   2.027 0.042694 *  
    ## Marital.StatusSingle        -0.09630    0.17473  -0.551 0.581548    
    ## Marital.StatusWidowed       -0.10363    0.22115  -0.469 0.639381    
    ## T.Stage                      0.37367    0.09478   3.942 8.07e-05 ***
    ## N.Stage                      0.79284    0.06790  11.677  < 2e-16 ***
    ## Grade                        0.42963    0.07906   5.434 5.51e-08 ***
    ## Estrogen.StatusPositive     -0.71579    0.17675  -4.050 5.13e-05 ***
    ## Progesterone.StatusPositive -0.59286    0.12772  -4.642 3.45e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                             edf Ref.df Chi.sq p-value    
    ## s(Age)                    2.941  3.683 44.531  <2e-16 ***
    ## s(Tumor.Size)             1.001  1.002  0.493  0.4836    
    ## s(Regional.Node.Examined) 2.227  2.839 16.348  0.0011 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.14   Deviance explained = 14.3%
    ## UBRE = -0.25725  Scale est. = 1         n = 4024

``` r
AIC(logistics_gam_auto, logistics_test2)
```

    ##                    df      AIC
    ## logistics_gam_auto 12 2990.083
    ## logistics_test2    16 2289.260

\#cv for GAM

## Model Diagnostics

``` r
# Test for linearity
test_logit_model <- glm(Status ~ ., family = binomial, data = data_cleaned)

probabilities <- predict(test_logit_model, type = "response")
mydata <- data_cleaned %>%
  dplyr::select(Age, Tumor.Size, Regional.Node.Examined, Reginol.Node.Positive)

predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](./-Breast-cancer-survival-prediction_files/figure-gfm/logit_diagnostics-1.png)<!-- -->

``` r
logit_vif_values <- vif(test_logit_model)
print(logit_vif_values)
```

    ##                             GVIF Df GVIF^(1/(2*Df))
    ## Age                     1.113643  1        1.055293
    ## Race                    1.077098  2        1.018741
    ## Marital.Status          1.134254  4        1.015872
    ## T.Stage                 4.433297  1        2.105540
    ## N.Stage                10.002709  1        3.162706
    ## X6th.Stage             12.521419  1        3.538562
    ## Grade                   1.097033  1        1.047393
    ## A.Stage                 1.116575  1        1.056681
    ## Tumor.Size              2.701953  1        1.643762
    ## Estrogen.Status         1.409987  1        1.187429
    ## Progesterone.Status     1.373098  1        1.171793
    ## Regional.Node.Examined  1.422387  1        1.192639
    ## Reginol.Node.Positive   3.749656  1        1.936403
    ## Survival.Months         1.035249  1        1.017472

``` r
data_cleaned <- data_cleaned %>% 
  select(-X6th.Stage)
test_logit_model2 <- glm(Status ~ ., family = binomial, data = data_cleaned)
logit_vif_values2 <- vif(test_logit_model2)
print(logit_vif_values2)
```

    ##                            GVIF Df GVIF^(1/(2*Df))
    ## Age                    1.112827  1        1.054906
    ## Race                   1.075632  2        1.018394
    ## Marital.Status         1.128726  4        1.015251
    ## T.Stage                2.674413  1        1.635363
    ## N.Stage                3.251078  1        1.803075
    ## Grade                  1.096509  1        1.047143
    ## A.Stage                1.112119  1        1.054570
    ## Tumor.Size             2.632957  1        1.622639
    ## Estrogen.Status        1.409027  1        1.187025
    ## Progesterone.Status    1.371578  1        1.171144
    ## Regional.Node.Examined 1.422903  1        1.192855
    ## Reginol.Node.Positive  3.597139  1        1.896613
    ## Survival.Months        1.034575  1        1.017141

``` r
# independence of errors
residuals <- residuals(test_logit_model2, type = "pearson")
plot(residuals, main = "Residual Plot", xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#outlier assumption
plot(test_logit_model2, which = 4, id.n = 3)
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
model.data <- broom::augment(test_logit_model2) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
```

    ## # A tibble: 3 × 21
    ##   Status   Age Race  Marital.Status T.Stage N.Stage Grade A.Stage Tumor.Size
    ##   <fct>  <int> <fct> <fct>            <dbl>   <dbl> <dbl>   <dbl>      <int>
    ## 1 1         46 White Separated            2       1     3       1         25
    ## 2 1         67 White Separated            1       1     2       1         15
    ## 3 1         42 Other Separated            2       1     3       1         21
    ## # ℹ 12 more variables: Estrogen.Status <fct>, Progesterone.Status <fct>,
    ## #   Regional.Node.Examined <int>, Reginol.Node.Positive <int>,
    ## #   Survival.Months <int>, .fitted <dbl>, .resid <dbl>, .hat <dbl>,
    ## #   .sigma <dbl>, .cooksd <dbl>, .std.resid <dbl>, index <int>

``` r
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Status), alpha = .5) +
  theme_bw()
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
model.data %>% 
  filter(abs(.std.resid) > 3)
```

    ## # A tibble: 5 × 21
    ##   Status   Age Race  Marital.Status T.Stage N.Stage Grade A.Stage Tumor.Size
    ##   <fct>  <int> <fct> <fct>            <dbl>   <dbl> <dbl>   <dbl>      <int>
    ## 1 1         38 Other Divorced             1       1     2       1         20
    ## 2 1         51 White Married              1       1     2       1         18
    ## 3 1         40 Other Single               3       1     2       1         60
    ## 4 1         61 White Divorced             1       1     2       1         10
    ## 5 1         41 White Married              2       1     2       1         35
    ## # ℹ 12 more variables: Estrogen.Status <fct>, Progesterone.Status <fct>,
    ## #   Regional.Node.Examined <int>, Reginol.Node.Positive <int>,
    ## #   Survival.Months <int>, .fitted <dbl>, .resid <dbl>, .hat <dbl>,
    ## #   .sigma <dbl>, .cooksd <dbl>, .std.resid <dbl>, index <int>

``` r
# existence of of strongly influential outliers
std_residuals <- rstandard(test_logit_model2)
leverage <- hatvalues(test_logit_model2)
cooks_distance <- cooks.distance(test_logit_model2)
plot_data <- data.frame(Leverage = leverage, Residuals = std_residuals)

ggplot(plot_data, aes(x = leverage, y = std_residuals)) +
  geom_point(aes(color = cooks_distance > 4 / nrow(data_cleaned))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2 * mean(leverage), linetype = "dashed", color = "blue") +
  labs(
    title = "Residuals vs Leverage with Cook's Distance",
    x = "Leverage (Hat Values)",
    y = "Standardized Residuals (Deviance)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("black", "purple"), labels = c("Not Influential", "Influential"))
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

Here we diagnose the logistric regression model by testing its
assumption. According to the results, we found non-linear relationship
between continuous variables and the log odd of binary outcome in our
initial model. There is multicolinearity for `T.Stage`, `N.Stage`,
`X6th.Stage`. However, we found that these variables might be highly
correlated since they are using similar measuring systems. By removing
`X6th.Stage`, the multicolinearity no longer exist. And out initial
model pass the test for error independence and influencial outliers.

# 4. Fairness Analysis

# 4. Fairness Analysis

``` r
# Define demographic groups to evaluate
group_var <- "Race"  # Example: Race
unique_groups <- unique(data_cleaned[[group_var]])

# Initialize results dataframe
fairness_results <- data.frame(
  Group = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each group and calculate metrics
for (group in unique_groups) {
  subset_data <- data_cleaned[data_cleaned[[group_var]] == group, ]
  
  # Make predictions
  probabilities <- predict(logistics_test2, newdata = subset_data, type = "response")
  predictions <- ifelse(probabilities > 0.5, 1, 0)
  # True labels
  true_labels <- subset_data$Status
  
predictions <- as.numeric(as.character(predictions))  # Convert from factor to numeric
true_labels <- as.numeric(as.character(true_labels))  # Convert from factor to numeric
accuracy <- mean(predictions == true_labels)
precision <- sum(predictions == 1 & true_labels == 1) / sum(predictions == 1)
recall <- sum(predictions == 1 & true_labels == 1) / sum(true_labels == 1)
# Add to results
  fairness_results <- rbind(
    fairness_results,
    data.frame(
      Group = group,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall
    )
  )
}

# Display results
print(fairness_results)
```

    ##   Group  Accuracy Precision    Recall
    ## 1 White 0.8986229 0.7789116 0.4490196
    ## 2 Black 0.8728522 0.8333333 0.6164384
    ## 3 Other 0.8968750 0.5000000 0.3030303

``` r
# Calculate positive prediction rates for each group
positive_rates <- data_cleaned %>%
  mutate(
    Predicted = predict(logistics_test2, type = "response") > 0.5
  ) %>%
  group_by(Race) %>%
  summarise(
    PositiveRate = mean(Predicted)
  )

# Display positive rates
print(positive_rates)
```

    ## # A tibble: 3 × 2
    ##   Race  PositiveRate
    ##   <fct>        <dbl>
    ## 1 Black       0.186 
    ## 2 Other       0.0625
    ## 3 White       0.0861

``` r
ggplot(positive_rates, aes(x = Race, y = PositiveRate, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Positive Prediction Rates by Race",
    x = "Race",
    y = "Positive Prediction Rate"
  ) +
  theme_minimal()
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Calculate recall (true positive rate) for each group
recall_rates <- data_cleaned %>%
  mutate(
    Predicted = predict(logistics_test2, type = "response") > 0.5
  ) %>%
  group_by(Race) %>%
  summarise(
    Recall = sum(Predicted & Status == 1) / sum(Status == 1)
  )

# Display recall rates
print(recall_rates)
```

    ## # A tibble: 3 × 2
    ##   Race  Recall
    ##   <fct>  <dbl>
    ## 1 Black  0.616
    ## 2 Other  0.303
    ## 3 White  0.449

``` r
# Calculate precision for each group
precision_rates <- data_cleaned %>%
  mutate(
    Predicted = predict(logistics_test2, type = "response") > 0.5
  ) %>%
  group_by(Race) %>%
  summarise(
    Precision = sum(Predicted & Status == 1) / sum(Predicted)
  )

# Display precision rates
print(precision_rates)
```

    ## # A tibble: 3 × 2
    ##   Race  Precision
    ##   <fct>     <dbl>
    ## 1 Black     0.833
    ## 2 Other     0.5  
    ## 3 White     0.779

``` r
# Combine "Black" and "Other" into one category
data_cleaned <- data_cleaned %>%
  mutate(
    Race_Grouped = ifelse(Race %in% c("Black", "Other"), "Black/Other", "White")
  )

# Check the new distribution of Race_Grouped
table(data_cleaned$Race_Grouped)
```

    ## 
    ## Black/Other       White 
    ##         611        3413

``` r
# Define the grouped Race variable
group_var <- "Race_Grouped"
unique_groups <- unique(data_cleaned[[group_var]])

# Initialize results dataframe
fairness_results_grouped <- data.frame(
  Group = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each group and calculate metrics
for (group in unique_groups) {
  subset_data <- data_cleaned[data_cleaned[[group_var]] == group, ]
  # Make predictions
  probabilities <- predict(logistics_test2, newdata = subset_data, type = "response")
  predictions <- ifelse(probabilities > 0.5, 1, 0)
  # True labels
  true_labels <- subset_data$Status
  
predictions <- as.numeric(as.character(predictions))  # Convert from factor to numeric
true_labels <- as.numeric(as.character(true_labels))  # Convert from factor to numeric
accuracy <- mean(predictions == true_labels)
precision <- sum(predictions == 1 & true_labels == 1) / sum(predictions == 1)
recall <- sum(predictions == 1 & true_labels == 1) / sum(true_labels == 1)
  
  # Add to results
  fairness_results_grouped <- rbind(
    fairness_results_grouped,
    data.frame(
      Group = group,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall
    )
  )
}

# Display results
print(fairness_results_grouped)
```

    ##         Group  Accuracy Precision    Recall
    ## 1       White 0.8986229 0.7789116 0.4490196
    ## 2 Black/Other 0.8854337 0.7432432 0.5188679

``` r
# Calculate positive prediction rates for each group
positive_rates_grouped <- data_cleaned %>%
  mutate(
    Predicted = predict(logistics_test2, type = "response") > 0.5
  ) %>%
  group_by(Race_Grouped) %>%
  summarise(
    PositiveRate = mean(Predicted)
  )

# Display positive rates
print(positive_rates_grouped)
```

    ## # A tibble: 2 × 2
    ##   Race_Grouped PositiveRate
    ##   <chr>               <dbl>
    ## 1 Black/Other        0.121 
    ## 2 White              0.0861

``` r
# Calculate recall (true positive rate) for each group
recall_rates_grouped <- data_cleaned %>%
  mutate(
    Predicted = predict(logistics_test2, type = "response") > 0.5
  ) %>%
  group_by(Race_Grouped) %>%
  summarise(
    Recall = sum(Predicted & Status == 1) / sum(Status == 1)
  )

# Display recall rates
print(recall_rates_grouped)
```

    ## # A tibble: 2 × 2
    ##   Race_Grouped Recall
    ##   <chr>         <dbl>
    ## 1 Black/Other   0.519
    ## 2 White         0.449

``` r
# Calculate precision for each group
precision_rates_grouped <- data_cleaned %>%
  mutate(
    Predicted = predict(logistics_test2, type = "response") > 0.5
  ) %>%
  group_by(Race_Grouped) %>%
  summarise(
    Precision = sum(Predicted & Status == 1) / sum(Predicted)
  )

# Display precision rates
print(precision_rates_grouped)
```

    ## # A tibble: 2 × 2
    ##   Race_Grouped Precision
    ##   <chr>            <dbl>
    ## 1 Black/Other      0.743
    ## 2 White            0.779

``` r
# Bar plot for positive prediction rates
ggplot(positive_rates_grouped, aes(x = Race_Grouped, y = PositiveRate, fill = Race_Grouped)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Positive Prediction Rates by Race Group",
    x = "Race Group",
    y = "Positive Prediction Rate"
  ) +
  theme_minimal()
```

![](./-Breast-cancer-survival-prediction_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Perform chi-squared test
chisq_test_grouped <- chisq.test(positive_rates_grouped$PositiveRate)
```

    ## Warning in chisq.test(positive_rates_grouped$PositiveRate): Chi-squared
    ## approximation may be incorrect

``` r
print(chisq_test_grouped)
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  positive_rates_grouped$PositiveRate
    ## X-squared = 0.0059011, df = 1, p-value = 0.9388
