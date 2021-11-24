hw6
================
Matthew Spotnitz
11/23/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-36. For overview type 'help("mgcv-package")'.

\#\#Problem 1 I will import and view the birthweight dataset.

``` r
birthweight_df = read.csv("birthweight.csv")
birthweight_df = janitor::clean_names(birthweight_df)
head(birthweight_df)
```

    ##   babysex bhead blength  bwt delwt fincome frace gaweeks malform menarche
    ## 1       2    34      51 3629   177      35     1    39.9       0       13
    ## 2       1    34      48 3062   156      65     2    25.9       0       14
    ## 3       2    36      50 3345   148      85     1    39.9       0       12
    ## 4       1    34      52 3062   157      55     1    40.0       0       14
    ## 5       2    34      52 3374   156       5     1    41.6       0       13
    ## 6       1    33      52 3374   129      55     1    40.7       0       12
    ##   mheight momage mrace parity pnumlbw pnumsga    ppbmi ppwt smoken wtgain
    ## 1      63     36     1      3       0       0 26.27184  148      0     29
    ## 2      65     25     2      0       0       0 21.34485  128      0     28
    ## 3      64     29     1      0       0       0 23.56517  137      1     11
    ## 4      64     18     1      0       0       0 21.84508  127     10     30
    ## 5      66     20     1      0       0       0 21.02642  130      1     26
    ## 6      66     23     1      0       0       0 18.60030  115      0     14

``` r
tail(birthweight_df)
```

    ##      babysex bhead blength  bwt delwt fincome frace gaweeks malform menarche
    ## 4337       2    35      52 3657   142      55     2    40.0       0       13
    ## 4338       1    34      50 3147   129      25     2    39.0       0       12
    ## 4339       1    34      51 3430   158      25     2    39.4       0       12
    ## 4340       1    35      52 3090   128      15     2    38.1       0       14
    ## 4341       2    32      46 2268   120       5     2    39.0       0       13
    ## 4342       2    34      52 3232   149      25     2    40.6       0       11
    ##      mheight momage mrace parity pnumlbw pnumsga    ppbmi ppwt smoken wtgain
    ## 4337      63     16     2      0       0       0 21.30149  120      0     22
    ## 4338      64     20     2      0       0       0 18.06089  105      0     24
    ## 4339      66     18     2      0       0       0 19.73249  122      0     36
    ## 4340      56     18     2      0       0       0 21.56776   96      3     32
    ## 4341      62     17     2      0       0       0 19.24491  105      0     15
    ## 4342      63     16     2      0       0       0 23.43164  132      0     17

``` r
str(birthweight_df)
```

    ## 'data.frame':    4342 obs. of  20 variables:
    ##  $ babysex : int  2 1 2 1 2 1 2 2 1 1 ...
    ##  $ bhead   : int  34 34 36 34 34 33 33 33 36 33 ...
    ##  $ blength : int  51 48 50 52 52 52 46 49 52 50 ...
    ##  $ bwt     : int  3629 3062 3345 3062 3374 3374 2523 2778 3515 3459 ...
    ##  $ delwt   : int  177 156 148 157 156 129 126 140 146 169 ...
    ##  $ fincome : int  35 65 85 55 5 55 96 5 85 75 ...
    ##  $ frace   : int  1 2 1 1 1 1 2 1 1 2 ...
    ##  $ gaweeks : num  39.9 25.9 39.9 40 41.6 ...
    ##  $ malform : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ menarche: int  13 14 12 14 13 12 14 12 11 12 ...
    ##  $ mheight : int  63 65 64 64 66 66 72 62 61 64 ...
    ##  $ momage  : int  36 25 29 18 20 23 29 19 13 19 ...
    ##  $ mrace   : int  1 2 1 1 1 1 2 1 1 2 ...
    ##  $ parity  : int  3 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumlbw : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumsga : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ppbmi   : num  26.3 21.3 23.6 21.8 21 ...
    ##  $ ppwt    : int  148 128 137 127 130 115 105 119 105 145 ...
    ##  $ smoken  : num  0 0 1 10 1 0 0 0 0 4 ...
    ##  $ wtgain  : int  29 28 11 30 26 14 21 21 41 24 ...

``` r
view(birthweight_df)
```

There are 4342 observations and 20 variables. Those variables are
combinations of integer and numerical formats. I will dnow drop missing
values.

``` r
birthweight_na_df = drop_na(birthweight_df)
str(birthweight_na_df)
```

    ## 'data.frame':    4342 obs. of  20 variables:
    ##  $ babysex : int  2 1 2 1 2 1 2 2 1 1 ...
    ##  $ bhead   : int  34 34 36 34 34 33 33 33 36 33 ...
    ##  $ blength : int  51 48 50 52 52 52 46 49 52 50 ...
    ##  $ bwt     : int  3629 3062 3345 3062 3374 3374 2523 2778 3515 3459 ...
    ##  $ delwt   : int  177 156 148 157 156 129 126 140 146 169 ...
    ##  $ fincome : int  35 65 85 55 5 55 96 5 85 75 ...
    ##  $ frace   : int  1 2 1 1 1 1 2 1 1 2 ...
    ##  $ gaweeks : num  39.9 25.9 39.9 40 41.6 ...
    ##  $ malform : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ menarche: int  13 14 12 14 13 12 14 12 11 12 ...
    ##  $ mheight : int  63 65 64 64 66 66 72 62 61 64 ...
    ##  $ momage  : int  36 25 29 18 20 23 29 19 13 19 ...
    ##  $ mrace   : int  1 2 1 1 1 1 2 1 1 2 ...
    ##  $ parity  : int  3 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumlbw : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumsga : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ppbmi   : num  26.3 21.3 23.6 21.8 21 ...
    ##  $ ppwt    : int  148 128 137 127 130 115 105 119 105 145 ...
    ##  $ smoken  : num  0 0 1 10 1 0 0 0 0 4 ...
    ##  $ wtgain  : int  29 28 11 30 26 14 21 21 41 24 ...

There were no missing values. Therefore, it is appropriate to use the
original dataframe. I will make a facetwrap that shows a histogram of
each variable

``` r
ggplot(gather(birthweight_df), aes(value)) + geom_histogram(bins = 10) + facet_wrap(~key, scales = 'free_x')
```

![](hw6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> After looking at
these plots, I intend to convert babysex, frace, mrace, malform, and
mrace into factor variables.

``` r
birthweight_factor_df = birthweight_df %>% mutate (babysex = as.factor(babysex), frace = as.factor(frace), mrace = as.factor(mrace), malform = as.factor(malform), mrace = as.factor(mrace))
str(birthweight_factor_df)
```

    ## 'data.frame':    4342 obs. of  20 variables:
    ##  $ babysex : Factor w/ 2 levels "1","2": 2 1 2 1 2 1 2 2 1 1 ...
    ##  $ bhead   : int  34 34 36 34 34 33 33 33 36 33 ...
    ##  $ blength : int  51 48 50 52 52 52 46 49 52 50 ...
    ##  $ bwt     : int  3629 3062 3345 3062 3374 3374 2523 2778 3515 3459 ...
    ##  $ delwt   : int  177 156 148 157 156 129 126 140 146 169 ...
    ##  $ fincome : int  35 65 85 55 5 55 96 5 85 75 ...
    ##  $ frace   : Factor w/ 5 levels "1","2","3","4",..: 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ gaweeks : num  39.9 25.9 39.9 40 41.6 ...
    ##  $ malform : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ menarche: int  13 14 12 14 13 12 14 12 11 12 ...
    ##  $ mheight : int  63 65 64 64 66 66 72 62 61 64 ...
    ##  $ momage  : int  36 25 29 18 20 23 29 19 13 19 ...
    ##  $ mrace   : Factor w/ 4 levels "1","2","3","4": 1 2 1 1 1 1 2 1 1 2 ...
    ##  $ parity  : int  3 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumlbw : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pnumsga : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ppbmi   : num  26.3 21.3 23.6 21.8 21 ...
    ##  $ ppwt    : int  148 128 137 127 130 115 105 119 105 145 ...
    ##  $ smoken  : num  0 0 1 10 1 0 0 0 0 4 ...
    ##  $ wtgain  : int  29 28 11 30 26 14 21 21 41 24 ...

I will make scatter plots for continuous variables that may be assocaite
with birthweight.

``` r
birthweight_factor_df %>% ggplot(aes(x = bhead, y= bwt)) + geom_point()  ##On inspection, there is a corrleation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = blength, y= bwt)) + geom_point() ##On inspection, there is a corrrelation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = delwt, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = gaweeks, y= bwt)) + geom_point() ##There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = menarche, y= bwt)) + geom_point()#There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = fincome, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = mheight, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = momage, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = ppbmi, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = ppwt, y= bwt)) + geom_point()#There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = smoken, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-11.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = wtgain, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-12.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = parity, y= bwt)) + geom_point() #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-6-13.png)<!-- --> Now I will plot
brithweight by non-continuous variables

``` r
birthweight_factor_df %>% ggplot(aes(x = babysex, y= bwt)) + geom_boxplot()   #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = frace, y= bwt)) + geom_boxplot()   #There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = mrace, y= bwt)) + geom_boxplot() #There is not a strong correlation 
```

![](hw6_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
birthweight_factor_df %>% ggplot(aes(x = malform, y= bwt)) + geom_boxplot() # There is not a strong correlation
```

![](hw6_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
#pnumlbw and pnumsga are exlusively zero values.
```

On the basis of this exploratory data analysis, a baby’s head
circumference and length at birth have the strongest correlation with
birthweight. I will map the residuals and fitted values of these models.

``` r
fit = lm(bwt ~ bhead + blength, data = birthweight_factor_df)
birthweight_factor_df %>% modelr::add_residuals(fit) %>% ggplot(aes(x=bhead, y = resid)) + geom_point() ##Overall, there is a uniform distribution of residuals
```

![](hw6_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
birthweight_factor_df %>% modelr::add_residuals(fit) %>% ggplot(aes(x=blength, y = resid)) + geom_point() ##Overall, there is a uniform distribution of residuals
```

![](hw6_files/figure-gfm/unnamed-chunk-8-2.png)<!-- --> Now I will plot
the fitted values

``` r
fit = lm(bwt ~ bhead + blength, data = birthweight_factor_df)
birthweight_factor_df %>% modelr::add_predictions(fit) %>% ggplot(aes(x=bhead, y = pred)) + geom_point() ##Overall, there is a direct correlation between baby's head circumference at birth and fitted values
```

![](hw6_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
birthweight_factor_df %>% modelr::add_predictions(fit) %>% ggplot(aes(x=blength, y = pred)) + geom_point() ##Overall, there is a direct correlation betwen baby's length at birth and fitted values.
```

![](hw6_files/figure-gfm/unnamed-chunk-9-2.png)<!-- --> Now I will
correlate the fitted values with residuals

``` r
fit = lm(bwt ~ bhead + blength, data = birthweight_factor_df)
birthweight_factor_df  %>% modelr::add_residuals(fit)%>% modelr::add_predictions(fit) %>% ggplot(aes(x=pred, y = resid)) + geom_point() +    labs(
    title = "Residuals vs. Fitted Values",
    x = "Fitted Values",
    y = "Residuals",
    caption = "Birthweight Data"
  )  + theme_minimal() #Overall, there is not a strong correlation between fitted values and residuals
```

![](hw6_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Now I will fit
more models. In order to make the model with baby sex run, the input
needs to be numeric and not a factor. Also, I will need to split the
data frame into training and testing data.

``` r
birthweight_df = mutate(birthweight_df, id = row_number())
view(birthweight_df)
train_df = sample_n(birthweight_df, 80)
test_df = anti_join(birthweight_df, train_df, by = 'id')
```

Fit the linear models with the training data

``` r
fit = lm(bwt ~ bhead + blength, data = birthweight_df)
fit_length_age = lm(bwt ~ blength + gaweeks, data = birthweight_df)
fit_three = lm (bwt ~ bhead + blength + babysex + bhead*blength*babysex, data = birthweight_df)
```

Now I will add model performance

``` r
train_df %>% 
  gather_predictions(fit, fit_length_age, fit_three) %>% 
  mutate(model = fct_inorder(model))
```

    ##              model babysex bhead blength  bwt delwt fincome frace gaweeks
    ## 1              fit       2    34      49 3742   190      75     1    40.4
    ## 2              fit       1    34      53 3600   141      15     1    44.6
    ## 3              fit       2    34      50 2750   163      96     2    38.1
    ## 4              fit       1    35      50 3232   136      95     1    41.6
    ## 5              fit       2    32      47 2608   115      25     4    32.0
    ## 6              fit       2    32      52 2948   151      15     2    40.6
    ## 7              fit       2    33      50 3005   158      35     4    40.6
    ## 8              fit       2    33      52 3005   156      25     2    41.6
    ## 9              fit       1    34      50 3600   141      96     1    42.1
    ## 10             fit       1    35      50 3487   172      45     1    43.0
    ## 11             fit       1    32      47 2495   162       5     2    36.6
    ## 12             fit       1    34      52 3629   129      25     4    35.1
    ## 13             fit       1    32      49 2580   136      55     1    39.9
    ## 14             fit       1    32      44 2892   117      96     2    38.3
    ## 15             fit       1    34      51 3175   148      25     1    43.4
    ## 16             fit       2    33      47 2608   117      96     2    33.4
    ## 17             fit       1    33      52 3402   144      35     2    40.7
    ## 18             fit       2    33      48 3090   127      96     1    41.3
    ## 19             fit       1    32      48 2665   199       5     1    39.7
    ## 20             fit       2    30      49 2920   180      25     1    41.3
    ## 21             fit       1    37      53 3969   162      45     2    40.7
    ## 22             fit       2    33      49 3005   151      35     2    38.9
    ## 23             fit       1    32      50 3118   175      15     2    41.0
    ## 24             fit       1    32      44 1616   122      25     1    36.3
    ## 25             fit       1    33      48 3175   139      55     1    39.4
    ## 26             fit       2    35      52 3799   159      55     1    39.4
    ## 27             fit       1    33      48 2835   150      45     2    37.0
    ## 28             fit       2    34      49 3033   113      25     2    39.9
    ## 29             fit       2    36      54 4054   149      15     2    36.9
    ## 30             fit       2    34      51 3374   146      15     2    36.3
    ## 31             fit       2    36      49 3487   172      35     2    40.7
    ## 32             fit       1    35      52 2778    97      45     2    28.4
    ## 33             fit       1    34      51 2722   135      25     1    37.7
    ## 34             fit       1    34      48 2551   156      35     1    35.9
    ## 35             fit       1    35      51 3742   146      25     1    39.9
    ## 36             fit       2    34      50 3033   129      35     2    45.3
    ## 37             fit       1    34      51 2920   146      25     2    36.3
    ## 38             fit       1    35      50 3033   158      35     2    40.7
    ## 39             fit       1    35      55 3345   170      25     2    38.7
    ## 40             fit       1    31      49 2353   153      15     2    37.6
    ## 41             fit       2    34      51 3203   139      35     2    40.4
    ## 42             fit       2    34      50 3685   158      35     2    41.4
    ## 43             fit       2    34      48 2807   135      15     1    39.7
    ## 44             fit       1    35      53 3827   146      35     1    42.6
    ## 45             fit       1    35      48 2778   125      96     2    39.3
    ## 46             fit       1    36      51 3799   139      35     4    40.6
    ## 47             fit       2    34      47 2523   163      15     2    36.6
    ## 48             fit       2    34      49 3062   126      25     2    43.0
    ## 49             fit       2    34      53 3487   144      85     1    42.7
    ## 50             fit       2    32      49 2722   150      25     2    35.4
    ## 51             fit       1    34      50 3317   166      45     3    37.0
    ## 52             fit       1    36      52 3629   165      75     2    40.9
    ## 53             fit       2    36      51 3402   134      65     1    41.0
    ## 54             fit       1    36      52 3685   174      95     1    41.3
    ## 55             fit       2    32      47 2608   140      35     1    40.4
    ## 56             fit       1    35      51 3289   141      45     2    42.7
    ## 57             fit       1    34      46 2948   156      85     1    36.9
    ## 58             fit       1    32      51 2750   130      35     2    37.9
    ## 59             fit       1    34      49 2977   115      35     1    40.4
    ## 60             fit       2    32      51 2977   133      15     2    40.4
    ## 61             fit       2    33      51 3685   123      96     1    39.1
    ## 62             fit       1    34      51 3799   134      65     1    40.6
    ## 63             fit       2    32      48 2977   130      85     1    39.4
    ## 64             fit       2    28      38 1247   166      55     2    32.7
    ## 65             fit       1    35      48 2863   133      25     2    38.7
    ## 66             fit       2    36      53 3884   149      65     1    39.4
    ## 67             fit       1    33      49 3005   173      75     2    37.7
    ## 68             fit       1    35      49 3090   157      25     2    39.4
    ## 69             fit       2    33      51 3175   150      35     2    38.9
    ## 70             fit       1    34      49 3430   152      75     1    39.7
    ## 71             fit       2    35      46 3629   143      15     1    40.9
    ## 72             fit       1    34      48 3232   215      65     2    36.0
    ## 73             fit       2    32      49 2637   145      45     1    39.4
    ## 74             fit       1    33      49 2920   164      25     1    43.4
    ## 75             fit       1    34      47 2920   147      85     1    37.4
    ## 76             fit       2    35      50 3232   126      45     2    38.9
    ## 77             fit       2    33      50 2778   160      25     2    41.4
    ## 78             fit       1    33      49 2977   128      65     1    40.9
    ## 79             fit       1    35      49 2948   155      15     2    41.0
    ## 80             fit       1    34      49 3175   144      25     2    39.3
    ## 81  fit_length_age       2    34      49 3742   190      75     1    40.4
    ## 82  fit_length_age       1    34      53 3600   141      15     1    44.6
    ## 83  fit_length_age       2    34      50 2750   163      96     2    38.1
    ## 84  fit_length_age       1    35      50 3232   136      95     1    41.6
    ## 85  fit_length_age       2    32      47 2608   115      25     4    32.0
    ## 86  fit_length_age       2    32      52 2948   151      15     2    40.6
    ## 87  fit_length_age       2    33      50 3005   158      35     4    40.6
    ## 88  fit_length_age       2    33      52 3005   156      25     2    41.6
    ## 89  fit_length_age       1    34      50 3600   141      96     1    42.1
    ## 90  fit_length_age       1    35      50 3487   172      45     1    43.0
    ## 91  fit_length_age       1    32      47 2495   162       5     2    36.6
    ## 92  fit_length_age       1    34      52 3629   129      25     4    35.1
    ## 93  fit_length_age       1    32      49 2580   136      55     1    39.9
    ## 94  fit_length_age       1    32      44 2892   117      96     2    38.3
    ## 95  fit_length_age       1    34      51 3175   148      25     1    43.4
    ## 96  fit_length_age       2    33      47 2608   117      96     2    33.4
    ## 97  fit_length_age       1    33      52 3402   144      35     2    40.7
    ## 98  fit_length_age       2    33      48 3090   127      96     1    41.3
    ## 99  fit_length_age       1    32      48 2665   199       5     1    39.7
    ## 100 fit_length_age       2    30      49 2920   180      25     1    41.3
    ## 101 fit_length_age       1    37      53 3969   162      45     2    40.7
    ## 102 fit_length_age       2    33      49 3005   151      35     2    38.9
    ## 103 fit_length_age       1    32      50 3118   175      15     2    41.0
    ## 104 fit_length_age       1    32      44 1616   122      25     1    36.3
    ## 105 fit_length_age       1    33      48 3175   139      55     1    39.4
    ## 106 fit_length_age       2    35      52 3799   159      55     1    39.4
    ## 107 fit_length_age       1    33      48 2835   150      45     2    37.0
    ## 108 fit_length_age       2    34      49 3033   113      25     2    39.9
    ## 109 fit_length_age       2    36      54 4054   149      15     2    36.9
    ## 110 fit_length_age       2    34      51 3374   146      15     2    36.3
    ## 111 fit_length_age       2    36      49 3487   172      35     2    40.7
    ## 112 fit_length_age       1    35      52 2778    97      45     2    28.4
    ## 113 fit_length_age       1    34      51 2722   135      25     1    37.7
    ## 114 fit_length_age       1    34      48 2551   156      35     1    35.9
    ## 115 fit_length_age       1    35      51 3742   146      25     1    39.9
    ## 116 fit_length_age       2    34      50 3033   129      35     2    45.3
    ## 117 fit_length_age       1    34      51 2920   146      25     2    36.3
    ## 118 fit_length_age       1    35      50 3033   158      35     2    40.7
    ## 119 fit_length_age       1    35      55 3345   170      25     2    38.7
    ## 120 fit_length_age       1    31      49 2353   153      15     2    37.6
    ## 121 fit_length_age       2    34      51 3203   139      35     2    40.4
    ## 122 fit_length_age       2    34      50 3685   158      35     2    41.4
    ## 123 fit_length_age       2    34      48 2807   135      15     1    39.7
    ## 124 fit_length_age       1    35      53 3827   146      35     1    42.6
    ## 125 fit_length_age       1    35      48 2778   125      96     2    39.3
    ## 126 fit_length_age       1    36      51 3799   139      35     4    40.6
    ## 127 fit_length_age       2    34      47 2523   163      15     2    36.6
    ## 128 fit_length_age       2    34      49 3062   126      25     2    43.0
    ## 129 fit_length_age       2    34      53 3487   144      85     1    42.7
    ## 130 fit_length_age       2    32      49 2722   150      25     2    35.4
    ## 131 fit_length_age       1    34      50 3317   166      45     3    37.0
    ## 132 fit_length_age       1    36      52 3629   165      75     2    40.9
    ## 133 fit_length_age       2    36      51 3402   134      65     1    41.0
    ## 134 fit_length_age       1    36      52 3685   174      95     1    41.3
    ## 135 fit_length_age       2    32      47 2608   140      35     1    40.4
    ## 136 fit_length_age       1    35      51 3289   141      45     2    42.7
    ## 137 fit_length_age       1    34      46 2948   156      85     1    36.9
    ## 138 fit_length_age       1    32      51 2750   130      35     2    37.9
    ## 139 fit_length_age       1    34      49 2977   115      35     1    40.4
    ## 140 fit_length_age       2    32      51 2977   133      15     2    40.4
    ## 141 fit_length_age       2    33      51 3685   123      96     1    39.1
    ## 142 fit_length_age       1    34      51 3799   134      65     1    40.6
    ## 143 fit_length_age       2    32      48 2977   130      85     1    39.4
    ## 144 fit_length_age       2    28      38 1247   166      55     2    32.7
    ## 145 fit_length_age       1    35      48 2863   133      25     2    38.7
    ## 146 fit_length_age       2    36      53 3884   149      65     1    39.4
    ## 147 fit_length_age       1    33      49 3005   173      75     2    37.7
    ## 148 fit_length_age       1    35      49 3090   157      25     2    39.4
    ## 149 fit_length_age       2    33      51 3175   150      35     2    38.9
    ## 150 fit_length_age       1    34      49 3430   152      75     1    39.7
    ## 151 fit_length_age       2    35      46 3629   143      15     1    40.9
    ## 152 fit_length_age       1    34      48 3232   215      65     2    36.0
    ## 153 fit_length_age       2    32      49 2637   145      45     1    39.4
    ## 154 fit_length_age       1    33      49 2920   164      25     1    43.4
    ## 155 fit_length_age       1    34      47 2920   147      85     1    37.4
    ## 156 fit_length_age       2    35      50 3232   126      45     2    38.9
    ## 157 fit_length_age       2    33      50 2778   160      25     2    41.4
    ## 158 fit_length_age       1    33      49 2977   128      65     1    40.9
    ## 159 fit_length_age       1    35      49 2948   155      15     2    41.0
    ## 160 fit_length_age       1    34      49 3175   144      25     2    39.3
    ## 161      fit_three       2    34      49 3742   190      75     1    40.4
    ## 162      fit_three       1    34      53 3600   141      15     1    44.6
    ## 163      fit_three       2    34      50 2750   163      96     2    38.1
    ## 164      fit_three       1    35      50 3232   136      95     1    41.6
    ## 165      fit_three       2    32      47 2608   115      25     4    32.0
    ## 166      fit_three       2    32      52 2948   151      15     2    40.6
    ## 167      fit_three       2    33      50 3005   158      35     4    40.6
    ## 168      fit_three       2    33      52 3005   156      25     2    41.6
    ## 169      fit_three       1    34      50 3600   141      96     1    42.1
    ## 170      fit_three       1    35      50 3487   172      45     1    43.0
    ## 171      fit_three       1    32      47 2495   162       5     2    36.6
    ## 172      fit_three       1    34      52 3629   129      25     4    35.1
    ## 173      fit_three       1    32      49 2580   136      55     1    39.9
    ## 174      fit_three       1    32      44 2892   117      96     2    38.3
    ## 175      fit_three       1    34      51 3175   148      25     1    43.4
    ## 176      fit_three       2    33      47 2608   117      96     2    33.4
    ## 177      fit_three       1    33      52 3402   144      35     2    40.7
    ## 178      fit_three       2    33      48 3090   127      96     1    41.3
    ## 179      fit_three       1    32      48 2665   199       5     1    39.7
    ## 180      fit_three       2    30      49 2920   180      25     1    41.3
    ## 181      fit_three       1    37      53 3969   162      45     2    40.7
    ## 182      fit_three       2    33      49 3005   151      35     2    38.9
    ## 183      fit_three       1    32      50 3118   175      15     2    41.0
    ## 184      fit_three       1    32      44 1616   122      25     1    36.3
    ## 185      fit_three       1    33      48 3175   139      55     1    39.4
    ## 186      fit_three       2    35      52 3799   159      55     1    39.4
    ## 187      fit_three       1    33      48 2835   150      45     2    37.0
    ## 188      fit_three       2    34      49 3033   113      25     2    39.9
    ## 189      fit_three       2    36      54 4054   149      15     2    36.9
    ## 190      fit_three       2    34      51 3374   146      15     2    36.3
    ## 191      fit_three       2    36      49 3487   172      35     2    40.7
    ## 192      fit_three       1    35      52 2778    97      45     2    28.4
    ## 193      fit_three       1    34      51 2722   135      25     1    37.7
    ## 194      fit_three       1    34      48 2551   156      35     1    35.9
    ## 195      fit_three       1    35      51 3742   146      25     1    39.9
    ## 196      fit_three       2    34      50 3033   129      35     2    45.3
    ## 197      fit_three       1    34      51 2920   146      25     2    36.3
    ## 198      fit_three       1    35      50 3033   158      35     2    40.7
    ## 199      fit_three       1    35      55 3345   170      25     2    38.7
    ## 200      fit_three       1    31      49 2353   153      15     2    37.6
    ## 201      fit_three       2    34      51 3203   139      35     2    40.4
    ## 202      fit_three       2    34      50 3685   158      35     2    41.4
    ## 203      fit_three       2    34      48 2807   135      15     1    39.7
    ## 204      fit_three       1    35      53 3827   146      35     1    42.6
    ## 205      fit_three       1    35      48 2778   125      96     2    39.3
    ## 206      fit_three       1    36      51 3799   139      35     4    40.6
    ## 207      fit_three       2    34      47 2523   163      15     2    36.6
    ## 208      fit_three       2    34      49 3062   126      25     2    43.0
    ## 209      fit_three       2    34      53 3487   144      85     1    42.7
    ## 210      fit_three       2    32      49 2722   150      25     2    35.4
    ## 211      fit_three       1    34      50 3317   166      45     3    37.0
    ## 212      fit_three       1    36      52 3629   165      75     2    40.9
    ## 213      fit_three       2    36      51 3402   134      65     1    41.0
    ## 214      fit_three       1    36      52 3685   174      95     1    41.3
    ## 215      fit_three       2    32      47 2608   140      35     1    40.4
    ## 216      fit_three       1    35      51 3289   141      45     2    42.7
    ## 217      fit_three       1    34      46 2948   156      85     1    36.9
    ## 218      fit_three       1    32      51 2750   130      35     2    37.9
    ## 219      fit_three       1    34      49 2977   115      35     1    40.4
    ## 220      fit_three       2    32      51 2977   133      15     2    40.4
    ## 221      fit_three       2    33      51 3685   123      96     1    39.1
    ## 222      fit_three       1    34      51 3799   134      65     1    40.6
    ## 223      fit_three       2    32      48 2977   130      85     1    39.4
    ## 224      fit_three       2    28      38 1247   166      55     2    32.7
    ## 225      fit_three       1    35      48 2863   133      25     2    38.7
    ## 226      fit_three       2    36      53 3884   149      65     1    39.4
    ## 227      fit_three       1    33      49 3005   173      75     2    37.7
    ## 228      fit_three       1    35      49 3090   157      25     2    39.4
    ## 229      fit_three       2    33      51 3175   150      35     2    38.9
    ## 230      fit_three       1    34      49 3430   152      75     1    39.7
    ## 231      fit_three       2    35      46 3629   143      15     1    40.9
    ## 232      fit_three       1    34      48 3232   215      65     2    36.0
    ## 233      fit_three       2    32      49 2637   145      45     1    39.4
    ## 234      fit_three       1    33      49 2920   164      25     1    43.4
    ## 235      fit_three       1    34      47 2920   147      85     1    37.4
    ## 236      fit_three       2    35      50 3232   126      45     2    38.9
    ## 237      fit_three       2    33      50 2778   160      25     2    41.4
    ## 238      fit_three       1    33      49 2977   128      65     1    40.9
    ## 239      fit_three       1    35      49 2948   155      15     2    41.0
    ## 240      fit_three       1    34      49 3175   144      25     2    39.3
    ##     malform menarche mheight momage mrace parity pnumlbw pnumsga    ppbmi ppwt
    ## 1         0       10      64     23     1      0       0       0 25.80128  150
    ## 2         0       12      62     21     1      0       0       0 21.26104  116
    ## 3         0       13      67     19     2      0       0       0 22.75770  145
    ## 4         0       14      65     19     1      0       0       0 18.34323  110
    ## 5         0       14      58     24     4      0       0       0 19.68710   94
    ## 6         0       13      64     23     2      0       0       0 21.84508  127
    ## 7         0       16      63     25     1      0       0       0 21.30149  120
    ## 8         0       11      59     19     2      0       0       0 25.29973  125
    ## 9         0       14      68     23     1      0       0       0 19.50303  128
    ## 10        0       15      65     21     1      0       0       0 25.01350  150
    ## 11        0       11      69     20     2      0       0       0 19.23779  130
    ## 12        0       11      63     17     4      0       0       0 20.59144  116
    ## 13        0       14      61     23     1      0       0       0 22.15318  117
    ## 14        0       12      61     17     2      0       0       0 17.98763   95
    ## 15        0       15      62     20     1      0       0       0 22.91060  125
    ## 16        0       13      63     19     2      0       0       0 19.52637  110
    ## 17        0       13      64     27     2      0       0       0 18.23290  106
    ## 18        0       13      61     20     1      0       0       0 20.63843  109
    ## 19        0       11      64     22     1      0       0       0 30.10149  175
    ## 20        0       12      63     17     1      0       0       0 26.62687  150
    ## 21        0       12      66     19     2      0       0       0 19.40900  120
    ## 22        0       13      68     17     2      0       0       0 20.56960  135
    ## 23        0        9      67     18     2      0       0       0 22.28685  142
    ## 24        0       13      66     18     1      0       0       0 16.17417  100
    ## 25        0       12      60     22     1      0       0       0 23.48490  120
    ## 26        0       13      64     22     1      0       0       0 24.08119  140
    ## 27        0       11      66     23     2      0       0       0 23.45255  145
    ## 28        0       13      57     18     2      0       0       0 19.51653   90
    ## 29        0       14      65     24     2      0       0       0 22.51215  135
    ## 30        0       13      64     24     2      0       0       0 23.22115  135
    ## 31        0       13      63     22     2      0       0       0 23.60915  133
    ## 32        0       12      61     15     2      0       0       0 16.09419   85
    ## 33        0       10      63     22     1      0       0       0 22.18905  125
    ## 34        0       13      64     25     1      0       0       0 24.08119  140
    ## 35        0       13      61     20     1      0       0       0 23.66793  125
    ## 36        0       13      63     15     2      0       0       0 17.75124  100
    ## 37        0       13      65     24     2      0       0       0 20.84458  125
    ## 38        0       13      67     23     2      0       0       0 22.91464  146
    ## 39        0       14      63     20     2      0       0       0 23.07662  130
    ## 40        0       11      61     22     2      0       0       0 23.09990  122
    ## 41        0       13      66     23     2      0       0       0 17.30636  107
    ## 42        0        9      63     20     2      0       0       0 23.96418  135
    ## 43        0       12      64     19     1      0       0       0 19.78098  115
    ## 44        0       11      62     23     1      0       0       0 21.99418  120
    ## 45        0       12      60     22     2      0       0       0 21.52782  110
    ## 46        0       12      62     15     4      0       0       0 21.07775  115
    ## 47        0       12      68     19     2      0       0       0 22.55037  148
    ## 48        0       12      61     16     2      0       0       0 19.69172  104
    ## 49        0       15      57     22     1      0       0       0 23.85354  110
    ## 50        0       12      64     15     2      0       0       0 20.29700  118
    ## 51        0       13      67     26     1      0       0       0 21.50210  137
    ## 52        0       11      61     22     2      0       0       0 26.88677  142
    ## 53        0       12      62     21     1      0       0       0 18.87834  103
    ## 54        0       14      67     20     1      0       0       0 21.18820  135
    ## 55        0       11      64     22     1      0       0       0 18.23290  106
    ## 56        0       14      63     20     2      0       0       0 19.52637  110
    ## 57        0       14      63     24     1      0       0       0 23.96418  135
    ## 58        0       14      63     17     2      0       0       0 19.52637  110
    ## 59        0       13      59     22     1      0       0       0 19.22779   95
    ## 60        0       12      60     19     2      0       0       0 23.48490  120
    ## 61        0       14      64     21     1      0       0       0 17.71688  103
    ## 62        0       11      61     19     1      0       0       0 19.31303  102
    ## 63        0       12      66     24     1      0       0       0 18.60030  115
    ## 64        0       13      66     21     2      0       0       0 25.87867  160
    ## 65        0       15      64     23     2      0       0       0 21.32906  124
    ## 66        0       11      63     21     1      0       0       0 22.18905  125
    ## 67        0        9      59     21     2      0       0       0 29.14529  144
    ## 68        0       12      64     23     2      0       0       0 23.04914  134
    ## 69        0       12      63     16     2      0       0       0 22.18905  125
    ## 70        0       11      66     22     1      0       0       0 21.02642  130
    ## 71        0       13      58     15     1      0       0       0 27.22684  130
    ## 72        0       12      68     17     2      0       0       0 27.73087  182
    ## 73        0       13      62     20     1      0       0       0 21.99418  120
    ## 74        0       11      64     18     1      0       0       0 24.08119  140
    ## 75        0       11      62     24     1      0       0       0 21.99418  120
    ## 76        0       13      67     18     2      0       0       0 17.26446  110
    ## 77        0       13      64     16     2      0       0       0 25.11324  146
    ## 78        0       16      60     21     1      0       0       0 19.17933   98
    ## 79        0       11      64     15     2      0       0       0 21.50106  125
    ## 80        0       13      64     17     2      0       0       0 21.84508  127
    ## 81        0       10      64     23     1      0       0       0 25.80128  150
    ## 82        0       12      62     21     1      0       0       0 21.26104  116
    ## 83        0       13      67     19     2      0       0       0 22.75770  145
    ## 84        0       14      65     19     1      0       0       0 18.34323  110
    ## 85        0       14      58     24     4      0       0       0 19.68710   94
    ## 86        0       13      64     23     2      0       0       0 21.84508  127
    ## 87        0       16      63     25     1      0       0       0 21.30149  120
    ## 88        0       11      59     19     2      0       0       0 25.29973  125
    ## 89        0       14      68     23     1      0       0       0 19.50303  128
    ## 90        0       15      65     21     1      0       0       0 25.01350  150
    ## 91        0       11      69     20     2      0       0       0 19.23779  130
    ## 92        0       11      63     17     4      0       0       0 20.59144  116
    ## 93        0       14      61     23     1      0       0       0 22.15318  117
    ## 94        0       12      61     17     2      0       0       0 17.98763   95
    ## 95        0       15      62     20     1      0       0       0 22.91060  125
    ## 96        0       13      63     19     2      0       0       0 19.52637  110
    ## 97        0       13      64     27     2      0       0       0 18.23290  106
    ## 98        0       13      61     20     1      0       0       0 20.63843  109
    ## 99        0       11      64     22     1      0       0       0 30.10149  175
    ## 100       0       12      63     17     1      0       0       0 26.62687  150
    ## 101       0       12      66     19     2      0       0       0 19.40900  120
    ## 102       0       13      68     17     2      0       0       0 20.56960  135
    ## 103       0        9      67     18     2      0       0       0 22.28685  142
    ## 104       0       13      66     18     1      0       0       0 16.17417  100
    ## 105       0       12      60     22     1      0       0       0 23.48490  120
    ## 106       0       13      64     22     1      0       0       0 24.08119  140
    ## 107       0       11      66     23     2      0       0       0 23.45255  145
    ## 108       0       13      57     18     2      0       0       0 19.51653   90
    ## 109       0       14      65     24     2      0       0       0 22.51215  135
    ## 110       0       13      64     24     2      0       0       0 23.22115  135
    ## 111       0       13      63     22     2      0       0       0 23.60915  133
    ## 112       0       12      61     15     2      0       0       0 16.09419   85
    ## 113       0       10      63     22     1      0       0       0 22.18905  125
    ## 114       0       13      64     25     1      0       0       0 24.08119  140
    ## 115       0       13      61     20     1      0       0       0 23.66793  125
    ## 116       0       13      63     15     2      0       0       0 17.75124  100
    ## 117       0       13      65     24     2      0       0       0 20.84458  125
    ## 118       0       13      67     23     2      0       0       0 22.91464  146
    ## 119       0       14      63     20     2      0       0       0 23.07662  130
    ## 120       0       11      61     22     2      0       0       0 23.09990  122
    ## 121       0       13      66     23     2      0       0       0 17.30636  107
    ## 122       0        9      63     20     2      0       0       0 23.96418  135
    ## 123       0       12      64     19     1      0       0       0 19.78098  115
    ## 124       0       11      62     23     1      0       0       0 21.99418  120
    ## 125       0       12      60     22     2      0       0       0 21.52782  110
    ## 126       0       12      62     15     4      0       0       0 21.07775  115
    ## 127       0       12      68     19     2      0       0       0 22.55037  148
    ## 128       0       12      61     16     2      0       0       0 19.69172  104
    ## 129       0       15      57     22     1      0       0       0 23.85354  110
    ## 130       0       12      64     15     2      0       0       0 20.29700  118
    ## 131       0       13      67     26     1      0       0       0 21.50210  137
    ## 132       0       11      61     22     2      0       0       0 26.88677  142
    ## 133       0       12      62     21     1      0       0       0 18.87834  103
    ## 134       0       14      67     20     1      0       0       0 21.18820  135
    ## 135       0       11      64     22     1      0       0       0 18.23290  106
    ## 136       0       14      63     20     2      0       0       0 19.52637  110
    ## 137       0       14      63     24     1      0       0       0 23.96418  135
    ## 138       0       14      63     17     2      0       0       0 19.52637  110
    ## 139       0       13      59     22     1      0       0       0 19.22779   95
    ## 140       0       12      60     19     2      0       0       0 23.48490  120
    ## 141       0       14      64     21     1      0       0       0 17.71688  103
    ## 142       0       11      61     19     1      0       0       0 19.31303  102
    ## 143       0       12      66     24     1      0       0       0 18.60030  115
    ## 144       0       13      66     21     2      0       0       0 25.87867  160
    ## 145       0       15      64     23     2      0       0       0 21.32906  124
    ## 146       0       11      63     21     1      0       0       0 22.18905  125
    ## 147       0        9      59     21     2      0       0       0 29.14529  144
    ## 148       0       12      64     23     2      0       0       0 23.04914  134
    ## 149       0       12      63     16     2      0       0       0 22.18905  125
    ## 150       0       11      66     22     1      0       0       0 21.02642  130
    ## 151       0       13      58     15     1      0       0       0 27.22684  130
    ## 152       0       12      68     17     2      0       0       0 27.73087  182
    ## 153       0       13      62     20     1      0       0       0 21.99418  120
    ## 154       0       11      64     18     1      0       0       0 24.08119  140
    ## 155       0       11      62     24     1      0       0       0 21.99418  120
    ## 156       0       13      67     18     2      0       0       0 17.26446  110
    ## 157       0       13      64     16     2      0       0       0 25.11324  146
    ## 158       0       16      60     21     1      0       0       0 19.17933   98
    ## 159       0       11      64     15     2      0       0       0 21.50106  125
    ## 160       0       13      64     17     2      0       0       0 21.84508  127
    ## 161       0       10      64     23     1      0       0       0 25.80128  150
    ## 162       0       12      62     21     1      0       0       0 21.26104  116
    ## 163       0       13      67     19     2      0       0       0 22.75770  145
    ## 164       0       14      65     19     1      0       0       0 18.34323  110
    ## 165       0       14      58     24     4      0       0       0 19.68710   94
    ## 166       0       13      64     23     2      0       0       0 21.84508  127
    ## 167       0       16      63     25     1      0       0       0 21.30149  120
    ## 168       0       11      59     19     2      0       0       0 25.29973  125
    ## 169       0       14      68     23     1      0       0       0 19.50303  128
    ## 170       0       15      65     21     1      0       0       0 25.01350  150
    ## 171       0       11      69     20     2      0       0       0 19.23779  130
    ## 172       0       11      63     17     4      0       0       0 20.59144  116
    ## 173       0       14      61     23     1      0       0       0 22.15318  117
    ## 174       0       12      61     17     2      0       0       0 17.98763   95
    ## 175       0       15      62     20     1      0       0       0 22.91060  125
    ## 176       0       13      63     19     2      0       0       0 19.52637  110
    ## 177       0       13      64     27     2      0       0       0 18.23290  106
    ## 178       0       13      61     20     1      0       0       0 20.63843  109
    ## 179       0       11      64     22     1      0       0       0 30.10149  175
    ## 180       0       12      63     17     1      0       0       0 26.62687  150
    ## 181       0       12      66     19     2      0       0       0 19.40900  120
    ## 182       0       13      68     17     2      0       0       0 20.56960  135
    ## 183       0        9      67     18     2      0       0       0 22.28685  142
    ## 184       0       13      66     18     1      0       0       0 16.17417  100
    ## 185       0       12      60     22     1      0       0       0 23.48490  120
    ## 186       0       13      64     22     1      0       0       0 24.08119  140
    ## 187       0       11      66     23     2      0       0       0 23.45255  145
    ## 188       0       13      57     18     2      0       0       0 19.51653   90
    ## 189       0       14      65     24     2      0       0       0 22.51215  135
    ## 190       0       13      64     24     2      0       0       0 23.22115  135
    ## 191       0       13      63     22     2      0       0       0 23.60915  133
    ## 192       0       12      61     15     2      0       0       0 16.09419   85
    ## 193       0       10      63     22     1      0       0       0 22.18905  125
    ## 194       0       13      64     25     1      0       0       0 24.08119  140
    ## 195       0       13      61     20     1      0       0       0 23.66793  125
    ## 196       0       13      63     15     2      0       0       0 17.75124  100
    ## 197       0       13      65     24     2      0       0       0 20.84458  125
    ## 198       0       13      67     23     2      0       0       0 22.91464  146
    ## 199       0       14      63     20     2      0       0       0 23.07662  130
    ## 200       0       11      61     22     2      0       0       0 23.09990  122
    ## 201       0       13      66     23     2      0       0       0 17.30636  107
    ## 202       0        9      63     20     2      0       0       0 23.96418  135
    ## 203       0       12      64     19     1      0       0       0 19.78098  115
    ## 204       0       11      62     23     1      0       0       0 21.99418  120
    ## 205       0       12      60     22     2      0       0       0 21.52782  110
    ## 206       0       12      62     15     4      0       0       0 21.07775  115
    ## 207       0       12      68     19     2      0       0       0 22.55037  148
    ## 208       0       12      61     16     2      0       0       0 19.69172  104
    ## 209       0       15      57     22     1      0       0       0 23.85354  110
    ## 210       0       12      64     15     2      0       0       0 20.29700  118
    ## 211       0       13      67     26     1      0       0       0 21.50210  137
    ## 212       0       11      61     22     2      0       0       0 26.88677  142
    ## 213       0       12      62     21     1      0       0       0 18.87834  103
    ## 214       0       14      67     20     1      0       0       0 21.18820  135
    ## 215       0       11      64     22     1      0       0       0 18.23290  106
    ## 216       0       14      63     20     2      0       0       0 19.52637  110
    ## 217       0       14      63     24     1      0       0       0 23.96418  135
    ## 218       0       14      63     17     2      0       0       0 19.52637  110
    ## 219       0       13      59     22     1      0       0       0 19.22779   95
    ## 220       0       12      60     19     2      0       0       0 23.48490  120
    ## 221       0       14      64     21     1      0       0       0 17.71688  103
    ## 222       0       11      61     19     1      0       0       0 19.31303  102
    ## 223       0       12      66     24     1      0       0       0 18.60030  115
    ## 224       0       13      66     21     2      0       0       0 25.87867  160
    ## 225       0       15      64     23     2      0       0       0 21.32906  124
    ## 226       0       11      63     21     1      0       0       0 22.18905  125
    ## 227       0        9      59     21     2      0       0       0 29.14529  144
    ## 228       0       12      64     23     2      0       0       0 23.04914  134
    ## 229       0       12      63     16     2      0       0       0 22.18905  125
    ## 230       0       11      66     22     1      0       0       0 21.02642  130
    ## 231       0       13      58     15     1      0       0       0 27.22684  130
    ## 232       0       12      68     17     2      0       0       0 27.73087  182
    ## 233       0       13      62     20     1      0       0       0 21.99418  120
    ## 234       0       11      64     18     1      0       0       0 24.08119  140
    ## 235       0       11      62     24     1      0       0       0 21.99418  120
    ## 236       0       13      67     18     2      0       0       0 17.26446  110
    ## 237       0       13      64     16     2      0       0       0 25.11324  146
    ## 238       0       16      60     21     1      0       0       0 19.17933   98
    ## 239       0       11      64     15     2      0       0       0 21.50106  125
    ## 240       0       13      64     17     2      0       0       0 21.84508  127
    ##     smoken wtgain   id     pred
    ## 1     0.00     40 1206 3101.565
    ## 2     3.00     25 3736 3441.686
    ## 3     0.00     18  832 3186.595
    ## 4     0.00     26  516 3332.616
    ## 5     0.00     21 2651 2639.463
    ## 6     0.00     24 3368 3064.614
    ## 7     0.00     38 2791 3040.575
    ## 8     1.00     31 3196 3210.635
    ## 9     0.00     13 1022 3186.595
    ## 10    0.00     22 2406 3332.616
    ## 11   20.00     32 1519 2639.463
    ## 12    0.00     13 2784 3356.656
    ## 13   20.00     19  290 2809.524
    ## 14    3.00     22 2006 2384.373
    ## 15   20.00     23  328 3271.626
    ## 16    5.00      7  261 2785.484
    ## 17    0.00     38 3229 3210.635
    ## 18   10.00     18 2004 2870.514
    ## 19    6.00     24 2914 2724.494
    ## 20    6.00     30 1994 2517.482
    ## 21    0.00     42 4324 3879.748
    ## 22    0.00     16 1565 2955.544
    ## 23   20.00     33 1429 2894.554
    ## 24   30.00     22 3899 2384.373
    ## 25    0.00     19  745 2870.514
    ## 26    0.00     19 2268 3502.677
    ## 27    0.00      5 2662 2870.514
    ## 28    0.00     23 2965 3101.565
    ## 29    1.00     14 3109 3818.758
    ## 30   30.00     11 3307 3271.626
    ## 31    0.00     39 2659 3393.607
    ## 32    0.00     12 1542 3502.677
    ## 33   10.00     10  418 3271.626
    ## 34   20.00     16 2573 3016.535
    ## 35    0.00     21 3820 3417.646
    ## 36    0.75     29 1594 3186.595
    ## 37    0.00     21 3305 3271.626
    ## 38    3.00     12 1287 3332.616
    ## 39    0.00     40 2014 3757.767
    ## 40   20.00     31 3299 2663.503
    ## 41    0.00     32 2068 3271.626
    ## 42    0.00     23 1396 3186.595
    ## 43    8.00     20 2232 3016.535
    ## 44    0.00     26  262 3587.707
    ## 45    0.00     15 1704 3162.556
    ## 46    0.00     24 2462 3563.667
    ## 47    9.00     15 3574 2931.505
    ## 48    0.00     22 3579 3101.565
    ## 49    2.00     34  576 3441.686
    ## 50    0.00     32 4158 2809.524
    ## 51    0.00     29 2326 3186.595
    ## 52    0.00     23 1671 3648.697
    ## 53    0.00     31 4009 3563.667
    ## 54    0.00     39  645 3648.697
    ## 55   20.00     34 3759 2639.463
    ## 56    1.00     31 1657 3417.646
    ## 57    0.00     21 1258 2846.475
    ## 58    2.00     20 3371 2979.584
    ## 59    0.00     20  672 3101.565
    ## 60    0.00     13 3146 2979.584
    ## 61    0.00     20  688 3125.605
    ## 62    0.00     32 2375 3271.626
    ## 63    0.00     15 1226 2724.494
    ## 64   15.00      6 3097 1290.109
    ## 65    4.00      9 1416 3162.556
    ## 66    0.00     24 3657 3733.727
    ## 67    5.00     29 1702 2955.544
    ## 68   20.00     23 1486 3247.586
    ## 69    0.00     25 3358 3125.605
    ## 70    0.00     22  932 3101.565
    ## 71    0.00     13 2618 2992.495
    ## 72    0.00     33 1332 3016.535
    ## 73   30.00     25  485 2809.524
    ## 74    0.00     24 3442 2955.544
    ## 75    0.00     27 1190 2931.505
    ## 76    0.00     16 4236 3332.616
    ## 77    0.00     14 3391 3040.575
    ## 78   20.00     30 4002 2955.544
    ## 79    0.00     30 4221 3247.586
    ## 80    0.00     17 2000 3101.565
    ## 81    0.00     40 1206 3044.250
    ## 82    3.00     25 3736 3672.069
    ## 83    0.00     18  832 3110.598
    ## 84    0.00     26  516 3205.262
    ## 85    0.00     21 2651 2559.946
    ## 86    0.00     24 3368 3435.326
    ## 87    0.00     38 2791 3178.215
    ## 88    1.00     31 3196 3462.373
    ## 89    0.00     13 1022 3218.785
    ## 90    0.00     22 2406 3243.127
    ## 91   20.00     32 1519 2684.361
    ## 92    0.00     13 2784 3286.569
    ## 93   20.00     19  290 3030.726
    ## 94    3.00     22 2006 2344.673
    ## 95   20.00     23  328 3382.501
    ## 96    5.00      7  261 2597.811
    ## 97    0.00     38 3229 3438.031
    ## 98   10.00     18 2004 2940.036
    ## 99    6.00     24 2914 2896.761
    ## 100   6.00     30 1994 3068.592
    ## 101   0.00     42 4324 3566.587
    ## 102   0.00     16 1565 3003.680
    ## 103  20.00     33 1429 3189.034
    ## 104  30.00     22 3899 2290.580
    ## 105   0.00     19  745 2888.647
    ## 106   0.00     19 2268 3402.870
    ## 107   0.00      5 2662 2823.735
    ## 108   0.00     23 2965 3030.726
    ## 109   1.00     14 3109 3592.365
    ## 110  30.00     11 3307 3190.470
    ## 111   0.00     39 2659 3052.364
    ## 112   0.00     12 1542 3105.356
    ## 113  10.00     10  418 3228.335
    ## 114  20.00     16 2573 2793.984
    ## 115   0.00     21 3820 3287.838
    ## 116   0.75     29 1594 3305.334
    ## 117   0.00     21 3305 3190.470
    ## 118   3.00     12 1287 3180.920
    ## 119   0.00     40 2014 3769.605
    ## 120  20.00     31 3299 2968.519
    ## 121   0.00     32 2068 3301.361
    ## 122   0.00     23 1396 3199.852
    ## 123   8.00     20 2232 2896.761
    ## 124   0.00     26  262 3617.975
    ## 125   0.00     15 1704 2885.943
    ## 126   0.00     24 2462 3306.770
    ## 127   9.00     15 3574 2684.361
    ## 128   0.00     22 3579 3114.571
    ## 129   2.00     34  576 3620.680
    ## 130   0.00     32 4158 2909.016
    ## 131   0.00     29 2326 3080.847
    ## 132   0.00     23 1671 3443.440
    ## 133   0.00     31 4009 3317.589
    ## 134   0.00     39  645 3454.259
    ## 135  20.00     34 3759 2787.138
    ## 136   1.00     31 1657 3363.569
    ## 137   0.00     21 1258 2563.919
    ## 138   2.00     20 3371 3233.744
    ## 139   0.00     20  672 3044.250
    ## 140   0.00     13 3146 3301.361
    ## 141   0.00     20  688 3266.200
    ## 142   0.00     32 2375 3306.770
    ## 143   0.00     15 1226 2888.647
    ## 144  15.00      6 3097 1421.877
    ## 145   4.00      9 1416 2869.715
    ## 146   0.00     24 3657 3531.426
    ## 147   5.00     29 1702 2971.224
    ## 148  20.00     23 1486 3017.203
    ## 149   0.00     25 3358 3260.791
    ## 150   0.00     22  932 3025.317
    ## 151   0.00     13 2618 2672.106
    ## 152   0.00     33 1332 2796.688
    ## 153  30.00     25  485 3017.203
    ## 154   0.00     24 3442 3125.390
    ## 155   0.00     27 1190 2705.998
    ## 156   0.00     16 4236 3132.235
    ## 157   0.00     14 3391 3199.852
    ## 158  20.00     30 4002 3057.773
    ## 159   0.00     30 4221 3060.478
    ## 160   0.00     17 2000 3014.498
    ## 161   0.00     40 1206 3111.605
    ## 162   3.00     25 3736 3419.357
    ## 163   0.00     18  832 3202.991
    ## 164   0.00     26  516 3323.560
    ## 165   0.00     21 2651 2649.533
    ## 166   0.00     24 3368 3073.214
    ## 167   0.00     38 2791 3053.366
    ## 168   1.00     31 3196 3229.487
    ## 169   0.00     13 1022 3169.444
    ## 170   0.00     22 2406 3323.560
    ## 171  20.00     32 1519 2607.980
    ## 172   0.00     13 2784 3336.053
    ## 173  20.00     19  290 2776.803
    ## 174   3.00     22 2006 2354.746
    ## 175  20.00     23  328 3252.749
    ## 176   5.00      7  261 2789.184
    ## 177   0.00     38 3229 3183.045
    ## 178  10.00     18 2004 2877.245
    ## 179   6.00     24 2914 2692.391
    ## 180   6.00     30 1994 2526.405
    ## 181   0.00     42 4324 3876.720
    ## 182   0.00     16 1565 2965.305
    ## 183  20.00     33 1429 2861.214
    ## 184  30.00     22 3899 2354.746
    ## 185   0.00     19  745 2847.614
    ## 186   0.00     19 2268 3542.034
    ## 187   0.00      5 2662 2847.614
    ## 188   0.00     23 2965 3111.605
    ## 189   1.00     14 3109 3894.376
    ## 190  30.00     11 3307 3294.376
    ## 191   0.00     39 2659 3404.206
    ## 192   0.00     12 1542 3489.061
    ## 193  10.00     10  418 3252.749
    ## 194  20.00     16 2573 3002.836
    ## 195   0.00     21 3820 3406.310
    ## 196   0.75     29 1594 3202.991
    ## 197   0.00     21 3305 3252.749
    ## 198   3.00     12 1287 3323.560
    ## 199   0.00     40 2014 3737.313
    ## 200  20.00     31 3299 2622.134
    ## 201   0.00     32 2068 3294.376
    ## 202   0.00     23 1396 3202.991
    ## 203   8.00     20 2232 3020.220
    ## 204   0.00     26  262 3571.811
    ## 205   0.00     15 1704 3158.058
    ## 206   0.00     24 2462 3559.872
    ## 207   9.00     15 3574 2928.835
    ## 208   0.00     22 3579 3111.605
    ## 209   2.00     34  576 3477.146
    ## 210   0.00     32 4158 2819.005
    ## 211   0.00     29 2326 3169.444
    ## 212   0.00     23 1671 3642.069
    ## 213   0.00     31 4009 3600.274
    ## 214   0.00     39  645 3642.069
    ## 215  20.00     34 3759 2649.533
    ## 216   1.00     31 1657 3406.310
    ## 217   0.00     21 1258 2836.228
    ## 218   2.00     20 3371 2945.626
    ## 219   0.00     20  672 3086.140
    ## 220   0.00     13 3146 2988.478
    ## 221   0.00     20  688 3141.427
    ## 222   0.00     32 2375 3252.749
    ## 223   0.00     15 1226 2734.269
    ## 224  15.00      6 3097 1447.981
    ## 225   4.00      9 1416 3158.058
    ## 226   0.00     24 3657 3796.342
    ## 227   5.00     29 1702 2931.471
    ## 228  20.00     23 1486 3240.809
    ## 229   0.00     25 3358 3141.427
    ## 230   0.00     22  932 3086.140
    ## 231   0.00     13 2618 2973.777
    ## 232   0.00     33 1332 3002.836
    ## 233  30.00     25  485 2819.005
    ## 234   0.00     24 3442 2931.471
    ## 235   0.00     27 1190 2919.532
    ## 236   0.00     16 4236 3352.615
    ## 237   0.00     14 3391 3053.366
    ## 238  20.00     30 4002 2931.471
    ## 239   0.00     30 4221 3240.809
    ## 240   0.00     17 2000 3086.140

Cross validation

``` r
cv_df = crossv_mc(birthweight_df, 100)
```

Creating testing and training datasets

``` r
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))
```

Now we will do cross validation on different models

``` r
cv_df = 
  cv_df %>% 
  mutate(
    model_one  = map(train, ~lm(bwt ~ bhead + blength, data = .x)),
    model_two  = map(train, ~lm(bwt ~ blength + gaweeks, data =.x)),
    model_three  = map(train, ~lm (bwt ~ bhead + blength + babysex + bhead*blength*babysex, data = .x))) %>% 
   mutate(
    rmse_model_one = map2_dbl(model_one, test, ~rmse(model = .x, data = .y)),
    rmse_model_two = map2_dbl(model_two, test, ~rmse(model = .x, data = .y)),
    rmse_model_three = map2_dbl(model_three, test, ~rmse(model = .x, data = .y)))
```

Now I will make violin plots of each model

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()+    labs(
    title = "Violin Plot of Residuals vs. Candidate Models",
    x = "Candidate Models",
    y = "Residuals",
    caption = "Birthweight Data"
  )  + theme_minimal()
```

![](hw6_files/figure-gfm/unnamed-chunk-17-1.png)<!-- --> Overall, the
model that used child length at birth and gestational age, without
interaction terms, had the highest residual error (“model\_two). My
model, which used head circumference and child length at birth without
interactions(”model\_one“), had similar performance to a model that used
head cirumferece and length at birth, sex, and interactions terms of all
three variables (”model\_three").
