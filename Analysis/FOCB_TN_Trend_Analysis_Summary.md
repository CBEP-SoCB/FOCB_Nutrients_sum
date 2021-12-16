Analysis of Friends of Casco Bay TN Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/26/2021

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Folder References](#folder-references)
    -   [Load Data](#load-data-1)
-   [Station Names](#station-names)
-   [Data Review](#data-review)
    -   [Data Distributions](#data-distributions)
-   [Trend Data](#trend-data)
    -   [Identify Trend Stations](#identify-trend-stations)
    -   [Generate Trend Data](#generate-trend-data)
        -   [Data Distribution](#data-distribution)
        -   [Data Prevalence](#data-prevalence)
    -   [Generate Core Months Trend
        Data](#generate-core-months-trend-data)
    -   [Models](#models)
        -   [Initial Linear Model](#initial-linear-model)
        -   [Check for Non-linear
            Patterns](#check-for-non-linear-patterns)
        -   [Final Linear Model](#final-linear-model)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This notebook Looks at TN numbers from Friends of Casco Bay samples.

# Load Libraries

``` r
library(readxl)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

library(mgcv)    # For generalized linear models
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> This is mgcv 1.8-38. For overview type 'help("mgcv-package")'.
library(emmeans)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Load Data

## Folder References

``` r
sibfldnm <- 'Data'
parent <- dirname(getwd())
sibling <- file.path(parent,sibfldnm)
```

## Load Data

The data we use here has had a number of suspiciously high NH4 values
removed. See “FOCB\_Nutrients\_Combined.Rmd” for details and
explanation.

``` r
strict_data <- read_csv(file.path(sibling, 
                                 "focb_n_data_strict.csv"))%>%
  mutate(month = factor(month, levels = month.abb),
         yearf = factor(year)) %>%
  mutate(dt = as.Date(dt))
#> Rows: 3324 Columns: 17
#> -- Column specification --------------------------------------------------------
#> Delimiter: ","
#> chr   (3): station, month, nh4_ext
#> dbl  (13): year, yearf, doy, tn_depth, din_depth, tn, nox, nh4, din, din_N, ...
#> dttm  (1): dt
#> 
#> i Use `spec()` to retrieve the full column specification for this data.
#> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# Station Names

``` r
fn <- 'FOCB Monitoring Sites SHORT NAMES.xlsx'
names_df <- read_excel(file.path(sibling, fn))
```

# Data Review

TN shows a sparse, sampling pattern, with most samples at just a handful
of sites before 2017.

## Data Distributions

``` r
ggplot(strict_data , aes(tn)) +
  geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> Warning: Removed 2413 rows containing non-finite values (stat_bin).
```

<img src="FOCB_TN_Trend_Analysis_Summary_files/figure-gfm/tn)hist-1.png" style="display: block; margin: auto;" />

Note the four very high values. Only one of those is relevant to this
analysis. That is the highest observed TN value, at over 3 mg/l at site
SMT50.

We have serious doubts about the validity of that observation, and it
tends to have very high leverage in model fits, so we chose to omit it -
-even though we have o basis in our source data to believe that the data
is in error (except for a value more than double the next highest
observed TN value).

# Trend Data

Few stations have data from more than a few years. TN data has been
collected over the past couple of years, at several stations in the mid
2000s, and at a handful of stations pretty much every year since 2001.
Generally the rule we have used to examine trends is to focus on sites
with relatively complete records, say at least two of the last five
years and at least ten years total.

## Identify Trend Stations

``` r
trend_sites <- strict_data %>%
  group_by(station, year) %>%
  summarize(was_sampled =  ! all(is.na(tn)),
            .groups = 'drop_last') %>%
  summarize(last_5 = sum(was_sampled & year > 2014),
            total = sum(was_sampled),
            .groups = 'drop') %>%
  filter(total >= 10, last_5 >= 2) %>%
  pull(station)
trend_sites
#> [1] "P5BSD" "P6FGG" "P7CBI" "PKT42" "SMT50"
```

## Generate Trend Data

Note that we remove the extreme value from the data here.

``` r
trend_data <- strict_data %>%
   filter(station %in% trend_sites) %>%
   mutate(tn = if_else(tn >= 1.5, NA_real_, tn)) %>%
   filter(! is.na(tn)) %>%
   mutate(station_name = names_df$Alt_Name[match(station,
                                                names_df$Station_ID)]) %>%
   mutate(station = factor(station),
          station_name = factor(station_name)) %>%
   mutate(station = fct_reorder(station, tn, na.rm = TRUE),
         station_name = fct_reorder(station_name, tn, na.rm = TRUE)) %>%
   relocate(station_name, .after = station) %>%
   select(-contains('n_N', ignore.case = FALSE), -contains('depth'), -organic_N)
```

### Data Distribution

``` r
ggplot(trend_data, aes(tn)) +
  geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="FOCB_TN_Trend_Analysis_Summary_files/figure-gfm/trend_data_histogram-1.png" style="display: block; margin: auto;" />

### Data Prevalence

``` r
xtabs(~ month + station, data = trend_data )%>%
  as_tibble() %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  filter(n>0) %>%

  ggplot(aes(station, month, fill = sqrt(n))) +
  geom_tile() +
  theme_cbep(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .25))
```

<img src="FOCB_TN_Trend_Analysis_Summary_files/figure-gfm/trend_data_months-1.png" style="display: block; margin: auto;" />
We have few cold weather samples, but fairly good coverage from may
through October.

``` r
xtabs(~ year + station, data = trend_data) %>%
  as_tibble() %>% 
  filter(n>0) %>%

  ggplot(aes(station, year, fill = sqrt(n))) +
  geom_tile() +
  theme_cbep(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .25))
```

<img src="FOCB_TN_Trend_Analysis_Summary_files/figure-gfm/trend_data_years-1.png" style="display: block; margin: auto;" />
Similarly, data coverage by year is pretty good for all these sites.

``` r
xtabs(~ year + month, data = trend_data) %>%
  as_tibble() %>% 
  mutate(month = factor(month, levels = month.abb))  %>%
  filter(n>0) %>%

  ggplot(aes(month, year, fill = sqrt(n))) +
  geom_tile() +
  theme_cbep(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .25))
```

<img src="FOCB_TN_Trend_Analysis_Summary_files/figure-gfm/trend_data_times-1.png" style="display: block; margin: auto;" />

We see the change in FOCB monitoring practices in 2017. Winter data is
not available in recent\_years. If there are seasonal TN trends, as our
analysis of the recent data suggests, annual averages may be biased. We
would be better served to restrict attention to the summer months, where
sampling has been most consistent over time. We focus on May through
October data.

## Generate Core Months Trend Data

``` r
core_months_data <- trend_data %>%
  filter(month %in% month.abb[5:10])
```

## Models

### Initial Linear Model

``` r
trnd_lm_1 <- lm(log(tn) ~ (year + station_name + month)^2 , 
                data = core_months_data)
anova(trnd_lm_1)
#> Analysis of Variance Table
#> 
#> Response: log(tn)
#>                     Df  Sum Sq Mean Sq F value    Pr(>F)    
#> year                 1  4.6382  4.6382 60.5961 1.257e-13 ***
#> station_name         4  3.5619  0.8905 11.6336 8.991e-09 ***
#> month                5  0.9838  0.1968  2.5706   0.02699 *  
#> year:station_name    4  0.5500  0.1375  1.7963   0.12958    
#> year:month           5  0.8380  0.1676  2.1897   0.05546 .  
#> station_name:month  20  0.8132  0.0407  0.5312   0.95231    
#> Residuals          289 22.1208  0.0765                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Note that in this setting, there is no reason to believe all stations
show the same trend, so a model that does not fit trends separately for
each station (via station x year interaction term) is of limited value,
even if the model (as here) suggests the interaction is not important.

We could be more cautious about claiming a trend by fitting a
hierarchical model that treats years as a random factor as well. That
would account for high intra-year autocorrelation. We choose not to do
that here.

``` r
trnd_lm_2 <- step(trnd_lm_1)
#> Start:  AIC=-808.15
#> log(tn) ~ (year + station_name + month)^2
#> 
#>                      Df Sum of Sq    RSS     AIC
#> - station_name:month 20   0.81323 22.934 -836.27
#> <none>                            22.121 -808.15
#> - year:station_name   4   0.60352 22.724 -807.29
#> - year:month          5   0.96334 23.084 -804.12
#> 
#> Step:  AIC=-836.27
#> log(tn) ~ year + station_name + month + year:station_name + year:month
#> 
#>                     Df Sum of Sq    RSS     AIC
#> - year:station_name  4   0.55710 23.491 -836.37
#> <none>                           22.934 -836.27
#> - year:month         5   0.83801 23.772 -834.46
#> 
#> Step:  AIC=-836.37
#> log(tn) ~ year + station_name + month + year:month
#> 
#>                Df Sum of Sq    RSS     AIC
#> <none>                      23.491 -836.37
#> - year:month    5    0.8309 24.322 -834.94
#> - station_name  4    3.4111 26.902 -799.77
```

``` r
anova(trnd_lm_2)
#> Analysis of Variance Table
#> 
#> Response: log(tn)
#>               Df  Sum Sq Mean Sq F value    Pr(>F)    
#> year           1  4.6382  4.6382 61.8000 6.188e-14 ***
#> station_name   4  3.5619  0.8905 11.8647 5.487e-09 ***
#> month          5  0.9838  0.1968  2.6217   0.02431 *  
#> year:month     5  0.8309  0.1662  2.2142   0.05275 .  
#> Residuals    313 23.4911  0.0751                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
summary(trnd_lm_2)
#> 
#> Call:
#> lm(formula = log(tn) ~ year + station_name + month + year:month, 
#>     data = core_months_data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.84216 -0.18231 -0.04746  0.13215  1.12941 
#> 
#> Coefficients:
#>                               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                  25.476942  16.739150   1.522  0.12902    
#> year                         -0.013396   0.008318  -1.611  0.10828    
#> station_nameClapboard Island  0.025089   0.049027   0.512  0.60919    
#> station_nameQuahog Bay        0.155738   0.055314   2.816  0.00518 ** 
#> station_nameFort Gorges       0.144999   0.048447   2.993  0.00298 ** 
#> station_nameSMCC Pier         0.266594   0.045376   5.875 1.08e-08 ***
#> monthJun                     25.540096  25.972245   0.983  0.32619    
#> monthJul                     -6.121752  23.903072  -0.256  0.79804    
#> monthAug                     67.858380  24.679620   2.750  0.00631 ** 
#> monthSep                     29.798411  24.391271   1.222  0.22275    
#> monthOct                     16.632100  26.367552   0.631  0.52865    
#> year:monthJun                -0.012655   0.012910  -0.980  0.32774    
#> year:monthJul                 0.003096   0.011876   0.261  0.79453    
#> year:monthAug                -0.033665   0.012265  -2.745  0.00640 ** 
#> year:monthSep                -0.014777   0.012122  -1.219  0.22376    
#> year:monthOct                -0.008175   0.013103  -0.624  0.53314    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.274 on 313 degrees of freedom
#> Multiple R-squared:  0.2989, Adjusted R-squared:  0.2653 
#> F-statistic: 8.896 on 15 and 313 DF,  p-value: < 2.2e-16
```

So the obvious linear model analysis suggests there is a weak negative
linear trend, and there are no differences in trend among stations.

The month to month terms have high standard errors, and the possible
interaction rests principally on the month of August.

``` r
oldpar <- par(mfrow=c(2,2))
plot(trnd_lm_2)
```

<img src="FOCB_TN_Trend_Analysis_Summary_files/figure-gfm/trend_lm_2_diagnostics-1.png" style="display: block; margin: auto;" />

``` r
par(oldpar)
```

Other than the heavy tails and slight skewness of the residuals, model
diagnostics are pretty good, suggesting these conclusions will be robust
to most other reasonable model specifications.

### Check for Non-linear Patterns

We start by fitting a polynomial

``` r
trnd_lm_3 <- lm(log(tn) ~ poly(year,2) + station + poly(year,2):station + 
                                month + month:year, data = core_months_data)
anova(trnd_lm_3)
#> Analysis of Variance Table
#> 
#> Response: log(tn)
#>                        Df  Sum Sq Mean Sq F value    Pr(>F)    
#> poly(year, 2)           2  4.6410 2.32052 31.1546 4.919e-13 ***
#> station                 4  3.6279 0.90697 12.1766 3.405e-09 ***
#> month                   5  0.9432 0.18863  2.5325   0.02892 *  
#> poly(year, 2):station   8  0.8059 0.10074  1.3525   0.21708    
#> month:year              5  0.8446 0.16893  2.2680   0.04777 *  
#> Residuals             304 22.6432 0.07448                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
anova(trnd_lm_2, trnd_lm_3, test = 'F')
#> Analysis of Variance Table
#> 
#> Model 1: log(tn) ~ year + station_name + month + year:month
#> Model 2: log(tn) ~ poly(year, 2) + station + poly(year, 2):station + month + 
#>     month:year
#>   Res.Df    RSS Df Sum of Sq      F Pr(>F)
#> 1    313 23.491                           
#> 2    304 22.643  9   0.84792 1.2649 0.2555
```

So there is no evidence that we need the non-linear terms to capture the
long-term trend.

### Final Linear Model

We force-fit separate slopes for each station, and drop the year by
month interaction term as being of limited interest, and possibly
misleading.

``` r
trnd_lm <- lm(log(tn) ~ station_name + station_name:year + month,
                data = core_months_data)
anova(trnd_lm)
#> Analysis of Variance Table
#> 
#> Response: log(tn)
#>                    Df  Sum Sq Mean Sq F value    Pr(>F)    
#> station_name        4  5.3691 1.34227 17.7297 3.900e-13 ***
#> month               5  0.9055 0.18109  2.3920   0.03773 *  
#> station_name:year   5  3.4593 0.69186  9.1387 3.943e-08 ***
#> Residuals         314 23.7720 0.07571                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summary(trnd_lm)
#> 
#> Call:
#> lm(formula = log(tn) ~ station_name + station_name:year + month, 
#>     data = core_months_data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.8923 -0.1825 -0.0320  0.1306  1.0674 
#> 
#> Coefficients:
#>                                     Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                        74.541641  17.453711   4.271 2.58e-05 ***
#> station_nameClapboard Island      -11.468651  24.519961  -0.468 0.640304    
#> station_nameQuahog Bay            -43.201490  26.547065  -1.627 0.104665    
#> station_nameFort Gorges           -26.166855  24.441942  -1.071 0.285184    
#> station_nameSMCC Pier             -54.898150  23.154428  -2.371 0.018345 *  
#> monthJun                            0.082046   0.053995   1.520 0.129636    
#> monthJul                            0.115013   0.050262   2.288 0.022785 *  
#> monthAug                            0.116318   0.051600   2.254 0.024871 *  
#> monthSep                            0.066779   0.052082   1.282 0.200724    
#> monthOct                            0.172540   0.054406   3.171 0.001667 ** 
#> station_nameBroad Sound:year       -0.037775   0.008671  -4.356 1.79e-05 ***
#> station_nameClapboard Island:year  -0.032065   0.008571  -3.741 0.000218 ***
#> station_nameQuahog Bay:year        -0.016237   0.009935  -1.634 0.103204    
#> station_nameFort Gorges:year       -0.024703   0.008518  -2.900 0.003993 ** 
#> station_nameSMCC Pier:year         -0.010352   0.007572  -1.367 0.172513    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.2751 on 314 degrees of freedom
#> Multiple R-squared:  0.2905, Adjusted R-squared:  0.2589 
#> F-statistic: 9.184 on 14 and 314 DF,  p-value: < 2.2e-16
```

Note that we have significant trends at three sites: Broad Sound,
Clapboard Island, and Fort Gorges.
