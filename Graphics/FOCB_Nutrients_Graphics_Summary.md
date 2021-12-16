Graphics From Friends of Casco Bay Nutrient Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/26/2021

-   [Introduction](#introduction)
-   [Load Data](#load-data)
    -   [Folder References](#folder-references)
    -   [Load Data](#load-data-1)
    -   [Station Names](#station-names)
-   [Recent Conditions](#recent-conditions)
    -   [TN Data 2018 and 2019](#tn-data-2018-and-2019)
        -   [Descriptive Statistics](#descriptive-statistics)
        -   [Output Descriptive Statistics for
            GIS](#output-descriptive-statistics-for-gis)
        -   [TN Graphic](#tn-graphic)
    -   [DIN Data from 2019](#din-data-from-2019)
        -   [Descriptive Statistics](#descriptive-statistics-1)
        -   [Output Descriptive Statistics for
            GIS](#output-descriptive-statistics-for-gis-1)
        -   [DIN Graphic](#din-graphic)
-   [Trend Data](#trend-data)
    -   [Identify TN Trend Stations](#identify-tn-trend-stations)
    -   [Generate Trend Data](#generate-trend-data)
    -   [Restrict Data to Summer
        Months](#restrict-data-to-summer-months)
-   [TN Graphics](#tn-graphics)
    -   [Create TN Trend Data](#create-tn-trend-data)
    -   [Showing Estimated Marginal
        Means](#showing-estimated-marginal-means)
        -   [Linear Model](#linear-model)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This notebook produces graphics summarizing DIN and TN data from Friends
of Casco Bay samples.

FOCB reports the TN samples and DIN samples were sent to different
laboratories, and so direct comparison relies on consistent calibration,
etc. across two labs. Accordingly, here we restrict our analysis to
looking at the two data sources as complementary views of nitrogen in
Casco Bay.

FOCB also reports that some DIN samples over the years had unusually
high ammonium values, and that those samples were noted by the
laboratory conducting the analyses, but not flagged as errors. We
created a reduced data set that dropped those potentially spurious
ammonium (and DIN) values. See the notebook
“FOCB\_Nutrients\_Combined.Rmd” for explanation and details.

Note that analysis showed significant site to site, year to year, and
seasonal variation in nutrient values (see “FOCB\_DIN\_Analysis.Rmd” and
“FOCB\_TN\_Analysis.Rmd” for details). Much of the data preparation here
consists of selecting a subset of available data where differences in
sampling history from site to site and month to month are unlikely to
bias results in any significant way.

\#Load Libraries

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

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Load Data

The data we use here has had a number of suspiciously high NH4 values
removed. See “FOCB\_Nutrients\_Combined.Rmd” for details and
explanation.

``` r
strict_data <- read_csv(file.path(sibling, 
                                 "focb_n_data_strict.csv"))%>%
  mutate(dt = as.Date(dt)) %>%
  mutate(month = factor(month, levels = month.abb),
         yearf = factor(year))
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

## Station Names

``` r
fn <- 'FOCB Monitoring Sites SHORT NAMES.xlsx'
names_df <- read_excel(file.path(sibling, fn))
```

# Recent Conditions

In our analyses (“FOCB\_DIN\_Analysis.Rmd” and
“FOCB\_TN\_Analysis.Rmd”), we concluded that uneven sampling across
sites, years, and times of year strongly influences models fit to
“recent” data. We have fairly complete data on DIN from all FOCB
stations from 2019, and for TN from 2018 and 2019.

Our primary goal is to provide a map and accompanying chart of DIN and
TN levels. For that, we want to compare all **sites** on an even
footing. We now know that there are important annual and seasonal
processes at work, so the uneven sampling history affects estimates of
site conditions. We worked through several models to account for those
differences, but ultimately concluded that we are better off simply
restricting data to subsets of the available data where those effects
are minimized.

We remove the Knightsville Landing Station, which has very limited TN
data and no DIN data.

``` r
recent_data <- strict_data %>%
  filter(year > 2014) %>%
  filter(station != 'KVL84') %>%
  select(station, dt, year, yearf, month, doy, tn, nox_N, nh4_N, din_N, organic_N)
```

``` r
recent_data <- recent_data %>%
   mutate(station_name = names_df$Alt_Name[match(station,
                                                names_df$Station_ID)]) %>%
   mutate(station = factor(station),
          station_name = factor(station_name)) %>%
   mutate(station = fct_reorder(station, tn, na.rm = TRUE),
         station_name = fct_reorder(station_name, tn, median, na.rm = TRUE)) %>%
   relocate(station_name, .after = station)
```

## TN Data 2018 and 2019

We restrict the data to May to September, for the years 2018 and 2019
only. We also filter out negative values (which are impossible) and very
high values that appear to be inconsistent with the DIN data.

``` r
tn_data_18_19 <- recent_data %>%
  filter(year > 2017) %>%
  filter(month %in% month.abb[5:9]) %>%
  filter(tn > 0, tn < 1.5) %>%
  select(station, station_name, dt, month, doy, tn) %>%
  mutate(station = fct_reorder(factor(station), tn, na.rm = TRUE)) %>%
  mutate(station_name = fct_reorder(factor(station_name), tn, na.rm = TRUE))
```

### Descriptive Statistics

``` r
tn_results_18_19 <- tn_data_18_19 %>%
  mutate(tn = if_else(tn > 1.5 | tn <= 0, NA_real_, tn)) %>%
  group_by(station) %>%
  summarize(across(tn, c(mn = ~ mean(.x, na.rm = TRUE),
                                  sd = ~ sd(.x, na.rm = TRUE), 
                                  n = ~sum(! is.na(.x)),
                                  md = ~ median(.x, na.rm = TRUE),
                                  iqr = ~ IQR(.x, na.rm = TRUE),
                                  p90 = ~ quantile(.x, .9, na.rm = TRUE),
                                  gm = ~ exp(mean(log(.x), na.rm = TRUE))))) %>%
  mutate(station_name = names_df$Alt_Name[match(station,
                                                names_df$Station_ID)]) %>%
  mutate(station = fct_reorder(factor(station), tn_md),
         station_name = fct_reorder(factor(station_name), tn_md)) %>%
  relocate(station_name, .after = station)
```

### Output Descriptive Statistics for GIS

``` r
write_csv(tn_results_18_19, file.path(sibling, 'GIS', 'tn_18_19.csv'))
```

### TN Graphic

``` r
plt <- ggplot(tn_data_18_19, aes(tn, station_name)) +

  geom_point(alpha = 1, color = cbep_colors()[4]) +
  
  ylab('') +
  xlab('Total Nitrogen (mg/l)') +
  
  theme_cbep(base_size = 12) +
  theme(axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 12),
        legend.position = 'None',
        panel.spacing.x = unit(1, 'lines')) +
  
  geom_vline(xintercept = 0.32, color = cbep_colors()[3], lty = 3) +
  geom_text(aes(x = 0.35, y = 3, label = '0.32'), 
            angle = 90, hjust = 0, size = 3,
                color = cbep_colors()[3]) +
  geom_vline(xintercept = 0.45, color = cbep_colors()[3], lty = 3) +
  geom_text(aes(x = 0.49, y = 3, label = '0.45'), 
            angle = 90, hjust = 0, size = 3,
                color = cbep_colors()[3]) +
  ggtitle('FOCB Data 2018-2019')
plt
```

<img src="FOCB_Nutrients_Graphics_Summary_files/figure-gfm/tn_points_only-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/tn_by_site.pdf', device = cairo_pdf, width = 3.5, height = 4)
```

## DIN Data from 2019

DIN coverage in 2019 is sparse, but consistent across stations. October
often has higher DIN values than other months, and we have uneven
sampling across stations, so we drop October data from the analysis,
since the uneven samples may bias results.

``` r
din_data_19 <- recent_data %>%
  filter(year == 2019)  %>%
  filter(month %in% month.abb[5:9]) %>%
  filter(! is.na(din_N)) %>%
  select(station, station_name, dt, month, doy, din_N) %>%
  mutate(station = factor(station, levels = levels(tn_data_18_19$station))) %>%
  mutate(station_name = factor(station_name, 
                               levels = levels(tn_data_18_19$station_name)))
```

### Descriptive Statistics

``` r
din_results_2019 <- din_data_19 %>%
  group_by(station) %>%
  summarize(across(din_N, c(mn = ~ mean(.x, na.rm = TRUE),
                                  sd = ~ sd(.x, na.rm = TRUE), 
                                  n = ~sum(! is.na(.x)),
                                  md = ~ median(.x, na.rm = TRUE),
                                  iqr = ~ IQR(.x, na.rm = TRUE),
                                  p90 = ~ quantile(.x, .9, na.rm = TRUE),
                                  gm = ~ exp(mean(log(.x), na.rm = TRUE)))))
```

### Output Descriptive Statistics for GIS

``` r
write_csv(din_results_2019, file.path(sibling, 'GIS', 'din_2019.csv'))
```

### DIN Graphic

``` r
plt <- ggplot(din_data_19, aes(din_N, station_name)) +

  geom_point(alpha = 1, color = cbep_colors()[6]) +
  
  ylab('') +
  xlab('Dissolved Inorganic Nitrogen\n(mg/l)') +
  
  theme_cbep(base_size = 12) +
  theme(axis.title.x = element_text(size = 9),
        legend.position = 'None',
        #panel.grid.major.x = element_line(color = 'gray85'),
        panel.spacing.x = unit(1, 'lines')) +
  ggtitle('FOCB Data 2019')
plt
```

<img src="FOCB_Nutrients_Graphics_Summary_files/figure-gfm/din_points_only-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/din_by_site.pdf', device = cairo_pdf, width = 3.5, height = 4)
```

# Trend Data

Few stations have data from more than a few years. DIN data has been
collected over the past couple of years, at several stations in the mid
2000s, and at a handful of stations pretty much every year since 2001.

## Identify TN Trend Stations

Generally the rule we have used to examine trends is to focus on sites
with relatively complete records, here at least two of the last five
years and at least ten years total.

``` r
trend_counts <- strict_data %>%
  group_by(station, year) %>%
  summarize(din_was_sampled =  ! all(is.na(din_N)),
            tn_was_sampled =  ! all(is.na(tn)),
            .groups = 'drop_last') %>%
  summarize(din_last_5 = sum(din_was_sampled & year > 2014),
            din_total = sum(din_was_sampled),
            tn_last_5 = sum(tn_was_sampled & year > 2014),
            tn_total = sum(tn_was_sampled),
            .groups = 'drop') %>%
  filter((din_total >= 10 & din_last_5 >= 2) | 
           (tn_total >= 10 & tn_last_5 >= 2))

trend_sites <- trend_counts %>%
  filter(tn_total > 10) %>%
  pull(station)
trend_sites
#> [1] "P5BSD" "P6FGG" "P7CBI" "PKT42" "SMT50"
rm(trend_counts)
```

## Generate Trend Data

``` r
trend_data <- strict_data %>%
  filter(station %in% trend_sites) %>%
  filter(!(is.na(din_N) & is.na(tn))) %>%
  mutate(station_name = names_df$Alt_Name[match(station,
                                                names_df$Station_ID)]) %>%
  mutate(station = factor(station),
         station_name = factor(station_name)) %>%
  mutate(station = fct_reorder(station, tn, na.rm = TRUE),
         station_name = fct_reorder(station_name, tn, na.rm = TRUE)) %>%
  relocate(station_name, .after = station) %>%
  select(- din_depth, -tn_depth, -c(nox:din))
```

## Restrict Data to Summer Months

Analysis showed that sampling at different times of year biased results,
with nitrogen levels generally higher in cold season. Since FOCB did not
sample at consistent times of year across all years, different sampling
histories can generate the spurious appearance of long term trends.

To avoid that problem, we restrict attention to just the “warm” months
from May through October, when FOCB is currently collecting data on a
consistent basis.

``` r
trend_data <- trend_data %>%
  filter(month %in% month.abb[5:10])
```

# TN Graphics

## Create TN Trend Data

We remove a nominal zero value and two anomalous high values, drop
unnecessary data, and drop all samples where no TN data is available.

``` r
tn_trend <- trend_data %>%
  mutate(tn = if_else(tn <= 0 | tn >= 1.5, NA_real_, tn)) %>%
  select(-nox_N, -nh4_N, -organic_N, -din_N) %>%
  filter(! is.na(tn))

tn_trend_summary  <-   tn_trend %>%
  select(station_name, year, tn) %>%
  group_by(station_name, year) %>%
  summarize(across(tn, 
                   .fns = list(mn = ~ mean(.x, na.rm = TRUE),
                               sd = ~ sd(.x, na.rm = TRUE), 
                               n = ~sum(! is.na(.x)),
                               md = ~ median(.x, na.rm = TRUE),
                               iqr = ~ IQR(.x, na.rm = TRUE),
                               p90 = ~ quantile(.x, .9, na.rm = TRUE),
                               gm = ~ exp(mean(log(.x), na.rm = TRUE)))),
            .groups = 'drop') %>%
  filter(! is.na(tn_md)) %>%
  mutate(dt = as.Date(paste0('06-15-', year), format = '%m-%d-%Y'))
```

## Showing Estimated Marginal Means

### Linear Model

``` r
trnd_lm <- lm(log(tn) ~ station_name + station_name:year + month,
              data = trend_data)
emms_tn <- emmeans(trnd_lm, c('station_name', 'year'), cov.keep = 'year', type = 'response')
#> NOTE: A nesting structure was detected in the fitted model:
#>     year %in% station_name
```

``` r
emms_df <- as_tibble(emms_tn) %>%
  mutate(dt  = as.Date(paste0(year, '-06-10')))
```

We remove the predictions for Stations without significant trends.

``` r
emms_sig_df <-  emms_df %>%
  filter(station_name %in% c('Broad Sound', 'Clapboard Island',  'Fort Gorges'))
```

``` r
ggplot(tn_trend, aes(dt, tn)) +
  geom_point( color = cbep_colors()[5]) + 
  geom_line(data = emms_sig_df, 
            mapping = aes(x = dt, y = response), 
            lwd = 1,
            color = cbep_colors()[3]) +
  scale_y_continuous(trans = 'log1p') +
  facet_wrap(~station_name, nrow = 5) +
  theme_cbep(base_size = 12) +
  theme(legend.position = 'None',
        panel.grid.major.y = element_line(color = 'gray85'),
        strip.text.y = element_text(size = 9)) +
  xlab('') +
  ylab('Total Nitrogen (mg/l)')
```

<img src="FOCB_Nutrients_Graphics_Summary_files/figure-gfm/core_months_plot_by_date_emms_tall-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/tn_trend_emmeans_tall.pdf', device = cairo_pdf, width = 4, height = 7)
```

``` r
ggplot(tn_trend, aes(dt, tn)) +
  geom_point( color = cbep_colors()[5]) + 
  geom_line(data = emms_sig_df, 
            mapping = aes(x = dt, y = response), 
            lwd = 1,
            color = cbep_colors()[3]) +
  scale_y_continuous(trans = 'log1p') +
  facet_wrap(~station_name, nrow = 1) +
  theme_cbep(base_size = 12) +
  theme(legend.position = 'None',
        panel.grid.major.y = element_line(color = 'gray85'),
        strip.text.y = element_text(size = 9)) +
  xlab('') +
  ylab('Total Nitrogen (mg/l)')
```

<img src="FOCB_Nutrients_Graphics_Summary_files/figure-gfm/core_months_plot_by_date_emms_wide-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/tn_trend_emmeans_wide.pdf', device = cairo_pdf, width = 4, height = 7)
```
