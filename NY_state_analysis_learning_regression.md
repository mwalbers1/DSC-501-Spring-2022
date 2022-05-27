Effect of COVID-19 on Learning Regression in New York State
================

Analysis of graduation rate in 2020 school year of 4-year cohort
students in NY state

# Introduction

COVID-19 had a big impact on student learning during the 2020 school
year in New York State and throughout the United States and the rest of
the world. This analysis will focus on learning regression among
school-age children in New York State. Learning regression is defined as
a decline in student learning as measured by gradation rate. This
analysis will explore potential explanatory variables that may be
associated with graduation rate in the 2020 New York Department of
Education database. The dependent variable is graduation rate. The
number of free and reduced price lunches among school-aged children in
New York State will be analyzed for a potential association with
graduation rate.

## Dataset

This dataset consists of the New York State graduation rate data and the
number of free and reduced price lunches for school-aged children for
school year 2019-2020. The data is sourced from the New York state
department of education. In New York state, a student qualifies for free
lunch if his/her family income is at or below $38,000 dollars a year for
family size of four or less. Students from families (size four or less)
earning up to $48,000 per year qualify for reduced price lunches.

``` r
df_ny_school_data$nrc_desc <- ifelse(df_ny_school_data$nrc_code=='2', 'Other', df_ny_school_data$nrc_desc)
```

### Number of Free and Reduced price lunches by county and nrc_desc

#### Filter on 4-year cohort and subgroup of ‘All Students’

``` r
df_all_students <- df_ny_school_data %>%
  filter(membership_code == 9) %>%
  filter(subgroup_code %in% c(1))
```

``` r
df_all_students <- subset(df_all_students,
                          select = c(nrc_code,
                               nrc_desc, 
                               county_name, 
                               nyc_ind, 
                               enroll_cnt,
                               grad_cnt,
                               grad_pct,
                               ENTITY_CD,
                               NUM_FREE_LUNCH,
                               NUM_REDUCED_LUNCH))
```

``` r
dim(df_all_students)
```

    ## [1] 1243   10

``` r
str(df_all_students)
```

    ## 'data.frame':    1243 obs. of  10 variables:
    ##  $ nrc_code         : int  3 7 5 6 5 3 5 6 3 6 ...
    ##  $ nrc_desc         : chr  "Urban-Suburban High Needs" "Charters" "Average Needs" "Low Needs" ...
    ##  $ county_name      : chr  "ALBANY" "BRONX" "ALBANY" "ALBANY" ...
    ##  $ nyc_ind          : int  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ enroll_cnt       : int  697 98 67 404 120 148 372 510 27 356 ...
    ##  $ grad_cnt         : int  504 55 64 383 110 127 349 478 24 339 ...
    ##  $ grad_pct         : num  0.72 0.56 0.96 0.95 0.92 0.86 0.94 0.94 0.89 0.95 ...
    ##  $ ENTITY_CD        : num  1.01e+10 NA 1.02e+10 1.03e+10 1.04e+10 ...
    ##  $ NUM_FREE_LUNCH   : int  5587 0 225 504 650 1102 1385 0 121 787 ...
    ##  $ NUM_REDUCED_LUNCH: int  31 0 48 52 89 24 179 0 27 110 ...

``` r
head(df_all_students)
```

    ##   nrc_code                  nrc_desc county_name nyc_ind enroll_cnt grad_cnt
    ## 1        3 Urban-Suburban High Needs      ALBANY       0        697      504
    ## 2        7                  Charters       BRONX       1         98       55
    ## 3        5             Average Needs      ALBANY       0         67       64
    ## 4        6                 Low Needs      ALBANY       0        404      383
    ## 5        5             Average Needs      ALBANY       0        120      110
    ## 6        3 Urban-Suburban High Needs      ALBANY       0        148      127
    ##   grad_pct   ENTITY_CD NUM_FREE_LUNCH NUM_REDUCED_LUNCH
    ## 1     0.72 10100010000           5587                31
    ## 2     0.56          NA              0                 0
    ## 3     0.96 10201040000            225                48
    ## 4     0.95 10306060000            504                52
    ## 5     0.92 10402060000            650                89
    ## 6     0.86 10500010000           1102                24

#### Filter on 4-year cohort and race/ethnicity groups of Black, Hispanic, Asian, White

``` r
df_race_ethnicity <- df_ny_school_data %>%
  filter(membership_code == 9) %>%
  filter(subgroup_code %in% c(5,6,7,8))
```

``` r
df_race_ethnicity <- subset(df_race_ethnicity, 
                    select = c(nrc_code,
                               nrc_desc, 
                               county_name, 
                               nyc_ind, 
                               subgroup_name,
                               enroll_cnt,
                               grad_cnt,
                               grad_pct,
                               ENTITY_CD,
                               NUM_FREE_LUNCH,
                               NUM_REDUCED_LUNCH))
```

``` r
dim(df_race_ethnicity)
```

    ## [1] 4182   11

``` r
str(df_race_ethnicity)
```

    ## 'data.frame':    4182 obs. of  11 variables:
    ##  $ nrc_code         : int  3 3 3 3 7 7 5 5 5 5 ...
    ##  $ nrc_desc         : chr  "Urban-Suburban High Needs" "Urban-Suburban High Needs" "Urban-Suburban High Needs" "Urban-Suburban High Needs" ...
    ##  $ county_name      : chr  "ALBANY" "ALBANY" "ALBANY" "ALBANY" ...
    ##  $ nyc_ind          : int  0 0 0 0 1 1 0 0 0 0 ...
    ##  $ subgroup_name    : chr  "Black" "Hispanic" "Asian/Pacific Islander" "White" ...
    ##  $ enroll_cnt       : int  368 107 57 145 28 70 2 3 1 59 ...
    ##  $ grad_cnt         : int  251 73 48 115 15 40 0 0 0 58 ...
    ##  $ grad_pct         : num  0.68 0.68 0.84 0.79 0.54 0.57 0 0 0 0.98 ...
    ##  $ ENTITY_CD        : num  1.01e+10 1.01e+10 1.01e+10 1.01e+10 NA ...
    ##  $ NUM_FREE_LUNCH   : int  5587 5587 5587 5587 0 0 225 225 225 225 ...
    ##  $ NUM_REDUCED_LUNCH: int  31 31 31 31 0 0 48 48 48 48 ...

``` r
head(df_race_ethnicity)
```

    ##   nrc_code                  nrc_desc county_name nyc_ind          subgroup_name
    ## 1        3 Urban-Suburban High Needs      ALBANY       0                  Black
    ## 2        3 Urban-Suburban High Needs      ALBANY       0               Hispanic
    ## 3        3 Urban-Suburban High Needs      ALBANY       0 Asian/Pacific Islander
    ## 4        3 Urban-Suburban High Needs      ALBANY       0                  White
    ## 5        7                  Charters       BRONX       1                  Black
    ## 6        7                  Charters       BRONX       1               Hispanic
    ##   enroll_cnt grad_cnt grad_pct   ENTITY_CD NUM_FREE_LUNCH NUM_REDUCED_LUNCH
    ## 1        368      251     0.68 10100010000           5587                31
    ## 2        107       73     0.68 10100010000           5587                31
    ## 3         57       48     0.84 10100010000           5587                31
    ## 4        145      115     0.79 10100010000           5587                31
    ## 5         28       15     0.54          NA              0                 0
    ## 6         70       40     0.57          NA              0                 0

``` r
df_lc <- df_race_ethnicity %>% 
    group_by(county_name, nrc_desc, ENTITY_CD) %>%
    summarize(NUM_FREE_LUNCH = max(NUM_FREE_LUNCH), NUM_REDUCED_LUNCH = max(NUM_REDUCED_LUNCH))
```

    ## `summarise()` has grouped output by 'county_name', 'nrc_desc'. You can override
    ## using the `.groups` argument.

``` r
head(df_lc)
```

    ## # A tibble: 6 × 5
    ## # Groups:   county_name, nrc_desc [2]
    ##   county_name nrc_desc        ENTITY_CD NUM_FREE_LUNCH NUM_REDUCED_LUNCH
    ##   <chr>       <chr>               <dbl>          <int>             <int>
    ## 1 ALBANY      Average Needs 10201040000            225                48
    ## 2 ALBANY      Average Needs 10402060000            650                89
    ## 3 ALBANY      Average Needs 10601060000           1385               179
    ## 4 ALBANY      Low Needs     10306060000            504                52
    ## 5 ALBANY      Low Needs     10802060000            787               110
    ## 6 ALBANY      Low Needs     11003060000             84                26

#### Aggregate Lunch counts by county and nrc_desc

``` r
df_agg_lc <- df_lc %>%
  group_by(county_name, nrc_desc) %>%
  summarize(NUM_FREE_LUNCH = sum(NUM_FREE_LUNCH), NUM_REDUCED_LUNCH = sum(NUM_REDUCED_LUNCH))
```

    ## `summarise()` has grouped output by 'county_name'. You can override using the
    ## `.groups` argument.

``` r
head(df_agg_lc)
```

    ## # A tibble: 6 × 4
    ## # Groups:   county_name [3]
    ##   county_name nrc_desc                  NUM_FREE_LUNCH NUM_REDUCED_LUNCH
    ##   <chr>       <chr>                              <int>             <int>
    ## 1 ALBANY      Average Needs                       2260               316
    ## 2 ALBANY      Low Needs                           1375               188
    ## 3 ALBANY      Urban-Suburban High Needs           7564               138
    ## 4 ALLEGANY    Average Needs                        197                28
    ## 5 ALLEGANY    Rural High Needs                    2545               263
    ## 6 BRONX       Charters                               0                 0

``` r
df_agg_lc %>% 
  group_by(nrc_desc) %>%
  summarize(NUM_FREE_LUNCH = sum(NUM_FREE_LUNCH), NUM_REDUCED_LUNCH = sum(NUM_REDUCED_LUNCH))
```

    ## # A tibble: 7 × 3
    ##   nrc_desc                  NUM_FREE_LUNCH NUM_REDUCED_LUNCH
    ##   <chr>                              <int>             <int>
    ## 1 Average Needs                     239403             30847
    ## 2 Charters                             561                28
    ## 3 Low Needs                          45733              6310
    ## 4 NYC                               620346             27223
    ## 5 Other                              80532               860
    ## 6 Rural High Needs                   69191              7265
    ## 7 Urban-Suburban High Needs         138646              5740

## Univariate Analysis

``` r
ggplot(data = df_agg_lc, aes(x = nrc_desc, y = NUM_FREE_LUNCH, fill=nrc_desc)) +
  geom_col() +
  ggtitle("Students eligible for Free Lunches in New York State") +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        legend.position = "none",
        plot.title = element_text(size=16, face="bold")) +
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000, 400000, 500000, 600000),
                     labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x="Needs Resource Group", y="No. Free Lunches")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/bar%20chart%20for%20Number%20of%20students%20eligible%20for%20free%20lunch%20by%20N/RC-1.png)<!-- -->

``` r
ggplot(data = df_agg_lc, aes(x = nrc_desc, y = NUM_REDUCED_LUNCH, fill=nrc_desc)) +
  geom_col() +
  ggtitle("Students eligible for Reduced Price Lunches in NY State") +
  theme(axis.text.x = element_text(angle=60, hjust=1), 
        legend.position = "none",
        plot.title = element_text(size=16, face="bold")) +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000),
                     labels = unit_format(unit = "K", scale = 1e-3)) +
  labs(x="Needs Resource Group", y="No. Reduced Lunches")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/bar%20chart%20for%20Number%20of%20students%20eligible%20for%20reduced%20price%20lunch%20by%20N/RC-1.png)<!-- -->

``` r
ggplot(data = df_race_ethnicity, aes(x = subgroup_name, y = enroll_cnt, fill = nrc_desc)) +
  geom_col(position='dodge') +
  ggtitle("Race/Ethnicity by Needs Resource Group") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        legend.position = "right",
        plot.title = element_text(size=16, face="bold")) +
  labs(x="Race/Ethnicity Group", y="Enrollment Count", fill="Needs Resource Group")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/Student%20enrollment%20by%20race/ethnicity%20and%20NR/C%20index-1.png)<!-- -->

## ANOVA Test for Number of Free Lunches

We see from the bar charts that the average count of free and reduced
priced lunches differs among the groups. The ANOVA test will confirm
this statistically. Because charter schools comprise less than one
percent of the total number of free and reduced price lunches, the
charter schools will be excluded.

Let
![\\mu_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_1 "\mu_1")
= Average Needs mean students eligible for free lunches

Let
![\\mu_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_2 "\mu_2")
= Low Needs mean students eligible for free lunches

Let
![\\mu_3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_3 "\mu_3")
= NYC mean students eligible for free lunches

Let
![\\mu_4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_4 "\mu_4")
= Other mean students eligible for free lunches

Let
![\\mu_5](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_5 "\mu_5")
= Rural High Needs mean students eligible for free lunches

Let
![\\mu_6](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_6 "\mu_6")
= Urban-Suburban High Needs mean students eligible for free lunches

![H_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0 "H_0")
:
![\\mu_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_1 "\mu_1")
![\\mu_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_2 "\mu_2")
![\\mu_3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_3 "\mu_3")
![\\mu_4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_4 "\mu_4")
![\\mu_5](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_5 "\mu_5")
![\\mu_6](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_6 "\mu_6")
are equal

![H_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_1 "H_1")
: At least one group mean is different from the others

``` r
excl_nrc_desc <- c("Charters")
df_anova_test <- subset(df_agg_lc, !(nrc_desc %in% excl_nrc_desc)) 
  
df_anova_test %>%
  group_by(nrc_desc) %>%
  summarize(NUM_FREE_LUNCH = sum(NUM_FREE_LUNCH), NUM_REDUCED_LUNCH = sum(NUM_REDUCED_LUNCH))
```

    ## # A tibble: 6 × 3
    ##   nrc_desc                  NUM_FREE_LUNCH NUM_REDUCED_LUNCH
    ##   <chr>                              <int>             <int>
    ## 1 Average Needs                     239403             30847
    ## 2 Low Needs                          45733              6310
    ## 3 NYC                               620346             27223
    ## 4 Other                              80532               860
    ## 5 Rural High Needs                   69191              7265
    ## 6 Urban-Suburban High Needs         138646              5740

``` r
anova_free_lunch = aov(NUM_FREE_LUNCH ~ nrc_desc, data = df_anova_test)
summary(anova_free_lunch)
```

    ##              Df    Sum Sq   Mean Sq F value Pr(>F)    
    ## nrc_desc      5 7.076e+10 1.415e+10   89.23 <2e-16 ***
    ## Residuals   137 2.173e+10 1.586e+08                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The p-value is less than 0.05 so we can reject the null hypothesis that
the mean number of free lunches is the same among the Needs Resource
groups.

``` r
anova_reduced_lunch = aov(NUM_REDUCED_LUNCH ~ nrc_desc, data = df_anova_test)
summary(anova_reduced_lunch)
```

    ##              Df    Sum Sq  Mean Sq F value Pr(>F)    
    ## nrc_desc      5 127680415 25536083   30.76 <2e-16 ***
    ## Residuals   137 113734451   830178                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The p-value is less than 0.05 so we can reject the null hypothesis that
mean number of reduced price lunches is the same across all of the Need
Resource groups.

## Bivariate Exploration

The bivariate scatter plot shows a potential relationship between free
and reduced price lunches and the school graduation rate.

``` r
df_grad_pct_plot <- df_all_students %>%
  filter(!(nrc_desc %in% "Charters"))
```

### All Needs Resource Groups

``` r
ggplot(df_grad_pct_plot, aes(x=NUM_FREE_LUNCH, 
                            y=grad_pct)) +
  geom_point(aes(col=nrc_desc)) +
  labs(x="No. Students Eligible Free Lunches", y="Graduation Pct", col="Needs Resource Group")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/scatter%20plot%20betwen%20num%20free%20lunches%20and%20graduation%20rate-1.png)<!-- -->

``` r
nrc_excl <- c("NYC", "Low Needs")

df_grad_pct_plot_2 <- df_grad_pct_plot %>%
  filter(grad_pct >= 0.50) %>%
  filter(!(nrc_desc %in% nrc_excl))
```

### Exclude Charter and NYC Schools

``` r
ggplot(df_grad_pct_plot_2, aes(x=NUM_FREE_LUNCH, 
                            y=grad_pct)) +
  geom_point(aes(col=nrc_desc), position="jitter") +
  labs(x="No. Students Eligible Free Lunches", y="Graduation Pct", col="Needs Resource Group")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/scatter%20plot%20without%20NYC%20grad%20pct%2050%20percent-1.png)<!-- -->

``` r
df_grad_pct_plot_3 <- df_grad_pct_plot_2 %>%
  filter(NUM_FREE_LUNCH <= 10000)
```

### Average, Rural, and Urban-Suburban

``` r
ggplot(df_grad_pct_plot_3, aes(x=NUM_FREE_LUNCH, 
                            y=grad_pct)) +
  geom_point(aes(col=nrc_desc), position="jitter") +
  labs(x="No. Students Eligible Free Lunches", y="Graduation Pct", col="Needs Resource Group")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/Grad%20pct%20v.%20Num%20Free%20Lunches%20plot%203-1.png)<!-- -->

``` r
ggplot(df_grad_pct_plot_3, aes(x=NUM_REDUCED_LUNCH, 
                            y=grad_pct)) +
  geom_point(aes(col=nrc_desc)) +
  geom_point(aes(col=nrc_desc), position="jitter") +
  labs(x="No. Students Eligible Reduced Lunches", y="Graduation Pct", col="Needs Resource Group")
```

![](NY_state_analysis_learning_regression_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Correlation

A correlation will be calculated between the dependent variable
graduation rate and the number of free/reduced price lunches.

One potential concern for creating a linear regression model is
multi-collinearity in that a correlation may exist among the independent
variables, number of free lunches and number of reduced price lunches.
To address this concern, a new variable will be created called “Lunch
Assist Index” which will be a weighted average between number of free
and reduced price lunches.

### First Correlation Test:

There are no filters applied for this correlation

``` r
df_corr_2 <- df_all_students %>%
  mutate(LA_INDEX = 0.85 * NUM_FREE_LUNCH + 0.15 * NUM_REDUCED_LUNCH)
  
head(df_corr_2)
```

    ##   nrc_code                  nrc_desc county_name nyc_ind enroll_cnt grad_cnt
    ## 1        3 Urban-Suburban High Needs      ALBANY       0        697      504
    ## 2        7                  Charters       BRONX       1         98       55
    ## 3        5             Average Needs      ALBANY       0         67       64
    ## 4        6                 Low Needs      ALBANY       0        404      383
    ## 5        5             Average Needs      ALBANY       0        120      110
    ## 6        3 Urban-Suburban High Needs      ALBANY       0        148      127
    ##   grad_pct   ENTITY_CD NUM_FREE_LUNCH NUM_REDUCED_LUNCH LA_INDEX
    ## 1     0.72 10100010000           5587                31  4753.60
    ## 2     0.56          NA              0                 0     0.00
    ## 3     0.96 10201040000            225                48   198.45
    ## 4     0.95 10306060000            504                52   436.20
    ## 5     0.92 10402060000            650                89   565.85
    ## 6     0.86 10500010000           1102                24   940.30

``` r
cor.test(~ LA_INDEX + grad_pct, method="pearson", data=df_corr_2)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  LA_INDEX and grad_pct
    ## t = -6.7223, df = 1241, p-value = 2.72e-11
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2405373 -0.1332291
    ## sample estimates:
    ##        cor 
    ## -0.1874424

``` r
model = lm(grad_pct ~ LA_INDEX, data = df_corr_2)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = grad_pct ~ LA_INDEX, data = df_corr_2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.85009 -0.00830  0.07218  0.12058  0.29835 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.501e-01  8.603e-03  98.813  < 2e-16 ***
    ## LA_INDEX    -4.271e-06  6.354e-07  -6.722 2.72e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2353 on 1241 degrees of freedom
    ## Multiple R-squared:  0.03513,    Adjusted R-squared:  0.03436 
    ## F-statistic: 45.19 on 1 and 1241 DF,  p-value: 2.72e-11

### Second Correlation Test

Filters applied: - graduaton rate \>= 0.50 - Exclude Charters, NYC, and
Low Needs NR/C groups - Number of Free Lunches up to 10,000

``` r
df_corr <- df_all_students %>%
  filter(grad_pct >= 0.50) %>%
  filter(!(nrc_desc %in% c("Charters", "NYC", "Low Needs"))) %>%
  filter(NUM_FREE_LUNCH <= 10000)
```

### Create index column from Number of Free and Reduced price lunches

``` r
df_corr <- df_corr %>%
  mutate(LA_INDEX = 0.85 * NUM_FREE_LUNCH + 0.15 * NUM_REDUCED_LUNCH)

head(df_corr)
```

    ##   nrc_code                  nrc_desc county_name nyc_ind enroll_cnt grad_cnt
    ## 1        3 Urban-Suburban High Needs      ALBANY       0        697      504
    ## 2        5             Average Needs      ALBANY       0         67       64
    ## 3        5             Average Needs      ALBANY       0        120      110
    ## 4        3 Urban-Suburban High Needs      ALBANY       0        148      127
    ## 5        5             Average Needs      ALBANY       0        372      349
    ## 6        3 Urban-Suburban High Needs      ALBANY       0         27       24
    ##   grad_pct   ENTITY_CD NUM_FREE_LUNCH NUM_REDUCED_LUNCH LA_INDEX
    ## 1     0.72 10100010000           5587                31  4753.60
    ## 2     0.96 10201040000            225                48   198.45
    ## 3     0.92 10402060000            650                89   565.85
    ## 4     0.86 10500010000           1102                24   940.30
    ## 5     0.94 10601060000           1385               179  1204.10
    ## 6     0.89 10701030000            121                27   106.90

### Correlation between Lunch Assist Index and Graduation Pct

``` r
cor.test(~ LA_INDEX + grad_pct, method="pearson", data=df_corr)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  LA_INDEX and grad_pct
    ## t = -10.276, df = 530, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4760982 -0.3341521
    ## sample estimates:
    ##        cor 
    ## -0.4075843

``` r
model = lm(grad_pct ~ LA_INDEX, data = df_corr)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = grad_pct ~ LA_INDEX, data = df_corr)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.39452 -0.02865  0.01404  0.04323  0.15052 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  9.244e-01  3.734e-03  247.54   <2e-16 ***
    ## LA_INDEX    -2.971e-05  2.891e-06  -10.28   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06816 on 530 degrees of freedom
    ## Multiple R-squared:  0.1661, Adjusted R-squared:  0.1646 
    ## F-statistic: 105.6 on 1 and 530 DF,  p-value: < 2.2e-16
