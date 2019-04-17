---
title: "ChiSquare"
author: "Your name here"
date: "4/18/2019"
output: 
  html_document:
    keep_md: yes
---

This lesson will introduce you to Chi Square analyses in R. We will cover both the Goodness of fit test (one factor) and the test of independence or chi square contingency test (2 factors).

The data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States. It began in the 1960s and since 1999 examines a nationally representative sample of about 5,000 people each year. The NHANES interview includes demographic, socioeconomic, dietary, and health-related questions. The physical exam includes medical, dental, and physiological measurements, as well as several standard laboratory tests. NHANES is used to determine the prevalence of major diseases and risk factors for those diseases. NHANES data are also the basis for national standards for measurements like height, weight, and blood pressure. Data from this survey is used in epidemiology studies and health sciences research, which help develop public health policy, direct and design health programs and services, and expand the health knowledge for the Nation.

We are using a small slice of this data. We're only using a handful of variables from the 2011-2012 survey years on about 5,000 individuals.

## First we will load the nhanes.csv dataset


```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.0       ✔ purrr   0.3.1  
## ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
## ✔ readr   1.3.1       ✔ forcats 0.4.0
```

```
## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
nh <- read_csv("nhanes.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   Gender = col_character(),
##   Race = col_character(),
##   Education = col_character(),
##   MaritalStatus = col_character(),
##   RelationshipStatus = col_character(),
##   Insured = col_character(),
##   HomeOwn = col_character(),
##   Work = col_character(),
##   Diabetes = col_character(),
##   PhysActive = col_character(),
##   SmokingStatus = col_character()
## )
```

```
## See spec(...) for full column specifications.
```

```r
nh
```

```
## # A tibble: 5,000 x 32
##       id Gender   Age Race  Education MaritalStatus RelationshipSta…
##    <dbl> <chr>  <dbl> <chr> <chr>     <chr>         <chr>           
##  1 62163 male      14 Asian <NA>      <NA>          <NA>            
##  2 62172 female    43 Black High Sch… NeverMarried  Single          
##  3 62174 male      80 White College … Married       Committed       
##  4 62174 male      80 White College … Married       Committed       
##  5 62175 male       5 White <NA>      <NA>          <NA>            
##  6 62176 female    34 White College … Married       Committed       
##  7 62178 male      80 White High Sch… Widowed       Single          
##  8 62180 male      35 White College … Married       Committed       
##  9 62186 female    17 Black <NA>      <NA>          <NA>            
## 10 62190 female    15 Mexi… <NA>      <NA>          <NA>            
## # … with 4,990 more rows, and 25 more variables: Insured <chr>,
## #   Income <dbl>, Poverty <dbl>, HomeRooms <dbl>, HomeOwn <chr>,
## #   Work <chr>, Weight <dbl>, Height <dbl>, BMI <dbl>, Pulse <dbl>,
## #   BPSys <dbl>, BPDia <dbl>, Testosterone <dbl>, HDLChol <dbl>,
## #   TotChol <dbl>, Diabetes <chr>, DiabetesAge <dbl>, nPregnancies <dbl>,
## #   nBabies <dbl>, SleepHrsNight <dbl>, PhysActive <chr>,
## #   PhysActiveDays <dbl>, AlcoholDay <dbl>, AlcoholYear <dbl>,
## #   SmokingStatus <chr>
```

Now we will filter the nh dataset so it just contains adults (>= 18 years old) and save the new dataset as nh


```r
nh <- nh %>%
  filter(Age >= 18)
```

## Chi Square Goodness of Fit Test

### Example 1. Gender in NHANES  
 
The chi square goodness of fit test assesses whether one factor is distributed as hypothesized. Do observed proportions match expected proportions?


```r
# ?chisq.test
```


For the goodness of fit test, the chisq.test() works by comparing a table of observed proportions to a vector of theoretical proportions `p`

Therefore, we will have to supply a table and a vector as input.

Let's say we want to test whether the 2 genders coded in NHANES were equally sampled for adults.

We will start by making a table of the counts of each Gender.

The [`xtabs()`](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/xtabs.html) function is useful for creating tables of categorical data for use in the chisq.test() function. Let's create the Gender table.


```r
xg <- xtabs(~Gender, data = nh)
xg
```

```
## Gender
## female   male 
##   1856   1851
```

```r
# you can also use the table() function
xgg <- table(nh$Gender)
```

Looks pretty even! But let's do the test anyway. Our hypothesis was that an equal number of males and females were sampled, so we will provide those as the expected probabilities.


```r
genderChi <- chisq.test(xg, p = c(.5, .5))
genderChi
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  xg
## X-squared = 0.006744, df = 1, p-value = 0.9345
```

Check the assumption that expected values > 5. We know it is true given the N, but let's see how to do it.


```r
genderChi$expected
```

```
## female   male 
## 1853.5 1853.5
```

We can see how different the observed are from the expected by calling the residuals out of the chi square object. The default is the Pearson residual which is calculated as $$\frac{O-E}{\sqrt E}$$


```r
xg
```

```
## Gender
## female   male 
##   1856   1851
```

```r
genderChi$expected
```

```
## female   male 
## 1853.5 1853.5
```

```r
genderChi$residuals
```

```
## Gender
##      female        male 
##  0.05806892 -0.05806892
```

```r
(1856 - 1853.5) / sqrt(1853.5)
```

```
## [1] 0.05806892
```

### Example 2. Race in NHANES

The original NHANES survey oversampled minority ethnicities. The nhanes.csv dataset attempted to undo that oversampling so that our sample would be representative of the US population overall. Let's see if our nhanes adults were sampled proportionally to the racial and ethnic proportions reported in the [US Census](https://www.census.gov/quickfacts/fact/table/US/PST045216). Note that this is actually several questions in the Census so I had to fudge a little (I divided Hispanic by 2 to get Mexican and Hispanic separately)

- Asian = .048
- Black = .126
- Hispanic = .089
- Mexican = .089
- Other = .035
- White = .613
 
Create a Race table


```r
xr <- xtabs(~Race, data=nh)
xr
```

```
## Race
##    Asian    Black Hispanic  Mexican    Other    White 
##      215      417      240      288       98     2449
```

Now test our observed race proportions in nh against those from the census. Note that I input them in the same order as the table so they are matched up properly


```r
raceChi <- chisq.test(xr, p = c(.048, .126, .089, .089, .035, .613))
raceChi
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  xr
## X-squared = 64.42, df = 5, p-value = 1.479e-12
```

The result is significant. Let's now compare the expected values to the observed values to see where the differences are.


```r
raceChi$expected
```

```
##    Asian    Black Hispanic  Mexican    Other    White 
##  177.936  467.082  329.923  329.923  129.745 2272.391
```

```r
xr
```

```
## Race
##    Asian    Black Hispanic  Mexican    Other    White 
##      215      417      240      288       98     2449
```

```r
raceChi$residuals
```

```
## Race
##     Asian     Black  Hispanic   Mexican     Other     White 
##  2.778564 -2.317315 -4.950676 -2.308054 -2.786955  3.704856
```

- More Asians & White people in nh than in Census
- Fewer Black, Hispanic, Mexican, & Other people in nha than in Census

## Chi Square Test of Independence

The chi-square test of independence is used to assess the relationship between two factors. The null hypothesis is that the two factors are  independent from one another or that one factor does not affect the other.

The `xtabs()` function is useful for creating contingency tables (with 2 factors) too. Or just use the `table()` function.

### Example 1. Gender and Diabetes in NHANES

Let's see if Gender and Diabetes are independent or related. The null hypothesis is that gender and diabetes are independent, meaning that under the null hypothesis we expect a proportionally equal number of diabetics across each sex.

We'll first create a gender by diabetes status contingency table, and assign it to an object called `xt`


```r
xt <- xtabs(~Gender+Diabetes, data=nh)
xt
```

```
##         Diabetes
## Gender     No  Yes
##   female 1692  164
##   male   1653  198
```

There are two useful functions, `addmargins()` and `prop.table()` that add more information or manipulate how the data is displayed in an `xtabs()` object. The first adds the sums across the rows and the columns. 

Add marginal totals


```r
addmargins(xt)
```

```
##         Diabetes
## Gender     No  Yes  Sum
##   female 1692  164 1856
##   male   1653  198 1851
##   Sum    3345  362 3707
```

By default, `prop.table()` will divide the number of observations in each cell by the total. But you may want to specify _which margin_ you want to get proportions over. Let's do this for the first (row) margin.

Get the proportional table


```r
prop.table(xt)
```

```
##         Diabetes
## Gender           No        Yes
##   female 0.45643377 0.04424063
##   male   0.44591314 0.05341246
```

```r
#each cell divided by grand total

# That isn't really what we wanted. Do this over the first margin (rows) only.
prop.table(xt, margin=1)
```

```
##         Diabetes
## Gender           No        Yes
##   female 0.91163793 0.08836207
##   male   0.89303079 0.10696921
```

Looks like men have slightly higher rates of diabetes than women. But is this significant?


```r
chisq.test(xt)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  xt
## X-squared = 3.4332, df = 1, p-value = 0.0639
```

Males seem to be at slightly higher risk of diabetes than females, but the difference is short of being statistically significant.


### Fisher's Exact Test

An alternative to the chi-square test is [Fisher's exact test](https://en.wikipedia.org/wiki/Fisher%27s_exact_test). 

Rather than relying on a critical value from a theoretical chi-square distribution, Fisher's exact test calculates the _exact_ probability of observing the contingency table as is. It's especially useful when there are very small _n_'s in one or more of the contingency table cells. Both the chi-square and Fisher's exact test give us p-values of approximately 0.06.


```r
fisher.test(xt)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  xt
## p-value = 0.05992
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.9883143 1.5466373
## sample estimates:
## odds ratio 
##   1.235728
```


### Example 2. Race and Insurance in NHANES

Let's create a different contingency table, this time looking at the relationship between race and whether the person had health insurance. Display the table with marginal totals.


```r
xri <- xtabs(~Race+Insured, data=nh)
addmargins(xri)
```

```
##           Insured
## Race         No  Yes  Sum
##   Asian      46  169  215
##   Black      86  330  416
##   Hispanic   89  151  240
##   Mexican   147  141  288
##   Other      33   65   98
##   White     307 2141 2448
##   Sum       708 2997 3705
```

Let's calculate the proportional table to show the proportion of people in each race category having health insurance. (we want to divide by the row sums)


```r
prop.table(xri, margin=1)
```

```
##           Insured
## Race              No       Yes
##   Asian    0.2139535 0.7860465
##   Black    0.2067308 0.7932692
##   Hispanic 0.3708333 0.6291667
##   Mexican  0.5104167 0.4895833
##   Other    0.3367347 0.6632653
##   White    0.1254085 0.8745915
```

Now, let's run a chi-square test of independence.


```r
chisq.test(xri)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  xri
## X-squared = 323.3, df = 5, p-value < 2.2e-16
```

The result is _highly_ significant. In fact, so significant, that the display rounds off the p-value. Let's look at the expected values and compare them to the observed table to see where the differences are.


```r
chisq.test(xri)$expected
```

```
##           Insured
## Race              No        Yes
##   Asian     41.08502  173.91498
##   Black     79.49474  336.50526
##   Hispanic  45.86235  194.13765
##   Mexican   55.03482  232.96518
##   Other     18.72713   79.27287
##   White    467.79595 1980.20405
```

```r
xri
```

```
##           Insured
## Race         No  Yes
##   Asian      46  169
##   Black      86  330
##   Hispanic   89  151
##   Mexican   147  141
##   Other      33   65
##   White     307 2141
```

```r
chisq.test(xri)$residuals
```

```
##           Insured
## Race               No        Yes
##   Asian     0.7667963 -0.3726947
##   Black     0.7296182 -0.3546247
##   Hispanic  6.3698376 -3.0960047
##   Mexican  12.3966594 -6.0252896
##   Other     3.2981916 -1.6030577
##   White    -7.4344164  3.6134341
```

- More uninsured Asian, Black, Hispanic, Mexican, & Other people than expected by chance
- Fewer uninsured White people than expected by chance

A helpful plot for visualizing categorical data called a mosaic plot: (this is a base R plot, not ggplot2). 


```r
mosaicplot(xri)
```

![](ChiSquare_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
# turn plot so it matches xri object
mosaicplot(t(xri), main=NA)
```

![](ChiSquare_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
#get rid of titles and axes labels
mosaicplot(t(xri), main=NA, xlab = NA, ylab = NA)
```

![](ChiSquare_files/figure-html/unnamed-chunk-20-3.png)<!-- -->

```r
#turn labels
mosaicplot(t(xri), main=NA, xlab = NA, ylab = NA, las = 2)
```

![](ChiSquare_files/figure-html/unnamed-chunk-20-4.png)<!-- -->

```r
#add shading by residuals
mosaicplot(t(xri), main=NA, xlab = NA, ylab = NA, las = 2, shade = T)
```

![](ChiSquare_files/figure-html/unnamed-chunk-20-5.png)<!-- -->

This shows the observed data, colored by the residuals. We can easily see the proportion of each race that is insured. The colors indicate where the greatest differences are between observed and expected

Another way to visualize these data is with a stacked barplot


```r
nh %>%
  ggplot(aes(Race)) + geom_bar(aes(fill = Insured))
```

![](ChiSquare_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

To see proportion of insured by each race use `position = "fill"`

```r
#scale up to 100%
nh %>%
  ggplot(aes(Race)) + geom_bar(aes(fill = Insured), position = "fill")
```

![](ChiSquare_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Let's put the races in order so the plot is more readable

```r
#put in order by % insured (manually reorder)
# "White", "Black", "Asian", "Other", "Hispanic", "Mexican"

nh %>%
  mutate(Race = factor(Race, levels = c("White", "Black", "Asian", "Other", "Hispanic", "Mexican"))) %>%
  ggplot(aes(Race)) + geom_bar(aes(fill = Insured), position = "fill")
```

![](ChiSquare_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
