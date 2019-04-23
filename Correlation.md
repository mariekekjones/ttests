---
title: "Correlation"
author: "BIMS8380"
date: "4/23/2019"
output: 
  html_document:
    keep_md: yes
---

This lesson will introduce you to correlation analyses in R. We will cover how to run the test, how to check the assumptions and what to do in case assumptions are not met.

In this lesson, the data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States. It began in the 1960s and since 1999 examines a nationally representative sample of about 5,000 people each year. The NHANES interview includes demographic, socioeconomic, dietary, and health-related questions. The physical exam includes medical, dental, and physiological measurements, as well as several standard laboratory tests. NHANES is used to determine the prevalence of major diseases and risk factors for those diseases. NHANES data are also the basis for national standards for measurements like height, weight, and blood pressure. Data from this survey is used in epidemiology studies and health sciences research, which help develop public health policy, direct and design health programs and services, and expand the health knowledge for the Nation.

We are using a small slice of this data. We're only using a handful of variables from the 2011-2012 survey years on about 5,000 individuals.

## First we will load the nhanes.csv dataset


```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.0       ✔ purrr   0.3.1  
## ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
## ✔ tidyr   0.8.3       ✔ stringr 1.4.0  
## ✔ readr   1.3.1       ✔ forcats 0.4.0
```

```
## ── Conflicts ───────────────────────────────────────────── tidyverse_conflicts() ──
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

Now we will filter the nh dataset so it just contains adults (>= 18 years old) and save the new dataset back into nh


```r
nh <- nh %>%
  filter(Age >= 18)
```

# Example 1

The first analysis we will run is a correlation between Weight and Pulse

### EDA

Create a scatterplot to see the relationship between these variables


```r
nh %>%
  ggplot(aes(Weight, Pulse)) + geom_point()
```

```
## Warning: Removed 145 rows containing missing values (geom_point).
```

![](Correlation_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

It does not look like there is a strong correlation between these variables, but the cloud of points is sort of elongated out towards the top right

### Hypotheses and alpha
Null hypothesis is that there is no relationship between Weight and Pulse
Alternative hypothesis is that there is a significant relationship between Weight and Pulse

I can see testing both sides -- as someone gets heavier, their Pulse increases because their heart is working harder -- OR -- as someone gets heavier, their Pulse decreases because they are generally less active overall. Therefore, I will do a two-tailed test

alpha = 0.05

### Check assumptions
Random sampling -- met

Need to make sure each variable is normally distributed


```r
nh %>%
  ggplot(aes(sample = Pulse)) + geom_qq() + geom_qq_line()
```

```
## Warning: Removed 123 rows containing non-finite values (stat_qq).
```

```
## Warning: Removed 123 rows containing non-finite values (stat_qq_line).
```

![](Correlation_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
nh %>%
  ggplot(aes(sample = Weight)) + geom_qq() + geom_qq_line()
```

```
## Warning: Removed 27 rows containing non-finite values (stat_qq).
```

```
## Warning: Removed 27 rows containing non-finite values (stat_qq_line).
```

![](Correlation_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
# not normal
# we will still run a Pearson correlation, but it is not robust to departures in normality, so let's then assess using Kendall's tau
```

Linear relationship
Homoscedasticity

To assess both of these, let's look again at the scatterplot


```r
nh %>%
  ggplot(aes(Weight, Pulse)) + geom_point()
```

```
## Warning: Removed 145 rows containing missing values (geom_point).
```

![](Correlation_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

No reason to believe that there is a non-linear relationship
No obvious heteroscedasticity in variance as weight increases or as Pulse increases

### Run test


```r
# ?cor.test
cor.test(~ Weight + Pulse, data = nh)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  Weight and Pulse
## t = 4.2415, df = 3560, p-value = 2.277e-05
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.03815543 0.10350927
## sample estimates:
##        cor 
## 0.07090845
```

r = 0.071 which is positive (as weight increases, pulse increases), but low in magnitude
We get a significant p-value but I am not convinced of its real-world relevance

Now with Kendall


```r
cor.test(~ Weight + Pulse, data = nh, method = "k")
```

```
## 
## 	Kendall's rank correlation tau
## 
## data:  Weight and Pulse
## z = 1.3657, p-value = 0.172
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##       tau 
## 0.0156425
```

In this case, due to the violations in normality, I would report Kendall's tau rather than Pearson's rho

# Example 2
Let's examine testosterone levels in males as they age.

H0: there is no relationship between age and testosterone
H1: as age increases, testosterone decreases
 
### EDA

Let's start by plotting the data


```r
nh %>%
  filter(Gender == "male") %>%
  ggplot(aes(Age, Testosterone)) + geom_point()
```

```
## Warning: Removed 121 rows containing missing values (geom_point).
```

![](Correlation_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

A few things stand out here. It looks like anyone above age 80 was classified as 80, so I will remove those datapoints in my analysis. Second, there is one extreme outlier with a testosterone level above 1500. A quick internet search tells me that this is very much outside the normal range for testosterone levels (250 - 1100 ng/dl). Therefore, I will remove this datapoint before running my analysis.

Let's filter out strange data


```r
nhT <- nh %>%
  filter(Gender == "male" & Age < 80 & Testosterone < 1500)
```

And create a new scatterplot


```r
nhT %>%
  ggplot(aes(Age, Testosterone)) + geom_point()
```

![](Correlation_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Now assess the assumptions
Random sampling -- met

Based on the scatterplot, I saw no violations in homoscedasticity of variance
Based on the scatterplot, I saw no violations of assumption of a linear relationship between age and testosterone

Normality of each variable


```r
nhT %>%
  ggplot(aes(sample = Testosterone)) + geom_qq() + geom_qq_line()
```

![](Correlation_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
nhT %>%
  ggplot(aes(sample = Age)) + geom_qq() + geom_qq_line()
```

![](Correlation_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
# neither is normal
# let's assess using Spearman's rho and Kendall's tau
```

### Run the test


```r
cor.test(~ Age + Testosterone, data = nhT, alternative = "less", method = "s")
```

```
## Warning in cor.test.default(x = c(35, 57, 57, 57, 57, 28, 28, 28, 28, 38, :
## Cannot compute exact p-value with ties
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  Age and Testosterone
## S = 829530000, p-value = 5.527e-06
## alternative hypothesis: true rho is less than 0
## sample estimates:
##        rho 
## -0.1079741
```

```r
#' r is -0.11 and is highly significant
#' As age increases, testosterone decreases

cor.test(~ Age + Testosterone, data = nhT, alternative = "less", method = "k")
```

```
## 
## 	Kendall's rank correlation tau
## 
## data:  Age and Testosterone
## z = -4.5152, p-value = 3.164e-06
## alternative hypothesis: true tau is less than 0
## sample estimates:
##         tau 
## -0.07488661
```

```r
#' tau is -0.07 and is highly significant
#' As age increases, testosterone decreases
```

It would be nice to see the tau printed on the scatterplot, so let's do that using the ggplot annotate function to add some text

```r
#save kendall tau result to use in plot
t <- cor.test(~ Age + Testosterone, data = nhT, alternative = "less", method = "k")

#make basic plot
nhT %>%
  ggplot(aes(Age, Testosterone)) + geom_point()
```

![](Correlation_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#add annotation layer
nhT %>%
  ggplot(aes(Age, Testosterone)) + geom_point() + 
  annotate("text", x = 23, y = 0, label = paste0("tau = ",round(t$estimate, 3)), size = 5)
```

![](Correlation_files/figure-html/unnamed-chunk-13-2.png)<!-- -->
