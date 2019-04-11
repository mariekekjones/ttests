---
title: "t-tests in R"
author: "your name here"
date: "April 11, 2019"
output: 
  html_document:
    keep_md: yes
---

## Create new project with NHANES data

I hope you are all using RStudio projects to manage your files and organize your work. Create a new project in a directory on your computer than contains the data files and skeleton script for today's class.

## Load tidyverse package


```r
library(tidyverse)
```

### About NHANES

The data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. You can read a lot more about NHANES on the [CDC's website](http://www.cdc.gov/nchs/nhanes/) or [Wikipedia](https://en.wikipedia.org/wiki/National_Health_and_Nutrition_Examination_Survey). 

NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States.

### Import & inspect

Now, let's load the data and take a look.


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

Let's convert all character variables to factor variables using dplyr's `mutate_if`

And let's remove the children so that all of our analyses will be on adults


```r
nh <- nh %>% mutate_if(is.character, as.factor) %>%
  filter(Age >= 18)
```


### T-tests

A two-sample t-test is used to test for the _difference in means between two groups_. The function for a t-test is `t.test()`. See the help for `?t.test`.

Are there differences in height for males versus females in this dataset?

To assess this question, first we make sure that a t-test is the correct type of analysis. A t-test tests the difference in 2 means - yes that is what we want to do. 

Next we need to decide what type of t-test we need to perform by thinking through the assumptions. Domain specific knowledge and exploratory data analyses will help here.

Here is a basic breakdown of the assumptions and what to do if they are not met:
1. random sampling -- if not met, no statistics
2. indedpendent samples -- met --> indep samples t-test
2. indedpendent samples -- not met --> paired samples t-test
3. normality -- met --> t-test
3. normality -- not met --> U-test
4. equal variance -- met --> equal var t-test
4. equal variance -- not met --> unequal var t-test (Welch's t-test)

For our question about heights in males v. females:
Random sampling -- YES

Independent samples -- YES (men and women are different people - unrelated). Would be a paired t-test if we were assessing height of husband-wife pairs or brother-sister pairs

Normality -- ?? well, we need to assess this. We'll discuss this in a few minutes.

Equal variance. Also called homoscedasticity of variances.
?? we could think about the populations of men and women and what we know about height and conclude reasonably that the variance is equal for height

Let's start with some **exploratory data analyses** for this question. 

I particularly like to start with density plots colored by group. This type of plot helps you see the shape of the distribution of the variable to assess equal variance. Also helps you see if there is a noticeable difference in groups.


```r
ggplot(nh, aes(Height, color = Gender, fill = Gender)) +
  geom_density(alpha = 0.5)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_density).
```

![](t-tests_files/figure-html/ttest_HeightGender-1.png)<!-- -->

```r
#looks like there is a significant difference between the populations.
#looks like we are safe to assume equal variance
```

The last assumption we need to talk about here is normality. Normality can be assessed graphically or via hypothesis tests. There are pros and cons to either approach. 

Graphically, we could look at a **histogram** or a more specialized plot to assess normality called a **QQ plot** (quantile-quantile plot or quantile comparison plot or normal probability plot). 

Histogram of Height


```r
ggplot(nh, aes(Height)) + geom_histogram() 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 26 rows containing non-finite values (stat_bin).
```

![](t-tests_files/figure-html/histograms-1.png)<!-- -->

```r
# More bins
ggplot(nh, aes(Height)) + geom_histogram(bins=50)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_bin).
```

![](t-tests_files/figure-html/histograms-2.png)<!-- -->

```r
# facet by Gender
ggplot(nh, aes(Height)) + 
  geom_histogram(bins=30) + 
  facet_wrap(~ Gender)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_bin).
```

![](t-tests_files/figure-html/histograms-3.png)<!-- -->

#### QQ plots

QQ plots are the best way to assess normality. Remember that the z-scores are plotted on the x-axis and your observed data are plotted on the y-axis. We are looking for the points to line up on the identity line (1:1 ratio between expected and observed)


```r
ggplot(nh, aes(sample = Height)) + 
  geom_qq() + 
  geom_qq_line(color = "blue")
```

```
## Warning: Removed 26 rows containing non-finite values (stat_qq).
```

```
## Warning: Removed 26 rows containing non-finite values (stat_qq_line).
```

![](t-tests_files/figure-html/qqplot-1.png)<!-- -->

```r
# bc assumption must be met for each group, better to facet on group
ggplot(nh, aes(sample = Height))  + 
  geom_qq() + 
  geom_qq_line(color = "blue") +
  facet_wrap(~Gender)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_qq).

## Warning: Removed 26 rows containing non-finite values (stat_qq_line).
```

![](t-tests_files/figure-html/qqplot-2.png)<!-- -->

Learning what is a normal QQ plot looks like is a process. If you are unsure, you could simulate data from a normal distribution and then graph a QQ plot from that data. Repeat several times to see if the normal QQ plots look like yours.


```r
x <- rnorm(n = 1850, mean = 0, sd = 1)
qqnorm(x, pch = 16) #nb, here we cannot use geom_qq bc x is a vector not a df
```

![](t-tests_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Now that we have decided about all of the assumptions, we have concluded that we should perform an equal variance two-sample t-test.


```r
# ?t.test
t.test(Height~Gender, data=nh, var.equal = TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  Height by Gender
## t = -55.713, df = 3679, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -14.29236 -13.32062
## sample estimates:
## mean in group female   mean in group male 
##             161.9618             175.7683
```

Males are significantly taller than females.

## Wilcoxon-Mann-Whitney U

If the assumption of normality is *not met*, then we should perform a Wilcoxon Mann Whitney U test instead of a t-test.

Let's answer the question "Does BMI differ between diabetics and non-diabetics?"

First we'll look at the distributions using a density plot

```r
ggplot(nh, aes(BMI, color = Diabetes, fill = Diabetes)) +
  geom_density(alpha = 0.5)
```

```
## Warning: Removed 31 rows containing non-finite values (stat_density).
```

![](t-tests_files/figure-html/ttest_BMIDiabetes-1.png)<!-- -->

Based on what we see in our exploratory data analysis, we should conduct a WMWU.

```r
wilcox.test(BMI~Diabetes, data=nh)
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  BMI by Diabetes
## W = 366960, p-value < 2.2e-16
## alternative hypothesis: true location shift is not equal to 0
```

Notice how the output from the WMWU does not include summary statistics. Let's calculate those using a grouped summary

```r
nh %>% 
  group_by(Diabetes) %>% 
  summarize(median(BMI, na.rm = TRUE))
```

```
## # A tibble: 2 x 2
##   Diabetes `median(BMI, na.rm = TRUE)`
##   <fct>                          <dbl>
## 1 No                              27.2
## 2 Yes                             31.8
```

```r
#people with diabetes have significantly higher BMI
```

> <i class="fa fa-exclamation-triangle" aria-hidden="true"></i> _**A note on one-tailed versus two-tailed tests:**_ A two-tailed test is usually more appropriate. The hypothesis you're testing here is spelled out in the results ("alternative hypothesis: true difference in means is not equal to 0"). If the p-value is very low, you can reject the null hypothesis that there's no difference in means. Because you may not know _a priori_ whether the difference in means will be positive or negative, you want to do the two-tailed test. However, if we _only_ wanted to test a very specific directionality of effect, we could use a one-tailed test and specify which direction we expect. This is more powerful if we "get it right", but much less powerful for the opposite effect. The p-value of a one-tailed test will be half of that of a two-tailed hypothesis test. BUT again, the **two-tailed test is almost always more appropriate** unless you know what direction your results will go _a priori_.

### YOUR TURN

Use a test of 2 means to assess whether men at or above age 70 show different levels of testosterone than their younger counterparts.

1. First, create the variable `AgeGroup` where >= 70 years old is "aged" and 69 and under are "not aged"


```r
# age groups 18-69 and >=70
nh <- nh %>%
  mutate(AgeGroup = if_else(Age >= 70, "aged", "not aged"))
```

2. Create a plot to explore the distribution of the variables and the relationship between these two variables.


```r
ggplot(nh %>% filter(Gender == "male"), aes(Testosterone, color = AgeGroup, fill = AgeGroup)) +
  geom_density(alpha = 0.5)
```

```
## Warning: Removed 121 rows containing non-finite values (stat_density).
```

![](t-tests_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3. Next, consider test assumptions to decide what analysis to run.

```r
#based on distributions above, choose wilcox
```

4. Run the analysis. Is the association between age gruop and testosterone statistically significant? If so, which group shows higher T?


```r
wilcox.test(Testosterone ~ AgeGroup, data = nh %>% filter(Gender == "male"))
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  Testosterone by AgeGroup
## W = 114050, p-value = 9.088e-05
## alternative hypothesis: true location shift is not equal to 0
```

```r
#to see median and Q1 and Q3
nh %>% 
  filter(Gender == "male") %>%
  group_by(AgeGroup) %>% 
  summarize(Q1 = quantile(Testosterone, probs = .25, na.rm = TRUE), 
            med = median(Testosterone, na.rm = TRUE), 
            Q3 = quantile(Testosterone, probs = .75, na.rm = TRUE))
```

```
## # A tibble: 2 x 4
##   AgeGroup    Q1   med    Q3
##   <chr>    <dbl> <dbl> <dbl>
## 1 aged      242.  334.  471.
## 2 not aged  289.  391.  512.
```

> <i class="fa fa-exclamation-triangle" aria-hidden="true"></i> _**A note on paired versus unpaired t-tests:**_ The t-tests we performed here were unpaired tests. Males and females are different people. The diabetics and nondiabetics are different samples. The aged and not aged individuals are completely independent, separate observations. In these cases, an _unpaired_ test is appropriate. An alternative design might be when data is derived from samples who have been measured at two different time points or locations, e.g., before versus after treatment, left versus right hand, etc. In these cases, a _**paired t-test**_ would be more appropriate. A paired test takes into consideration the intra and inter-subject variability, and is more powerful than the unpaired test. There is a paired = TRUE option for both the t-test and the Wilcoxon test.

## EXTRA MATERIAL

#### Histogram with normal overlay

Let's say we want to overlay a normal curve on top of our Height histogram. This was not as simple as I had hoped, but hopefully you don't find it too bad.


```r
#histogram of height
ggplot(nh, aes(Height)) + 
  geom_histogram(bins = 50)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_bin).
```

![](t-tests_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Now we'll plot a histogram of Height with aesthetic set to density. Note the y-axis is now scaled as density rather than raw counts

```r
ggplot(nh, aes(Height)) + 
  geom_histogram(aes(y = ..density..), bins = 50)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_bin).
```

![](t-tests_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#looks same as histogram of counts -- good
```

Finally, we will add a normal curve on top of the histogram layer. We'll add dnorm to the stat_function layer and specify our mean and sd so R knows which normal curve to draw.

```r
mh <- mean(nh$Height, na.rm = T)
sdh <- sd(nh$Height, na.rm = T)

ggplot(nh, aes(Height)) + 
  geom_histogram(aes(y = ..density..), bins = 50) +
  stat_function(
    fun = dnorm, 
    args = c(mean = mh, sd = sdh), 
    color = "darkred", 
    size = 1.5
    )
```

```
## Warning: Removed 26 rows containing non-finite values (stat_bin).
```

![](t-tests_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


#### Tests of normality

Certain fields love hypothesis tests of normality and sometimes reviewers will specifically request one. There is a theoretical problem with trying to _prove_ a null hypothesis and they are known to reject the null when sample sizes are large. My best advice is to use your brain, subject matter expertise, and graphical assessments as much as possible, but in case you are forced to do a hypothesis test for normality check out shapiro.test()

The least awful seems to be the Shapiro-Wilk (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3693611/) but there are several options (Kolmogorov-Smirnov, Anderson-Darling, Lillefors, etc)


