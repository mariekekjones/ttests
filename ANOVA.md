---
title: "ANOVA"
author: "Your name here"
date: "4/16/2019"
output: 
  html_document:
    keep_md: yes
---


### Create new project with NHANES data

I hope you are all using RStudio projects to manage your files and organize your work. Create a new project in a directory on your computer than contains the data files and skeleton script for today's class.

### Load tidyverse package


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

## Relationship between ANOVA, t-test and linear regression

Where t-tests and their nonparametric alternatives are used for assessing the differences in means between two groups, ANOVA is used to assess the significance of differences in means between multiple groups. 

In fact, a t-test is just a specific case of ANOVA when you only have two groups. And both t-tests and ANOVA are just specific cases of linear regression, where you're trying to fit a model describing how a continuous outcome (e.g., BMI) changes with some predictor variable (e.g., diabetic status, race, age, etc.). Typically when we have a discrete predictor, we call it ANOVA and when we have a continuous predictor, we call it linear regression. However, we can perform linear regression with a categorical predictor too. 

So what is the real difference?

Well, the distinction is largely semantic. With a linear model you're asking, "do levels of a categorical variable affect the response?" where with ANOVA or t-tests you're asking, "does the mean response differ between levels of a categorical variable?"

Let's examine the relationship between Height and relationship status (`RelationshipStatus` was derived from `MaritalStatus`, coded as _Committed_ if MaritalStatus is Married or LivePartner, and _Single_ otherwise). Let's first do this with a t-test, and for now, let's assume that the variances between groups _are_ equal.


```r
#EDA
nh %>%
  filter(!is.na(RelationshipStatus)) %>%
  ggplot(aes(Height, col = RelationshipStatus, fill = RelationshipStatus)) +
  geom_density(alpha = .5)
```

```
## Warning: Removed 26 rows containing non-finite values (stat_density).
```

![](ANOVA_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#equal var and normal --> pooled var t-test
t.test(Height~RelationshipStatus, data=nh, var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  Height by RelationshipStatus
## t = 5.5521, df = 3557, p-value = 3.029e-08
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.254015 2.623174
## sample estimates:
## mean in group Committed    mean in group Single 
##                169.5930                167.6544
```
It looks like single people have a lower mean height than those in a committed relationship. This difference of 2 inches is trivial, but is none the less, statistically significant. 

Let's do the same test in a linear modeling framework. First, let's create the fitted model and store it in an object called `fit`. 


```r
fit <- lm(Height~RelationshipStatus, data=nh)
```

You can display the object itself, but that isn't too interesting. You can get the more familiar ANOVA table by calling the `anova()` function on the `fit` object. More generally, the `summary()` function on a linear model object will tell you much more. (Note this is different from dplyr's **summarize** function).


```r
fit
```

```
## 
## Call:
## lm(formula = Height ~ RelationshipStatus, data = nh)
## 
## Coefficients:
##              (Intercept)  RelationshipStatusSingle  
##                  169.593                    -1.939
```

```r
anova(fit)
```

```
## Analysis of Variance Table
## 
## Response: Height
##                      Df Sum Sq Mean Sq F value    Pr(>F)    
## RelationshipStatus    1   3176  3176.1  30.826 3.029e-08 ***
## Residuals          3557 366491   103.0                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov(fit))
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = fit)
## 
## $RelationshipStatus
##                       diff       lwr       upr p adj
## Single-Committed -1.938594 -2.623174 -1.254015     0
```

```r
summary(fit)
```

```
## 
## Call:
## lm(formula = Height ~ RelationshipStatus, data = nh)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.154  -7.293  -0.254   7.207  32.246 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              169.5930     0.2175 779.735  < 2e-16 ***
## RelationshipStatusSingle  -1.9386     0.3492  -5.552 3.03e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.15 on 3557 degrees of freedom
##   (148 observations deleted due to missingness)
## Multiple R-squared:  0.008592,	Adjusted R-squared:  0.008313 
## F-statistic: 30.83 on 1 and 3557 DF,  p-value: 3.029e-08
```

Go back and re-run the t-test assuming equal variances as we did before. Now notice a few things:


```r
t.test(Height~RelationshipStatus, data=nh, var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  Height by RelationshipStatus
## t = 5.5521, df = 3557, p-value = 3.029e-08
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.254015 2.623174
## sample estimates:
## mean in group Committed    mean in group Single 
##                169.5930                167.6544
```

1. The p-values from all three tests (t-test, ANOVA, and linear regression) are all identical (p=3.03e-08). This is because the tests are all identical: a t-test _is_ a specific case of ANOVA, which _is_ a specific case of linear regression. 

1. The test statistics are all related. The _t_ statistic from the t-test is **5.552**, which is the same as the t-statistic from the linear regression. If you square that, you get **30.83**, the _F_ statistic from the ANOVA. 

1. The `t.test()` output shows you the means for the two groups, Committed and Single. Just displaying the `fit` object itself or running `summary(fit)` shows you the coefficients for a linear model. Here, the model assumes the "baseline" RelationshipStatus level is _Committed_ (first alphabetically), and that the _intercept_ in a regression model (e.g., $\beta_{0}$ in the model $Y = \beta_{0} +  \beta_{1}X$) is the mean of the baseline group (169.5930 is the mean Height for Committed people). Being _Single_ results in a decrease in Height of 1.94 cm. This is the $\beta_{1}$ coefficient in the model. You can easily change the ordering of the levels. See the help for `?factor`, and check out the [**forcats** package](http://forcats.tidyverse.org/), which provides tools **for** manipulating **cat**egorical variables.

## ANOVA with 3+ groups

Recap: t-tests are for assessing the differences in means between _two_ groups. A t-test is a specific case of ANOVA, which is a specific case of a linear model. Let's run ANOVA, but this time looking for differences in means between more than two groups.

Let's look at the relationship between smoking status (Never, Former, or Current), and Height.


```r
fit1 <- lm(Height~SmokingStatus, data=nh)

summary(fit1)
```

```
## 
## Call:
## lm(formula = Height ~ SmokingStatus, data = nh)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.008  -7.208  -0.008   6.999  32.392 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         171.4761     0.3824 448.464  < 2e-16 ***
## SmokingStatusFormer  -1.6747     0.5147  -3.254  0.00115 ** 
## SmokingStatusNever   -3.9685     0.4434  -8.950  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.07 on 3558 degrees of freedom
##   (146 observations deleted due to missingness)
## Multiple R-squared:  0.02481,	Adjusted R-squared:  0.02426 
## F-statistic: 45.26 on 2 and 3558 DF,  p-value: < 2.2e-16
```

The F-test on the ANOVA table tells us that there _is_ a significant difference in means between current, former, and never smokers (p=$< 2.2*10^{-16}$). However, the linear model output might not have been what we wanted. Because the default handling of categorical variables is to treat the alphabetical first level as the baseline, "Current" smokers are treated as baseline, and this mean becomes the intercept, and the coefficients on "Former" and "Never" describe how those groups' means differ from current smokers. 

What if we wanted "Never" smokers to be the baseline, followed by Former, then Current? Have a look at `?factor` and `?relevel` to change the factor levels. As mentioned above, if you are dealing with several factor variables or factor variables with many levels, the **forcats** package is a great resource.


```r
?factor
# Look at nha$SmokingStatus
nh$SmokingStatus
```

Let's relevel Smoking Status to make 'Never' the reference categrory. Then we'll take a look

```r
factor(nh$SmokingStatus, levels = c('Never', 'Former', 'Current'))
```

```
##    [1] Current Never   Never   Never   Never   Never   Former  Former 
##    [9] Former  Former  Never   Never   Never   Never   Never   Never  
##   [17] Never   Never   Never   Never   Never   Never   Never   <NA>   
##   [25] Never   Never   Never   Former  Never   Current Former  Former 
##   [33] Former  Never   Never   <NA>    <NA>    Former  Never   Former 
##   [41] Former  Current Current Never   Never   Never   Never   Never  
##   [49] Former  Former  Never   Never   Never   Never   Never   Never  
##   [57] Never   Never   Never   Never   Never   Never   Never   Never  
##   [65] Never   Never   Never   Never   Never   <NA>    Never   Never  
##   [73] Never   Current Current Current Former  Former  Former  Current
##   [81] Never   Former  Never   Never   Never   Never   Never   Current
##   [89] Former  Former  Former  Former  Former  Never   Never   Never  
##   [97] Never   Never   Current Current Current Current Never   Never  
##  [105] Never   Never   Never   Never   Never   Never   Former  Never  
##  [113] Never   Never   Never   Former  Former  Never   Never   Never  
##  [121] Never   Former  Former  Never   Never   Former  Never   Never  
##  [129] Never   Current Never   Never   Never   Never   Never   Never  
##  [137] Never   Former  Former  Never   Never   Current Current Never  
##  [145] Never   Current Never   Never   Never   Current Never   Never  
##  [153] Never   Current Current Current Current Never   Never   Never  
##  [161] Never   Never   Never   Never   Never   Never   Never   Never  
##  [169] Former  Current Never   Never   Never   Current Current Current
##  [177] Current Current Never   Never   Never   Never   Never   Former 
##  [185] Former  Never   Never   Current Never   Never   Never   Never  
##  [193] Never   Never   Never   Current Former  Former  Former  Former 
##  [201] Former  Never   Former  Current Former  Never   Never   Never  
##  [209] Never   Never   Former  Never   Never   Former  Never   Never  
##  [217] Never   Never   Never   Never   Never   Never   Never   Never  
##  [225] Former  Former  <NA>    Never   Never   Former  Never   Former 
##  [233] <NA>    Former  Current Never   Former  Never   Never   Never  
##  [241] Never   Never   Former  Former  Former  Former  Never   Current
##  [249] Former  Former  Former  Never   Never   Never   Never   Never  
##  [257] Former  Former  Current Former  Current Never   Never   Never  
##  [265] Current <NA>    <NA>    Current Never   Current Never   Never  
##  [273] Never   Never   Former  Former  Current Current Never   Never  
##  [281] Never   Never   Former  Former  Former  Current Never   Never  
##  [289] Never   Former  Never   Current Current Current Current Never  
##  [297] Former  Former  Former  Never   Never   Never   Never   Never  
##  [305] Never   Never   Former  Current Never   Never   Former  Never  
##  [313] Never   Never   <NA>    <NA>    <NA>    <NA>    Current Former 
##  [321] Former  Never   Never   Never   Former  Never   Former  Current
##  [329] Current Current <NA>    <NA>    Never   Never   Never   Never  
##  [337] Never   Never   Never   Current Former  Former  Former  Former 
##  [345] Former  Never   Never   Never   Never   Never   Never   Never  
##  [353] Never   Never   Never   Never   Current Current Current Current
##  [361] Never   Never   Never   Never   Never   Never   Never   Never  
##  [369] Current Current Former  Former  Former  Never   Never   Current
##  [377] Never   Never   Never   Never   Never   Never   Never   Never  
##  [385] Never   Never   Never   Never   Never   Never   <NA>    <NA>   
##  [393] Never   Never   Never   <NA>    Former  Former  Former  Never  
##  [401] Current Current Current Current Current Current Current Never  
##  [409] Never   Never   Never   Never   Never   Never   Former  Former 
##  [417] Former  Current Current Current Current Former  Current Current
##  [425] Former  Former  Never   Never   Current Current Current Never  
##  [433] Never   Never   Former  Former  Never   Never   Current Never  
##  [441] Never   Current Current Never   Never   Never   Never   Former 
##  [449] Former  Former  Never   Former  Never   Never   Never   Never  
##  [457] Never   Never   Never   Never   Current Current Current Never  
##  [465] Never   Current Current Current Former  Never   Current Current
##  [473] Current Current Current Current Never   Never   Never   Never  
##  [481] Never   Never   Current Never   Former  Current Current Former 
##  [489] Never   Never   Never   Never   Never   Never   Former  Never  
##  [497] Never   Former  Former  Former  Former  Former  Never   Never  
##  [505] Never   Never   Current Current Never   Former  Current Never  
##  [513] Never   Never   Never   Never   Never   Never   Never   Never  
##  [521] Never   Former  Former  Former  <NA>    Current Current Current
##  [529] Current Current Former  Former  Former  Former  Former  Current
##  [537] Current Former  Former  Former  Never   Never   Never   Never  
##  [545] Never   Never   Never   Never   Never   Never   Never   Never  
##  [553] Former  Never   Never   Never   Never   Never   Former  Never  
##  [561] Never   Never   Never   Never   Never   Never   Never   Current
##  [569] <NA>    <NA>    Never   Never   Never   Current Never   Never  
##  [577] Never   Never   Never   Never   Former  Former  Never   Former 
##  [585] Never   Current Never   Never   Former  Former  Never   Never  
##  [593] Never   <NA>    Never   Never   Never   Never   Never   Former 
##  [601] Former  Former  Former  <NA>    <NA>    Never   Never   Never  
##  [609] Never   Former  Never   Never   Never   Never   Current Never  
##  [617] Never   Never   Former  Former  Current Never   Never   Never  
##  [625] Never   Never   Never   Never   Never   Never   Never   Never  
##  [633] Never   Never   Never   Never   Current Never   Never   Current
##  [641] Never   Never   Current Current Current Current Current Current
##  [649] Current Never   Never   Never   Never   Former  Former  Never  
##  [657] Never   Current Current Current Current Former  Former  Current
##  [665] Current Current Current Current Never   Current Never   Former 
##  [673] Never   Never   Never   Never   Former  Former  Former  Never  
##  [681] Never   Never   Never   Never   Never   Never   Former  Former 
##  [689] Former  Never   Never   Never   Never   Never   Never   Never  
##  [697] <NA>    Never   Never   Former  Never   Never   Former  Never  
##  [705] Former  Former  Current Current Former  Never   Never   <NA>   
##  [713] Former  Former  Former  Former  Current Never   <NA>    Current
##  [721] Current Current Current Current Current Never   Never   Never  
##  [729] Never   Never   Never   Current Never   Never   Never   Never  
##  [737] Current Current Current Current Former  Former  Never   Former 
##  [745] Former  Never   Never   Never   Current Current Former  Never  
##  [753] Never   Never   Never   Current Former  Former  Former  Former 
##  [761] Never   Never   Never   Never   Former  Never   Former  Former 
##  [769] Former  Never   <NA>    Never   Never   Never   Never   Current
##  [777] Never   Former  Never   Never   Former  Former  Current Current
##  [785] Never   Never   Never   Never   Never   Current <NA>    Current
##  [793] Never   Never   Never   Never   Former  Former  Former  <NA>   
##  [801] Never   Former  Never   Never   Never   Former  Former  Never  
##  [809] Never   Never   Never   Never   Former  Former  Former  Former 
##  [817] Former  Never   Former  Current Current Former  Former  Former 
##  [825] Never   Never   Never   Current Current <NA>    Never   Never  
##  [833] Former  Never   Never   Current Never   Never   Current Never  
##  [841] Never   Current Never   Never   Never   Current Never   Never  
##  [849] Never   Never   Never   Never   Never   Current Current Current
##  [857] Never   Former  Former  Never   Current Current Former  Never  
##  [865] Former  Never   Former  Former  Current Former  Former  Never  
##  [873] Never   Never   Never   Never   Never   Never   Never   Never  
##  [881] Never   Never   Current Current Current Current Current <NA>   
##  [889] Never   Never   Never   Never   Never   Former  Never   Never  
##  [897] Never   <NA>    Former  Current Never   Current Never   Former 
##  [905] Current Former  Never   Former  <NA>    Never   Current Current
##  [913] Current Current Former  Former  Current Former  Former  Never  
##  [921] Former  Former  Current Current Current Never   Never   Never  
##  [929] Never   Never   Never   Former  Former  Former  Former  Former 
##  [937] Former  Never   Never   Never   Former  Never   Former  Former 
##  [945] Former  Never   Never   Never   Never   Never   Former  Former 
##  [953] Never   Former  Never   Never   Never   Never   Current Never  
##  [961] Never   Never   Never   Former  Former  Current Never   Never  
##  [969] Never   Never   Never   Never   Never   Current Current Former 
##  [977] Former  Never   Never   Current Current Current Current Current
##  [985] Current Current Never   Never   Never   Never   Never   Never  
##  [993] Never   Never   Never   Never   Never   Never   Never   Former 
## [1001] Never   Current Never   Never   Former  Former  Never   Never  
## [1009] Never   <NA>    Never   Current <NA>    Former  Former  Former 
## [1017] Never   Never   Never   Never   Never   Never   Former  Former 
## [1025] Former  Never   Never   Former  Former  Former  Former  Current
## [1033] Current Current Current Current Never   Never   <NA>    <NA>   
## [1041] <NA>    <NA>    <NA>    <NA>    Never   Never   Never   Current
## [1049] Current Current Current Current Never   Former  Never   Never  
## [1057] Never   Never   Never   Never   Never   Never   Never   Current
## [1065] Never   Never   Never   Never   Never   Never   Never   Never  
## [1073] Never   Never   Never   Never   Never   Never   Never   Never  
## [1081] Never   Former  Former  Former  Current Current <NA>    Never  
## [1089] Never   Current Never   Former  Former  Former  Former  Current
## [1097] Former  Never   Never   Never   Never   Never   Never   Never  
## [1105] Never   Never   Never   Never   Never   Current Never   Never  
## [1113] Never   Never   Never   Never   Never   Never   Former  Former 
## [1121] Former  Former  Never   Former  Never   Never   Never   Never  
## [1129] Never   Never   Never   Never   Former  Former  Never   Former 
## [1137] Never   Former  Former  Never   Never   Never   Never   Never  
## [1145] Never   Current Never   Never   Never   Never   Never   Never  
## [1153] Never   Former  Former  Former  Former  Former  Current Never  
## [1161] Never   Current Never   Never   Never   Never   Never   Never  
## [1169] Current Never   Never   Former  Former  Former  Never   Former 
## [1177] Former  Never   Never   Never   Never   Never   Never   Never  
## [1185] Never   Never   Former  Former  Former  Never   Current Former 
## [1193] Former  Former  Former  Former  Former  Former  Current Current
## [1201] Current Never   Never   Never   Never   Current Current Never  
## [1209] Never   Never   Never   Never   Never   Never   Never   Never  
## [1217] Never   Never   Current Never   Never   Never   Never   Never  
## [1225] Never   Never   Never   Never   Current Former  Current Current
## [1233] Current Never   Never   Never   Never   Never   Current Former 
## [1241] Never   Former  Never   Never   Current Current Current Current
## [1249] Current Current Current Never   Never   Never   Never   Never  
## [1257] Never   Never   Never   Never   Never   Never   Never   Current
## [1265] <NA>    Never   Never   Never   Never   Never   Never   Never  
## [1273] Never   Former  Never   Never   Never   Never   Never   Never  
## [1281] Never   Current Former  Never   Never   <NA>    Never   Never  
## [1289] Never   Never   Never   Never   Never   Never   Never   Never  
## [1297] Never   Never   Never   Never   Former  Never   Current Current
## [1305] Current Never   Never   Never   Never   Never   Never   Never  
## [1313] Never   Never   Never   Former  Never   Never   Never   Never  
## [1321] Never   Former  Former  Former  Former  Former  <NA>    <NA>   
## [1329] Never   Never   Never   Never   Never   Never   Never   Never  
## [1337] Never   Current Never   Never   Current Current Current Current
## [1345] Current Never   Former  Former  <NA>    <NA>    <NA>    Former 
## [1353] Former  Never   Never   Never   Never   Never   Former  Current
## [1361] Current Never   Never   Never   Never   Never   Never   Never  
## [1369] Never   Former  Former  Former  Former  Former  Former  Never  
## [1377] Current Current Current <NA>    Current Current Former  <NA>   
## [1385] <NA>    Current Current Never   Never   Never   Never   Never  
## [1393] Never   Never   Never   Never   Never   Never   Never   Never  
## [1401] Never   Never   Never   Never   Never   Never   Current Current
## [1409] Former  Never   Never   Never   Never   Never   Never   Never  
## [1417] Never   Current Never   Never   Former  Never   Never   Former 
## [1425] Current Current Current Current Current Never   Never   Never  
## [1433] Current Former  Current Never   Never   Never   Current Never  
## [1441] Never   Never   Never   Never   Current Current Current Never  
## [1449] Never   Never   Never   Never   Never   Never   Never   Never  
## [1457] Never   Never   Never   Never   Never   Former  Current Former 
## [1465] Never   Former  Current Never   Former  Never   Never   Never  
## [1473] Never   Never   Never   Former  Former  Former  Former  Former 
## [1481] Former  Former  Former  Never   Never   Current Never   Never  
## [1489] Never   Never   Never   Never   Former  Current Never   Current
## [1497] Never   Former  Never   Never   Never   Never   Never   Never  
## [1505] Never   Former  Never   Current Never   Never   Never   Never  
## [1513] Never   Never   Current <NA>    Never   Former  Never   Never  
## [1521] Never   Never   Never   Never   Former  Former  Former  Never  
## [1529] Never   Former  Never   Never   Former  Former  Former  Former 
## [1537] Never   Never   Former  Current Current Never   Never   Never  
## [1545] Never   Former  Never   Current Former  Former  Former  Never  
## [1553] Never   Former  Former  Never   Never   Current Current Never  
## [1561] Former  Former  Current Current Never   Never   Never   Never  
## [1569] Current Current Current Never   Never   Never   Never   Never  
## [1577] Never   Never   Never   Never   Never   Never   Never   Never  
## [1585] Former  Current Never   Never   Never   Former  Former  Never  
## [1593] Never   Never   Current Former  Never   Current Current Never  
## [1601] Never   Never   Never   Never   Never   Never   Never   Never  
## [1609] Current Never   Current Current Current Current Former  Never  
## [1617] Former  Current Current Never   Former  Former  Former  Former 
## [1625] Former  Former  Current Former  Never   Current <NA>    <NA>   
## [1633] Former  Current Current Former  Never   Never   Never   Former 
## [1641] Former  Former  Never   Never   Never   Never   Former  Never  
## [1649] Never   Never   Never   Never   Current Never   Never   Never  
## [1657] Never   Never   Former  Former  Never   Never   Never   Never  
## [1665] Never   Never   Former  Former  Former  Former  Former  Former 
## [1673] Never   Never   Never   Never   Never   Never   <NA>    Never  
## [1681] Never   Never   Never   Never   Former  Former  Current Current
## [1689] Current Never   Never   Never   Never   Never   Never   Never  
## [1697] Never   Never   Never   Never   Never   Current Current Never  
## [1705] Current Current Former  <NA>    Former  Former  Former  Former 
## [1713] Never   Former  Never   Never   Never   Never   Current Never  
## [1721] Never   Never   Never   Never   Never   Never   Never   Never  
## [1729] Former  Never   Never   Never   Never   Former  Former  Former 
## [1737] Never   Former  Former  Never   Never   Current Never   Current
## [1745] Current Never   Never   Never   Former  Former  Former  Never  
## [1753] Former  Former  Former  Never   <NA>    Never   Never   Never  
## [1761] Never   Never   Never   Current <NA>    Never   Never   Never  
## [1769] Never   Former  Never   Never   Never   Never   Never   Never  
## [1777] Never   Never   Former  Current Never   Never   Never   Never  
## [1785] Former  Never   Never   Never   Never   Never   Current Current
## [1793] Current Never   Never   Former  Former  Current Former  Never  
## [1801] Never   Former  Former  Never   Never   Never   Never   Never  
## [1809] Current Current Never   Never   Current Current Former  Former 
## [1817] Former  Former  Former  Never   Never   Never   Never   Never  
## [1825] Never   Never   Former  Former  Former  Former  Never   Never  
## [1833] Never   Never   Current Former  Former  Former  <NA>    Never  
## [1841] Never   Never   Never   Former  Never   Former  Former  Former 
## [1849] <NA>    Current Current Current Current Never   Current Never  
## [1857] Never   Former  Former  Never   <NA>    Current Current Current
## [1865] Never   Never   Never   Never   Former  Former  Never   Former 
## [1873] Current Current Former  Former  Former  Former  Former  Former 
## [1881] Former  Former  Current Former  Current Current Current Current
## [1889] Former  Former  Former  Never   Never   Never   Never   Former 
## [1897] Current Current Former  Current Current Never   Never   Former 
## [1905] Former  Former  Never   Former  Former  Former  Never   Never  
## [1913] Never   Former  Current <NA>    Never   Never   Never   Never  
## [1921] Never   Former  Never   Never   <NA>    Never   Never   Never  
## [1929] Never   Never   Never   Never   Former  Former  Never   Current
## [1937] Former  Former  Former  Former  Former  Former  Current Current
## [1945] Current Never   Current Current Never   Never   Never   Current
## [1953] Former  Never   Never   Never   Never   Never   Former  Never  
## [1961] Never   Never   Never   Never   Former  Current Former  Current
## [1969] Current Never   Never   Never   Former  Former  Former  Never  
## [1977] Never   Never   Former  Former  Former  Never   Current Current
## [1985] Never   Never   Never   Never   Never   Never   Former  Former 
## [1993] Former  Former  Never   Current Current Current Current Current
## [2001] Never   Never   Never   Current Current Never   Never   Never  
## [2009] Never   Never   Never   Current Current Current Never   Never  
## [2017] Never   Current Never   Never   Current Current Current Current
## [2025] Current Current Never   Never   Never   Former  Current Never  
## [2033] Never   Never   Former  Former  Former  Former  Former  Never  
## [2041] Current Current Never   Never   Never   Current Current Current
## [2049] Current Current Current Current Never   Former  Never   Never  
## [2057] Current Never   Never   Former  Former  Never   Never   Current
## [2065] Current Current Former  Former  Former  Never   Never   Current
## [2073] Current Current Never   Never   Former  Former  Never   Never  
## [2081] Current Current Current Never   Former  Never   Never   Current
## [2089] <NA>    <NA>    Current Former  Former  Current Never   Former 
## [2097] Never   Never   Never   Never   Current <NA>    <NA>    Never  
## [2105] Never   Never   Former  Never   Never   Never   Never   Never  
## [2113] Current Never   Never   Never   Never   Never   Never   Never  
## [2121] Never   Current Former  Former  Former  Former  Former  Former 
## [2129] Former  Former  Never   Current Current Never   Former  Former 
## [2137] Former  Former  Former  Never   Never   Never   Never   Never  
## [2145] Former  Former  Former  Former  Former  Former  Former  Never  
## [2153] Former  Former  Former  Former  Never   Former  Never   Never  
## [2161] Never   Former  Never   Never   Never   Never   Never   Never  
## [2169] Never   Never   Former  Former  Never   Current Former  Former 
## [2177] Never   Current Current Never   Never   Never   Current Current
## [2185] Current Current Current Current Current Current Current Never  
## [2193] Never   Never   Never   Never   Current Current Never   Never  
## [2201] Never   Never   Never   Never   Never   Never   Never   Former 
## [2209] <NA>    Never   Never   Former  Former  Former  Former  Former 
## [2217] Former  Never   Former  Former  Former  Current Current Current
## [2225] Current <NA>    Never   Current Never   Never   Never   Never  
## [2233] Never   Never   Never   Never   Never   Never   Former  Never  
## [2241] Current Never   Never   Current Former  Former  Former  Never  
## [2249] Never   Current Current Never   Never   Never   Never   Current
## [2257] Current Former  Current Current Former  Former  Former  Never  
## [2265] Never   Current Current Never   Never   Never   Never   Never  
## [2273] Former  Former  Never   Former  Current Current Never   Never  
## [2281] Former  Former  Former  Former  Never   Never   Never   Never  
## [2289] Never   Never   Never   Never   Never   Never   Never   Never  
## [2297] Never   Never   Never   Former  Former  Never   Never   Never  
## [2305] Never   Current Current Current Current Current <NA>    Never  
## [2313] <NA>    Never   Current Never   Never   Current Never   Never  
## [2321] Never   Former  Former  Former  Never   Never   Current Current
## [2329] Never   Never   Former  Former  <NA>    Former  Former  Current
## [2337] Current Current Never   Never   Never   <NA>    <NA>    <NA>   
## [2345] Never   Former  Former  Former  Former  Former  Former  Current
## [2353] Current Current Current Current Never   Never   Never   Never  
## [2361] Current Current Former  Former  Former  Never   Never   Never  
## [2369] Never   Never   Never   Never   Never   Never   Never   <NA>   
## [2377] <NA>    <NA>    <NA>    Former  Never   Never   Never   Former 
## [2385] Former  Former  Never   Current Current Current Current Current
## [2393] Current Never   Never   Never   Never   Never   Never   Never  
## [2401] Current Former  Never   Never   Never   Never   Former  Former 
## [2409] Never   Never   Never   Current Current Current Current Current
## [2417] Former  Never   Current Current Former  Current Current Never  
## [2425] Never   Never   Never   <NA>    <NA>    Never   Never   Former 
## [2433] Former  <NA>    Former  Former  Current Never   Never   Never  
## [2441] Never   Never   Never   Former  Former  Former  Current Current
## [2449] Former  Former  Former  Current Current Never   Never   Never  
## [2457] Never   Never   Never   <NA>    <NA>    <NA>    Never   Never  
## [2465] Never   Never   Never   Never   Never   Never   Never   Never  
## [2473] Current Current Current Current Current Never   Never   Former 
## [2481] Never   Never   Never   Never   Never   Former  Former  Former 
## [2489] Former  Former  Former  Current Never   Never   Never   Never  
## [2497] Never   Never   Never   Never   Former  Never   Former  Never  
## [2505] Current Never   Never   Never   Never   Never   Current Current
## [2513] Current Current Current Current Current Current Never   Never  
## [2521] Former  Never   Former  Former  Former  Never   Never   Former 
## [2529] Former  Never   Former  Never   Never   Former  Never   Never  
## [2537] Never   Never   Never   Never   Current Never   Never   Never  
## [2545] Never   Never   Never   Never   Never   Former  Former  Former 
## [2553] Never   Never   Never   Never   Never   Never   Current Never  
## [2561] Never   Never   Never   Never   Never   Never   Current Never  
## [2569] Never   Never   Never   Never   Current Never   Never   Former 
## [2577] Current Current Never   Former  Former  Former  Former  Former 
## [2585] Former  Former  Current Never   Never   Never   Never   Never  
## [2593] Current Never   Never   Former  Former  Former  Current Never  
## [2601] Never   Former  Former  Current Current Never   Current Never  
## [2609] Current Current Former  Never   Never   Never   Never   Never  
## [2617] Never   Never   Never   Never   <NA>    Never   Never   Former 
## [2625] Never   Current Current Never   Current Never   Never   Never  
## [2633] Never   <NA>    Never   Never   Former  Former  Never   Current
## [2641] Former  Former  Former  Former  Current Former  Former  Never  
## [2649] Never   Former  Never   Former  Former  Never   Never   Never  
## [2657] Never   Never   Never   Never   Never   Never   Never   Never  
## [2665] Former  Never   Former  Former  Former  Former  Former  Former 
## [2673] Former  Current Current Former  Never   Former  Current Former 
## [2681] Current Current Current Current Former  Never   Never   Never  
## [2689] Never   Never   Former  Former  Former  Former  Current Never  
## [2697] Never   Never   Never   Current Never   Never   Current <NA>   
## [2705] Former  Never   Current Never   Never   Never   Never   Never  
## [2713] Former  Former  Former  Never   Never   Current Never   Never  
## [2721] Former  Former  Former  Former  Former  Never   Never   Never  
## [2729] Never   Never   Former  Former  Never   Never   Never   Never  
## [2737] Never   Never   Never   Never   Former  Current Never   Never  
## [2745] Never   Never   Never   Never   Never   Current Never   Never  
## [2753] Never   Never   Never   Never   Never   Never   <NA>    <NA>   
## [2761] <NA>    <NA>    Never   Never   Never   Never   Former  Current
## [2769] Former  Current Never   Never   Never   Former  Current Former 
## [2777] Former  Former  Former  Current Never   Never   Never   Never  
## [2785] Never   Never   <NA>    Current Never   Never   Former  Former 
## [2793] <NA>    Never   Never   <NA>    Former  Former  Former  Former 
## [2801] Never   Never   Never   Former  Never   Never   Current Never  
## [2809] Never   Never   Never   Never   Never   Never   Never   Never  
## [2817] Never   Never   Never   Never   Never   Never   Never   Never  
## [2825] Never   Never   Never   Never   Never   Never   Never   Never  
## [2833] Never   Never   Never   Never   Never   Never   Never   Never  
## [2841] Never   Current Current Former  Former  Current Former  Former 
## [2849] Former  Former  Never   Never   Never   Never   Never   Never  
## [2857] Never   Never   Current Current Former  Former  Never   Never  
## [2865] Never   Never   Never   Never   Never   Never   Current Current
## [2873] Current Never   Never   Current Current Never   Never   Never  
## [2881] Never   Current Never   Never   Never   Never   Current Never  
## [2889] Former  Never   Never   Never   Former  Former  Never   Never  
## [2897] Never   Never   Never   Never   Current Former  Never   Never  
## [2905] Never   Never   Never   Never   Never   Never   Current Never  
## [2913] Never   Never   Never   Never   Never   Never   Never   Current
## [2921] Current Current Current Current Never   Never   Never   Never  
## [2929] Former  Former  Never   Never   Current Never   Never   Current
## [2937] Never   Never   Former  Current Current Current Current Current
## [2945] Never   Current Current Never   Never   Never   Former  Former 
## [2953] Former  Never   Never   Never   Current Never   Never   Never  
## [2961] Former  Former  Former  Never   Never   Never   Former  Former 
## [2969] Never   Never   Former  Never   Never   Never   Never   Current
## [2977] Never   Never   Never   Never   Never   Never   Former  Never  
## [2985] Never   Never   Never   Never   Never   Never   Never   Former 
## [2993] Former  Never   Never   Never   Current Current Current Never  
## [3001] <NA>    <NA>    Never   Never   Former  Former  Former  Never  
## [3009] Former  <NA>    Current <NA>    <NA>    Never   Current Current
## [3017] Current Former  Former  Former  Never   Former  Former  Former 
## [3025] Former  Former  Former  Former  Never   Former  Former  Former 
## [3033] Former  Former  <NA>    Never   Current Never   Never   Never  
## [3041] Former  Former  Former  Current Never   Never   Current Current
## [3049] Former  Never   Never   Former  Former  Never   Never   Never  
## [3057] Never   <NA>    Former  Former  Never   Current Never   Never  
## [3065] Never   Never   Never   Never   Never   Never   Never   Never  
## [3073] Never   Never   Never   Never   Current Current Current Current
## [3081] Current Current Never   Former  Never   Current Former  Former 
## [3089] Never   Never   Current Current Never   Never   Former  Never  
## [3097] <NA>    Former  Current Current Former  Former  Former  Former 
## [3105] Former  Never   Never   Current Never   Current Current Never  
## [3113] Never   Former  Never   Never   Never   Never   Former  Current
## [3121] Current Current Current Former  Former  Former  Former  Former 
## [3129] Former  Former  Former  Never   Former  Former  Former  Never  
## [3137] Never   Never   Never   Former  Current Never   Never   Never  
## [3145] Never   Never   Never   Former  Never   Never   Never   Former 
## [3153] Never   Current Current Current Never   <NA>    Never   Never  
## [3161] Never   Current Current <NA>    <NA>    <NA>    Never   Never  
## [3169] Never   Current Never   Never   Never   Never   Never   Never  
## [3177] Current Never   Never   Never   Never   Never   Former  Current
## [3185] Former  Former  Former  Former  Current Current Current Never  
## [3193] Former  Never   Never   Never   Never   Current Never   Never  
## [3201] Never   Never   Never   Never   Never   Never   Never   Never  
## [3209] Never   Never   Never   Current Current Current Current Former 
## [3217] Never   Never   Current Never   Never   Never   Former  Current
## [3225] Never   Current Current Current Current Current Current Former 
## [3233] Former  Former  Never   Never   Never   Never   Never   Never  
## [3241] Never   Current Never   Never   Current Current Current Never  
## [3249] Current Current Current Never   Former  Former  Never   Former 
## [3257] Former  <NA>    <NA>    Current Current Current Never   Never  
## [3265] Never   Never   Never   Never   Never   Never   Never   Never  
## [3273] Never   Never   Never   Former  Former  Never   Current Never  
## [3281] Never   Former  Former  Never   Never   Never   Never   Current
## [3289] Current Current Current Never   Never   Former  Former  Former 
## [3297] Former  Never   Never   Never   Never   Never   Never   Never  
## [3305] Never   Never   Never   Current Current Current Never   Never  
## [3313] Never   Former  Never   Never   Never   Never   Former  Former 
## [3321] Former  Former  Former  Former  Current Current Never   Never  
## [3329] Never   Former  Former  Former  Former  Current Former  Former 
## [3337] Former  Never   Current Current Current Never   Former  Never  
## [3345] Never   Never   Never   Never   Never   Never   Never   Never  
## [3353] Never   Never   Never   Never   Never   Never   Never   Never  
## [3361] Never   Never   Never   Never   Never   Never   Former  Never  
## [3369] Never   Never   Never   Former  Former  Former  Former  Former 
## [3377] Former  Former  Former  Never   Never   Never   Never   <NA>   
## [3385] Current Former  Former  Former  Former  Former  Current Current
## [3393] Never   Former  Former  Former  Former  Former  Former  Former 
## [3401] Never   Never   Never   Current Current Current Current Current
## [3409] Current Current Current Former  Current Never   Never   Current
## [3417] Former  Former  Never   Current Former  Former  Former  Never  
## [3425] Never   Former  Never   Former  Former  Former  Former  Former 
## [3433] Never   Former  Former  Never   Never   <NA>    <NA>    <NA>   
## [3441] Never   Current Current Current Former  Former  Never   Never  
## [3449] Never   Never   Never   Never   Never   Never   Never   Never  
## [3457] Never   Never   Current Current Current Current Current Current
## [3465] Never   Never   Never   Never   Never   Current Never   Never  
## [3473] Never   Never   Never   Never   Never   Never   Current Current
## [3481] Former  Former  Former  Never   Never   Never   Never   Never  
## [3489] Never   Former  Former  Former  Current Current Never   Never  
## [3497] Never   Never   Never   Never   Never   Current Never   Former 
## [3505] Former  Current Current Current Current Former  Current Former 
## [3513] Current Former  Current Current Never   Never   Never   Never  
## [3521] Never   Never   Never   Former  Former  Never   Former  Never  
## [3529] Never   Never   Current Current Former  Former  Current Never  
## [3537] Never   Never   <NA>    Never   Never   Never   Never   Never  
## [3545] Never   Never   Never   Never   Current Current Current Never  
## [3553] Current Former  Current Never   Former  Never   Never   Never  
## [3561] Never   Never   Never   Never   Never   Never   Former  Former 
## [3569] Never   Never   Never   Former  Former  Never   Never   Current
## [3577] Current Current Former  Current Former  Former  Former  Never  
## [3585] Never   Former  Former  <NA>    Never   Never   Never   Never  
## [3593] Current Current <NA>    <NA>    Never   Never   Former  Former 
## [3601] Former  Never   Never   Current Former  Former  Former  Former 
## [3609] Never   Never   Never   Former  Never   Never   Never   Never  
## [3617] Never   Never   Former  Never   Never   Never   Former  Former 
## [3625] Never   Current Never   Former  Never   Never   Never   Never  
## [3633] Never   Never   Never   Current Never   Never   Current Current
## [3641] Current Current Current Never   Never   Never   Never   Never  
## [3649] Never   Former  Former  Never   Current Never   Never   Never  
## [3657] Never   Never   Former  Former  Never   Never   Former  Former 
## [3665] Current Never   Never   <NA>    Current Former  Former  Never  
## [3673] Never   Never   Former  Former  Never   <NA>    Former  Former 
## [3681] Never   Never   Never   Former  Former  Former  Former  Current
## [3689] Current Current Never   Never   Never   Never   Never   Never  
## [3697] Never   Never   Never   Never   Never   Current Current Current
## [3705] Never   Never   Never  
## Levels: Never Former Current
```

If we're happy with that, let's change the value of nha$SmokingStatus in place. I'm showing the dplyr way using mutate

```r
nh <- nh %>% 
  mutate(SmokingStatus=factor(SmokingStatus, levels = c('Never', 'Former', 'Current')))
```

Re-fit the model then look at the ANOVA and LM summaries

```r
fit1 <- lm(Height~SmokingStatus, data=nh)

# Show the ANOVA table
anova(fit1)
```

```
## Analysis of Variance Table
## 
## Response: Height
##                 Df Sum Sq Mean Sq F value    Pr(>F)    
## SmokingStatus    2   9185  4592.4  45.261 < 2.2e-16 ***
## Residuals     3558 361009   101.5                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# Print the full model statistics
summary(fit1)
```

```
## 
## Call:
## lm(formula = Height ~ SmokingStatus, data = nh)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.008  -7.208  -0.008   6.999  32.392 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          167.5076     0.2246 745.920  < 2e-16 ***
## SmokingStatusFormer    2.2938     0.4112   5.578 2.61e-08 ***
## SmokingStatusCurrent   3.9685     0.4434   8.950  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.07 on 3558 degrees of freedom
##   (146 observations deleted due to missingness)
## Multiple R-squared:  0.02481,	Adjusted R-squared:  0.02426 
## F-statistic: 45.26 on 2 and 3558 DF,  p-value: < 2.2e-16
```

Notice that the p-value on the ANOVA/regression didn't change, but the coefficients did. _Never_ smokers are now treated as baseline. The intercept coefficient (167.5076) is now the mean Height for _Never_ smokers. The `SmokingStatusFormer` coefficient of 2.2938 shows the average increase in Height that former smokers have when compared to never smokers, and that difference is statistically significant (2.61e-08). 

The `SmokingStatusCurrent` coefficient of 3.9685 shows that current smokers  have a higher Height than never smokers, and that this increase is highly significant.

Finally, you can do the typical post-hoc ANOVA procedures on the fit object. For example, the `TukeyHSD()` function will run [_Tukey's test_](https://en.wikipedia.org/wiki/Tukey%27s_range_test) (also known as _Tukey's range test_, the _Tukey method_, _Tukey's honest significance test_, _Tukey's HSD test_ (honest significant difference), or the _Tukey-Kramer method_). Tukey's test computes all pairwise mean difference calculation, comparing each group to each other group, identifying any difference between two groups that's greater than the standard error, while controlling the type I error for all multiple comparisons. First run `aov()` (**not** `anova()`) on the fitted linear model object, then run `TukeyHSD()` on the resulting analysis of variance fit.


```r
TukeyHSD(aov(fit1))
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = fit1)
## 
## $SmokingStatus
##                    diff       lwr      upr     p adj
## Former-Never   2.293849 1.3296709 3.258027 0.0000001
## Current-Never  3.968526 2.9288200 5.008232 0.0000000
## Current-Former 1.674677 0.4679661 2.881388 0.0032932
```

```r
plot(TukeyHSD(aov(fit1)))
```

![](ANOVA_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

This output shows the pairwise differences between each combination. Conducting 3 t-tests to compare groups would increase the type 1 error rate, and is not appropriate. Tukey's HSD accounts for multiple testing.

### Diagnostics

Remember that the assumptions of ANOVA are:

- Random sampling
- Independent samples
- Normality of residuals
- Constant variance (homoscedasticity)

We looked for equal variance in our density plot and though we used that plot to assess normality, the true assumption is normality of the residuals. We can pull out the residuals from the fit object using `$residuals`

Because the residuals are a vector, it will be easier to use the base R function `qqnorm()` than the ggplot2 `geom_qq()`


```r
qqnorm(fit1$residuals); qqline(fit1$residuals)
```

![](ANOVA_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

These look great, so we are happy with the model diagnostics
