ch-7-linear-models
================
Sonya Hua
September 12, 2017

Linear Models: Identifying outcome drivers
------------------------------------------

\*\*Satisfaction drivers analysis\* : A common application in survey analysis whereby we model satisfaction with a product in relation to specific elements of the product.

**marketing mix modeling** Modeling how price and advertising are related to sales

Driver does not imply caucstion, but simply a linear assocation among variables. Linear models is a loose term for regression analysis or least-squares fitting.

### 7.1 Simulating Data: Amusement Park Satisfaction

Data comprises of 500 observations/responses:

-   `weekend`: whether the respondent visited on a weekend
-   `num.child`: number of hcildren brought
-   `distance` : distance traveled to the park
-   `overall` : overall satisfaction
-   `rides, games, wait, clean` : satisfaction with the rides, games, waiting times, and cleanliness respectively

``` r
set.seed(08226)
nresp <- 500
halo <- rnorm(n=nresp, mean=0, sd=5) # Satisfaction Halo effect with a random var per customer. Halo does not appear in the final data but is used to influence other ratings

# Assume data is on a 100-point scale. By adding halo to each response, we create naturally positive correlation between the responses
rides <- floor(halo + rnorm(n=nresp, mean=80, sd=3) +1)
games <- floor(halo + rnorm(n=nresp, mean=70, sd=7)+5)
wait <- floor(halo + rnorm(n=nresp, mean=65, sd=10) +9)
clean <- floor(halo + rnorm(n=nresp, mean=85, sd=2) +1)

# Verify correlation between vars that share the halo
cor(rides, games)
```

    ## [1] 0.4534134

``` r
# Sample a lognormal distribution for distance
distance <- rlnorm(n=nresp, meanlog=3, sdlog=1)

# Sample discrete distributions for weekend and num.child
num.child<- sample(x=0:5, size=nresp, replace=TRUE,
                   prob=c(0.3,0.15,0.25, 0.15, 0.1, 0.05))

weekend <- as.factor(sample(x=c("yes","no"), size=nresp, replace=TRUE,
                            prob=c(0.5,0.5)))

# Create overall satisfaction rating as function of ratings for the various aspects of vist
# Includes halo to capture latent satisfaction
# Adds the satis vars with weight for each one
# inlcudes weighted contributions for other influences
# random normal variation using rnorm()
# Uses floor() to produce an integer with a constant -51 to adjust the total to be >= 100 points

overall <- floor(halo + 0.5*rides +0.1*games +0.3*wait +0.2*clean +
                   0.3*distance +5*(num.child==0) + 0.3*wait*(num.child>0) +
                   rnorm(n=nresp, mean=0, sd=7) -51)

# Combine vars into a df and removed unneeded objects from workspace
sat.df <- data.frame(weekend, num.child, distance, rides, games, wait, clean, overall)

rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, overall)
```

### 7.2 Fitting linear models with `lm()`

Inspect data first:

``` r
summary(sat.df)
```

    ##  weekend     num.child        distance            rides      
    ##  no :259   Min.   :0.000   Min.   :  0.5267   Min.   :67.00  
    ##  yes:241   1st Qu.:0.000   1st Qu.: 10.3181   1st Qu.:77.00  
    ##            Median :2.000   Median : 19.0191   Median :80.00  
    ##            Mean   :1.738   Mean   : 31.0475   Mean   :80.38  
    ##            3rd Qu.:3.000   3rd Qu.: 39.5821   3rd Qu.:84.00  
    ##            Max.   :5.000   Max.   :239.1921   Max.   :94.00  
    ##      games            wait            clean          overall      
    ##  Min.   :52.00   Min.   : 43.00   Min.   :72.00   Min.   :  9.00  
    ##  1st Qu.:68.00   1st Qu.: 65.75   1st Qu.:82.00   1st Qu.: 48.00  
    ##  Median :74.00   Median : 74.00   Median :85.00   Median : 60.00  
    ##  Mean   :74.14   Mean   : 73.42   Mean   :85.39   Mean   : 61.25  
    ##  3rd Qu.:80.00   3rd Qu.: 81.00   3rd Qu.:89.00   3rd Qu.: 73.00  
    ##  Max.   :96.00   Max.   :103.00   Max.   :98.00   Max.   :139.00

For some reason, I'm not able to replicate the data set in the book (notice there's points over 100) so I'm going to use the shortcut as our df

``` r
sat.df <- read.csv("http://goo.gl/HKnl74")
str(sat.df)
```

    ## 'data.frame':    500 obs. of  8 variables:
    ##  $ weekend  : Factor w/ 2 levels "no","yes": 2 2 1 2 1 1 2 1 1 2 ...
    ##  $ num.child: int  0 2 1 0 4 5 1 0 0 3 ...
    ##  $ distance : num  114.6 27 63.3 25.9 54.7 ...
    ##  $ rides    : int  87 87 85 88 84 81 77 82 90 88 ...
    ##  $ games    : int  73 78 80 72 87 79 73 70 88 86 ...
    ##  $ wait     : int  60 76 70 66 74 48 58 70 79 55 ...
    ##  $ clean    : int  89 87 88 89 87 79 85 83 95 88 ...
    ##  $ overall  : int  47 65 61 37 68 27 40 30 58 36 ...

``` r
summary(sat.df)
```

    ##  weekend     num.child        distance            rides       
    ##  no :259   Min.   :0.000   Min.   :  0.5267   Min.   : 72.00  
    ##  yes:241   1st Qu.:0.000   1st Qu.: 10.3181   1st Qu.: 82.00  
    ##            Median :2.000   Median : 19.0191   Median : 86.00  
    ##            Mean   :1.738   Mean   : 31.0475   Mean   : 85.85  
    ##            3rd Qu.:3.000   3rd Qu.: 39.5821   3rd Qu.: 90.00  
    ##            Max.   :5.000   Max.   :239.1921   Max.   :100.00  
    ##      games             wait           clean          overall      
    ##  Min.   : 57.00   Min.   : 40.0   Min.   : 74.0   Min.   :  6.00  
    ##  1st Qu.: 73.00   1st Qu.: 62.0   1st Qu.: 84.0   1st Qu.: 40.00  
    ##  Median : 78.00   Median : 70.0   Median : 88.0   Median : 50.00  
    ##  Mean   : 78.67   Mean   : 69.9   Mean   : 87.9   Mean   : 51.26  
    ##  3rd Qu.: 85.00   3rd Qu.: 77.0   3rd Qu.: 91.0   3rd Qu.: 62.00  
    ##  Max.   :100.00   Max.   :100.0   Max.   :100.0   Max.   :100.00

#### 7.2.1 Prelim Data Inspection

Before modeling, check that each variable has a reasonably normal distribution and that joint relationships among the vars are approrpriate for modeling.

``` r
library(gpairs)
gpairs(sat.df)
```

    ## Loading required package: grid

    ## Loading required package: lattice

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png" style="display: block; margin: auto;" /> *Observe* Most vars are normal with the exception of distance which is left-skewed. We can do a log transformation on distance to see if it helps normalize the distribution

``` r
sat.df$logdist <- log(sat.df$distance)
```

``` r
gpairs(sat.df)
```

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png" style="display: block; margin: auto;" /> *Observe* \* logdist is more normally distributed \* The pairwise scatterplots of our continuous vars are generally elliptical in shape- a good indication they are appropriate for linear modeling. Howeever, the vars in the lower right are positive correlated; there's multicollinearity. When vars are strongly related in this way, it's difficult to assess their individual effects with stat models. The effect can be so severe that the relationships become uninterpretable without taking some action to handle the high correlations

*Recs*:

-   Given the positive assocations, investigate the correlation structure further

``` r
library(corrplot)

# Select columns 2, 4:9 to exlucde the categorical vars and the raw var distance since we transformed it in logdist
corrplot.mixed(cor(sat.df[,c(2,4:9)]), upper="ellipse") # Add upper=ellipse to mixed command for easier corr spotting
```

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

*Observe*

-   The satisfaction items are moderately to strongly associated with one another.
-   None of the vars appear to be nearly identical, as would be indicated by correlations exceeding r &gt; 0.9
-   rides and clean are highly related (r=0.79) but not so strongly that remediation is strictly required
-   It appears to be acceptable to proceeed with modeling hte relationships among these vars

#### Linear Model with 1 Predictor

##### To what extent is satisfaction with the rides related to overall experience? Is the relationship strong or weak?

Plot these two vars

``` r
plot(overall~rides, data=sat.df)
```

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png" style="display: block; margin: auto;" /> *Observe*

-   There's a tendency for people with higher satisfaction with rides to have higher overall satisfaction

`lm(formula,data)` where data is a df, for linear modeling

``` r
(m1 <-lm(overall~rides, data=sat.df))
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides, data = sat.df)
    ## 
    ## Coefficients:
    ## (Intercept)        rides  
    ##     -94.962        1.703

*Observe* This provides the intercept and slope of a fitted line which we can use to calculate predicted values based on a rating from rides, or used to plot a fitted line among the data. abline() recognizes lm objects as a line

``` r
# The expected overall rating for someone who gives a rating of 95 for ride satisfaction
-94.962 + 1.703 * 95
```

    ## [1] 66.823

``` r
# plot
plot(overall~ rides, data=sat.df,
     xlab="Ride Satisfaction", ylab="Overall Satisfaction")
abline(m1, col="red", lwd=2)  #lwd = line width
```

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
# contains everything lm() knows about the model
str(m1)
```

    ## List of 12
    ##  $ coefficients : Named num [1:2] -95 1.7
    ##   ..- attr(*, "names")= chr [1:2] "(Intercept)" "rides"
    ##  $ residuals    : Named num [1:500] -6.22 11.78 11.18 -17.93 19.89 ...
    ##   ..- attr(*, "names")= chr [1:500] "1" "2" "3" "4" ...
    ##  $ effects      : Named num [1:500] -1146.2 -207.9 11.5 -17.9 20.3 ...
    ##   ..- attr(*, "names")= chr [1:500] "(Intercept)" "rides" "" "" ...
    ##  $ rank         : int 2
    ##  $ fitted.values: Named num [1:500] 53.2 53.2 49.8 54.9 48.1 ...
    ##   ..- attr(*, "names")= chr [1:500] "1" "2" "3" "4" ...
    ##  $ assign       : int [1:2] 0 1
    ##  $ qr           :List of 5
    ##   ..$ qr   : num [1:500, 1:2] -22.3607 0.0447 0.0447 0.0447 0.0447 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:500] "1" "2" "3" "4" ...
    ##   .. .. ..$ : chr [1:2] "(Intercept)" "rides"
    ##   .. ..- attr(*, "assign")= int [1:2] 0 1
    ##   ..$ qraux: num [1:2] 1.04 1.01
    ##   ..$ pivot: int [1:2] 1 2
    ##   ..$ tol  : num 1e-07
    ##   ..$ rank : int 2
    ##   ..- attr(*, "class")= chr "qr"
    ##  $ df.residual  : int 498
    ##  $ xlevels      : Named list()
    ##  $ call         : language lm(formula = overall ~ rides, data = sat.df)
    ##  $ terms        :Classes 'terms', 'formula'  language overall ~ rides
    ##   .. ..- attr(*, "variables")= language list(overall, rides)
    ##   .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:2] "overall" "rides"
    ##   .. .. .. ..$ : chr "rides"
    ##   .. ..- attr(*, "term.labels")= chr "rides"
    ##   .. ..- attr(*, "order")= int 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 1
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. ..- attr(*, "predvars")= language list(overall, rides)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. ..- attr(*, "names")= chr [1:2] "overall" "rides"
    ##  $ model        :'data.frame':   500 obs. of  2 variables:
    ##   ..$ overall: int [1:500] 47 65 61 37 68 27 40 30 58 36 ...
    ##   ..$ rides  : int [1:500] 87 87 85 88 84 81 77 82 90 88 ...
    ##   ..- attr(*, "terms")=Classes 'terms', 'formula'  language overall ~ rides
    ##   .. .. ..- attr(*, "variables")= language list(overall, rides)
    ##   .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : chr [1:2] "overall" "rides"
    ##   .. .. .. .. ..$ : chr "rides"
    ##   .. .. ..- attr(*, "term.labels")= chr "rides"
    ##   .. .. ..- attr(*, "order")= int 1
    ##   .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. ..- attr(*, "response")= int 1
    ##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. .. ..- attr(*, "predvars")= language list(overall, rides)
    ##   .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. .. ..- attr(*, "names")= chr [1:2] "overall" "rides"
    ##  - attr(*, "class")= chr "lm"

``` r
# Hand calculated CI 
1.7033 + 1.96 * 0.1055
```

    ## [1] 1.91008

``` r
1.7033 - 1.96 * 0.1055
```

    ## [1] 1.49652

``` r
# equivalent: confint()
confint(m1) # default level is 95%
```

    ##                   2.5 %     97.5 %
    ## (Intercept) -112.800120 -77.124371
    ## rides          1.495915   1.910656

``` r
confint(m1, level=.99) # 99% CI
```

    ##                   0.5 %    99.5 %
    ## (Intercept) -118.438170 -71.48632
    ## rides          1.430371   1.97620

To get summarized features of the fitted model, use `summary()`

Interpretation of output:

-   The Residuals quartile section tells us how closely or not the data follows the best fit line. A *residual* is the difference between the model-predicted value vs. observed value.

-   Standard error indicates uncertainty in the coefficient estimate, assuming the data are a random sample of a larger population.

-   The t-value, p-value, and signiciance codes indicate a *Wald Test*, which asseses whether the coefficient is significantly different from 0. A traditional estimate of a 95% CI for the coefficient estimate is it will fall within +/- 1.96 x Standard Error. Always report our CI's along with the point estimate.

-   Residual standard error is an estimate of the avg error of the residuals. It's a measure of how close the data points are to the estimate line.

-   R-squared is a measure of how much *variation in y *is captured by the model

-   F-statistic provides a stat test with null hypothesis that a model without predictors perform as well as the model. It tests whether the model predicts the data better than simply taking the average of the y var and using htat as the prediction or all obs.

``` r
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides, data = sat.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -33.597 -10.048   0.425   8.694  34.699 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -94.9622     9.0790  -10.46   <2e-16 ***
    ## rides         1.7033     0.1055   16.14   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.88 on 498 degrees of freedom
    ## Multiple R-squared:  0.3434, Adjusted R-squared:  0.3421 
    ## F-statistic: 260.4 on 1 and 498 DF,  p-value: < 2.2e-16

*Observe*:

-   The rides coefficient 1.70 means: each additional rides rating point results in an increase of 1.7 points in overall rating
-   Residuals are quite wide ranging from -34 to 35, suggesting our predictions can be quite a bit off for any given data point ( &gt; 30points on the rating scale). The quartiles of the residuals suggest they're fairly symmetric around 0. That's a good sign this model although imprecise, is unbiased.
-   On average, points deviate 12.8 points away from the fitted function.
-   According to R^2 = 0.34, about 34% of the variation in overall satisfaction is explained by the model
-   According to the F-test, we reject the null hypothesis that a model without predictors performs as well as our model. In other words, this model performs better than a straight up average of overall satisfaction

#### 7.2.5 Checking Model Fits and Diagnostics

There's 5 key assumptions in a linear model that we must check for before finalizing results:

-   Linear relationship - prediction errors are normally distributed and look like random noise with no pattern
-   Multivariate normality - does not deviate significantly from normal distribution
-   No or little multicollinearity
-   No auto-correlation.
-   Homoscedasticity - homogeneous band of errors across observed values

`plot()` the regression model for the diagnostic plots:

1.  Fitted Values vs. Residuals - There should be no obvious pattern and residuals should resemble random error

2.  Fitted Values vs. Square Root of the standardized residuals - Checks for homscedasticity. There should be no clear pattern. Observations with ihgh residuals are flagged as potential outliers which we should inspect later. A common pattern in residual plots is a cone or funnel where errors get progressively larger for larger or smaller fitted values, which suggests *heteroskedasticity*. When values in one part of the range have a much larger spread than those in another area, they will have biased influence on the estimation of hte line. Sometimes a transformation on the predictor or outcome var will resolve this

3.  Normal QQ Plot -Checks for normality. It compares the values that residuals would be expected to take if they are normally distributed vs. observed values.

4.  Leverage vs. standardized residuals - *Leverage* of each point is the measure of the point's influence on the estimated model coefficients. The leverage is based on *Cook's distance*, an estimate of how much predicted values (y) would change if the model were re-estimated without the point in the data. If there's obs with high Cook's distance, this chart would show dotted red lines for the distances. Any pot'l outliers will have their obs number called out.

Best practice is to inspect pot'l outliers for any problems within the data. We generally do not omit outliers except when they're obvious errors in the data.

``` r
par(mfrow=c(2,2))
plot(m1)
```

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png" style="display: block; margin: auto;" /> *Observe*:

-   1st Plot: There's no obvious pattern between fitted values for overall satisfaction and the residuals. Residuals resemble random error.
-   2nd Plot (Scale-Location): There's no clear pattern and suggests homoskedasticity
-   3rd Plot (Normal QQ): The data fits normality
-   4th Plot: There are no extreme outliers but there are some pot'l outliers 57, 129, and 295

Let's inspect the pot'l outliers for any data errors

``` r
sat.df[c(57, 129, 295),]
```

    ##     weekend num.child distance rides games wait clean overall  logdist
    ## 57      yes         2 63.29248    98    87   89   100     100 4.147767
    ## 129     yes         0 11.89550    76    77   51    77       6 2.476161
    ## 295      no         0 11.74474    98    83   63    92      45 2.463406

*Observe* \* Row 129 might be inaccurate, but we don't know

We'll keep all of the observations presently. Overall, the model relating overall satisfaction to satisfaction with rides seems reasonable.

### 7.3 Linear Models with Multiple Predictors

Interpretation of the coefficients changes: each coefficient represents the strength of the relationship between x and y, based on the values of the other predictors (have not changed)

##### Which features of the park most closely related to overall satisfactions?

``` r
(m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df))
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean, data = sat.df)
    ## 
    ## Coefficients:
    ## (Intercept)        rides        games         wait        clean  
    ##   -131.4092       0.5291       0.1533       0.5533       0.9842

``` r
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean, data = sat.df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.944  -6.841   1.072   7.167  28.618 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -131.40919    8.33377 -15.768  < 2e-16 ***
    ## rides          0.52908    0.14207   3.724 0.000219 ***
    ## games          0.15334    0.06908   2.220 0.026903 *  
    ## wait           0.55333    0.04781  11.573  < 2e-16 ***
    ## clean          0.98421    0.15987   6.156 1.54e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.59 on 495 degrees of freedom
    ## Multiple R-squared:  0.5586, Adjusted R-squared:  0.5551 
    ## F-statistic: 156.6 on 4 and 495 DF,  p-value: < 2.2e-16

*Observe*:

-   The R-Squared increased from 0.34 to 0.56. Our prediction has improved by including all satisfaction features in our model. About half of the variation in overall ratings is explained by the ratings for specific features.
-   The residual SE is now 10.59, decreased from 12.88. The predications are more accurate.
-   Residuals appear to be symmetric according to the Residual Quartiles.
-   All 4 features are identified as being statistically significant (p &lt; 0.05).
-   The coefficients for rides changed from m1 to m2 (1.70 to 0.529). This is because rides correlated with all other vars. Customers who are more satisfied with rides tend to be more satisfaied with the wait and games. Coefficients genreally change unless the vars are entirely uncorrelated We can plot the coefficients along with the CI's by calling `coefplot()` of the `coefplot` package, adding `intercept=FALSE` to plot just the var coefficients.

``` r
#install.packages("coefplot")
#install.packages("dplyr")
#install.packages("plyr")
library(coefplot)
```

    ## Loading required package: ggplot2

``` r
coefplot(m2, intercept=F, 
         outerCI = 1.96, # for 95% CI
         lwdOuter=1,
         ylab="Feature Rating",
         xlab="Association with Overall Satisfaction")
```

    ## Warning: Ignoring unknown aesthetics: xmin, xmax

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png" style="display: block; margin: auto;" /> *Observe*: \* Cleanliness is estamed to be the most important feature associated with overall satisfaction, followed by wait and rides. Satisfaction with games is estimated to be relatively less important.

Sorting the coefficient plot may be easier to see the priority of coefficients by calling `sort = "magnitude"`. A coef plot is often a key output of a satisfaction drivers analysis.

``` r
coefplot(m2, intercept=F, 
         outerCI = 1.96, # for 95% CI
         lwdOuter=1,
         ylab="Feature Rating",
         xlab="Association with Overall Satisfaction",
         sort = "magnitude")
```

    ## Warning: Ignoring unknown aesthetics: xmin, xmax

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

#### 7.4.1 Comparing Linear Models

##### Which model is better in fit? m1 or m2?

We can compare adj. r-squared values of each model to see which model has a larger adj. r-squared

``` r
# Use adjusted-R-squared values which control for number of predictors in m2
summary(m1)$adj.r.squared
```

    ## [1] 0.3420614

``` r
summary(m2)$adj.r.squared
```

    ## [1] 0.5550543

*Observe* The adj. r-squared suggested m2 explains more of the variation in overall satisfaction

It's best practice to also compare the predictions of the models visually by plotting the fitted vs. actual values for each model. If the model fits the data perfectly, the points will fall on a diagonal line. By comparing models, we can see which model fits the data better by forming a tighter diagonal.

``` r
plot(sat.df$overall, fitted(m1), col='red',
     xlim=c(0,100), ylim=c(0,100),
     xlab="Actual Overall Satisfaction",
     ylab="Fitted Overall Satisfaction")

# Now plot m2 points
points(sat.df$overall, fitted(m2), col="blue")

# Add a legend
legend("topleft", legend=c("model 1", "model 2"),
       col=c("red","blue"), pch=1)
```

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-20-1.png" style="display: block; margin: auto;" /> *Observe* Model 2 (blue points) is more tightly clusterd along a diag line, showing m2 explains more variation in the data than m1.

For a more formal test, which is possible since the models are nested, we can use ANOVA to determine whether m2 explains more variation:

``` r
anova(m1, m2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: overall ~ rides
    ## Model 2: overall ~ rides + games + wait + clean
    ##   Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    498 82612                                  
    ## 2    495 55532  3     27080 80.463 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*Observe*: p-value is significant, so we can reject the null hypothesis of no difference in the 2nd model

#### 7.3.2 Making Predictions From Model

#### What's the predicted overall rating for a customer who rated the 4 separate features as 100 points each?

Predicted Point = Intercept + Coeff \* Observed Point +... Coeff \* Observed Point

``` r
coef(m2)
```

    ##  (Intercept)        rides        games         wait        clean 
    ## -131.4091939    0.5290780    0.1533361    0.5533264    0.9842126

``` r
# This is a named vector. We access the individual columns using their names
```

``` r
# don't forget the parentheses around intercept

# Predicted Point:
coef(m2)["(Intercept)"] + coef(m2)["rides"]*100 + coef(m2)["games"]*100 + coef(m2)["wait"]*100 + coef(m2)["clean"]*100
```

    ## (Intercept) 
    ##    90.58612

*Observe*: The best estimate is 90.586 using model m2.

``` r
# Method 2 # %*%

coef(m2)%*%c(1,100,100,100,100) # multiplies each element of coef(m2) with each element in our vector 
```

    ##          [,1]
    ## [1,] 90.58612

``` r
# 1* Intercept = intercept
```

``` r
#Method 3: using predict(object, newdata) where newdata is a dataframe with the same column names as the data used in the model.
# Prediction for first 5 customers
predict(m2, sat.df[1:5,])
```

    ##        1        2        3        4        5 
    ## 46.60864 54.26012 51.17289 50.30434 52.94625

``` r
# Method 4: Using fitted() to access fitted values of obs
fitted(m2)[1:5]
```

    ##        1        2        3        4        5 
    ## 46.60864 54.26012 51.17289 50.30434 52.94625

#### 7.3.3 Standardizing Predictors (Using Z=Score)

If vars have different scales, then coefficient values would not be directly comparable. I.e., the distance and logdist vars are not on a 100 point scale. When we wish to compare estimate coefficients, it's helpful to *standardize* data on a commo unit such as the z-score before fitting a model. Transform first any variables to a normal scale, and then standardize. Z-score standardization converts values to N(0,std), by subtracting a var's mean from each obs, and then divide by the standard deviation using `scale()`.

We will create a a scaled version of `sat.df` to a new data frame called `sat.std` while dropping the untransformed values of raw `distance` with `[ , -3]` because we use `logdist`. Do not standardize `weekend` since it's a factor var vs. numeric. Only standardize numeric vars.

We'll leave `num.child` for now since we haven't analyzed it. Note we do not alter the original df in case there's errors we need to recover. After standardizing, we should check the results that the mean of 0 and values within a few units of hte mean

``` r
summary(sat.df$rides)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   72.00   82.00   86.00   85.85   90.00  100.00

``` r
# standardizing using scale()
head(scale(sat.df$rides))
```

    ##            [,1]
    ## [1,]  0.2112477
    ## [2,]  0.2112477
    ## [3,] -0.1548662
    ## [4,]  0.3943047
    ## [5,] -0.3379232
    ## [6,] -0.8870941

``` r
summary(scale(sat.df$rides))
```

    ##        V1          
    ##  Min.   :-2.53461  
    ##  1st Qu.:-0.70404  
    ##  Median : 0.02819  
    ##  Mean   : 0.00000  
    ##  3rd Qu.: 0.76042  
    ##  Max.   : 2.59099

``` r
# equivalent
head((sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides))
```

    ## [1]  0.2112477  0.2112477 -0.1548662  0.3943047 -0.3379232 -0.8870941

``` r
sat.std <- sat.df[,-3] # sat but remove distance
sat.std[,3:8] <- scale(sat.std[,3:8])
head(sat.std)
```

    ##   weekend num.child      rides       games         wait       clean
    ## 1     yes         0  0.2112477 -0.69750817 -0.918784090  0.21544189
    ## 2     yes         2  0.2112477 -0.08198737  0.566719693 -0.17555973
    ## 3      no         1 -0.1548662  0.16422095  0.009655775  0.01994108
    ## 4     yes         0  0.3943047 -0.82061233 -0.361720171  0.21544189
    ## 5      no         4 -0.3379232  1.02595006  0.381031720 -0.17555973
    ## 6      no         5 -0.8870941  0.04111679 -2.032911927 -1.73956621
    ##      overall   logdist
    ## 1 -0.2681587 1.7886823
    ## 2  0.8654385 0.3226360
    ## 3  0.6135280 1.1862757
    ## 4 -0.8979350 0.2803106
    ## 5  1.0543714 1.0385034
    ## 6 -1.5277112 0.1452467

``` r
summary(sat.std)
```

    ##  weekend     num.child         rides              games         
    ##  no :259   Min.   :0.000   Min.   :-2.53461   Min.   :-2.66717  
    ##  yes:241   1st Qu.:0.000   1st Qu.:-0.70404   1st Qu.:-0.69751  
    ##            Median :2.000   Median : 0.02819   Median :-0.08199  
    ##            Mean   :1.738   Mean   : 0.00000   Mean   : 0.00000  
    ##            3rd Qu.:3.000   3rd Qu.: 0.76042   3rd Qu.: 0.77974  
    ##            Max.   :5.000   Max.   : 2.59099   Max.   : 2.62630  
    ##       wait               clean             overall        
    ##  Min.   :-2.775664   Min.   :-2.71707   Min.   :-2.85024  
    ##  1st Qu.:-0.733096   1st Qu.:-0.76206   1st Qu.:-0.70900  
    ##  Median : 0.009656   Median : 0.01994   Median :-0.07923  
    ##  Mean   : 0.000000   Mean   : 0.00000   Mean   : 0.00000  
    ##  3rd Qu.: 0.659564   3rd Qu.: 0.60644   3rd Qu.: 0.67651  
    ##  Max.   : 2.794975   Max.   : 2.36595   Max.   : 3.06966  
    ##     logdist        
    ##  Min.   :-3.67074  
    ##  1st Qu.:-0.65352  
    ##  Median :-0.03327  
    ##  Mean   : 0.00000  
    ##  3rd Qu.: 0.71008  
    ##  Max.   : 2.53453

*Observe* sat.std matches expectation of mean=0.

### 7.4 Prediction using Factors

When can continue to improve our m2 model by seeing if including factor variables can help explain the overall satisfaction.

``` r
# Add weekend, logdist, num.child to standardized model called m3

m3 <- lm(overall~ rides + games + wait + clean +
           weekend + logdist + num.child, data=sat.std)

summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean + weekend + 
    ##     logdist + num.child, data = sat.std)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.51427 -0.40271  0.01142  0.41613  1.69000 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.37271    0.04653  -8.009 8.41e-15 ***
    ## rides        0.21288    0.04197   5.073 5.57e-07 ***
    ## games        0.07066    0.03026   2.335   0.0199 *  
    ## wait         0.38138    0.02777  13.734  < 2e-16 ***
    ## clean        0.29690    0.04415   6.725 4.89e-11 ***
    ## weekendyes  -0.04589    0.05141  -0.893   0.3725    
    ## logdist      0.06470    0.02572   2.516   0.0122 *  
    ## num.child    0.22717    0.01711  13.274  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5709 on 492 degrees of freedom
    ## Multiple R-squared:  0.6786, Adjusted R-squared:  0.674 
    ## F-statistic: 148.4 on 7 and 492 DF,  p-value: < 2.2e-16

*Observe*:

-   Model shows substantial improvement compared to m2 in adj. R-squared (from 0.55 to 0.67).
-   `logdist` and `num.child` coefficients are signicantly greater than 0, suggesting that people who travel further and have more children, have higher overall satisfaction ratings.
-   Weekend has been labelled to `weekendyes` to denote which direction the coefficient applies to compared to `weekendno`. On average, those who come on the weekend rate their overall satisfaction -0.046 standard deviation lower than those who come on a weekday. NOTE: R converts factor vars automatically by breaking down the factor values into its own predictors in this fashion.
-   Weekend is not a significant predictor of overall satisfaction. We can remove tihs var
-   `num.child` was treated as a numeric var in this model, when it should be treated more like a factor. We can't assume satisfaction has a linear relationship with number of children and that the effect is the same per addit'l child. We should correct this var by turning it into a factor and re-estimating the model

Convert `num.child` to a factor, remove `weekend` and re-estimate model called m4

``` r
sat.std$num.child <-as.factor(sat.std$num.child)
m4 <-  lm(overall~ rides + games + wait + clean +
            logdist + num.child, data=sat.std)
summary(m4)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean + logdist + 
    ##     num.child, data = sat.std)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.27036 -0.34945 -0.00408  0.31292  1.53750 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.70229    0.03915 -17.940  < 2e-16 ***
    ## rides        0.22388    0.03536   6.332 5.48e-10 ***
    ## games        0.04272    0.02549   1.676   0.0944 .  
    ## wait         0.38453    0.02336  16.460  < 2e-16 ***
    ## clean        0.30893    0.03718   8.308 9.65e-16 ***
    ## logdist      0.03290    0.02162   1.522   0.1286    
    ## num.child1   1.01733    0.07121  14.286  < 2e-16 ***
    ## num.child2   1.03828    0.05632  18.434  < 2e-16 ***
    ## num.child3   0.98023    0.07017  13.969  < 2e-16 ***
    ## num.child4   0.93252    0.08023  11.623  < 2e-16 ***
    ## num.child5   1.00173    0.10361   9.668  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4792 on 489 degrees of freedom
    ## Multiple R-squared:  0.775,  Adjusted R-squared:  0.7704 
    ## F-statistic: 168.4 on 10 and 489 DF,  p-value: < 2.2e-16

*Observe* \* Adj. r-squared increased from m3 (0.67 to 0.77) \* The baseline level is `num.child.factor0` (no children) since it's not included in here as a coefficient. We can interpret each coefficient as the difference between that factor level vs. the baseline of no children. \* All 5 `num.child` coefficients are significant which suggests some redundancy in the data. The satisfaction is about the same regardless of how many children are in the party (almost 1 std. deviation higher than people with no children). We can declare a new var that summarizes parties with children as `has.child` (Y/N). This simplifies the model and moight increase accuracy. \* I.e. parties with 1 child rate their overall satisfaction ~1.017 std. deviation higher than parties with no children.

Declare a new var called `has.child` which is TRUE when party has children. Re-estimate the model using new factor var

``` r
sat.std$has.child <- factor(sat.std$num.child !=0) # When num.chid is > 0, provide a 1
m5 <- lm(overall~ rides + games + wait + clean +
            logdist + has.child, data=sat.std)

summary(m5)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean + logdist + 
    ##     has.child, data = sat.std)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.23491 -0.35539 -0.00838  0.32435  1.46624 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -0.70195    0.03906 -17.969  < 2e-16 ***
    ## rides          0.22272    0.03512   6.342 5.12e-10 ***
    ## games          0.04424    0.02539   1.742   0.0821 .  
    ## wait           0.38582    0.02326  16.589  < 2e-16 ***
    ## clean          0.30876    0.03696   8.354 6.75e-16 ***
    ## logdist        0.03512    0.02148   1.635   0.1027    
    ## has.childTRUE  1.00565    0.04683  21.472  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4782 on 493 degrees of freedom
    ## Multiple R-squared:  0.7741, Adjusted R-squared:  0.7713 
    ## F-statistic: 281.5 on 6 and 493 DF,  p-value: < 2.2e-16

*Observe*

-   R-square didn't change from m4, which means the simplification didn't deter the model

##### Is the relationship between satisfaction and waiting times different for parties with/without children?

We might hypothesize that wait time would be more important to parties with children. To explore this hypothesis, we need to incorporate interaction terms into the model.

### 7.5 Interaction Terms

Include an interaction of 2 terms by ":" operator between the vars. When adding interaction:

1.  Consider standardizing the predictors when moreling interactions in order to have interpretable and comparable scale for interaction

2.  Always include main effects when including interaction . If we don't estimate the main effects, we don't know whether an interaction is in fact due to the interaction or due to one of the vars' main effects

Let's create a new model with interactions between each satisfaction var (4 total) and two vars: `has.child` and `weekend`. There will be 8 interaction terms total.

``` r
m6 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child
         + rides:has.child + games:has.child + wait:has.child + clean:has.child
         + rides:weekend + games:weekend + wait:weekend + clean:weekend, data=sat.std)

summary(m6)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean + weekend + 
    ##     logdist + has.child + rides:has.child + games:has.child + 
    ##     wait:has.child + clean:has.child + rides:weekend + games:weekend + 
    ##     wait:weekend + clean:weekend, data = sat.std)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.15097 -0.31487 -0.01245  0.30277  1.45388 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -0.677443   0.043034 -15.742  < 2e-16 ***
    ## rides                0.146980   0.067982   2.162  0.03110 *  
    ## games                0.079569   0.049365   1.612  0.10765    
    ## wait                 0.129718   0.044266   2.930  0.00355 ** 
    ## clean                0.312757   0.079685   3.925 9.93e-05 ***
    ## weekendyes          -0.020461   0.041261  -0.496  0.62021    
    ## logdist              0.025801   0.020671   1.248  0.21258    
    ## has.childTRUE        0.995076   0.044869  22.177  < 2e-16 ***
    ## rides:has.childTRUE  0.057837   0.073070   0.792  0.42902    
    ## games:has.childTRUE -0.064043   0.052797  -1.213  0.22572    
    ## wait:has.childTRUE   0.350649   0.047241   7.423 5.21e-13 ***
    ## clean:has.childTRUE -0.001854   0.079710  -0.023  0.98146    
    ## rides:weekendyes     0.061784   0.067750   0.912  0.36225    
    ## games:weekendyes     0.018511   0.049036   0.377  0.70597    
    ## wait:weekendyes      0.035168   0.044463   0.791  0.42936    
    ## clean:weekendyes    -0.027305   0.071005  -0.385  0.70074    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4521 on 484 degrees of freedom
    ## Multiple R-squared:  0.8018, Adjusted R-squared:  0.7956 
    ## F-statistic: 130.5 on 15 and 484 DF,  p-value: < 2.2e-16

*Observe*:

-   Only one of the interaction terms, `wait:has.childTRUE` is significant.
-   We can drop the non-significant interactions to create a new model m7. Also drop `weekend` since none of the interaction terms are signifciant nor the main effect.
-   `logdist` is not significant. Let's drop it from next model. As we will see, R-squared doesn't change much after dropping it.

``` r
m7 <- lm(formula = overall ~ rides + games + wait + clean + 
     has.child + wait:has.child, data = sat.std)

summary(m7)
```

    ## 
    ## Call:
    ## lm(formula = overall ~ rides + games + wait + clean + has.child + 
    ##     wait:has.child, data = sat.std)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.11351 -0.31235 -0.00945  0.31453  1.44507 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -0.69596    0.03683 -18.895  < 2e-16 ***
    ## rides               0.21054    0.03313   6.354 4.77e-10 ***
    ## games               0.04828    0.02397   2.015   0.0445 *  
    ## wait                0.14983    0.03691   4.059 5.73e-05 ***
    ## clean               0.30476    0.03485   8.744  < 2e-16 ***
    ## has.childTRUE       1.00234    0.04412  22.718  < 2e-16 ***
    ## wait:has.childTRUE  0.34921    0.04382   7.970 1.11e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4513 on 493 degrees of freedom
    ## Multiple R-squared:  0.7988, Adjusted R-squared:  0.7963 
    ## F-statistic: 326.2 on 6 and 493 DF,  p-value: < 2.2e-16

*Observe*:

-   Attending the park with chldren is a predictor of higher satisfaction
-   Waiting time is more important influencer among those with children than those with no children. Perhaps children go on more rides and their parents are therefore more influenced by wait times?

To share results with others, create a new satisfaction drivers plot using `coef()plot`:

``` r
library(coefplot)
coefplot(m7, 
     intercept=F,
     outerCI = 1.96, # for 95 CI
     ylab= "Rating of Feature",
     xlab="Relationship with Overall Satisfaction", 
     sort="magnitude")
```

    ## Warning: Ignoring unknown aesthetics: xmin, xmax

<img src="ch-7-linear_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-35-1.png" style="display: block; margin: auto;" />

##### What can we do with these results as marketers? What actions can we take?

After interpretation, a crucial step is to think carefully about the implications and where one might be able to make a product or marketing intervention:

-   If we want to increase overall satisfaction, we can try to increase the \# of visitors w/children.
-   If we want to appeal to visitors w/o children, engage in further research of why their ratings are lower than visitors with children. What can we do to appeal to them? and why are they less satisfied?
-   If we are allocating budget to personnel, the importance of cleanliness suggests continuing to allocate resources there (vs. games, etc.).
-   Learn more about the assocation between children and waiting time - are there things we can do to decrease waiting time or make it more enjoyable?

##### On Interpreting Intercepts

-   It guarantees that your residuals have a mean of zero.
-   The intercept (often labeled the constant) is the expected mean value of Y when all X=0.
-   FMI, <http://blog.minitab.com/blog/adventures-in-statistics-2/regression-analysis-how-to-interpret-the-constant-y-intercept>

#### Advanced Formula Syntax

-   "x+z" for main effects
-   "x:z" for specific interactions between 2 vars
-   "x\*z" for both main effects and interactions
-   "." to include all main effects
-   "-x" to omit var z
-   `y ~ . -x` to include all vars except x
-   `-1` to remove the intercept. (DO NOT REMOVE in most cases)
-   `y ~ (u + v + w)^3` include u,v,w vars and all interactions among each up to 3 way interaction terms (u:v:w)

### 7.6 Overfitting

As we add more predictors to a model:

-   Coefficient estimates become less precise (larger CI's) due to both the number of effects and associations among the vars
-   There's larger standard errosr of the coefficients, indicating lower confidence in the estimates (p-values)
-   R^2 will always increase

The process of adding too many vars that end up with a less precise or non-generalizable model is called *overfitting*.

One way to counteract this is to select a subset of data to *hold out* and not use to estimate the model. After fitting hte model with the training data set, use `predict()` on the test data set (hold out data set) and see how well it performs. Overfitted models will perform poorly when predicting outcomes for holdout data

Stepwise selection is a tradit'l approach to select vars while trying to avoid overfitting.

### 7.7 Best Practices Steps for Linear Model Fitting

1.  Inspect dat ato ensure it's clean and without erros

2.  Check distributions of each var to ensure there's no high skew. If a var is skewed, consider transformations

3.  Examine bivariate scatterplots and corr matrix to check for extremely correlated vars (r &gt; 0.85). Omit those vars or consider transforming them if needed

4.  Consider standardizing the data with `scale()` to ensure estimates are on a consistent scale and for cmoparisons

5.  Fit the model. Check residual quantiles in the output. The residuals show how well the model accoutns for individual obs.

6.  Run diagnostic plots using `plot()` to eval whether a linear model is appropriate or whether there's nonlinearity. It'll also identify pot'l outliers. Check outliers

7.  Try several models and compare them for overall interpretability and model fit by inspecting 1) residuals' spread and 2) R^2. If models are nested, use ANOVA for comparison

8.  Report out on confidence intervals along with interpretations and recommendations

### 7.8 Bayesian Linear Models with `MCMCRegress()`

Bayesian inference for linear models estimate the *most probable coefficients relating to the explanatory vars*. However, the Bayesian method does this by sampling from the posterior distribution of estimated model parameters using the Markov-Chain Monte Carlo.

``` r
library(MCMCpack)
```

    ## Loading required package: coda

    ## Loading required package: MASS

    ## ##
    ## ## Markov Chain Monte Carlo Package (MCMCpack)

    ## ## Copyright (C) 2003-2017 Andrew D. Martin, Kevin M. Quinn, and Jong Hee Park

    ## ##
    ## ## Support provided by the U.S. National Science Foundation

    ## ## (Grants SES-0350646 and SES-0350613)
    ## ##

``` r
m7.bayes <- MCMCregress(overall ~ rides + games + wait + clean + has.child + wait:has.child, data=sat.std)

summary(m7.bayes)
```

    ## 
    ## Iterations = 1001:11000
    ## Thinning interval = 1 
    ## Number of chains = 1 
    ## Sample size per chain = 10000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##                        Mean      SD  Naive SE Time-series SE
    ## (Intercept)        -0.69602 0.03715 0.0003715      0.0003715
    ## rides               0.21053 0.03357 0.0003357      0.0003357
    ## games               0.04819 0.02422 0.0002422      0.0002422
    ## wait                0.15039 0.03668 0.0003668      0.0003565
    ## clean               0.30460 0.03505 0.0003505      0.0003505
    ## has.childTRUE       1.00248 0.04470 0.0004470      0.0004470
    ## wait:has.childTRUE  0.34870 0.04350 0.0004350      0.0004350
    ## sigma2              0.20436 0.01297 0.0001297      0.0001331
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##                          2.5%      25%      50%      75%    97.5%
    ## (Intercept)        -0.7681536 -0.72091 -0.69641 -0.67134 -0.62173
    ## rides               0.1449194  0.18814  0.21079  0.23311  0.27688
    ## games               0.0008568  0.03172  0.04797  0.06469  0.09511
    ## wait                0.0779791  0.12577  0.15046  0.17511  0.22346
    ## clean               0.2365368  0.28102  0.30464  0.32833  0.37402
    ## has.childTRUE       0.9141947  0.97301  1.00238  1.03254  1.08859
    ## wait:has.childTRUE  0.2638350  0.31924  0.34898  0.37794  0.43340
    ## sigma2              0.1802559  0.19535  0.20384  0.21295  0.23113

*Observe*:

-   Model has drawn 10,000 samples from the estimated distribution of possible coefficients for model m7. It then describes those 10K sets of estimates in 2 ways: using central tendency estimates (mean, std. dev) and distirbution quantiles
-   Coefficient estimates, means, medians, are very similar to classical model earlier.

##### Tips on Interpreting Bayesian Linear Models

-   Bayesian posterior distribution may be asymmetric; the distribution of estimates could be skewed if that provides a better fit to the data
-   There are no stat tests or p-values; null hypothesis is not emphasized in Bayesian methods. To determine whether a parameter is liekly to be non-zero, check the 95% Credible Range to directly interpret the credible interval. For example, we conclude the coefficient for `wait:has.childTRUE` is *credibly different* from 0 at a 95% confidence.
-   Draws are producted from the posterior of a linear model.
-   Inferences such as hypothesis testing are more clear and interpretable in the Bayesian approach.
