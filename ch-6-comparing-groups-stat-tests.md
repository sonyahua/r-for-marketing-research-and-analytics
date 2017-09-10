ch-6-comparing-groups-tests
================
Sonya Hua
September 8, 2017

"It looks different but is it really different?" The answer involves our first [inferential statistical](https://statistics.laerd.com/statistical-guides/descriptive-inferential-statistics.php) procedures: *chi-square, t-tests, and ANOVA*.

It's all about estimating parameters and testing hypotheses.

### 6.1 Data for Comparing Groups

``` r
load("C:/Users/sonya/Documents/git/r-for-marketing-research-and-analytics/segdf-Rintro-Ch5.RData")
summary(seg.df)
```

    ##       age           gender        income            kids        ownHome   
    ##  Min.   :19.26   Female:157   Min.   : -5183   Min.   :0.00   ownNO :159  
    ##  1st Qu.:33.01   Male  :143   1st Qu.: 39656   1st Qu.:0.00   ownYes:141  
    ##  Median :39.49                Median : 52014   Median :1.00               
    ##  Mean   :41.20                Mean   : 50937   Mean   :1.27               
    ##  3rd Qu.:47.90                3rd Qu.: 61403   3rd Qu.:2.00               
    ##  Max.   :80.49                Max.   :114278   Max.   :7.00               
    ##   subscribe         Segment   
    ##  subNo :260   Moving Up : 70  
    ##  subYes: 40   Suburb Mix:100  
    ##               Travelers : 80  
    ##               Urban Hip : 50  
    ##                               
    ## 

### 6.2 Chi-Square Test: Testing differences between Group Frequencies

Most of the work in marketing analytics and marketing research involves summarizing the differences between groups using group averages and cross tabs. However, best practice is to be able to use *statistical tests* to determine whether the differences are real or might instead be due to noise in the data.

One of the simplest tests is the *[chi-square test](https://onlinecourses.science.psu.edu/statprogram/node/158)*, which is used with frequency count tables. \*A chi-square test determines whether the frequencies in cells are significantly different from what one would expect on the basis of their total counts if data was randomly sampled from a large population where groups are equally distributed given a sample size. For example, knowing var A will not help inform var B. There are not related or are independent of each other

H0: Variable A and Variable B are independent. Ha: Variable A and Variable B are not independent (dependent)

`chisq.test()` operates on a `table()`

``` r
# Chi-square test # 1
example.data <- rep(c(1:4), times= c(25,25,25,20))
tmp.tab <- table(example.data)
tmp.tab
```

    ## example.data
    ##  1  2  3  4 
    ## 25 25 25 20

``` r
chisq.test(tmp.tab)
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  tmp.tab
    ## X-squared = 0.78947, df = 3, p-value = 0.852

*Observe*:

-   we generated 95 observations of 1:4, compile those in a table, and then test that table for *chi-square independence*.
-   Test evaluates the likelihood of seeing such a result under the null hypothesis that the data is randomly sampled from a population where the values 1 to 4 are equally distributed, given a marginal count of N=95 observations.
-   The p-value of 0.852: Under null hypothesis, there is an 85.2% chance of seeing a data set with differences similar to or greater than those in our table.
-   Under the asssumptions of the chi-square test, our table does not suggest significant differences in frequency between the 4 cells
-   The data shows no evidence that the groups in the population are of unequal size if it was randomly sampled from an equal distribution
-   1:4 are indepndent of each other.

``` r
tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
```

    ## 
    ##  1  2  3  4 
    ## 25 25 25 10

``` r
chisq.test(tmp.tab)
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  tmp.tab
    ## X-squared = 7.9412, df = 3, p-value = 0.04724

*Observe*:

-   p=0.047: We can reject the null hypothesis with 95% confidence that there is no difference between the cells
-   The data in this sample suggests that the distribution of the values 1 to 4 is likely to be unequal in the *larger population*, assuming the data is randomly sampled.

If we have a smaller sample, we would not get the same results even if the relative proportions of customers in each group are the same. Significance tests are sensitive to both the observed difference and sample size:

``` r
tmp.tab <-tmp.tab/5
tmp.tab
```

    ## 
    ## 1 2 3 4 
    ## 5 5 5 2

``` r
chisq.test(tmp.tab)
```

    ## Warning in chisq.test(tmp.tab): Chi-squared approximation may be incorrect

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  tmp.tab
    ## X-squared = 1.5882, df = 3, p-value = 0.6621

*Observe*:

-   The results are not significant and there is no evidence of a real difference in group sizes even though the proportion of people in group 4 is the same as in larger sample above where results were significant.

That's one of the cautions about statistical significance testing: *it's dependent on sample size as well as on the real effect*

Another example, let's return to our simulated segment data (N=300 obs).

##### Are the segment sizes significantly different from one another?

Assuming that the 300 customers are *random sample of a larger population*, we can use the chi-square test. We'll combine `chisq.test()` and `table()` into one command:

``` r
table(seg.df$Segment)
```

    ## 
    ##  Moving Up Suburb Mix  Travelers  Urban Hip 
    ##         70        100         80         50

``` r
chisq.test(table(seg.df$Segment))
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  table(seg.df$Segment)
    ## X-squared = 17.333, df = 3, p-value = 0.0006035

*Observe*

-   With p=0.0006, there are significant differences in segment size
-   Our sample does not support the hypothesis that there is an idenditcal number of custommers in each segment.

##### Is subscription status independent from home ownership?

In other words, are respondents just as likely to subscribe or not subscribe, regardless of home ownership status? Are customers just as likely to own a home or not, independent of subscription status? The null hypothesis is that home ownership and subscription status is independent i.e. that the counts in the cells are as one might expect from marginal proportions.

There's 2 optionos for chi-square test:

1.  For 2x2 contingency tables, chi-square test defaults to using *Yates' correction* which adjusts the chi-square statistic since data is not continuous (comes from a lumpy binomial distribution). To turn this off, use `correct=F` option.

2.  Chi-square tests can calculate confidence intervals based on simulation, whereby it compares the observed table to thousands of simulated tables wit hteh same marginal counts. The p-value indicates the proportion of those simulations with differences between the cell counts nad marginal proportions at least as large as the ones in the observed table using the `sim=T` and `B=SIMULATIONS` arguments

``` r
table(seg.df$subscribe, seg.df$ownHome)
```

    ##         
    ##          ownNO ownYes
    ##   subNo    137    123
    ##   subYes    22     18

``` r
# Using's Yates' correction
chisq.test(table(seg.df$subscribe, seg.df$ownHome))
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table(seg.df$subscribe, seg.df$ownHome)
    ## X-squared = 0.010422, df = 1, p-value = 0.9187

``` r
# Using traditional values without Yates' correction
chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct=F)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(seg.df$subscribe, seg.df$ownHome)
    ## X-squared = 0.074113, df = 1, p-value = 0.7854

``` r
# Using simulation method of 10000 trials
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim=T, B=10000)
```

    ## 
    ##  Pearson's Chi-squared test with simulated p-value (based on 10000
    ##  replicates)
    ## 
    ## data:  table(seg.df$subscribe, seg.df$ownHome)
    ## X-squared = 0.074113, df = NA, p-value = 0.8628

*Observe*

-   Based on high p-value, we fail to reject null hypothesis and conclude that the factors are unrelated. Home ownership is independent of subscription status in our data. There is no relationship between subscriiption rate and home ownership. The factors are independent.

### 6.3 Testing Observed Proportions: `binom.test()`

**Binomial variables** are variables that have only 2 types of values ("Y/N" "0,1").

For example, Chris took a walk in Manhattan and observed 12 groups of Seattle fans and 8 groups of Denver fans. Assuming the observations are a random sample of a binomial value (either Seattle or Denver fans). Is the observed value of 60% Seattle fans significantly different from equal representation (which would be 50% each)?

We use `binom.test(successes, trials, probability)` to test what's the likelihood of randomly observing 12 groups out of 20 in one direction, if the true likelhood is 50%?

``` r
binom.test(12,20,p=0.5)
```

    ## 
    ##  Exact binomial test
    ## 
    ## data:  12 and 20
    ## number of successes = 12, number of trials = 20, p-value = 0.5034
    ## alternative hypothesis: true probability of success is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3605426 0.8088099
    ## sample estimates:
    ## probability of success 
    ##                    0.6

*Observe*:

-   The 95% CI is 36-81% which includes the null hypothesis value of 50% probability.
-   We conclude that observing 60% seattle fans in a sample of 20 does not conclusively demonstate that there are more Seattle fans in the larger group of fans roaming New York.
-   The p-value=0.5034 is not significant - we fail to support the idea that the results are different from the null hypothesis

#### 6.4.1 Interpreting Confidence Intervals

The definition of a 95% CI: *It's the range of possible sample estimates that we would expect to see 95% of the time if we repeatedly estimate the statistic using random samples of the same sample size under the assumption that the true value in an infinite or very large population is the same as our current estimate*

It's the best guess of possible answers we would expect with repeated random sampling.

When the CI excludes null hypothesis, the result is statistically significant.

CI's DO NOT express our degree of confidence in our results since they don't reflect the confidence elvel in the assumptinos made.

CI's are often misunderstood to imply that "the true value lies in the CI range" when it's the other way around "if the true value is what we obtained then we would expect additional esitmates to fall within this CI 95% of the time with random sampling."

The CI is about sample estimates, not about the true value of a population.

Before interpreting a result, make surue it's statitically significant. If not, then the evidence for our result is weak and we should not intepret it. Best pracice is to chart the confifdence intervals when available and always report out on confidence intervals for a more complete and accurate description to stakeholders.

`confint()` determines the CIs for a statistical model

#### What's the probability we would observe 8:12 Seattle fans out of 20, if the true rate is 50%?

Use the *density estimate* for a binomial distribution across the range of interest and sum the point probabilities:

``` r
sum(dbinom(8:12, 20, 0.5))
```

    ## [1] 0.736824

*Observe* If we observe 20 fans and the true split is 50%, there's a 73.7% chance that would would observe between 8 to 12 fans.

##### What if we observed 120 out of 200 people to be Seattle fans? (The same proportion as before but in a larger sample)

``` r
binom.test(120,200,.5)
```

    ## 
    ##  Exact binomial test
    ## 
    ## data:  120 and 200
    ## number of successes = 120, number of trials = 200, p-value =
    ## 0.005685
    ## alternative hypothesis: true probability of success is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.5285357 0.6684537
    ## sample estimates:
    ## probability of success 
    ##                    0.6

*Observe*:

-   The CI no longer includes 50%. The p-value &lt; 0.05, indicating there is a statistically signifciant difference.

An *exact* binomial test may be too conservative (wide CI) in its estimation of CI's. Another method is to use the *[agresti-coull](http://www.stat.ufl.edu/~aa/articles/agresti_coull_1998.pdf)* method to get a slightly smaller CI but still includes 50%. Use `binom.confint(method="ac")`

``` r
library(binom)
binom.confint(12, 20, method="ac")
```

    ##          method  x  n mean     lower     upper
    ## 1 agresti-coull 12 20  0.6 0.3860304 0.7817446

The `binom` package also computes several other varients of the binomial test including a Bayesian version.
