ch-13-choice-modeling-conjoint-analysis
================
Sonya Hua
September 18, 2017

Choice Modeling Using Conjoint Analysis
---------------------------------------

Choice-based conjoint analysis is a survey method used to analyze customers' product choices within a product category to understand how features and price affects which product a customer will choose. Survey data on customers' choices can be analyzed to determine which features of a product i.e. package size, brand, flavor, are most attractive to customers and how they trade off desirable features vs. price.

Product choice data doesn't fit well into the linear modeling frameowrk, because the outcome we observe is not a number per product, but rather a choice among several options, each of which as its own set of attributes. To account for this unique data, marketers adapt choice models, which are well suited to udnerstanding relationships between product attributes and customers' choices among sets of products. *Multinomial logit model* is the most frequently used choice model in marketing.

A *Multinomial logistic regression* (often just called 'multinomial regression') is used to predict a *categorical* dependent variable given one or more independent variables. It is sometimes considered an extension of binomial logistic regression to allow for a dependent variable with more than two categories.

Conjoint Survey Terminology: **Alternatives** - The product options (product profile) in the survey **Attributes** - product features **Level** - Within each attribute are levels of values that vary across the alternatives (price at $3, $4, $5)

Choosing products in a survey mimics the natural task that consumers face everyday when in shopping. Hence, this is the standard method for conjoint analysis (choice-based).

The difference between choice-based conjoint and metric conjoint is the structure of the data we collect. Each observation is a choice among alternatives with varying levels of attributes.

``` r
cbc.df <-read.csv("http://goo.gl/5xQObB")
head(cbc.df)
```

    ##   resp.id ques alt carpool seat cargo  eng price choice
    ## 1       1    1   1     yes    6   2ft  gas    35      0
    ## 2       1    1   2     yes    8   3ft  hyb    30      0
    ## 3       1    1   3     yes    6   3ft  gas    30      1
    ## 4       1    2   1     yes    6   2ft  gas    30      0
    ## 5       1    2   2     yes    7   3ft  gas    35      1
    ## 6       1    2   3     yes    6   2ft elec    35      0

*Obs*:

-   The first 3 rows describe the first question that was asked of repondant 1 (resp.id =1). Respondant 1 choice ALT3.
-   The `choice` column shows chosen alternatives.
-   The data organized in this manner is called the long format where each alternative is on its own row per respondant. This is generally preferred.

There's also a *wide* format where each row corresponds to different question and another format where profiles are stored separately apart from choices. Pay close attention to the required data format for data collection when using R packages or other software systems. Consider creating functions for reformatting for reuse. For example, `Rcbc` provides a helpful set of utilities for converting from the format used by *Sawtooth Software* into the format used by the `ChoiceModelR` package.

#### 13.3.1 Inspect Choice Data

Once we have data properly formatted, get an understanding of the data using basic descriptives:

``` r
summary(cbc.df)
```

    ##     resp.id            ques         alt    carpool         seat      
    ##  Min.   :  1.00   Min.   : 1   Min.   :1   no :6345   Min.   :6.000  
    ##  1st Qu.: 50.75   1st Qu.: 4   1st Qu.:1   yes:2655   1st Qu.:6.000  
    ##  Median :100.50   Median : 8   Median :2              Median :7.000  
    ##  Mean   :100.50   Mean   : 8   Mean   :2              Mean   :6.995  
    ##  3rd Qu.:150.25   3rd Qu.:12   3rd Qu.:3              3rd Qu.:8.000  
    ##  Max.   :200.00   Max.   :15   Max.   :3              Max.   :8.000  
    ##  cargo        eng           price        choice      
    ##  2ft:4501   elec:3010   Min.   :30   Min.   :0.0000  
    ##  3ft:4499   gas :3005   1st Qu.:30   1st Qu.:0.0000  
    ##             hyb :2985   Median :35   Median :0.0000  
    ##                         Mean   :35   Mean   :0.3333  
    ##                         3rd Qu.:40   3rd Qu.:1.0000  
    ##                         Max.   :40   Max.   :1.0000

*Obs*: We see for 3-level attributes that each level occurred ~3000x times and 4500x for 2-level attributes.

However, a more informational way to summarize choice data is to compute *choice counts*, which are cross tabs on the number of times respondents chose an alternative x each feature. Best practice is to compute choice counts for each attribute before estimating a choice model. If we find that our model's estimates or predicted shares are not consistent with the raw counts, there might have been a mistake in the data formatting.

``` r
xtabs(choice ~ price, data=cbc.df)
```

    ## price
    ##   30   35   40 
    ## 1486  956  558

*Obs* The $30(K) price point was chosen more often than others.

``` r
xtabs(choice~ cargo, data=cbc.df)
```

    ## cargo
    ##  2ft  3ft 
    ## 1312 1688

#### 13.3 Fitting a Choice Model

Using `mlogit` package, `mlogit` estimates the most basic and commonly used choice model, the *multinomial logit model* aka conditional logit. `mlogit` requires choice data to be specially formatted using the `mlogit.data()` function. We pass our choice data to `mlogit.data` along with a few paramters telling it how the data is organized. It can accept long or wide format using `shape=` parameter. The `choice, varying, and id.var` parameters indicate which columns contain the response data, attributes, and the respondent IDs respectively.

``` r
#install.packages("mlogit")
library(mlogit)
```

    ## Loading required package: Formula

    ## Loading required package: maxLik

    ## Loading required package: miscTools

    ## 
    ## Please cite the 'maxLik' package as:
    ## Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.
    ## 
    ## If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
    ## https://r-forge.r-project.org/projects/maxlik/

``` r
cbc.mlogit<- mlogit.data(data=cbc.df, choice="choice", shape="long", 
                         varying=3:6, alt.levels=paste("pos",1:3),
                         id.var="resp.id")
```

*Obs*: The resulting `cbc.mlogit` is an mlogit.data object that can be used to estimate our model using `mlogit()`

In the `mlogit()` model, we specify 0 + in the formula indicating we don't want an intercept included. When we include the intercept, mlogit adds 2 addditiaonl paramaters that indciate preference for hte different positions in the question (left, right, middle) . In a conjoint analysis, we typically don't expect ppl to chooce a minivan because it's on the left or right in a survey question. For that rason, we do not expect the setimated alternative specific constants to differ from 0. If however, we found one of these parameters to be significant, then that indicates that some respondents are simply choosing the first or last option without reading hte question.

``` r
m1 <-mlogit(choice~0 + seat + cargo + eng + price, data=cbc.mlogit)
summary(m1)
```

    ## 
    ## Call:
    ## mlogit(formula = choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit, 
    ##     method = "nr", print.level = 0)
    ## 
    ## Frequencies of alternatives:
    ##   pos 1   pos 2   pos 3 
    ## 0.32700 0.33467 0.33833 
    ## 
    ## nr method
    ## 5 iterations, 0h:0m:0s 
    ## g'(-H)^-1g = 4.49E-05 
    ## successive function values within tolerance limits 
    ## 
    ## Coefficients :
    ##            Estimate Std. Error  t-value  Pr(>|t|)    
    ## seat     -0.1573471  0.0308754  -5.0962 3.466e-07 ***
    ## cargo3ft  0.4725247  0.0505942   9.3395 < 2.2e-16 ***
    ## enggas    1.5149617  0.0669463  22.6295 < 2.2e-16 ***
    ## enghyb    0.7094536  0.0650369  10.9085 < 2.2e-16 ***
    ## price    -0.1720938  0.0068905 -24.9754 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Log-Likelihood: -2606.9
