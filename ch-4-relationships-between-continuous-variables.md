ch-4-relationships-between-continuous-variables
================
Sonya Hua
September 4, 2017

The most important insights in marketing analysis often come from undrstanding relationships between variables. Identifying these kinds of relationships helps marketers understand how to reach customers more effectively. For example, if people who live closer to a store visit more frequently and buy more, then an obvious strategy would be to send adivertisements to people who live in the area.

In this chapter we focus on understanding pair-wise relationships between variables in multivariate data, and examine how to visualize the relationships and compute statistics that describe their associations. These are the most important ways to assess relationships between continuous variables. The first step in any analylsis is to exploer the data and its basic properties including the relationships among pairs of variables before model-building.

### 4.1 Simulating Customer Data (Retailer Data with Online/In-store sales, Survey responses)

We simulate a data set that describes each customer of a multi-channel retailer (Online & In-store) and their transactions for 1 year. This data also includes a subset of custoemrs for whom we have survey data on product satisfaction. It contains a data set of 1000 customers. This data is typical of what one might sample from a company's CRM system.

``` r
set.seed(21821)
ncust <- 1000 
cust.df <- data.frame(cust.id=as.factor(c(1:ncust))) # customer id's are factors
```

**Customer Id**- unique identifier per customer

**Age** - sampling from a random normal distribution with mean 35 and sd =5

**Credit Score** - sampling from a rnorm where its mean of the distribution is related to the customer's age, with older customers having higher credit scores on average

**Email**- (Y/N) indicating whether the customer has an email on file using the sample() function

**Distance to Store (Miles) **- follows expontential of the normal distribution. This gives distances that are all positive with many distances that are relatively close to the nearest store and fewer that are far from a store. aka. lognormal distribution using `rlnorm()`

``` r
cust.df$age <- rnorm(n=ncust, mean=35, sd=5)
cust.df$credit.score <- rnorm(n=ncust, mean=3*cust.df$age + 620, sd=50)

cust.df$email <- factor(sample(c("yes","no"), size=ncust, replace=TRUE, prob=c(0.8,0.2))) # Factor var with yes, no

cust.df$distance.to.store <- rlnorm(n=ncust, meanlog=2, sdlog=1.2)

summary(cust.df)
```

    ##     cust.id         age         credit.score   email    
    ##  1      :  1   Min.   :19.34   Min.   :543.0   no :186  
    ##  2      :  1   1st Qu.:31.43   1st Qu.:691.7   yes:814  
    ##  3      :  1   Median :35.10   Median :725.5            
    ##  4      :  1   Mean   :34.92   Mean   :725.5            
    ##  5      :  1   3rd Qu.:38.20   3rd Qu.:757.2            
    ##  6      :  1   Max.   :51.86   Max.   :880.8            
    ##  (Other):994                                            
    ##  distance.to.store 
    ##  Min.   :  0.2136  
    ##  1st Qu.:  3.3383  
    ##  Median :  7.1317  
    ##  Mean   : 14.6553  
    ##  3rd Qu.: 16.6589  
    ##  Max.   :267.0864  
    ## 

**/\# of Online visits** - follow a negative binomial distribution - a discrete distribution often used to model counts of events over time. Like the log normal distribution, the negative binomial distribution generates positive values and a long right-hand tail, meaning that most customers make relatively few visits and a few customers make many visits. We use `rnbinom()` function.

-   The `size` argument sets the degree of dispersion (variation) for the samples
-   We add ~15 online visits for customers who have an email on file using `ifelse()` to generate a vector 0 or 15 based on whether they have an email on file
-   We add/subtract visits from the target mean based on the customer's age relative to the sample median since customers who are younger tend to make more online visits

**/\# of Online Transactions **- Per online visit, we assume there's a 30% probability of placicng an order (1) vs. not (0).

**Online spend** - We assume that amount spent in those orders are lognormally distributed (`exp(rnorm())`). The random value for amount spent per transaction (follows lognormal distribution with mean 3, sd=0.1) is multiplied by the \# of online transactions in order to get the total amount spent.

``` r
cust.df$online.visits <-rnbinom(ncust, size=0.3, mu=15 + ifelse(cust.df$email=="yes",15,0) - 0.7 * (cust.df$age-median(cust.df$age)))

cust.df$online.trans <- rbinom(ncust, size=cust.df$online.visits, prob=0.3)
cust.df$online.spend <- rlnorm(ncust, meanlog=3, sdlog=0.1) * cust.df$online.trans

#Best practice to check data along the way
summary(cust.df)
```

    ##     cust.id         age         credit.score   email    
    ##  1      :  1   Min.   :19.34   Min.   :543.0   no :186  
    ##  2      :  1   1st Qu.:31.43   1st Qu.:691.7   yes:814  
    ##  3      :  1   Median :35.10   Median :725.5            
    ##  4      :  1   Mean   :34.92   Mean   :725.5            
    ##  5      :  1   3rd Qu.:38.20   3rd Qu.:757.2            
    ##  6      :  1   Max.   :51.86   Max.   :880.8            
    ##  (Other):994                                            
    ##  distance.to.store  online.visits     online.trans      online.spend    
    ##  Min.   :  0.2136   Min.   :  0.00   Min.   :  0.000   Min.   :   0.00  
    ##  1st Qu.:  3.3383   1st Qu.:  0.00   1st Qu.:  0.000   1st Qu.:   0.00  
    ##  Median :  7.1317   Median :  6.00   Median :  2.000   Median :  37.03  
    ##  Mean   : 14.6553   Mean   : 28.29   Mean   :  8.385   Mean   : 170.32  
    ##  3rd Qu.: 16.6589   3rd Qu.: 31.00   3rd Qu.:  9.000   3rd Qu.: 177.89  
    ##  Max.   :267.0864   Max.   :606.00   Max.   :169.000   Max.   :3593.03  
    ## 

**Store Transactions (In-store)** - We assume that transcations follow a negative binomial distribution with lower average number of visits for customers who live farther away

**Store Spend (In-store)** - lognormally distributed simply multiplied by the number of transactions

``` r
cust.df$store.trans <- rnbinom(ncust, size=5,
                               mu=3 / sqrt(cust.df$distance.to.store))

cust.df$store.spend <- rlnorm(ncust, mean=3.5, sd=0.4) * cust.df$store.trans

summary(cust.df)
```

    ##     cust.id         age         credit.score   email    
    ##  1      :  1   Min.   :19.34   Min.   :543.0   no :186  
    ##  2      :  1   1st Qu.:31.43   1st Qu.:691.7   yes:814  
    ##  3      :  1   Median :35.10   Median :725.5            
    ##  4      :  1   Mean   :34.92   Mean   :725.5            
    ##  5      :  1   3rd Qu.:38.20   3rd Qu.:757.2            
    ##  6      :  1   Max.   :51.86   Max.   :880.8            
    ##  (Other):994                                            
    ##  distance.to.store  online.visits     online.trans      online.spend    
    ##  Min.   :  0.2136   Min.   :  0.00   Min.   :  0.000   Min.   :   0.00  
    ##  1st Qu.:  3.3383   1st Qu.:  0.00   1st Qu.:  0.000   1st Qu.:   0.00  
    ##  Median :  7.1317   Median :  6.00   Median :  2.000   Median :  37.03  
    ##  Mean   : 14.6553   Mean   : 28.29   Mean   :  8.385   Mean   : 170.32  
    ##  3rd Qu.: 16.6589   3rd Qu.: 31.00   3rd Qu.:  9.000   3rd Qu.: 177.89  
    ##  Max.   :267.0864   Max.   :606.00   Max.   :169.000   Max.   :3593.03  
    ##                                                                         
    ##   store.trans      store.spend    
    ##  Min.   : 0.000   Min.   :  0.00  
    ##  1st Qu.: 0.000   1st Qu.:  0.00  
    ##  Median : 1.000   Median : 30.05  
    ##  Mean   : 1.323   Mean   : 47.58  
    ##  3rd Qu.: 2.000   3rd Qu.: 66.49  
    ##  Max.   :12.000   Max.   :705.66  
    ## 

**Overall Satisfaction** - It's common for reatilers to survey their customers and record responses in the CRM system. To simulate survey responses, we assume that each customer has an unobserved or pyschological construct of overall satisfaction with the brand. We generate this overall satisfaction from a normal distribution (m=3.1, sd=0.7). It's not directly observable. Instead the survey collects info on 2 items: service satisfaction and selection of products satisfaction. We assume that customers' responses to the survey items are based on unobserved levels of satisfaction overall (aka "halo" in survey response) + the specific levels of satisfaction with the service and product selection

-   To create such a score from a halo variable, we add `sat.overall` (the halo) to a random value specifci to the item, drawn using `rnorm()`. Because the survey responese are on a discrete, ordinal scale (i.e. "Very Satisfied" to "Very Unsatisfied"), we convert our continous random values to discrete integers using `floor()`.

-   We use `cbind()` in our summary() function to temporarily combine our two vectors of data into a *matrix* so that we can get a combined summary with a single line of code.

``` r
# create temporary variables to be assigned to data frame vars later
sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))

summary(cbind(sat.overall, sat.service, sat.selection))
```

    ##   sat.overall     sat.service    sat.selection   
    ##  Min.   :0.617   Min.   :0.000   Min.   :-1.000  
    ##  1st Qu.:2.632   1st Qu.:3.000   1st Qu.: 2.000  
    ##  Median :3.087   Median :3.000   Median : 2.000  
    ##  Mean   :3.100   Mean   :3.106   Mean   : 2.404  
    ##  3rd Qu.:3.569   3rd Qu.:4.000   3rd Qu.: 3.000  
    ##  Max.   :5.293   Max.   :6.000   Max.   : 5.000

*Observe* We have min/max values we need to adjust

We will replace values that are greater than 5 with 5, values that are &lt; 1 with 1. This enforces the *floor* and *ceiling* effects often noted in survey response literature.

``` r
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection <1] <- 1
sat.selection[sat.selection >5] <- 5

# check
summary(cbind(sat.service, sat.selection))
```

    ##   sat.service    sat.selection  
    ##  Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:3.000   1st Qu.:2.000  
    ##  Median :3.000   Median :2.000  
    ##  Mean   :3.106   Mean   :2.426  
    ##  3rd Qu.:4.000   3rd Qu.:3.000  
    ##  Max.   :5.000   Max.   :5.000

**No Response (T/F)** - Some customers do not respond to surveys.We will eliminate the simulated answers for a subset of responedents who are modeled as "not answering". We do this by create a var of T/F values called `no.response` and then assign a value of NA for the survey response for customers whose `no.response` is TRUE. We model non.response as a function of age, with higher probability of not responding to the survey for *older customers*.

``` r
no.response <- as.logical(rbinom(ncust, size=1, prob=cust.df$age/100))
summary(no.response)
```

    ##    Mode   FALSE    TRUE 
    ## logical     659     341

``` r
# use the T/F in no.response to clear some of the survey satisfaction values for some customers
sat.selection[no.response] <- NA
sat.service[no.response] <- NA

# check
summary(cbind(sat.service, sat.selection))
```

    ##   sat.service   sat.selection  
    ##  Min.   :1.00   Min.   :1.000  
    ##  1st Qu.:3.00   1st Qu.:2.000  
    ##  Median :3.00   Median :2.000  
    ##  Mean   :3.07   Mean   :2.401  
    ##  3rd Qu.:4.00   3rd Qu.:3.000  
    ##  Max.   :5.00   Max.   :5.000  
    ##  NA's   :341    NA's   :341

*Observe* There are 324 NA's according to summary.

``` r
# Add the survery responses to cust.df

cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection

summary(cust.df)
```

    ##     cust.id         age         credit.score   email    
    ##  1      :  1   Min.   :19.34   Min.   :543.0   no :186  
    ##  2      :  1   1st Qu.:31.43   1st Qu.:691.7   yes:814  
    ##  3      :  1   Median :35.10   Median :725.5            
    ##  4      :  1   Mean   :34.92   Mean   :725.5            
    ##  5      :  1   3rd Qu.:38.20   3rd Qu.:757.2            
    ##  6      :  1   Max.   :51.86   Max.   :880.8            
    ##  (Other):994                                            
    ##  distance.to.store  online.visits     online.trans      online.spend    
    ##  Min.   :  0.2136   Min.   :  0.00   Min.   :  0.000   Min.   :   0.00  
    ##  1st Qu.:  3.3383   1st Qu.:  0.00   1st Qu.:  0.000   1st Qu.:   0.00  
    ##  Median :  7.1317   Median :  6.00   Median :  2.000   Median :  37.03  
    ##  Mean   : 14.6553   Mean   : 28.29   Mean   :  8.385   Mean   : 170.32  
    ##  3rd Qu.: 16.6589   3rd Qu.: 31.00   3rd Qu.:  9.000   3rd Qu.: 177.89  
    ##  Max.   :267.0864   Max.   :606.00   Max.   :169.000   Max.   :3593.03  
    ##                                                                         
    ##   store.trans      store.spend      sat.service   sat.selection  
    ##  Min.   : 0.000   Min.   :  0.00   Min.   :1.00   Min.   :1.000  
    ##  1st Qu.: 0.000   1st Qu.:  0.00   1st Qu.:3.00   1st Qu.:2.000  
    ##  Median : 1.000   Median : 30.05   Median :3.00   Median :2.000  
    ##  Mean   : 1.323   Mean   : 47.58   Mean   :3.07   Mean   :2.401  
    ##  3rd Qu.: 2.000   3rd Qu.: 66.49   3rd Qu.:4.00   3rd Qu.:3.000  
    ##  Max.   :12.000   Max.   :705.66   Max.   :5.00   Max.   :5.000  
    ##                                    NA's   :341    NA's   :341

*Observe* the data set is now complete and ready for analysis

### 4.2 Exploring Associations b/w Variables via Scatterplots
