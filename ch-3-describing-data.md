Ch 3. Describing Data
================
Sonya Hua
September 1, 2017

3.1 Simulating Data
-------------------

It's important to describe and explore any data set before moving on to more complex analysis. We will be created data to be analyzed in later parts of the chapter. The process of creating data lets us practice and deepen R skills from Ch. 2. It also lets us manipulate synthetic dat, run analyses again, and examine how the results can change. R analysts often use simulated data to prove that their methods are working as expected.

Our 1st data set is composed of observations of total sales by week for 2 products at a chain of stores around the world (20 stores total) over 2 years, with price and promotion status.

### 3.1.1 Store Data & Setting Up the Data Structure

``` r
k.stores <- 20
k.weeks <- 104

# create a data frame of initially missing values to hold the data
store.df <- data.frame(matrix(NA, ncol=10, nrow=k.stores * k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales", 
                     "p1price", "p2price", "p1prom", "p2prom", "country")  # Assign Var Names to df
str(store.df)
```

    ## 'data.frame':    2080 obs. of  10 variables:
    ##  $ storeNum: logi  NA NA NA NA NA NA ...
    ##  $ Year    : logi  NA NA NA NA NA NA ...
    ##  $ Week    : logi  NA NA NA NA NA NA ...
    ##  $ p1sales : logi  NA NA NA NA NA NA ...
    ##  $ p2sales : logi  NA NA NA NA NA NA ...
    ##  $ p1price : logi  NA NA NA NA NA NA ...
    ##  $ p2price : logi  NA NA NA NA NA NA ...
    ##  $ p1prom  : logi  NA NA NA NA NA NA ...
    ##  $ p2prom  : logi  NA NA NA NA NA NA ...
    ##  $ country : logi  NA NA NA NA NA NA ...

``` r
dim(store.df) # get dimensions of df
```

    ## [1] 2080   10

Create 2 vectors that will represent the store number and country per observation:

``` r
(store.num <- 101:(100+k.stores))
```

    ##  [1] 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
    ## [18] 118 119 120

``` r
(store.cty <- c(rep("US",3), rep("DE",5), rep("GB", 3), rep("BR",2),
                rep("JP",4), rep("AU",1), rep("CN",2)))  # Store's country location
```

    ##  [1] "US" "US" "US" "DE" "DE" "DE" "DE" "DE" "GB" "GB" "GB" "BR" "BR" "JP"
    ## [15] "JP" "JP" "JP" "AU" "CN" "CN"

``` r
length(store.cty)
```

    ## [1] 20

Now we replace the appropriate cols in the df with those values using `rep()` to expand the vectors to match the \# of stores and weeks

``` r
store.df$storeNum <- rep(store.num, each=k.weeks) # each = # of times each element is repeated
# This is different from "times=" which repeats the whole vector n-times
store.df$country <- rep(store.cty, each =k.weeks) 
rm(store.num, store.cty) # clean up memory
```

Do the same process for the Week and Year columns:

``` r
store.df$Week <- rep(c(1:52),times=k.stores * 2) # Replicate 52 weeks 40 times so that there will be 2 years per store
store.df$Year <- rep(c(1,2), each=52, times=k.stores)  # Replicate Year 1 and 2-  52x per store
```

Let's check the overall data structure:

``` r
str(store.df)
```

    ## 'data.frame':    2080 obs. of  10 variables:
    ##  $ storeNum: int  101 101 101 101 101 101 101 101 101 101 ...
    ##  $ Year    : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Week    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ p1sales : logi  NA NA NA NA NA NA ...
    ##  $ p2sales : logi  NA NA NA NA NA NA ...
    ##  $ p1price : logi  NA NA NA NA NA NA ...
    ##  $ p2price : logi  NA NA NA NA NA NA ...
    ##  $ p1prom  : logi  NA NA NA NA NA NA ...
    ##  $ p2prom  : logi  NA NA NA NA NA NA ...
    ##  $ country : chr  "US" "US" "US" "US" ...

*Observe*

-   Data values populated as expected, with proper col names
-   `country` has a char type when it should be factor var since it is a categorical value
-   `storeNum` has a int type when it should be factor var since we will be categorizing by store number and it is a label from something else

By converting `country` and `storeNum` to factors, R will know to tream them as categorical inputs in subsequent analyses i.e. regression models. It's best practice to set var types correctly early on as they are created to avoid errors later:

``` r
store.df$storeNum <- factor(store.df$storeNum)
store.df$country <- factor(store.df$country)
store.df$Week <- as.integer(store.df$Week)
store.df$Year <- as.integer(store.df$Year)
str(store.df)
```

    ## 'data.frame':    2080 obs. of  10 variables:
    ##  $ storeNum: Factor w/ 20 levels "101","102","103",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Year    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Week    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ p1sales : logi  NA NA NA NA NA NA ...
    ##  $ p2sales : logi  NA NA NA NA NA NA ...
    ##  $ p1price : logi  NA NA NA NA NA NA ...
    ##  $ p2price : logi  NA NA NA NA NA NA ...
    ##  $ p1prom  : logi  NA NA NA NA NA NA ...
    ##  $ p2prom  : logi  NA NA NA NA NA NA ...
    ##  $ country : Factor w/ 7 levels "AU","BR","CN",..: 7 7 7 7 7 7 7 7 7 7 ...

*Observe* storeNum (with 20 levels) and country (with 7 levels) has been converted to factors.

``` r
# check first and last rows for mistakes
head(store.df, 120)
```

    ##     storeNum Year Week p1sales p2sales p1price p2price p1prom p2prom
    ## 1        101    1    1      NA      NA      NA      NA     NA     NA
    ## 2        101    1    2      NA      NA      NA      NA     NA     NA
    ## 3        101    1    3      NA      NA      NA      NA     NA     NA
    ## 4        101    1    4      NA      NA      NA      NA     NA     NA
    ## 5        101    1    5      NA      NA      NA      NA     NA     NA
    ## 6        101    1    6      NA      NA      NA      NA     NA     NA
    ## 7        101    1    7      NA      NA      NA      NA     NA     NA
    ## 8        101    1    8      NA      NA      NA      NA     NA     NA
    ## 9        101    1    9      NA      NA      NA      NA     NA     NA
    ## 10       101    1   10      NA      NA      NA      NA     NA     NA
    ## 11       101    1   11      NA      NA      NA      NA     NA     NA
    ## 12       101    1   12      NA      NA      NA      NA     NA     NA
    ## 13       101    1   13      NA      NA      NA      NA     NA     NA
    ## 14       101    1   14      NA      NA      NA      NA     NA     NA
    ## 15       101    1   15      NA      NA      NA      NA     NA     NA
    ## 16       101    1   16      NA      NA      NA      NA     NA     NA
    ## 17       101    1   17      NA      NA      NA      NA     NA     NA
    ## 18       101    1   18      NA      NA      NA      NA     NA     NA
    ## 19       101    1   19      NA      NA      NA      NA     NA     NA
    ## 20       101    1   20      NA      NA      NA      NA     NA     NA
    ## 21       101    1   21      NA      NA      NA      NA     NA     NA
    ## 22       101    1   22      NA      NA      NA      NA     NA     NA
    ## 23       101    1   23      NA      NA      NA      NA     NA     NA
    ## 24       101    1   24      NA      NA      NA      NA     NA     NA
    ## 25       101    1   25      NA      NA      NA      NA     NA     NA
    ## 26       101    1   26      NA      NA      NA      NA     NA     NA
    ## 27       101    1   27      NA      NA      NA      NA     NA     NA
    ## 28       101    1   28      NA      NA      NA      NA     NA     NA
    ## 29       101    1   29      NA      NA      NA      NA     NA     NA
    ## 30       101    1   30      NA      NA      NA      NA     NA     NA
    ## 31       101    1   31      NA      NA      NA      NA     NA     NA
    ## 32       101    1   32      NA      NA      NA      NA     NA     NA
    ## 33       101    1   33      NA      NA      NA      NA     NA     NA
    ## 34       101    1   34      NA      NA      NA      NA     NA     NA
    ## 35       101    1   35      NA      NA      NA      NA     NA     NA
    ## 36       101    1   36      NA      NA      NA      NA     NA     NA
    ## 37       101    1   37      NA      NA      NA      NA     NA     NA
    ## 38       101    1   38      NA      NA      NA      NA     NA     NA
    ## 39       101    1   39      NA      NA      NA      NA     NA     NA
    ## 40       101    1   40      NA      NA      NA      NA     NA     NA
    ## 41       101    1   41      NA      NA      NA      NA     NA     NA
    ## 42       101    1   42      NA      NA      NA      NA     NA     NA
    ## 43       101    1   43      NA      NA      NA      NA     NA     NA
    ## 44       101    1   44      NA      NA      NA      NA     NA     NA
    ## 45       101    1   45      NA      NA      NA      NA     NA     NA
    ## 46       101    1   46      NA      NA      NA      NA     NA     NA
    ## 47       101    1   47      NA      NA      NA      NA     NA     NA
    ## 48       101    1   48      NA      NA      NA      NA     NA     NA
    ## 49       101    1   49      NA      NA      NA      NA     NA     NA
    ## 50       101    1   50      NA      NA      NA      NA     NA     NA
    ## 51       101    1   51      NA      NA      NA      NA     NA     NA
    ## 52       101    1   52      NA      NA      NA      NA     NA     NA
    ## 53       101    2    1      NA      NA      NA      NA     NA     NA
    ## 54       101    2    2      NA      NA      NA      NA     NA     NA
    ## 55       101    2    3      NA      NA      NA      NA     NA     NA
    ## 56       101    2    4      NA      NA      NA      NA     NA     NA
    ## 57       101    2    5      NA      NA      NA      NA     NA     NA
    ## 58       101    2    6      NA      NA      NA      NA     NA     NA
    ## 59       101    2    7      NA      NA      NA      NA     NA     NA
    ## 60       101    2    8      NA      NA      NA      NA     NA     NA
    ## 61       101    2    9      NA      NA      NA      NA     NA     NA
    ## 62       101    2   10      NA      NA      NA      NA     NA     NA
    ## 63       101    2   11      NA      NA      NA      NA     NA     NA
    ## 64       101    2   12      NA      NA      NA      NA     NA     NA
    ## 65       101    2   13      NA      NA      NA      NA     NA     NA
    ## 66       101    2   14      NA      NA      NA      NA     NA     NA
    ## 67       101    2   15      NA      NA      NA      NA     NA     NA
    ## 68       101    2   16      NA      NA      NA      NA     NA     NA
    ## 69       101    2   17      NA      NA      NA      NA     NA     NA
    ## 70       101    2   18      NA      NA      NA      NA     NA     NA
    ## 71       101    2   19      NA      NA      NA      NA     NA     NA
    ## 72       101    2   20      NA      NA      NA      NA     NA     NA
    ## 73       101    2   21      NA      NA      NA      NA     NA     NA
    ## 74       101    2   22      NA      NA      NA      NA     NA     NA
    ## 75       101    2   23      NA      NA      NA      NA     NA     NA
    ## 76       101    2   24      NA      NA      NA      NA     NA     NA
    ## 77       101    2   25      NA      NA      NA      NA     NA     NA
    ## 78       101    2   26      NA      NA      NA      NA     NA     NA
    ## 79       101    2   27      NA      NA      NA      NA     NA     NA
    ## 80       101    2   28      NA      NA      NA      NA     NA     NA
    ## 81       101    2   29      NA      NA      NA      NA     NA     NA
    ## 82       101    2   30      NA      NA      NA      NA     NA     NA
    ## 83       101    2   31      NA      NA      NA      NA     NA     NA
    ## 84       101    2   32      NA      NA      NA      NA     NA     NA
    ## 85       101    2   33      NA      NA      NA      NA     NA     NA
    ## 86       101    2   34      NA      NA      NA      NA     NA     NA
    ## 87       101    2   35      NA      NA      NA      NA     NA     NA
    ## 88       101    2   36      NA      NA      NA      NA     NA     NA
    ## 89       101    2   37      NA      NA      NA      NA     NA     NA
    ## 90       101    2   38      NA      NA      NA      NA     NA     NA
    ## 91       101    2   39      NA      NA      NA      NA     NA     NA
    ## 92       101    2   40      NA      NA      NA      NA     NA     NA
    ## 93       101    2   41      NA      NA      NA      NA     NA     NA
    ## 94       101    2   42      NA      NA      NA      NA     NA     NA
    ## 95       101    2   43      NA      NA      NA      NA     NA     NA
    ## 96       101    2   44      NA      NA      NA      NA     NA     NA
    ## 97       101    2   45      NA      NA      NA      NA     NA     NA
    ## 98       101    2   46      NA      NA      NA      NA     NA     NA
    ## 99       101    2   47      NA      NA      NA      NA     NA     NA
    ## 100      101    2   48      NA      NA      NA      NA     NA     NA
    ## 101      101    2   49      NA      NA      NA      NA     NA     NA
    ## 102      101    2   50      NA      NA      NA      NA     NA     NA
    ## 103      101    2   51      NA      NA      NA      NA     NA     NA
    ## 104      101    2   52      NA      NA      NA      NA     NA     NA
    ## 105      102    1    1      NA      NA      NA      NA     NA     NA
    ## 106      102    1    2      NA      NA      NA      NA     NA     NA
    ## 107      102    1    3      NA      NA      NA      NA     NA     NA
    ## 108      102    1    4      NA      NA      NA      NA     NA     NA
    ## 109      102    1    5      NA      NA      NA      NA     NA     NA
    ## 110      102    1    6      NA      NA      NA      NA     NA     NA
    ## 111      102    1    7      NA      NA      NA      NA     NA     NA
    ## 112      102    1    8      NA      NA      NA      NA     NA     NA
    ## 113      102    1    9      NA      NA      NA      NA     NA     NA
    ## 114      102    1   10      NA      NA      NA      NA     NA     NA
    ## 115      102    1   11      NA      NA      NA      NA     NA     NA
    ## 116      102    1   12      NA      NA      NA      NA     NA     NA
    ## 117      102    1   13      NA      NA      NA      NA     NA     NA
    ## 118      102    1   14      NA      NA      NA      NA     NA     NA
    ## 119      102    1   15      NA      NA      NA      NA     NA     NA
    ## 120      102    1   16      NA      NA      NA      NA     NA     NA
    ##     country
    ## 1        US
    ## 2        US
    ## 3        US
    ## 4        US
    ## 5        US
    ## 6        US
    ## 7        US
    ## 8        US
    ## 9        US
    ## 10       US
    ## 11       US
    ## 12       US
    ## 13       US
    ## 14       US
    ## 15       US
    ## 16       US
    ## 17       US
    ## 18       US
    ## 19       US
    ## 20       US
    ## 21       US
    ## 22       US
    ## 23       US
    ## 24       US
    ## 25       US
    ## 26       US
    ## 27       US
    ## 28       US
    ## 29       US
    ## 30       US
    ## 31       US
    ## 32       US
    ## 33       US
    ## 34       US
    ## 35       US
    ## 36       US
    ## 37       US
    ## 38       US
    ## 39       US
    ## 40       US
    ## 41       US
    ## 42       US
    ## 43       US
    ## 44       US
    ## 45       US
    ## 46       US
    ## 47       US
    ## 48       US
    ## 49       US
    ## 50       US
    ## 51       US
    ## 52       US
    ## 53       US
    ## 54       US
    ## 55       US
    ## 56       US
    ## 57       US
    ## 58       US
    ## 59       US
    ## 60       US
    ## 61       US
    ## 62       US
    ## 63       US
    ## 64       US
    ## 65       US
    ## 66       US
    ## 67       US
    ## 68       US
    ## 69       US
    ## 70       US
    ## 71       US
    ## 72       US
    ## 73       US
    ## 74       US
    ## 75       US
    ## 76       US
    ## 77       US
    ## 78       US
    ## 79       US
    ## 80       US
    ## 81       US
    ## 82       US
    ## 83       US
    ## 84       US
    ## 85       US
    ## 86       US
    ## 87       US
    ## 88       US
    ## 89       US
    ## 90       US
    ## 91       US
    ## 92       US
    ## 93       US
    ## 94       US
    ## 95       US
    ## 96       US
    ## 97       US
    ## 98       US
    ## 99       US
    ## 100      US
    ## 101      US
    ## 102      US
    ## 103      US
    ## 104      US
    ## 105      US
    ## 106      US
    ## 107      US
    ## 108      US
    ## 109      US
    ## 110      US
    ## 111      US
    ## 112      US
    ## 113      US
    ## 114      US
    ## 115      US
    ## 116      US
    ## 117      US
    ## 118      US
    ## 119      US
    ## 120      US

``` r
tail(store.df, 120)
```

    ##      storeNum Year Week p1sales p2sales p1price p2price p1prom p2prom
    ## 1961      119    2   37      NA      NA      NA      NA     NA     NA
    ## 1962      119    2   38      NA      NA      NA      NA     NA     NA
    ## 1963      119    2   39      NA      NA      NA      NA     NA     NA
    ## 1964      119    2   40      NA      NA      NA      NA     NA     NA
    ## 1965      119    2   41      NA      NA      NA      NA     NA     NA
    ## 1966      119    2   42      NA      NA      NA      NA     NA     NA
    ## 1967      119    2   43      NA      NA      NA      NA     NA     NA
    ## 1968      119    2   44      NA      NA      NA      NA     NA     NA
    ## 1969      119    2   45      NA      NA      NA      NA     NA     NA
    ## 1970      119    2   46      NA      NA      NA      NA     NA     NA
    ## 1971      119    2   47      NA      NA      NA      NA     NA     NA
    ## 1972      119    2   48      NA      NA      NA      NA     NA     NA
    ## 1973      119    2   49      NA      NA      NA      NA     NA     NA
    ## 1974      119    2   50      NA      NA      NA      NA     NA     NA
    ## 1975      119    2   51      NA      NA      NA      NA     NA     NA
    ## 1976      119    2   52      NA      NA      NA      NA     NA     NA
    ## 1977      120    1    1      NA      NA      NA      NA     NA     NA
    ## 1978      120    1    2      NA      NA      NA      NA     NA     NA
    ## 1979      120    1    3      NA      NA      NA      NA     NA     NA
    ## 1980      120    1    4      NA      NA      NA      NA     NA     NA
    ## 1981      120    1    5      NA      NA      NA      NA     NA     NA
    ## 1982      120    1    6      NA      NA      NA      NA     NA     NA
    ## 1983      120    1    7      NA      NA      NA      NA     NA     NA
    ## 1984      120    1    8      NA      NA      NA      NA     NA     NA
    ## 1985      120    1    9      NA      NA      NA      NA     NA     NA
    ## 1986      120    1   10      NA      NA      NA      NA     NA     NA
    ## 1987      120    1   11      NA      NA      NA      NA     NA     NA
    ## 1988      120    1   12      NA      NA      NA      NA     NA     NA
    ## 1989      120    1   13      NA      NA      NA      NA     NA     NA
    ## 1990      120    1   14      NA      NA      NA      NA     NA     NA
    ## 1991      120    1   15      NA      NA      NA      NA     NA     NA
    ## 1992      120    1   16      NA      NA      NA      NA     NA     NA
    ## 1993      120    1   17      NA      NA      NA      NA     NA     NA
    ## 1994      120    1   18      NA      NA      NA      NA     NA     NA
    ## 1995      120    1   19      NA      NA      NA      NA     NA     NA
    ## 1996      120    1   20      NA      NA      NA      NA     NA     NA
    ## 1997      120    1   21      NA      NA      NA      NA     NA     NA
    ## 1998      120    1   22      NA      NA      NA      NA     NA     NA
    ## 1999      120    1   23      NA      NA      NA      NA     NA     NA
    ## 2000      120    1   24      NA      NA      NA      NA     NA     NA
    ## 2001      120    1   25      NA      NA      NA      NA     NA     NA
    ## 2002      120    1   26      NA      NA      NA      NA     NA     NA
    ## 2003      120    1   27      NA      NA      NA      NA     NA     NA
    ## 2004      120    1   28      NA      NA      NA      NA     NA     NA
    ## 2005      120    1   29      NA      NA      NA      NA     NA     NA
    ## 2006      120    1   30      NA      NA      NA      NA     NA     NA
    ## 2007      120    1   31      NA      NA      NA      NA     NA     NA
    ## 2008      120    1   32      NA      NA      NA      NA     NA     NA
    ## 2009      120    1   33      NA      NA      NA      NA     NA     NA
    ## 2010      120    1   34      NA      NA      NA      NA     NA     NA
    ## 2011      120    1   35      NA      NA      NA      NA     NA     NA
    ## 2012      120    1   36      NA      NA      NA      NA     NA     NA
    ## 2013      120    1   37      NA      NA      NA      NA     NA     NA
    ## 2014      120    1   38      NA      NA      NA      NA     NA     NA
    ## 2015      120    1   39      NA      NA      NA      NA     NA     NA
    ## 2016      120    1   40      NA      NA      NA      NA     NA     NA
    ## 2017      120    1   41      NA      NA      NA      NA     NA     NA
    ## 2018      120    1   42      NA      NA      NA      NA     NA     NA
    ## 2019      120    1   43      NA      NA      NA      NA     NA     NA
    ## 2020      120    1   44      NA      NA      NA      NA     NA     NA
    ## 2021      120    1   45      NA      NA      NA      NA     NA     NA
    ## 2022      120    1   46      NA      NA      NA      NA     NA     NA
    ## 2023      120    1   47      NA      NA      NA      NA     NA     NA
    ## 2024      120    1   48      NA      NA      NA      NA     NA     NA
    ## 2025      120    1   49      NA      NA      NA      NA     NA     NA
    ## 2026      120    1   50      NA      NA      NA      NA     NA     NA
    ## 2027      120    1   51      NA      NA      NA      NA     NA     NA
    ## 2028      120    1   52      NA      NA      NA      NA     NA     NA
    ## 2029      120    2    1      NA      NA      NA      NA     NA     NA
    ## 2030      120    2    2      NA      NA      NA      NA     NA     NA
    ## 2031      120    2    3      NA      NA      NA      NA     NA     NA
    ## 2032      120    2    4      NA      NA      NA      NA     NA     NA
    ## 2033      120    2    5      NA      NA      NA      NA     NA     NA
    ## 2034      120    2    6      NA      NA      NA      NA     NA     NA
    ## 2035      120    2    7      NA      NA      NA      NA     NA     NA
    ## 2036      120    2    8      NA      NA      NA      NA     NA     NA
    ## 2037      120    2    9      NA      NA      NA      NA     NA     NA
    ## 2038      120    2   10      NA      NA      NA      NA     NA     NA
    ## 2039      120    2   11      NA      NA      NA      NA     NA     NA
    ## 2040      120    2   12      NA      NA      NA      NA     NA     NA
    ## 2041      120    2   13      NA      NA      NA      NA     NA     NA
    ## 2042      120    2   14      NA      NA      NA      NA     NA     NA
    ## 2043      120    2   15      NA      NA      NA      NA     NA     NA
    ## 2044      120    2   16      NA      NA      NA      NA     NA     NA
    ## 2045      120    2   17      NA      NA      NA      NA     NA     NA
    ## 2046      120    2   18      NA      NA      NA      NA     NA     NA
    ## 2047      120    2   19      NA      NA      NA      NA     NA     NA
    ## 2048      120    2   20      NA      NA      NA      NA     NA     NA
    ## 2049      120    2   21      NA      NA      NA      NA     NA     NA
    ## 2050      120    2   22      NA      NA      NA      NA     NA     NA
    ## 2051      120    2   23      NA      NA      NA      NA     NA     NA
    ## 2052      120    2   24      NA      NA      NA      NA     NA     NA
    ## 2053      120    2   25      NA      NA      NA      NA     NA     NA
    ## 2054      120    2   26      NA      NA      NA      NA     NA     NA
    ## 2055      120    2   27      NA      NA      NA      NA     NA     NA
    ## 2056      120    2   28      NA      NA      NA      NA     NA     NA
    ## 2057      120    2   29      NA      NA      NA      NA     NA     NA
    ## 2058      120    2   30      NA      NA      NA      NA     NA     NA
    ## 2059      120    2   31      NA      NA      NA      NA     NA     NA
    ## 2060      120    2   32      NA      NA      NA      NA     NA     NA
    ## 2061      120    2   33      NA      NA      NA      NA     NA     NA
    ## 2062      120    2   34      NA      NA      NA      NA     NA     NA
    ## 2063      120    2   35      NA      NA      NA      NA     NA     NA
    ## 2064      120    2   36      NA      NA      NA      NA     NA     NA
    ## 2065      120    2   37      NA      NA      NA      NA     NA     NA
    ## 2066      120    2   38      NA      NA      NA      NA     NA     NA
    ## 2067      120    2   39      NA      NA      NA      NA     NA     NA
    ## 2068      120    2   40      NA      NA      NA      NA     NA     NA
    ## 2069      120    2   41      NA      NA      NA      NA     NA     NA
    ## 2070      120    2   42      NA      NA      NA      NA     NA     NA
    ## 2071      120    2   43      NA      NA      NA      NA     NA     NA
    ## 2072      120    2   44      NA      NA      NA      NA     NA     NA
    ## 2073      120    2   45      NA      NA      NA      NA     NA     NA
    ## 2074      120    2   46      NA      NA      NA      NA     NA     NA
    ## 2075      120    2   47      NA      NA      NA      NA     NA     NA
    ## 2076      120    2   48      NA      NA      NA      NA     NA     NA
    ## 2077      120    2   49      NA      NA      NA      NA     NA     NA
    ## 2078      120    2   50      NA      NA      NA      NA     NA     NA
    ## 2079      120    2   51      NA      NA      NA      NA     NA     NA
    ## 2080      120    2   52      NA      NA      NA      NA     NA     NA
    ##      country
    ## 1961      CN
    ## 1962      CN
    ## 1963      CN
    ## 1964      CN
    ## 1965      CN
    ## 1966      CN
    ## 1967      CN
    ## 1968      CN
    ## 1969      CN
    ## 1970      CN
    ## 1971      CN
    ## 1972      CN
    ## 1973      CN
    ## 1974      CN
    ## 1975      CN
    ## 1976      CN
    ## 1977      CN
    ## 1978      CN
    ## 1979      CN
    ## 1980      CN
    ## 1981      CN
    ## 1982      CN
    ## 1983      CN
    ## 1984      CN
    ## 1985      CN
    ## 1986      CN
    ## 1987      CN
    ## 1988      CN
    ## 1989      CN
    ## 1990      CN
    ## 1991      CN
    ## 1992      CN
    ## 1993      CN
    ## 1994      CN
    ## 1995      CN
    ## 1996      CN
    ## 1997      CN
    ## 1998      CN
    ## 1999      CN
    ## 2000      CN
    ## 2001      CN
    ## 2002      CN
    ## 2003      CN
    ## 2004      CN
    ## 2005      CN
    ## 2006      CN
    ## 2007      CN
    ## 2008      CN
    ## 2009      CN
    ## 2010      CN
    ## 2011      CN
    ## 2012      CN
    ## 2013      CN
    ## 2014      CN
    ## 2015      CN
    ## 2016      CN
    ## 2017      CN
    ## 2018      CN
    ## 2019      CN
    ## 2020      CN
    ## 2021      CN
    ## 2022      CN
    ## 2023      CN
    ## 2024      CN
    ## 2025      CN
    ## 2026      CN
    ## 2027      CN
    ## 2028      CN
    ## 2029      CN
    ## 2030      CN
    ## 2031      CN
    ## 2032      CN
    ## 2033      CN
    ## 2034      CN
    ## 2035      CN
    ## 2036      CN
    ## 2037      CN
    ## 2038      CN
    ## 2039      CN
    ## 2040      CN
    ## 2041      CN
    ## 2042      CN
    ## 2043      CN
    ## 2044      CN
    ## 2045      CN
    ## 2046      CN
    ## 2047      CN
    ## 2048      CN
    ## 2049      CN
    ## 2050      CN
    ## 2051      CN
    ## 2052      CN
    ## 2053      CN
    ## 2054      CN
    ## 2055      CN
    ## 2056      CN
    ## 2057      CN
    ## 2058      CN
    ## 2059      CN
    ## 2060      CN
    ## 2061      CN
    ## 2062      CN
    ## 2063      CN
    ## 2064      CN
    ## 2065      CN
    ## 2066      CN
    ## 2067      CN
    ## 2068      CN
    ## 2069      CN
    ## 2070      CN
    ## 2071      CN
    ## 2072      CN
    ## 2073      CN
    ## 2074      CN
    ## 2075      CN
    ## 2076      CN
    ## 2077      CN
    ## 2078      CN
    ## 2079      CN
    ## 2080      CN

*Obs*: The data seemed to have been inputted correctly

We can now move on to filling in the rest of the data points, namely the specific measures like sales, price, promotion (Y/N)

### 3.1.2 Simulating Measurement Data Points

We'll complete store.df with random data for *store-by\_week* observations of the sales, price, and promotional status of 2 products.

##### On Randomizing Data

It's best practice to set the random number generation **seed** to make the data replicable. When setting a seed, we draw random samples in the same sequence again and get **pseudo-random** numbers via \*\*Pseudorandom number generators (PRNGs) using `set.seed()`.

`p1prom, p2prom`: Per observation (or week), we will set the status of whether each product was promoted (1 = Yes, 0 = No), by drawing randomly from a binomial distirbution that counts the number of "heads" in a collection of coin tosses where the coin can have a specified proportion of heads). To do this, we use `rbinom(n, size, p)` for random binomial function. For every row, we draw from this distribution with specified number of heads in a single toss `n=nrow(store.df), size=1`.

-   *Assume p1 has a `p=0.1` probability and p2 has a `p=0.2` probability of being promoted *

``` r
store.df$p1prom <- rbinom(n=nrow(store.df), size=1, p=0.10) # product 1 is promoted 10% of time
store.df$p2prom <- rbinom(n=nrow(store.df), size=1, p=0.15) # product 2 is promoted 15% of time
```

`p1price, p2price` : Assume each product is sold at 1:5 distinct price points ranging from $2.19 to $3.19 overall. We will randomly draw a price for each week by defining a vector with the price points and using `sample(x, size, replace)` to draw from it as many times as we have rows of data `size=nrow(store.df)`. We want to sample with replacement so random prices is reflected in the data with `replace=TRUE`.

``` r
store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99), size=nrow(store.df), replace=TRUE)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), size=nrow(store.df), replace=TRUE) # slightly more expensive that product 1

# check progress
head(store.df)
```

    ##   storeNum Year Week p1sales p2sales p1price p2price p1prom p2prom country
    ## 1      101    1    1      NA      NA    2.19    2.29      0      0      US
    ## 2      101    1    2      NA      NA    2.49    2.49      0      1      US
    ## 3      101    1    3      NA      NA    2.19    2.59      0      0      US
    ## 4      101    1    4      NA      NA    2.99    2.29      0      0      US
    ## 5      101    1    5      NA      NA    2.99    2.29      0      0      US
    ## 6      101    1    6      NA      NA    2.19    2.99      0      1      US

`p1sales, p2sales` (in Units): We can calculate sales as a relative function between *price* and *promotional status* of each. Since item sales are in unit counts, we use the **Poisson Distribution** to generate count data `rpois(n, lambda)` where `n=` \# of draws and `lambda=` mean value of units per week. For each row `(nrow=store.df)` we draw from this random poisson count. Assume product 1 mean sales (lambda=120) is higher than product 2 (lambda=100).

-   Price effects - often follow a logarithmic relationship vs. linear so we should scale these counts up/down according to the relative prices using `log(price)`. For price effects, we assume that sales vary inversely with prices between p1 and p2. The customer will select p1 if it's cheaper than p2. E.g. sales of product 1 go up when `log(price)` of product 1 is lower than \`log(price) of product 2.

-   Promo effects - Assume sales get a 30% or 40% lift when each product is promoted in store. Simply multiply promotional status x 0.3 or 0.4 respectively, then multiple sales vector by that.

Use `floor()` function to drop fractional values and ensure integer counts for weekly unit sales.

``` r
# first, create default sales without promotion
tmp.sales1 <- rpois(nrow(store.df),lambda=120) # p1 mean sales is slightly higher than p2
tmp.sales2 <- rpois(nrow(store.df),lambda=100)

#second, scale counts up/down based on the RATIO OF LOG(PRICE)
tmp.sales1 <- tmp.sales1 * log(store.df$p2price) / log(store.df$p1price) # when p1 is cheaper, sales go up as part of the denominator
tmp.sales2 <- tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

# third, p1 sales get a 30% lift when promoted and p2 sales get a 40% lift when promoted
store.df$p1sales = floor(tmp.sales1 * 1 + store.df$p1prom * 0.3)
store.df$p2sales = floor(tmp.sales2 * 1 + store.df$p2prom * 0.4)

# inspect data frame and check for errors
head(store.df)
```

    ##   storeNum Year Week p1sales p2sales p1price p2price p1prom p2prom country
    ## 1      101    1    1     137      88    2.19    2.29      0      0      US
    ## 2      101    1    2     127     104    2.49    2.49      0      1      US
    ## 3      101    1    3     134      74    2.19    2.59      0      0      US
    ## 4      101    1    4      90     138    2.99    2.29      0      0      US
    ## 5      101    1    5      80     141    2.99    2.29      0      0      US
    ## 6      101    1    6     153      69    2.19    2.99      0      1      US

``` r
# use some() to further inspect from random sampling
library("car")
some(store.df)
```

    ##      storeNum Year Week p1sales p2sales p1price p2price p1prom p2prom
    ## 86        101    2   34     152      86    2.19    2.49      0      1
    ## 157       102    2    1     160      83    2.19    2.99      0      0
    ## 499       105    2   31     140      85    2.49    2.99      0      0
    ## 541       106    1   21     173      69    2.29    3.19      0      0
    ## 735       108    1    7     122     106    2.49    2.49      1      0
    ## 858       109    1   26     116      78    2.19    2.59      0      0
    ## 1088      111    1   48     147      60    2.29    3.19      0      0
    ## 1090      111    1   50     134      95    2.79    2.99      0      1
    ## 1994      120    1   18      94     145    2.99    2.29      1      1
    ## 2019      120    1   43     126      70    2.19    2.59      0      0
    ##      country
    ## 86        US
    ## 157       US
    ## 499       DE
    ## 541       DE
    ## 735       DE
    ## 858       GB
    ## 1088      GB
    ## 1090      GB
    ## 1994      CN
    ## 2019      CN

*Obs*: sales seem to have been calculated correctly based on price ratios and promotional lifts.

### 3.2 Functions to Summarize a Variable

Obs may comprise of discrete data that occurs at specific levels or continuous data with many possible values within an interval.
