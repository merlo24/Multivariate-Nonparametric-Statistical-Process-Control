Implementation of SREWMA Control Chart
================
Jorge Merlo
6/5/2021

# Synopsis

In any industry, quality of a process is determined by their capacity to
generate products/services that met the requirements established by the
consumers. To meet these specifications multivariate statistical process
control (MSPC), through control charts, evaluates a multivariate process
quality by monitoring its underlying distribution in real-time, the
purpose of this is to detect as soon as possible any potential mean or
scale shift attributed to special causes.

Although the traditional Hotelling’s *T*<sup>2</sup> is the most common
control chart in MSPC, it is built under the assumption that process
follows a multivariate normal distribution. Nevertheless, it is well
known that in practice this assumption is rarely fulfilled because of
the process often following an unknown distribution.

Therefore, multivariate nonparametric approaches such as the Signed Rank
Exponentially Weighted Average (SREWMA) control chart **(cita)** can be
considered as an efficient alternative that allows the monitoring of
processes for which no known distribution is assumed. In this document
we discuss the implementation of SREWMA Control Chart to an important
process monitoring problem in a semiconductor manufacturing industry.
The dataset is available from the UC Irvine Machine Learning Repository
(<http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.names>).

# Data Cleaning

In order to make a simple reproduction of the following analysis,
instead of working with data that comes from a local .csv file we
directly download the data from the url and load it into the
environment:

``` r
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data", destfile = "secom.data") # explanatory variables

download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data", destfile  = "secom_labels.data") # output

secom <- read.table("secom.data")
secom_lab <- read.table("secom_labels.data")[,1]
secom <- cbind(secom,secom_lab) # concatenation of variables
```

There are originally 1567 observations and 591 variables, but the
dataset contains many missing values. Therefore, we process and clean
the data before using it in our illustration. We remove the variables
having 5 or more missing values:

``` r
col.na <- sapply(secom, function(x) sum(is.na(x))) # counting the number of NA's in each variable
# sum(col.na >= 5) # detecting how many columns contain more than 5 missing values
secom <- secom[, which(col.na < 5)] # filtering of data based on the previous condition
```

There are 278 remaining variables. Now, this data set has some missing
observations, as variables with less than 5 missing observations are
retained. Observations with missing values are ommited and finally, we
remove the variables with constant value.

``` r
row.na <- rowSums(is.na(secom)) # detecting observations with missing values
secom <- secom[which(row.na==0), ] # filtering data based on previous condition
secom.fdf <- secom[ ,apply(secom, 2, var) != 0] # final data frame to be analyzed (variables with constant value are removed)
```

After data cleaning, there are 1549 observations and 248 variables. It
is known from the data source that out of the 1544 observations
available after cleaning, in 1447 cases the item passes the quality test
whereas it fails in remaining 102 cases. Therefore, we consider the 1447
observations as our reference sample.

# Exploratory Analysis

It is desirable to set up an on-line detection system to monitor the
production process of the secom manufacturing process to guarantee its
quality. The sample correlation matrix of this data contains several
large entries, which demonstrates that the variables have considerable
interrelationships and consequently a multivariate control chart is
likely to be more appropriate than a univariate control chart. The plot
below illustrates the pairwise variables that have a correlation greater
of 0.999.

``` r
source("corr_simple.R")
corr_simple(secom.fdf, sig = 0.999)
```

    ## Warning: package 'corrplot' was built under R version 4.0.5

    ## corrplot 0.84 loaded

    ##       Var1 Var2       Freq
    ## 6421   V35  V37 -1.0000000
    ## 20994 V173 V175  0.9999998
    ## 34332 V308 V310  0.9999994
    ## 29587 V153 V288  0.9999971
    ## 40455 V253 V391  0.9999915
    ## 40208 V252 V390  0.9999402
    ## 39714 V250 V388  0.9999384
    ## 59279 V584 V586  0.9998902
    ## 48058 V177 V448  0.9998867
    ## 53546 V390 V524  0.9998568
    ## 53492 V252 V524  0.9998373
    ## 52751 V249 V521  0.9997336
    ## 28599 V148 V283  0.9995987
    ## 48305 V178 V449  0.9995360
    ## 50034 V188 V460  0.9995303
    ## 28846 V149 V284  0.9995253
    ## 47811 V176 V447  0.9994788
    ## 41883 V148 V421  0.9993374
    ## 50528 V196 V468  0.9992742
    ## 53986 V255 V527  0.9992474
    ## 48799 V182 V454  0.9991312

![](srewma_implementation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
