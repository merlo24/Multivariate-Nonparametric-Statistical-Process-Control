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
sum(col.na >= 5) # detecting which columns contain more than 5 missing values
```

    ## [1] 314

``` r
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
