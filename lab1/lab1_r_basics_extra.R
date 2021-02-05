# Title:    Quant III (Lab 1)
# Name:     Junlong Aaron Zhou
# Date:     September 11, 2020
# Summary:  Data structures; memory and vectorization; conditions and loops; 
#           functions; data manipulation (data.table); simulation
#
# Thank to Denis Stukal 
#################################################################################

rm(list = ls())

# install.packages('stm')
# install.packages('data.table')

library(stm)
library(data.table)

setwd('~/Dropbox/Teaching/2020_Fall_Quant_3')

##################################### Functions #####################################
#####################################################################################
# This section borrows from Hadley Wickham's "Advanced R" ("Fuctions" chapter): 
# http://adv-r.had.co.nz/Functions.html


### Every operation is a function call in R!



### 3 components: formals(), body(), environment()
square <- function(x) { x*x }

square
formals(square)
body(square)
environment(square)

# look inside a package function
manyTopics
stm::manyTopics 


### Primitive functions call C code directly
body(sum)


### We said everything is a function call...
x <- 2
y <- 5

# sum
x + y
'+'(x,y)

?'+'

# multiply
x * y
'*'(x,y)

# for-loop
for (i in 1:5) { 
  print(i)
}

'for'(i, 1:5, print(i))



### Name masking
x <- 1 

f1 <- function() {
  y <- 2
  x <- 3
  c(x, y)
}

f2 <- function() {
  y <- 2
  c(x, y)
}

f1()
f2()

x <- 1:5

f3 <- function() {
  y <- 2
  x[1] <- 3
  c(x, y)
}

f3()
x


# Take-away: avoid using same name unless you like confusion.



### Supplying arguments to functions (4 ways)
my.weird.function <- function(x_arg = 1, y_arg = 2, z_arg = 3) {
  return(c(x_arg, y_arg, z_arg))
}

# 1) by position
my.weird.function(4,5,6)

# 2) by name
my.weird.function(z_arg = 6, x_arg = 4, y_arg = 5)

# 3) by partial name
my.weird.function(z = 6, x = 4, y = 5)

# 4) with a list of arguments
my.args <- list(z_arg = 6, x_arg = 4, y_arg = 5)
do.call(my.weird.function, my.args) # Useful when working with lists! See below

my.args <- list(c(1:100,NA), na.rm=T)
do.call(mean, my.args)



### Passing an arbitrary number of arguments
my.concatenator <- function(...) {
  # Input: an arbitrary number of arbitrary arguments
  # Output: a concatenated vector of passed arguments
  inputs <- list(...) # this is the trick to work with ...
  return( unlist(inputs) ) 
}

my.concatenator(letters[1:5], letters[6:10], letters[11:15])
my.concatenator(letters[1:5], letters[6:10], letters[11:15], letters[16:20])



##################################### Data structures #####################################
###########################################################################################

# 3 of them: vectors, arrays, and data frames


### A) Vectors: atomic or lists
(atom.num <- 1:10)
str(atom.num)

(atom.char <- letters[1:5])
str(atom.char)

atom.mix <- c(atom.num, atom.char)
str(atom.mix)
# Takeaway: atomic vectors allow only for elements of the same type; atomatic type coercion 
# to the more flexible type
# Reminder: logical --> integer --> double --> character



#----------
#### Lists 
l1 <- list(colors = c('red', 'blue', 'pink'), numbers = 1:10)
str(l1)

# 3 ways to reference list contents
l1$colors
l1[['colors']]
l1[[1]]

# NB! [[]] vs []
str(l1[[1]])
str(l1[1])

l2 <- list(colors = c('red', 'blue', 'pink'), numbers = 1:10, mixed = c(1, 'a'))
str(l2)

l3 <- list(1, 'a', l1)
str(l3)
l3[[3]][[1]]
l3[[3]][['colors']]
l3[[3]]$colors


str(unlist(l3))
# Takeaway: lists allow for elements of different types; can be nested



#----------
### B) Arrays (e.g. matrices)
(X <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2, byrow = T))
str(X)
dim(X)
ncol(X)
nrow(X)

length(X)
X[3]

# transpose a matrix
t(X)
X
t(X)[3]

# X'X
(XX <- t(X) %*% X)

# (X'X)^{-1}
solve( t(X) %*% X )
solve(XX)


round(solve( t(X) %*% X ) %*% XX, 5)
solve( t(X) %*% X ) %*% XX

# other useful stuff: determinant, eigen decomposition, Cholesky decomposition
det(XX)
eig <- eigen(XX)
chol(XX)

str(eig)

eig$vectors %*% diag(eig$values) %*% solve(eig$vectors) 
eig$vectors %*% diag(eig$values) %*% solve(eig$vectors) == XX

prod(eig$values) == det(XX)
 
# Takeaway: floating point arithmetic can be tricky!
# Another famous example: round(.1,1) == round((.3 / 3),1)
# Use round():
round(prod(eig$values)) == round(det(XX))
round(prod(eig$values), 13) == round(det(XX), 13)

XXsqrt <- chol(XX)
t(XXsqrt) %*% XXsqrt == XX
round(t(XXsqrt) %*% XXsqrt,5) == XX


#---------------------
### Digression: OLS

# Function to compute OLS coefficients
ols <- function(X, y) {
  # Input: vector or matrix X, vector y
  # Returns: coefficient vector for OLS regression (X'X)^{-1}X'y
  if ( !all(as.matrix(X)[,1] == 1) ) {X <- cbind(1, X)}
  out <- solve(t(X) %*% X) %*% t(X) %*% y
  rownames(out) <- c('Intercept', rep('', nrow(out)-1 ))
  return( t(out) )
}


# Simulate some data
set.seed(12345)
nobs = 15
X <- rnorm(nobs)
y <- 3 + 2 * X + rnorm(nobs)

# Compare outputs: lm() vs ols()
lm(y ~ X)
round(ols(X = X, y = y), 3)


#------------
### Back to arrays (e.g. matrices)

# Need not be matrices
(tensor <- array(1:12, dim = c(2,3,2)))
str(tensor)
dim(tensor)


(X <- matrix(c(1:12), ncol = 3, nrow = 4))
dim(X) <- c(2,3,2)
X

# Takeaway: matrices and tensors are represented in R as arrays. Matrix multiplication in easy. 
#           Tensor arithmetic is not. Can use package 'tensorA' if needed



#-----------

rm(list = ls())

### C) Data frames: the main data structure for data analysis
dat <- data.frame(country = c('US', 'UK', 'Germany'), population = c(323.1, 65.6, 82.7), 
                  stringsAsFactors = F)
str(dat)

# Data frames combine features of lists and matrices
(df <- data.frame(x = 1:3, y = 1:4)) # matrix --> must be rectangular
df <- data.frame(x = 1:3)
(df$y <- list(1:6)) # list --> can be nested
str(df)

(df <- data.frame(x = 1:3, y = I(list(1:4, 1:5, 1:6))))
str(df)

(df <- data.frame(x = 1:3, y = I(rep(list(dat),3))))
str(df)
# Example: scraping social media profiles

# Combine data frames
(d1 <- data.frame(country = c('US', 'UK', 'Germany'), population = c(323.1, 65.6, 82.7)))
(d2 <- data.frame(country = c('China', 'India', 'Indonesia'), population = c(1379, 1324, 263.5)))

rbind(d1, d2) # NB! colnames must be identical!

(d3 <- data.frame(capital = c('Washington D.C.', 'London', 'Berlin')))
cbind(d1, d3) # NB! nrow must be identical!

# From lists to data frames
(l1 <- list(country = c('US', 'UK', 'Germany'), 
            capital = c('Washington D.C.', 'London', 'Berlin'),
            population = c(323.1, 65.6, 82.7)))

(df <- data.frame(country = l1$country, capital = l1$capital, population = l1$population, stringsAsFactors = F))
(df <- do.call('data.frame', l1))
str(df)


(df <- do.call('cbind', l1))
str(df)

# Takeaways:  Data frames are useful in data analysis. They combine characteristics of lists and matrices.
#             Keep in mind stringsAsFactors = F



##################################### Memory and vectorization #####################################
####################################################################################################

rm(list = ls())

#------------
### Memory 

# Growing objects are bad
n = 100000
system.time( { vec <- numeric(0); for (i in 1:n) {vec[i] <- i} } )
system.time( vec <- 1:n)

# For time, similar to:
t1 <- proc.time()
vec <- 1:n
proc.time()-t1

# Garbage collection
rm(list = ls()) # remove variables
gc() #collect unused memory



#-----------------
### Vectorization

# A function is vectorized if it applies to a vector of values as it would to every single value
# E.g.: sum of logged values
x <- 1:1e7 # 10 mln

system.time( {
  logsum <- 0
  for (i in 1:length(x)) 
    { logsum <- logsum + log(x[i]) } 
  } )

system.time(logsum <- sum(log(x)))

# Why vectorization? 
# R is an interpreted language, with many basic functions written in C (or C++, or FORTRAN). 
# R needs to read the data type before passing data to the compiled code (in C). 
# If you apply a function element-wise, this data type reading happens as many times as there are elements. 
# If you pass a vector, it happens only once. 



##################################### Conditions and loops #####################################
################################################################################################

rm(list = ls())

(x <- c(rep(1,3), rep(0,5)))

### if and ifelse
# if accepts only single-element conditions
if (x < 1) {
  print('zero')
} else {
  print('one')
}


# An UGLY way around using a for-loop
for (i in 1:length(x)) {
  if (x[i] < 1) {
    print('zero')
  } else {
    print('one')
  }
}


# A FINE way around using ifelse that works with vectors
ifelse(x < 1, 'zero', 'one')
# NB! Here, it also returns an object (vector)



#--------
### apply-family functions (apply, lapply, sapply)
# Are these vectorizations? Tricky...
apply
lapply


# Suppose, want cumulative sums
x <- 1:1e4

# BEST: directly vectorized
system.time( cumsum(x) )

# BAD: growing object with a for-loop
system.time( {
vec <- x[1]
for (i in 2:length(x)) {
  vec[i] <- vec[i-1] + x[i]
}
})

# WORST: sapply or lapply
system.time( {vec <- sapply(1:length(x), function(k) sum(x[1:k])) } )
system.time( { l <- lapply(1:length(x), function(k) sum(x[1:k])) } )

# Any Idea Why?


### apply is useful with matrices
rm(list = ls())

(X <- matrix(1:6, nrow = 3, ncol = 2))

# Count even numbers in every column
apply(X, 2, function(k) length(k[k %% 2 == 0]) )

# Less useful
apply(X, 2, mean)
apply(X, 1, mean)
rowMeans(X)


### lapply and sapply work with lists and vectors
(l <- list(A = matrix(1:6, nrow = 3, ncol = 2),
          B = matrix(7:12, nrow = 3, ncol = 2),
          C = matrix(13:18, nrow = 3, ncol = 2)))

# extract Colmeans from each matrix
lapply(l, colMeans ) 

# extract Colmeans from each matrix
sapply(l, colMeans )

# get determinant of X'X, where X is the matrix stored in an element of l
sapply(l, function(k) det( t(k) %*% k ) )
lapply(l, function(k) det( t(k) %*% k ) )
# Takeaway: apply-family functions look good, but not necessarily the best way to go.


##################################### Data manipulation #####################################
#############################################################################################

rm(list = ls() )
# This part of the code borrows heavily from https://cran.r-project.org/web/packages/data.table/vignettes/

# install.packages('data.table')
# install.packages('dplyr')
library(data.table)
library(dplyr)
# There are different ways to manipulate data. A popular one is to use the dplyr package.
# However, dplyr is REALLY slow with large datasets. 
# data.table is a more modern package that is fantastic for small and large datasets alike. 



#----------------
###  Read data in
system.time( dat <- read.table(file = 'lab1_r_basics/flights14.csv', header = T, sep = ',', stringsAsFactors = F) )  
DT <- data.table(dat)
str(DT)

# Alternative "data.table" way
system.time( DT <- fread(input = 'lab1_r_basics/flights14.csv') )
str(DT)


#----------------
### Select rows
table(DT$carrier) 
DT[carrier == 'AA']

DT %>% filter(carrier == 'AA') 



#----------------
### Select columns
DT[, .(carrier)] 
# The dot-notation is an alias for calling lists. 
# The same command could've been written as DT[, list(carrier)]
# If you call lists or use the dot-notation, you're telling data.table to return a data.table
# Otherwise returns a vector: 
str(DT[, carrier])
str(DT[, list(carrier)])
str(DT[, .(carrier)])

DT %>% select(carrier)
str(DT %>% select(carrier))
str(dat %>% select(carrier))

# dplyr almost always return the original data type

#----------------
### Select both rows and columns

DT[carrier == 'AA', .(carrier, dest)]

DT %>% filter(carrier == 'AA') %>%
  select(carrier,dest) 

# rename a column
DT[carrier == 'AA', .(carrier, destination = dest)] 

DT %>% filter(carrier == 'AA') %>%
  select(carrier,dest) %>% rename(destination=dest)

# select multiple a column

DT[carrier == 'AA', carrier:dest] 
DT[carrier == 'AA', 9:13]

DT %>% filter(carrier == 'AA') %>%
  select(carrier:dest) 

cols_to_select <- c('carrier', 'dest')
DT[carrier == 'AA', cols_to_select] # not working: no "cols_to_select" in names(DT)
DT[carrier == 'AA', cols_to_select, with = F] # with = FALSE restores the data.framish way of selection

DT %>% filter(carrier == 'AA') %>%  select(cols_to_select) 


DT[carrier == 'AA', -c("carrier", "dest")]
DT[carrier == 'AA', -cols_to_select, with = F]

DT %>% filter(carrier == 'AA') %>%  select(-cols_to_select) 
DT %>% filter(carrier == 'AA') %>%  select(-c("carrier", "dest")) 


#----------------
### Computations on columns
# Compute mean arrival and departure delays for American Airlines
DT[carrier == 'AA', .( mean(arr_delay), mean(dep_delay) ) ]
DT[carrier == 'AA', .( av.arr.delay = mean(arr_delay), av.dep.delay = mean(dep_delay) ) ]

DT %>% filter(carrier == 'AA') %>% 
  mutate( av.arr.delay = mean(arr_delay), av.dep.delay = mean(dep_delay) ) 

DT %>% filter(carrier == 'AA') %>% 
  summarise( av.arr.delay = mean(arr_delay), av.dep.delay = mean(dep_delay) ) 

# Add sd and number of obs
DT[carrier == 'AA', .( av.arr.delay = mean(arr_delay), 
                       av.dep.delay = mean(dep_delay),
                       sd.arr.delay = sd(arr_delay),
                       sd.dep.delay = sd(dep_delay),
                       N = .N) ]

DT %>% filter(carrier == 'AA') %>% 
  summarise( av.arr.delay = mean(arr_delay), 
             av.dep.delay = mean(dep_delay),
             sd.arr.delay = sd(arr_delay),
             sd.dep.delay = sd(dep_delay),
             N =  n()) 
 

#----------------
### Computations on groups of obs
DT[, .( av.arr.delay = mean(arr_delay), N = .N), by = .(carrier) ]
DT[, .( av.arr.delay = mean(arr_delay), N = .N), by = 'carrier' ]

DT %>% group_by(carrier) %>% 
  summarise( av.arr.delay = mean(arr_delay), 
             av.dep.delay = mean(dep_delay),
             sd.arr.delay = sd(arr_delay),
             sd.dep.delay = sd(dep_delay),
             N =  n()) 

DT %>% group_by(carrier) %>% 
  summarise( av.arr.delay = mean(arr_delay), 
             av.dep.delay = mean(dep_delay),
             sd.arr.delay = sd(arr_delay),
             sd.dep.delay = sd(dep_delay),
             N =  n())  

DT %>% group_by(carrier) %>% 
  mutate( av.arr.delay = mean(arr_delay), 
             av.dep.delay = mean(dep_delay),
             sd.arr.delay = sd(arr_delay),
             sd.dep.delay = sd(dep_delay),
             N =  n())  


# Add row selection
DT[origin == 'JFK', .(origin, av.arr.delay = mean(arr_delay), N = .N), by = .(carrier) ]



#---------------- 

# You can call a lot function via dplyr

DT %>% group_by(carrier) %>% 
  summarise( av.arr.delay = mean(arr_delay), 
             av.dep.delay = mean(dep_delay),
             sd.arr.delay = sd(arr_delay),
             sd.dep.delay = sd(dep_delay),
             N =  n())  %>%
  str()


#----------------
### Reshaping: wide to long, long to wide

rm(list = ls())

bot_tweets <- get(load('lab1_r_basics/bot_tweets.RData') )
str(bot_tweets)
# tw.num.kr number of tweets pro-Kremlin bots (.kr) posted on a given day/ 
# .ne stands for neutral bots; .op stands for pro-opposition bots

### melt: wide to long
d <- melt(bot_tweets, 
          id.vars = 'date', 
          measure.vars = c('tw.num.kr', 'tw.num.ne', 'tw.num.op'), 
          variable.name = 'orientation',
          value.name = 'num.tw')

setorder(d, date) # order by date
d



### dcast: long to wide
dcast(d, date ~ orientation, value.var = 'num.tw')


#----------------
### Not enough for you? Please, read https://cran.r-project.org/web/packages/data.table/vignettes/



##################################### Simulations #####################################
#######################################################################################

rm(list = ls())

# What's set.seed() ? How does it relate to randomness?

# NB! Always set the random seed when doing simulations!!! Otherwise, you'll never replicate them!

### Basic simulations: replicate()
set.seed(20200911)
n.reps <- 100
n.sample <- 1e3

replicate(n.reps, mean( rnorm(n.sample) )  ) # 10 means of samples from N(0,1)

hist( replicate(n.reps, mean( rnorm(n.sample) )  ), main = 'Distribution of normal means', xlab = 'Mean value' )
 
### Basic Monte Carlo
# Integrate log(x) from 0 to 1

# 1) Generate uniform values (0-1), 2) compute the integrand, 3) take the average
N = 1e5
u <- runif(N)
n <- 1:N
h_sim <- log(u)
s <- cumsum(h_sim)/n
plot(s, type = 'l', main = 'ln(x) 0-1')

sum(h_sim) / N # answer



# Integrate log(x) from 1 to 5
N = 1e5
u <- 4*runif(N) + 1
4 * sum( log(u) ) / N  # Where does this 4 come from?



# Simulate the N(0,1) cdf at t: F(t) = Pr(X <= t)
rm(list = ls())

t = 1.6
set.seed(20200911)

# Option 1: simulate an N(0,1) sample and compute proportions
n.sim = 1e6
x <- rnorm(n.sim)
sum(x <= t) / n.sim

# Option 2: what if we don't have the rnorm() function? 
# Imagine it's not N(0,1), but some other crazy function. How would you proceed?
# Then:
# Step 1: generate a uniform sample
# Step 2: get from the uniform sample to the normal sample (Box-Mueller)
# Step 3: average the required values
t = 1.6
N <- 1e7
u1 <- runif(N/2) # generate uniform values
u2 <- runif(N/2) # generate uniform values
x1 <- sqrt(-2*log(u1)) * cos(2*pi*u2) # apply Box-Mueller transform to get N(0,1)
x2 <- sqrt(-2*log(u1)) * sin(2*pi*u2) # apply Box-Mueller transform to get N(0,1)
x <- c(x1,x2)  # Here, we produced a normal sample based on the original uniform sample


sum(x <= t)/N # Monte Carlo step: average

# Compare to the exact result
sum(x <= t)/N  -  pnorm(q = t)


# Option 3: Rejection Sampling 
# In the case you know density f, but cannot sample/=.
# You will learn about this later
# Step 1: sample 1 value, x, from a known distribution g(say uniform (-100,100))
# Step 2: Keep this value with probability = f(x)/g(x)*Constant<1. 
# Why?

hist(rnorm(100000), freq=F, ylim=c(0,1))
abline(h=0.4)

# How: generate u ~ unif(0,1), keep it if u<f(x)/g(x)*Constant.

t = 1.6
N <- 1e7

x1 <- runif(n=N, min = -10,max=10)
x2 <- runif(n=N,min=0,max=1)
x2 <- ifelse(x2<dnorm(x1)/2,x1,NA)

x <- x2[!is.na(x2)]
hist(x)
sum(x <= t)/length(x)  -  pnorm(q = t)

# Let's see we can also sample N(0,1) From N(10,10)

plot(density(rnorm(n=N, mean=10, sd=5)), ylim=c(0,1))
lines(density(rnorm(N)))

x1 <- rnorm(n=N, mean=10, sd=5)
x2 <- runif(n=N,min=0,max=1)
x2 <- ifelse(x2<dnorm(x1)/dnorm(x1,mean=10,sd=5)/100,x1,NA)

x <- x2[!is.na(x2)]
hist(x)
sum(x <= t)/length(x)  -  pnorm(q = t)

# We need to sample a lot because we need to reject a lot.


### Takeaway: Monte Carlo is a general simulation-based approach to computing integrals, 
# performing optimization and sampling from distributions



