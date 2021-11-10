
# Distributions functions in R

# d : given a number , it gives its probability

# p : cdf , given a number it gives prob of rv being smaller than that

# q : compliment to cdf , given a prob returns the number 

# r : generate random number from the distribution

n=100
p=0.1
# d : given a number , it gives its probability
dbinom(4,n,p)

# p : cdf , given a number it gives prob of rv being smaller than that

pbinom(10,n,p)

# q : compliment to cdf , given a prob returns the number 

qbinom(0.58,n,p)

# r : generate random number from the distribution
rbinom(4,n,p)


x=rbinom(10000,1000,0.5)
x
df=data.frame(x=x)

library(ggplot2)
ggplot(df,aes(x=x))+geom_histogram(bins=100)
ggplot(df,aes(x=x))+geom_density()

# whats the probability that 200 of the patients will be cured
dbinom(200,1000,0.2)

# whats the probability that upto 200 patients will be cured
pbinom(210,1000,0.2)

pbinom(210,1000,0.2)-pbinom(190,1000,0.2)

# what is the probability that more than 200 patients will be cured
1-pbinom(200,1000,0.2)

# poisson distribution : dpois,ppois,rpois

# a dr's clinic recieves on an avg 20 patients per hr 
# they need to do special arrangements if that number exceeds 25 for any hr
# what are the chances that they'd need to go for special arrangements

#whats the parameter of poisson distribution : rate /unit time

dpois(10,20)
ppois(10,20)
1-ppois(10,20)

1-ppois(25,20)

# exponential dist:
# on an average it takes 3 minutes for taking a blood sample 
# whats the probability that it will take less than 2 minutes

pexp(2,1/3)

# what does a normal distribution look like

x=rnorm(10000,2,0.25)
d=data.frame(x=x)

ggplot(d,aes(x=x))+geom_histogram(bins=100)


# central limit theorem

library(ggplot2)

set.seed(1)

d=data.frame(X=rbeta(200000,2,5))

p=ggplot(d,aes(x=X))

p+geom_histogram()

mean(d$X)# pop_mean= 0.286
var(d$X)# pop_variance= 0.0255


k=numeric(10000)

for(i in 1:10000){
  s=sample(1:200000,1000)
  samp_mean=mean(d$X[s])
  k[i]=samp_mean
}

df=data.frame(k=k)
ggplot(df,aes(x=k))+geom_histogram(bins = 100)

# The mean and variance of these sample averages 
mean(k)
var(k)
# make histogram and draw a comparative normal curve with it

# reference from data viz script : 
  
library(hflights)
library(dplyr)

f_sub=filter(hflights,DepDelay<100)
ggplot(f_sub,aes(x=DepDelay))+geom_density(color="red")+
stat_function(fun=dnorm,
              args=list(mean=mean(f_sub$DepDelay),sd=sd(f_sub$DepDelay)),
              color="green")
## ----
## distribution of sample averages from a parabolic distribution
## do the same for this simulated data with sample size 100
set.seed(1)
t=runif(20000)


X=ifelse(k>0.5,4+sqrt(1-1.332*t),4-sqrt(1-1.332*t))

d=data.frame(X=X)
p=ggplot(d,aes(x=X))
p+geom_histogram()

k=numeric(10000)

for(i in 1:10000){
  s=sample(1:20000,1000)
  samp_mean=mean(d$X[s],na.rm=T)
  k[i]=samp_mean
}

df=data.frame(k=k)
ggplot(df,aes(x=k))+geom_histogram(bins = 100)
## ----

library(vcd)
library(ggplot2)
data("Arthritis")
p=ggplot(Arthritis,aes(x=Improved,y=Age)) 
p+geom_bar() 

## ------------------------------------------------------------------------

wq=read.csv("C:\\Users\\Larry Williams\\Desktop\\Mamata\\Edvancer\\R\\Data\\Data\\winequality-white.csv",
            sep=";")

## ------------------------------------------------------------------------

t.test(wq$alcohol,mu = 10)

# here the null hypothesis is that the average alcohol content in white wines is 10 units

# default alternate is of simple inequality

## ------------------------------------------------------------------------

t.test(wq$alcohol,mu =10,alternative = 'greater')


## ------------------------------------------------------------------------
t.test(wq$alcohol,mu = 10,alternative ="less" )


t.test(wq$alcohol,mu = 10.5,alternative ="greater" )
## ------------------------------------------------------------------------


### unpaierd test

read_f=d$read[d$female==1]

read_m=d$read[d$female==0]

var.test(read_f,read_m) # H_0 : ratio of variance = 1

t.test(read_f,read_m,paired = FALSE,var.equal = TRUE)


## ----
fit = aov(alcohol ~ quality ,data=wq)

summary(fit)


## ------------------------------------------------------------------------

pairwise.t.test(wq$alcohol, wq$quality, p.adj = "bonf")

## what is bonferroni adjustment
## how do i interpret the outcome of the code given above : santosh 



# does ethnicity has any impact on time in hosptial? 

### catagorical variable tests

table(d$race)

prop.table(table(d$race))

## ------------------------------------------------------------------------
chisq.test(table(d$race),p=c(0.2,0.1,0.1,0.6))


# Its a general assumption that among the patients where insulin
# level is checked [ insulin is not recorded as "No"]
# proportion of [Down,Steady , Up] is roughly [15%, 60%,25%]
# verify this

## ------------------------------------------------------------------------
chisq.test(table(d$ses,d$female))

## ------------------------------------------------------------------------
chisq.test(table(d$ses,d$race))

## ------------------------------------------------------------------------
table(d$race,d$ses)

## ------------------------------------------------------------------------
fisher.test(table(d$race,d$ses))

# does insulin level has any impact on readmission?

## ------------------------------------------------------------------------

## Dont ignore business context , and dont trust results of hypothesis tests blindly 

x=runif(400)

hist(x)

shapiro.test(x)

## --------------------------------------------------------------
y=rbeta(6000,2,8)

shapiro.test(y)
library(nortest)
ad.test(y)

## ------------------------------------------------------------------------

## ----
library(ggplot2)
df=data.frame(x,y)

ggplot(df,aes(x))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$x),sd=sd(df$x)),color="green")+
  ggtitle("Visual Normality Test for x ")

ggplot(df,aes(y))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(df$y),sd=sd(df$y)),color="green")+
  ggtitle("Visual Normality Test for y ")

## ------------------------------------------------------------------------
set.seed(1)
v1=rlnorm(20,0,0.4)
shapiro.test(v1)

## ------------------------------------------------------------------------
df=data.frame(v1)
ggplot(df,aes(x=v1))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(v1),sd=sd(v1)),color="green")+
  ggtitle("Visual Normality Test for v1 ")

## ------------------------------------------------------------------------
set.seed(1)
v2 = rt(60000,29)
ad.test(v2)

## ------------------------------------------------------------------------
df=data.frame(v2)
ggplot(df,aes(x=v2))+geom_density(color="red")+
  stat_function(fun=dnorm,args=list(mean=mean(v2),sd=sd(v2)),color="green")+
  ggtitle("Visual Normality Test for v2 ")

