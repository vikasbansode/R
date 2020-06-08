# https://www.r-tutor.com/elementary-statistics/hypothesis-testing/upper-tail-test-population-mean-known-variance

# d	density/mass function
# p	probability (cumulative distribution function) P(X <= x)
# q	quantiles, given q, the smallest x such that P(X <= x) > q
# r	random number generation


# normal Distribution
x <- seq(-3,3,0.1)
plot(x=x, y=dnorm(x, mean=0, sd=1), type='l')

# Binomial Distribution
x <- seq(0,20,1)
plot(x=x, y=dbinom(x,20,0.5))

# Uniform Distribution
u <- runif(20)

punif(u) == u
dunif(u) == 1

var(runif(10000)) 

# Log-normal
samples <- rlnorm(100, meanlog=0, sdlog=1)
par(fig=c(0,1,0,0.35))
boxplot(samples, horizontal=T, bty="n", xlab="log-normal distribution")
par(fig=c(0,1,0.25,1), new=T)
s <- seq(0,max(samples),0.1)
d <- dlnorm(s, meanlog=0, sdlog=1)
hist(samples, prob=T, main="", col=gray(0.9), ylim=c(0,max(d)))
lines(density(samples), lty=2)
curve(dlnorm(x, meanlog=0, sdlog=1), lwd=2, add=T)
rug(samples)

# Exponential
dexp(1) - exp(-1)
## a fast way to generate *sorted*  U[0,1]  random numbers:
rsunif <- function(n) { n1 <- n+1
cE <- cumsum(rexp(n1)); cE[seq_len(n)]/cE[n1] }
plot(rsunif(1000), ylim=0:1, pch=".")
abline(0,1/(1000+1), col=adjustcolor(1, 0.5))

###################################################
#                 Binomial Distribution
# The binomial distribution is a discrete probability distribution.
# It describes the outcome of n independent trials in an experiment. 
# Each trial is assumed to have only two outcomes, either success or 
# failure. If the probability of a successful trial is p, then the 
# probability of having x successful outcomes in an experiment of n 
# independent trials is as follows.
###################################################

####################################################
# Problem
# Suppose there are twelve multiple choice questions in an English
# class quiz. Each question has five possible answers, and only one
# of them is correct. Find the probability of having four or less
# correct answers if a student attempts to answer every question 
# at random.
#
# Solution
# Since only one out of five possible answers is correct, 
# the probability of answering a question correctly by random is
# 1/5=0.2. We can find the probability of having exactly 4 correct 
# answers by random attempts as follows.
####################################################
dbinom(4, size=12, prob=0.2)
dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2)
# Alternatively, we can use the cumulative probability 
# function for binomial distribution pbinom.
pbinom(4, size=12, prob=0.2) 

#Answer
# The probability of four or less questions answered correctly
# by random in a twelve question multiple choice quiz is 92.7%.

####################################################
#             Poisson Distribution
# The Poisson distribution is the probability distribution of 
# independent event occurrences in an interval. 
# If ?? is the mean occurrence per interval, then the probability of
# having x occurrences within a given interval is:
# 
# Problem
# If there are twelve cars crossing a bridge per minute on average, 
# find the probability of having seventeen or more cars crossing the
# bridge in a particular minute.
# 
# Solution
# 
# The probability of having sixteen or less cars crossing the bridge 
# in a particular minute is given by the function ppois.
###################################################################

ppois(16, lambda=12)   # lower tail

# Hence the probability of having seventeen or more cars crossing the bridge in 
# a minute is in the upper tail of the probability density function
ppois(16, lambda=12, lower=FALSE)   # upper tail

# Answer
# If there are twelve cars crossing a bridge per minute on average, 
# the probability of having seventeen or more cars crossing the bridge 
# in a particular minute is 10.1%.

####################################################
#               Continuous Uniform Distribution
# The continuous uniform distribution is the probability distribution 
# of random number selection from the continuous interval between a and b.
# Its density function is defined by the following.
# Problem
# Select ten random numbers between one and three.
# Solution
# We apply the generation function runif of the uniform distribution to 
# generate ten random numbers between one and three.
####################################################
runif(10, min=1, max=3)
####################################################
#                 Exponential Distribution
# The exponential distribution describes the arrival time of a randomly 
# recurring independent event sequence. If ?? is the mean waiting time for
# the next event recurrence, its probability density function is:
# Problem
# Suppose the mean checkout time of a supermarket cashier is three minutes. 
# Find the probability of a customer checkout being completed by the cashier 
# in less than two minutes.
# Solution
# The checkout processing rate is equals to one divided by the mean checkout
# completion time. Hence the processing rate is 1/3 checkouts per minute. 
# We then apply the function pexp of the exponential distribution with 
# rate=1/3.
####################################################
pexp(2, rate=1/3)
# Answer
# The probability of finishing a checkout in under two minutes by 
# the cashier is 48.7%
####################################################
#                Normal Distribution
# The normal distribution is defined by the following probability density function, where ?? is the population mean and ??2 is the variance.
# If a random variable X follows the normal distribution, then we write:
# In particular, the normal distribution with ?? = 0 and ?? = 1 is called the standard normal distribution, and is denoted as N(0,1). It can be graphed as follows.
#
# The normal distribution is important because of the Central Limit Theorem, which states that the population of all possible samples of size n from a population with mean ?? and variance ??2 approaches a normal distribution with mean ?? and ??2???n when n approaches infinity.
# Problem
# Assume that the test scores of a college entrance exam fits a normal distribution. Furthermore, the mean test score is 72, and the standard deviation is 15.2. What is the percentage of students scoring 84 or more in the exam?
# Solution
# We apply the function pnorm of the normal distribution with mean 72 and standard deviation 15.2. Since we are looking for the percentage of students scoring higher than 84, we are interested in the upper tail of the normal distribution.
#
####################################################
pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 
# Answer
# The percentage of students scoring 84 or more in the college entrance exam is 21.5%
####################################################
#               Chi-squared Distribution 
# If X1,X2,.,Xm are m independent random variables having the standard normal distribution, then the following quantity follows a Chi-Squared distribution with m degrees of freedom. Its mean is m, and its variance is 2m.
# Problem
# Find the 95th percentile of the Chi-Squared distribution with 7 degrees of freedom.
# Solution
# We apply the quantile function qchisq of the Chi-Squared distribution against the decimal values 0.95.
#
####################################################
qchisq(.95, df=7)        # 7 degrees of freedom
# Answer
# The 95th percentile of the Chi-Squared distribution with 7 degrees of freedom is 14.067.
####################################################
#                Student t Distribution
# Assume that a random variable Z has the standard normal distribution, and another random variable V has the Chi-Squared distribution with m degrees of freedom. Assume further that Z and V are independent, then the following quantity follows a Student t distribution with m degrees of freedom.
# Problem
# Find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.
# Solution
# We apply the quantile function qt of the Student t distribution against the decimal values 0.025 and 0.975.
# 
####################################################

qt(c(.025, .975), df=5)   # 5 degrees of freedom
# Answer
# The 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees 
# of freedom are -2.5706 and 2.5706 respectively.

####################################################
#               F Distribution
# If V 1 and V 2 are two independent random variables having the Chi-Squared distribution with m1 and m2 degrees of freedom respectively, then the following quantity follows an F distribution with m1 numerator degrees of freedom and m2 denominator degrees of freedom, i.e., (m1,m2) degrees of freedom.
# Problem
# Find the 95th percentile of the F distribution with (5, 2) degrees of freedom.
# Solution
# We apply the quantile function qf of the F distribution against the decimal value 0.95.
####################################################
qf(.95, df1=5, df2=2) 

# Answer
# The 95th percentile of the F distribution with (5, 2) degrees of freedom is 19.296.

####################################################
#                 Interval Estimation
# It is a common requirement to efficiently estimate population parameters based on simple random sample data. In the R tutorials of this section, we demonstrate how to compute the estimates. The steps are to be illustrated with a built-in data frame named survey. It is the outcome of a Statistics student survey in an Australian university.
# The data set belongs to the MASS package, which has to be pre-loaded into the R workspace prior to use.
#
#
####################################################

library(MASS)
head(survey) 

####################################################
#               Point Estimate of Population Mean
# For any particular random sample, we can always compute its sample mean. Although most often it is not the actual population mean, it does serve as a good point estimate. For example, in the data set survey, the survey is performed on a sample of the student population. We can compute the sample mean and use it as an estimate of the corresponding population parameter.
# Problem 
# Find a point estimate of mean university student height with the sample data from survey.
# Solution
# For convenience, we begin with saving the survey data of student heights in a variable height.survey.
####################################################
height.survey = survey$Height

# It turns out not all students have answered the question, and we must filter out the missing values. Hence we apply the mean function with the "na.rm" argument as TRUE.
mean(height.survey, na.rm=TRUE)  # skip missing values 

#Answer
# A point estimate of the mean student height is 172.38 centimeters.

####################################################
#     Interval Estimate of Population Mean with Known Variance
# After we found a point estimate of the population mean, we would need a way to quantify its accuracy. Here, we discuss the case where the population variance ??2 is assumed known.
# Let us denote the 100(1 ????????2) percentile of the standard normal distribution as z?????2. For random sample of sufficiently large size, the end points of the interval estimate at (1 ??? ??) confidence level is given as follows:
# Problem
# Assume the population standard deviation ?? of the student height in survey is 9.48. Find the margin of error and interval estimate at 95% confidence level.
# Solution
# We first filter out missing values in survey$Height with the na.omit function, and save it in height.response.
#
####################################################

height.response = na.omit(survey$Height)

#Then we compute the standard error of the mean.

n = length(height.response) 

sigma = 9.48     # population standard deviation
sem = sigma/sqrt(n); sem # standard error of the mean 

# Since there are two tails of the normal distribution, the 95% confidence level would imply the 97.5th percentile of the normal distribution at the upper tail. Therefore, z?????2 is given by qnorm(.975). We multiply it with the standard error of the mean sem and get the margin of error.
E = qnorm(.975) * sem; E         # margin of error 
# We then add it up with the sample mean, and find the confidence interval as told.
xbar = mean(height.response)   # sample mean
xbar + c(-E, E) 
#Answer
#Assuming the population standard deviation ?? being 9.48, the margin of error for the student height survey at 95% confidence level is 1.2852 centimeters. The confidence interval is between 171.10 and 173.67 centimeters.

####################################################
#
#
#
####################################################


####################################################
#
#
#
####################################################


####################################################
#
#
#
####################################################


####################################################
#
#
#
####################################################


####################################################
#
#
#
####################################################
