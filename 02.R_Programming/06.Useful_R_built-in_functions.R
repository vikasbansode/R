# duplicate
# unique
# cbind
# rbind
# with
# within
# which
# grep
# sub
# paste
# tolower
# toupper
# strplit

# numerica functions
abs(x)
sqrt(x)
ceiling(x)
floor(x)
trunc(x)
round(x,digits = n)
signif(x,digits = n)
cos(x)
sin(x)
log(x)
log2(x)
log10(x)
exp(x)

# Character functions
substr(x, start=n1, stop=n2)
grep(pattern, x , ignore.case=FALSE, fixed=FALSE)
sub(pattern, replacement, x, ignore.case =FALSE, fixed=FALSE)	
strsplit(x, split)
paste(..., sep="")
toupper(x)
tolower(x)	

# Probability Functions
dnorm(x)
pnorm(q)
qnorm(p)
rnorm(n, m=0,sd=1)

dbinom(x, size, prob)
pbinom(q, size, prob)
qbinom(p, size, prob)
rbinom(n, size, prob)

dpois(x, lamda)
ppois(q, lamda)
qpois(p, lamda)
rpois(n, lamda)

dunif(x, min=0, max=1)
punif(q, min=0, max=1)
qunif(p, min=0, max=1)
runif(n, min=0, max=1)

# Statistical Functions

mean(x, trim=0,na.rm=FALSE)
median(x)
sd(x)
quantile(x, probs)
range(x)
sum(x)
diff(x, lag=1)
min(x)
max(x)
scale(x, center=TRUE, scale=TRUE)

# Other Functions
seq(from , to, by)
rep(x, ntimes)
cut(x, n)
