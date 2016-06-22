## ============================================================================
## Functional forms of density dependence
## Jan 7, 2016
## ============================================================================

x <- seq(0,100)

# BH
a <- 0.02
B <- 10
y.bh <- 1/(1/a + B*x) 

plot(y.bh~x, type="l")


# Logistic
r <- 0.8
K <- 30
y.log <- 1+r*(1-x/K)

plot(y.log~x, type="l")


# Fox
y.fox <- 1+r*(1-log(x)/log(K))

plot(y.fox~x, type="l")


# Pella-Tomlinson
# parameterizations to keep:
params <- list(K=30, m=20, n=12, range=c(1,30))

x.pel <- seq(0,30, 0.05)
K <- 30 # the place where it crosses the zero threshold
m <- 2000 # the steepness of the curve near zero
n <- 12 # influences the shape of the curve as it approaches K
y.pel <- (1+m*(n^(n/(n-1))/(n-1))*(x.pel/K - (x.pel/K)^n))/x.pel

x.pel <- seq(1,30, 0.05)
K <- 50 # the place where it crosses the zero threshold
m <- 200 # the steepness of the curve near zero
n <- 12 # influences the shape of the curve as it approaches K
y.pel <- (1+m*(n^(n/(n-1))/(n-1))*(x.pel/K - (x.pel/K)^n))/x.pel

plot(y.pel~x.pel, type="l")

# Ricker
R0 <- 4
y.rick <- exp(R0*(1-x/K))

plot(y.rick~x, type="l")


# Hassel
K <- 20
y.has <- K*(R0+1)/(K+x*R0)

plot(y.has~x, type="l")


# Gompertz ### SOMETHING IS WRONG WITH THIS EQUATION ###
q <- 10
f <- .20
t <- 20

y.gom <- q*exp(log(f*x/q)*exp(a*t))/x

plot(y.gom~x, type="l")


# Multi-plot
pdf("DD_types_comparison.pdf", width=7.5, height=5)
par(mfrow=c(2,3))
par(mar=c(3.1, 3.1, 2.1, 1))
plot(y.bh~x, type="l", main="Beverton-Holt", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

plot(y.log~x, type="l", main="Logistic", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

plot(y.fox~x, type="l", main="Fox", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

plot(y.pel~x.pel, type="l", main="Pella-Tomlinson", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

plot(y.rick~x, type="l", main="Ricker", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

plot(y.has~x, type="l", main="Hassel", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

dev.off()

pdf("DD_types_Pella-T_comparison.pdf", width=7.5, height=2.5)
par(mfrow=c(1,3))
par(mar=c(3.1, 3.1, 2.1, 1))

x.pel <- seq(1,30, 0.05)
K <- 50 # the place where it crosses the zero threshold
m <- 200 # the steepness of the curve near zero
n <- 12 # influences the shape of the curve as it approaches K
y.pel <- (1+m*(n^(n/(n-1))/(n-1))*(x.pel/K - (x.pel/K)^n))/x.pel

plot(y.pel~x.pel, type="l", main="Pella-Tomlinson", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

x.pel <- seq(0,30, 0.05)
K <- 30 # the place where it crosses the zero threshold
m <- 200 # the steepness of the curve near zero
n <- 12 # influences the shape of the curve as it approaches K
y.pel <- (1+m*(n^(n/(n-1))/(n-1))*(x.pel/K - (x.pel/K)^n))/x.pel

plot(y.pel~x.pel, type="l", main="Pella-Tomlinson", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

x.pel <- seq(0,30, 0.05)
K <- 30 # the place where it crosses the zero threshold
m <- 2000 # the steepness of the curve near zero
n <- 12 # influences the shape of the curve as it approaches K
y.pel <- (1+m*(n^(n/(n-1))/(n-1))*(x.pel/K - (x.pel/K)^n))/x.pel

plot(y.pel~x.pel, type="l", main="Pella-Tomlinson", xlab="", ylab="",axes=F)
box()
mtext("f(N)", 2, line=1.2)
mtext("N", 1, line=1.2)

dev.off()


# Others from bellows
b <- 0.2
y.bel1 <- x^-b
y.bel2 <- exp(-a*x)
y.bel3 <- (1+a*x)^-1
y.bel4 <- exp(-a*x^b)
y.bel5 <- (1+(a*x)^b)^-1
y.bel6 <- (1+a*x)^-b
y.bel7 <- (1+exp(b*x-a))^-1

par(mfrow=c(2,3))
plot(y.bel1~x, type="l", xlab="N", ylab="f(N)")
plot(y.bel2~x, type="l", xlab="N", ylab="f(N)")
plot(y.bel3~x, type="l", xlab="N", ylab="f(N)")
plot(y.bel4~x, type="l", xlab="N", ylab="f(N)")
plot(y.bel5~x, type="l", xlab="N", ylab="f(N)")
plot(y.bel7~x, type="l", xlab="N", ylab="f(N)")
