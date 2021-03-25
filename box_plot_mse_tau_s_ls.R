source("FastTau/FastTau.R")
source("Fast-S/fasts.R")
data(Boston, package='MASS')
Boston
x <- model.matrix(medv ~ ., data=Boston)
y <- Boston$medv
# Regression of median value by t est Tau estimator 
tauest <- FastTau(x=x, y=y, N=500, kk=2, tt=5, rr=2, approximate=0, seed=456)
# Classic lm
lmest <- lm(medv ~ ., data=Boston)
round(cbind(tauest$beta, coef(lmest)),digits = 3)
round(c(tauest$scale, summary(lmest)$sigma), 2)
# An interesting experiment is to compare the prediction properties of the robusts and least squares estimator. 
# In order to do this, we will use 10-fold CV, and, compare the squared prediction errors of each estimator. 
# To avoid penalizing poor predictions of outlying observations, we will compute the mean squared prediction 
# error of the 75% smallest residuals (in other words, we will use a 25% trimmed mean squared prediction error):
ls <- c()
tau <- c()
s <- c()
for (u in 1:10){
  tms <- function(e, trim=0.25) {
    es <- sort(e^2)
    n <- length(e)
    return( mean( es[1:floor(n*(1-trim))] ) )
  }
  tms <- function(e, trim=0.25) {
    es <- sort(e^2)
    n <- length(e)
    return( mean( es[1:floor(n*(1-trim))] ) )
  }
# The following chunck of code runs 10-fold CV and computes the tau- and the LS- predictions.
  n <- dim(x)[1]
  k <- 10
  ii <- sample( (1:n) %% k + 1)
  pr.tau <- pr.ls <- rep(NA, n)
  for(j in 1:k) {
    trs <- (ii != j)
    tr.x <- x[ trs, ]
    tr.y <- y[ trs ]
    taues <- FastTau(x=tr.x, y=tr.y, N=500, kk=2, tt=5, rr=2, approximate=0, seed=456)
    ses <- fast.s(x=tr.x, y=tr.y, int=0, N=500, k=2, best.r=5, seed=123)
    lses <- lm(medv ~ ., data=Boston, subset = trs)
    pr.ls[ !trs ] <- predict(lses, newdata = Boston[ !trs, ])
    pr.tau[ !trs ] <- as.vector( x[ !trs, ] %*% taues$beta )
    lses <- lm(medv ~ ., data=Boston, subset = trs)
    pr.s[ !trs ] <- as.vector( x[ !trs, ] %*% ses[[1]] )
    
  }
ls[u] <- tms((y - pr.ls))
tau[u] <- tms((y - pr.tau))
s[u] <- tms((y-pr.s))
  
}
# Boxplot of the trimmed mean squared prediction errors for Least Squares, Tau-Estimates and S-estimates.
boxplot(ls,tau,s,names = c("LS","Tau-estimates","S estimates"),col = c("lightblue","lightyellow","lightgreen"))
