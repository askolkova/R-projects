library(haven)
library(Matrix)
library(foreach)
library(glmnet)
cps <- read_dta("cps09mar.dta")


y <- matrix(log(cps$earnings/cps$hours/cps$week))
edu = matrix(cps$education)
ex <- matrix(cps$age - edu - 6)

married1 <- matrix(cps$marital==1)
married2 <- matrix(cps$marital==2)
married3 <- matrix(cps$marital==3)
windowed <- matrix(cps$marital==4)
divorced <- matrix(cps$marital==5)
separated <- matrix(cps$marital==6)
nevermarried <- matrix(cps$marital==7)
hisp <- matrix(cps$hisp==1)
female <- matrix(cps$female==1)
ne <- matrix(cps$region==1)
mw <- matrix(cps$region==2)
south <- matrix(cps$region==3)
union <- matrix(cps$union==1)
edu12 <- (edu==12)
edu13 <- (edu==13)
edu14 <- (edu==14)
edu16 <- (edu==16)
edu18 <- (edu==18)
edu20 <- (edu==20)
mx <- max(ex)
ex1 <- ex/mx
ex2 <- ex1^2
ex3 <- ex1^3
ex4 <- ex1^4
ex5 <- ex1^5
ex6 <- ex1^6
ex7 <- ex1^7
ex8 <- ex1^8
ex9 <- ex1^9
white <- matrix(cps$race==1)
black <- matrix(cps$race==2)
ai <- matrix(cps$race==3)
asian <- matrix(cps$race==4)
hawaiian <- matrix(cps$race==5)
n <- nrow(y)

X <- cbind(edu,edu12,edu13,edu14,edu16,edu18,edu20,ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,white,black,ai,asian,hawaiian)
X <- cbind(X,married1,married3,windowed,divorced,separated,nevermarried,female,hisp,ne,mw,south,union)

#3) Estimate the regression using least squares
#a) OLS
ols1 <- lm(y~X)
betaols <- coef(ols1)
##b) Ridge Regression
mRidge <- cv.glmnet(X,y, alpha=0, family="gaussian",nfolds=50)
betaRidge <- coef(mRidge,mRidge$lambda.min)
#c) Lasso
mLasso <- cv.glmnet(X, y, family="gaussian", nfolds=50)
betaLasso <- coef(mLasso,mLasso$lambda.min)
#d) Elastic Net with alpha=1/2
mElastic <- cv.glmnet(X, y, alpha=1/2, family="gaussian", nfolds=50)
betaElastic <- coef(mElastic, mElastic$lambda.min)
