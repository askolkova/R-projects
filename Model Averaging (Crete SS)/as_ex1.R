library(haven)
data <- read_dta("cps09mar.dta")

sample1 = (data$hisp==1) * (data$female==1)
y <- matrix(log(data$earnings[sample1==1] / data$hours[sample1==1] / data$week[sample1==1]))
edu <- matrix(data$education[sample1==1])
exp <- matrix(data$age[sample1==1] - edu - 6)
exp <- exp / max(exp)
exp2 <- exp ^ 2
exp3 <- exp ^ 3
exp4 <- exp ^ 4
exp5 <- exp ^ 5
exp6 <- exp ^ 6
married <- matrix(data$marital[sample1==1] <=3)
ne <- matrix(data$region[sample1==1]==1)
mw <- matrix(data$region[sample1==1]==2)
south <- matrix(data$region[sample1==1]==3)
college <- matrix(edu >= 16)

edu12 <- matrix(edu==12)
edu13 <- matrix(edu==13)
edu14 <- matrix(edu==14)
edu16 <- matrix(edu==16)
edu18 <- matrix(edu==18)
edu20 <- matrix(edu==20)

n <- nrow(y)

#preallocation
bic <- matrix(0, 1, 7)
aic <- matrix(0, 1, 7)
cv <- matrix(0, 1, 7)

# 7 - regression
x1 <- as.matrix(cbind(college, exp, exp2, married, ne, mw, south, matrix(1, n, 1)))
k1 = ncol(x1)
invx1 <- solve( t(x1) %*% x1)
b1 <- invx1 %*% ( t(x1) %*% y)
e1 <- y - x1 %*% b1
leverage <- rowSums(x1 * (x1 %*% invx1))
r1 <- e1 / (1 - leverage)

#8 
bic[1] <- n * log(2 * pi * mean(e1 ^ 2)) + k1 * log(n)
aic[1] <- n * log(2 * pi * mean(e1 ^ 2)) + 2 * k1
cv[1] <- sum(r1 ^ 2)

# 9 replace college with education
x2 <- as.matrix(cbind(edu, exp, exp2, married, ne, mw, south, matrix(1, n, 1)))
k2 = ncol(x2)
invx2 <- solve( t(x2) %*% x2)
b2 <- invx2 %*% ( t(x2) %*% y)
e2 <- y - x2 %*% b2
leverage2 <- rowSums(x2 * (x2 %*% invx2))
r2 <- e2 / (1 - leverage2)
bic[2] <- n * log(2 * pi * mean(e2 ^ 2)) + k2 * log(n)
aic[2] <- n * log(2 * pi * mean(e2 ^ 2)) + 2 * k2
cv[2] <- sum(r2 ^ 2)

# 10 Repeat with a regression replacing education with the six education dummies
x3 <- as.matrix(cbind(edu12, edu13, edu14, edu16, edu18, edu20, exp, exp2, married, ne, mw, south, matrix(1, n, 1)))
k3 = ncol(x3)
invx3 <- solve( t(x3) %*% x3)
b3 <- invx3 %*% ( t(x3) %*% y)
e3 <- y - x3 %*% b3
leverage3 <- rowSums(x3 * (x3 %*% invx3))
r3 <- e3 / (1 - leverage3)
bic[3] <- n * log(2 * pi * mean(e3 ^ 2)) + k3 * log(n)
aic[3] <- n * log(2 * pi * mean(e3 ^ 2)) + 2 * k3
cv[3] <- sum(r3 ^ 2)

# 11. Repeat adding a cubic in experience
x4 <- as.matrix(cbind(edu12, edu13, edu14, edu16, edu18, edu20, exp, exp2, exp3, married, ne, mw, south, matrix(1, n, 1)))
k4 = ncol(x4)
invx4 <- solve( t(x4) %*% x4)
b4 <- invx4 %*% ( t(x4) %*% y)
e4 <- y - x4 %*% b4
leverage4 <- rowSums(x4 * (x4 %*% invx4))
r4 <- e4 / (1 - leverage4)
bic[4] <- n * log(2 * pi * mean(e4 ^ 2)) + k4 * log(n)
aic[4] <- n * log(2 * pi * mean(e4 ^ 2)) + 2 * k4
cv[4] <- sum(r4 ^ 2)

# 12. Repeat adding a 4th power in experience
x5 <- as.matrix(cbind(edu12, edu13, edu14, edu16, edu18, edu20, exp, exp2, exp3, exp4, married, ne, mw, south, matrix(1, n, 1)))
k5 = ncol(x5)
invx5 <- solve( t(x5) %*% x5)
b5 <- invx5 %*% ( t(x5) %*% y)
e5 <- y - x5 %*% b5
leverage5 <- rowSums(x5 * (x5 %*% invx5))
r5 <- e5 / (1 - leverage5)
bic[5] <- n * log(2 * pi * mean(e5 ^ 2)) + k5 * log(n)
aic[5] <- n * log(2 * pi * mean(e5 ^ 2)) + 2 * k5
cv[5] <- sum(r5 ^ 2)

# 13. Repeat adding a 3rd, 4th, and 5th power in experience
x6 <- as.matrix(cbind(edu12, edu13, edu14, edu16, edu18, edu20, exp, exp2, exp3, exp4, exp5, married, ne, mw, south, matrix(1, n, 1)))
k6 = ncol(x6)
invx6 <- solve( t(x6) %*% x6)
b6 <- invx6 %*% ( t(x6) %*% y)
e6 <- y - x6 %*% b6
leverage6 <- rowSums(x6 * (x6 %*% invx6))
r6 <- e6 / (1 - leverage6)
bic[6] <- n * log(2 * pi * mean(e6 ^ 2)) + k6 * log(n)
aic[6] <- n * log(2 * pi * mean(e6 ^ 2)) + 2 * k6
cv[6] <- sum(r6 ^ 2)

# 14. Repeat adding a 3rd, 4th, and 5th, and 6th power in experience.
x7 <- as.matrix(cbind(edu12, edu13, edu14, edu16, edu18, edu20, exp, exp2, exp3, exp4, exp5, exp6, married, ne, mw, south, matrix(1, n, 1)))
k7 = ncol(x7)
invx7 <- solve( t(x7) %*% x7)
b7 <- invx7 %*% ( t(x7) %*% y)
e7 <- y - x7 %*% b7
leverage7 <- rowSums(x7 * (x7 %*% invx7))
r7 <- e7 / (1 - leverage7)
bic[7] <- n * log(2 * pi * mean(e7 ^ 2)) + k7 * log(n)
aic[7] <- n * log(2 * pi * mean(e7 ^ 2)) + 2 * k7
cv[7] <- sum(r7 ^ 2)

sink("exercise1.log")
cat("BIC \n")
print(bic)
cat("AIC \n")
print(aic)
cat("CV \n")
print(cv)
sink()