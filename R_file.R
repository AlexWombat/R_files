
library(readr)
library(car)
library(olsrr)
library("GGally")
library(robustHD)
library(moments)
library(MASS)

bodyfatmen <- read_csv("~/År 3/Regression analysis/Project 1/bodyfatmen.csv")
View(bodyfatmen)

plot(bodyfatmen$age,bodyfatmen$density)
plot(bodyfatmen$weight,bodyfatmen$density)
lm.weight <- lm(density ~ weight,data =bodyfatmen)
plot(lm.weight,which=1)
plot(log(bodyfatmen$weight),bodyfatmen$density)
lm.weightlog <- lm(density ~ log(weight),data =bodyfatmen)
plot(lm.weightlog,which=1)
plot(bodyfatmen$height,bodyfatmen$density)
plot(bodyfatmen$neck,bodyfatmen$density)
lm.neck <- lm(density ~ neck, data = bodyfatmen)
abline(lm.neck)
plot(bodyfatmen$chest,bodyfatmen$density)
plot(bodyfatmen$abdomen,bodyfatmen$density)
plot(bodyfatmen$hip,bodyfatmen$density)
plot(bodyfatmen$thigh,bodyfatmen$density)
plot(bodyfatmen$knee,bodyfatmen$density)
plot(bodyfatmen$ankle,bodyfatmen$density)
plot(bodyfatmen$biceps,bodyfatmen$density)
plot(bodyfatmen$forearm,bodyfatmen$density)
plot(bodyfatmen$wrist,bodyfatmen$density)


lm.fit <- lm(density ~ ., data = bodyfatmen)
# lm.fit1 <- lm(density ~ bodyfatmen$height, data = bodyfatmen)
summary(lm.fit)
qqnorm(lm.fit$residuals, pch=20)
qqline(lm.fit$residuals)
p <- length(lm.fit$coefficients)
n <- nrow(bodyfatmen)
cooks.cutoff <- qf(0.5, p, n - p, lower.tail = FALSE) # Montgomery p. 215
par(mfrow = c(2, 3))
plot(lm.fit, which = 1:5, pch = 20)

b <- boxcox(lm.fit, lambda = seq(-9,2))               # optimal lambda is ish -4
CI <- b$x[b$y > max(b$y) - 1/2 * qchisq(.95,1)]
lm.box <- lm(density^(-4) ~ ., data = bodyfatmen)

hii <- lm.influence(lm.fit)$hat
cutoff_hii <- 4*p/n
plot(hii)
abline(h=cutoff_hii,col='blue')


covr = covratio(lm.fit)
covr
covr_cutoff <- 1+3*p/n
covr_cutoff

ggpairs(data = bodyfatmen)

X1 <- model.matrix(lm.fit)
X <- X1[, -1]
corr_matr <- cor(X)
eig_val <- eigen(corr_matr)$values
condition_number <- max(eig_val)/min(eig_val)
condition_index <- ols_eigen_cindex(lm.fit)           # multicollinearity diagnsostics
vif(lm.fit)
Xs <- scale(X)
test <- t(Xs) %*% Xs
test1 <- test/247

lm.log <- lm(log(density) ~ ., data = bodyfatmen)

lm.sqrt <- lm(sqrt(density) ~ ., data = bodyfatmen)

lm.rec <- lm((density)^(-1) ~ ., data = bodyfatmen)

lm.recsqrt <- lm(sqrt((density)^(-1)) ~ ., data = bodyfatmen)

lm.abd <- lm(density ~ abdomen, data = bodyfatmen)
plot(lm.abd$fitted.values, lm.abd$residuals)

all_possible <- ols_step_all_possible(lm.fit)

forward_sel <- ols_step_forward_p(lm.fit, details = TRUE)

backward_eli <- ols_step_backward_p(lm.fit, details = TRUE)

stepwise_reg <- ols_step_both_p(lm.fit, details = TRUE)

outlierTest(lm.fit) # Bonferonni p-value for most extreme obs

qqPlot(lm.fit, main="QQ Plot") #qq plot for studentized resid

leveragePlots(lm.fit) # leverage plots

ols_plot_cooksd_bar(lm.fit) # rule of thumb threshold: 4/n = 4/248 = 0.016

lm.chest <- lm(density ~ chest, data = bodyfatmen)
lm.abdomen <- lm(density ~ abdomen, data = bodyfatmen)
lm.weight <- lm(density ~ weight, data = bodyfatmen)
ols_plot_cooksd_bar(lm.chest)
ols_plot_cooksd_bar(lm.abdomen)
ols_plot_cooksd_bar(lm.weight)

bodyfatmen39r = bodyfatmen[-c(39),]
lm.39r = lm(density ~ ., data = bodyfatmen39r)
par(mfrow = c(2, 3))
plot(lm.39r, which = 1:5, pch = 20)
plot(bodyfatmen39r$neck,bodyfatmen39r$density)
lm.neck39r <- lm(density ~ neck, data = bodyfatmen39r)
abline(lm.neck39r)
lm.neck <- lm(density ~ neck, data = bodyfatmen39r)

summary(lm.39r)
ols_plot_cooksd_bar(lm.39r)
cooks.cutoff <- qf(0.5, p, 248 - p, lower.tail = FALSE) # Montgomery p. 215
plot(lm.39r, which = 1:5, pch = 20)


all_possible_39r <- ols_step_all_possible(lm.39r)
plot(bodyfatmen39r$abdomen,bodyfatmen39r$density)

bodyfatmen39r83r <- bodyfatmen39r[-c(82),]
lm.39r83r = lm(density ~ ., data = bodyfatmen39r83r)
summary(lm.39r83r)
plot(bodyfatmen39r83r$abdomen,bodyfatmen39r83r$density)
ols_plot_cooksd_bar(lm.39r83r)

plot(log(bodyfatmen$chest),bodyfatmen$density)
plot(bodyfatmen$chest, bodyfatmen$density)
lm.chestlog <- lm(density ~ bodyfatmen$chest, data = bodyfatmen)

testfinal <- lm(density ~ age+weight+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen)
testfinal1 <- lm(density ~ age+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen)
testfinal2 <- lm(density ~ age+weight+neck+abdomen+thigh+forearm+wrist, data = bodyfatmen)
testfinal3 <- lm(density ~ age+weight+neck+hip+thigh+forearm+wrist, data = bodyfatmen)
testfinal4 <- lm(density ~ age+weight+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen)
testfinal5 <- lm(density ~ age+weight+neck+abdomen+hip+forearm+wrist, data = bodyfatmen)

