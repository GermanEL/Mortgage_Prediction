library('readxl')
library('DIMORA')
df <- read_excel("mortgage_data.xlsx")
df <- as.data.frame(df)

df$month <- as.Date(df$month, "%Y%m/%d")
program <- df$program_loans[17:30]
month <- df$month[17:30]

plot(program ~ month, xaxt = "n", type = "l")
axis(1, month, format(month, "%b %Y"), cex.axis = .6, gap.axis = 0)

######### BASS MODEL ######################################################################## 

#options(scipen=999)
BMs <- BASS.standard(program, display = F, ous = 30)
print(summary(BMs),digits=2)
summary(BMs)
coef(BMs)
Acf(program)

res <- residuals(BMs)
plot(res)
Acf(res)
predict(BMs, newx=1:30)

plot(BMs, mode="i", oos="y")
plot(BMs, type = "l", mode="i", oos="y", xlim = c(1,30))
acf(program)

######## GENERALIZED BASS MODEL ######################################################################### 

plot(program, type="l", xlab = "time")
GBMe1 <- BASS.generalized(program, shock="exp", nshock=1, 
                          ous = 30, display = F,
                          prelimestimates =  
                            c(6.328582e+05, 4.289052e-02, 1.831325e-01, # m, p, q = obtained with BMs
                              7, -0.1, 0.2)) # a1, b1, c1 = make a guess
# a1 is a starting point, guess by graph, 
#    where could be a change of behavior of the 
#    series in case of presence of positive shock
# b1 = -0.1 negative memory, shock is absorbed
# c1 = 0.1 positive shock
summary(GBMe1)

GBMe2 <- BASS.generalized(program, shock="mixed", nshock=1, 
                          ous = 30, display = F,
                          prelimestimates =  
                            c(6.328582e+05, 4.289052e-02, 1.831325e-01, # m, p, q = obtained with BMs
                              6, -0.1, 0.1)) # a1, b1, c1 = make a guess
summary(GBMe2)

GBMe3 <- BASS.generalized(program, shock="mixed", nshock=1, 
                          ous = 30, display = F,
                          prelimestimates =  
                            c(7.178791e+05, 2.864600e-02, 2.704991e-01, # m, p, q = obtained with BMs
                              6, -0.1, 0.3)) # a1, b1, c1 = make a guess
summary(GBMe3)


res <- residuals(GBMe1)
plot(res)
Acf(res)
predict(GBMe1, newx=1:30)

plot(GBMe1, mode="i", oos="y")
plot(GBMe1, type = "l", mode="i", oos="y", xlim = c(1,30))

######## GGM PROGRAM MODELING #########################################################################

GGM <- GG.model(program, prelimestimates=c(6.328582e+05, 0.2, 0.1,
                                           0.043, 0.183), display=F)
summary(GGM)

residuals(GGM)
plot(residuals(GGM))
Acf(residuals(GGM))
predict(GGM, newx=1:30)
plot(GGM, type = "l", mode='i', oos='y', xlim=c(1,30), typeog=1)

########## ARIMA MODELS OVERALL MORTGAGE #######################################################################

library(fpp2)
library(forecast)
library(lmtest)
library(TSA)

mortgage = ts(df$all_loans, start = c(2019, 1), frequency = 12)

aa <- auto.arima(mortgage)
summary(aa)
plot(mortgage, type = 'l')
lines(fitted(aa), col=2)
plot(forecast(aa))
Acf(residuals(aa))

a_rate <- Arima(mortgage, xreg=df$rate, order=c(1,2,1))
summary(a_rate)
res1 <- residuals(a_rate)
Acf(res1)
plot(mortgage, type = 'l')
lines(fitted(a_rate), col=2)

aa_rate <- auto.arima(mortgage, xreg=df$rate)
summary(aa_rate)
res1 <- residuals(aa_rate)
Acf(res1)
plot(mortgage, type = 'l')
lines(fitted(aa_rate), col=2)


a_income <- Arima(mortgage, xreg=df$real_income, order=c(1,3,3))
summary(a_income)
res1 <- residuals(a_income)
Acf(res1)
plot(mortgage, type = 'l')
lines(fitted(a_income), col=2)

aa_income <- auto.arima(mortgage, xreg=df$real_income)
summary(aa_income)
res1 <- residuals(aa_income)
Acf(res1)
plot(mortgage, type = 'l')
lines(fitted(aa_income), col=2)

plot(forecast(aa_rate, xreg = df$rate))
### Final plot
plot(mortgage)
lines(fitted(aa), col=2)
lines(fitted(aa_rate), col=4)
lines(fitted(aa_income), col=3)

###### LINEAR REGRESSION MORTGAGE OVERALL ###########################################################################

install.packages("corrplot")
library('corrplot')
library('regclass')
library('car')

df <- as.data.frame(df)
df_x <- df[,5:9]
mat <- cor(df_x)
all_loans <- df$all_loans

Acf(all_loans)
Pacf(all_loans)

corrplot(mat, method = 'number')
corrplot.mixed(mat, order = 'AOE')

fit1 <- lm(mortgage ~ df$rate + df$price_m2 + df$unemp  + df$mkd_m2  + df$real_income)
summary(fit1)
dwtest(fit1, alt="two.sided")
vif(fit1)
fit1 = fitted(fit1)
plot(mortgage, type ="l")
lines(fit1, col=2)
res <- residuals(fit1)
plot(res)
Acf(res)

fit1_1 <- lm(all_loans ~ df$unemp  + df$mkd_m2  + df$real_income)
summary(fit1_1)
dwtest(fit1_1, alt="two.sided")
vif(fit1_1)
fit1_1 = fitted(fit1_1)
plot(mortgage)
lines(fit1_1, col=4)
res <- residuals(fit1_1)
plot(res)
Acf(res)

AIC(fit1_1)

fit2 <- lm(all_loans ~ df$price_m2 + df$mkd_m2  + df$real_income)
summary(fit2)
dwtest(fit2, alt="two.sided")
vif(fit2)
fit = fitted(fit2)
plot(mortgage)
lines(fit, col=2)
res <- residuals(fit2)
plot(res)
Acf(res)

fit3 <- lm(all_loans ~ df$real_income + df$rate)
summary(fit3)
dwtest(fit3, alt="two.sided")
vif(fit3)
fit = fitted(fit3)
plot(mortgage)
lines(fit, col=2)

fit4 <- lm(all_loans ~ df$price_m2)
summary(fit4)
dwtest(fit4, alt="two.sided")
vif(fit4)
fit = fitted(fit4)
plot(mortgage)
lines(fit, col=2)

###### GAM MODELS MORTGAGE OVERALL ###########################################################################
library(gam)

tt <- (1:length(mortgage)) #trend
seas <- factor(c(rep(1:12, length(mortgage)/12), 1:10))
seas

g0 <- gam(mortgage ~ tt + seas)
summary(g0)

g1 <- gam(mortgage ~ s(tt) + seas + s(df$real_income))
options(scipen=999)
summary(g1)

plot(as.numeric(mortgage), type = "l")
lines(fitted(g0), col=2)
lines(fitted(g1), col=4)
lines(fitted(g2), col=3)

AIC(g0)
AIC(g1)
#par("mar")
par(mar=c(1,1,1,1))
tsdisplay(residuals(g1))
tsdisplay(residuals(g0))

g2<- gam(mortgage ~ lo(tt) + seas + lo(df$real_income))
summary(g2)
AIC(g2)
tsdisplay(residuals(g2))


aar1 <- auto.arima(residuals(g1))
plot(as.numeric(mortgage), type = "l")
lines(fitted(g1), col=2)
lines(fitted(g1) + fitted(aar1), col=3)
