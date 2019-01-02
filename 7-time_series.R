# 1
a <- as.Date('1901-01-01')
b <- as.Date('2000-12-31')
round(difftime(b,a,units='days'))


# 2
library('TSA')
library('ggplot2')
data(wages)

library('dplyr')

tibble(Wages = as.numeric(wages), Time = as.numeric(time(wages))) %>% ggplot(aes(x=Time, y = Wages)) + geom_line()

model.lm <- lm(wages ~ time(wages))
model.q <- lm(wages ~ time(wages) + I(time(wages)^2))

tibble(Wages = as.numeric(wages), Time = as.numeric(time(wages))) %>% ggplot(aes(x=Time, y=Wages)) + geom_line() + geom_smooth(method ='lm', se = FALSE) + geom_smooth(method ='lm', se = FALSE, formula = y ~ poly(x,2), col = 'red')

y ~ x + I(x^2)

AIC(model.lm, model.q)
BIC(model.lm, model.q)

par(mfrow=c(1,2))
plot(rstandard(model.lm), pch = 20, main = 'Linear')
plot(rstandard(model.q), pch = 20, main = 'Quadratic')
shapiro.test(rstandard(model.lm))
shapiro.test(rstandard(model.q))

# Wybieramy model.q na podstawie kryterium AIC, ponieważ ma mniejszą wartość

# model.lm - reszty nie mają rozkładu normalnego
# model.q - reszty mają rozkład normalny - lepszy model


# 3
library('ggplot2')
female <- read.table("female.txt")
female <- ts(female, frequency = 12, start = c(1961,7))
female.filter <- stats::filter(female, rep(1/17,17))
tibble(Time = as.vector(time(female)), Female = as.numeric(female), Filter = as.numeric(female.filter)) %>% ggplot(aes(x = Time, y = Female)) + geom_line() + geom_line(aes(y = Filter), col = 'red', size = 1.2)
auto.arima(female)

# Sezonowość miesięczna


# 4
library('ggplot2')
unemp <- read.table("unemp.txt")
unemp <- ts(unemp, frequency = 12, start = c(1975,7))
unemp.filter <- stats::filter(unemp, rep(1/12,12))
model.hw <- c(rep(NA, 12), unclass(HoltWinters(unemp)$fitted)[,1])
tibble(Time = as.vector(time(unemp)), Unemp = as.numeric(unemp), Filter = as.numeric(unemp.filter), HW = model.hw) %>% ggplot(aes(x = Time, y = Unemp)) + geom_line() + geom_line(aes(y = Filter), col = 'red', size = 1.2) + geom_line(aes(y = HW), col = 'blue')
auto.arima(unemp)

# Występuje sezonowość


# 5
1. ARMA(2,1)
2. AR(2)
3. ARMA(2,2)

# 6
abs(polyroot(c(1, -3/2, 1/2)))
# proces nie jest stacjonarny

abs(polyroot(c(1, -5/6, 1/6)))
# proces jest stacjonarny

abs(polyroot(c(1, -2/3, 5/3)))
# proces nie jest stacjonarny


# 7
library('TSA')
data(robot)
plot(robot)
# library('ggplot2')
# autoplot(robot)
# nie ma jednoznacznej odpowiedzi, czy proces jest stacjonarny, czy nie
# proces nie jest stacjnarny

library('tseries')
ar1 <- arima(robot, order = c(1, 0, 0))
# (ar1 <- arima(robot, order = c(1, 0, 0)))
summary(ar1)
arima011 <- arima(robot, order = c(0, 1, 1))
# (arima011 <- arima(robot, order = c(0, 1, 1)))
summary(arima011)

AIC(ar1,arima011)
# model ARIMA(0,1,1) jest lepszy, wartość kryterim jest mniejsza

predict(arima011, n.ahead = 5)


# 8
library('TSA')
data(gold)
plot(gold)
plot(diff(log(gold)))

library('forecast')
model <- auto.arima(diff(log(gold)))
summary(model)