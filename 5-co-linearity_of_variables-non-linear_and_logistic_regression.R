# 1
library('DAAG')
litters = data.frame(litters)
View(litters)
litters.lm <- lm(litters$brainwt ~ litters$bodywt + litters$lsize)
vif(litters.lm)

# Występuje rozdęcie wariancji

# 2
library('carData')
USPop = data.frame(USPop)
View(USPop)
library(nlstools)
nls(population ~ SSlogis(year,a,b,c), data = USPop) -> model.nls
library('ggplot2')
ggplot(USPop, aes(x = year, y = population)) + geom_point() + stat_function(fun = function(x) coef(model.nls)[1]/(1+exp((coef(model.nls)[2] - x) / coef(model.nls)[3])), col = 'red')

plot(nlsResiduals(model.nls))
test.nlsResiduals(nlsResiduals(model.nls))


# 3
library('drc')
heartrate = data.frame(heartrate)
View(heartrate)
nls(rate ~ SSlogis(pressure,a,b,c), data = heartrate) -> model1.nls
ggplot(heartrate, aes(x = pressure, y = rate)) +  geom_point() + stat_function(fun = function(x) coef(model1.nls)[1]/(1+exp((coef(model1.nls)[2] - x) / coef(model1.nls)[3])), col = 'red')

plot(nlsResiduals(model1.nls))
test.nlsResiduals(nlsResiduals(model1.nls))


# 4
v = c(10,16.3,23,27.5,31,35.6,39,41.5,42.9,45,46,45.5,46,49,50)
t = 1:15
model2.nls <- nls(v ~ SSmicmen(t, a, b))
ggplot(data=data.frame(t=t,v=v), aes(x = t, y = v)) + geom_point() + stat_function(fun = function(x) coef(model2.nls)[1]*x/(coef(model2.nls)[2]+x), col = 'red') +
geom_point(aes(x=18,y=predict(model2.nls,list(t=18))[1]),col='green')


# 5
library('DAAG')
moths = data.frame(moths)
View(moths)
model.poisson <- glm(A ~ log(meters), family = poisson, data = moths)
summary(model.poisson)


# 6
koncentracja = c(0.1,0.5,1,10,20,30,50,70,80,100,150)
Nie = c(7,1,10,9,2,9,13,1,1,4,3)
Tak = c(0,0,3,4,0,6,7,0,0,1,7)
prawd = Tak/(Tak+Nie)
suma = Tak + Nie
dane = data.frame(koncentracja, Nie, Tak, prawd, suma)

model <- glm(prawd ~ log(koncentracja), family = binomial, weights = suma, data = dane)
summary(model)