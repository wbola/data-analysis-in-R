# 1
library('ggplot2')
library('car')
library('MASS')
cars = data.frame(cars)
View(cars)
ggplot(cars, aes(x = speed, y = dist)) + geom_point() + geom_smooth(method = 'lm', se=FALSE) + stat_smooth(se = FALSE, method = 'lm', formula = y ~ x + I(x^2), color = 'red')


# 2
library('MASS')
hills = data.frame(hills)
View(hills)
ggplot(hills, aes(x = time, y = dist)) + geom_point() + geom_smooth(method = 'lm', se = FALSE, color = 'blue') -> p1
ggplot(hills, aes(x = time, y = climb)) + geom_point() + geom_smooth(method = 'lm', se = FALSE, color = 'blue') -> p2
library('gridExtra')
grid.arrange(p1, p2, ncol = 2)
library('broom')
lmfit <- lm(dist ~ time, hills)
tidy(lmfit)
lmfit1 <- lm(climb ~ time, hills)
tidy(lmfit1)


# 3
library('UsingR')
homedata = data.frame(homedata)
View(homedata)
library('broom')
lmfit2 <- lm(y2000 ~ y1970, homedata)
tidy(lmfit2)
plot(lmfit2)
predict(lmfit2, data.frame(y1970 = c(75000)))


# 4
Cena = c(300, 250, 400, 550, 317, 389, 425, 289, 389, 559)
Liczba.pokoi = c(3, 3, 4, 5, 4, 3, 6, 3, 4, 5)
dom = data.frame(Cena, Liczba.pokoi)
View(dom)
ggplot(dom, aes(x = Liczba.pokoi, y = Cena)) + geom_point() + geom_smooth(method = 'lm', se = FALSE, color = 'blue')
library('broom')
lmfit3 <- lm(Cena ~ Liczba.pokoi, dom)
tidy(lmfit3)
predict(lmfit3, data.frame(Liczba.pokoi = c(2)))


# 5
library('plotly')
library('UsingR')
florida = data.frame(florida)
View(florida)
ggplot(florida, aes(x = BUSH, y = BUCHANAN)) + geom_point()
a <- ggplot(florida, aes(x = BUSH, y = BUCHANAN)) + geom_point()
ggplotly(a)
library('broom')

which(florida$BUSH == 289456)
which(florida$BUCHANAN == 561)
which(florida$BUSH == 152846)
which(florida$BUCHANAN == 3407)

dane = data.frame(COUNTRY = florida$County[-50][-13], BUCHANAN = florida$BUCHANAN[-50][-13], BUSH = florida$BUSH[-50][-13])
lmfit4 <- lm(BUCHANAN ~ BUSH, dane)
tidy(lmfit4)

predict(lmfit4, data.frame(BUSH = florida$BUSH[florida$County == 'DADE']))


# 6
library('plotly')
library('UsingR')
emissions = data.frame(emissions)
View(emissions)
ggplot(emissions, aes(x = GDP, y = CO2)) + geom_point()
b <- ggplot(emissions, aes(x = GDP, y = CO2)) + geom_point()
ggplotly(b)
library('broom')
lmfit5 <- lm(CO2 ~ GDP, emissions)
tidy(lmfit5)

which(emissions$GDP == 8083000)
which(emissions$CO2 == 6750)

emissions2 = data.frame(CO2 = emissions$CO2[-1], GDP = emissions$GDP[-1], perCapita = emissions$perCapita[-1])

lmfit6 <- lm(CO2 ~ GDP, emissions2)
library('broom')
tidy(lmfit6)

emissionspredict <- data.frame(GDP = 8083000, CO2=predict(lmfit6, data.frame(GDP = 8083000)), perCapita = NA)
dane = rbind(emissions2, emissionspredict)

ggplot() + geom_point(data=emissions, aes(GDP, CO2)) + geom_smooth(data=emissions, aes(GDP,CO2), method = 'lm', se = FALSE) + geom_smooth(data = dane, aes(GDP,CO2), method = 'lm', se = FALSE, color = 'red')


# 7
library('UsingR')
homeprice = data.frame(homeprice)
View(homeprice)
library('broom')
lmfit6 <- lm(sale ~ ., homeprice)
tidy(lmfit6)
lmfit7 <- lm(sale ~ . - 1, homeprice)
tidy(lmfit7)