# 1
set.seed(123)
hist(rnorm(100, mean = 0, sd = 1))
hist(rnorm(100, mean = 0, sd = 1))


# 2
set.seed(123)
wektor <- rnorm(100, mean = 100, sd = 10)
srednia = mean(wektor)
odchylenie = sd(wektor)
length(wektor[wektor < srednia + 2*odchylenie & wektor > srednia - 2*odchylenie])/length(wektor)
length(wektor[wektor < 100 + 2*10 & wektor > 100 - 2*10])/length(wektor)


# 3
set.seed(123)
wektor1 <- rnorm(200, mean = 0, sd = 1)
srednia1 = mean(wektor1)
odchylenie1 = sd(wektor1)
length(wektor1[wektor1 < srednia1 + odchylenie1 & wektor1 > srednia1 - odchylenie1])/length(wektor1)
length(wektor1[wektor1 < srednia1 + 2*odchylenie1 & wektor1 > srednia1 - 2*odchylenie1])/length(wektor1)
length(wektor1[wektor1 < srednia1 + 3*odchylenie1 & wektor1 > srednia1 - 3*odchylenie1])/length(wektor1)


# 4
symul <- function(x) { 
  result=c()
  for(i in 1:x) {
    result[i] <- sum(sample(1:6, 3, replace = TRUE)) 
  }
  hist(result)
}

set.seed(123)
symul(1000)


# 5
zarowka <- function() {
  sample(c(1, 0), 500, prob = c(0.99, 0.01), replace = TRUE)
}
#set.seed(123)
srednia=c()
war=c()
for(i in 1:100){
  zar=zarowka()
  srednia[i] = mean(zar)
  war[i] = var(zar) }
mean(srednia)
mean(war)


# 6
wektor = rnorm(50, mean = 4.8, sd = 0.4)
length(wektor[wektor>mean(wektor)+0.8 | wektor<mean(wektor) -0.8])/length(wektor)

50*(2-2*pnorm(abs(4-4.8)/0.4))
50*(2-2*pnorm(abs(6-4.8)/0.4))

# Stosując kryterium Chauveneta odrzucamy obserwację o wartości 4, natomiast o wartości 6 przyjmujemy.

# 7
wyniki = c(12,34,22,14,22,17,24,22,18,14,18,12)
library('outliers')
dixon.test(wyniki)
grubbs.test(wyniki)
length(wyniki) * (2 - 2 * pnorm(abs(outlier(wyniki) - mean(wyniki)) / sd(wyniki)))

# Wynik o wielkości 34 należy uznać za odstający, ponieważ 0.2059207 < 1/2