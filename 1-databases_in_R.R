library('dbplyr')
library('PogromcyDanych')

# 1
auta2012=data.frame(auta2012)
View(auta2012)
auta2012 %>% sapply(class) %>% table()


# 2
auta2012 %>% group_by(Marka) %>% summarise(n = n()) %>% arrange(-n)

auta2012$Marka %>% table() %>% desc() %>% sort() %>% abs()
auta2012$Marka %>% table() %>% desc() %>% sort() %>% abs() %>% rownames()


# 3
auta2012 %>% filter(Rodzaj.paliwa == "benzyna") %>% nrow()/nrow(auta2012) * 100

auta2012 %>% group_by(Rodzaj.paliwa) %>% summarise(n = n()) %>% mutate(percent = n/sum(n) * 100) %>% arrange(-Rodzaj.paliwa)


# 4
auta2012 %>% filter(Cena.w.PLN<2000) %>% nrow()


# 5
auta2012 %>% filter(Pojemnosc.skokowa>=1500) %>% nrow()/nrow(auta2012) * 100


# 6
auta2012 %>% filter(Kraj.aktualnej.rejestracji == "Polska", Cena.w.PLN < 2000) %>% nrow()


# 7
auta2012 %>% filter(Pojemnosc.skokowa > 1500, Rodzaj.paliwa == "olej napedowy (diesel)") %>% nrow()/nrow(auta2012) * 100


# 8
library('MASS')
Cars93 = data.frame(Cars93)
View(Cars93)
cars <- filter(Cars93, Cars93$Type == "Sporty" | Cars93$Type == "Small")
nrow(cars)


# 9
cats_birds = data.frame(koty_ptaki)
View(cats_birds)
cats_birds %>% filter(dlugosc>1)


# 10
```{r}
cats_birds = data.frame(koty_ptaki)
View(cats_birds)
arrange(cats_birds, predkosc)


# 11
auta2012 %>% filter(Marka == 'Volkswagen') -> auta
auta$Rodzaj.paliwa %>% table()


# 12
auta2012 %>% filter(Marka == "Volkswagen") %>% summarise(mean.price = mean(Cena.w.PLN), mean.mileage = mean(Przebieg.w.km, na.rm = TRUE))


# 13
auta2012 %>% group_by(Marka) %>% summarise(mean.price=mean(Cena.w.PLN))


# 14
auta2012 %>% filter(Marka == 'Toyota', Model == 'Corolla') %>% summarise(kw1 = quantile(Cena.w.PLN, probs=c(0.25)), kw3 = quantile(Cena.w.PLN, probs=c(0.75)))


# 15
auta2012 %>% filter(Marka == 'Toyota') %>% group_by(Model) %>% summarise(mean.price=mean(Cena.w.PLN)) %>% arrange(-mean.price)


# 16
auta2012 %>% filter(Marka == 'Volkswagen', Model == 'Passat', Rok.produkcji == 2006) -> auta1 
auta1 %>% summarise(mean.price=mean(Cena.w.PLN))
auta1 %>% filter(Cena.w.PLN < 35000) %>% nrow()/nrow(auta1) * 100


# 17
auta2012 %>% filter(Rok.produkcji == 2007) %>% group_by(Marka) %>% summarise(count = n()) %>% arrange(count)