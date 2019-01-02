library("dbplyr")
library("PogromcyDanych")
library("ggplot2")


# 1
cats_birds = data.frame(koty_ptaki)
View(cats_birds)

ggplot(cats_birds, aes(x = dlugosc, y = predkosc, color = druzyna)) + geom_point() +  geom_smooth(method = 'lm')


# 2
pearson = data.frame(pearson)
View(pearson)

ggplot(pearson, aes(x = ojciec, y = syn)) + geom_point() + geom_smooth(method = 'lm', se = FALSE, color = 'red')


# 3
serialeIMDB = data.frame(serialeIMDB)
View(serialeIMDB)

ggplot(serialeIMDB, aes(x = ocena, y = serial)) + geom_boxplot()
serialeIMDB$serial <- reorder(serialeIMDB$serial, serialeIMDB$ocena, median)
ggplot(serialeIMDB, aes(x = ocena, y = serial)) + geom_boxplot()


# 4
diagnoza = data.frame(diagnoza)
View(diagnoza)

ggplot(diagnoza, aes(x = eduk4_2013, fill = gp29)) + geom_bar()


# 5
auta2012 = data.frame(auta2012)
View(auta2012)

auta2012 %>% filter(Marka == 'Volkswagen', Model == 'Passat') -> Volkswagen.Passat
figure <- ggplot(Volkswagen.Passat, aes(x = Rok.produkcji, y = Cena.w.PLN)) + geom_smooth(method = 'lm')
figure
figure + theme_bw() 
library("ggthemes")
figure + theme_excel()
figure + theme_economist()


# 6
ggplot(cats_birds, aes(x = waga, y = predkosc, size = zywotnosc, color = zywotnosc)) + geom_point() + scale_color_gradient(low = 'green', high = 'red') + ggtitle('Zależność pomiędzy wagą i prędkością') + xlab('Waga [kg]') + ylab('Prędkość [km/h]') + scale_shape_manual(values = zywotnosc) + geom_point(shape=15)


# 7
auta2012 %>% filter(Marka == 'Toyota') %>% group_by(Model) %>% tally() %>% arrange(desc(n)) %>% top_n(5) -> wybrane
wybrane

ggplot(wybrane, aes(x="", y=n, fill=Model)) + geom_col(width=1) + coord_polar(theta = "y") + theme_void() + labs(title = "The most popular models of Toyota")


# 8
ggplot(pearson, aes(x = syn)) + geom_histogram(aes(y = ..density..), bins = 30, color = 'white') + geom_density() + labs(title = "Synowie")
ggplot(pearson, aes(x = ojciec)) + geom_histogram(aes(y = ..density..), bins = 30, color = 'white') + geom_density() + labs(title = "Ojcowie")