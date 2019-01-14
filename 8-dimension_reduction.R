# 1
library(magrittr)
library(dplyr)
library(ggplot2)
library(MASS)
painters = data.frame(painters)
dane <- painters[,1:4]
diag(var(dane))
model.pca <- prcomp(dane)
plot(model.pca)
summary(model.pca)

cbind(data.frame(model.pca$x), School = painters$School) %>% 
  as_tibble() %>% 
  ggplot(aes(x = PC1, y = PC2, col = School, label = rownames(.))) + 
  geom_text()


# 2
library(magrittr)
library(dplyr)
library(ggplot2)
library(MASS)
Cars93 = data.frame(Cars93)
dane <- Cars93[-c(19,57), c(4:8, 12:15, 17:23, 23)]
ds.Origin <- tibble(Origin = Cars93$Origin[-c(19, 57)])
ds.Type <- tibble(Type = Cars93$Type[-c(19, 57)])
ds.Model <- tibble(Model = Cars93$Model[-c(19, 57)])

prcomp(dane, scale = TRUE)$x %>% 
  as_tibble() %>% select(PC1, PC2) %>% 
  bind_cols(ds.Origin, ds.Type, ds.Model) -> new.data

new.data %>% 
  ggplot(aes(x = PC1, y = PC2, col = Origin, label = Model)) + 
  geom_text() -> p1

new.data %>% 
  ggplot(aes(x = PC1, y = PC2, col = Type, label = Model)) + 
  geom_text() -> p2

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


# 3
swiss = data.frame(swiss)
library(ggplot2)
library(dplyr)
library(stats)
cmdscale(dist(swiss)) %>% 
  as_tibble() %>% 
  ggplot(aes(x = V1, y = -V2, label = rownames(swiss))) + 
  geom_text() +
  labs(x = 'MDS1', y = 'MDS2', title = 'Skalowanie metryczne')


# 4

# Tablica kontygnencji
# Analiza korespondencji

library(factoextra)
library(ca)
library(dplyr)
housetasks = data.frame(housetasks)
model.ca <- ca(housetasks)
model.ca$rowcoord %>% 
  as_tibble() %>% 
  ggplot(aes(x = Dim1, y = Dim2, label = rownames(housetasks))) + 
  geom_text(col = 'red') + 
  geom_text(data = as_tibble(model.ca$colcoord), aes(label = colnames(housetasks)), col = 'blue')