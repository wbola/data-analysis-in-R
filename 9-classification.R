# 1
library(caret)
ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid')

library(MASS)
painters = data.frame(painters)

model.lda <- train(School ~., data = painters, method = 'lda', trControl = ctrl.loo)
1 - model.lda$results[2] 
mean(predict(model.lda) != painters$School)
confusionMatrix(predict(model.lda), painters$School)


# 2
library(caret)
ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid')

library(DAAG)
leafshape = data.frame(leafshape)

model.lda <- train(location ~ bladewid + bladelen + petiole, 
                    data = leafshape, 
                    method = 'lda', 
                    trControl = ctrl.loo)
1 - model.lda$results[2] 
mean(predict(model.lda) != leafshape$location)

model.qda <- train(location ~ bladewid + bladelen + petiole, 
                    data = leafshape, 
                    method = 'qda',
                    trControl = ctrl.loo)
1 - model.qda$results[2] 
mean(predict(model.qda) != leafshape$location)

# Wybieramy prostsz¹ metodê, czyli LDA


# 3
library(caret)
# nb - Naiwny Bayes

depresja <- c(6,4,0,4,0,11,11,5,8,4,12,8,9,8,11)
niepokoj <- c(8,3,2,1,8,9,6,7,6,9,11,8,6,10,4)
chaos <- c(9,3,8,6,4,8,6,4,5,4,6,5,7,8,3)
group <- factor(rep(1:3, each = 5))
data.set <- data.frame(depresja, niepokoj, chaos, group)

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid')
                         
model.lda <- train(group ~., 
                    data = data.set, 
                    method = 'lda',
                    trControl = ctrl.loo)
1 - model.lda$results[2] 

model.qda <- train(group ~., 
                    data = data.set, 
                    method = 'qda',
                    trControl = ctrl.loo)
1 - model.qda$results[2] 

model.nb <- train(group ~., 
                    data = data.set, 
                    method = 'nb',
                    trControl = ctrl.loo,
                    tuneGrid = data.frame(usekernel = FALSE, fL = 0, adjust = 1))
1 - model.nb$results[4] 

# Najlepsza QDA


# 4
data.set <- read.table('http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data', header = TRUE, row.names = 1, sep = ',')

ctrl.boot <- trainControl(method = 'boot', number = 100, search = 'grid')

model.1NN <- train(famhist~., data = data.set, method = 'knn', tuneGrid = data.frame(k = 1), trControl = ctrl.boot)
1 - model.1NN$results[2]
mean(predict(model.1NN) != data.set$famhist)

model.rf <- train(famhist~., data=data.set, method ='rf', trControl = ctrl.boot, tuneGrid = data.frame(mtry = 2))
1 - model.rf$results[2]
mean(predict(model.rf) != data.set$famhist)

model.rf$finalModel$confusion