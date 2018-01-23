Zajęcia 6 - LASY LOSOWE

1. Wczytywanie i eksploracja danych
data <- read_csv2("data/income.csv", na = c("?", ""))
str(data)
summary(data)
apply(data, 2, unique)
data <- na.omit(data)
data <- select(data, age, workclass,education, income, maritalStatus, occupation, capitalGain,
               capitalLoss, hourPerWeek, nativeCountry, race,sex, relationship)
if (any(data$income == "<=50K")) data$income <- ifelse(data$income == "<=50K", "low", "high")

table(data$income)

data <- mutate(data, 
               income = factor(income),
               workclass = factor(workclass),
               education = factor(education), 
               maritalStatus = factor(maritalStatus),
               occupation = factor(occupation),
               nativeCountry = factor(nativeCountry), 
               race = factor(race),
               sex = factor(sex), 
               relationship = factor(relationship)
               )
               
2. Podział zbioru na treningowy i uczący się
train_dataset_proportion <- 0.8
train_index <- (runif(nrow(data), 0, 1) <= train_dataset_proportion)

train <- data[train_index, ]
test <- data[!train_index, ]

3. Budowa lasu losowego
rf <- randomForest(income ~., data = train)

# Uzyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego prĂłbkowania jako kandydaci przy kaĹĽdym podziale

varImpPlot(rf)

4. Ocena modelu
funkcja jak z zajec5 zad.1 

5. wykresy diagnostyczne

forecast <- predict(rf, newdata = test, type = "prob")[,2]
plottingData <- prediction(forecast, test$income)

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(plottingData,"tpr","fpr"),lwd=2, colorize=T) 

#AUC (Area Under Curve) - pole pod krzywa ROC
performance(plottingData,"auc")@y.values[[1]]
# skladnia obiektowa: @ zamiast $, [[1]] do wyciagniecia elementu z listy
# Sensitivity/specificity plots ~ trade-off
plot(performance(plottingData ,"sens","spec"),lwd=2) 

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 


ZADANIE 1
Uzupelnij skrypt z cwiczen 5 (przewidywanie jakosci bialego wina na podstawie jego
parametrow chemicznych) o model lasu losowego. Porownaj rozne metryki jakosci
tego modelu do analogicznych metryk jakosci modeli uzywanych na cwiczeniach 5.


dane <- read.csv2('https://raw.githubusercontent.com/kaftanowicz/sgh_ird_2017/bae80011e646b910bab577b87019edc2aad67379/data/winequality-white.csv',  
stringsAsFactors = FALSE, dec = '.')
set.seed(1)
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]
rf_reg <- randomForest(quality ~., data = train)

a) ocena lasu losowego regresji

varImpPlot(rf_reg)
OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("Suma kwadatow reszt RSS")
  print(sapply(modele, function(x) sum((dane[[predicted_col_name]] - predict(x, dane))^2) ))
  
  print("Ĺšredni bĹ‚Ä…d absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
  
  print("Pierwiastek bĹ‚Ä™du Ĺ›redniokwadratowego RMSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) ))
  
  print("WzglÄ™dny bĹ‚Ä…d absolutny RAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/sum(abs(dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))) ))
  
  print("Pierwiastek WzglÄ™dnego bĹ‚Ä™du Ĺ›redniokw RRSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/sum((dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))^2)) ))
  
}

OcenaModeli(list(rf_reg), train, 'quality')
OcenaModeli(list(rf_reg), test, 'quality')

b) klasyfikacja - czynnikowa, dwuwartościowa zmienna przewidywana

dane <- read.csv2('https://raw.githubusercontent.com/kaftanowicz/sgh_ird_2017/bae80011e646b910bab577b87019edc2aad67379/data/winequality-white.csv',
stringsAsFactors = FALSE, dec = '.')

 * zmiana jakości na zmienna o 2 wartościach
 dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')

* Zamiana jakosci na zmienna typu czynnikowego
dane$quality <- as.factor(dane$quality)

set.seed(1)
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

rf_class <- randomForest(quality ~., data = train)
cm <- table(predict(rf_class, new = test, type = "class"), test$quality)

EvaluateModel <- function(classif_mx)
{  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  condition_positive <- sum(classif_mx[ ,1])
  condition_negative <- sum(classif_mx[ ,2])
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  return(list(accuracy = accuracy, 
              sensitivity = sensitivity,
              specificity = specificity))}
              
  EvaluateModel(cm)

library(ROCR) # do krzywej ROC
prognoza_ciagla <- predict(rf_class, newdata = test, type = "prob")
# trzeba podac type = "prob", bo dla lasu losowego domyslnie zwracal przewidywana klase
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])
# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(prediction(prognoza_ciagla,test$quality),"tpr","fpr"),lwd=2, colorize=T) 
# AUC (Area Under Curve) - pole pod krzywa ROC
performance(prediction(prognoza_ciagla, test$quality),"auc")
# Sensitivity/specificity plots ~ trade-off
plot(performance(prediction(prognoza_ciagla,test$quality),"sens","spec"),lwd=2) 
# Lift chart
plot(performance(prediction(prognoza_ciagla,test$quality),"lift","rpp"),lwd=2, col = "darkblue") 
#Lift is a measure of the effectiveness of a predictive model calculated 
#as the ratio between the results obtained with and without the predictive model.        


ZADANIE 2

Uzywajac pakietu caret, zbuduj model drzewa klasyfikacyjnego 
# do przewidywania jakosci czerwonego wina. 
# Uzyj 3-krotnej walidacji krzyzowej. Pokaz jego metryki dla zbioru uczacego i testowego.
# Zbuduj kolejne drzewo, tym razem uzywajac 10-krotnej walidacji krzyzowej.
# Porownaj jego metryki (dla zbioru uczacego i testowego) z metrykami poprzedniego drzewa.

a)'
dane <- read.csv2('https://raw.githubusercontent.com/kaftanowicz/sgh_ird_2017/bae80011e646b910bab577b87019edc2aad67379/data/winequality-red.csv',  
stringsAsFactors = FALSE, dec = '.')
if (typeof(dane$quality) == "integer") dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')
## Uzycie warunku na typ zmiennej zabezpiecza nas przed zepsuciem danych
## w przypadku przypadkowego wywolania tej linijki wiecej niz 1 raz.
## Nie jest konieczne, ale czyni kod bezpieczniejszym.

library(caret)
set.seed(1)

b) podział na zbiór testowy i uczący się
inTraining <- createDataPartition(dane$quality, p = .8, list = FALSE)
training <- dane[ inTraining,]
training  <- na.omit(training)
testing  <- dane[-inTraining,]

c) 3krotna walidacja krzyzowa
fitControl_3 <- trainControl(
  method = "cv",
  number = 3)

treeCaret_3 <- train(quality ~ ., data = training, 
                          method = "rpart", 
                          trControl = fitControl_3)

plot(treeCaret_3)
rpart.plot(treeCaret_3$finalModel)

d) ewaluacja

confusionMatrix(data = predict(treeCaret_3, training), reference = training$quality, mode = "everything")
confusionMatrix(data = predict(treeCaret_3, testing), reference = testing$quality, mode = "everything")

e) 10krotna walidacja

fitControl_10 <- trainControl(
  method = "cv",
  number = 10)

treeCaret_10 <- train(quality ~ ., data = training, 
                     method = "rpart", 
                     trControl = fitControl_10)

plot(treeCaret_10)
rpart.plot(treeCaret_10$finalModel)

f) ewaluacja
confusionMatrix(data = predict(treeCaret_10, training), reference = training$quality, mode = "everything")
confusionMatrix(data = predict(treeCaret_10, testing), reference = testing$quality, mode = "everything")







