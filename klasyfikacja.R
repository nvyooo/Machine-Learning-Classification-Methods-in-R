#install.packages('factoextra')
library(factoextra)
#install.packages('lattice')
library(lattice)
install.packages('hardhat')
#install.packages('party') 
library(party)
install.packages("caret")
library(caret)
install.packages('kernlab')
library(kernlab)


set.seed(100)
dane<-read.csv("C:/Users/theal/Desktop/Projekt/predictive-maintenance-dataset.csv",sep=",")
dane<-dane[2:9] 

vib<-dane[,3]
dane<-dane[,-3]
dane<-cbind(vib,dane)
dane<-na.omit(dane)
dane$vib<-round(dane$vib)
dane$vib<-as.factor(dane$vib)
#Bierzemy jedynie 100 danych do dalszej analizy 

ind<-sample(2,nrow(dane),replace = TRUE,prob = c(0.05,0.95))

data_1<-dane[ind==1,]

ind2<-sample(2,nrow(data_1),replace = TRUE,prob = c(0.7,0.3))

data_train<-data_1[ind2==1,]
data_test<-data_1[ind2==2,]

myf<- vib ~ revolutions+humidity+x1+x2+x3+x4+x5

windy_ctree<-ctree(myf,data=data_train)

temp<-table(predict(windy_ctree),data_train$vib)
temp

#wizualizacja drzewa dla jakiej wartosci ile razy wystepuje 
plot(windy_ctree)
#spróbuje teraz przewidziec co algorytm drzewa zwroci dla danych testowych

test_pred<-predict(windy_ctree,newdata=data_test)
test_pred
table(test_pred,data_test$vib)

#jak można odczytać z tabeli tam gdzie wyniki się pokrywają program poprawnie przewidział jakie wibracje
#ma winda w porównaniu do innych parametrów


#sprobuje innej metody przewidywania

model<-train(vib ~.,data=data_train,
             method="svmPoly",
             na.action = na.omit,
             preProcess= c("scale","center"),
             trControl=trainControl(method="none"),
             tuneGrid=data.frame(degree=1,scale=1,C=1))

#buduje model cv

model_cv<-train(vib ~.,data=data_train,
             method="svmPoly",
             na.action = na.omit,
             preProcess= c("scale","center"),
             trControl=trainControl(method="cv",number = 10),
                        tuneGrid=data.frame(degree=1,scale=1,C=1))

#przewiduje
model.training<-predict(model,data_train) #dane traningowe

model.training

model.testing<-predict(model,data_test) #dane testowe


model.cv<-predict(model_cv,data_train) #robie spardzenie krzyżowe



#macierz konfuzji
model.training.conf<-confusionMatrix(model.training,data_train$vib)
model.testing.conf<-confusionMatrix(model.testing,data_test$vib)
model.cv.conf<-confusionMatrix(model.cv,data_train$vib)


print(model.training.conf)
print(model.testing.conf)
print(model.cv.conf)



#wizualizacja które parametry mają wpływ na dany wynik
Importance<-varImp(model)
plot(Importance,col="red")





