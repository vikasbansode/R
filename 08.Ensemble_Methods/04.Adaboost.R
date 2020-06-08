download.file("https://raw.githubusercontent.com/z-o-e/bank_data_analysis/master/bank-full.csv",destfile = "bank-full.csv")

install.packages("adabag")
library(adabag)

library(adabag);
adadata<-read.csv('bank-full.csv',header=TRUE,sep=";")
head(adadata)
adaboost<-boosting(y~age+job+marital+education+default+balance+
                     housing+loan+contact+day+month+duration+campaign+pdays+previous+
                     poutcome, data=adadata, boos=TRUE, mfinal=20,coeflearn='Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,adadata)
predict(adaboost,adadata)
t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)
