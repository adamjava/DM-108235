#target variable: default12


setwd("G:/code r/uksw eksploracja danych")
library(dplyr)
df <- read.csv("abt_app.csv",sep="\t")

#"wizualne" przeglądanie danych
View(df)
summary(df)
str(df)
dim(df) #11437   219

#wyświetlenie zmiennych wczytanych jako "factor"
head(df[,sapply(df, function(x)class(x)=="factor")])
#aid i cross_aid nie wyglądają na factory tylko na identyfiaktory tekstowe, więc zamieniam je na tekst 
#df %>% select(aid, cross_aid) %>% mutate_if(is.factor, as.character)  
df$aid <- as.character(df$aid)
df$cross_aid <- as.character(df$cross_aid)
class(df$aid)
class(df$cross_aid)
#tak czy inaczej są to zmienne nie wnoszące nic do analizy, więc można je usunąć
ncol(df)
head(df[,!grepl("aid",names(df))])
df <- df[,!grepl("aid",names(df))]
ncol(df) 
any(grepl("aid",names(df)))

ncases <- nrow(df)
#colnames(df)
#typy kolumn różne od liczbowych?
func.is.numeric <- function(x){class(x)!="numeric"}
columns.non.numeric.indexes <- sapply(df, func.is.numeric)
ncol(df[,columns.non.numeric.indexes]) #13
head(df[,columns.non.numeric.indexes])
columns.non.numeric.names <- names(df[,columns.non.numeric.indexes])
columns.non.numeric.names

##CZYSZCZENIE


#app_char sprawdzić czy dobrze wpisane wszystkie zmienne
#head(df)
cols.app_char <- df[,grepl("app_char",names(df))]
summary(cols.app_char)
#app_char_home_status with parents - tylko 0
table(df$app_char_home_status)
ncol(df)
#app_char_branch - jedna zmienna
df$app_char_branch <- NULL


#usuwam kolumny mające tylko 1 wartość zmiennej
ncol(df)
func.one.value <- function(x){
  return(length(unique(x))==1)
}
sum(sapply(df, func.one.value)) #7 czyli mamy 7 kolumn zawierającą tylko jedną wartość
cols.one.value <- sapply(df, func.one.value)
df <- df[,!cols.one.value]
ncol(df)

#usuwam kolumny podobne do zmiennej celu
drop <- paste("default",c(3,6,9),sep="")
drop
ncol(df[,names(df)%in%drop])
df <- df[,!names(df)%in%drop]
ncol(df)


#szukam kolumn zawierających dużo NA
func.many.na <- function(x){
  return(sum(is.na(x)))
}
howmany.na <- sapply(df,func.many.na)
hist(howmany.na/ncases, breaks=20)
rug(howmany.na/ncases)
abline(v=0.3, col="red")

#usuwam kolumny mające więcej niż 30% NAs
drop2 <- howmany.na/ncases > 0.3
sum(drop2)
df <- df[,!drop2]
ncol(df)

#sprawidzić czy są wiersze z dużą ilośćią NA
func.sum.na.row <- function(x){
  return(sum(is.na(x)))
}
sum.na.row<-apply(df,1,func.sum.na.row)
hist(sum.na.row)
#ile rzędów ma więcej niż 1/3 NA
sum(sum.na.row > ncol(df)/3) #433
drops <- sum.na.row > ncol(df)/3
nrow(df)
#ile rzędów ma więcej niż 0.75 NA
sum(sum.na.row > ncol(df)*0.75) #0
#usuwam rzędy mające więcej niż 1/3 NA
#nrow(df[!drops,])
df <- df[!drops,]
nrow(df)

#we wszystkich numerycznyc kolumnach imputuj medianę
n<-ncol(df)
sum(is.na(df)) #[1] 95062
for(i in 1:n){
  if(class(df[,i])=="numeric"){
    med<-median(df[,i],na.rm=T)
    nas<-is.na(df[,i])
    df[,i][nas] <- med
  }
}

sum(is.na(df))
dim(df)
#View(df)



##ograniczam ramkę do 500 rzędów, bo za nic w świecie nie mogę tego policzyć na swoim laptopiku
#te 500 rzędów to będzie zbiór uczący
max <- 500
randomRows <- sample(1:nrow(df), max)

#feature selection

df3 <- df[randomRows,]
dim(df3)
library(Boruta)
classifBoruta <- Boruta(default12~., data = df3) #długo się liczy
classifBoruta

#Boruta wybrał 30-40 cechy, które są istotne dla budowy modelu
df3.confirmed <- df3[classifBoruta$finalDecision=="Confirmed"]
df3.confirmed$default12 <- df3$default12
dim(df3.confirmed)

#budowa modelu przy użyciu lasu losowego
class.randmoforest <- randomForest(default12~., data = df3.confirmed, importance=T)

#zbiór testowy
df3.test <- df[-randomRows,]
dim(df3.test)
randomRowsTest <- sample(1:nrow(df3.test),max)
df3.test <- df3.test[randomRowsTest,]
dim(df3.test)

#testowanie modelu
predictions <- as.character(predict(class.randmoforest, df3.test, type="class"))
predictions[predictions<=0.5] <- 0
predictions[predictions>0.5] <- 1

#stopień 
correlation <- sum(predictions == df3.test$default12) #369 w moim przypadku
correlation
correlation/nrow(df3.test) #0.738 czyli w 74% dostałem prawidłową predykcję, pewnie gdyby mój komputer był w stanie policzyć Borutę dla większej liczby danych byłoby lepiej








