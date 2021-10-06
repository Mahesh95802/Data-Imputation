library(dplyr)
library(utils)
library(stringr)
library(stats)
library(VIM)
#library(bnstruct)
X <- as.data.frame(read.csv("https://raw.githubusercontent.com/Maheshwaran-k2019/DataImputation/main/weatherAUS.csv"))
cd <- c()
for(i in 1:length(X))
{
  NAcp <- sum(is.na(X[[i]]))/length(X[[i]])
  if(NAcp >= 0.6)
    cd <<- append(cd,i)
}
if(!is.null(cd))
{
  X <- subset(X,select = -cd)
}
X$na_count <- apply(X, 1, function(x) sum(is.na(x)))
X <- subset(X, na_count/(ncol(X)-1) < 0.6 )
X <- X[ , !(names(X) %in% c("na_count"))]
X[["Date"]] <- strptime(X[["Date"]],"%Y-%m-%d")
for(i in 1:length(X))
{
  if (is.numeric(X[[i]]))
  {
    var = readline(prompt = paste0("1-Mean 2-Median \nEnter The Method for ",dimnames(X)[[2]][i]," : "))
    if (var == 1)
      X[[i]][is.na(X[[i]])]<-as.numeric(sprintf(mean(X[[i]],na.rm=TRUE), fmt = '%#.2f'))
    else if (var == 2)
      X[[i]][is.na(X[[i]])]<-as.numeric(sprintf(median(X[[i]],na.rm=TRUE), fmt = '%#.2f'))
  }
}
