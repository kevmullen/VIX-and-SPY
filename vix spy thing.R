library(readxl)
vi_spy_and_return <- read_excel("~/Desktop/vi spy and return.xlsx")
vi_spy_and_return$v<-vi_spy_and_return$`Open vix`
vi_spy_and_return$r<-vi_spy_and_return$return
library(ggplot2)
vix <- c(1:6449)
rr <- c(1:6449)
df <- data.frame(vix,rr)
k <- 1
for (i in 0:50){
  for (j in 1:6439){
   if (vi_spy_and_return$v[j] <= i + .5 && vi_spy_and_return$v[j] >= i - .5) {
     df$vix[k] <- i
     df$rr[k] <- vi_spy_and_return$r[j]
     k <- k+1
   }
  }
}
ggplot(df,aes(rr)) + geom_density(kernel = 'biweight') + facet_wrap(~ vix,scales = "free")

ggplot(df, aes(rr)) + 
  geom_histogram(aes(y=..density..),bins = 15, colour="black", fill="white")+
  geom_density(kernel = 'biweight',fill="#FF6666",alpha=.2) + 
  facet_wrap(~ vix, scales = "free") + 
  geom_vline(aes(xintercept = mean(rr)),col='red',size=.5)

m<- c()
n<- c()
for (i in 9:30) {
  m <- c(m,mean(df[(df$vix == i),'rr']))
  n <- c(n,i)
}
df3 <- data.frame(n,m)
  



ggplot(vi_spy_and_return,aes(v,r)) + geom_boxplot(aes(group = cut_width(v, 1))) + xlim(9,20) + ylim(-3,3)
