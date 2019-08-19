#These lines import data which was retrieved from Yahoo Finance and assigns new column names.  The data being examined is the daily VIX price and the daily percent change in SPY.
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/kevmullen/VIX-and-SPY/master/vi%20spy%20and%20return.csv")
vi_spy_and_return <- read.csv(text = x)
vi_spy_and_return$v<-vi_spy_and_return$Open.vix
vi_spy_and_return$r<-vi_spy_and_return$return


#Here I reorganized the data into two columns.  The first column are integers of vix and the second is the rate of return.  I did this by rounding the vix prices to their closest integer.
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

#This plot shows histograms overlayed by density plots of the rate of returns for each integer of vix between 9 and 50.  In each historgram, the mean rate of return is shown by a red bar in the graph.
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
  


#This graph shows a box plot of the daiily VIX price vs the daily rate of return of SPY.
ggplot(vi_spy_and_return,aes(v,r)) + geom_boxplot(aes(group = cut_width(v, 1))) + xlim(9,20) + ylim(-3,3)
