setwd("C:/Users/rufus.turner/Documents/RStudio/R/ASLT")
Arr1 <- read.csv("arrb_csv.csv")
head(Arr1)
library(ggplot2)
g <- ggplot(data=Arr1, aes(x=ï..inv_T, y= Ln_k)) + geom_point()
g
g.lm = lm(Ln_k ~ ï..inv_T, Arr1)
g.lm
newdata = data.frame(ï..inv_T=3.5568)
t <-predict(g.lm, newdata, response = 'SE', se.fit=T)
t
CI <- 2.54*0.2525
LCI <- 7.7060002 - CI
HCI <- 7.7060002 + CI
pred_time <- exp(c( 7.7060002, LCI, HCI))
pred_time
pred2 <- predict.lm(g.lm, newdata, interval = "confidence")
pred2
pred_time2 <- exp(c(7.70626, 6.80514, 8.978261))
pred_time2
