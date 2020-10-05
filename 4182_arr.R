Arr1 <- read.csv("Arr9_csv.csv")
head(Arr1)
library(ggplot2)
g <- ggplot(data=Arr1, aes(x=ï..Inv_T, y= LnK)) + geom_point()
g
g.lm = lm(LnK ~ ï..Inv_T, Arr1)
newdata = data.frame(ï..Inv_T=3.5568)
t <-predict(g.lm, newdata, response = 'SE', se.fit=T)
t
CI <- 2.04*0.0597
LCI <- 6.848473 - CI
HCI <- 6.848473 + CI
pred_time <- exp(c( 6.848473, LCI, HCI))
pred_time
pred2 <- predict.lm(g.lm, newdata, interval = "confidence")
pred2
pred_time2 <- exp(c(6.848473, 6.726539, 6.970406))
pred_time2
