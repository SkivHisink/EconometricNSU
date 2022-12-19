setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages("modeldata")
df <- modeldata::lending_club
proportions(table(df$Class))*100
df$bad <- (df$Class=="bad")+0
mean(df$bad)
df$bad <- ifelse(df$Class=="bad", 1, 0)
mean(df$bad)

hist(df$funded_amnt, breaks = 100)
df$log_amnt <- log(df$funded_amnt)
hist(df$log_amnt, breaks = 100)

hist(df$annual_inc, breaks = 100)
summary(df$annual_inc)

df$log_inc <- log(pmax(df$annual_inc, 1000))
hist(df$log_inc, breaks = 100)
rug(df$log_inc)

table(df$term)

table(df$int_rate)
#mistake for int_rate
plot(proportions(table(df$int_rate)))
#second variant
hist(df$int_rate, breaks = 100)
#third variant
summary(df$int_rate)
#min 5 max 29 thats why we use this values in seq
hist(df$int_rate, breaks=seq(5,29,1))

table(df$sub_grade)
barplot(proportions(table(df$sub_grade))*100)

table(df$emp_length)
barplot(proportions(table(df$emp_length))*100)

df$term_num <- ifelse(df$term=="term_36", 36, 60)
# Transfer sub grade to grade in numeric format
df$grade <- as.numeric(df$sub_grade)
table(df$grade)

barplot(proportions(table(df$grade)))
#Decode empl len
df$empl <- c(0, 1, 10, 2:9, 11)[df$emp_length]

table(df$empl)
barplot(proportions(table(df$empl))*100)

#look at correlation table

round(cor(df[c("bad", "log_amnt", "term_num", "int_rate",  "grade", "log_inc", "empl")]), 3)
# doxod bad and empl bad

proportions(table(df$term, df$Class), margin = 1) * 100
plot(proportions(table(df$term, df$Class), margin = 1) * 100)

plot(proportions(table(df$sub_grade, df$Class), margin = 1) * 100)

plot(proportions(table(round(df$int_rate), df$Class), margin = 1) * 100)

plot(proportions(table(df$emp_length, df$Class), margin = 1) * 100)

#build model

lgt <- glm(bad ~ log_amnt + term_num + int_rate + grade + log_inc + empl,
           data = df, family = "binomial")
summary(lgt)
#log amnt - neznach
# term - znach
# grade - neznazh
# rate - znach
# doxod - znach
# eto ne tochno
# predict - lineyanaya combinatia obyasnyauoshix peremennix x_i*b
plot(df$bad ~ predict(lgt), pch="|")
grid()
#fitted raschetnie veroyatnosti s krishechkoy F(X, b) = p with ^
points(fitted(lgt) ~ predict(lgt))
abline(h = 0.5)
# perviy classs chobi bilo po strochkam
table(df$Class,fitted(lgt)>0.5)
# we can change rate
table(df$Class,fitted(lgt)>0.3)
prop <- proportions(table(df$Class, fitted(lgt) >0.1), margin = 1)
prop[,2]
points(prop[,2])

install.packages("pROC")
pROC::plot.roc(df$bad, fitted(lgt))
pROC::roc(df$bad, fitted(lgt))

n <- nrow(df)
set.seed(123)
train <- sample(1:n, 0.8*n)
test <- (1:n)[-train]

lgt1 <- glm(bad ~ 1, data=df, subset = train, family = "binomial")

summary(lgt1)
lgt2 <- glm(bad ~ log_inc, data=df, subset = train, family = "binomial")

summary(lgt2)
lgt3 <- glm(bad ~ log_amnt+term_num+log_inc+int_rate, data=df, subset = train, family = "binomial")

summary(lgt3)
lgt4 <- glm(bad ~ log_amnt+term_num+log_inc+int_rate+grade, data=df, subset = train, family = "binomial")

summary(lgt4)
#test - po strochkam
#pp - predicted predict
pp1 <- predict(lgt1, newdata = df[test,], type ="response")
pp2 <- predict(lgt2, newdata = df[test,], type ="response")
pp3 <- predict(lgt3, newdata = df[test,], type ="response")
pp4 <- predict(lgt4, newdata = df[test,], type ="response")

table(pp1)
table(pp4)

hist(pp4, breaks = 100)

table(df$Class[test], pp4>0.3)

pROC::plot.roc(df$bad[test], pp4)
pROC::roc(df$bad[test], pp4)

pROC::plot.roc(df$bad[test], pp1)
#ploshad pod krivoy
pROC::roc(df$bad[test], pp1)

pROC::plot.roc(df$bad[test], pp4, col = "green")
roc4<-pROC::roc(df$bad[test], pp4)
pROC::plot.roc(df$bad[test], pp3, add =1, col= "blue")
roc3<-pROC::roc(df$bad[test], pp3)
pROC::plot.roc(df$bad[test], pp2, add =1, col="red")
roc2<-pROC::roc(df$bad[test], pp2)
pROC::plot.roc(df$bad[test], pp1, add =1, col="purple")
roc1<-pROC::roc(df$bad[test], pp1)

c(auc1=roc1$auc,auc2=roc2$auc,auc3=roc3$auc,auc4=roc4$auc)

pp <- cbind(pp1, pp2, pp3, pp4)

#srednee po stolbcam
Brier <- colMeans((df$bad[test] - pp)^2)

sqrt(Brier) * 100
# First variant
LogS1 <- df$bad[test] * log(pp1) + (1 - df$bad[test]) * log(1 - pp1)
LogS2 <- df$bad[test] * log(pp2) + (1 - df$bad[test]) * log(1 - pp2)
LogS3 <- df$bad[test] * log(pp3) + (1 - df$bad[test]) * log(1 - pp3)
LogS4 <- df$bad[test] * log(pp4) + (1 - df$bad[test]) * log(1 - pp4)
colMeans(cbind(LogS1, LogS2, LogS3, LogS4))
#Second
LogS <- colMeans(df$bad[test] * log(pp) + (1 - df$bad[test]) * log(1 - pp))
LogS


BB <- (df$bad[test] - pp)^2
reg.DM <- lm(BB[,4] - BB[,2] ~ 1)
summary(reg.DM)