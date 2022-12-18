setwd(dir = "C:/Users/bazof/Desktop/�����������/��1")

n = 100

e1 = rnorm(n, mean = 0, sd = 2)
e2 = rnorm(n, mean = 0, sd = 3)

x = 10+(11*e1)-(12*e2)
y = -3+(4*e1)+(5*e2) 

table1 = matrix(nrow = 2, ncol = 3)
rownames(table1) = c("x","y")
colnames(table1) = c("mean","var", 'sd')
table1[1,1] = mean(x)
table1[1,2] = var(x)
table1[1,3] = sd(x)
table1[2,1] = mean(y)
table1[2,2] = var(y)
table1[2,3] = sd(y)
table1

cor(x,y)

cor.test(x,y)

varx = var(x)*(n-1)/n
vary = var(y)*(n-1)/n
t.test((x-mean(x))*(y-mean(y))/(varx*vary)^(1/2) + 0.507, conf.level = 0.99)
rm(varx,vary)

m1 = lm(y ~ x)
summary(m1)
plot(x ~ y)

m2 = lm(y ~ x + I(x^2))
summary(m2)
plot(x ~ y) 
