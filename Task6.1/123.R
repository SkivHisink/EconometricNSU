a = DataSamples[12]
1+2
class(1)
class("hell")
as.Date("2022-09-18")
x = 1
x <<- 1
# привет
aa <- list(1, "abc", list(1, 2, 3))
print(aa)
#View(aa)
#str(aa)
1:5
class(1:5)
x <- c(1, 2, c(6, 5, 4), 0:10)
rep(1, 10)
x[2]
x[2:5]
x[-2]
x[-2]
x[-(2:5)]
x[1]
x[length(x)]
x[c(2, 10)]
x[-c(2,10)]
x>5
x[x>5]
x+x^2 == 1
y<- x
y[-(2:5)] <- NA
y
y[x>5] <- NA
y
aa[1]
aa[[1]]
aa[2:3]
aa[2:3][1]
#Матрицы
matrix(x, ncol = 2)
A <- matrix(x, ncol = 2, byrow = T)
str(A)
t(A)
cbind(x)
cbind(x, y)
#View(cbind(x))
rbind(x)
rbind(x,y)
length(x)
length(y)
rbind(x, y)
# %*%
Mat = rbind(x,y)%*%cbind(x,y)
Mat[Mat == NA] = 0
#Frame 
df <- data.frame(
  id = 1:5,
  name = c("john", "mahmed", "mahmut", "algambek", "nokambek"),
  date = as.Date(c("2000-02-02", "2000-09-11", "2001-09-11", "2002-09-11", "2003-09-11"))
)
df
print(df)
class(df)
#View(df)
str(df)
head(df, 1)
tail(df, 1)

df[2:3,]

df[2]
df$id
names(df) <- c("idd", "namme", "datee")
df
names(df) <- c("id", "name", "date")
