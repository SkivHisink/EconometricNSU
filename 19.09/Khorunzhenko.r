setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#View(Dow_Jones)
df <-Dow_Jones
Dow_Jones <- 0
df$`Year Close` <-as.numeric(df$`Year Close`)
df$`Year Open` <-as.numeric(df$`Year Open`)
# Watch on data
print(df)
head(df, 5)
df <- df[108:1,]
djia <- df$`Year Close`
names(djia) <- df$Year
plot(djia ~ names(djia), type="b", pch="*", cex = 2, log="y", xlab= "text")
grid()
summary(dija)
r = diff(log(djia))*100
#View(df)
names(r) = df$Year
plot(r ~ names(r), type = 'l', xlab="year")
points(r ~ names(r), pch=20, cex = 0.8, col='blue')
abline(h=0, col = 'red', lty=3, lwd = 2)
summary(r)
#install.packages(psych)
psych::describe(r)

