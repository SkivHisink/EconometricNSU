#Ссылка для скачивания данных
#Для рассчёта был Взят мексиканский песо
#https://www.imf.org/external/np/fin/ert/GUI/Pages/Report.aspx?CU=%27MXN%27&EX=REP&P=DateRange&Fr=628929792000000000&To=638051904000000000&CF=Compressed&CUF=Period&DS=Ascending&DT=Blank

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read.delim(file = "datatask11.csv", skip = 2, sep = ",",
stringsAsFactors = TRUE) # читаем данные, скипаем шапку

names(df) <- c("Date", "MXN")
df$MXN <- as.numeric(df$MXN)
df$Date <- as.Date(df$Date, "%Y-%m-%d")
# Добавим логарифм наших данных
df$log <- log(df$MXN)

