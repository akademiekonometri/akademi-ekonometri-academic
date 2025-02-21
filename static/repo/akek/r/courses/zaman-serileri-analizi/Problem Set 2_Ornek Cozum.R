#===============================================================================
# Dr. Omer Kara
# Faculty Member, Ph.D.
# Department of Economics | School of Economics and Business Administration
# Eskisehir Osmangazi University | Eskisehir, Turkey 26480
# Mobile: +90 (539) 794-0221 | omer.kara.ylsy@gmail.com
#===============================================================================

#========================== Zaman Serileri Analizi =============================
#================================= Set 2 =======================================
#============================== Problem Set 2 ==================================
#============================== Ornek Cozumler =================================

# Devtools ve okara paketlerinin yüklenmesi.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(install.packages("devtools")))}
suppressWarnings(suppressMessages(library("devtools"))) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
if("okara" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara", force = FALSE)))}
suppressWarnings(suppressMessages(library("okara"))) ## okara paketi.

# Gerekli paketlerin yüklenmesi.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "lubridate", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge"))

# TSDL Data Sorusu
data.ts <- tsdl[[56]] ## Sıra numarası kullanarak datayı yüklüyoruz. Bu ornekte 56. siradaki dataya bakacagiz. Bu datanın zaman serisi objesi olduğuna dikkat edin.
head(data.ts, 30) ## Datanın yapısını inceleyebiliriz.
frequency(data.ts) ## Datanin frekansini buluyoruz.

# World Bank Data Sorusu
data <- WDI(country = c("BE"), indicator = c("PA.NUS.FCRF"), start = 1990, end = 2008, extra = FALSE) ## Indikatör ismini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz. Bu ornekte ulke: BE, indikator: PA.NUS.FCRF, baslangic yili: 1990, ve bitis yili: 2008.
data ## Datanın yapısını inceleyelim.
data <- data[, c("year", "PA.NUS.FCRF")] ## İstediğimiz değişkenleri belirtiyoruz (sadece year ve sorudaki indikator ismini kullanin).
colnames(data) <- c("Year", "Zaman.Serisi") ## Değişkenlere yeni isimler veriyoruz (indikator ismini Zaman.Serisi olarak degistiriyoruz).
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "Zaman.Serisi")] ## Değişkenleri sütun olarak sıralıyoruz (indikator ismini Zaman.Serisi olarak degistirmistik).
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.
data$Zaman.Serisi[18]

# Yahoo Data Sorusu
loadSymbols(Symbols = "AMZN", periodicity = "daily", return.class = "data.frame") ## Yahoo Finance uzerinden buldugumuz sembolu kullanarak ve datanın frekansını seçerek datayı indirip yükleyebiliriz. İndirdiğimiz datanın ismi sembol ismi ile aynı olacaktır. Bu ornekte sembol: AMZN, frekans: gunluk oldugu icin "daily" kullandik.
AMZN ## Datanın yapısını inceleyelim.
data <- data.frame(Date = gsub("\\.", "-", gsub("X", "", rownames(AMZN))), Zaman.Serisi = AMZN$AMZN.Open, stringsAsFactors = FALSE) ## Bu ornekte acilis fiyatlari (Open) secilmis. Yukaridaki kodu sembole ismine ve kullanilmak istenen fiyat turune gore modifiye ediyoruz. Ayrica, kullanmak istedigimiz fiyat turune Zaman.Serisi adini veriyoruz.
data$Date <- as.Date(data$Date) ## Date kategori olarak değiştiriliyor.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.
data[data$Zaman.Serisi == 10.8420000076294, ]$Date ## Belirtilen degerin hangi gun oldugunu buluyoruz. Soruda size verilen kod bir sonuc cikarmayabilir. Bu durumda asagidaki kodu kullanin.
data[round(data$Zaman.Serisi, 13) == 10.8420000076294, ]$Date ## Soruda verilen rakamin kac basamak ondaligi varsa (ornekte 13) ona gore kodu degistirin.

# TSDL Grafik Sorusu
data.ts <- tsdl[[29]] ## Sıra numarası kullanarak datayı yüklüyoruz. Bu ornekte 29. siradaki dataya bakacagiz. Bu datanın zaman serisi objesi olduğuna dikkat edin.
head(data.ts, 30) ## Datanın yapısını inceleyebiliriz.
plot(data.ts) ## Elimizdeki veri zaman serisi objesi olduğu için plot fonksiyonunu kullanarak direkt olarak grafiğe dökebiliriz.

# World Bank Grafik Sorusu
data <- WDI(country = c("BE"), indicator = c("NY.GDP.PCAP.PP.KD"), start = 1966, end = 2012, extra = FALSE) ## Indikatör ismini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz. Bu ornekte ulke: BE, indikator: NY.GDP.PCAP.PP.KD, baslangic yili: 1966, ve bitis yili: 2012.
data ## Datanın yapısını inceleyelim.
data <- data[, c("year", "NY.GDP.PCAP.PP.KD")] ## İstediğimiz değişkenleri belirtiyoruz (sadece year ve sorudaki indikator ismini kullanin).
colnames(data) <- c("Year", "Zaman.Serisi") ## Değişkenlere yeni isimler veriyoruz (indikator ismini Zaman.Serisi olarak degistiriyoruz).
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "Zaman.Serisi")] ## Değişkenleri sütun olarak sıralıyoruz (indikator ismini Zaman.Serisi olarak degistirmistik).
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

temp <- data
variable <- "Zaman.Serisi" ## Zaman.Serisi olarak degistirmeyi unutmayin.
variable.name <- "Zaman.Serisi" ## Zaman.Serisi olarak degistirmeyi unutmayin.

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")

# Yahoo Grafik Sorusu
loadSymbols(Symbols = "TSLA", periodicity = "daily", return.class = "data.frame") ## Yahoo Finance uzerinden buldugumuz sembolu kullanarak ve datanın frekansını seçerek datayı indirip yükleyebiliriz. İndirdiğimiz datanın ismi sembol ismi ile aynı olacaktır. Bu ornekte sembol: TSLA, frekans: gunluk oldugu icin "daily" kullandik.
TSLA ## Datanın yapısını inceleyelim.
data <- data.frame(Date = gsub("\\.", "-", gsub("X", "", rownames(TSLA))), Zaman.Serisi = TSLA$TSLA.High, stringsAsFactors = FALSE) ## Bu ornekte en yuksek fiyatlar (High) secilmis. Yukaridaki kodu sembole ismine ve kullanilmak istenen fiyat turune gore modifiye ediyoruz. Ayrica, kullanmak istedigimiz fiyat turune Zaman.Serisi adini veriyoruz.
data$Date <- as.Date(data$Date) ## Date kategori olarak değiştiriliyor.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

data <- data[263:641, ] ## Soruda verilen rakamlara gore datanin al kumesini aliyoruz.

temp <- data
variable <- "Zaman.Serisi" ## Zaman.Serisi olarak degistirmeyi unutmayin.
variable.name <- "Zaman.Serisi" ## Zaman.Serisi olarak degistirmeyi unutmayin.

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")

# Wooldridge Veri Turu Sorusu 1
data(ceosal1) ## Datayı yüklüyoruz. Bu ornekte ceosal1 datasini soruyor.
ceosal1 ## Datayi print ediyoruz.
str(ceosal1) ## Datanin yapisi.
?ceosal1 ## Datanın metadatası. Help kisminda acilan bilgilere bakarak soruyu cevaplayabiliriz.

# Wooldridge Veri Turu Sorusu 2
data(cement) ## Datayı yüklüyoruz. Bu ornekte cement datasini soruyor.
cement ## Datayi print ediyoruz.
str(cement) ## Datanin yapisi.
?cement ## Datanın metadatası. Help kisminda acilan bilgilere bakarak soruyu cevaplayabiliriz.

# Wooldridge Veri Turu Sorusu 3
data(crime2) ## Datayı yüklüyoruz. Bu ornekte crime2 datasini soruyor.
crime2 ## Datayi print ediyoruz.
str(crime2) ## Datanin yapisi.
?crime2 ## Datanın metadatası. Help kisminda acilan bilgilere bakarak soruyu cevaplayabiliriz.

# Wooldridge Veri Turu Sorusu 4
data(jtrain) ## Datayı yüklüyoruz. Bu ornekte jtrain datasini soruyor.
jtrain ## Datayi print ediyoruz.
str(jtrain) ## Datanin yapisi.
?jtrain ## Datanın metadatası. Help kisminda acilan bilgilere bakarak soruyu cevaplayabiliriz.
