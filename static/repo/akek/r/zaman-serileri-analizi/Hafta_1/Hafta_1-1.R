#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#=========================== Zaman Serileri Analizi ============================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#================================= Hafta #1-1 ==================================
# Notlar:
#
## Bu yazıda kullandığımız datayı (eger varsa) web sitemizdeki ilgili bölümde bulabilirsiniz.
## Aşağıdaki R kodu, öncelikle gerekli R paketlerini yüklüyor ve daha sonra working directory'yi bilgisayarınızda bu kaynak dosyasının bulunduğu lokasyona göre değiştiriyor. Son olarak ise ilgili R kodunu çalıştırıyor.

#=============================== Gerekli Paketler ==============================
# Tek bir adımda gerekli paketlerin yüklenmesi ve kurulması.
# Bu adimi daha kolay hale getirmek için öncelikle "Load.Install" fonksiyonunu tanımlayalım.
#=========================
Load.Install <- function(Package.Names) {
    #update.packages() ## Eger tüm paketleri güncellemek isterseniz kullanabilirsiniz.
    is_installed <- function(mypkg) is.element(mypkg, utils::installed.packages()[ ,1])
    for (Package.Names in Package.Names) {
        if (!is_installed(Package.Names)) {
            utils::install.packages(Package.Names, dependencies = TRUE)
        }
        suppressMessages(library(Package.Names, character.only = TRUE, quietly = TRUE, verbose = FALSE))
    }
}
#=========================
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "ggplot2", "xtable", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "ggseas"))
#==========
## Load.Install(Package.Names = "readxl")
## Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#==========

#======================== Working Directory'yi Belirlemek ======================
# Working directory'nin bu kaynak dosyasının olduğu lokasyonda belirlenmesi.
#=========================
getwd() ## Şimdiki working directory.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının uzantısını buluyor.
setwd(paste0(main.path, "/")) ## Yeni working directory bu kaynak dosyasının lokasyonunda belirleniyor.

#============================ Gerekli Dosya Isimleri ===========================
# Analiz sirasinda gerekli olan kullanici tarafindan belirlenmis dosya isimleri.
#=========================
functions.folder.name <- "../../_functions"
figs.tabs.folder.name <- "_figs-tabs"

#============================= Gerekli Fonksiyonlar ============================
# Analiz sirasinda gerekli olan kullanici tarafindan yazilmis fonksiyonlarin yuklenmesi
#=========================
# "human_num" fonksiyonu ve benzerleri icin https://github.com/fdryan/R/blob/master/ggplot2_formatter.r linkine ya da "estimation_functions.R" dosyasina bakabilirsiniz.
# "lagpad" fonsiyonu icin "estimation_functions.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "estimation_functions.R"))

# "annotation_compass" fonsiyonu icin "graphic_functions.R"dosyasina bakabilirsiniz. Bu fonksiyon ile ggplot grafiklerine yazi ekleyebiliriz.
source(paste0(main.path, "/", functions.folder.name, "/", "graphic_functions.R"))

#=============================== Seed Belirlenmesi =============================
# Rassal sayilar kullanarak olusturdugumuz verilerin her seferinde ayni olmasi ve yeniden uretilebilir bir analiz (reproducible analysis) icin seed ayarlaniyor.
#=========================
set.seed(1234)

#================================= Genel Bilgi =================================
# Bu bolumde cesitli kaynaklardan elde ettigimiz zaman serisi datalarini grafikle gostermeye calisacagiz.
# Daha sonra bu grafikleri pdf formatinda kaydedecegiz.

#================================== Grafikler ==================================
# Belirli alt basliklar icinde grafikler ve onlara bagli tablolar.
#=========================

#=========================
# World Bank'ten Turkiye icin Gayri Safi Yurtici Hasila, GSYH Deflatoru (2009 Baz Yili) ve Populasyon datasi.
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*current.*LCU", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.MKTP.CN" GDP (current LCU) datasini kullanacagiz.
WDIsearch(string = "gdp.*constant.*LCU", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.MKTP.KN" GDP (constant LCU) datasini kullanacagiz.
WDIsearch(string = "gdp.*capita.*constant.*LCU", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.KN" GDP per capita (constant LCU) datasini kullanacagiz. 2009 baz yili kullaniliyor.

data <- WDI(country = c("TR"), indicator = c("NY.GDP.MKTP.CN", "NY.GDP.MKTP.KN", "NY.GDP.PCAP.KN"), start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "NY.GDP.MKTP.CN", "NY.GDP.MKTP.KN", "NY.GDP.PCAP.KN")] ## IStedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "GDP", "R.GDP", "R.GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "GDP", "R.GDP", "R.GDP.PCAP")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# gdp-tr-tl grafigi.
temp <- data
variable <- "GDP"
variable.name <- "Nominal GSYH (TL)"
file.name <- "gdp-tr-tl"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# real-gdp-tr-tl grafigi.
temp <- data
variable <- "R.GDP"
variable.name <- "Reel GSYH (2009 TL)"
file.name <- "real-gdp-tr-tl"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# real-gdp-pcap-tr-tl grafigi.
temp <- data
variable <- "R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2009 TL)"
file.name <- "real-gdp-pcap-tr-tl"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

#=========================
# World Bank'ten Turkiye icin Kisi basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.KD" GDP per capita (constant 2010 US$) datasini kullanacagiz.

data <- WDI(country = c("TR"), indicator = "NY.GDP.PCAP.KD", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "NY.GDP.PCAP.KD")] ## IStedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "R.GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "R.GDP.PCAP")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Kisi basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)'nin Buyume Orani hesaplaniyor.
temp <- data ## Asagidaki dongunun islemesi icin datayi farkli bir isimle kaydedip onunla islem yapiyoruz.
for (i in 3:ncol(temp)) {
    x <- temp[ , i] ## Degisken
    growth.x <- 100 * (diff(x, lag = 1, differences = 1) / x[-length(x)])
    growth.x <- round(growth.x, 3)
    x <- as.data.frame(c(NA, growth.x), stringsAsFactors = FALSE)
    colnames(x) <- paste0("Gr.", colnames(temp)[i]) ## Buyume degerlerini "Gr." ile ifade ediyoruz.
    temp <- cbind(temp, x)
}
data <- temp ## Tekrar ayni ismi kullanmaya basliyoruz.

# real-gdp-pcap-tr grafigi.
temp <- data
variable <- "R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "real-gdp-pcap-tr"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# real-gdp-pcap-tr tablosu: Gecikme degerleriyle beraber
temp <- data
file.name <- "real-gdp-pcap-tr"

temp$R.GDP.PCAP <- round(temp$R.GDP.PCAP, 0) ## Tablonun daha iyi gorunmesi icin sayilar yuvarlaniyor.
temp$t <- 1:nrow(temp) ## Trend degiskeni ya da t indeks degeri ekleniyor.
temp <- temp[, c("Year", "t", "R.GDP.PCAP")] ## Degiskenler siraya dizildi.
temp$R.GDP.PCAP.t1 <- lagpad(temp$R.GDP.PCAP, 1) ## 1. Gecikme olusturuldu.
temp$R.GDP.PCAP.t2 <- lagpad(temp$R.GDP.PCAP, 2) ## 2. Gecikme olusturuldu.
temp$R.GDP.PCAP.t3 <- lagpad(temp$R.GDP.PCAP, 3) ## 3. Gecikme olusturuldu.
temp$R.GDP.PCAP.t4 <- lagpad(temp$R.GDP.PCAP, 4) ## 4. Gecikme olusturuldu.
for (i in 3:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i][1] <- paste0("\\red{", temp[which(!is.na(temp[, i])), i][1], "}")
}
for (i in 1:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i] <- paste0("$", temp[which(!is.na(temp[, i])), i], "$")
}
temp <- rbind(temp[1:6, ], "$\\vdots$", temp[(nrow(temp)-3):nrow(temp), ])
col.names <- c("Yıl", "$t$", "GSYH$_{t}$", "GSYH$_{t-1}$", "GSYH$_{t-2}$", "GSYH$_{t-3}$", "GSYH$_{t-4}$")
colnames(temp) <- col.names

temp.output.path <- paste0(figs.tabs.folder.name, "/", file.name, ".tex")
temp.output <- print.xtable(xtable(temp), type = "latex", file = "", sanitize.text.function = function(x){x}, print.results = FALSE, NA.string = "NA", include.rownames = FALSE, include.colnames = TRUE, only.contents = TRUE, booktabs = TRUE, comment = FALSE) ## sanitize.text.function should be added to correctly run the output in latex.
table.correction <- "\\toprule\n"
cat(table.correction, temp.output, file = temp.output.path) ## Writing the table.

# real-gdp-pcap-tr'nin SACF grafigi
variable <- "R.GDP.PCAP"
temp <- data[, variable]
temp <- temp[complete.cases(temp)]
title <- "Kişi Başı Reel GSYH (2010 ABD Doları): SACF"
file.name <- "real-gdp-pcap-tr-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 20, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

# gr-real-gdp-pcap-tr grafigi.
temp <- data
variable <- "Gr.R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH Büyüme Oranı"
file.name <- "gr-real-gdp-pcap-tr"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# gr-real-gdp-pcap-tr tablosu: Gecikme degerleriyle beraber
temp <- data
file.name <- "gr-real-gdp-pcap-tr"

temp$R.GDP.PCAP <- round(temp$R.GDP.PCAP, 0) ## Tablonun daha iyi gorunmesi icin sayilar yuvarlaniyor.
temp$Gr.R.GDP.PCAP <- round(temp$Gr.R.GDP.PCAP, 2) ## Tablonun daha iyi gorunmesi icin sayilar yuvarlaniyor.
temp$t <- 1:nrow(temp) ## Trend degiskeni ya da t indeks degeri ekleniyor.
temp <- temp[, c("Year", "t", "R.GDP.PCAP", "Gr.R.GDP.PCAP")] ## Degiskenler siraya dizildi.
temp$Gr.R.GDP.PCAP.t1 <- lagpad(temp$Gr.R.GDP.PCAP, 1) ## 1. Gecikme olusturuldu.
temp$Gr.R.GDP.PCAP.t2 <- lagpad(temp$Gr.R.GDP.PCAP, 2) ## 2. Gecikme olusturuldu.
temp$Gr.R.GDP.PCAP.t3 <- lagpad(temp$Gr.R.GDP.PCAP, 3) ## 3. Gecikme olusturuldu.
temp$Gr.R.GDP.PCAP.t4 <- lagpad(temp$Gr.R.GDP.PCAP, 4) ## 4. Gecikme olusturuldu.
for (i in 4:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i][1] <- paste0("\\red{", temp[which(!is.na(temp[, i])), i][1], "}")
}
for (i in 1:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i] <- paste0("$", temp[which(!is.na(temp[, i])), i], "$")
}
temp <- rbind(temp[1:6, ], "$\\vdots$", temp[(nrow(temp)-1):nrow(temp), ])
col.names <- c("Yıl", "$t$", "GSYH$_{t}$", "GR$_{t}$", "GR$_{t-1}$", "GR$_{t-2}$", "GR$_{t-3}$", "GR$_{t-4}$")
colnames(temp) <- col.names

temp.output.path <- paste0(figs.tabs.folder.name, "/", file.name, ".tex")
temp.output <- print.xtable(xtable(temp), type = "latex", file = "", sanitize.text.function = function(x){x}, print.results = FALSE, NA.string = "NA", include.rownames = FALSE, include.colnames = TRUE, only.contents = TRUE, booktabs = TRUE, comment = FALSE) ## sanitize.text.function should be added to correctly run the output in latex.
table.correction <- "\\toprule\n"
cat(table.correction, temp.output, file = temp.output.path) ## Writing the table.

# gr-real-gdp-pcap-tr'nin SACF grafigi
variable <- "Gr.R.GDP.PCAP"
temp <- data[, variable]
temp <- temp[complete.cases(temp)]
title <- "Kişi Başı Reel GSYH Büyüme Oranı: SACF"
file.name <- "gr-real-gdp-pcap-tr-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 20, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

#=========================
# World Bank'ten Kore icin Kisi basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.KD" GDP per capita (constant 2010 US$) datasini kullanacagiz.

data <- WDI(country = c("KR"), indicator = "NY.GDP.PCAP.KD", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "NY.GDP.PCAP.KD")] ## IStedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "R.GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year , "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "R.GDP.PCAP")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Kisi basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)'nin Buyume Orani hesaplaniyor.
temp <- data ## Asagidaki dongunun islemesi icin datayi farkli bir isimle kaydedip onunla islem yapiyoruz.
for (i in 3:ncol(temp)) {
    x <- temp[ , i] ## Degisken
    growth.x <- 100 * (diff(x, lag = 1, differences = 1) / x[-length(x)])
    growth.x <- round(growth.x, 3)
    x <- as.data.frame(c(NA, growth.x), stringsAsFactors = FALSE)
    colnames(x) <- paste0("Gr.", colnames(temp)[i]) ## Buyume degerlerini "Gr." ile ifade ediyoruz.
    temp <- cbind(temp, x)
}
data <- temp ## Tekrar ayni ismi kullanmaya basliyoruz.

# real-gdp-pcap-kr grafigi.
temp <- data
variable <- "R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "real-gdp-pcap-kr"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# gr-real-gdp-pcap-kr grafigi.
temp <- data
variable <- "Gr.R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH Büyüme Oranı"
file.name <- "gr-real-gdp-pcap-kr"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# gr-real-gdp-pcap-kr'nin SACF grafigi
variable <- "Gr.R.GDP.PCAP"
temp <- data[, variable]
temp <- temp[complete.cases(temp)]
title <- "Kişi Başı Reel GSYH Büyüme Oranı: SACF"
file.name <- "gr-real-gdp-pcap-kr-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 20, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

#=========================
# World Bank'ten secilmis bazi ulkeler icin basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.KD" GDP per capita (constant 2010 US$) datasini kullanacagiz.

data <- WDI(country = c("TR", "KR", "MYS", "MEX", "GRC", "USA"), indicator = "NY.GDP.PCAP.KD", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "country", "NY.GDP.PCAP.KD")] ## Istedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "Country", "R.GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Country <- ifelse(data$Country == "Turkey", "Türkiye", ifelse(data$Country == "Greece", "Yunanistan", ifelse(data$Country == "Korea, Rep.", "Kore", ifelse(data$Country == "Mexico", "Meksika", ifelse(data$Country == "United States", "ABD", "Malezya")))))
data$Date <- as.Date(paste(data$Year , "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "Country", "R.GDP.PCAP")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Country, data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# real-gdp-pcap-all-1 grafigi.
temp <- data
temp <- temp[temp$Country %in% c("Kore", "Meksika", "Malezya", "Türkiye"), ]
rownames(temp) <- 1:nrow(temp) ## Satir sayilarini duzenliyoruz.
variable <- "R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "real-gdp-pcap-all-1"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = Country), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = Country), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24)) + theme(legend.title = element_blank())
print(g)
dev.off()

# real-gdp-pcap-all-2 grafigi.
temp <- data
variable <- "R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "real-gdp-pcap-all-2"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = Country), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = Country), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
g <- g + theme(legend.title = element_blank()) + guides(col = guide_legend(nrow = 1))
print(g)
dev.off()

#=========================
# World Bank'ten Turkiye icin Tuketici Fiyatlari Indeksi (2010 Baz yili)
#==========
Load.Install("WDI")
WDIsearch(string = "consumer.*price.*index", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "FP.CPI.TOTL" Consumer price index (2010 = 100) datasini kullanacagiz.

data <- WDI(country = c("TR"), indicator = "FP.CPI.TOTL", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "FP.CPI.TOTL")] ## Istedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "CPI.2010") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "CPI.2010")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Yeni bir baz yilina gore (2000 Baz Yili) indeksi tekrar hesapliyoruz.
data$CPI.2019 <- (data$CPI.2010/data[data$Year == 2019, "CPI.2010"]) * 100

# Turkiye icin Tuketici Fiyatlari Indeksi (2010 Baz yili)'nin Buyume Orani yani enflasyon hesaplaniyor.
temp <- data
for (i in 3:ncol(temp)) {
    x <- temp[ , i] ## Degisken
    growth.x <- 100 * (diff(x, lag = 1, differences = 1) / x[-length(x)])
    growth.x <- round(growth.x, 3)
    x <- as.data.frame(c(NA, growth.x), stringsAsFactors = FALSE)
    colnames(x) <- paste0("Gr.", colnames(temp)[i]) ## Buyume degerlerini "Gr." ile ifade ediyoruz.
    temp <- cbind(temp, x)
}
data <- temp

# CPI-2010 grafigi.
temp <- data
variable <- "CPI.2010"
variable.name <- "Tüketici Fiyatları İndeksi (2010 Baz Yılı)"
file.name <- "cpi-2010"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# CPI-2019 grafigi.
temp <- data
variable <- "CPI.2019"
variable.name <- "Tüketici Fiyatları İndeksi (2019 Baz Yılı)"
file.name <- "cpi-2019"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# Gr-CPI-2010 grafigi.
temp <- data
variable <- "Gr.CPI.2010"
variable.name <- "Enflasyon Oranı (2010 Baz Yılı Verisi ile)"
file.name <- "gr-cpi-2010"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# Gr-CPI-2019 grafigi.
temp <- data
variable <- "Gr.CPI.2019"
variable.name <- "Enflasyon Oranı (2019 Baz Yılı Verisi ile)"
file.name <- "gr-cpi-2019"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) + geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 3) +
    xlab("Zaman (Yıl)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

#=========================
# Avustralya Elektrik Uretimi (milyar kWh) (Ceyreklik) - Quarterly Australian Electricity production
## fpp2 paketindeki qauselec adli data kullanilmistir.
#==========
Load.Install("fpp2")
dat <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
dat[dat$Item == "qauselec", c(1,3,4)] ## Data hakkindaki bilgi.

data(qauselec)
data <- qauselec
data <- data.frame(Date = as.Date(time(data)), Elec.Prod = as.matrix(data), stringsAsFactors = FALSE)
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Elec.Prod grafigi.
temp <- data
variable <- "Elec.Prod"
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"
file.name <- "elec-prod"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Çeyreklik)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# Elec.Prod'in Mevsimsel grafigi.
variable <- "Elec.Prod"
temp <- ts(data = data[, variable], start = c(year(data$Date[1]), month(data$Date[1])), frequency = 4)
temp <- window(temp, start = NULL, end = c(2009, 4)) ## 2010 yilına ait veri tam olmadigi icin cikartiliyor.
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"
title <- "Avustralya Elektrik Üretimi (milyar kWh): Mevsimsel Grafik"
quarter.labels <- c("Ç1", "Ç2", "Ç3", "Ç4")
file.name <- "elec-prod-seasonal"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggseasonplot(x = temp, season.labels = quarter.labels, year.labels = TRUE, year.labels.left = TRUE, continuous = FALSE, polar = FALSE, labelgap = 0.04) + ## year.labels = FALSE ve year.labels.left = FALSE oldugunda legend gosterilmez onun yerine yillara ait etiket gosterilir.
    xlab("Zaman (Çeyreklik)") + ylab(latex2exp::TeX(variable.name)) +
    labs(title = latex2exp::TeX(title)) +
    scale_y_continuous(breaks = pretty(temp), labels = human_num) +
    theme_grey() +
    # guides(color = guide_legend(title = "Yıl")) + ## year.labels = FALSE ve year.labels.left = FALSE oldugunda kullanilmali.
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

# Elec.Prod'in Mevsimsel Altseri grafigi.
variable <- "Elec.Prod"
temp <- ts(data = data[, variable], start = c(year(data$Date[1]), month(data$Date[1])), frequency = 4)
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"
title <- "Avustralya Elektrik Üretimi (milyar kWh): Mevsimsel Altseri Grafiği"
quarter.labels <- c("Ç1", "Ç2", "Ç3", "Ç4")
file.name <- "elec-prod-seasonal-sub"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggsubseriesplot(temp) +
    xlab("Zaman (Çeyreklik)") + ylab(latex2exp::TeX(variable.name)) +
    labs(title = latex2exp::TeX(title)) +
    scale_x_continuous(breaks = 1:frequency((temp)), labels = quarter.labels) +
    scale_y_continuous(breaks = pretty(temp), labels = human_num) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

# Elec.Prod'in SACF grafigi
variable <- "Elec.Prod"
temp <- data[, variable]
temp <- temp[complete.cases(temp)]
title <- "Avustralya Elektrik Üretimi (milyar kWh): SACF"
file.name <- "elec-prod-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 20, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

#=========================
# Nottingham Ortalama Sicaklik (F°) (Aylik) - Average Monthly Temperatures at Nottingham
## datasets paketindeki nottem adli data kullanilmistir.
#==========
Load.Install("datasets")
dat <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
dat[dat$Item == "nottem", c(1,3,4)] ## Data hakkindaki bilgi.

data(nottem)
data <- nottem
data <- data.frame(Date = as.Date(time(data)), Ave.Temp = as.matrix(data), stringsAsFactors = FALSE)
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Ave.Temp grafigi.
temp <- data
variable <- "Ave.Temp"
variable.name <- "Nottingham Ortalama Sıcaklık (F°)"
file.name <- "ave-temp"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Ay)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# Ave.Temp'in SACF grafigi
variable <- "Ave.Temp"
temp <- data[, variable]
temp <- temp[complete.cases(temp)]
title <- "Nottingham Ortalama Sıcaklık (F°): SACF"
file.name <- "ave-temp-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 30, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

#=========================
# İstanbul'a Verilen Temiz Su Miktarları (Aylik)
## Dataya su linkten ulasilabilir: https://data.ibb.gov.tr/dataset/istanbul-a-verilen-temiz-su-miktarlari
#==========
Load.Install("forecast")

# Data dosyasinin adi.
file.path <- "clean-water.xlsx"

# Ham data dosyasinin yuklenmesi.
# Ham datanin yuklenmesi.
data <- read_excel(path = file.path, sheet = 3, range = cell_limits(c(1, 1), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yukledigimiz datayi read_excel fonksiyonu tibble formatinda kaydediyor.
data <- as.data.frame(data, stringsAsFactors = FALSE) ## Datayi data.frame formatina ceviriyoruz.
colnames(data)[1] <- "Month"
data$Month <- 1:12

# Datanin donusturulmesi.
data <- reshape2::melt(data, id.vars = c("Month"), variable_name = "value")

# Bazi degisiklikler
data <- dplyr::rename(data, Year = variable)
data <- dplyr::rename(data, Clean.Water = value)
data$Year <- as.numeric(as.character(data$Year))
data$Clean.Water <- as.numeric(data$Clean.Water)
data$Date <- as.Date(paste(data$Year, data$Month, "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "Month", "Clean.Water")]
data <- data[data$Year != 2019, ] ## 2019 datasinin son gozleminde bir problem oldugu icin 2019 yili silindi.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Clean.Water grafigi.
temp <- data
variable <- "Clean.Water"
variable.name <- "İstanbul'a Verilen Temiz Su Miktarı (Ton)"
file.name <- "clean-water"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Ay)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# Clean.Water Mevsimsel grafigi.
variable <- "Clean.Water"
temp <- ts(data = data[, variable], start = c(data$Year[1], data$Month[1]), frequency = 12)
variable.name <- "İstanbul'a Verilen Temiz Su Miktarı (Ton)"
title <- "İstanbul'a Verilen Temiz Su Miktarı (Ton): Mevsimsel Grafik"
month.labels <- c("Oca", "Şub", "Mar", "Nis", "May", "Haz", "Tem", "Ağu", "Eyl", "Eki", "Kas", "Ara")
file.name <- "clean-water-seasonal"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggseasonplot(x = temp, season.labels = month.labels, year.labels = TRUE, year.labels.left = TRUE, continuous = FALSE, polar = FALSE, labelgap = 0.04) + ## year.labels = FALSE ve year.labels.left = FALSE oldugunda legend gosterilmez onun yerine yillara ait etiket gosterilir.
    xlab("Zaman (Ay)") + ylab(latex2exp::TeX(variable.name)) +
    labs(title = latex2exp::TeX(title)) +
    scale_y_continuous(breaks = pretty(temp), labels = human_num) +
    theme_grey() +
    # guides(color = guide_legend(title = "Yıl")) + ## year.labels = FALSE ve year.labels.left = FALSE oldugunda kullanilmali.
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

# Clean.Water Mevsimsel Altseri grafigi.
variable <- "Clean.Water"
temp <- ts(data = data[, variable], start = c(data$Year[1], data$Month[1]), frequency = 12)
variable.name <- "İstanbul'a Verilen Temiz Su Miktarı (Ton)"
title <- "İstanbul'a Verilen Temiz Su Miktarı (Ton): Mevsimsel Altseri Grafiği"
labels <- c("Oca", "Şub", "Mar", "Nis", "May", "Haz", "Tem", "Ağu", "Eyl", "Eki", "Kas", "Ara")
file.name <- "clean-water-seasonal-sub"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggsubseriesplot(temp) +
    xlab("Zaman (Ay)") + ylab(latex2exp::TeX(variable.name)) +
    labs(title = latex2exp::TeX(title)) +
    scale_x_continuous(breaks = 1:frequency((temp)), labels = labels) +
    scale_y_continuous(breaks = pretty(temp), labels = human_num) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

# Clean.Water'in SACF grafigi
variable <- "Clean.Water"
temp <- data[, variable]
temp <- temp[complete.cases(temp)]
title <- "İstanbul'a Verilen Temiz Su Miktarı (Ton): SACF"
file.name <- "clean-water-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 30, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

#=========================
# Istanbul barajlarinin doluluk oranlari (Gunluk)
## Dataya su linkten ulasilabilir: https://data.ibb.gov.tr/dataset/istanbul-dam-occupany-rates-data
#==========
# Data dosyasinin adi.
file.path <- "dam_occupancy.csv"

# Ham data dosyasinin yuklenmesi.
data <- read.csv(file = file.path, header = TRUE, sep = ",", dec = ".", colClasses = "character", comment.char = "", na.string = "")
colnames(data) <- c("Date", "Occupancy.Rate", "Reserved.Water")
data$Occupancy.Rate <- as.numeric(data$Occupancy.Rate)
data$Reserved.Water <- as.numeric(data$Reserved.Water)
data$Date <- as.Date(data$Date, tz = "UTC", format = "%Y-%m-%d")
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# dam-occupancy-rate grafigi.
temp <- data
variable <- "Occupancy.Rate"
variable.name <- "İstanbul Barajları Doluluk Oranı"
file.name <- "dam-occupancy-rate"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

#=========================
# Yahoo Finance: Tesla, Twitter ve Facebook hisseleri (Gunluk)
#==========
Load.Install("quantmod")
loadSymbols(Symbols = "TSLA", periodicity = "daily", return.class = "data.frame")
loadSymbols(Symbols = "FB", periodicity = "daily", return.class = "data.frame")
loadSymbols(Symbols = "TWTR", periodicity = "daily", return.class = "data.frame")

# Datalarin birlestirilmesi
tsla <- data.frame(Date = rownames(TSLA), Tesla = TSLA$TSLA.Close, stringsAsFactors = FALSE)
fb <- data.frame(Date = rownames(FB), Facebook = FB$FB.Close, stringsAsFactors = FALSE)
twtr <- data.frame(Date = rownames(TWTR), Twitter = TWTR$TWTR.Close, stringsAsFactors = FALSE)
temp <- full_join(tsla, fb, by = "Date")
data <- full_join(temp, twtr, by = "Date")

# Datanin donusturulmesi.
data <- reshape2::melt(data, id.vars = c("Date"), variable_name = "value")

# Bazi degisiklikler
data <- dplyr::rename(data, Symbol = variable)
data <- dplyr::rename(data, Close = value)
data$Date <- as.Date(data$Date)
data$Symbol <- as.character(data$Symbol)
data <- data[order(data$Symbol, data$Date, decreasing = FALSE, na.last = FALSE), ]
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Stocks-1 grafigi.
temp <- data
variable <- "Close"
variable.name <- "Kapanış Fiyatı (ABD Doları)"
file.name <- "stocks-1"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = Symbol), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24)) + theme(legend.title = element_blank())
print(g)
dev.off()

#=========================
# Yahoo Finance: Amazon ve Google hisseleri (Gunluk)
#==========
Load.Install("quantmod")
loadSymbols(Symbols = "AMZN", periodicity = "daily", return.class = "data.frame")
loadSymbols(Symbols = "GOOG", periodicity = "daily", return.class = "data.frame")

# Datalarin birlestirilmesi
amzn <- data.frame(Date = rownames(AMZN), Amazon = AMZN$AMZN.Close, stringsAsFactors = FALSE)
goog <- data.frame(Date = rownames(GOOG), Google = GOOG$GOOG.Close, stringsAsFactors = FALSE)
data <- full_join(amzn, goog, by = "Date")

# Datanin donusturulmesi.
data <- reshape2::melt(data, id.vars = c("Date"), variable_name = "value")

# Bazi degisiklikler
data <- dplyr::rename(data, Symbol = variable)
data <- dplyr::rename(data, Close = value)
data$Date <- as.Date(data$Date)
data$Symbol <- as.character(data$Symbol)
data <- data[order(data$Symbol, data$Date, decreasing = FALSE, na.last = FALSE), ]
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Stocks-2 grafigi.
temp <- data
variable <- "Close"
variable.name <- "Kapanış Fiyatı (ABD Doları)"
file.name <- "stocks-2"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = Symbol), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable]), labels = human_num) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24)) + theme(legend.title = element_blank())
print(g)
dev.off()

#=========================
# Yahoo Finance: Google hissesi gunluk getiri orani (Gunluk)
#==========
Load.Install("quantmod")
loadSymbols(Symbols = "GOOG", periodicity = "daily", return.class = "data.frame")

data <- data.frame(Date = rownames(GOOG), Close = GOOG$GOOG.Close, stringsAsFactors = FALSE)

# Bazi degisiklikler
data$Date <- as.Date(data$Date)
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ]
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# Google hissesi gunluk getiri orani (buyume orani) hesaplaniyor.
temp <- data ## Asagidaki dongunun islemesi icin datayi farkli bir isimle kaydedip onunla islem yapiyoruz.
for (i in 2:ncol(temp)) {
    x <- temp[ , i] ## Degisken
    growth.x <- 100 * (diff(x, lag = 1, differences = 1) / x[-length(x)])
    growth.x <- round(growth.x, 3)
    x <- as.data.frame(c(NA, growth.x), stringsAsFactors = FALSE)
    colnames(x) <- paste0("Gr.", colnames(temp)[i]) ## Buyume degerlerini "Gr." ile ifade ediyoruz.
    temp <- cbind(temp, x)
}
data <- temp ## Tekrar ayni ismi kullanmaya basliyoruz.

# google-return grafigi.
temp <- data
variable <- "Gr.Close"
variable.name <- "Google Hisse Senedi Getiri Oranı"
file.name <- "google-return"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

#=========================
# Yahoo Finance: TR/USD doviz kuru (Gunluk)
#==========
Load.Install(c("quantmod", "forecast"))
loadSymbols(Symbols = "TRY=X", periodicity = "daily", return.class = "data.frame")

data <- data.frame(Date = rownames(`TRY=X`), Close = `TRY=X`$`TRY=X.Close`, stringsAsFactors = FALSE)

# Bazi degisiklikler
data$Date <- as.Date(data$Date)
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ]
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# TR-USD grafigi.
temp <- data
variable <- "Close"
variable.name <- "Döviz Kuru (TL/USD)"
file.name <- "tr-usd"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# ln(TR-USD) grafigi.
temp <- data
variable <- "Close"
temp[, variable] <- log(temp[, variable])
y.lab <- "Ln Döviz Kuru (TL/USD)"
variable.name <- "Logaritmik Transformasyon ile Döviz Kuru (TL/USD)"
file.name <- "tr-usd-ln"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(y.lab)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

# Box-Cox Transformasyonu yapılmıs TR-USD grafigi.
temp <- data
variable <- "Close"
value <- temp[, variable]
## Box-Cox Transformasyonu yapılmıs Beta dagiliminin'nin Olasilik Yogunluk grafigi
### Box-cox transformasyonu.
#### Box-cox transformasyonu ile bulunan lambda degerine gore ham veri degistiriliyor: Eger lambda = 1 ise transformasyona gerek yok, eger lambda == 0 ise ln(x), ve eger lambda farkli bir deger ise (x^lambda - 1)/lambda.
s.lambda <- forecast::BoxCox.lambda(value, method = c("loglik"), lower = -2, upper = 2) ## Box-cox transformasyonu sonucunda bulunan lambda degeri. method = c("guerrero") kriteri de kullanilabilir. Genelde kullanilan alt limit -2 ve ust limit ise 2'dir.
if(s.lambda == 1) { ## Lambda = 1.
    value.boxcox <- value
    message("Box-Cox transformasyonu sonucu: Transformasyona gerek yok.")
} else if(s.lambda == 0) { ## Lambda = 0.
    value.boxcox <- log(value)
    message("Box-Cox transformasyonu sonucu: Logaritmik Transformasyon yapıldı.")
} else { ## Lambda farkli bir deger ise.
    value.boxcox <- (value^s.lambda - 1)/s.lambda
    message(paste0("Box-Cox transformasyonu sonucu: λ = ", round(s.lambda, 2), " kullanılarak Box-Cox transformasyonu yapıldı."))
}
temp[, variable] <- value.boxcox
y.lab <- paste0("Box-Cox $(\\lambda = ", round(s.lambda, 2), ")$ ile Döviz Kuru (TL/USD)")
variable.name <- paste0("Box-Cox Transformasyonu ile Döviz Kuru (TL/USD)")
file.name <- "tr-usd-boxcox"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(y.lab)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
print(g)
dev.off()

#=========================
# Yahoo Finance: BTC/USD (Gunluk)
#==========
Load.Install("quantmod")
loadSymbols(Symbols = "BTC-USD", periodicity = "daily", return.class = "data.frame")

# BTC-USD grafigi.
file.name <- "btc-usd"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

candleChart(`BTC-USD`, theme = "white", TA = "addSMA()", subset = "2020-12::2021") ## With simple moving average
dev.off()

#=========================
# Pur Rassal Surec
#==========
n <- 1000 ## Gozlem sayisi.
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = n)
mean(WN)
var(WN)

data <- data.frame(Date = 1:length(WN), White.Noise = as.matrix(WN), stringsAsFactors = FALSE)
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# WN grafigi.
temp <- data
variable <- "White.Noise"
variable.name <- "Simüle Edilmiş Pür Rassal Süreç"
file.name <- "white-noise"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 10, parse = TRUE)
print(g)
dev.off()

# WN(t) vs WN(t-s) icin bazi duzenlemeler.
s <- 1 ## Secili gecikme uzunlugunu belirliyoruz.
WN <- c(WN) ## Zaman serisini normal bir vektore ceviriyoruz.
WNt <- WN[(1 + s):length(WN)] ## WN(t). Cari donem verisi.
WNts <- WN[1:(length(WN) - s)] ## WN(t-s). s gecikme onceki veri.
data <- data.frame(WNt = WNt, WNts = WNts, stringsAsFactors = FALSE)

# WN(t) vs WN(t-s) grafigi.
temp <- data
file.name <- "white-noise-t-vs-ts"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_point(aes(x = WNt, y = WNts, colour = "Variable"), size = 3, colour = "darkblue") +
    xlab(latex2exp::TeX(paste0("$wn_{t}$"))) + ylab(latex2exp::TeX(paste0("$wn_{t-", s, "}$"))) +
    labs(title = latex2exp::TeX(paste0("Simüle Edilmiş Pür Rassal Süreç $wn_{t}$ vs $wn_{t-", s, "}$"))) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    geom_vline(aes(xintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 10, parse = TRUE)
print(g)
dev.off()

# WN'nin SACF grafigi
temp <- WN
temp <- temp[complete.cases(temp)]
title <- "Simüle Edilmiş Pür Rassal Süreç: SACF"
file.name <- "white-noise-sacf"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggAcf(x = temp, ci = 0.95, lag.max = 10, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = latex2exp::TeX(title)) +
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5))
print(g)
dev.off()

#=========================
# Matematiksel Transformasyon
## Bu bolumde ornek olmasi acisindan Beta Dagilimi kullanilacak ve matematiksel transformasyon sonucunda verinin normal dagilima yakinsadigi gosterilecektir.
## Matematiksel transformasyonlar olarak sadece dogal logaritma ve box-cox transfromasyonu kullanilacaktir.
#==========
Load.Install("forecast")

# Beta dagilimi.
## Beta dagilimi datasini olusturma.
n <- 5000 ## Gozlem sayisi
p.alpha <- 1 ## Beta dagilimi alfa parametresi.
p.beta <- 3 ## Beta dagilimi beta parametresi.
value <- rbeta(n = n, shape1 = p.alpha, shape2 = p.beta) ## Beta dagilimi yapan rassal degerler.
beta.dist <- data.frame(Value = value, stringsAsFactors = FALSE) ## Beta dagilimi yapan degerler data icine aliniyor.

## Beta dagiliminin'nin Olasilik Yogunluk grafigi.
temp <- beta.dist
binwidth <- NULL
x.lab <- paste0("$\\Beta(\\alpha = ", p.alpha, ", \\, \\beta = ", p.beta, ")$")
y.lab <- "Olasılık Yoğunluğu"
title <- paste0("Beta Dağılımı: Olasılık Yoğunluk Fonksiyonu")
file.name <- "beta-dist"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp, aes(x = Value)) +
    xlab(latex2exp::TeX(x.lab)) + ylab(y.lab) +
    labs(title = latex2exp::TeX(title)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey", size = 0.1) + ## Histogram: sadece yogunluk. Ya bu kodu ya da usttekini kullanin.
    geom_density(colour = "red", fill = "#FF6666", alpha = 0.5, size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi (alan gosterimi ile).
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 10, parse = TRUE)
print(g)
dev.off()

## ln(Beta) dagiliminin'nin Olasilik Yogunluk grafigi
### Logaritmik Transformasyon.
value.ln <- log(value)
beta.dist.ln <- data.frame(Value = value.ln, stringsAsFactors = FALSE) ## Beta dagilimi yapan degerlerin dogal logaritmasi (ln(beta)) alinip data icine aliniyor.
temp <- beta.dist.ln
binwidth <- NULL
x.lab <- paste0("$\\ln \\Beta(\\alpha = ", p.alpha, ", \\, \\beta = ", p.beta, ")$")
y.lab <- "Olasılık Yoğunluğu"
title <- paste0("Logaritmik Transformasyon ile Beta Dağılımı: Olasılık Yoğunluk Fonksiyonu")
file.name <- "beta-dist-ln"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp, aes(x = Value)) +
    xlab(latex2exp::TeX(x.lab)) + ylab(y.lab) +
    labs(title = latex2exp::TeX(title)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey", size = 0.1) + ## Histogram: sadece yogunluk. Ya bu kodu ya da usttekini kullanin.
    geom_density(colour = "red", fill = "#FF6666", alpha = 0.5, size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi (alan gosterimi ile).
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 10, parse = TRUE)
print(g)
dev.off()

## Box-Cox Transformasyonu yapılmıs Beta dagiliminin'nin Olasilik Yogunluk grafigi
### Box-cox transformasyonu.
#### Box-cox transformasyonu ile bulunan lambda degerine gore ham veri degistiriliyor: Eger lambda = 1 ise transformasyona gerek yok, eger lambda == 0 ise ln(x), ve eger lambda farkli bir deger ise (x^lambda - 1)/lambda.
s.lambda <- forecast::BoxCox.lambda(value, method = c("loglik"), lower = -2, upper = 2) ## Box-cox transformasyonu sonucunda bulunan lambda degeri. method = c("guerrero") kriteri de kullanilabilir. Genelde kullanilan alt limit -2 ve ust limit ise 2'dir.
if(s.lambda == 1) { ## Lambda = 1.
    value.boxcox <- value
    message("Box-Cox transformasyonu sonucu: Transformasyona gerek yok.")
} else if(s.lambda == 0) { ## Lambda = 0.
    value.boxcox <- log(value)
    message("Box-Cox transformasyonu sonucu: Logaritmik Transformasyon yapıldı.")
} else { ## Lambda farkli bir deger ise.
    value.boxcox <- (value^s.lambda - 1)/s.lambda
    message(paste0("Box-Cox transformasyonu sonucu: λ = ", round(s.lambda, 2), " kullanılarak Box-Cox transformasyonu yapıldı."))
}
temp <- data.frame(Value = value.boxcox, stringsAsFactors = FALSE)
binwidth <- NULL
x.lab <- paste0("Box-Cox $(\\lambda = ", round(s.lambda, 2), ")$ ile $\\Beta(\\alpha = ", p.alpha, ", \\, \\beta = ", p.beta, ")$")
y.lab <- "Olasılık Yoğunluğu"
title <- paste0("Box-Cox Transformasyonu ile Beta Dağılımı: Olasılık Yoğunluk Fonksiyonu")
file.name <- "beta-dist-boxcox"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp, aes(x = Value)) +
    xlab(latex2exp::TeX(x.lab)) + ylab(y.lab) +
    labs(title = latex2exp::TeX(title)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey", size = 0.1) + ## Histogram: sadece yogunluk. Ya bu kodu ya da usttekini kullanin.
    geom_density(colour = "red", fill = "#FF6666", alpha = 0.5, size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi (alan gosterimi ile).
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 10, parse = TRUE)
print(g)
dev.off()

# Normal dagilim
## Normal dagilim datasini olusturma.
n <- 5000 ## Gozlem sayisi
p.mean <- 0 ## Normal dagilim mean (ortalama) parametresi.
p.sd <- 1 ## Normal dagilim sd (standart hata) parametresi.
value <- rnorm(n = n, mean = p.mean, sd = p.sd) ## Normal dagilim yapan rassal degerler.
normal.dist <- data.frame(Value = value, stringsAsFactors = FALSE) ## Normal dagilim yapan degerler data icine aliniyor.

## Normal dagilimin'nin Olasilik Yogunluk grafigi
temp <- normal.dist
binwidth <- NULL
x.lab <- paste0("$N(", p.mean, ", \\, ", p.sd, ")$")
y.lab <- "Olasılık Yoğunluğu"
title <- paste0("Normal Dağılım: Olasılık Yoğunluk Fonksiyonu")
file.name <- "normal-dist"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp, aes(x = Value)) +
    xlab(latex2exp::TeX(x.lab)) + ylab(y.lab) +
    labs(title = latex2exp::TeX(title)) +
    geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey", size = 0.1) + ## Histogram: sadece yogunluk. Ya bu kodu ya da usttekini kullanin.
    geom_density(colour = "red", fill = "#FF6666", alpha = 0.5, size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi (alan gosterimi ile).
    theme_grey() +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(plot.title = element_text(size = 24, hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 10, parse = TRUE)
print(g)
dev.off()

#====================================
Load.Install("ggseas")
library(ggseas)

AirPassengers
ap_df <- tsdf(AirPassengers)
head(ap_df)

ggplot(ap_df, aes(x = x, y = y)) +
    geom_line(colour = "grey75")

ggplot(ap_df, aes(x = x, y = y)) +
    stat_index(index.ref = 1)

ggplot(ap_df, aes(x = x, y = y)) +
    stat_index(index.ref = 120, index.basis = 1000)

ggplot(ap_df, aes(x = x, y = y)) +
    geom_line(colour = "grey75") +
    stat_rollapplyr(width = 12, align = "right") +
    labs(x = "", y = "Number of US Air Passengers\n(rolling average and original)")


ap_df <- tsdf(AirPassengers)
ggsdc(ap_df, aes(x = x, y = y), method = "decompose") +
    geom_line()

ggsdc(ap_df, aes(x = x, y = y), method = "decompose", type = "multiplicative", facet.titles = c("Veri", "Trend", "Mevsimsellik", "Kalıntı")) +
    geom_line(colour = "blue", size = 0.5) +
    theme_grey()

ggplot(ap_df, aes(x = x, y = y)) +
    geom_line(colour = "grey50") +
    stat_seas(colour = "blue") +
    stat_stl(s.window = 7, colour = "red") +
    stat_decomp(type = "multiplicative", colour = "purple")




#====================================



#==================================== SON ======================================
