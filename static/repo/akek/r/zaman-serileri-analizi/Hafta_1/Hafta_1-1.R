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
## Aşağıdaki R kodu, öncelikle working directory'yi bilgisayarınızda bu kaynak dosyasının bulunduğu lokasyona göre değiştiriyor ve daha sonra gerekli R paketlerini yüklüyor. Son olarak ise ilgili R kodunu çalıştırıyor.

#======================== Working Directory'yi Belirlemek ======================
# Working directory'nin bu kaynak dosyasının olduğu lokasyonda belirlenmesi.
#=========================
getwd() ## Şimdiki working directory.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının uzantısını buluyor.
setwd(paste0(main.path, "/")) ## Yeni working directory bu kaynak dosyasının lokasyonunda belirleniyor.

#============================ Gerekli Dosya Isimleri ===========================
# Analiz sirasinda gerekli olan kullanici tarafindan belirlenmis dosya isimleri.
#=========================
functions.folder.name <- "_functions"
figs.tabs.folder.name <- "_figs-tabs"

#============================= Gerekli Fonksiyonlar ============================
# Analiz sirasinda gerekli olan kullanici tarafindan yazilmis fonksiyonlarin yuklenmesi
#=========================
## "human_num" fonksiyonu ve benzerleri icin https://github.com/fdryan/R/blob/master/ggplot2_formatter.r linkine ya da "estimation_functions.R" dosyasina bakabilirsiniz.
## "lagpad" fonsiyonu icin estimation_functions.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "estimation_functions.R"))

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
Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "ggplot2", "latex2exp", "WDI", "fpp2", "datasets", "quantmod"))
#==========
## Load.Install(Package.Names = "readxl")
## Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#==========

#================================= Genel Bilgi =================================
# Bu bolumde cesitli kaynaklardan elde ettigimiz zaman serisi datalarini grafikle gostermeye calisacagiz.
# Daha sonra bu grafikleri pdf formatinda kaydedecegiz.

#================================== Grafikler ==================================
# Belirli alt basliklar icinde grafikler ve onlara bagli tablolar.
#=========================
# World Bank'ten Turkiye icin Kisi basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.PP.KD" GDP per capita (constant 2010 US$) datasini kullanacagiz.

data <- WDI(country = c("TR"), indicator = "NY.GDP.PCAP.KD", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "NY.GDP.PCAP.KD")] ## IStedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "GDP.PCAP")] ## Degiskenleri siraliyoruz.
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

# GDP.PCAP-TR grafigi.
temp <- data
variable <- "GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "GDP.PCAP-TR"

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

# GDP.PCAP-TR tablosu: Gecikme degerleriyle beraber
temp <- data
temp$GDP.PCAP <- round(temp$GDP.PCAP, 0) ## Tablonun daha iyi gorunmesi icin sayilar yuvarlaniyor.
temp$t <- 1:nrow(temp) ## Trend degiskeni ya da t indeks degeri ekleniyor.
temp <- temp[, c("Year", "t", "GDP.PCAP")] ## Degiskenler siraya dizildi.
temp$GDP.PCAP.t1 <- lagpad(temp$GDP.PCAP, 1) ## 1. Gecikme olusturuldu.
temp$GDP.PCAP.t2 <- lagpad(temp$GDP.PCAP, 2) ## 2. Gecikme olusturuldu.
temp$GDP.PCAP.t3 <- lagpad(temp$GDP.PCAP, 3) ## 3. Gecikme olusturuldu.
temp$GDP.PCAP.t4 <- lagpad(temp$GDP.PCAP, 4) ## 4. Gecikme olusturuldu.
for (i in 3:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i][1] <- paste0("\\red{", temp[which(!is.na(temp[, i])), i][1], "}")
}
for (i in 1:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i] <- paste0("$", temp[which(!is.na(temp[, i])), i], "$")
}
temp <- rbind(temp[1:6, ], "$\\vdots$", temp[(nrow(temp)-3):nrow(temp), ])
col.names <- c("Yıl", "$t$", "GSYH$_{t}$", "GSYH$_{t-1}$", "GSYH$_{t-2}$", "GSYH$_{t-3}$", "GSYH$_{t-4}$")
colnames(temp) <- col.names

temp.output.path <- paste0(figs.tabs.folder.name, "/", "GDP.PCAP-TR.tex")
temp.output <- print.xtable(xtable(temp), type = "latex", file = "", sanitize.text.function = function(x){x}, print.results = FALSE, NA.string = "NA", include.rownames = FALSE, include.colnames = TRUE, only.contents = TRUE, booktabs = TRUE, comment = FALSE) ## sanitize.text.function should be added to correctly run the output in latex.
table.correction <- "\\toprule\n"
cat(table.correction, temp.output, file = temp.output.path) ## Writing the table.

# Gr.GDP.PCAP-TR grafigi.
temp <- data
variable <- "Gr.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH Büyüme Oranı"
file.name <- "Gr.GDP.PCAP-TR"

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

# Gr.GDP.PCAP-TR tablosu: Gecikme degerleriyle beraber
temp <- data
temp$GDP.PCAP <- round(temp$GDP.PCAP, 0) ## Tablonun daha iyi gorunmesi icin sayilar yuvarlaniyor.
temp$Gr.GDP.PCAP <- round(temp$Gr.GDP.PCAP, 2) ## Tablonun daha iyi gorunmesi icin sayilar yuvarlaniyor.
temp$t <- 1:nrow(temp) ## Trend degiskeni ya da t indeks degeri ekleniyor.
temp <- temp[, c("Year", "t", "GDP.PCAP", "Gr.GDP.PCAP")] ## Degiskenler siraya dizildi.
temp$Gr.GDP.PCAP.t1 <- lagpad(temp$Gr.GDP.PCAP, 1) ## 1. Gecikme olusturuldu.
temp$Gr.GDP.PCAP.t2 <- lagpad(temp$Gr.GDP.PCAP, 2) ## 2. Gecikme olusturuldu.
temp$Gr.GDP.PCAP.t3 <- lagpad(temp$Gr.GDP.PCAP, 3) ## 3. Gecikme olusturuldu.
temp$Gr.GDP.PCAP.t4 <- lagpad(temp$Gr.GDP.PCAP, 4) ## 4. Gecikme olusturuldu.
for (i in 4:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i][1] <- paste0("\\red{", temp[which(!is.na(temp[, i])), i][1], "}")
}
for (i in 1:ncol(temp)) {
    temp[which(!is.na(temp[, i])), i] <- paste0("$", temp[which(!is.na(temp[, i])), i], "$")
}
temp <- rbind(temp[1:6, ], "$\\vdots$", temp[(nrow(temp)-1):nrow(temp), ])
col.names <- c("Yıl", "$t$", "GSYH$_{t}$", "GR$_{t}$", "GR$_{t-1}$", "GR$_{t-2}$", "GR$_{t-3}$", "GR$_{t-4}$")
colnames(temp) <- col.names

temp.output.path <- paste0(figs.tabs.folder.name, "/", "Gr.GDP.PCAP-TR.tex")
temp.output <- print.xtable(xtable(temp), type = "latex", file = "", sanitize.text.function = function(x){x}, print.results = FALSE, NA.string = "NA", include.rownames = FALSE, include.colnames = TRUE, only.contents = TRUE, booktabs = TRUE, comment = FALSE) ## sanitize.text.function should be added to correctly run the output in latex.
table.correction <- "\\toprule\n"
cat(table.correction, temp.output, file = temp.output.path) ## Writing the table.

#=========================
# World Bank'ten Kore icin Kisi basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.PP.KD" GDP per capita (constant 2010 US$) datasini kullanacagiz.

data <- WDI(country = c("KR"), indicator = "NY.GDP.PCAP.KD", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "NY.GDP.PCAP.KD")] ## IStedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year , "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "GDP.PCAP")] ## Degiskenleri siraliyoruz.
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

# GDP.PCAP-KR grafigi.
temp <- data
variable <- "GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "GDP.PCAP-KR"

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

# Gr.GDP.PCAP-KR grafigi.
temp <- data
variable <- "Gr.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH Büyüme Oranı"
file.name <- "Gr.GDP.PCAP-KR"

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

#=========================
# World Bank'ten secilmis bazi ulkeler icin basi Gayri Safi Yurtici Hasila Datasi (2010 US Doları)
#==========
Load.Install("WDI")
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olasi seriler ve kodlari. Biz "NY.GDP.PCAP.PP.KD" GDP per capita (constant 2010 US$) datasini kullanacagiz.

data <- WDI(country = c("TR", "KR", "MYS", "MEX", "GRC", "USA"), indicator = "NY.GDP.PCAP.KD", start = 1960, end = 2019, extra = FALSE)
data <- data[, c("year", "country", "NY.GDP.PCAP.KD")] ## Istedigimiz degiskenleri seciyoruz.
colnames(data) <- c("Year", "Country", "GDP.PCAP") ## Degiskenlere yeni isimler veriyoruz.
data$Country <- ifelse(data$Country == "Turkey", "Türkiye", ifelse(data$Country == "Greece", "Yunanistan", ifelse(data$Country == "Korea, Rep.", "Kore", ifelse(data$Country == "Mexico", "Meksika", ifelse(data$Country == "United States", "ABD", "Malezya")))))
data$Date <- as.Date(paste(data$Year , "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "Country", "GDP.PCAP")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Country, data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# GDP.PCAP-All-1 grafigi.
temp <- data
temp <- temp[temp$Country %in% c("Kore", "Meksika", "Malezya", "Türkiye"), ]
rownames(temp) <- 1:nrow(temp) ## Satir sayilarini duzenliyoruz.
variable <- "GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "GDP.PCAP-All-1"

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

# GDP.PCAP-All-2 grafigi.
temp <- data
variable <- "GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2010 ABD Doları)"
file.name <- "GDP.PCAP-All-2"

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
colnames(data) <- c("Year", "CPI") ## Degiskenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz.
data <- data[, c("Date", "Year", "CPI")] ## Degiskenleri siraliyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

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

# CPI grafigi.
temp <- data
variable <- "CPI"
variable.name <- "Tüketici Fiyatları İndeksi (2010 Baz Yılı)"
file.name <- "CPI"

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
file.name <- "Elec.Prod"

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
file.name <- "Ave.Temp"

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

#=========================
# İstanbul'a Verilen Temiz Su Miktarları (Aylik)
## Dataya su linkten ulasilabilir: https://data.ibb.gov.tr/dataset/istanbul-a-verilen-temiz-su-miktarlari
#==========
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
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayi tarihe gore siraliyoruz.
rownames(data) <- 1:nrow(data) ## Satir sayilarini duzenliyoruz.

# clean-water grafigi.
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
variable.name <- "Kapanış Fiyatı"
file.name <- "Stocks-1"

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
variable.name <- "Kapanış Fiyatı"
file.name <- "Stocks-2"

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
# Yahoo Finance: TR/USD doviz kuru (Gunluk)
#==========
Load.Install("quantmod")
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
file.name <- "TR-USD"

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
# Yahoo Finance: BTC/USD (Gunluk)
#==========
Load.Install("quantmod")
loadSymbols(Symbols = "BTC-USD", periodicity = "daily", return.class = "data.frame")

# BTC-USD grafigi.
file.name <- "BTC-USD"
cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))
candleChart(`BTC-USD`, theme = "white", TA = "addSMA()", subset = "2020-12::2021") ## With simple moving average
dev.off()

#=========================
# Pur Rassal Surec
#==========
n <- 200 ## Gozlem sayisi.
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
file.name <- "White.Noise"

cairo_pdf(paste0(figs.tabs.folder.name, "/", file.name, ".pdf"), family = "Times", width = 16, height = 9, onefile = FALSE)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    annotation_compass(label = latex2exp::TeX(paste0("$n = ", n)), position = "NE") +
    xlab("Zaman") + ylab(latex2exp::TeX(variable.name)) +
    # labs(title = variable.names) + ## Grafik icin isterseniz baslik eklenebilir.
    # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 24)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 24)) + theme(legend.text = element_text(size = 24))
g <- g + annotation_compass(label = latex2exp::TeX(paste0("$n = ", n)), position = "NE")
print(g)
dev.off()

#==================================== SON ======================================
