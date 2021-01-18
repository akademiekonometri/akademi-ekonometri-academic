#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#============================ VAR Ornek 2 (Detayli) ============================
#====== R'da Excel Uzantılı Data Yüklemek, Temizlemek, ve Grafik Cikarmak ======
#========== Birim Kok Testleri, VAR, Granger Nedensellik Testi ve IRF ==========
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
functions.folder.name <- "functions"
figs.tabs.folder.name <- "figs-tabs"

#============================= Gerekli Fonksiyonlar ============================
# Analiz sirasinda gerekli olan kullanici tarafindan yazilmis fonksiyonlarin yuklenmesi
#=========================
## "human_usd" fonksiyonu icin https://github.com/fdryan/R/blob/master/ggplot2_formatter.r linkine ya da "human_numbers.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "human_numbers.R")) ## Grafikler icin gerekli: Eksenlerdeki rakamlarin daha duzgun yazilabilmesi icin fonksiyonlarin yuklenmesi.

## "Seasonal.Adjust" fonksiyonu icin "seasonal_adjust.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "seasonal_adjust.R")) ## Degiskenler icindeki mevsimsel etkinin "X-13ARIMA-SEATS" algoritmasi ile cikartilmsi yontemi. Not: Sadece aylik ve ceyreklik data varsa kullanin. Yillik verilerde kullanilmaz.

## "ADF.Enders.Procedure" fonksiyonu icin "adf_enders_procedure.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "adf_enders_procedure.R")) ## Degiskenler icin Enders'in kitabinda bulunan ve ADF unit root testinin kullanildigi proseduru takip eder ve unit root testi yapar. Az sayida gozlem iceren data icin calismayabilir.

## "UR.Stationary.Tests" fonksiyonu icin "unit_root_stationary_tests.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "unit_root_stationary_tests.R")) ## Degiskenler icin unit root testleri (PP ve ERS) ve stationary testi (KPSS). Az sayida gozlem iceren data icin calismayabilir.

## "UR.Stationary.Results" fonksiyonu icin "unit_root_stationary_results.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "unit_root_stationary_results.R")) ## Degiskenler icin uygulanan "ADF Unit Root Procedure" ve unit root testlerini (PP ve ERS) ve stationary testini (KPSS) gorsel olarak gostermek amaciyla tabloya doker. Az sayida gozlem iceren data icin calismayabilir.

## "Coint.Pairwise.Tests.1" fonksiyonu icin "cointegration_pairwise_tests_1.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "cointegration_pairwise_tests_1.R")) ## Degiskenler icin uygulanan cointegration pairwise testleri. Phillips-Ouliaris (PO) - Asimetrik ve JO (Johansen) - Simetrik testleri. Az sayida gozlem iceren data icin calismayabilir. Not: Kullanicinin belirledigi anlamlilik duzeyine gore sonucu istatistikleri gostermeden verir.

## "Coint.Pairwise.Tests.2" fonksiyonu icin "cointegration_pairwise_tests_1.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "cointegration_pairwise_tests_2.R")) ## Degiskenler icin uygulanan cointegration pairwise testleri. Phillips-Ouliaris (PO) - Asimetrik ve JO (Johansen) - Simetrik testleri. Az sayida gozlem iceren data icin calismayabilir. Not: Test istatistiklerini ve hangi anlamlilik duzeylerine gore red edebilebilecegini gosterir.

## "Coint.Select" fonksiyonu icin "cointegration_rank_select.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "cointegration_rank_select.R")) ## Ikili ya da daha fazla degisken icin uygulanan johansen cointegration testinin sonucunu direkt olarak verir.

## "Granger.Causality.TYDL.1" fonksiyonu icin "granger_causality_TYDL_1.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "granger_causality_TYDL_1.R")) ## VAR ya da VECM analizinde kullanilacak olak degiskenler arasinda iki denklemli ve cok denklemli olarak VAR modelini kurup degiskenler arasinda asimetrik TYDL Granger Nedensellik testi sonuclarini direkt olarak verir. Not: Kullanicinin belirledigi anlamlilik duzeyine gore sonucu istatistikleri gostermeden verir.

## "Granger.Causality.TYDL.2" fonksiyonu icin "granger_causality_TYDL_2.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "granger_causality_TYDL_2.R")) ## VAR ya da VECM analizinde kullanilacak olak degiskenler arasinda iki denklemli ve cok denklemli olarak VAR modelini kurup degiskenler arasinda asimetrik TYDL Granger Nedensellik testi sonuclarini direkt olarak verir. Not: Test istatistiklerini ve hangi anlamlilik duzeylerine gore red edebilebilecegini gosterir.

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
Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "pastecs", "stargazer", "gridExtra", "scales", "ggplot2", "forecast", "aTSA", "urca", "FitAR", "vars", "aod"))
Load.Install(c("latexpdf")) ## Sadece bilgisayarinizda LaTeX kuruluysa kullanin.
Load.Install(c("seasonal")) ## Sadece seasonal adjustment (mevsimsel duzeltme) yapacaksiniz kullanin.
options(java.parameters = "-Xmx8000m") ## Bazen excel datalarini yuklerken memory sorunu ciktigi icin gerekli bir kod.
#==========
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
#==========

#================================= Genel Bilgi =================================
# Bu bolumde Turkiye Istatistik Kurumu'ndan (TUIK) elde ettigimiz 3 farkli degiskene (Reel Cari Acik - Real Current Account Deficit, Reel Gayri Safi Yurtici Hasila - Real GDP ve Reel Doviz Kuru - Real Exchange Rate) ait 2005-2019 arasindaki yillik verilerini kullanacagiz (baz yil 2005=100). Temel amacimiz bu verileri kullanarak VAR modeli uygulamak olacak. VAR modelinden elde ettigmiz tahminlerle son olarak Impulse Respons Function (Etki-Tepki Fonksiyonlari) hesaplayip bu fonksiyonlari grafige dokecegiz. Asagida bu bolumde yapilacak analizler sirasiyla verilmistir.
    ## 1. Datayi R'a yukleyip temizleme
    ## 2. Datayi transforme etme
    ## 3. Ozet data istatistiklerini bulma
    ## 4. Datayi zaman serine cevirme ve grafige dokme
    ## 5. Unit Root Tests (Birim Kok Testleri)
    ## 6. Cointegration Test (Es-butunlesme Testleri)
    ## 7. Granger Causality Test (Granger Nedensellik Testleri)
    ## 8. VAR and VECM Models (VAR ve VECM Modelleri)
    ## 9. Impulse Response Functions (Etki-Tepki Fonksiyonlari)

#========================= Datayi Yukleme ve Temizleme =========================
# Data dosyasinin adi.
file.name <- "CAD.xlsx"

# Ham datanin yuklenmesi: Yontem 1.
workbook <- loadWorkbook(filename = file.name, create = FALSE) ## Once excel workbook yukleniyor.
data <- readWorksheet(object = workbook, sheet = 1, startRow = 1, startCol = 1, header = TRUE, colTypes = "character") ## Sonra workbook icindeki belirli bir worksheet yukleniyor.

# Ham datanin yapisi.
str(data)

# Ham data dosyasinin RData formatinda disa aktarilmasi.
RData.Name <- "CAD"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Data manipulasyonlari.
colnames(data) <- c("Year", "RCAD", "RGDP", "RER") ## Degiskenlerin isimleri degistirildi.
data <- as.data.frame(sapply(data, function(numeric) as.numeric(numeric)), stringsAsFactors = FALSE) ## Tum degiskenlerin sinifi degistirildi ve numeric yapildi.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adli yeni bir degisken olusturuyoruz. Bu zaman serilerini grafige dokerken gunu belirten bu degisken isimize yarayacak.
data <- data[, c("Date", "Year", "RCAD", "RGDP", "RER")]

# Datanin siralanmasi.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ]
rownames(data) <- 1:nrow(data)

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "CAD_Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================== Datanin Mevsimselikten Arindirilmasi ====================
# Data yillik oldugundan, yani frekansi 1 oldugundan mevsimsellik etkisi yoktur.

#==================== Temizlenmis Data icin Transformasyon =====================
# Degiskenlerdeki buyume oranin yuzdesel olarak hesaplanmasi.
# Not: Analizde ilerledikce degiskenlerin unit root olma durumuna gore ya da model sonuclarini yorumlamada kolaylik olmasi amaciyla degiskenlerin buyume orani hesaplaniyor.
# Not: CAD (Cari Acik) degiskeni negatif degerler icerdiginden dogal logaritmasi alinamayacagi icin transformasyonu sadece buyume orani hesaplayarak yapiyoruz.
## Not: Daha oncekinden farkli olarak hesaplama yapilirken elde edilen yuzdesel buyume degerleri yuvarlanmiyor. Sadece ozet data istatistiklerini rapor ederken yuvarlama yapacagiz.
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

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "CAD_Processed_Trans"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#================= Data Istatistikleri Tablosunun Olusturulmasi =================
# Ozet data istatistiklerini gosterirken kullanmak istedigimiz degiskenleri siraliyoruz.
variables <- c("RCAD", "RGDP", "RER", "Gr.RCAD", "Gr.RGDP", "Gr.RER")
variable.names <- c("Real Current Account Deficit (RCAD)", "Real GDP (RGDP)", "Real Exchange Rate (RER)", "% Growth in Real Current Account Deficit (Gr.RCAD)", "% Growth in Real GDP (Gr.RGDP)", "% Growth in Real Exchange Rate (Gr.RER)")

# Varolan datayi kaybetmemek ve yanlislikla degistirmemek icin grafikleri olustururken temp adli datanin kopyasindan yararlanacagiz.
temp <- data
temp <- temp[, variables]

# Degiskenlerdeki eksik veri sayisini buluyoruz.
sapply(temp, function(missing) sum(is.na(missing))) ## Degiskenlerdeki eksik veri sayisi ayri ayri gosterildi.
sum(sapply(temp, function(missing) sum(is.na(missing)))) ## Tum degiskenlerdeki toplam eksik veri sayisi.

# Structure of data.
str(temp)

# Summary Statistics
temp.summary <- stat.desc(temp, basic = TRUE, desc = TRUE, norm = TRUE)
## Kurtosis: https://tr.wikipedia.org/wiki/Bas%C4%B1kl%C4%B1k ve https://en.wikipedia.org/wiki/Kurtosis
## Skewness: https://tr.wikipedia.org/wiki/%C3%87arp%C4%B1kl%C4%B1k ve https://en.wikipedia.org/wiki/Skewness
## Burada hesaplanan kurtosis istatistigi excess kurtosis olarak gecer ve (kurtosis - 3) olarak hesaplanir. Ardindaki mantik normal dagilimin kurtosisi 3 oldugundan normal dagilima gore nasil bir farka sahip oldugu gosterilmeye calisilir.
## Bir degiskenin normal dagilim yaptigini kabul etmek icin Tabachnick et al. (2001) skewness ve kurtosis icin ±2 degerlerinin kabul edilebilcegini soylerken, West et al. (1995) ise skewness icin ±2 kurtosis icin ise ±7 degerlerinin kabul edilebilcegini soyler.

temp.summary <- as.data.frame(t(temp.summary[c("nbr.val", "min", "max", "median", "mean", "std.dev", "skewness", "kurtosis"),])) ## Sectigimiz ozet data istatistikleri. Son olarak data frame objesine ceviriyoruz.
temp.summary <- cbind(variable.names, temp.summary) ## Degisken isimlerini de ilk sutuna yaziyoruz.
colnames(temp.summary) <- c("Variable", "\\# of Obs.", "Min.", "Max.", "Median", "Mean", "Std. Dev.", "Skewness", "Kurtosis") ## Sutun isimlerini degistiriyoruz.
row.names(temp.summary) <- 1:nrow(temp.summary) ## Satir isimlerini duzenliyoruz.

for (i in 2:ncol(temp.summary)) { ## Bazi sutunlarda yuvarlama yapiyoruz.
    if (i == 2) {
        temp.summary[, i] <- format(round(temp.summary[, i], 0), nsmall = 0)
    }
    if (i != 2) {
        temp.summary[, i] <- format(round(temp.summary[, i], 2), nsmall = 2)
    }
}

# Ozet data istatistigini gosteren data objesinin yapisi.
str(temp.summary)

#============ Ozet Data Istatistikleri Tablosunun Disa Aktarilmasi =============
# Simdi de ozet data istatistikleri tablosunu daha sonra kullanilmak amaciyla farkli formatlarda disa aktaralim.
file.name <- "summary_statistics" ## Disa aktarirken her seferinde dosya adini girmekle ugrasmamak adina dosya ismini iceren bir deger olusturuyoruz.
file.path <- paste0(figs.tabs.folder.name, "/", file.name) ## Dosyalari kaydetmek istedigimiz bilgisayarinizda var olan bir klasoru tanimliyoruz.

## stargazer paketi ile.
stargazer(temp.summary, summary = FALSE, type = "text", out = paste0(file.path, ".txt")) ## txt dosyasi olarak.
stargazer(temp.summary, summary = FALSE, type = "html", out = paste0(file.path, ".html")) ## html dosyasi olarak.

## gridExtra paketi ile.
png(paste0(file.path, ".png"), height = 50*nrow(temp.summary), width = 125*ncol(temp.summary)) ## png dosyasi olarak.
grid.table(temp.summary)
dev.off()

jpeg(paste0(file.path, ".jpeg"), height = 50*nrow(temp.summary), width = 125*ncol(temp.summary)) ## png dosyasi olarak.
grid.table(temp.summary)
dev.off()

pdf(paste0(file.path, ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9) ## pdf dosyasi olarak. Not: pdf olarak disa aktardigimizde yazi tipini ve encoding secebildigimize dikkat edin.
grid.table(temp.summary)
dev.off()

## latexpdf paketi ile.
as.pdf(temp.summary, stem = paste0(file.name, "-latex"), dir = "./figs-tabs") ## pdf dosyasi olarak

#================== Datayi Zaman Serisi Objesine Donusturmek ===================
# Temizlenmis ve transforme edilmis datanin yapisi.
str(data)

# Datanin zaman serisi objesine cevirilmesi.
data.ts <- ts(data[, 3:ncol(data)], frequency = 1, start = c(as.numeric(data$Year[1]))) ## Data objesi frekansi 11 olan zaman serisi objesine cevriliyor. Baslangic olarak ilk yil aliniyor.

# Zaman serisi objesine cevrilmis datanin yapisi.
str(data.ts)
dim(data.ts)
frequency(data.ts)
start(data.ts)
end(data.ts)

# Zaman serisi objesinde analizlerin yapilabilmesi icin datanin icinde bos degerlerin (NA) olmamasi gerekiyor. Ayrica ilerleyen analizlerde duzey formundaki degiskenleri mi yoksa buyume formundaki degiskenleri kullanacagimizi henuz bilmedigimizden datayi iki ayri parcaya boluyoruz. Ilk parca tamamen duzey formundaki degiskenleri kapsarken, ikinci parca tamamen buyume formundaki degiskenleri kapsiyor.
data1 <- data[, c("Date", "Year", "RCAD", "RGDP", "RER")] ## Birinci parce duzey formunda.
data2 <- data[-1, c("Date", "Year", "Gr.RCAD", "Gr.RGDP", "Gr.RER")] ## Ikinci parca buyume formunda oldugu icin ilk gozlem siliniyor.

# Iki parcanin da ayri olarak zaman serisi objesine cevirilmesi.
data1.ts <- ts(data1[, 3:ncol(data1)], frequency = 1, start = c(as.numeric(data1$Year[1]))) ## 1. Parca
data2.ts <- ts(data2[, 3:ncol(data2)], frequency = 1, start = c(as.numeric(data2$Year[1]))) ## 2. Parca

#====================== Grafik: Zaman Serisi Objesi ile ========================
# Tum degiskenleri bir grafik ile gostermek.
plot.ts(data.ts) # Tek bir grafik uzerinde farkli bolumlerde gostermek.
plot.ts(data.ts, plot.type = "single") # Tek bir grafik uzerinde ayni bolumde gostermek.

#======================= Grafik: Data Frame Objesi ile =========================
# Tum degiskenler icin grafikleri ayri ayri pdf dosyasina yazdirip disari aktaralim.
## PDF dosyalarini olusturuken yazilarin boyutunu buyuttuk va bazi degisimler yaptik.
temp <- data
variables <- c("RCAD", "RGDP", "RER", "Gr.RCAD", "Gr.RGDP", "Gr.RER")
variable.names <- c("Real Current Addount Deficit", "Real GDP", "Real Exchange Rate", "Growth in Real Current Account Deficit", "Growth in Real GDP", "Growth in Real Exchange Rate")
for (i in 1:length(variables)) {
    pdf(paste0(figs.tabs.folder.name, "/", variable.names[i], ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9)
    par(mar = c(2, 2, 2, 2))

    g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variables[i]], colour = "Variable"), linetype = 1, size = 1) +
        xlab("Time") + ylab(variable.names[i]) +
        # labs(title = variable.names[i]) + ## Grafik icin isterseniz baslik eklenebilir.
        scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
        scale_y_continuous(breaks = pretty(temp[ , variables[i]]), labels = human_num) +
        scale_colour_manual(name = "", labels = c(variable.names[i]), values = c("Variable" = "black")) +
        theme_grey() +
        theme(legend.position = "top") +
        theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 18)) +
        theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 18)) + theme(legend.text = element_text(size = 18))
    print(g)
    dev.off()
}

# Tum duzey degiskenlerini beraber ayni grafik uzerinde farkli bolumlerde gosterelim.
temp <- data
variables <- c("RCAD", "RGDP", "RER")
variable.names <- c("Real Current Addount Deficit", "Real GDP", "Real Exchange Rate")
temp <- reshape2::melt(temp, id.vars = c(grep("(Date)|(Year))", colnames(temp), value = TRUE)), measure.vars = variables, variable_name = "variable", na.rm = FALSE)
temp$variable <- factor(temp$variable, labels = variable.names)

pdf(paste0(figs.tabs.folder.name, "/", "levels", ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = value, colour = "Variable"), linetype = 1, size = 1) +
    xlab("Time") + ylab("Variable") +
    # labs(title = title) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    scale_y_continuous(labels = human_num) +
    # scale_y_continuous(breaks = pretty(temp$value)) + ## To have free Y axis scale for each variable this part is commented out.
    scale_colour_manual(name = "", labels = c("Variable"), values = c("Variable" = "black")) +
    facet_grid(variable ~ ., scales = "free_y", switch = "y") +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 18)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 18)) + theme(legend.text = element_text(size = 18)) +
    theme(strip.text.y = element_text(size = 12, colour = "black", angle = 90))
print(g)
dev.off()

# Tum buyume degiskenlerini beraber ayni grafik uzerinde farkli bolumlerde gosterelim.
temp <- data
variables <- c("Gr.RCAD", "Gr.RGDP", "Gr.RER")
variable.names <- c("Growth in Real Current Account Deficit", "Growth in Real GDP", "Growth in Real Exchange Rate")
temp <- reshape2::melt(temp, id.vars = c(grep("(Date)|(Year))", colnames(temp), value = TRUE)), measure.vars = variables, variable_name = "variable", na.rm = FALSE)
temp$variable <- factor(temp$variable, labels = variable.names)

pdf(paste0(figs.tabs.folder.name, "/", "growth", ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9)
par(mar = c(2, 2, 2, 2))

g <- ggplot(temp) + geom_line(aes(x = Date, y = value, colour = "Variable"), linetype = 1, size = 1) +
    xlab("Time") + ylab("Variable") +
    # labs(title = title) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    scale_y_continuous(labels = human_num) +
    # scale_y_continuous(breaks = pretty(temp$value)) + ## To have free Y axis scale for each variable this part is commented out.
    scale_colour_manual(name = "", labels = c("Variable"), values = c("Variable" = "black")) +
    facet_grid(variable ~ ., scales = "free_y", switch = "y") +
    theme_grey() +
    theme(legend.position = "top") +
    theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 18)) +
    theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 18)) + theme(legend.text = element_text(size = 18)) +
    theme(strip.text.y = element_text(size = 12, colour = "black", angle = 90))
print(g)
dev.off()

#=========== Mevsimsel Birim Kok Testleri (Seasonal Unit Root Tests) ===========
# Her degiskendeki birim kok durumunu ayri ayri test etmeden once, degiskenlerde mevsimsel birim kok olup olmadigini test etmek yararli olacaktir (belki bazi degiskenlerde mevsimsel farkin, seasonal differencing, alinmasi gerekli olabilir). Kullanilan iki yontem: Canova-Hansen, Canova and Hansen (1995) testi ve Osborn-Chui-Smith-Birchenhall, Osborn et. al (1988) testi.
# Note: Datanin frekansi 1 oldugundan yani data yillik oldugundan mevsimsel birim kok testine gerek yoktur.

temp.ts <- data1.ts

season.diff <- NULL
for (i in 1:ncol(temp.ts)) {
    ts <- temp.ts[, colnames(temp.ts)[i]]
    ch <- nsdiffs(ts, m = frequency(ts), test = c("ch")) ## Canova-Hansen
    if (ch != 0) {
        message(paste0(dQuote(colnames(temp.ts)[i]), " needs seasonal differencing in Canova-Hansen test."))
    }
    ocsb <- nsdiffs(ts, m = frequency(ts), test = c("ocsb")) ## Osborn-Chui-Smith-Birchenhall
    if (ocsb != 0) {
        message(paste0(dQuote(colnames(temp.ts)[i]), " needs seasonal differencing in Osborn-Chui-Smith-Birchenhall test."))
    }
    season.diff[i] <- ifelse(ch + ocsb == 2, 1, 0)
    if (season.diff[i] != 0) {
        message(paste0(dQuote(colnames(temp.ts)[i]), " needs seasonal differencing."))
    }
}
if (sum(season.diff) == 0) {
    message("None of the univariate time series needs seasonal differencing.")
}
print(season.diff) ## Degiskenlerde mevsimsel birim kok olup olmamasi sonucuna gore degiskenlerde mevsimsel ilk farklarin alinmasi ya da ayni haliyle birakilmasi gerekir.

#=================== Unit Root Tests - (Birim Kok Testleri) ====================
# Her degisken icin unit root test yapiyoruz.
## Kullanilan testler: ADF, PP ve KPSS. Burada dikkat edilmesi gereken ADF ve PP'de bos hipotez unit root derken alternatif hipotez ise stationary diyor.
## Tum testlerde belirli anlamlilik duzeyindeki hesapladigimiz test istatistiginin kritik degerden buyuk olup olmadigine bakiyoruz.
## Asagida bu testleri uygularken aTSA paketi kullanildi. Bu pakette sectiginiz gecikme uzunlugu 1 azaltilarak size veriliyor. Bunun nedeni ise modele giren gecikme uzunlugu p-1 olarak modele alinmis. Yorumlamada sonuc degismiyor.

# RCAD, Gr.RCAD ve ayni zamanda RCAD'nin ilk farklari icin.
## Sonuc: Duzey duragan degil, buyume, duzey ilk farklar ve buyume ilk farklar duragan.
ts1 <- data1.ts[, "RCAD"] ## Duzey
ts2 <- data2.ts[, "Gr.RCAD"] ## Buyume
ts1.diff <- diff(ts1) ## Duzey ilk farklar
ts2.diff <- diff(ts2) ## Buyume ilk farklar (Ikinci farklar olarak dusunulebilir)
adf.test(ts1, nlag = NULL, output = TRUE) ## Duzey: unit root (duragan degil).
adf.test(ts2, nlag = NULL, output = TRUE) ## Buyume: buyuk ihtimalle stationary (duragan).
adf.test(ts1.diff, nlag = NULL, output = TRUE) ## Duzey ilk farklar: buyuk ihtimalle stationary (duragan).
adf.test(ts2.diff, nlag = NULL, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

pp.test(ts1, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey: unit root (duragan degil).
pp.test(ts2, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume: buyuk ihtimalle stationary (duragan).
pp.test(ts1.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: buyuk ihtimalle stationary (duragan).
pp.test(ts2.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

kpss.test(ts1, lag.short = TRUE, output = TRUE) ## Duzey: stationary (duragan).
kpss.test(ts2, lag.short = TRUE, output = TRUE) ## Buyume: stationary (duragan).
kpss.test(ts1.diff, lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: stationary (duragan).
kpss.test(ts2.diff, lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

# RGDP, Gr.RGDP ve ayni zamanda RGDP'nin ilk farklari icin.
## Sonuc: Duzey, buyume ve duzey ilk farklar duragan degil, buyume ilk farklar duragan.
ts1 <- data1.ts[, "RGDP"] ## Duzey
ts2 <- data2.ts[, "Gr.RGDP"] ## Buyume
ts1.diff <- diff(ts1) ## Duzey Ilk farklar
ts2.diff <- diff(ts2) ## Buyume ilk farklar (Ikinci farklar olarak dusunulebilir)
adf.test(ts1, nlag = NULL, output = TRUE) ## Duzey: unit root (duragan degil).
adf.test(ts2, nlag = NULL, output = TRUE) ## Buyume: buyuk ihtimalle unit root (duragan degil).
adf.test(ts1.diff, nlag = NULL, output = TRUE) ## Duzey ilk farklar: unit root (duragan degil).
adf.test(ts2.diff, nlag = NULL, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

pp.test(ts1, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey: unit root (duragan degil).
pp.test(ts2, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume: buyuk ihtimalle unit root (duragan degil).
pp.test(ts1.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: unit root (duragan degil).
pp.test(ts2.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

kpss.test(ts1, lag.short = TRUE, output = TRUE) ## Duzey: stationary (duragan).
kpss.test(ts2, lag.short = TRUE, output = TRUE) ## Buyume: stationary (duragan).
kpss.test(ts1.diff, lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: stationary (duragan).
kpss.test(ts2.diff, lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

# RER, Gr.RER ve ayni zamanda RER'nin ilk farklari icin.
## Sonuc: Duzey duragan degil, buyume, duzey ilk farklar ve buyume ilk farklar duragan.
ts1 <- data1.ts[, "RER"] ## Duzey
ts2 <- data2.ts[, "Gr.RER"] ## Buyume
ts1.diff <- diff(ts1) ## Duzey Ilk farklar
ts2.diff <- diff(ts2) ## Buyume ilk farklar (Ikinci farklar olarak dusunulebilir)
adf.test(ts1, nlag = NULL, output = TRUE) ## Duzey: unit root (duragan degil).
adf.test(ts2, nlag = NULL, output = TRUE) ## Buyume: stationary (duragan).
adf.test(ts1.diff, nlag = NULL, output = TRUE) ## Duzey ilk farklar: stationary (duragan).
adf.test(ts2.diff, nlag = NULL, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

pp.test(ts1, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey: unit root (duragan degil).
pp.test(ts2, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume: stationary (duragan).
pp.test(ts1.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: stationary (duragan).
pp.test(ts2.diff, type = c("Z_rho"), lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

kpss.test(ts1, lag.short = TRUE, output = TRUE) ## Duzey: unit root (duragan degil).
kpss.test(ts2, lag.short = TRUE, output = TRUE) ## Buyume: stationary (duragan).
kpss.test(ts1.diff, lag.short = TRUE, output = TRUE) ## Duzey ilk farklar: stationary (duragan).
kpss.test(ts2.diff, lag.short = TRUE, output = TRUE) ## Buyume ilk farklar: stationary (duragan).

## Genel sonuc:
### 1. Tum degiskenler duzey formunda unit root.
### 2. Hatta bazi degiskenler ilk farklar formunda hala unit root. Bu durum duzey formundaki degiskenlerin I(2) oldugunu gosteriyor.
### 3. Buyume formundaki bazi degiskenler (ilk farklar alinmis hali olarak da dusunulebilir), I(1) yani unit root formunda.
### 4. Buyume ilk farklar formunda ise tum degiskenler I(0) yani stationary durumunda.
### 5. Yukaridaki sonuclar nedeniyle duzey formunda degiskenlerin I(2) oldugu anlasilmistir. Bu nedenle asil degiskenlerimizin buyume formundaki degiskenler (yani I(1)) oldugunu varsayacagiz.

## Daha sonra yapacagimiz cointegration testlerin sonucunda:
### 1. Eger buyume formundaki degiskenler arasinda cointegration bulunursa VECM modelini buyume formundaki degiskenlerle tahmin edecegiz. VECM modelinde degiskenlerin I(I) olmasi gerekir. Not: Buyume formundaki degiskenleri I(1) bulmustuk.
### 2. Eger buyume formundaki degiskenler arasinda cointegration bulunmaz ise bu durumda VAR modelini buyume ilk farklar formundaki degiskenlerle (yani I(0) olan degiskenlerle) tahmin edecegiz.

#=========================
# Detayli unit root ve stationary testleri.
## Detayli bilgi icin "—note_unit-root_stationary.R" dosyasini inceleyin.
## urca adli R paketindeki unit root testleri kullanarak asagidaki 3 fonksiyonu hazirladim. Bu fonksiyonlari kullanarak ADF, PP, ERS ve KPSS testlerini uygulayabilir ve bunlarin sonuclarini tablo olarak kolayca yazdirabilirsiniz.
## urca paketi unit root testleri icin size en genis ve dogru bilgiyi verir. Fakat ozellikle ADF testlerinde yorumlamasi zor oldugundan genelde tercih edilmez. Asagida verilen fonksiyonlarin icerigine bakarak ADF testinde izlemeniz gereken test surecini anlayabilirsiniz.
## Asagidaki fonksiyonlarin kullanilabilmesi icin gozlem sayinizin buyuk olmasi beklenir.

# ADF.Enders.Procedure(data2.ts, Significance.Level = 5, Diff = FALSE, Reshape = TRUE) ## Buyume formundaki degiskelere uygulandi.
# adf.tests ## Yukaridaki fonksiyonun sonucu
# UR.Stationary.Tests(data2.ts, Significance.Level = 5, Diff = FALSE, Reshape = TRUE) ## Buyume formundaki degiskelere uygulandi.
# ur.stationary.tests ## Yukaridaki fonksiyonun sonucu
# UR.Stationary.Results(data2.ts, Significance.Level = 5, Diff = FALSE, Table = TRUE) ## Buyume formundaki degiskelere uygulandi.
# ur.stationary.results ## Yukaridaki fonksiyonun sonucu
#=========================

#======== Cointegration Pairwise Tests - (Esbutunlesme Ikili Testleri) =========
# Her ikili degisken arasinda cointegration testleri yapiyoruz.
## Kullanilan testler: Engle-Granger test (EG) - Asimetrik, Phillips-Ouliaris (PO) - Asimetrik ve JO (Johansen) - Simetrik. Burada dikkat edilmesi gereken PO simetrik degil ve JO ise simetriktir.
## PO testinde type argumaninda Pu yerine Pz kullanildiginda denklemin sol tarafinda hangi degiskenin oldugu fark etmez. Eger simetrik bir PO testi yapmak isterseniz bunu kullanabilirsiniz.
## Tum testlerde belirli anlamlilik duzeyindeki hesapladigimiz test istatistiginin kritik degerden buyuk olup olmadigine bakiyoruz.
## Asagida bu testleri uygularken urca ve aTSA paketleri kullanildi.
## Not: Asagidaki cointegration pairwise (esbutunlesme ikili) testleri ben sadece "Gr.RCAD" ve "Gr.RCAD" arasinda uyguladim. Eger siz isterseniz diger ikili degiskenler icin de bu testlerin aynisini uygulayabilirsiniz.

# Gr.RCAD ve Gr.RGDP
## Sonuc: Gr.RCAD ve Gr.RGDP arasinda iki yonde de cointegration yok.
### aTSA paketi ile Engle-Granger test (EG) - Asimetrik
ts1 <- data2.ts[, c("Gr.RGDP")]
ts2 <- data2.ts[, c("Gr.RCAD")]
coint.test(ts1, ts2, d = 0, nlag = NULL, output = TRUE) ## RCAD'den RGDP'ye: Cointegration yok.
coint.test(ts2, ts1, d = 0, nlag = NULL, output = TRUE) ## RGDP'den RCAD'ye: Cointegration yok.

### urca paketi ile Phillips-Ouliaris (PO)
#### Gr.RCAD'den Gr.RGDP'ye dogru.
ts <- data2.ts[, c("Gr.RCAD", "Gr.RGDP")]
po <- ca.po(ts, demean = "constant", lag = "short", type = "Pu")
po@teststat; po@cval ## Gr.RCAD'den Gr.RGDP'ye: Cointegration yok.

#### Gr.RGDP'den Gr.RCAD'ye dogru.
ts <- data2.ts[, c("Gr.RGDP", "Gr.RCAD")]
po <- ca.po(ts, demean = "constant", lag = "short", type = "Pu")
po@teststat; po@cval ## Gr.RGDP'den Gr.RCAD'ye: Cointegration yok.

### urca paketi ile Johansen (JO)
ts <- data2.ts[, c("Gr.RCAD", "Gr.RGDP")]
eq.num <- ncol(ts)
jo.lag <- VARselect(ts, lag.max = frequency(ts), type = "const", season = NULL)
jo.lag$selection ## Tum degerlendirmelere gore secilmesi gereken gecikme uzunlugu (lag) 1. Fakat Johansen cointegration testini uygulayabilmek icin secilmesi gerekn minumum lag 2. Bu nedenle johansen testini yaparken lag'i 2 olarak seciyoruz.
jo <- ca.jo(ts, type = "trace", ecdet = "const", K = 2, spec = "transitory") ## Johansen cointegration testte Lag = 1 olamaz. Cunku secilen bu lag degeri VAR'daki lag degeridir. VECM'da lag - 1 kullanilacagindan Lag = 1 olamaz. Bu nedenle Lag = 2 olarak aliniyor.
summary(jo) ## Gr.RGDP ve Gr.RCAD arasinda cointegration yok.
Coint.Select(Johansen.Object = jo, Equation.Number = eq.num) ## Benim yazdigim Coint.Select fonksiyonunu kullanarak otomatikman johansnen test sonucunu bulabiliriz.

#=========================
# Detayli cointegration pairwise testleri.
## Detayli bilgi icin "—note_cointegration_pairwise.R" dosyasini inceleyin.
## urca adli R paketindeki cointegration testleri kullanarak asagidaki 2 fonksiyonu hazirladim. Bu fonksiyonlari kullanarak Phillips-Ouliaris (PO) cointegration - Asimetrik ve JO (Johansen) cointegration - Simetrik testlerini uygulayabilir ve bunlarin sonuclarini tablo olarak kolayca yazdirabilirsiniz.

Coint.Pairwise.Tests.1(data2.ts, Significance.Level = 5, PO.Lag = "Short", Johansen.Lag = "BIC", Model.Type = "Constant", Johansen.Type = "Trace", Season = NULL) ## %1 anlamlilik duzeyinde sadece johansen testi ile Gr.RGDP ve Gr.RER arasinda cointegration bulunmus. DS (difference stationary), LS (level stationary), N (not cointegrated), C (cointegrated).
coint.pairwise.results ## Ust ucgende ikili denklemden elde edilen Johansen Cointegration testi - Simetrik sonucu varken alt ucgende ikili denklemden elde edilen Phillips-Ouliaris (PO) - Asimetrik testi sonucu var.

Coint.Pairwise.Tests.2(data2.ts, PO.Lag = "Short", Johansen.Lag = "BIC", Model.Type = "Constant", Johansen.Type = "Trace", Season = NULL) ## %1 anlamlilik duzeyinde sadece johansen testi ile Gr.RGDP ve Gr.RER arasinda cointegration bulunmus. DS (difference stationary), LS (level stationary), N (not cointegrated), C (cointegrated).
coint.pairwise.po.results ## Phillips-Ouliaris (PO) cointegration - Asimetrik
coint.pairwise.jo.results ## JO (Johansen) cointegration - Simetrik

## Genel sonuc:
### 1. Phillips-Ouliaris (PO) cointegration - Asimetrik testinin sonucuna gore hic bir iki degisken arasinda cointegration yok, yani VAR kullan.
### 2. JO (Johansen) cointegration - Simetrik testinin sonucuna gore:
    #### a. Gr.RCAD ve Gr.RGDP kullanilarak yapilan modelde cointegration (r) = 0 ==> DS (differance stationary - ilk farklar duragan), yani first difference VAR (ilk farklar alinmis).
    #### b. Gr.RCAD ve Gr.RER kullanilarak yapilan modelde cointegration (r) = 1 ==> C (cointegration var), yani VECM kullan.
    #### c. Gr.RGDP ve Gr.RER kullanilarak yapilan modelde cointegration (r) = 2 ==> LS (duragan), yani VAR kullan.
### 3. Iki degiskenli ve 2 denklemli modellerde ne kadar cointegration bulunduysa Iki degiskenli ve 2 denklemli modellerde en az o sayida TYDL Granger nedensellik (tek bir yonde de olsa) bulmamiz lazim.

#=============== Cointegration Tests - (Esbutunlesme Testleri) =================
# Coklu denklem sistemi icin cointegration testi yapiyoruz.
## Kullanilan testler: JO (Johansen) - Simetrik. Burada dikkat edilmesi gereken PO simetrik degil ve JO ise simetriktir.
## Tum testlerde belirli anlamlilik duzeyindeki hesapladigimiz test istatistiginin kritik degerden buyuk olup olmadigine bakiyoruz.
## Asagida bu testleri uygularken urca paketi kullanildi.

# 3 degiskenli ve 3 denklemli model icin.
ts <- data2.ts ## Buyume formundaki degiskenler kullaniliyor.
eq.num <- ncol(ts) ## Toplam denklem sayisi. Tum degiskenler endojen (endogenous) olarak alindi.
jo.lag <- VARselect(ts, lag.max = frequency(ts), type = "const", season = NULL)
jo.lag$selection ## Tum degerlendirmelere gore secilmesi gereken gecikme uzunlugu (lag) 1.
lag <- jo.lag$selection[[1]] ## AIC ile secilmis lag
lag <- lag + 1 ## Johansen cointegration testini uygulayabilmek ve VECM yapabilmek icin secilmesi gerekn minumum lag 2. Bu nedenle johansen testini yaparken lag'i 2 olarak seciyoruz.
jo <- ca.jo(ts, type = "trace", ecdet = "const", K = lag, spec = "transitory", season = NULL, dumvar = NULL) ## Johansen cointegration testte Lag = 1 olamaz. Cunku secilen bu lag degeri VAR'daki lag degeridir. VECM'da lag - 1 kullanilacagindan Lag = 1 olamaz. Bu nedenle Lag = 2 olarak aliniyor.
summary(jo) ## Gr.RGDP, Gr.RCAD ve Gr.RCER arasinda %1 anlamlilik duzeyinde 1 tane cointegration var.
Coint.Select(Johansen.Object = jo, Equation.Number = eq.num) ## Benim yazdigim Coint.Select fonksiyonunu kullanarak otomatikman johansnen test sonucunu bulabiliriz.

## Genel sonuc:
### 1. 3 degiskenli 3 denklemli modelde cointegration (r) = 1 ==> C (cointegration var), yani VECM kullan.
### 2. 3 degiskenli 3 denklemli modelde en az 1 cointegration bulundugu icin 3 degiskenli 3 denklemli modelde en azindan 1 tane TYDL Granger nedensellik (tek bir yonde de olsa) bulmamiz lazim.

#========== TYDL Granger Causality Pairwise and Multivariate Tests =============
# Detayli TYDL Granger Causality pairwise ve multivariate (cok denklemli) testleri.
# Her ikili degisken arasindaki TYDL Granger Nedensellik testi ve coklu denklem sistemi icin TYDL Granger Nedesellik testi yapiyoruz.
## Kullanilan testler: TYDL Granger Nedensellik testi - Asimetrik (R icinde TYDL Granger nedensellik testi yok bu nedenle benim tarafimdan yazildi).
## Detayli bilgi icin "—note_granger_causality_TYDL.R" dosyasini inceleyin.
## vars ve aod adli R paketlerindeki fonksiyonlari kullanarak asagidaki 2 fonksiyonu hazirladim. Bu fonksiyonu kullanarak TYDL Granger Nedensellik testlerini uygulayabilir ve bunlarin sonuclarini tablo olarak kolayca yazdirabilirsiniz.

ts <- data2.ts ## Buyume formundaki degiskenler kullaniliyor.
int.numbers <- c(rep(1, ncol(ts))) ## Her degisken icin integration seviyesi, yani I(1) ise 1 degeri veriliyor. I(0) ise 0 degeri veriliyor. Kullanici bu degerleri unit root ve stationary testlerinin sonuclarina bakarak bulmali ve sirasi ile yazmali. Buyume degiskenleri kullanildigindan ve I(1) olduklarindan in.numbers 1 degerlerinden olusuyor.
Granger.Causality.TYDL.1(data2.ts, Significance.Level = 5, Int.Numbers = int.numbers, Lag.Selection = "BIC", Divide.Lag.Max = 1, Model.Type = "Constant", Season = NULL, Exogen = NULL, Result.Type = "Both")
granger.TYDL.multivariate.pairwise.results ## Ust ucgende cok denklemli modelden elde edilen TYDL Granger nedensellik testi sonucu varken alt ucgende ikili denklemden elde edilen TYDL Granger nedensellik testi sonucu var.

Granger.Causality.TYDL.2(data2.ts, Int.Numbers = int.numbers, Lag.Selection = "BIC", Divide.Lag.Max = 1, Model.Type = "Constant", Season = NULL, Exogen = NULL, Result.Type = "Both")
granger.TYDL.pairwise.results ## Iki denklemli modelden elde edilen TYDL Granger nedensellik testi istatistikleri.
granger.TYDL.multivariate.results ## Cok denklemli modelden elde edilen TYDL Granger nedensellik testi istatistikleri.

## Genel sonuc:
### 1. TYDL Granger Causality pairwise: "==>" granger nedenselligi belirtir.
    #### a. Gr.RGDP ==> GR.RCAD, Gr.RGDP ==> GR.RER, Gr.RER ==> GR.RGDP
### 2. TYDL Granger Causality multivariate: "==>" granger nedenselligi belirtir.
    #### a. Gr.RGDP ==> GR.RCAD, Gr.RGDP ==> GR.RER, Gr.RER ==> GR.RCAD
### 3. Sonuc olarak pairwise ve multivariate'de benzer sonuclar bulduk.
### 4. Multivariate'de 1 cointegration bulmustuk. Granger nedensellik sonucu da bunu dogruluyor 3 adet nedensellik bulduk. Dikkat ceken ise Gr.RGDP'den diger degiskenlere gidis var.
### 5. Buyume degiskenleri I(1) oldugundan ve aralarindan cointegration bulundugundan dolayi tahminde VECM modeli kullanilacaktir.

#==================================== VECM =====================================
# Coklu denklem sistemi (3 degiskenli ve 3 denklemli model) icin VECM modelini uyguluyoruz.
## Tum testlerde belirli anlamlilik duzeyindeki hesapladigimiz test istatistiginin kritik degerden buyuk olup olmadigine bakiyoruz.
## Asagidaki kodlarda urca paketi kullanildi.

# Kullanilacak data.
ts <- data2.ts ## Buyume formundaki degiskenler kullaniliyor.
eq.num <- ncol(ts) ## Toplam denklem sayisi. Tum degiskenler endojen (endogenous) olarak alindi.

# VECM kullanilacak lag (gecikme uzunlugunun) belirlenmesi.
jo.lag <- VARselect(ts, lag.max = frequency(ts), type = "const", season = NULL)
jo.lag$selection ## Tum degerlendirmelere gore secilmesi gereken gecikme uzunlugu (lag) 1.
lag <- jo.lag$selection[[1]] ## AIC ile secilmis lag (gecikme uzunlugu).
lag <- lag + 1 ## Johansen cointegration testini uygulayabilmek ve VECM yapabilmek icin secilmesi gerekn minumum lag 2. Bu nedenle johansen testini yaparken lag'i 2 olarak seciyoruz.

# VECM estimation.
jo <- ca.jo(ts, type = "trace", ecdet = "const", K = lag, spec = "transitory", season = NULL, dumvar = NULL) ## VECM-Constant (Case 2 in Johansen (1995): restricted constant and no trend): No linear trend in the level data nor in the CI relations. The only deterministic component is intercept in CI relations.
summary(jo) ## Gr.RGDP, Gr.RCAD ve Gr.RCER arasinda %1 anlamlilik duzeyinde 1 tane cointegration var.
Coint.Select(Johansen.Object = jo, Equation.Number = eq.num) ## Benim yazdigim Coint.Select fonksiyonunu kullanarak otomatikman johansnen test sonucunu bulabiliriz. Buldugumuz sonucu yani 1 tane cointegration durumunu VECM'de yerine koyacagiz, yani kisitlanmis VECM tahmin edecegiz.
coint <- 1 ## Secilen cointegration. Lag BIC ile secildikten sonra, Johansen cointegration testi yapildi ve %1 seviyesinde 1 tane cointegration durumuna karar verildi.

# Kisitlanmis VECM (Restricted VECM)
## Kisitlanmis VECM'yi SEKK ile tahmin ediyoruz ve her denklem icin kisitlanmis VECM tahminlerini gosteriyoruz.
vecm.rest <- cajorls(johansen, r = coint, reg.number = NULL) ## Kisitlanmis VECM (Restricted VECM). Yani VECM, cointegration = 1 kisiti ile yeniden tahmin ediliyoe ve data icindeki ilk degiskene (Gr.RCAD) gore long-run relationship (uzun-donemli) iliski normalize ediliyor.
vecm.rest.summary <- summary(vecm.rest$rlm) ## Her denklem icin kisitlanmis VECM tahminleri.
vecm.rest.summary

# VECM tahminini VAR donusturuyoruz.
## Kisitlanmis VECM'yi VAR'a donduruyoruz (bu islem sirasinda daha once buldugumuz cointegration degerini kisitlama yapmak icin belirtiyoruz) ve her denklem icin Transformed VAR formunda VECM katsayilarini gosteriyoruz.
vecm.to.var <- vec2var(z = jo, r = coint) ## VECM'yi VAR'a donusturuluyor.



# Diagnostic Test (Teshis Testleri)
# Daha sonra yapilacak.



# Impulse Response Functions (Etki Tepki Fonksiyonlari)
irfs <- vars::irf(vecm.to.var, impulse = NULL, response = NULL, n.ahead = 10, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 100, seed = 123456)
plot(irfs)

IRF.vecm2var(vecm.to.var, impulse = NULL, response = NULL, n.ahead = 10, IRF.Type = "Generalized", Unit.or.Std = "Unit", DF.Correct = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 100, seed = 123456)

#==================================== SON ======================================
