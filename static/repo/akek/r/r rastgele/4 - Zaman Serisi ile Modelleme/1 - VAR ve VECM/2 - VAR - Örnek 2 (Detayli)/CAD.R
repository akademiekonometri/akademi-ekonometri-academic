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
Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "pastecs", "vars"))
Load.Install(c("latexpdf")) ## Sadece bilgisayarinizda LaTeX kuruluysa kullanin.
Load.Install(c("seas")) ## Sadece seasonal adjustment (mevsimsel duzeltme) yapacaksiniz kullanin.
options(java.parameters = "-Xmx8000m") ## Bazen excel datalarini yuklerken memory sorunu ciktigi icin gerekli bir kod.
#==========
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
#==========

#============================= Gerekli Fonksiyonlar ============================
# Analiz sirasinda gerekli olan kullanici tarafindan yazilmis fonksiyonlarin yuklenmesi
#=========================
## "human_usd" fonksiyonu icin https://github.com/fdryan/R/blob/master/ggplot2_formatter.r linkine ya da "human_numbers.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", "functions", "/", "human_numbers.R")) ## Grafikler icin gerekli: Eksenlerdeki rakamlarin daha duzgun yazilabilmesi icin fonksiyonlarin yuklenmesi.

## "Seasonal.Adjust" fonksiyonu ici "seasonal_adjust.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", "functions", "/", "seasonal_adjust.R")) ## Degiskenler icindeki mevsimsel etkinin "X-13ARIMA-SEATS" algoritmasi ile cikartilmsi yontemi. Not: Sadece aylik ve ceyreklik data varsa kullanin. Yillik verilerde kullanilmaz.

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
RData.Name <- "CPI_PPI_Processed_Trans"
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
file.path <- paste0("figs-tabs", "/", file.name) ## Dosyalari kaydetmek istedigimiz bilgisayarinizda var olan bir klasoru tanimliyoruz.

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
dev.off()

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
data1 <- data[, c("Date", "Year", "RCAD", "RGDP", "RER")]
data2 <- data[-1, c("Date", "Year", "Gr.RCAD", "Gr.RGDP", "Gr.RER")]

# Datanin zaman serisi objesine cevirilmesi.
data1.ts <- ts(data1[, 3:ncol(data1)], frequency = 1, start = c(as.numeric(data1$Year[1])))
data2.ts <- ts(data2[, 3:ncol(data2)], frequency = 1, start = c(as.numeric(data2$Year[1])))








#================= Regresyon Analizi icin Datanin Hazirlanmasi =================
# Daha sonraki adimlarda regresyon analizi yapabilmek icin temizlenmis data icinden bazi degiskenleri secelim.
## Secilen degiskenler: "NY.GDP.MKTP.CD" (GDP - GSYH Simdiki US Fiyatlariyla), "NE.GDI.TOTL.ZS" (Capital - Sermaye Stogu % GSYH olarak), "DT.DOD.DECT.CD" (Ext.Debt - Dis Borc Stogu Simdiki US Fiyatlariyla), "NE.EXP.GNFS.ZS" (Export - Ihracat % GSYH olarak) ve son olarak "SP.POP.TOTL" (Population - Toplam Nufus GSYH ve Dis Borc Stogu'ndaki nufusun etkisini yok etmek icin)
## Not: GDP ve Ext.Debt simdiki US fiyatlariyla oldugu icin bu degiskenlerin icindeki fiyat etkisi cikarilmali yani fiyat endeksini kullanarak nominal formda olan bu degiskenler reel forma donusturulmeli. Fakat datanin icinde GSYH Deflatoru ya da TUFE olmadigindan bu kisim bilerek atlanmistir.
series <- c("Year", "NY.GDP.MKTP.CD", "NE.GDI.TOTL.ZS", "DT.DOD.DECT.CD", "NE.EXP.GNFS.ZS", "SP.POP.TOTL") ## Secilen degiskenlerin kodlari vektor halinde yazildi.
series.names <- c("Year", "GDP", "Per.Capital", "Ext.Debt", "Per.Export", "Population") ## Secilen degiskenlerin degistirilmis isimleri vektor halinde yazildi.
temp <- test[, series] ## test datasinin secilen degiskenlere gore alt kumesi alindi ve yeni dataya temp adi verildi.
colnames(temp) <- series.names ## temp datasinin sutun isimleri degistirildi.
temp <- temp[complete.cases(temp), ] ## temp datasindaki tum NA iceren satirlar silindi.

temp$GDP.pC <- temp$GDP / temp$Population ## Kisi basi GDP.
temp$Ext.Debt.pC <- temp$Ext.Debt / temp$Population ## Kisi basi Ext.Debt.

temp$Ln.GDP.pC <- log(temp$GDP.pC) ## Model icinde yorumlamak daha kolay olsun diye GDP.pC degiskeninin dogal logaritmasi aliniyor.
temp$Ln.Ext.Debt.pC <- log(temp$Ext.Debt.pC) ## Model icinde yorumlamak daha kolay olsun diye Ext.Debt.pC degiskeninin dogal logaritmasi aliniyor.

# Islenmis datayi data olarak kaydediyoruz.
data <- temp

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "workbank2_Processed_Trans"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.










#===================== Coklu Dogrusal Regresyon ile Tahmin =====================
# Coklu dogrusal regresyonla model tahmini.
## Zaman serisi datasinin kesinlikle SEKK yontemi kullanarak coklu regresyonla tahmin edilmemesi gerekir. Bunun nedeni zaman serisi datasinda siklikla gorulen otokorolasyondur. Zaman serisi datasinda siklikla pozitif otokorelasyon gorulur ve bunun sonucunda parametre tahmincilerinin varyansi duser, t degerleri buyur (yani H0 hipotezi yanlis olarak red edilir ve parametre tahmincileri gercekte istatistiki olarak anlamli olmamalarina ragmen anlamli sonucuna varilir), modelin geneli icin yapilan F testinde F degeri buyur (yani H0 hipotezi yanlis olarak red edilir ve model genel olarak anlamli olmamasina ragmen anlamli sonucuna varilir) ve R2 degeri buyur. Kisacasi cikan sonuclara guvenilmemesi gerekir.
## Fakat simdilik sadece gosterim olmasi acisindan bir bu yontemi kullanacagiz.
model <- lm(data = data, formula = Ln.GDP.pC ~ Per.Capital + Per.Export + Ln.Ext.Debt.pC, singular.ok = FALSE)
summary(model)

#================= VAR (Vector Autoregressive Model) ile Tahmin ================
# VAR (Vector Autoregressive Model) ile model tahmin oncesi bazi duzenlemeler.
data <- data[, c("Year", "Ln.GDP.pC", "Per.Capital", "Per.Export", "Ln.Ext.Debt.pC")] ## Oncelikle sadece VAR'da kullanacagimiz degiskenler icin datanin alt kumesini alalim.
data.ts <- ts(data[, grep("(Year)", colnames(data), invert = TRUE)], frequency = 1, start = data$Year[1]) ## Alt kumesi alinmis datayi, "Year" degiskenini datanin icinden atip, frekansini ve baslangic zamanini secerek datayi artik zaman serisi datasina cevirelim. Son olarak zaman serisine cevrilmis datayi data.ts olarak kaydedelim.
str(data.ts) ## temp.ts'nin yapisi.
frequency(data.ts) ## temp.ts'nin frekansi.
class(data.ts) ## temp.ts'nin sinifi.
data.ts ## temp.ts'nin gorunumu.

# VAR (Vector Autoregressive Model) ile model icin gecikme uzunlugu ve deterministic degisken secimi.
lag <- 1 ## Gecikme uzunlugu.
deterministic.selected <- "const" ## Deterministik terim.

# VAR (Vector Autoregressive Model) ile model tahmin.
var.est <- vars::VAR(data.ts, p = lag, type = deterministic.selected, season = NULL, exogen = NULL) ## VAR tahmin edildi.
summary(var.est) ## Modelin ozeti.

#==================================== SON ======================================
