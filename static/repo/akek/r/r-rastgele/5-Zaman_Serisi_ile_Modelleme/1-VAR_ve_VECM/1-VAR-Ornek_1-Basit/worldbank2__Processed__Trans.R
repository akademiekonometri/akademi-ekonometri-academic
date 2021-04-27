#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.netlify.app/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#=============================== Bizi Takip Edin ===============================

#============================ VAR - Ornek 1 (Basit) ============================
# Notlar:
#
## R'da Excel Uzantılı Data Yüklemek, Temizlemek, CDR ve VAR ile Tahmin
## Bu yazıda kullandığımız datayı (eger varsa) web sitemizdeki ilgili bölümde bulabilirsiniz.
## Aşağıdaki R kodu, öncelikle gerekli R paketlerini yüklüyor ve daha sonra working directory'yi bilgisayarınızda bu kaynak dosyasının bulunduğu lokasyona göre değiştiriyor. Son olarak ise ilgili R kodunu çalıştırıyor.

#=============================== Gerekli Paketler ==============================
# Tek bir adımda gerekli paketlerin yüklenmesi ve kurulması.
# Bu adimi daha kolay hale getirmek için öncelikle "Load.Install" fonksiyonunu tanımlayalım.
#===
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
#===
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "vars", "pastecs"))
#===
## Load.Install(Package.Names = "readxl")
## Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#===

#======================== Working Directory'yi Belirlemek ======================
# Working directory'nin bu kaynak dosyasının olduğu lokasyonda belirlenmesi.
#===
getwd() ## Şimdiki working directory.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının uzantısını buluyor.
setwd(paste0(main.path, "/")) ## Yeni working directory bu kaynak dosyasının lokasyonunda belirleniyor.

#================================= Genel Bilgi =================================
# Bu bolumde WorldBank'ten elde ettigimiz 5 farkli degiskene "NY.GDP.MKTP.CD" (GDP - GSYH Simdiki US Fiyatlariyla), "NE.GDI.TOTL.ZS" (Capital - Sermaye Stogu % GSYH olarak), "DT.DOD.DECT.CD" (Ext.Debt - Dis Borc Stogu Simdiki US Fiyatlariyla), "NE.EXP.GNFS.ZS" (Export - Ihracat % GSYH olarak) ve son olarak "SP.POP.TOTL" (Population - Toplam Nufus GSYH ve Dis Borc Stogu'ndaki nufusun etkisini yok etmek icin) ait 1970-2018 arasindaki yillik nominal verilerini kullanacagiz. Temel amacimiz bu verileri kullanarak VAR modeli uygulamak olacak. Asagida bu bolumde yapilacak analizler sirasiyla verilmistir.
    ## 1. Datayi R'a yukleyip temizleme
    ## 2. Datayi transforme etme
    ## 3. Ozet data istatistiklerini bulma
    ## 4. Datayi zaman serine cevirme
    ## 5. VAR Modeli tahmini
    ## 6. Diagnostic testleri
    ## 7. Impulse Response Functions (Etki-Tepki Fonksiyonlari)
    ## 8. FEVD: Forecast Error Variance Decomposition (Tahmin Hatasi Varyans Ayristirma)

#========================= Datayi Yukleme ve Temizleme =========================
# Data dosyasinin adi.
file.name <- "worldbank2.xlsx"

# Ham datanin yuklenmesi.
data <- read_excel(path = file.name, sheet = 1, range = cell_limits(c(1, 1), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yukledigimiz datayi read_excel fonksiyonu tibble formatinda kaydediyor.
data <- as.data.frame(data, stringsAsFactors = TRUE) ## Datayi data.frame formatina ceviriyoruz.

# Ham datanin yapisi.
str(data)

# Ham data dosyasinin RData formatinda disa aktarilmasi.
RData.Name <- "workbank2"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Data manipulasyonlari.
test <- data[, -c(1, 2)] ## Ilk iki surundaki degiskenler hep ayni oldugu icin siliniyor. Yeni data, test olarak adlandiriliyor.
test <- test[complete.cases(test[, c(1, 2)]), ] ## 1. ve 2. sutunlarda NA degerine sahip satirlarin hepsi siliniyor.
ref <- test[, c(1, 2)] ## 1. ve 2. sutun ayri bir data olarak daha sonra kullanilmak uzere ref adi ile saklaniyor.
test <- test[, -1] ## 1. sutun cok uzun ve gereksiz oldugu icin siliniyor.
test <- t(test) ## Transpoz aliniyor. Bu noktadan sonra artik datamiz dataframe degil, bu nedenle daha sonra yeniden dataframe'e cevrilmesi lazim.

test <- cbind(rownames(test), test) ## Transpoz alindiktan sonra yillar satir isimleri olarak kaldigi icin, satir isimleri ve test datasi sutun olarak birlestiriliyor.
colnames(test) <- test[1, ] ## Ilk satirdaki degisken isimleri sutun isimlerine ataniyor.
test <- test[-1, ] ## Ilk satir gereksiz oldugu icin siliniyor.
rownames(test) <- 1:nrow(test) ## Satir isimleri yeniden duzenleniyor.

test[, 1] <- gsub(" \\[YR.*$", "", test[, 1]) ## Yillari belirtirken ilk sutundaki "[YRYIL]" degerleri regular expression kullanilarak siliniyor. Regular expression hakkinda daha fazla bilgi icin bknz: https://regexr.com/
test[, 1] <- gsub("X", "", test[, 1]) ## Yillari belirten ilk sutundaki "X" degerleri regular expression kullanilarak siliniyor. Regular expression hakkinda daha fazla bilgi icin bknz: https://regexr.com/
test[test == ".."] <- NA ## ".." olarak verilen tum verileri NA'ya ceviriyoruz.
colnames(test)[1] <- "Year"
test <- as.data.frame(test, stringsAsFactors = FALSE) ## Dataframe'e donusum yapiliyor.

# Degiskenlerin sinifinin cevrilmesi.
str(test) ## Cevrilmeden onceki datanin yapisi.
test <- data.frame(sapply(test, function(numeric) as.numeric(numeric)), stringsAsFactors = FALSE) ## Degiskenlerin hepsi toplu olarak numeric sinifa cevriliyor ve cikan sonuc yeniden data.frame olarak kaydediliyor.

# Islenmis datayi data olarak kaydediyoruz.
data <- test

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "workbank2__Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#================= Regresyon Analizi icin Datanin Hazirlanmasi =================
# Daha sonraki adimlarda regresyon analizi yapabilmek icin temizlenmis data icinden bazi degiskenleri secelim.
## Secilen degiskenler: "NY.GDP.MKTP.CD" (GDP - GSYH Simdiki US Fiyatlariyla), "NE.GDI.TOTL.ZS" (Capital - Sermaye Stogu % GSYH olarak), "DT.DOD.DECT.CD" (Ext.Debt - Dis Borc Stogu Simdiki US Fiyatlariyla), "NE.EXP.GNFS.ZS" (Export - Ihracat % GSYH olarak) ve son olarak "SP.POP.TOTL" (Population - Toplam Nufus GSYH ve Dis Borc Stogu'ndaki nufusun etkisini yok etmek icin)
## Not: GDP ve Ext.Debt simdiki US fiyatlariyla oldugu icin bu degiskenlerin icindeki fiyat etkisi cikarilmali yani fiyat endeksini kullanarak nominal formda olan bu degiskenler reel forma donusturulmeli. Fakat datanin icinde GSYH Deflatoru ya da TUFE olmadigindan bu kisim bilerek atlanmistir.
series <- c("Year", "NY.GDP.MKTP.CD", "NE.GDI.TOTL.ZS", "DT.DOD.DECT.CD", "NE.EXP.GNFS.ZS", "SP.POP.TOTL") ## Secilen degiskenlerin kodlari vektor halinde yazildi.
series.names <- c("Year", "GDP", "Per.Capital", "Ext.Debt", "Per.Export", "Population") ## Secilen degiskenlerin degistirilmis isimleri vektor halinde yazildi.
temp <- test[, series] ## test datasinin secilen degiskenlere gore alt kumesi alindi ve yeni dataya temp adi verildi.
colnames(temp) <- series.names ## temp datasinin sutun isimleri degistirildi.
temp <- temp[complete.cases(temp), ] ## temp datasindaki tum NA iceren satirlar silindi.

temp$GDP.PC <- temp$GDP / temp$Population ## Kisi basi GDP.
temp$Ext.Debt.PC <- temp$Ext.Debt / temp$Population ## Kisi basi Ext.Debt.

temp$Ln.GDP.PC <- log(temp$GDP.PC) ## Model icinde yorumlamak daha kolay olsun diye GDP.PC degiskeninin dogal logaritmasi aliniyor.
temp$Ln.Ext.Debt.PC <- log(temp$Ext.Debt.PC) ## Model icinde yorumlamak daha kolay olsun diye Ext.Debt.PC degiskeninin dogal logaritmasi aliniyor.

# Islenmis datayi data olarak kaydediyoruz.
data <- temp

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "workbank2__Processed__Trans"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#===================== Coklu Dogrusal Regresyon ile Tahmin =====================
# Coklu dogrusal regresyonla model tahmini.
## Zaman serisi datasinin kesinlikle SEKK yontemi kullanarak coklu regresyonla tahmin edilmemesi gerekir. Bunun nedeni zaman serisi datasinda siklikla gorulen otokorolasyondur. Zaman serisi datasinda siklikla pozitif otokorelasyon gorulur ve bunun sonucunda parametre tahmincilerinin varyansi duser, t degerleri buyur (yani H0 hipotezi yanlis olarak red edilir ve parametre tahmincileri gercekte istatistiki olarak anlamli olmamalarina ragmen anlamli sonucuna varilir), modelin geneli icin yapilan F testinde F degeri buyur (yani H0 hipotezi yanlis olarak red edilir ve model genel olarak anlamli olmamasina ragmen anlamli sonucuna varilir) ve R2 degeri buyur. Kisacasi cikan sonuclara guvenilmemesi gerekir.
## Fakat simdilik sadece gosterim olmasi acisindan bir bu yontemi kullanacagiz.
model <- lm(data = data, formula = Ln.GDP.PC ~ Per.Capital + Per.Export + Ln.Ext.Debt.PC, singular.ok = FALSE)
summary(model)

#================= VAR (Vector Autoregressive Model) ile Tahmin ================
# VAR (Vector Autoregressive Model) ile model tahmin oncesi bazi duzenlemeler.
data <- data[, c("Year", "Ln.GDP.PC", "Per.Capital", "Per.Export", "Ln.Ext.Debt.PC")] ## Oncelikle sadece VAR'da kullanacagimiz degiskenler icin datanin alt kumesini alalim.
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
var.est ## Model.
summary(var.est) ## Modelin ozeti.

# Model Denge Kontrollu
## roots(model) fonsksiyonu katsayi matriksine ait eigenvalueleri verir.
## Eger eigenvaluler birim kok icinde olursa sistem dinamik olarak dengededir.
if (sum(!(roots(var.est) < 1)) == 0) { ## Stability check
    stability.note <- "Tüm kökler birim çemberin içinde ve sistem dinamik olarak dengede."
}
if (sum(!(roots(var.est) < 1)) != 0) { ## Stability check
    stability.note <- "Bazı kökler birim çemberin dışında ve sistem dinamik olarak dengede değil."
}
message(stability.note)

## Modelin dengede olup olmadigini kontrol etmek icin ayrica stability() fonksiyonu kullanilabilir.
var.est.stabil <- stability(var.est, type = "Rec-CUSUM") ## Diger opsiyonlar: "OLS-CUSUM", "Rec-MOSUM", "OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM" and "fluctuation".
par(mar = c(1, 1, 1, 1))
plot(var.est.stabil)

# Diagnostic Test (Teshis Testleri)
## Gecikme uzunlugu secimi
## 1. Otokorelasyon testlerinde ve degisen varyans testlerinde Tsay, R.S. (2005) {Analysis of Financial Time Series. Vol. 543. John Wiley &; Sons. page 33)} tarafindan onerilen gecikme uzunlugu = lag ≈ ln(length(residuals)) kullanildi.
## 2. Bazi simlasyonlar sonucunda kullanilmasi gereken gecikme uzunlugu 10 olmali diyor: http://robjhyndman.com/hyndsight/ljung-box-test/.

## Otokorelasyon testi (Ljung-Box Portmanteau testi)
## 1. Buyuk orneklem icin "PT.asymptotic" ya da "BG" kullanin, kucuk orneklem icin ise "PT.adjusted" ya da "ES" kullanin.
## 2. Bos hipotez otokorelasyon olmadigini soyler.
## 3. Asagidaki testler model geneli icin yapilmistir. Tekil denklemler icin degil.
lag.serial <- ceiling(log(nrow(residuals(var.est))))
serial.test(var.est, lags.pt = lag.serial, lags.bg = lag.serial, type = "PT.adjusted") ## Otokorelasyon var.
lag.serial <- 10
serial.test(var.est, lags.pt = lag.serial, lags.bg = lag.serial, type = "PT.adjusted") ## Otokorelasyon yok.

## Degisen Varyans testi (ARCH-LM testi)
## 1. Bos hipotez degisen varyans olmadigini, yani sabit varyans oldugunu soyler.
## 2. Asagidaki testler model geneli icin yapilmistir. Tekil denklemler icin degil.
lag.arch <- ceiling(log(nrow(residuals(var.est))))
arch.test(var.est, lags.multi = lag.arch, lags.single = lag.arch, multivariate.only = TRUE) ## Degisen varyans yok.

## Normallik testi (Jarque-Bera testi)
## 1. Bos hipotez hata terimlerinin normal dagildiigni soyler.
## 2. Asagidaki testler model geneli icin yapilmistir. Tekil denklemler icin degil.
normality.test(var.est, multivariate.only = TRUE) ## Normallik yok.

# Impulse Response Functions (Etki Tepki Fonksiyonlari)
irf.ahead <- 10 ## Kac donem ilerisi gosterilsin?
irf.runs <- 100 ## Bootstrap'de kac kere calistirilsin.
irf.ci <- 0.95 ## Bootstrap hata bandi icin guven araligi.
irf.obj <- vars::irf(var.est, impulse = NULL, response = NULL, n.ahead = irf.ahead, ortho = TRUE, boot = TRUE, ci = irf.ci, runs = irf.runs, seed = 1234)
plot(irf.obj)

# FEVD: Forecast Error Variance Decomposition (Tahmin Hatasi Varyans Ayristirma)
## FEVD, her bir değişkenin modeldeki diğer değişkenlere katkıda bulunduğu bilgi miktarını gösterir. Her bir değişkenin tahmin hatası varyansının ne kadarının diğer değişkenlere exojen şoklarla açıklanabileceğini belirler.
fevd.ahead <- 10 ## Kac donem ilerisi gosterilsin?
fevd.obj <- vars::fevd(var.est, n.ahead = fevd.ahead)
plot(fevd.obj)

#==================================== SON ======================================
