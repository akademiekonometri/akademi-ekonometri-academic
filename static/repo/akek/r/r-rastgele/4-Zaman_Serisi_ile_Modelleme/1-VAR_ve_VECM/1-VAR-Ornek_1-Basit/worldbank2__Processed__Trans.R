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

#============================ VAR - Ornek 1 (Basit) ============================
#==== R'da Excel Uzantılı Data Yüklemek, Temizlemek, CDR ve VAR ile Tahmin =====
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
Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "vars", "pastecs"))
#==========
## Load.Install(Package.Names = "readxl")
## Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#==========

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

temp$GDP.pC <- temp$GDP / temp$Population ## Kisi basi GDP.
temp$Ext.Debt.pC <- temp$Ext.Debt / temp$Population ## Kisi basi Ext.Debt.

temp$Ln.GDP.pC <- log(temp$GDP.pC) ## Model icinde yorumlamak daha kolay olsun diye GDP.pC degiskeninin dogal logaritmasi aliniyor.
temp$Ln.Ext.Debt.pC <- log(temp$Ext.Debt.pC) ## Model icinde yorumlamak daha kolay olsun diye Ext.Debt.pC degiskeninin dogal logaritmasi aliniyor.

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
