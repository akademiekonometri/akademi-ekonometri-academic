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

#=========== R'da Dataframe Objesini Zaman Serisi Objesine Cevirmek ============
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
Load.Install(Package.Names = c("XLConnect", "vars"))
#==========
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
#==========

#=================== Datayi Yukleme ve Temizleme icin R Kodu ===================
# Data dosyasinin adi.
dosya.uzantisi <- "worldbank2.xlsx"

# Ham datanin yuklenmesi: Yontem 1.
workbook <- loadWorkbook(filename = dosya.uzantisi, create = FALSE) ## Once excel workbook yukleniyor.
data <- readWorksheet(object = workbook, sheet = 1, startRow = 1, startCol = 1, header = TRUE, colTypes = "character") ## Sonra workbook icindeki belirli bir worksheet yukleniyor.

# Ham datanin yuklenmesi: Yontem 2.
data <- readWorksheetFromFile(file = dosya.uzantisi, sheet = 1, startRow = 1, startCol = 1, header = TRUE, colTypes = "character") ## Direkt olarak excel workbook icindeki belirli bir worksheet yukleniyor.

# Ham data dosyasinin RData formatinda disa aktarilmasi.
save(list = "data", file = "workbank2.RData") ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Ham datanin yapisi.
str(data)

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

test[, 1] <- gsub("\\.\\..*$", "", test[, 1]) ## Yillari belirtirken ilk sutundaki "..YRYIL." degerleri regular expression kullanilarak siliniyor. Regular expression hakkinda daha fazla bilgi icin bknz: https://regexr.com/
test[, 1] <- gsub("X", "", test[, 1]) ## Yillari belirten ilk sutundaki "X" degerleri regular expression kullanilarak siliniyor. Regular expression hakkinda daha fazla bilgi icin bknz: https://regexr.com/
test[test == ".."] <- NA ## ".." olarak verilen tum verileri NA'ya ceviriyoruz.
colnames(test)[1] <- "Yil"
test <- as.data.frame(test, stringsAsFactors = FALSE) ## Dataframe'e donusum yapiliyor.

# Degiskenlerin sinifinin cevrilmesi.
str(test) ## Cevrilmeden onceki datanin yapisi.
test <- data.frame(sapply(test, function(numeric) as.numeric(numeric)), stringsAsFactors = FALSE) ## Degiskenlerin hepsi toplu olarak numeric sinifa cevriliyor ve cikan sonuc yeniden data.frame olarak kaydediliyor.

# Son halini almis datanin yapisinin incelenmesi.
str(test)

#================= Regresyon Analizi icin Datanin Hazirlanmasi =================
# Daha sonraki adimlarda regresyon analizi yapabilmek icin temizlenmis data icinden bazi degiskenleri secelim.
## Secilen degiskenler: "NY.GDP.MKTP.CD" (GSYH - Simdiki US Fiyatlariyla), "NE.GDI.TOTL.ZS" (Sermaye Stogu - % GSYH olarak), "DT.DOD.DECT.CD" (Dis Borc Stogu - Simdiki US Fiyatlariyla), "NE.EXP.GNFS.ZS" (Ihracat - % GSYH olarak) ve son olarak "SP.POP.TOTL" (Toplam Nufus - GSYH ve Dis Borc Stogu'ndaki nufusun etkisini yok etmek icin)
## Not: GSYH ve Dis Borc Stogu simdiki US fiyatlariyla oldugu icin bu degiskenlerin icindeki fiyat etkisi cikarilmali yani fiyat endeksini kullanarak nominal formda olan bu degiskenler reel forma donusturulmeli. Fakat datanin icinde GSYH Deflatoru ya da TUFE olmadigindan bu kisim bilerek atlanmistir.
secilen.degiskenler <- c("Yil", "NY.GDP.MKTP.CD", "NE.GDI.TOTL.ZS", "DT.DOD.DECT.CD", "NE.EXP.GNFS.ZS", "SP.POP.TOTL") ## Secilen degiskenlerin kodlari vektor halinde yazildi.
degisken.isimleri <- c("Yil", "GSYH", "Sermaye", "Dis.Borc", "Ihracat", "Nufus") ## Secilen degiskenlerin degistirilmis isimleri vektor halinde yazildi.
temp <- test[, secilen.degiskenler] ## test datasinin secilen degiskenlere gore alt kumesi alindi ve yeni dataya temp adi verildi.
colnames(temp) <- degisken.isimleri ## temp datasinin sutun isimleri degistirildi.
temp <- temp[complete.cases(temp), ] ## temp datasindaki tum NA iceren satirlar silindi.

temp$GSYH.KB <- temp$GSYH / temp$Nufus ## Kisi basi GSYH.
temp$Dis.Borc.KB <- temp$Dis.Borc / temp$Nufus ## Kisi basi Dis.Borc.

temp$Ln.GSYH.KB <- log(temp$GSYH.KB) ## Model icinde yorumlamak daha kolay olsun diye GYSH.KB degiskeninin dogal logaritmasi aliniyor.
temp$Ln.Dis.Borc.KB <- log(temp$Dis.Borc.KB) ## Model icinde yorumlamak daha kolay olsun diye GYSH.KB degiskeninin dogal logaritmasi aliniyor.

#===================== Coklu Dogrusal Regresyon ile Tahmin =====================
# Coklu dogrusal regresyonla model tahmini.
## Zaman serisi datasinin kesinlikle SEKK yontemi kullanarak coklu regresyonla tahmin edilmemesi gerekir. Bunun nedeni zaman serisi datasinda siklikla gorulen otokorolasyondur. Zaman serisi datasinda siklikla pozitif otokorelasyon gorulur ve bunun sonucunda parametre tahmincilerinin varyansi duser, t degerleri buyur (yani H0 hipotezi yanlis olarak red edilir ve parametre tahmincileri gercekte istatistiki olarak anlamli olmamalarina ragmen anlamli sonucuna varilir), modelin geneli icin yapilan F testinde F degeri buyur (yani H0 hipotezi yanlis olarak red edilir ve model genel olarak anlamli olmamasina ragmen anlamli sonucuna varilir) ve R2 degeri buyur. Kisacasi cikan sonuclara guvenilmemesi gerekir.
## Fakat simdilik sadece gosterim olmasi acisindan bir bu yontemi kullanacagiz.
model <- lm(data = temp, formula = Ln.GSYH.KB ~ Sermaye + Ihracat + Ln.Dis.Borc.KB, singular.ok = FALSE)
summary(model)

#================= VAR (Vector Autoregressive Model) ile Tahmin ================
# VAR (Vector Autoregressive Model) ile model tahmin oncesi bazi duzenlemeler.
temp <- temp[, c("Yil", "Ln.GSYH.KB", "Sermaye", "Ihracat", "Ln.Dis.Borc.KB")] ## Oncelikle sadece VAR'da kullanacagimiz degiskenler icin datanin alt kumesini alalim.
temp.ts <- ts(temp[, grep("(Yil)", colnames(temp), invert = TRUE)], frequency = 1, start = temp$Yil[1]) ## Alt kumesi alinmis datayi, "Yil" degiskenini datanin icinden atip, frekansini ve baslangic zamanini secerek datayi artik zaman serisi datasina cevirelim. Son olarak zaman serisine cevrilmis datayi temp.ts olarak kaydedelim.
str(temp.ts) ## temp.ts'nin yapisi.
frequency(temp.ts) ## temp.ts'nin frekansi.
class(temp.ts) ## temp.ts'nin sinifi.
temp.ts ## temp.ts'nin gorunumu.

# VAR (Vector Autoregressive Model) ile model icin gecikme uzunlugu ve deterministic degisken secimi.
lag <- 1 ## Gecikme uzunlugu.
deterministik.terim <- "const" ## Deterministik terim.

# VAR (Vector Autoregressive Model) ile model tahmin.
var.est <- vars::VAR(temp.ts, p = lag, type = deterministic.selected, season = NULL, exogen = NULL) ## VAR tahmin edildi.
summary(var.est) ## Modelin ozeti.

#==================================== SON ======================================
