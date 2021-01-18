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

#============================= Data Transformasyonu ============================
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
## "Seasonal.Adjust" fonksiyonu ici n"seasonal_adjust.R" dosyasina bakabilirsiniz.
source(paste0(main.path, "/", functions.folder.name, "/", "seasonal_adjust.R")) ## Degiskenler icindeki mevsimsel etkinin "X-13ARIMA-SEATS" algoritmasi ile cikartilmsi yontemi. Not: Sadece aylik ve ceyreklik data varsa kullanin. Yillik verilerde kullanilmaz.

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
Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
Load.Install(c("seasonal")) ## Sadece seasonal adjustment (mevsimsel duzeltme) yapacaksiniz kullanin.
options(java.parameters = "-Xmx8000m") ## Bazen excel datalarini yuklerken memory sorunu ciktigi icin gerekli bir kod.
#==========
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#==========

#================================= Genel Bilgi =================================
# Daha once Amerikada enflasyon ve istihdam ile ilgili verileri yayinlayan kurum olan Bureau of Labor Statistics (BLS)'den direkt olarak 2 farkli veri seti (CPI ve PPI) indirip bu veri setinden 3 adet indeksi kullanip (CUSR0000SA0, CUUR0000SA0, WPU00000000) datayi temizlemistik. Son olarak ise temizlenmis datayi .RData dosyasi olarak CPI_PPI_Processed.RData adi altinda kaydetmistik.
    ## Bu datayi indirmek ve temizlemek icin gerekli bilgilere onceki bolumlerden ulasabilirsiniz.
    ## Bu bolumde ise temizlenmis CPI_PPI_Processed.RData dosyasini direkt olarak R'in icine cekecegiz ve istatistiki olarak gerekli oldugunu bulmamiz halinde bazi degiskenleri mevsimsellik etkisinden arindiracagiz.
    ## Temel amacimiz mevsimsellik etkisinden arindirilmamis olan CPI indeksi CUUR0000SA0'yi mevsimsellikten arindirip buldugumuz degerlerin internetten indirdigimiz ve mevsimsellikten arindirilmis olan CPI indeksi CUSR0000SA0'nin degerlerine yakinsayip yakinsamadigini gormeye calisacagiz.

#=============================== Datayi Yuklemek ===============================
# Daha once temizlenmis CPI_PPI_Processed.RData datasini yukleme.
load(file = "CPI_PPI_Processed.RData", verbose = FALSE)
data <- CPI_PPI_Processed ## R icine aktarilan bu temizlenmis datanin ismi CPI_PPI_Processed oldugundan, data ile degistiriliyor.

# Daha once Temizlenmis datanin yapisi.
str(data)

# Datanin icinden "PPI.US" degiskenini cikartiyoruz cunku biz sadece CPI ile ilgileniyoruz.
data <- data[, c("Year", "Month", "CPI.S", "CPI.US")]

# Seasonal adjustment yaparken malesef gozlem sayisi cok oldugu icin "X-13ARIMA-SEATS" algoritmasi hata veriyor. Bu nedenle datanin 2000-01 ayindan baslayan bir alt kumesi aliniyor.
data <- data[data$Year >= 2000, ]
rownames(data) <- 1:nrow(data)

# Datanin yapisi.
str(data)

#========================= Mevsimsellikten Arindirmak ==========================
# Varolan datayi kaybetmemek ve yanlislikla degistirmemek icin mevsimsellikten arindirirken temp adli datanin kopyasini kullaniyoruz.
temp <- data

# Datanin zaman serisi objesine cevirilmesi.
temp.ts <- ts(temp[, 3:ncol(temp)], frequency = 12, start = c(as.numeric(temp$Year[1]), as.numeric(temp$Month[1]))) ## Data objesi frekansi 12 olan zaman serisi objesine cevriliyor. Baslangic olarak ilk yilin ilk ayi aliniyor.

# Mevsimsellikten arindirmadan sadece %5 anlamlilik duzeyinde hangi degiskenlerde mevsimsellik etkisi olduguna bakiyoruz.
Seasonal.Adjust(temp.ts, Significance.Level = 5, Adjust = FALSE)
season.pvalues ## Her bir degisken icin mevsimsellikten arindirilmadan once ve sonraki mevsimsellik etkisini arastiran hipotezin p-degeri.
season.results ## Her bir degisken icin mevsimsellikten arindirilmadan once ve sonraki mevsimsellik etkisini arastiran hipotezin sonucunda varilan karar. NS: not seasonal (mevsimsellik yok), S: seasonal (mevsimsellik var)
## Not: CPI.S'de mevsimsellik etkisi bulmadik cunku zaten bu Bureau of Labor Statistics (BLS) tarafindan mevsimsellikten arindirilmis olarak verilen bir seriydi. Fakat CPI.US'de mevsimsellik etkisi bulduk cunku bu Bureau of Labor Statistics (BLS) tarafindan mevsimsellik etkisinden arindirilmamis olarak verilmisti.

# %5 anlamlilik duzeyinde degiskenlerde istatistiki olarak bulunan mevsimsellik etkisini ortadan kaldiriyoru. Mevsimsellik etkisi ortadan kaldirilmis haldeki degiskenleri iceren yeni data "ts.seasonal.adj" adli data ile fonksiyonun sonucu olarak veriliyor.
Seasonal.Adjust(temp.ts, Significance.Level = 5, Adjust = TRUE)
ts.seasonal.adj ## Mevsimsellik etkisinden arindirilmis data. Not: Sadece CPI.US mevsimsellikten arindirildi.
temp.ts - ts.seasonal.adj ## Mevsimsellikten arindirilmis ve arindirilmamis data arasindaki fark. CPI.US mevsimsellikten dolayi olusan farki gosterdi. CPI.S'de fark yok.

# Simdi mevsimlerin etkisini daha yakindan inceleyelim.
temp.ts[, 2] - ts.seasonal.adj[, 2] ## Farki alinmis seride ikinci degisken bize tum mevsimlerin etkisini ayri ayri veriyor.

# Simdi bizim hesapladigimiz mevsimsellikten arindirilmis CPI.US ile Bureau of Labor Statistics (BLS) tarafindan hazirlanan mevsimsellikten arindirilmis CPI.S arasindaki farka bakalim.
temp.ts[, 1] - ts.seasonal.adj[, 2] ## Tum yil ve aylarda belli bir fark oldugu icin BLS'nin mevsimsellikten arindirmak icin kullandigi yontem bizim kullandigimiz yonteme gore biraz farkli diyebiliriz.

#==================================== SON ======================================
