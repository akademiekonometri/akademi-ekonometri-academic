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

#============================= Data Transformasyonu ============================
# Notlar:
#
## Bu yazıda kullandığımız datayı (eger varsa) web sitemizdeki ilgili bölümde bulabilirsiniz.
## Aşağıdaki R kodu, öncelikle gerekli R paketlerini yüklüyor ve daha sonra working directory'yi bilgisayarınızda bu kaynak dosyasının bulunduğu lokasyona göre değiştiriyor. Son olarak ise ilgili R kodunu çalıştırıyor.

#=============================== Gerekli Paketler ==============================
# Tek bir adımda gerekli paketlerin yüklenmesi ve kurulması.
#===
# Devtools ve okara paketlerinin yüklenmesi.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(install.packages("devtools")))}
suppressWarnings(suppressMessages(library("devtools"))) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
if("okara" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara", force = FALSE)))}
suppressWarnings(suppressMessages(library("okara"))) ## okara paketi.
#===
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
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
# Daha once Amerikada enflasyon ve istihdam ile ilgili verileri yayinlayan kurum olan Bureau of Labor Statistics (BLS)'den direkt olarak 2 farkli veri seti (CPI ve PPI) indirip bu veri setinden 3 adet indeksi kullanip (CUSR0000SA0, CUUR0000SA0, WPU00000000) datayi temizlemistik. Son olarak ise temizlenmis datayi .RData dosyasi olarak CPI_PPI_Processed.RData adi altinda kaydetmistik.
    ## Bu datayi indirmek ve temizlemek icin gerekli bilgilere onceki bolumlerden ulasabilirsiniz.
    ## Bu bolumde ise temizlenmis CPI_PPI_Processed.RData dosyasini direkt olarak R'in icine cekecegiz ve bazi transformasyonlar yapacagiz.
    ## Temel amacimiz CPI ve PPI indekslerini kullanarak enflasyonu hesaplamak olacak.

#=============================== Datayi Yuklemek ===============================
# Daha once temizlenmis CPI_PPI_Processed.RData datasini yukleme.
load(file = "CPI_PPI__Processed.RData", verbose = FALSE)
data <- CPI_PPI__Processed ## R icine aktarilan bu temizlenmis datanin ismi CPI_PPI_Processed oldugundan, data ile degistiriliyor.

# Daha once Temizlenmis datanin yapisi.
str(data)

# Date adli yeni bir degisken olusturuyoruz. Bu zaman serilerini grafige dokerken gunu belirten bu degisken isimize yarayacak.
data$Date <- as.Date(paste(data$Year, data$Month, "1", sep = "-"))
data <- data[, c("Date", "Year", "Month", "CPI.S", "CPI.US", "PPI.US")]

#==================== Temizlenmis Data icin Transformasyon =====================
# Degiskenlerdeki buyume oranin yuzdesel olarak hesaplanmasi. Yani enflasyonun hesaplanmasi.
## Not: hesaplama yapilirken elde edilen yuzdesel buyume degerleri 3. ondalik basamaga yuvarlanmistir.
temp <- data ## Asagidaki dongunun islemesi icin datayi farkli bir isimle kaydedip onunla islem yapiyoruz.
for (i in 4:ncol(temp)) {
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
RData.Name <- "CPI_PPI__Processed__Trans"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================================== SON ======================================
