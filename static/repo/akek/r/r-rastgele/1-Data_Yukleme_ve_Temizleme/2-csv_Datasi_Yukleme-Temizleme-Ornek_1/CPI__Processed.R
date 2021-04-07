#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#============================== Bizi Takip Edin ================================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#============================== Bizi Takip Edin ================================

#=========== CSV Uzantılı Data Yüklemek ve Temizlemek - Ornek 1 ================
# Notlar:
#
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

#========================= Datayi Yukleme ve Temizleme =========================
# Data dosyasinin adi.
file.path <- "CPI.csv"

# Ham data dosyasinin yuklenmesi.
data <- read.csv(file = file.path, header = TRUE, sep = ",", dec = ".", colClasses = "character", comment.char = "", na.string = "")

# Ham datanin yapisi.
str(data)

# Ham data dosyasinin RData formatinda disa aktarilmasi.
RData.Name <- "CPI"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Datanin temizlenmesi.
test <- data[, -c(1, 5)] ## 1. ve 5. sutun siliniyor ve yeni test datasi olusturuluyor.
colnames(test) <- c("Year", "Month", "CPI") ## Sutun isimleri degistiriliyor.
test$Month <- gsub("(M0)|(M)", "", test$Month) ## "Month" sutununda M0 ve M gorunen yerlere "" yaziliyor.
test <- test[grep("(13)|(S01)|(S02)|(S03)", test$Month, invert = TRUE), ] ## "Month sutununda 13, S01, S02 ve S03 degerleri varsa bu degerlerin oldugu satirlar siliniyor.
unique(test$Month) ## "Month" sutunundaki tekil degerler kontrol ediliyor.

# Degiskenlerin sinifinin cevrilmesi.
str(test) ## Cevrilmeden onceki datanin yapisi.
test$Year <- as.numeric(test$Year) ## Numeric sinif.
test$Month <- as.numeric(test$Month) ## Numeric sinif.
test$CPI <- as.numeric(test$CPI) ## Numeric sinif.

# Islenmis datayi data olarak kaydediyoruz.
data <- test

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "CPI__Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================================== SON ======================================
