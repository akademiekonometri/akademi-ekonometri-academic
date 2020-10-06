#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#============================== Bizi Takip Edin ================================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#============== R'da CSV Uzantılı Data Yüklemek ve Temizlemek ==================
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
Load.Install(Package.Names = c("XLConnect"))
#==========
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
#==========

#=================== Datayi Yukleme ve Temizleme icin R Kodu ===================
# Data dosyasinin adi.
file.path <- "CPI.csv"

# Ham data dosyasinin yuklenmesi.
data <- read.csv(file.path, header = TRUE, sep = ",", dec = ".", colClasses = "character", comment.char = "", na.string = "")

# Ham data dosyasinin RData formatinda disa aktarilmasi.
save(list = "data", file = "CPI.RData") ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Ham datanin yapisi.
str(data)

# Datanin temizlenmesi.
test <- data[, -c(1, 5)] ## 1. ve 5. sutun siliniyor ve yeni test datasi olusturuluyor.
colnames(test) <- c("Yil", "Ay", "TUFE") ## Sutun isimleri degistiriliyor.
test$Ay <- gsub("(M0)|(M)", "", test$Ay) ## "Ay" sutununda M0 ve M gorunen yerlere "" yaziliyor.
test <- test[grep("(13)|(S01)|(S02)|(S03)", test$Ay, invert = TRUE), ] ## "Ay sutununda 13, S01, S02 ve S03 degerleri varsa bu degerlerin oldugu satirlar siliniyor.
unique(test$Ay) ## "Ay" sutunundaki tekil degerler kontrol ediliyor.

# Degiskenlerin sinifinin cevrilmesi.
str(test) ## Cevrilmeden onceki datanin yapisi.
test$Yil <- as.numeric(test$Yil) ## Numeric sinif.
test$Ay <- as.numeric(test$Ay) ## Numeric sinif.
test$TUFE <- as.numeric(test$TUFE) ## Numeric sinif.

# Son halini almis datanin yapisinin incelenmesi.
str(test)

#==================================== SON ======================================
