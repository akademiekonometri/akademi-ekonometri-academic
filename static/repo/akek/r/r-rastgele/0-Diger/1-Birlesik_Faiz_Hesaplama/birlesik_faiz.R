#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#============================== Bizi Takip Edin ================================
# Web Sitemiz: https://akademiekonometri.netlify.app/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#============================== Bizi Takip Edin ================================

#========================= R'da Birlesik Faiz Hesaplama ========================
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
Load.Install(c("rstudioapi", "XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#===
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#===

#======================== Working Directory'yi Belirlemek ======================
# Working directory'nin bu kaynak dosyasının olduğu lokasyonda belirlenmesi.
#===
getwd() ## Şimdiki working directory.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının uzantısını buluyor.
setwd(paste0(main.path, "/")) ## Yeni working directory bu kaynak dosyasının lokasyonunda belirleniyor.

#============================= Birlesik Faiz Hesabi ============================
# Notlar:
#
## Asagidaki faiz hesabi odemenin tum donemin sonunda oldugu birlesik faiz hesabidir. Eger geri odeme aylik olarak yapiliyorsa baska bir yontem/formul kullanilmalidir.
## Asagidaki hesaplamada zaman periodu yil olarak alinmistir. Bu nedenle, faiz oraninin da yillik olarak yazilmasi gerekir. Bu sekildeki bir kullanim zaman periodunun sikligi degistiginde hesaplamayi kolaylastirir.

# Daha fazla bilgi icin asagidaki linkleri inceleyebilirsiniz.
## 1: https://www.calculatorsoup.com/calculators/financial/compound-interest-calculator.php
## 2: http://www.moneychimp.com/articles/finworks/fmfutval.htm
## 3: http://www.moneychimp.com/features/rule72.htm

# Belirli bir period icin birlesik faiz formulu: A = P * (1 + r/n)^(n * t)
# Surekli (anlik) birlesik faiz formulu: A = P * e^(r * t)
## A = Toplam geri odeme miktari (ana para + faiz miktari)
## P = Ana para
## I = Faiz miktari
## r = Yillik faiz orani (ondalik olarak)
## n = Birim zamandaki toplam period / yil icinde kac kere birlesik faiz hesabi yapilacagi
## t = Zaman (yil olarak yazilmali)

#===
# Toplam faiz hesaplamasi icin on bilgiler.
P <- 10000 ## Ana para.
r <- 0.0079 * 12 ## Yillik faiz orani (ondalik olarak).
n <- 12 ## Birim zamandaki toplam period.
t <- 5 ## Zaman.

# Oncelikle yukarida verilen bilgilerle belirli bir period icin toplam faiz miktarini hesaplamaya calisalim.
A <- P * (1 + r/n)^(n * t) ## Toplam geri odeme miktari.
A ## Toplam geri odeme miktari.
I <- A - P ## Faiz miktari.
I ## Faiz miktari.

# Yukarida verilen bilgilerle simdi surekli birlesik faizi kullanarak toplam faiz miktarini hesaplamaya calisalim.
A <- P * exp(1)^(r * t) ## Toplam geri odeme miktari.
A ## Toplam geri odeme miktari.
I <- A - P ## Faiz miktari.
I ## Faiz miktari.
#===

#===
# Zaman hesaplamasi icin on bilgiler.
A <- 16034.18 ## Toplam geri odenmesi gereken miktar.
P <- 10000 ## Ana para
r <- 0.0079 * 12 ## Yillik faiz orani (ondalik olarak.
n <- 12 ## Birim zamandaki toplam period.

# Yukaridaki bilgileri kullanarak bu miktarin kac yilda odenmesi gerektigini hesaplayalim.
## Formul: A = P * (1 + r/n)^(n * t)
## t: ln(A/P) / (n * (ln(1 + r/n)))
t <- log(A/P) / (n * (log(1 + r/n))) ## Zaman.
t ## Zaman.
#===

#===
# Faiz oraninin hesaplanmasi icin on bilgiler.
A <- 16034.18 ## Toplam geri odenmesi gereken miktar.
P <- 10000 ## Ana para
n <- 12 ## Birim zamandaki toplam period.
t <- 5

# Yukaridaki bilgileri kullanarak bu miktarin hangi faiz orani ile odenmesi gerektigini hesaplayalim.
## Hatirlatma: A = P * (1 + r/n)^(n * t)
## r: n * ((A/P)^(1/(n * t)) - 1)
r <- n * ((A/P)^(1/(n * t)) - 1) ## Faiz orani.
r ## Faiz orani.
#===

#==================================== SON ======================================
