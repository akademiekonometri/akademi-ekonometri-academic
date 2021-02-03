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

#============ Excel Uzantılı Data Yüklemek ve Temizlemek - Ornek 2 =============
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
Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#==========
## Load.Install(Package.Names = "readxl")
## Load.Install(c("readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2"))
#==========

#================================= Genel Bilgi =================================
# Bu bolumde Global Financial Inclusion (Global Findex) Database 2017'den indirdigimiz veriler uzerinde calisacagiz. Global Findex database (GFI), yetişkinlerin nasıl tasarruf ettiği, ödünç aldığı, ödeme yaptığı ve riski nasıl yönettiği konusunda dünyanın en kapsamlı veri kümesidir. GFI database tum dunyayi kapsamasina ragmen bu bolumde sadece Turkiye'ye ait datalari kullandik. Kullanacagimiz degiskenler kadin (female), age (yas), educ (egitim), inc_q (gelir kartil olarak), emp_in (isgucunde ya da degil).

# GFI 2017 ana sayfasina su linkten ulasabilirsiniz: https://globalfindex.worldbank.org/
## 1. World (Tum Dunya icin)
    ## GFI 2017 Worldbank ana sayfasinda su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/3324
    ## GFI 2017 Worldbank, ulkelere gore ayrilmis dataya su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/global-findex
    ## GFI 2017 micro datasina (farkli dosya turlerine gore) su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/3324/get-microdata
    ## GFI 2017 micro datasina (csv dosya turu icin) su linkten indirebilirsiniz: https://microdata.worldbank.org/index.php/catalog/3324/download/44596
    ## GFI 2017 micro data icin metadataya su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/3324/pdf-documentation

## 2. Turkey (Turkiye icin)
    ## GFI 2017 ana sayfasinda su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/3232
    ## GFI 2017 micro datasina (farkli dosya turlerine gore) su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/3232/get-microdata
    ## GFI 2017 micro datasina (excel dosya turu icin) su linkten indirebilirsiniz: https://microdata.worldbank.org/index.php/catalog/3232/download/42722
    ## GFI 2017 micro data icin metadataya su linkten ulasabilirsiniz: https://microdata.worldbank.org/index.php/catalog/3232/pdf-documentation

#======================== Datayi Yukleme ve Temizleme ==========================
# Data dosyasinin adi.
file.path <- "GFI_TR_2017.xls"

# Ham datanin yuklenmesi.
data <- read_excel(path = file.path, sheet = 1, range = cell_limits(c(1, 1), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yukledigimiz datayi read_excel fonksiyonu tibble formatinda kaydediyor.
data <- as.data.frame(data, stringsAsFactors = TRUE) ## Datayi data.frame formatina ceviriyoruz.

# Ham datanin yapisi.
str(data)

# Ham data dosyasinin RData formatinda disa aktarilmasi.
RData.Name <- "GFI_TR_2017"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Data manipulasyonlari.
test <- data[, c("female", "age", "educ", "inc_q", "emp_in")] ## Secilen degiskenler: female, age, educ, inc_q ve emp_in. Fakat baska sutunlardaki degiskenler de secilebilir. Yeni data, test olarak adlandiriliyor.

# Degiskenler numeric degerlere cevriliyor.
sort(unique(test$female)) ## female degiskeninde kac farkli kategori oldugu cikartiliyor.
test$female <- ifelse(test$female == "Male", 0, 1) ## female degiskeninde "Male" gorunen yere 0, "Female" gorunen yere 1 yaziliyor.

test$age <- as.numeric(test$age) ## age degiskeni numeric yapildi.

sort(unique(test$educ)) ## educ degiskeninde kac farkli kategori oldugu cikartiliyor.
test$educ <- ifelse(test$educ == "completed primary or less", 1, ifelse(test$educ == "secondary", 2, 3)) ## educ degiskeninde "completed primary or less" gorunen yere 1, "secondary" gorunen yere 2 yaziliyor ve "completed tertiary or more" gorunen yere 3 yaziliyor.

sort(unique(test$inc_q)) ## inc_q degiskeninde kac farkli kategori oldugu cikartiliyor.
test[which(test$inc_q == "Poorest 20%"), ]$inc_q <- 1 ## "Poorest 20%" icin 1.
test[which(test$inc_q == "Second 20%"), ]$inc_q <- 2 ## "Second 20%" icin 2.
test[which(test$inc_q == "Middle 20%"), ]$inc_q <- 3 ## "Middle 20%" icin 3.
test[which(test$inc_q == "Fourth 20%"), ]$inc_q <- 4 ## "Fourth 20%" icin 4.
test[which(test$inc_q == "Richest 20%"), ]$inc_q <- 5 ## "Richest 20%" icin 5.
test$inc_q <- as.numeric(test$inc_q) ## inc_q degiskeni numeric yapildi.

sort(unique(test$emp_in)) ## emp_in degiskeninde kac farkli kategori oldugu cikartiliyor.
test$emp_in <- ifelse(test$emp_in == "out of workforce", 0, 1) ## emp_in degiskeninde "out of workforce" gorunen yere 1, "in workforce" gorunen yere 2 yaziliyor.

# Sutun isimleri degisimi.
colnames(test) <- c("Gender", "Age", "Education", "Income", "Employed")

# Islenmis datayi data olarak kaydediyoruz.
data <- test

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "GFI_TR_2017__Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================================== SON ======================================
