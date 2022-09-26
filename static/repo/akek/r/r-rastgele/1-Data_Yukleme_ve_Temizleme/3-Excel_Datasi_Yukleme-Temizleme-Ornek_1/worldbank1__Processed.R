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

#============ Excel Uzantılı Data Yüklemek ve Temizlemek - Ornek 1 =============
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

#======================== Datayi Yukleme ve Temizleme ==========================
# Data dosyasinin adi.
file.path <- "worldbank1.xls"

# Ham datanin yuklenmesi.
data <- read_excel(path = file.path, sheet = 1, range = cell_limits(c(4, 3), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yukledigimiz datayi read_excel fonksiyonu tibble formatinda kaydediyor.
data <- as.data.frame(data, stringsAsFactors = FALSE) ## Datayi data.frame formatina ceviriyoruz.

# Ham datanin yapisi.
str(data)

# Ham data dosyasinin RData formatinda disa aktarilmasi.
RData.Name <- "workbank1"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

# Data manipulasyonlari.
test <- data[c(5, 11, 12), ] ## 5., 11. ve 12. satirdaki degiskenler seciliyor. Fakat baska satirlardaki degiskenler de farkli rakamlar kullanilarak secilebilir. Yeni data, test olarak adlandiriliyor.
test <- test[, -1] ## Degiskenler secildigi icin ilk sutun siliniyor.
test <- t(test) ## Transpoz aliniyor. Bu noktadan sonra artik datamiz dataframe degil, bu nedenle daha sonra yeniden dataframe'e cevrilmesi lazim.

test <- cbind(rownames(test), test) ## Transpoz alindiktan sonra yillar satir isimleri olarak kaldigi icin, satir isimleri ve test datasi sutun olarak birlestiriliyor.
colnames(test) <- test[1, ] ## Ilk satirdaki degisken isimleri sutun isimlerine ataniyor.
test <- test[-1, ] ## Ilk satir gereksiz oldugu icin siliniyor.
rownames(test) <- 1:nrow(test) ## Satir isimleri yeniden duzenleniyor.
test[, 1] <- gsub("X", "", test[, 1]) ## Yillari belirten ilk sutundaki "X" degerleri regular expression kullanilarak siliniyor. Regular expression hakkinda daha fazla bilgi icin bknz: https://regexr.com/
test <- as.data.frame(test, stringsAsFactors = FALSE) ## Dataframe'e donusum yapiliyor.

# Sutun isimleri degisimi.
colnames(test) <- c("Year", "Pop.Growth", "Migration", "Unemployment")

# Degiskenlerin sinifinin cevrilmesi.
str(test) ## Cevrilmeden onceki datanin yapisi.
test$Year <- as.numeric(test$Year) ## Numeric sinif.
test$Pop.Growth <- as.numeric(test$Pop.Growth)  ## Numeric sinif.
test$Migration <- as.numeric(test$Migration) ## Numeric sinif.
test$Unemployment <- as.numeric(test$Unemployment) ## Numeric sinif.

# Islenmis datayi data olarak kaydediyoruz.
data <- test

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "workbank1__Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================================== SON ======================================
