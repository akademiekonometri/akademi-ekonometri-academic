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

#====================== Internetten R Icine Data Indirmek ======================
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
# Bu bolumde Amerikada enflasyon ve istihdam ile ilgili verileri yayinlayan kurum olan Bureau of Labor Statistics (BLS)'den direkt olarak 2 farkli veri seti indirecegiz. Bunlar CPI (Consumer Price Index) yani tuketici fiyatlari indeksi ve PPI (Producer Price Index) yani uretici fiyatlari indeksi.

# 1. CPI:
    ## CPI ile ilgili verilen tum belgelere su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/cu/
    ## CPI datasi ile ilgili tum detayli metadata bilgisine su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/cu/cu.txt
    ## CPI datasi ile ilgili tum ozet metadata bilgisine su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/cu/cu.series
    ## CPI datasina su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems
    ## CPI datasinda sepet kompozisyonuna gore bircok farkli index oldugundan biz bunlar arasinda sadece 2 tane indeksi kullanacagiz: "CUSR0000SA0" (All items in U.S. city average, all urban consumers, seasonally adjusted) ve "CUUR0000SA0" (All items in U.S. city average, all urban consumers, not seasonally adjusted).
    ## CPI datasinda baz yil/ay 1982-84=100 ayi olarak verilmistir.

# 2. PPI:
    ## PPI ile ilgili verilen tum belgelere su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/wp/
    ## PPI datasi ile ilgili tum detayli metadata bilgisine su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/wp/wp.txt
    ## PPI datasi ile ilgili tum ozet metadata bilgisine su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/wp/wp.series
    ## PPI datasina su linkten ulasabilirsiniz: https://download.bls.gov/pub/time.series/wp/wp.data.1.AllCommodities
    ## PPI datasinda sepet kompozisyonuna gore bircok farkli index oldugundan biz bunlar arasinda sadece 1 tane indeksi kullanacagiz: "WPU00000000" (PPI Commodity data for All commodities, not seasonally adjusted).
    ## PPI datasinda baz yil/ay 1982 Ocak ayi olarak verilmistir.

#========================= Datayi Indirmek ve Kaydetmek ========================
# CPI ve PPI datasini toplu olarak indirmek icin dosya adi ve linkler.
file.path <- c("CPI.txt", "PPI.txt") ## Indirilecek datalarin dosya isimleri.
fileURL <- c("https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems", "https://download.bls.gov/pub/time.series/wp/wp.data.1.AllCommodities") ## Datanin indirilecegi linkler.

# Datanin toplu halde indirilip birlestirilmesi.
for (i in 1:length(file.path)) {
    invisible(file.remove(list.files(pattern = file.path[i]))) ## Datayi guncellemek icin daha onceden indirilen dosyalarin silinmesi.
    download.file(url = fileURL[i], destfile = file.path[i], method = "auto") ## Datanin indirilmesi.
    if (i == 1) {
        data <- read.csv(file = file.path[i], header = TRUE, sep = "", dec = ".", colClasses = "character", comment.char = "", na.string = "")
    } else {
        temp <- read.csv(file = file.path[i], header = TRUE, sep = "", dec = ".", colClasses = "character", comment.char = "", na.string = "")
        names(temp) <- names(data)
        data <- rbind(data, temp)
    }
}

# Ham datanin yapisi.
str(data)

# Ham data dosyasinin RData formatinda disa aktarilmasi.
RData.Name <- "CPI_PPI"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#========================= Datayi Yukleme ve Temizleme =========================
# Sutun isimlerinin degistirilmesi.
colnames(data) <- c("Series", "Year", "Month", "Value", "Footnote.Codes")

# Secilen indeksler.
series <- c("CUSR0000SA0", "CUUR0000SA0", "WPU00000000")

# Secilen indekslerin data icinden cekilmesi.
data <- subset(data, Series %in% series & Month != "M13", select = -5) ## Ayrica Ay icindeki M13 degeri iceren satirlarin cikartilmasi. Son olarak son sutunun silinmesi.

# Diger manipulasyonlar.
data$Month <- gsub("M", "", data$Month, fixed = TRUE) ## "Ay" sutununda M0 ve M gorunen yerlere "" yaziliyor.

# Degiskenlerin sinifinin cevrilmesi.
str(data) ## Cevrilmeden onceki datanin yapisi.
invisible(sapply(c(names(data))[2:4], function(numeric) data[, numeric] <<- as.numeric(data[, numeric])))
str(data)

# Datanin donusturulmesi.
data <- reshape2::melt(data, id.vars = c("Series", "Year", "Month"), measure.vars = "Value", variable_name = "variable")
data <- reshape2::dcast(data, Year + Month + variable ~ Series)

# Datanin siralanmasi ve 3. sutunun silinmesi.
data <- data[order(data$Year, data$Month, decreasing = FALSE, na.last = FALSE), -3]
rownames(data) <- 1:nrow(data)

# Sutun isimlerinin degistirilmesi.
colnames(data)[3:ncol(data)] <- c("CPI.S", "CPI.US", "PPI.US")

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "CPI_PPI__Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================================== SON ======================================
