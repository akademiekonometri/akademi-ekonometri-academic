#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
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

#================================= Genel Bilgi =================================
# Bu bolumde Amerikada GSYH ile ilgili verileri yayinlayan kurum olan Bureau of Economic Analysis (BEA)'den direkt olarak 1 veri seti indirecegiz. Bu veri seti icinden 2 farkli degiskeni kullanacagiz: GDP (Gross Domestic Product) yani GSYH ve GDP Deflator (Implicit Price Deflators for Gross Domestic Product) yani GSYH Deflatoru indeksi.

# 1. GDP ve GDP Deflator:
    ## GDP ve GDP Deflator hakkindaki ana sayfaya su linkten ulasabilirsiniz: https://www.bea.gov/data/gdp
    ## GDP ve GDP Deflator datasi ile ilgili tum detayli metadata bilgisine su linkten ulasabilirsiniz: https://www.bea.gov/data/gdp/gross-domestic-product
    ## GDP ve GDP Deflator interaktif datasina su linkten ulasabilirsiniz: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=2#reqid=19&step=2&isuri=1&1921=survey
    ## GDP ve GDP Deflator datasi "Section 1"'den interaktif olarak indirilebilir. Fakat biz bu bolumun hemen ustunde verilen "view data in XLS or other formats" adli linke tiklayip tek bir excel dosyasi olarak tum verileri indirecegiz.
    ## GDP ve GDP Deflator datasina su linkten ulasabilirsiniz: https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx
    ## Biz bu indirilen data icinden sadece ceyreklik nominal GDP (indirilen excel dosyasindaki T10105-Q tablosunda A191RC kodu ile verilen) ve ceyreklik GDP deflator (indirilen excel dosyasindaki T10109-Q tablosunda A191RD kodu ile verilen) datasini kullanacagiz.
    ## Indirilen bu iki seri icinden mevsimsellik etkisi cikartilmistir (seasonally adjusted)
    ## GDP Deflator datasinda baz yil 2012=100 olarak verilmistir.

#========================= Datayi Indirmek ve Kaydetmek ========================
# Datasiyi indirmek icin dosya adi ve linkler.
file.name <- c("GDP.xlsx") ## Indirilecek datalarin dosya isimleri.
fileURL <- c("https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx") ## Datanin indirilecegi linkler.

invisible(file.remove(list.files(pattern = file.name))) ## Datayi guncellemek icin daha onceden indirilen dosyalarin silinmesi.
download.file(url = fileURL, file.name, method = "auto") ## Datanin indirilmesi.

# GDP ham datanin yuklenmesi.
gdp <- read_excel(path = file.name, sheet = "T10105-Q", range = cell_limits(c(8, 3), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yukledigimiz gdp datasini read_excel fonksiyonu tibble formatinda kaydediyor.
gdp <- as.data.frame(gdp, stringsAsFactors = TRUE) ## GDP datasini data.frame formatina ceviriyoruz.

# GDP ham datanin yuklenmesi.
gdp.deflator <- read_excel(path = file.name, sheet = "T10109-Q", range = cell_limits(c(8, 3), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yukledigimiz gdp deflator datasini read_excel fonksiyonu tibble formatinda kaydediyor.
gdp.deflator <- as.data.frame(gdp.deflator, stringsAsFactors = TRUE) ## GDP deflator datasini data.frame formatina ceviriyoruz.

# GDP ve GDP Deflator datalarinin birlestirilmeleri.
sum(colnames(gdp) != colnames(gdp.deflator)) ## Birlestirilme sirasinda basit bir yontem kullanilacagindan her iki datanin sutun isimlerinin ayni olup olmadigina bakiliyor.
data <- rbind(gdp, gdp.deflator) ## Birlestirilmis ham data.

# Ham datanin yapisi.
str(data)

# Birlestirilmis datanin RData formatinda ham olarak disa aktarilmasi.
RData.Name <- "GDP"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#========================= Datayi Yukleme ve Temizleme =========================
# Birlestirilmis ham data icin manipulasyon.
test <- data ## Birlestirilmis ham datayi kaybetmemek icin data test olarak tekrar kaydediliyor.
test <- test[complete.cases(test), ] ## Tum degerleri NA olan satirlar siliniyor.
test <- test[test[, 1] != "ZZZZZZ", ] ## Birinci sutunde "ZZZZZZ" olan satirlar siliniyor.

test <- as.data.frame(t(test), stringsAsFactors = FALSE) ## Transpoz aliniyor.
test <- cbind(rownames(test), test) ## Transpoz alindiktan sonra donem satir isimleri olarak kaldigi icin, satir isimleri ve test datasi sutun olarak birlestiriliyor.
colnames(test) <- test[1, ] ## Ilk satirdaki degisken isimleri sutun isimlerine ataniyor.
test <- test[-1, ] ## Ilk satir gereksiz oldugu icin siliniyor.
rownames(test) <- 1:nrow(test) ## Satir isimleri yeniden duzenleniyor.
test[, 1] <- gsub("X", "", test[, 1]) ## Donemi belirten ilk sutundaki "X" degerleri regular expression kullanilarak siliniyor. Regular expression hakkinda daha fazla bilgi icin bknz: https://regexr.com/
test <- separate(data = test, col = 1, into = c("Year", "Quarter"), sep = "Q") ## Birinci sutunu "Q" harfinden bolum Year ve Quarter sutunlarini olusturuyoruz.

str(test)
test <- as.data.frame(sapply(test, function(gsub) gsub(",", "", gsub)), stringsAsFactors = FALSE) ## Bazi sutunlardaki degiskenler icindeki buyuk rakamlar virgul kullanilarak yazilmisti bu virguller silindi. Not: odaliklar nokta ile gosteriliyor.
test <- as.data.frame(sapply(test, function(numeric) as.numeric(numeric)), stringsAsFactors = FALSE) ## Tum degiskenlerin sinifi degistirildi ve numeric yapildi.
str(test)

# Date adli yeni bir degisken olusturuyoruz. Bu zaman serilerini grafige dokerken gunu belirten bu degisken isimize yarayacak.
test$Date <- as.Date(paste(test$Year, (3 * test$Quarter), "1", sep = "-"))

# Secilen degiskenler.
series <- c("Date", "Year", "Quarter", "A191RC", "A191RD")
test <- test[, colnames(test) %in% series] ## Secilen degiskenlere gore datanin alt kumesi alindi.
test <- test[, series]

# Sutun isimlerinin degistirilmesi.
colnames(test)[4:ncol(test)] <- c("GDP", "GDP.Deflator")

# GDP milyon cinsinden yazildigi icin 10^6 ile carpmamiz lazim.
test$GDP <- test$GDP * (10^6)

# Datanin siralanmasi.
test <- test[order(test$Date, test$Year, test$Quarter, decreasing = FALSE, na.last = FALSE), ]
rownames(test) <- 1:nrow(test)

# Birlestirilmis ve temizlenmis datayi data olarak kaydediyoruz.
data <- test

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "GDP__Processed"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================== Temizlenmis Data icin Transformasyon =====================
# Reel GDP datasinin hesaplanmasi. Reel GDP "RGDP" olarak adlandiriliyor.
data$RGDP <- (data$GDP / data$GDP.Deflator) * data[data$Year == 2012 & data$Quarter == 1, ]$GDP.Deflator
## GDP datasini GDP.Deflator kullanarak realize ediyoruz. Elimizdeki data ceyreklik oldugu icin ve baz yili 2012 oldugundan direkt olarak 100 ile carpmaktansa 2012 1. ceyrek GDP.Deflator degeri ile carpip tum GDP datasini 2012 1. Ceyrek fiyatlari ile hesapliyoruz.

# GDP.Deflator indeksindeki baz yil/ceyreklik doneminin degistirilmesi. Baz donem olarak 2000 yili 1. ceyregini secelim.
data$GDP.Deflator.New <- (data$GDP.Deflator / data[data$Year == 2000 & data$Quarter == 1, ]$GDP.Deflator) * 100

# GDP ve RGDP'nin dogal logaritmik transformasyonu.
## Degiskenlerin dogal logatirmasinin alinmasi genellikle degiskenlerdeki varyansin biraz da olsa dusurulerek o degiskenin normal dagilima yakin bir dagilim yapmasi icin tercih edilir. Ayrica dogal logaritmik fonksiyonu alinmis bir degiskende ilk farklar kullanilarak o degiskenin ne kadar buyudugu yaklasik olarak hesaplanabilir.
data$Ln.GDP <- log(data$GDP)
data$Ln.RGDP <- log(data$RGDP)

# Degiskenlerdeki buyume oranin yuzdesel olarak hesaplanmasi.
## Not: hesaplama yapilirken elde edilen yuzdesel buyume degerleri 3. ondalik basamaga yuvarlanmistir.
## Not: Dogal logaritmik fonksiyonu alinmis bir degiskende ilk farklar kullanilarak o degiskenin ne kadar buyudugu yaklasik olarak hesaplanabilir.
temp <- data ## Asagidaki dongunun islemesi icin datayi farkli bir isimle kaydedip onunla islem yapiyoruz.
for (i in 4:ncol(temp)) {
    x <- temp[ , i] ## Degisken
    if (colnames(temp)[i] %in% c("GDP", "RGDP", "GDP.Deflator", "GDP.Deflator.New")) { ## Dogal logaritmasi alinmamis degiskenler.
        growth.x <- 100 * (diff(x, lag = 1, differences = 1) / x[-length(x)])
    }
    if (colnames(temp)[i] %in% c("Ln.GDP", "Ln.RGDP")) { ## Dogal logaritmasi alinmamis degiskenler.
        growth.x <- 100 * diff(x, lag = 1, differences = 1)
    }
    growth.x <- round(growth.x, 3)
    x <- as.data.frame(c(NA, growth.x), stringsAsFactors = FALSE)
    colnames(x) <- paste0("Gr.", colnames(temp)[i]) ## Buyume degerlerini "Gr." ile ifade ediyoruz.
    temp <- cbind(temp, x)
}
data <- temp ## Tekrar ayni ismi kullanmaya basliyoruz.

# Son halini almis datanin yapisinin incelenmesi.
str(data)

# Islenmis datanin RData formatinda disa aktarilmasi.
RData.Name <- "GDP__Processed__Trans"
assign(RData.Name, data)
save(list = RData.Name, file = paste0(RData.Name, ".RData")) ## Disa aktarilacak RData uzantili bu dosya, bu kaynak kodun lokasyonunda olacaktir. Bilgisayarinizda uzerine tikladiginizda RStudio direkt olarak bu dosyayi acacak ve ham datayi R'a yukleyecektir.

#==================================== SON ======================================
