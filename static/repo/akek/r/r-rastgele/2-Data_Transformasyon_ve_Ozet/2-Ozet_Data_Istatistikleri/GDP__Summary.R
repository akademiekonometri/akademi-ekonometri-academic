#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================
#============================== R Rastgele Serisi ==============================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.netlify.app/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#========================== Ozet Data Istatistikleri ===========================
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
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "pastecs", "stargazer", "gridExtra"))
Load.Install(c("latexpdf")) ## Sadece bilgisayarinizda LaTeX kuruluysa kullanin.
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

#============================ Gerekli Dosya Isimleri ===========================
# Analiz sirasinda gerekli olan kullanici tarafindan belirlenmis dosya isimleri.
#===
figs.tabs.folder.name <- "_figs-tabs"

#================================= Genel Bilgi =================================
# Daha once Amerikada GSYH ile ilgili verileri yayinlayan kurum olan Bureau of Economic Analysis (BEA)'den direkt olarak 1 veri seti (GDP) indirip bu veri serinden 2 farkli degiskeni (GDP (Gross Domestic Product) yani GSYH ve GDP Deflator (Implicit Price Deflators for Gross Domestic Product) yani GSYH Deflatoru indeksi) kullanip datayi temizlemistik. Daha sonra bu temizlenmis data uzerinde bazi transformasyonlar yapip datanin son halini GDP_Processed_Trans.RData olarak kaydetmistik.
    ## Bu datalari indirmek, temizlemek ve transformasyona sokmak icin gerekli bilgilere onceki bolumlerden ulasabilirsiniz.
    ## Bu bolumde ise temizlenmis ve transformasyona ugratilmis GDP_Processed_Trans.RData dosyasini direkt olarak R'in icine cekecegiz ve bu datanin istatistiki ozetini veren tabloyu olusturmaya calisacagiz.

#=============================== Datayi Yuklemek ===============================
# Datanin yuklenmesi.
load(file = "GDP__Processed__Trans.RData", verbose = FALSE)
data <- GDP__Processed__Trans ## R icine aktarilan bu temizlenmis ve transforme edilmis datanin ismi GDP_Processed_Trans oldugundan, data ile degistiriliyor.

# Temizlenmis ve transforme edilmis datanin yapisi.
str(data)

#============== Ozet Data Istatistikleri Tablosunun Olusturulmasi ==============
# Daha once olusturdugumuz "GDP.Deflator.New", "Gr.GDP.Deflator.New", "Gr.Ln.GDP" ve "Gr.Ln.RGDP" degiskenlerini kullanmayacagimiz icin o degiskenler icin ozet data istatistikleri olusturmayacagiz. Ayrica kullanmak istedigimiz degiskenleri de kendi istegimize gore siraliyoruz.
variables <- c("GDP", "GDP.Deflator", "RGDP", "Ln.GDP", "Ln.RGDP", "Gr.GDP", "Gr.GDP.Deflator", "Gr.RGDP")
variable.names <- c("GDP", "GDP Deflator", "Real GDP", "Log GDP", "Log Real GDP", "% Growth in GDP", "% Growth in GDP Deflator", "% Growth in Real GDP")

# Varolan datayi kaybetmemek ve yanlislikla degistirmemek icin grafikleri olustururken temp adli datanin kopyasindan yararlanacagiz.
temp <- data
temp <- temp[, variables]

# Degiskenlerdeki eksik veri sayisini buluyoruz.
sapply(temp, function(missing) sum(is.na(missing))) ## Degiskenlerdeki eksik veri sayisi ayri ayri gosterildi.
sum(sapply(temp, function(missing) sum(is.na(missing)))) ## Tum degiskenlerdeki toplam eksik veri sayisi.

# Ozet data istatistikleri olusturulacak datanin yapisi.
str(temp)

# Ozet data istatistikleri
temp.summary <- stat.desc(temp, basic = TRUE, desc = TRUE, norm = TRUE)
## Kurtosis: https://tr.wikipedia.org/wiki/Bas%C4%B1kl%C4%B1k ve https://en.wikipedia.org/wiki/Kurtosis
## Skewness: https://tr.wikipedia.org/wiki/%C3%87arp%C4%B1kl%C4%B1k ve https://en.wikipedia.org/wiki/Skewness
## Burada hesaplanan kurtosis istatistigi excess kurtosis olarak gecer ve (kurtosis - 3) olarak hesaplanir. Ardindaki mantik normal dagilimin kurtosisi 3 oldugundan normal dagilima gore nasil bir farka sahip oldugu gosterilmeye calisilir.
## Bir degiskenin normal dagilim yaptigini kabul etmek icin Tabachnick et al. (2001) skewness ve kurtosis icin ±2 degerlerinin kabul edilebilcegini soylerken, West et al. (1995) ise skewness icin ±2 kurtosis icin ise ±7 degerlerinin kabul edilebilcegini soyler.

temp.summary <- as.data.frame(t(temp.summary[c("nbr.val", "min", "max", "median", "mean", "std.dev", "skewness", "kurtosis"),])) ## Sectigimiz ozet data istatistikleri. Son olarak data frame objesine ceviriyoruz.
temp.summary <- cbind(variable.names, temp.summary) ## Degisken isimlerini de ilk sutuna yaziyoruz.
colnames(temp.summary) <- c("Variable", "\\# of Obs.", "Min.", "Max.", "Median", "Mean", "Std. Dev.", "Skewness", "Kurtosis") ## Sutun isimlerini degistiriyoruz.
row.names(temp.summary) <- 1:nrow(temp.summary) ## Satir isimlerini duzenliyoruz.

for (i in 2:ncol(temp.summary)) { ## Bazi sutunlarda yuvarlama yapiyoruz.
    if (i == 2) {
        temp.summary[, i] <- format(round(temp.summary[, i], 0), nsmall = 0)
    }
    if (i != 2) {
        temp.summary[, i] <- format(round(temp.summary[, i], 2), nsmall = 2)
    }
}

# Ozet data istatistigini gosteren data objesinin yapisi.
str(temp.summary)

#============ Ozet Data Istatistikleri Tablosunun Disa Aktarilmasi =============
# Simdi de ozet data istatistikleri tablosunu daha sonra kullanilmak amaciyla farkli formatlarda disa aktaralim.
file.name <- "summary_statistics" ## Disa aktarirken her seferinde dosya adini girmekle ugrasmamak adina dosya ismini iceren bir deger olusturuyoruz.
file.path <- paste0(figs.tabs.folder.name, "/", file.name) ## Dosyalari kaydetmek istedigimiz bilgisayarinizda var olan bir klasoru tanimliyoruz.

## stargazer paketi ile.
stargazer(temp.summary, summary = FALSE, type = "text", out = paste0(file.path, ".txt")) ## txt dosyasi olarak.
stargazer(temp.summary, summary = FALSE, type = "html", out = paste0(file.path, ".html")) ## html dosyasi olarak.

## gridExtra paketi ile.
png(paste0(file.path, ".png"), height = 50*nrow(temp.summary), width = 125*ncol(temp.summary)) ## png dosyasi olarak.
grid.table(temp.summary)
dev.off()

jpeg(paste0(file.path, ".jpeg"), height = 50*nrow(temp.summary), width = 125*ncol(temp.summary)) ## jpeg dosyasi olarak.
grid.table(temp.summary)
dev.off()

pdf(paste0(file.path, ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9) ## pdf dosyasi olarak. Not: pdf olarak disa aktardigimizde yazi tipini ve encoding secebildigimize dikkat edin.
grid.table(temp.summary)
dev.off()

## latexpdf paketi ile.
as.pdf(temp.summary, stem = paste0(file.name, "-latex"), dir = "./figs-tabs") ## pdf dosyasi olarak

# Ozet data istatistigini gosteren data objesinin yapisi.
str(temp.summary)

#==================================== SON ======================================
