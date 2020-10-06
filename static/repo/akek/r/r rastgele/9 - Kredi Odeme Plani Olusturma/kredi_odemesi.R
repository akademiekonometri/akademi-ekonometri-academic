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

#========================= R'da Kredi Odemesi Hesaplama ========================
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
Load.Install(Package.Names = c("FinCal"))
#==========
## Load.Install(Package.Names = "XLConnect")
## Load.Install(c("XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
#==========

#======================= Kredi Odemesi Hesabi icin R Kodu ======================
# Notlar:
#
## Asagidaki hesaplama aylik olarak geri odenecek kredi odeme tablosunun hesaplamasini gosteriyor.
## Asagidaki hesaplamada zaman periodu ay olarak alinmistir. Bu nedeni bankalarin genellikle aylik faiz oranlariyla kredi paketlerini duyurmasidir. Eger elinizde yillik faiz orani varsa, bu rakami 12'ye bolerek aylik yaklasik faiz oranini asagidaki formulde kullanabilirsiniz.
## Asagidaki hesaplamada Turkiye'de kredi kullaniminda alinan vergilerde hesaplamaya dahil edilmistir. Bunlar KKDF ve BSMV'dir. Detayli bilgi icin asagidaki bilgileri inceleyiniz.

# Daha fazla bilgi icin asagidaki linkleri inceleyebilirsiniz.
## 1: https://www.thebalance.com/loan-payment-calculations-315564
## 2: https://www.codingfinance.com/post/2018-03-23-car-payment/
## 3: https://rstudio-pubs-static.s3.amazonaws.com/16816_32d8346d6b6a411cafb4e95f5504472c.html
## 4: https://www.hesapkurdu.com/ihtiyac-kredisi/h/kkdf-bsmv-kredi-vergisi-hesaplama

# Aylik Odenecek Para icin Formul: m = P/((((1 + r)^n) - 1)/(r * (1 + r)^n))
## A = Toplam geri odeme miktari (ana para + faiz miktari)
## P = Kredi Miktari
## I = Faiz miktari
## r = Aylik faiz orani
## n = Toplam aylik odeme sayisi
## m = Aylik odeme miktari
## k = KKDF (Kaynak Kullanımı Destekleme Fonu). Kredi için alınan faiz üzerinden Gelir İdaresi Başkanlığına aktarılmak üzere banka tarafından %15 olarak alınır. Konut kredisi hariç tüm kredi işlemlerinde hesaplanan faiz üzerinden alınır.
## b = BSMV (Banka Sigorta Muamele Vergisi). Kredi için alınan faiz üzerinden Gelir İdaresi Başkanlığına aktarılmak üzere banka tarafından %5 olarak alınır. Konut kredisi ve konut ipotekli ihtiyaç kredisi haricinde tüm kredi işlemlerinde hesaplanan faiz üzerinden alınır.

#==========
# Aylik odeme miktarinin hesabi icin on bilgiler.
P <- 10000 ## Kredi miktari.
r <- 0.0079 ## Aylik faiz orani (ondalik olarak).
n <- 60 ## Toplam aylik odeme sayisi.
k <- 0.15 ## KKDF vergisi (ondalik olarak).
b <- 0.05 ## BSMV vergisi (ondalik olarak).

# Vergisiz durumda (KKDF ve BSMV yokken) aylik odeme miktarinin hesabi.
m <- P/((((1 + r)^n) - 1)/(r * (1 + r)^n))
m ## Aylik odeme miktari

# Vergili durumda (KKDF ve BSMV varken) aylik odeme miktarinin hesabi.
## KKDF ve BSMV faiz uzerinden aldigi icin yeni faiz oraninin hesaplanmasi gerekir.
r <- r * (1 + (k + b)) ## Vergiyle beraber odeyeceginiz faiz orani.
m <- P/((((1 + r)^n) - 1)/(r * (1 + r)^n))
m ## Aylik odeme miktari

A <- m * n ## Toplam geri odeme miktari.
A ## Toplam geri odeme miktari.

I <- A - P ## Toplam faiz odemesi.
I ## Toplam faiz odemesi.
#==========

#==========
# FinCal paketi ile aylik odeme miktarinin hesaplanmasi icin verilen on bilgiler.
P <- 10000 ## Kredi miktari.
r <- 0.0079 ## Aylik faiz orani (ondalik olarak).
n <- 60 ## Toplam aylik odeme sayisi.
k <- 0.15 ## KKDF vergisi (ondalik olarak).
b <- 0.05 ## BSMV vergisi (ondalik olarak).

# Vergili durumda (KKDF ve BSMV varken) aylik odeme miktarinin hesabi.
r <- r * (1 + (k + b)) ## Vergiyle beraber odeyeceginiz faiz orani.
m <- pmt(pv = -P, r = r, n = n, fv = 0, type = 0) ## FinCal paketindeki pmt fonksiyonunu kullandik. Kredi miktarinin onundeki -'ye dikkat edin (kredi bir borc oldugundan negatif olarak yazildi.) Fonksiyonun argumanlari: pv (present value - simdiki deger), r (interest rate - faiz orani), n (number of periods - period sayisi), fv (future value - gelecekteki deger), type (eger odemi donemin sonunda ise 0, basinda ise 1 olmali).
m ## Aylik odeme miktari
#==========

#==========
# Aylik odeme planini ve diger bilgileri tablo uzerinde gosterilmesi icin on bilgiler.
P <- 10000 ## Kredi miktari.
r <- 0.0079 ## Aylik faiz orani (ondalik olarak).
n <- 60 ## Toplam aylik odeme sayisi.
k <- 0.15 ## KKDF vergisi (ondalik olarak).
b <- 0.05 ## BSMV vergisi (ondalik olarak).

# Vergili durumda (KKDF ve BSMV varken) aylik odeme miktarinin hesabi.
r.bk <- r * (1 + (b + k)) ## Vergiyle beraber odeyeceginiz faiz orani.
m <- P/((((1 + r.bk)^n) - 1)/(r.bk * (1 + r.bk)^n))
m ## Aylik odeme miktari

# Simdi yukarida verilen bilgilere gore bir odeme tablosu olusturalim.
degiskenler <- c("Donem", "Borc", "Taksit.Tutari", "Anapara", "Faiz.Vergi", "Faiz", "KKDF", "BSMV", "Kalan.Borc") ## Tablodaki degisken isimleri.
o.tablo <- matrix(nrow = n, ncol = length(degiskenler)) ## Odeme tablosu olulsturuldu.
o.tablo <- as.data.frame(o.tablo) ## Odeme tablosu data.frame'e cevrildi.
colnames(o.tablo) <- degiskenler ## Sutun isimleri girildi.

## Verilerin toplu halde for dongusu ile girilmesi.
for (i in 1:n) {
    if (i == 1) {
        o.tablo$Donem[i] <- i ## Donem sayisi.
        o.tablo$Borc[i] <- P ## Borc.
        o.tablo$Taksit.Tutari[i] <- m ## Taksit tutari.
        o.tablo$Faiz[i] <- P * r ## Faiz odemesi.
        o.tablo$KKDF[i] <- o.tablo$Faiz[i] * k ## KKDF vergisi odemesi.
        o.tablo$BSMV[i] <- o.tablo$Faiz[i] * b ## BSMV vergisi odemesi.
        o.tablo$Faiz.Vergi[i] <- o.tablo$Faiz[i] + o.tablo$KKDF[i] + o.tablo$BSMV[i] ## Toplam faiz ve vergi odemesi.
        o.tablo$Anapara[i] <- o.tablo$Taksit.Tutari[i] - o.tablo$Faiz.Vergi[i] ## Anapara odemesi.
        o.tablo$Kalan.Borc[i] <- P - o.tablo$Anapara[i] ## Kalan borc.
    }
    if (i != 1) {
        o.tablo$Donem[i] <- i ## Donem sayisi.
        o.tablo$Borc[i] <- o.tablo$Kalan.Borc[i - 1] ## Borc.
        o.tablo$Taksit.Tutari[i] <- m ## Taksit tutari.
        o.tablo$Faiz[i] <- o.tablo$Kalan.Borc[i - 1] * r ## Faiz odemesi.
        o.tablo$KKDF[i] <- o.tablo$Faiz[i] * k ## KKDF vergisi odemesi.
        o.tablo$BSMV[i] <- o.tablo$Faiz[i] * b ## BSMV vergisi odemesi.
        o.tablo$Faiz.Vergi[i] <- o.tablo$Faiz[i] + o.tablo$KKDF[i] + o.tablo$BSMV[i] ## Toplam faiz ve vergi odemesi.
        o.tablo$Anapara[i] <- o.tablo$Taksit.Tutari[i] - o.tablo$Faiz.Vergi[i] ## Anapara odemesi.
        o.tablo$Kalan.Borc[i] <- o.tablo$Kalan.Borc[i - 1] - o.tablo$Anapara[i] ## Kalan borc.
    }
}

o.tablo <- data.frame(sapply(o.tablo, function(round) round(round, 2)), stringsAsFactors = FALSE) ## Daha iyi gorunmesi icin rakamsal degerler yuvarlaniyor.

## Tablonun sonunda toplam satirinin eklenmesi
o.tablo <- rbind(o.tablo, NA)
o.tablo$Donem[n + 1] <- "Toplam"
o.tablo$Taksit.Tutari[n + 1] <- sum(o.tablo$Taksit.Tutari, na.rm = TRUE)
o.tablo$Anapara[n + 1] <- sum(o.tablo$Anapara, na.rm = TRUE)
o.tablo$Faiz.Vergi[n + 1] <- sum(o.tablo$Faiz.Vergi, na.rm = TRUE)
o.tablo$Faiz[n + 1] <- sum(o.tablo$Faiz, na.rm = TRUE)
o.tablo$KKDF[n + 1] <- sum(o.tablo$KKDF, na.rm = TRUE)
o.tablo$BSMV[n + 1] <- sum(o.tablo$BSMV, na.rm = TRUE)
#==========

#==================================== SON ======================================
