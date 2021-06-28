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

#========================= R'da Kredi Odemesi Hesaplama ========================
# Notlar:
#
## Bu yazıda kullandığımız datayı (eger varsa) web sitemizdeki ilgili bölümde bulabilirsiniz.
## Aşağıdaki R kodu, öncelikle gerekli R paketlerini yüklüyor ve daha sonra working directory'yi bilgisayarınızda bu kaynak dosyasının bulunduğu lokasyona göre değiştiriyor. Son olarak ise ilgili R kodunu çalıştırıyor.

#=============================== Gerekli Paketler ==============================
# Tek bir adımda gerekli paketlerin yüklenmesi ve kurulması.
#===
# Devtools ve okara paketlerinin yüklenmesi.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools")) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara")))
suppressWarnings(library("okara")) ## okara paketi.
#===
Load.Install(c("rstudioapi", "XLConnect", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "FinCal", "ggplot2", "plotly"))
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

#============================= Kredi Odemesi Hesabi ============================
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

#=============================== Kredi Bilgileri ===============================
# Aylik odeme miktarinin hesabi icin on bilgiler.
#===
P <- 10000 ## Kredi miktari.
r <- 0.79 ## Aylik faiz orani (yuzde olarak).
n <- 60 ## Toplam aylik odeme sayisi.
k <- 15 ## KKDF vergisi (yuzde olarak).
b <- 5 ## BSMV vergisi (yuzde olarak).

#=============================== Elle Hesaplama ================================
# Aylik odeme, toplam geri odeme ve toplam faiz odemesini elle hesaplama.
#===
# Faiz, KKDF ve BSMV yuzdelik oranlarinin odaliga donusturulmasi.
r <- r/100
k <- k/100
b <- b/100

# Vergisiz durumda (KKDF ve BSMV yokken) aylik odeme miktarinin hesabi.
m <- P/((((1 + r)^n) - 1)/(r * (1 + r)^n))
m ## Aylik odeme miktari

# Vergili durumda (KKDF ve BSMV varken) aylik odeme miktarinin hesabi.
## KKDF ve BSMV faiz uzerinden aldigi icin yeni faiz oraninin hesaplanmasi gerekir.
r.kb <- r * (1 + (k + b)) ## Vergiyle beraber odeyeceginiz faiz orani.
m <- P/((((1 + r.kb)^n) - 1)/(r.kb * (1 + r.kb)^n))
m ## Aylik odeme miktari.

A <- m * n ## Toplam geri odeme miktari.
A ## Toplam geri odeme miktari.

I <- A - P ## Toplam faiz odemesi.
I ## Toplam faiz odemesi.
#===

#=============================== Elle Hesaplama ================================
# FinCal paketi ile aylik odeme miktarinin hesaplanmasi.
#===
# Vergili durumda (KKDF ve BSMV varken) aylik odeme miktarinin hesabi.
r.kb <- r * (1 + (k + b)) ## Vergiyle beraber odeyeceginiz faiz orani.
m <- pmt(pv = -P, r = r.kb, n = n, fv = 0, type = 0) ## FinCal paketindeki pmt fonksiyonunu kullandik. Kredi miktarinin onundeki -'ye dikkat edin (kredi bir borc oldugundan negatif olarak yazildi.) Fonksiyonun argumanlari: pv (present value - simdiki deger), r (interest rate - faiz orani), n (number of periods - period sayisi), fv (future value - gelecekteki deger), type (eger odeme donemin sonunda ise 0, basinda ise 1 olmali).
m ## Aylik odeme miktari
#===

#================================ Odeme Tablosu ================================
# Simdi yukarida verilen bilgilere gore bir odeme tablosu olusturalim.
#===
degiskenler <- c("Ay", "Borc", "Odeme", "Anapara", "Faiz", "KKDF", "BSMV", "Vergi", "Faiz.Vergi", "Kalan.Borc") ## Tablodaki degisken isimleri.
o.tablo <- matrix(nrow = n, ncol = length(degiskenler)) ## Odeme tablosu olulsturuldu.
o.tablo <- as.data.frame(o.tablo) ## Odeme tablosu data.frame'e cevrildi.
colnames(o.tablo) <- degiskenler ## Sutun isimleri girildi.

## Verilerin toplu halde for dongusu ile girilmesi.
for (i in 1:n) {
    if (i == 1) {
        o.tablo$Borc[i] <- P ## Borc.
    }
    if (i != 1) {
        o.tablo$Borc[i] <- o.tablo$Kalan.Borc[i - 1] ## Borc.
    }
    o.tablo$Ay[i] <- i ## Ay sayisi.
    o.tablo$Odeme[i] <- m ## Taksit tutari.
    o.tablo$Faiz[i] <- o.tablo$Borc[i] * r ## Faiz odemesi.
    o.tablo$KKDF[i] <- o.tablo$Faiz[i] * k ## KKDF vergisi odemesi.
    o.tablo$BSMV[i] <- o.tablo$Faiz[i] * b ## BSMV vergisi odemesi.
    o.tablo$Vergi[i] <- o.tablo$KKDF[i] + o.tablo$BSMV[i] ## Toplam vergisi odemesi.
    o.tablo$Faiz.Vergi[i] <- o.tablo$Faiz[i] + o.tablo$Vergi[i] ## Toplam faiz ve vergi odemesi.
    o.tablo$Anapara[i] <- o.tablo$Odeme[i] - o.tablo$Faiz.Vergi[i] ## Anapara odemesi.
    o.tablo$Kalan.Borc[i] <- o.tablo$Borc[i] - o.tablo$Anapara[i] ## Kalan borc.
}

## Tablonun sonunda toplam satirinin eklenmesi ve rakamsal degerlerin yuvarlanmasi.
temp.plot <- o.tablo
o.tablo <- rbind(o.tablo, NA)
o.tablo$Odeme[n + 1] <- sum(o.tablo$Odeme, na.rm = TRUE)
o.tablo$Anapara[n + 1] <- sum(o.tablo$Anapara, na.rm = TRUE)
o.tablo$Faiz[n + 1] <- sum(o.tablo$Faiz, na.rm = TRUE)
o.tablo$KKDF[n + 1] <- sum(o.tablo$KKDF, na.rm = TRUE)
o.tablo$BSMV[n + 1] <- sum(o.tablo$BSMV, na.rm = TRUE)
o.tablo$Vergi[n + 1] <- sum(o.tablo$Vergi, na.rm = TRUE)
o.tablo$Faiz.Vergi[n + 1] <- sum(o.tablo$Faiz.Vergi, na.rm = TRUE)
o.tablo <- data.frame(sapply(o.tablo, function(round) round(round, 2)), stringsAsFactors = FALSE) ## Rakamsal degerler yuvarlaniyor.
o.tablo$Ay[n + 1] <- "Toplam"

## Grafigin olusturulmasi.
temp <- temp.plot
temp$Ay <- as.factor(temp$Ay)
temp <- reshape2::melt(temp[, c("Vergi", "Faiz", "Anapara", "Ay")], id.vars = "Ay")
temp$value <- round(temp$value, 2)
colnames(temp) <- c("Ay", "Ödeme", "Miktar")

ggplot(temp, aes(x = Ay, y = Miktar, fill = Ödeme)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(y = "Ödeme") +
    scale_y_continuous(limits = c(0, m + 1), labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "top")

plotly::ggplotly(p = ggplot2::last_plot())
#===

#==================================== SON ======================================
