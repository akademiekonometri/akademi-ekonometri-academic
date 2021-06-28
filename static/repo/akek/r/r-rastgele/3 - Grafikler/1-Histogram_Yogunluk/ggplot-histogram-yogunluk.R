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

#================ R'da ggplot ile Histogram ve Yogunluk Grafigi ================
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
Load.Install(c("rstudioapi", "ggplot2"))
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

#=============================== Beta Dagilimi =================================
# Beta dagiliminin histogramini ve yogunlugunu olusturma.
#===
# Beta dagilimi datasini olusturma.
n <- 5000 ## Gozlem sayisi
p.alpha <- 2 ## Beta dagilimi alfa parametresi.
p.beta <- 5 ## Beta dagilimi beta parametresi.
value <- rbeta(n = n, shape1 = p.alpha, shape2 = p.beta) ## Beta dagilimi yapan rassal degerler.
beta.dist <- data.frame(Value = value, stringsAsFactors = FALSE) ## Beta dagilimi yapan degerler data icine aliniyor.

# Beta dagiliminin'nin Olasilik Yogunluk grafigi.
temp <- beta.dist
binwidth <- NULL
x.lab <- paste0("$\\Beta(\\alpha = ", p.alpha, ", \\, \\beta = ", p.beta, ")$")
y.lab <- "Olasılık Yoğunluk Fonksiyonu (PDF)"
title <- paste0("Beta Dağılımı - ", "$\\Beta(\\alpha = ", p.alpha, ", \\, \\beta = ", p.beta, ")$: Olasılık Yoğunluk Fonksiyonu")

g <- ggplot(temp, aes(x = Value)) +
    xlab(latex2exp::TeX(x.lab)) + ylab(y.lab) +
    labs(title = latex2exp::TeX(title)) +
    geom_rug(aes(x = Value, y = 0), position = position_jitter(width = 0, height = 0), sides = "b") + ## Verinin sikligini gosterek kucuk tikler yerlestiriyor x eksenine.
    geom_histogram(aes(y = ..density.., fill = ..count..), binwidth = binwidth, colour = "black", size = 0.1) + ## Histogram: yogunluk ve frekans. Ya bu kodu ya da alttakini kullanin.
    # geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey", size = 0.1) + ## Histogram: sadece yogunluk. Ya bu kodu ya da usttekini kullanin.
    scale_fill_gradient("Frekans", low = "#56B1F7", high = "#132B43") + ## Histogrami frekansa gore renklendiriyoruz.
    geom_density(colour = "red", fill = "#FF6666", alpha = 0.3, size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi (alan gosterimi ile).
    stat_density(geom = "line", aes(colour = "Ampirik"), size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi. Ampirik vs Teorik PDF karsilastirmasi istemiyorsan kaldir.
    stat_function(aes(colour = "Teorik"), size = 1, fun = dbeta, args = list(shape1 = p.alpha, shape2 = p.beta)) + ## Teorik olasilik yogunluk fonksiyonu grafigi. Ampirik vs Teorik PDF karsilastirmasi istemiyorsan kaldir.
    scale_colour_manual(name = "PDF", labels = c("Ampirik PDF", "Teorik PDF"), values = c("red", "blue")) + ## Ampirik vs Teorik PDF karsilastirmasi istemiyorsan kaldir.
    theme_grey() +
    theme(legend.position = "right", legend.direction = "vertical") +
    # theme(legend.position = c(0.90, 0.65), legend.direction = "vertical") + ## Legend belli bir yerde istenirse kullanilmali.
    theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, parse = TRUE)

g

#============================== Normal Dagilim =================================
# Normal dagilimin histogramini ve yogunlugunu olusturma.
#===
# Normal dagilim datasini olusturma.
n <- 5000 ## Gozlem sayisi
p.mean <- 0 ## Normal dagilim mean (ortalama) parametresi.
p.sd <- 1 ## Normal dagilim sd (standart hata) parametresi.
value <- rnorm(n = n, mean = p.mean, sd = p.sd) ## Normal dagilim yapan rassal degerler.
norm.dist <- data.frame(Value = value, stringsAsFactors = FALSE) ## Normal dagilim yapan degerler data icine aliniyor.

# Normal dagilimin'nin Olasilik Yogunluk grafigi
temp <- norm.dist
binwidth <- NULL
x.lab <- paste0("$N(", p.mean, ", \\, ", p.sd, ")$")
y.lab <- "Olasılık Yoğunluk Fonksiyonu"
title <- paste0("Normal Dağılımı - ", "$N(", p.mean, ", \\, ", p.sd, ")$: Olasılık Yoğunluk Fonksiyonu")

g <- ggplot(temp, aes(x = Value)) +
    xlab(latex2exp::TeX(x.lab)) + ylab(y.lab) +
    labs(title = latex2exp::TeX(title)) +
    geom_rug(aes(x = Value, y = 0), position = position_jitter(width = 0, height = 0), sides = "b") + ## Verinin sikligini gosterek kucuk tikler yerlestiriyor x eksenine.
    geom_histogram(aes(y = ..density.., fill = ..count..), binwidth = binwidth, colour = "black", size = 0.1) + ## Histogram: yogunluk ve frekans. Ya bu kodu ya da alttakini kullanin.
    # geom_histogram(aes(y = ..density..), binwidth = binwidth, colour = "black", fill = "grey", size = 0.1) + ## Histogram: sadece yogunluk. Ya bu kodu ya da usttekini kullanin.
    scale_fill_gradient("Frekans", low = "#56B1F7", high = "#132B43") + ## Histogrami frekansa gore renklendiriyoruz.
    geom_density(colour = "red", fill = "#FF6666", alpha = 0.3, size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi (alan gosterimi ile).
    stat_density(geom = "line", aes(colour = "Ampirik"), size = 1) + ## Ampirik olasilik yogunluk fonksiyonu grafigi. Ampirik vs Teorik PDF karsilastirmasi istemiyorsan kaldir.
    stat_function(aes(colour = "Teorik"), size = 1, fun = dnorm, args = list(mean = p.mean, sd = p.sd)) + ## Teorik olasilik yogunluk fonksiyonu grafigi. Ampirik vs Teorik PDF karsilastirmasi istemiyorsan kaldir.
    scale_colour_manual(name = "PDF", labels = c("Ampirik PDF", "Teorik PDF"), values = c("red", "blue")) + ## Ampirik vs Teorik PDF karsilastirmasi istemiyorsan kaldir.
    theme_grey() +
    theme(legend.position = "right", legend.direction = "vertical") +
    # theme(legend.position = c(0.90, 0.65), legend.direction = "vertical") + ## Legend belli bir yerde istenirse kullanilmali.
    theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::annotate(geom = "text", label = latex2exp::TeX(paste0("$n = ", n, "$")), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, parse = TRUE)

g

#==================================== SON ======================================
