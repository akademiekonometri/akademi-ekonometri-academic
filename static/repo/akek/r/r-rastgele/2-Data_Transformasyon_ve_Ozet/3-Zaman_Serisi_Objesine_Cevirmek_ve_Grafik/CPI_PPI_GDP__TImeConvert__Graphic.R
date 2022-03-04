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

#================== Zaman Serisi Objesine Cevirmek ve Grafik ===================
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
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "ggplot2"))
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
functions.folder.name <- "../../../_functions/"
figs.tabs.folder.name <- "_figs-tabs/"

#================================= Genel Bilgi =================================
# Daha once Amerikada enflasyon ve istihdam ile ilgili verileri yayinlayan kurum olan Bureau of Labor Statistics (BLS)'den direkt olarak 2 farkli veri seti (CPI ve PPI) indirip bu veri setinden 3 adet indeksi (CUSR0000SA0, CUUR0000SA0, WPU00000000) kullanip datayi temizlemistik. Daha sonra bu temizlenmis data uzerinde bazi transformasyonlar yapip datanin son halini CPI_PPI_Processed_Trans.RData olarak kaydetmistik.

# Yine daha once Amerikada GSYH ile ilgili verileri yayinlayan kurum olan Bureau of Economic Analysis (BEA)'den direkt olarak 1 veri seti (GDP) indirip bu veri serinden 2 farkli degiskeni (GDP (Gross Domestic Product) yani GSYH ve GDP Deflator (Implicit Price Deflators for Gross Domestic Product) yani GSYH Deflatoru indeksi) kullanip datayi temizlemistik. Daha sonra bu temizlenmis data uzerinde bazi transformasyonlar yapip datanin son halini GDP_Processed_Trans.RData olarak kaydetmistik.
    ## Bu datalari indirmek, temizlemek ve transformasyona sokmak icin gerekli bilgilere onceki bolumlerden ulasabilirsiniz.
    ## Bu bolumde ise temizlenmis ve transformasyona ugratilmis CPI_PPI_Processed_Trans.RData ve GDP_Processed_Trans.RData dosyalarini direkt olarak R'in icine cekecegiz ve bu datalari ayri ayri zaman serisi objesine donusturup degiskenlerin grafiklerini olusturmaya calisacagiz.

#================== Datayi Zaman Serisi Objesine Donusturmek ===================
# Datanin yuklenmesi.
# CPI_PPI_Processed_Trans datasi
#===
load(file = "CPI_PPI__Processed__Trans.RData", verbose = FALSE)
data <- CPI_PPI__Processed__Trans ## R icine aktarilan bu temizlenmis ve transforme edilmis datanin ismi CPI_PPI_Processed_Trans oldugundan, data ile degistiriliyor.

# Temizlenmis ve transforme edilmis datanin yapisi.
str(data)

# Datanin zaman serisi objesine cevirilmesi.
data.ts <- ts(data[, 4:ncol(data)], frequency = 12, start = c(as.numeric(data$Year[1]), as.numeric(data$Month[1]))) ## Data objesi frekansi 12 olan zaman serisi objesine cevriliyor. Baslangic olarak ilk yilin ilk ayi aliniyor.

# Zaman serisi objesine cevrilmis datanin yapisi.
str(data.ts)
dim(data.ts)
frequency(data.ts)
start(data.ts)
end(data.ts)

#====================== Grafik: Zaman Serisi Objesi ile ========================
# Tum degiskenleri bir grafik ile gostermek.
plot.ts(data.ts) # Tek bir grafik uzerinde farkli bolumlerde gostermek.
plot.ts(data.ts, plot.type = "single") # Tek bir grafik uzerinde ayni bolumde gostermek.

#======================= Grafik: Data Frame Objesi ile =========================
# Great Depression, Great Recession, Covid Recession icin baslama ve bitis gunleri. Grafiklerde kullanilacak.
great.depression <- c("1929-11-01", "1939-11-01")
great.recession <- c("2007-12-01", "2009-06-01")
covid.recession <- c("2020-02-01", as.character(data$Date[nrow(data)]))

# Varolan datayi kaybetmemek ve yanlislikla degistirmemek icin grafikleri olustururken temp adli datanin kopyasindan yararlanacagiz.
temp <- data

# Once sadece ilk degiskenin grafigini olusturalim.
g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , "CPI.S"], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Time") + ylab("CPI.S") +
    # labs(title = "Consumer Price Index (S)") + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , "CPI.S"])) +
    geom_ribbon(data = subset(temp, great.depression[1] <= Date & Date <= great.depression[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Depression"), alpha = 0.2, show.legend = TRUE) +
    geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
    geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
    scale_colour_manual(name = "", labels = c("Consumer Price Index (S)"), values = c("Variable" = "black")) +
    scale_fill_manual(name = "", labels = c("Great Depression", "Great Recession", "Covid Recession"), values = c("Great Depression" = "green", "Great Recession" = "blue", "Covid Recession" = "red")) +
    guides(fill = guide_legend(override.aes = list(fill = c("green", "blue", "red")))) +
    theme_grey() +
    theme(legend.position = "top")
print(g)

# Tek tek her degisken icin bu grafikleri turetmek cok zaman alacagi icin yukaridaki kodu bir dongu icinde yazalim.
variables <- c("CPI.S", "CPI.US", "PPI.US", "Gr.CPI.S", "Gr.CPI.US", "Gr.PPI.US")
variable.names <- c("Consumer Price Index (S)", "Consumer Price Index (US)", "Producer Price Index (US)", "Inflation (CPI-S)", "Inflation (CPI-US)", "Inflation (PPI-US)")

for (i in 1:length(variables)) {
    g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variables[i]], colour = "Variable"), linetype = 1, size = 1) +
        xlab("Time") + ylab(variable.names[i]) +
        # labs(title = variable.names[i]) + ## Grafik icin isterseniz baslik eklenebilir.
        scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
        scale_y_continuous(breaks = pretty(temp[ , variables[i]])) +
        geom_ribbon(data = subset(temp, great.depression[1] <= Date & Date <= great.depression[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Depression"), alpha = 0.2, show.legend = TRUE) +
        geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
        geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
        scale_colour_manual(name = "", labels = c(variable.names[i]), values = c("Variable" = "black")) +
        scale_fill_manual(name = "", labels = c("Great Depression", "Great Recession", "Covid Recession"), values = c("Great Depression" = "green", "Great Recession" = "blue", "Covid Recession" = "red")) +
        guides(fill = guide_legend(override.aes = list(fill = c("green", "blue", "red")))) +
        theme_grey() +
        theme(legend.position = "top")
    print(g)
}

# Simdide iki tane degiskeni (isterseniz daha cok da olabilir) beraber ayni grafik uzerinde farkli bolumlerde gosterelim.
variables <- c("Gr.CPI.US", "Gr.PPI.US")
variable.names <- c("Inflation (CPI-US)", "Inflation (PPI-US)")

temp <- reshape2::melt(temp, id.vars = c(grep("(Date)|(Year)|(Month)", colnames(temp), value = TRUE)), measure.vars = variables, variable_name = "variable", na.rm = FALSE)
temp$variable <- factor(temp$variable, labels = variable.names)

g <- ggplot(temp) + geom_line(aes(x = Date, y = value, colour = "Variable"), linetype = 1, size = 1) +
    xlab("Time") + ylab("Index") +
    # labs(title = "title") + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    # scale_y_continuous(breaks = pretty(temp$value)) + ## To have free Y axis scale for each variable this part is commented out.
    geom_ribbon(data = subset(temp, great.depression[1] <= Date & Date <= great.depression[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Depression"), alpha = 0.2, show.legend = TRUE) +
    geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
    geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
    scale_colour_manual(name = "", labels = c("Index"), values = c("Variable" = "black")) +
    scale_fill_manual(name = "", labels = c("Great Depression", "Great Recession", "Covid Recession"), values = c("Great Depression" = "green", "Great Recession" = "blue", "Covid Recession" = "red")) +
    guides(fill = guide_legend(override.aes = list(fill = c("green", "blue", "red")))) +
    facet_grid(variable ~ ., scales = "free_y", switch = "y") +
    theme_grey() +
    theme(legend.position = "top")
print(g)

# Simdide ise 4 tane degiskeni (isterseniz daha cok da olabilir) ayri ayri grafiklerde ilk farkli (first differenced) alinmis sekilde gosterelim.
## Ilk farklarin alinmasi zaman serisi analizinda degiskenleri duragan yapmak icin kullanilir, tabi bahse konu degiskenler eger I(1) ise, yani integrated order of one ise.
## Ayrica diger olusturulan grafiklerden farkli olarak bu sefer grafikleri ayri ayri pdf dosyasina yazdirip disari aktaralim.
## PDF dosyalarini olusturuken daha onceki grafiklerden farkli olarak yazilarin boyutunu buyuttuk va bazi degisimler yaptik.

variables <- c("CPI.US", "PPI.US", "Gr.CPI.US", "Gr.PPI.US")
variable.names <- c("Firts Differenced CPI (US)", "First Differenced PPI (US)", "First Differenced Inflation (CPI-US)", "Firts Differenced Inflation (PPI-US)") ## Buyume degerleri zaten ilk farklar alinarak hesaplandigi icin uygulamada genelde bu degiskenler icin ilk farklarin alinmasina gerek kalmadan bunlar duragan halde olurlar. Fakat simdi sadece gostermek amaciyla bu degiskenlerin de ilk farklari alinmistir.

temp <- cbind(data[-1, c(grep("(Date)|(Year)|(Month)", colnames(data)))], as.data.frame(diff(data.ts, lag = 1, diff = 1))) ## Tum degiskenlerin (zamanla ilgili olanlar haric) ilk farklarini aliyoruz ve ilk farklari alinmis datayi temp olarak kaydediyoruz.

for (i in 1:length(variables)) {
    pdf(paste0(figs.tabs.folder.name, variable.names[i], ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9)
    par(mar = c(2, 2, 2, 2))

    g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variables[i]], colour = "Variable"), linetype = 1, size = 1) +
        xlab("Time") + ylab(variable.names[i]) +
        # labs(title = variable.names[i]) + ## Grafik icin isterseniz baslik eklenebilir.
        scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
        scale_y_continuous(breaks = pretty(temp[ , variables[i]])) +
        geom_ribbon(data = subset(temp, great.depression[1] <= Date & Date <= great.depression[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Depression"), alpha = 0.2, show.legend = TRUE) +
        geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
        geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
        scale_colour_manual(name = "", labels = c(variable.names[i]), values = c("Variable" = "black")) +
        scale_fill_manual(name = "", labels = c("Great Depression", "Great Recession", "Covid Recession"), values = c("Great Depression" = "green", "Great Recession" = "blue", "Covid Recession" = "red")) +
        guides(fill = guide_legend(override.aes = list(fill = c("green", "blue", "red")))) +
        theme_grey() +
        theme(legend.position = "top") +
        theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 18)) +
        theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 18)) + theme(legend.text = element_text(size = 18))
    print(g)
    dev.off()
}

#================== Datayi Zaman Serisi Objesine Donusturmek ===================
# Datanin yuklenmesi.
# GDP_Processed_Trans datasi
#===
load(file = "GDP__Processed__Trans.RData", verbose = FALSE)
data <- GDP__Processed__Trans ## R icine aktarilan bu temizlenmis ve transforme edilmis datanin ismi GDP_Processed_Trans oldugundan, data ile degistiriliyor.

# Temizlenmis ve transforme edilmis datanin yapisi.
str(data)

# Daha once olusturdugumuz "GDP.Deflator.New", "Gr.GDP.Deflator.New", "Gr.Ln.GDP" ve "Gr.Ln.RGDP" degiskenlerini kullanmayacagimiz icin datanin icinden cikartiyoruz.
data <- data[, -grep("(GDP.Deflator.New)|(Gr.GDP.Deflator.New)|(Gr.Ln.GDP)|(Gr.Ln.RGDP)", colnames(data))]

# Datanin zaman serisi objesine cevirilmesi.
data.ts <- ts(data[, 4:ncol(data)], frequency = 4, start = c(as.numeric(data$Year[1]), as.numeric(data$Quarter[1]))) ## Data objesi frekansi 12 olan zaman serisi objesine cevriliyor. Baslangic olarak ilk yilin ilk ceyrekligi aliniyor.

# Zaman serisi objesine cevrilmis datanin yapisi.
str(data.ts)
dim(data.ts)
frequency(data.ts)
start(data.ts)
end(data.ts)

#====================== Grafik: Zaman Serisi Objesi ile ========================
# Tum degiskenleri bir grafik ile gostermek.
plot.ts(data.ts) # Tek bir grafik uzerinde farkli bolumlerde gostermek.
plot.ts(data.ts, plot.type = "single") # Tek bir grafik uzerinde ayni bolumde gostermek.

#======================= Grafik: Data Frame Objesi ile =========================
# Great Recession ve Covid Recession icin baslama ve bitis gunleri. Grafiklerde kullanilacak.
great.recession <- c("2007-12-01", "2009-06-01")
covid.recession <- c("2020-02-01", as.character(data$Date[nrow(data)]))

# Varolan datayi kaybetmemek ve yanlislikla degistirmemek icin grafikleri olustururken temp adli datanin kopyasindan yararlanacagiz.
temp <- data

# Once sadece ilk degiskenin grafigini olusturalim.
g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , "GDP"], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Time") + ylab("GDP") +
    # labs(title = "GDP") + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = pretty(temp[ , "GDP"]), labels = human_usd) +
    geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
    geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
    scale_colour_manual(name = "", labels = c("Gross Domestic Product"), values = c("Variable" = "black")) +
    scale_fill_manual(name = "", labels = c("Great Recession", "Covid Recession"), values = c("Great Recession" = "blue", "Covid Recession" = "red")) +
    guides(fill = guide_legend(override.aes = list(fill = c("blue", "red")))) +
    theme_grey() +
    theme(legend.position = "top")
print(g)

# Tek tek her degisken icin bu grafikleri turetmek cok zaman alacagi icin yukaridaki kodu bir dongu icindeyazalim.
variables <- c("GDP", "GDP.Deflator", "RGDP", "Ln.GDP", "Ln.RGDP", "Gr.GDP", "Gr.GDP.Deflator", "Gr.RGDP")
variable.names <- c("GDP", "GDP Deflator", "Real GDP", "Log GDP", "Log Real GDP", "% Growth in GDP", "% Growth in GDP Deflator", "% Growth in Real GDP")

for (i in 1:length(variables)) {
    g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variables[i]], colour = "Variable"), linetype = 1, size = 1) +
        xlab("Time") + ylab(variable.names[i]) +
        # labs(title = variable.names[i]) + ## Grafik icin isterseniz baslik eklenebilir.
        scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
        scale_y_continuous(breaks = pretty(temp[ , variables[i]]), labels = human_numbers) +
        geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
        geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
        scale_colour_manual(name = "", labels = c(variable.names[i]), values = c("Variable" = "black")) +
        scale_fill_manual(name = "", labels = c("Great Recession", "Covid Recession"), values = c("Great Recession" = "blue", "Covid Recession" = "red")) +
        guides(fill = guide_legend(override.aes = list(fill = c("blue", "red")))) +
        theme_grey() +
        theme(legend.position = "top")
    print(g)
}

# Simdide iki tane degiskeni (isterseniz daha cok da olabilir) beraber ayni grafik uzerinde farkli bolumlerde gosterelim.
variables <- c("Gr.GDP", "Gr.GDP.Deflator", "Gr.RGDP")
variable.names <- c("% Growth in GDP", "% Growth in GDP Deflator", "% Growth in Real GDP")

temp <- reshape2::melt(temp, id.vars = c(grep("(Date)|(Year)|(Quarter)", colnames(temp), value = TRUE)), measure.vars = variables, variable_name = "variable", na.rm = FALSE)
temp$variable <- factor(temp$variable, labels = variable.names)

g <- ggplot(temp) + geom_line(aes(x = Date, y = value, colour = "Variable"), linetype = 1, size = 1) +
    xlab("Time") + ylab("Index") +
    # labs(title = title) + ## Grafik icin isterseniz baslik eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(breaks = seq(from = -10, to = 10, by = 2), labels = human_numbers) +
    # scale_y_continuous(breaks = pretty(temp$value)) + ## To have free Y axis scale for each variable this part is commented out.
    geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
    geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
    scale_colour_manual(name = "", labels = c("Index"), values = c("Variable" = "black")) +
    scale_fill_manual(name = "", labels = c("Great Recession", "Covid Recession"), values = c("Great Recession" = "blue", "Covid Recession" = "red")) +
    guides(fill = guide_legend(override.aes = list(fill = c("blue", "red")))) +
    facet_grid(variable ~ ., scales = "free_y", switch = "y") +
    theme_grey() +
    theme(legend.position = "top")
print(g)

# Simdide ise 4 tane degiskeni (isterseniz daha cok da olabilir) ayri ayri grafiklerde ilk farki alinmis (first differenced) sekilde gosterelim.
## Ilk farklarin alinmasi zaman serisi analizinda degiskenleri duragan yapmak icin kullanilir, tabi bahse konu degiskenler eger I(1) ise, yani integrated order of one ise.
## Ayrica diger olusturulan grafiklerden farkli olarak bu sefer grafikleri ayri ayri pdf dosyasina yazdirip disari aktaralim.
## PDF dosyalarini olusturuken daha onceki grafiklerden farkli olarak yazilarin boyutunu buyuttuk va bazi degisimler yaptik.

variables <- c("GDP", "RGDP", "Ln.GDP", "Ln.RGDP", "Gr.GDP", "Gr.RGDP")
variable.names <- c("Firts Differenced GDP", "Firts Differenced RGDP", "Firts Differenced Log GDP", "Firts Differenced Log RGDP", "First Differenced Growth in Real GDP", "First Differenced Growth in Real GDP") ## Buyume degerleri zaten ilk farklar alinarak hesaplandigi icin uygulamada genelde bu degiskenler icin ilk farklarin alinmasina gerek kalmadan bunlar duragan halde olurlar. Fakat simdi sadece gostermek amaciyla bu degiskenlerin de ilk farklari alinmistir.

temp <- cbind(data[-1, c(grep("(Date)|(Year)|(Quarter)", colnames(data)))], as.data.frame(diff(data.ts, lag = 1, diff = 1))) ## Tum degiskenlerin (zamanla ilgili olanlar haric) ilk farklarini aliyoruz ve ilk farklari alinmis datayi temp olarak kaydediyoruz.

for (i in 1:length(variables)) {
    pdf(paste0(figs.tabs.folder.name, variable.names[i], ".pdf"), family = "Times", encoding = "ISOLatin1.enc", pagecentre = TRUE, paper = "special", width = 16, height = 9)
    par(mar = c(2, 2, 2, 2))

    g <- ggplot(temp) + geom_line(aes(x = Date, y = temp[ , variables[i]], colour = "Variable"), linetype = 1, size = 1) +
        xlab("Time") + ylab(variable.names[i]) +
        # labs(title = variable.names[i]) + ## Grafik icin isterseniz baslik eklenebilir.
        scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
        scale_y_continuous(breaks = pretty(temp[ , variables[i]]), labels = human_numbers) +
        geom_ribbon(data = subset(temp, great.recession[1] <= Date & Date <= great.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Great Recession"), alpha = 0.2, show.legend = TRUE) +
        geom_ribbon(data = subset(temp, covid.recession[1] <= Date & Date <= covid.recession[2]), aes(x = Date, ymin = -Inf, ymax = Inf, fill = "Covid Recession"), alpha = 0.2, show.legend = TRUE) +
        scale_colour_manual(name = "", labels = c(variable.names[i]), values = c("Variable" = "black")) +
        scale_fill_manual(name = "", labels = c("Great Recession", "Covid Recession"), values = c("Great Recession" = "blue", "Covid Recession" = "red")) +
        guides(fill = guide_legend(override.aes = list(fill = c("blue", "red")))) +
        theme_grey() +
        theme(legend.position = "top") +
        theme(axis.title = element_text(size = 24)) + theme(axis.text = element_text(size = 18)) +
        theme(legend.key.size = unit(1, "cm")) + theme(legend.key.width = unit(1.2, "cm")) + theme(legend.title = element_text(size = 18)) + theme(legend.text = element_text(size = 18))
    print(g)
    dev.off()
}

#==================================== SON ======================================
