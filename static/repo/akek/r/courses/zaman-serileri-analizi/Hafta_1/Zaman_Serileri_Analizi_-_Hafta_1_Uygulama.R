## ----Settings.Functions, include = FALSE, cache = TRUE----
# Değiştirmeyin.



## ----Settings.Packages, cache = TRUE-----------
# Devtools ve okara paketlerinin yüklenmesi.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(install.packages("devtools")))}
suppressWarnings(suppressMessages(library("devtools"))) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
if("okara" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara", force = FALSE)))}
suppressWarnings(suppressMessages(library("okara"))) ## okara paketi.

# Gerekli paketlerin yüklenmesi.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "lubridate", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge"))


## ----Settings.Seed-----------------------------
set.seed(1234)


## ----Settings.Working.Directory----------------
# Değiştirmeyin.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının, yani üzerinde çalıştığınız dosyanın, bilgisayarınızda hangi lokasyonda olduğunu buluyor.
setwd(paste0(main.path)) ## Yeni çalışma klasörü (yani working directory) bu kaynak dosyasının lokasyonunda belirleniyor.


## ----World.Bank.Data---------------------------
WDIsearch(string = "gdp.*current.*LCU", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.MKTP.CN" GDP (current LCU) datasını kullanacağız.
data <- WDI(country = c("TR"), indicator = c("NY.GDP.MKTP.CN"), start = 1960, end = 2024, extra = FALSE) ## Bir önceki kodda belirlediğimiz indikatör ismini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.
data <- data[, c("year", "NY.GDP.MKTP.CN")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "GDP") ## Değişkenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "GDP")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "GDP"
variable.name <- "Nominal GSYH (TL)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----fpp2.Package.Data-------------------------
data(qauselec) ## Datayı yüklüyoruz.
data <- qauselec ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
head(data, 20) ## Datanın yapısını inceleyelim. İlk 20 gözlemin gösterilmesini istedik.
data <- data.frame(Date = as.Date(date_decimal(as.numeric(time(data)))), Elec.Prod = as.matrix(data), stringsAsFactors = FALSE) ## Yüklediğimiz data time series formatında olduğu için bu datayı data.frame formatına çeviriyoruz.
data ## Datanın yapısını inceleyelim.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "Elec.Prod"
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Çeyreklik)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----tsdl.Package.Data.1-----------------------
Load.Install("tsdl")
tsdl ## tsdl paketinin içinde bulunan verilerin konulara ve frekansa göre kategorisi. Daha sonra bu obje datanın yüklenmesi için kullanılacaktır.
datatable(meta_tsdl, filter = "top", options = list(pageLength = 5, autoWidth = TRUE)) ## tsdl paketinin içinde bulunan verilerin metadatası (bilgisi). Bu tabloya bakarak istediğimız datayı sıra numarasını kullanarak ya da diğer başka fonksiyonları kullanarak seçebiliriz.

## ----tsdl.Package.Data.2-----------------------
# İstenilen datanın yüklenmesi.
## İstediğimiz data 134. sırada bulunan: Monthly Australian imports from Japan: thousands of dollars. Jul 65 – Oct 93.
data <- subset(tsdl, description = "Monthly Australian imports from Japan")[[1]] ## Alternatif olarak datayı bu şekilde de yükleyebiliriz.
data <- tsdl[[134]] ## Sıra numarası kullanarak datayı yüklüyoruz. Bu datanın zaman serisi objesi olduğuna dikkat edin.
head(data, 30) ## Datanın yapısını inceleyelim.

# Grafik
## Elimizdeki veri zaman serisi objesi olduğu için plot fonksiyonunu kullanarak direkt olarak grafiğe dökebiliriz.
plot(data)


## ----Excel.Data--------------------------------
file.path <- "clean-water.xlsx" ## Data dosyasının ismi ve uzantısı.

# Ham datanın yüklenmesi
data <- read_excel(path = file.path, sheet = 3, range = cell_limits(c(1, 1), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yüklediğimiz datayı tibble formatında kaydediyoruz.
data <- as.data.frame(data, stringsAsFactors = FALSE) ## Datayı data.frame formatına çeviriyoruz.
data ## Datanın yapısını inceleyelim.
colnames(data)[1] <- "Month" ## İlk sütun ismini değiştiriyoruz.
data$Month <- 1:12 ## İlk sütundaki hücrelere ay ismi yerine rakam veriyoruz.

# Datanın dönüştürülmesi.
data <- reshape2::melt(data, id.vars = c("Month"), variable_name = "value")

# Bazı değişiklikler.
data <- dplyr::rename(data, Year = variable)
data <- dplyr::rename(data, Clean.Water = value)
data$Year <- as.numeric(as.character(data$Year))
data$Clean.Water <- as.numeric(data$Clean.Water)
data$Date <- as.Date(paste(data$Year, data$Month, "1", sep = "-")) ## Date adlı tarihi belirten yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "Month", "Clean.Water")] ## İstediğimiz değişkenleri belirtiyoruz.
data <- data[data$Year != 2019, ] ## 2019 datasının son gözleminde bir problem olduğu için 2019 yılını siliyoruz.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "Clean.Water"
variable.name <- "İstanbul'a Verilen Temiz Su Miktarı (Ton)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Ay)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----CSV.Data----------------------------------
file.path <- "dam_occupancy.csv" ## Data dosyasının ismi ve uzantısı.

# Ham datanın yüklenmesi
data <- read.csv(file = file.path, header = TRUE, sep = ",", dec = ".", colClasses = "character", comment.char = "", na.string = "") ## Datanın yüklenmesi.
data ## Datanın yapısını inceleyelim.
colnames(data) <- c("Date", "Occupancy.Rate", "Reserved.Water") ## Sütun isimlerinin değiştirilmesi.
data$Occupancy.Rate <- as.numeric(data$Occupancy.Rate) ## Numerik kategori olarak değiştirildi.
data$Reserved.Water <- as.numeric(data$Reserved.Water) ## Numerik kategori olarak değiştirildi.
data$Date <- as.Date(data$Date, tz = "UTC", format = "%Y-%m-%d") ## Date kategori olarak değiştirildi.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "Occupancy.Rate"
variable.name <- "İstanbul Barajları Doluluk Oranı"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Yahoo.Finance.Data------------------------
loadSymbols(Symbols = "GOOG", periodicity = "daily", return.class = "data.frame") ## Yahoo Finance uzerinden buldugumuz sembolu (GOOG) kullanarak ve datanın frekansını seçerek datayı indirip yükleyebiliriz. İndirdiğimiz datanın ismi sembol ismi ile aynı olacaktır.
GOOG ## Datanın yapısını inceleyelim.

data <- data.frame(Date = gsub("\\.", "-", gsub("X", "", rownames(GOOG))), Google = GOOG$GOOG.Close, stringsAsFactors = FALSE) ## Kapanış fiyatları seçiliyor ve sütun ismi olarak Google veriliyor.
data$Date <- as.Date(data$Date) ## Date kategori olarak değiştiriliyor.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "Google"
variable.name <- "Google Hisse Senedi Fiyatı"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Inflastion.Adjustment---------------------
WDIsearch(string = "gdp.*current.*LCU", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.MKTP.CN" GDP (current LCU) datasını kullanacağız.
WDIsearch(string = "gdp.*deflator", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.DEFL.ZS" GDP deflator (base year varies by country) datasını kullanacağız. 2009 baz yılı kullanılıyor.
data <- WDI(country = c("TR"), indicator = c("NY.GDP.MKTP.CN", "NY.GDP.DEFL.ZS"), start = 1960, end = 2024, extra = FALSE) ## Bir önceki kodda belirlediğimiz indikatör isimlerini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.
data <- data[, c("year", "NY.GDP.MKTP.CN", "NY.GDP.DEFL.ZS")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "GDP", "GDP.Deflator") ## Değişkenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "GDP", "GDP.Deflator")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Enflasyon ayarlaması.
data[data$GDP.Deflator == 100, "Year"] ## Datanin baz yılı belirleniyor. 2009 yılı.
data$R.GDP <- (data$GDP / data$GDP.Deflator) * 100 ## Enflasyon ayarlaması yapılıyor.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "R.GDP"
variable.name <- "Real GSYH (2009 TL)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Population.Adjustment---------------------
WDIsearch(string = "gdp.*constant.*LCU", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.MKTP.KN" GDP (constant LCU) datasını kullanacağız. 2009 baz yılı kullanılıyor.
WDIsearch(string = "SP.POP.TOTL", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "SP.POP.TOTL" Total Population datasını kullanacağız.
data <- WDI(country = c("TR"), indicator = c("NY.GDP.MKTP.KN", "SP.POP.TOTL"), start = 1960, end = 2024, extra = FALSE) ## Bir önceki kodda belirlediğimiz indikatör isimlerini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.

data <- data[, c("year", "NY.GDP.MKTP.KN", "SP.POP.TOTL")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "R.GDP", "Population") ## Değişkenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "R.GDP", "Population")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Nüfus ayarlaması.
data$R.GDP.PCAP <- data$R.GDP / data$Population ## Nüfus ayarlaması yapılıyor.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH (2009 TL)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Exchange.Rate.Adjustment------------------
WDIsearch(string = "gdp.*capita.*constant.*LCU", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.PCAP.KN" GDP per capita (constant LCU) datasını kullanacağız. 2009 baz yılı kullanılıyor.
WDIsearch(string = "exchange.*rate.*LCU", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "PA.NUS.FCRF" Official exchange rate (LCU per US$, end period) datasını kullanacağız.
data <- WDI(country = c("TR"), indicator = c("NY.GDP.PCAP.KN", "PA.NUS.FCRF"), start = 2000, end = 2024, extra = FALSE) ## Bir önceki kodda belirlediğimiz indikatör isimlerini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.

data <- data[, c("year", "NY.GDP.PCAP.KN", "PA.NUS.FCRF")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "R.GDP.PCAP", "Exchange.Rate") ## Değişkenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "R.GDP.PCAP", "Exchange.Rate")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Kur ayarlaması ayarlaması.
data$R.GDP.PCAP.USD <- data$R.GDP.PCAP / data$Exchange.Rate ## Nüfus ayarlaması yapılıyor.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "R.GDP.PCAP.USD"
variable.name <- "Kişi Başı Reel GSYH (2009 USD)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Growth.Rate-------------------------------
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.PCAP.KD" GDP per capita (constant 2010 US$) datasını kullanacağız. 2010 baz yılı kullanılıyor.
data <- WDI(country = c("TR"), indicator = c("NY.GDP.PCAP.KD"), start = 1960, end = 2024, extra = FALSE) ## Bir önceki kodda belirlediğimiz indikatör ismini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.

data <- data[, c("year", "NY.GDP.PCAP.KD")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "R.GDP.PCAP") ## Değişkenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "R.GDP.PCAP")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Büyüme oranı hesaplaniyor.
temp <- data ## Aşağıdaki dögünün işlemesi için datayı farklı bir isimle kaydedip onunla işlem yapıyoruz.
for (i in 3:ncol(temp)) {
    x <- temp[ , i]
    growth.x <- 100 * (diff(x, lag = 1, differences = 1) / x[-length(x)])
    growth.x <- round(growth.x, 3)
    x <- as.data.frame(c(NA, growth.x), stringsAsFactors = FALSE)
    colnames(x) <- paste0("Gr.", colnames(temp)[i]) ## Büyüme değerlerini "Gr." ile ifade ediyoruz.
    temp <- cbind(temp, x)
}
data <- temp ## Tekrar aynı ismi kullanmaya başlıyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "Gr.R.GDP.PCAP"
variable.name <- "Kişi Başı Reel GSYH Büyüme Oranı (USD)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Index.Adjustment--------------------------
WDIsearch(string = "consumer.*price.*index", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "FP.CPI.TOTL" Consumer price index (2010 = 100) datasını kullanacağız. 2010 baz yılı kullanılıyor.
data <- WDI(country = c("TR"), indicator = "FP.CPI.TOTL", start = 1960, end = 2024, extra = FALSE)  ## Bir önceki kodda belirlediğimiz indikatör ismini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.

data <- data[, c("year", "FP.CPI.TOTL")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "CPI.2010") ## Değişkenlere yeni isimler veriyoruz.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "CPI.2010")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Yeni bir baz yılına göre (2019 Baz Yılı) indeksi tekrar hesaplıyoruz.
data$CPI.2019 <- (data$CPI.2010/data[data$Year == 2019, "CPI.2010"]) * 100
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "CPI.2019"
variable.name <- "Tüketici Fiyatları İndeksi (2019 Baz Yılı)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    geom_point(aes(x = Date, y = temp[ , variable], colour = "Variable"), size = 1) +
    xlab("Zaman (Yıl)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Math.Transformation-----------------------
loadSymbols(Symbols = "TRY=X", periodicity = "daily", return.class = "data.frame") ## Datanın indirilmesi.
data <- data.frame(Date = gsub("\\.", "-", gsub("X", "", rownames(`TRY=X`))), Close = `TRY=X`$`TRY=X.Close`, stringsAsFactors = FALSE) ## Datanın data.frame olarak kaydedilmesi.
data$Date <- as.Date(data$Date) ## Date kategori olarak değiştirildi.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Logaritmik transformasyon.
data$Ln.Close <- log(data$Close)

# Box-Cox transformasyonu.
value <- data$Close
## Box-cox transformasyonu ile bulunan lambda değerine göre ham veri değiştiriliyor: Eğer lambda = 1 ise transformasyona gerek yok, eğer lambda == 0 ise ln(x), ve eğer lambda farklı bir değer ise (x^lambda - 1)/lambda.
s.lambda <- forecast::BoxCox.lambda(value, method = c("loglik"), lower = -2, upper = 2) ## Box-cox transformasyonu sonucunda bulunan lambda değeri. method = c("guerrero") kriteri de kullanılabilir. Genelde kullanılan alt limit -2 ve üst limit ise 2'dir.
if(s.lambda == 1) { ## Lambda = 1.
    value.boxcox <- value
    message("Box-Cox transformasyonu sonucu: Transformasyona gerek yok.")
} else if(s.lambda == 0) { ## Lambda = 0.
    value.boxcox <- log(value)
    message("Box-Cox transformasyonu sonucu: Logaritmik Transformasyon yapıldı.")
} else { ## Lambda farklı bir değer ise.
    value.boxcox <- (value^s.lambda - 1)/s.lambda
    message(paste0("Box-Cox transformasyonu sonucu: λ = ", round(s.lambda, 2), " kullanılarak Box-Cox transformasyonu yapıldı."))
}
data$Box.Cox.Close <- value.boxcox
data ## Datanın yapısını inceleyelim.

# Düzey Formu: Grafik
temp <- data
variable <- "Close"
variable.name <- "Döviz Kuru (TL/USD)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")

# Logaritmik Form: Grafik
temp <- data
variable <- "Ln.Close"
y.lab <- "Ln Döviz Kuru (TL/USD)"
variable.name <- "Logaritmik Transformasyon ile Döviz Kuru (TL/USD)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(y.lab) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")

# Box-Cox Transformasyonu: Grafik
temp <- data
variable <- "Box.Cox.Close"
y.lab <- paste0("Box-Cox $(\\lambda = ", round(s.lambda, 2), ")$ ile Döviz Kuru (TL/USD)")
variable.name <- paste0("Box-Cox Transformasyonu ile Döviz Kuru (TL/USD)")

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Gün)") + ylab(latex2exp::TeX(y.lab)) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")


## ----Lagged.Variables--------------------------
WDIsearch(string = "gdp.*capita.*constant", field = "name", short = TRUE, cache = NULL) ## Olası seriler ve kodları. Biz "NY.GDP.PCAP.PP.KD" GDP per capita, PPP (constant 2017 international $) datasını kullanacağız. 2017 baz yılı kullanılıyor.
data <- WDI(country = c("US"), indicator = c("NY.GDP.PCAP.PP.KD"), start = 1990, end = 2024, extra = FALSE) ## Bir önceki kodda belirlediğimiz indikatör ismini ve ülke kısaltmasını kullanıyoruz. Datanın başlangıç ve bitiş tarihlerini de ayrı ayrı belirtiyoruz.
data ## Datanın yapısını inceleyelim.

data <- data[, c("year", "NY.GDP.PCAP.PP.KD")] ## İstediğimiz değişkenleri belirtiyoruz.
colnames(data) <- c("Year", "GDP") ## Değişkenlere yeni isimler veriyoruz. Tablo sütunlarının iyi görünmesi için satın alma gücü paritesine göre kişi başı reel gayri safi yurtiçi hasıla özellikle GDP olarak gösterildi.
data$Date <- as.Date(paste(data$Year, "1", "1", sep = "-")) ## Date adlı yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "GDP")] ## Değişkenleri sütun olarak sıralıyoruz.
data <- data[order(data$Date, data$Year, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.
data ## Datanın yapısını inceleyelim.

# Tablo
temp <- data
temp$GDP <- round(temp$GDP, 0) ## Tablonun daha iyi görünmesi için sayılar yuvarlanıyor.
temp$t <- 1:nrow(temp) ## Trend değişkeni ya da t indeks değeri ekleniyor.
temp <- temp[, c("Year", "t", "GDP")] ## Değişkenler sıraya dizildi.
temp$GDP.t1 <- ecm::lagpad(x = temp$GDP, k = 1) ## 1. Gecikme oluşturuldu.
temp$GDP.t2 <- ecm::lagpad(x = temp$GDP, k = 2) ## 2. Gecikme oluşturuldu.
temp$GDP.t3 <- ecm::lagpad(x = temp$GDP, k = 3) ## 3. Gecikme oluşturuldu.
temp$GDP.t4 <- ecm::lagpad(x = temp$GDP, k = 4) ## 4. Gecikme oluşturuldu.
data <- temp
data


## ----Seasonal.Graphics-------------------------
data(qauselec) ## Datayı yüklüyoruz.
data <- qauselec ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
head(data, 20) ## Datanın yapısını inceleyelim.
data <- data.frame(Date = as.Date(date_decimal(as.numeric(time(data)))), Elec.Prod = as.matrix(data), stringsAsFactors = FALSE) ## Yüklediğimiz data time series formatında olduğu için bu datayı data.frame formatına çeviriyoruz.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.

# Grafik
temp <- data
variable <- "Elec.Prod"
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman (Çeyreklik)") + ylab(variable.name) +
    # labs(title = "Başlık") + ## Grafik için isterseniz başlık eklenebilir.
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")

# Mevsimsel Grafik
variable <- "Elec.Prod"
temp <- ts(data = data[, variable], start = c(year(data$Date[1]), month(data$Date[1])), frequency = 4) ## Mevsimsel grafikleri oluşturabilmek için datanın zaman serisi objesi (time series) olması lazım. Bu nedenle dönüşüm yapıldı.
temp <- window(temp, start = NULL, end = c(2009, 4)) ## 2010 yılına ait veri tam olmadığı için çıkartılıyor.
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"
title <- "Avustralya Elektrik Üretimi (milyar kWh): Mevsimsel Grafik"
quarter.labels <- c("Ç1", "Ç2", "Ç3", "Ç4")

ggseasonplot(x = temp, season.labels = quarter.labels, year.labels = TRUE, year.labels.left = TRUE, continuous = FALSE, polar = FALSE, labelgap = 0.04) + ## year.labels = FALSE ve year.labels.left = FALSE olduğunda legend gösterilmez onun yerine yıllara ait etiket gösterilir.
    xlab("Zaman (Çeyreklik)") + ylab(variable.name) +
    labs(title = title) +
    theme_grey()

# Mevsimsel Alt Seri Grafiği
variable <- "Elec.Prod"
temp <- ts(data = data[, variable], start = c(year(data$Date[1]), month(data$Date[1])), frequency = 4) ## Mevsimsel grafikleri oluşturabilmek için datanın zaman serisi objesi (time series) olması lazım. Bu nedenle dönüşüm yapıldı.
temp <- window(temp, start = NULL, end = c(2009, 4)) ## 2010 yılına ait veri tam olmadığı için çıkartılıyor.
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"
title <- "Avustralya Elektrik Üretimi (milyar kWh): Mevsimsel Altseri Grafiği"
quarter.labels <- c("Ç1", "Ç2", "Ç3", "Ç4")

ggsubseriesplot(temp) +
    xlab("Zaman (Çeyreklik)") + ylab(variable.name) +
    labs(title = title) +
    scale_x_continuous(breaks = 1:frequency((temp)), labels = quarter.labels) +
    theme_grey()


## ----Additive.Decomposition--------------------
file.path <- "clean-water.xlsx" ## Data dosyasının ismi ve uzantısı.

# Ham datanın yüklenmesi
data <- read_excel(path = file.path, sheet = 3, range = cell_limits(c(1, 1), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yüklediğimiz datayı tibble formatında kaydediyoruz.
data <- as.data.frame(data, stringsAsFactors = FALSE) ## Datayı data.frame formatına çeviriyoruz.
colnames(data)[1] <- "Month" ## İlk sütun ismini değiştiriyoruz.
data$Month <- 1:12 ## İlk sütundaki hücrelere ay ismi yerine rakam veriyoruz.

# Datanın dönüştürülmesi.
data <- reshape2::melt(data, id.vars = c("Month"), variable_name = "value")

# Bazı değişiklikler.
data <- dplyr::rename(data, Year = variable)
data <- dplyr::rename(data, Clean.Water = value)
data$Year <- as.numeric(as.character(data$Year))
data$Clean.Water <- as.numeric(data$Clean.Water)
data$Date <- as.Date(paste(data$Year, data$Month, "1", sep = "-")) ## Date adlı tarihi belirten yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "Month", "Clean.Water")] ## İstediğimiz değişkenleri belirtiyoruz.
data <- data[data$Year != 2019, ] ## 2019 datasının son gözleminde bir problem olduğu için 2019 yılını siliyoruz.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.

# Grafik
temp <- data
variable <- "Clean.Water"
colnames(temp)[colnames(temp) == variable] <- "y"
variable.name <- "İstanbul'a Verilen Temiz Su Miktarı (Ton)"
title <- "İstanbul'a Verilen Temiz Su Miktarı (Ton): Toplamsal Klasik Dekompozizasyon"
frequency <- 12 ## Datanın frekansı.

ggsdc(temp, aes(x = Date, y = y), frequency = frequency, method = "decompose", type = "additive", facet.titles = c("Veri", "Trend", "Mevsimsellik", "Kalıntı")) +
    geom_line(colour = "darkblue", size = 1) +
    xlab("Zaman (Çeyreklik)") + ylab(variable.name) +
    labs(title = title) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_grey()


## ----Multiplicative.Decomposition--------------
data(qauselec) ## Datayı yüklüyoruz.
data <- qauselec ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
head(data, 20) ## Datanın yapısını inceleyelim.
data <- data.frame(Date = as.Date(date_decimal(as.numeric(time(data)))), Elec.Prod = as.matrix(data), stringsAsFactors = FALSE) ## Yüklediğimiz data time series formatında olduğu için bu datayı data.frame formatına çeviriyoruz.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.

# Grafik
temp <- data
variable <- "Elec.Prod"
colnames(temp)[colnames(temp) == variable] <- "y"
variable.name <- "Avustralya Elektrik Üretimi (milyar kWh)"
title <- "Avustralya Elektrik Üretimi (milyar kWh): Çarpımsal Klasik Dekompozizasyon"
frequency <- 4 ## Datanın frekansı.

ggsdc(temp, aes(x = Date, y = y), frequency = frequency, method = "decompose", type = "multiplicative", facet.titles = c("Veri", "Trend", "Mevsimsellik", "Kalıntı")) +
    geom_line(colour = "darkblue", size = 1) +
    xlab("Zaman (Çeyreklik)") + ylab(variable.name) +
    labs(title = title) +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    theme_grey()


## ----Moving.Average----------------------------
file.path <- "clean-water.xlsx" ## Data dosyasının ismi ve uzantısı.

# Ham datanın yüklenmesi
data <- read_excel(path = file.path, sheet = 3, range = cell_limits(c(1, 1), c(NA, NA)), col_names = TRUE, col_types = "text") ## Yüklediğimiz datayı tibble formatında kaydediyoruz.
data <- as.data.frame(data, stringsAsFactors = FALSE) ## Datayı data.frame formatına çeviriyoruz.
colnames(data)[1] <- "Month" ## İlk sütun ismini değiştiriyoruz.
data$Month <- 1:12 ## İlk sütundaki hücrelere ay ismi yerine rakam veriyoruz.

# Datanın dönüştürülmesi.
data <- reshape2::melt(data, id.vars = c("Month"), variable_name = "value")

# Bazı değişiklikler.
data <- dplyr::rename(data, Year = variable)
data <- dplyr::rename(data, Clean.Water = value)
data$Year <- as.numeric(as.character(data$Year))
data$Clean.Water <- as.numeric(data$Clean.Water)
data$Date <- as.Date(paste(data$Year, data$Month, "1", sep = "-")) ## Date adlı tarihi belirten yeni bir değişken oluşturuyoruz.
data <- data[, c("Date", "Year", "Month", "Clean.Water")] ## İstediğimiz değişkenleri belirtiyoruz.
data <- data[data$Year != 2019, ] ## 2019 datasının son gözleminde bir problem olduğu için 2019 yılını siliyoruz.
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını sıralıyoruz.

# Hareketli ortalama hesabı.
temp <- data
variable <- "Clean.Water"
temp$`3-MA` <- slider::slide_dbl(.x = temp[, variable], .f = mean, .before = 1, .after = 1, .complete = TRUE) ## 3-MA (3 Dönemlik hareketli ortalama) - Ortalama merkezden alınmıştır.
temp$`5-MA` <- slider::slide_dbl(.x = temp[, variable], .f = mean, .before = 2, .after = 2, .complete = TRUE) ## 5-MA (5 Dönemlik hareketli ortalama) - Ortalama merkezden alınmıştır.
temp$`7-MA` <- slider::slide_dbl(.x = temp[, variable], .f = mean, .before = 3, .after = 3, .complete = TRUE) ## 5-MA (7 Dönemlik hareketli ortalama) - Ortalama merkezden alınmıştır.
temp$`9-MA` <- slider::slide_dbl(.x = temp[, variable], .f = mean, .before = 4, .after = 4, .complete = TRUE) ## 9-MA (9 Dönemlik hareketli ortalama) - Ortalama merkezden alınmıştır.
temp.table <- temp ## Hareketli ortalama tablosunda kullanılacak data.

temp1 <- reshape2::melt(temp, id.vars = c(grep("(Date)|(Year)|(Day)|(Week)|(Month)", colnames(temp), value = TRUE)), measure.vars = c(grep("(MA)", colnames(temp), value = TRUE)), variable_name = "Variable", na.rm = FALSE)
temp.figure <- temp1 ## Hareketli ortalama grafiğinde kullanılacak data.

# Tablo
temp <- temp.table
temp$`3-MA` <- round(temp$`3-MA`, 0) ## Tablonun daha iyi görünmesi için sayılar yuvarlanıyor.
temp$`5-MA` <- round(temp$`5-MA`, 0) ## Tablonun daha iyi görünmesi için sayılar yuvarlanıyor.
temp$`7-MA` <- round(temp$`7-MA`, 0) ## Tablonun daha iyi görünmesi için sayılar yuvarlanıyor.
temp$`9-MA` <- round(temp$`9-MA`, 0) ## Tablonun daha iyi görünmesi için sayılar yuvarlanıyor.
temp <- temp[, -1] ## Date değişkeni çıkarılıyor.
temp

# Grafik
temp <- data
temp1 <- temp.figure
variable <- "Clean.Water"
variable.name <- "İstanbul'a Verilen Temiz Su Miktarı (Ton)"
title <- "İstanbul'a Verilen Temiz Su Miktarı (Ton): Hareketli Ortalama (m-MA)"

ggplot(temp1, aes(x = Date, y = value)) +
    geom_line(colour = "#FF6666", size = 0.5, linetype = 1) +
    facet_wrap(variable ~ ., scales = "free_y", strip.position = "top") +
    geom_line(data = temp, aes(x = Date, y = Clean.Water), colour = "darkblue", size = 0.5) +
    xlab("Zaman (Ay)") + ylab(variable.name) +
    labs(title = title) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    theme_grey()


## ----Autocorrelation.Function------------------
n <- 1000 ## Gozlem sayisi.
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = n) ## Simüle edilmiş pür rassal süreç verisi. Zaman serisi objesi olduğunu ve plot(WN) kullanılarak hemen grafiğe dökülebileceğini unutmayın.
str(WN)
mean(WN) ## Pür rassal sürecin ortalaması sıfırdır. Simüle edilmiş veri olduğundan sıfıra yakın çıkıyor.
var(WN) ## Gözlem sayısı yeteri kadar büyük olduğunda merkezi limit teorimi kullanılarak pür rassal sürecin standart normal yaptığı bilinmektedir. Simüle veride de bu durum gözlenmiştir.

data <- data.frame(Date = 1:length(WN), White.Noise = as.matrix(WN), stringsAsFactors = FALSE)
data <- data[order(data$Date, decreasing = FALSE, na.last = FALSE), ] ## Datayı tarihe göre sıralıyoruz.
rownames(data) <- 1:nrow(data) ## Satır sayılarını düzenliyoruz.
data ## Datanın yapısını inceleyelim.

# Grafik
temp <- data
variable <- "White.Noise"
variable.name <- "Simüle Edilmiş Pür Rassal Süreç"

ggplot(temp) +
    geom_line(aes(x = Date, y = temp[ , variable], colour = "Variable"), linetype = 1, size = 1) +
    xlab("Zaman") + ylab(variable.name) +
    scale_y_continuous(breaks = pretty(temp[ , variable])) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    scale_colour_manual(name = "", labels = c(variable.name), values = c("Variable" = "darkblue")) +
    theme_grey() +
    theme(legend.position = "top")

# WN(t) vs WN(t-s) için bazı düzenlemeler.
s <- 1 ## Seçili gecikme uzunluğunu belirliyoruz.
WN <- c(WN) ## Zaman serisini normal bir vektöre çeviriyoruz.
WNt <- WN[(1 + s):length(WN)] ## WN(t). Cari dönem verisi.
WNts <- WN[1:(length(WN) - s)] ## WN(t-s). s gecikme önceki veri.
data <- data.frame(WNt = WNt, WNts = WNts, stringsAsFactors = FALSE)

# WN(t) vs WN(t-s) grafiği.
temp <- data

ggplot(temp) +
    geom_point(aes(x = WNt, y = WNts, colour = "Variable"), size = 3, colour = "darkblue") +
    xlab(paste0("wn_t")) + ylab(paste0("wn_t-", s)) +
    labs(title = paste0("Simüle Edilmiş Pür Rassal Süreç wn_t vs wn_t-", s)) +
    geom_hline(aes(yintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    geom_vline(aes(xintercept = 0), show.legend = FALSE, linetype = 1, colour = 2, size = 0.25) +
    theme_grey()

# WN'nin SACF grafigi
temp <- WN
title <- "Simüle Edilmiş Pür Rassal Süreç: SACF"

ggAcf(x = temp, ci = 0.95, lag.max = 10, type = "correlation", plot = TRUE, demean = TRUE) +
    xlab("Gecikme Değeri") + ylab("Örneklem Otokorelasyon Katsayısı") +
    labs(title = title) +
    theme_grey()

