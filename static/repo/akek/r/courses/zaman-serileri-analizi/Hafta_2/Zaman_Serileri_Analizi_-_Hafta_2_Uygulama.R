## ----Settings.Functions, include = FALSE, cache = TRUE----
# Değiştirmeyin.



## ----Settings.Packages, cache = TRUE--------------------
# Devtools ve okara paketlerinin yüklenmesi.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools")) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara")))
suppressWarnings(library("okara")) ## okara paketi.

# Gerekli paketlerin yüklenmesi.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "lubridate", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge", "dynlm", "car"))


## ----Settings.Seed--------------------------------------
set.seed(1234)


## ----Settings.Working.Directory-------------------------
# Değiştirmeyin.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının, yani üzerinde çalıştığınız dosyanın, bilgisayarınızda hangi lokasyonda olduğunu buluyor.
setwd(paste0(main.path)) ## Yeni çalışma klasörü (yani working directory) bu kaynak dosyasının lokasyonunda belirleniyor.


## ----Static.Models.SLR----------------------------------
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.

data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Cari dönemde enflasyon ve işsizlik arasındaki statik model.
model.1 <- lm(data = data, formula = inf ~ unem, singular.ok = FALSE)
summary(model.1) ## Tahmin özeti.

# İlk farklar enflasyon ve işsizlik arasındaki statik model.
model.2 <- lm(data = data, formula = cinf ~ cunem, singular.ok = FALSE)
summary(model.2) ## Tahmin özeti.


## ----Static.Models.MLR----------------------------------
data(intdef) ## Datayı yüklüyoruz.
?intdef ## Datanın metadatası.

data <- intdef ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Cari dönemde faiz, enflasyon ve bütçe açığı arasındaki statik model.
model <- lm(data = data, formula = i3 ~ inf + def, singular.ok = FALSE)
summary(model) ## Tahmin özeti.


## ----FDL.Models.1---------------------------------------
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.

data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
data.ts <- ts(data, start = 1948) ## Data 1948 yılından itibaren başlamış.

# Model: Enflasyon ve işsizlik arasındaki FDL(1) modeli.
model <- dynlm(data = data.ts, formula = inf ~ unem + L(unem, 1), singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri.
coef(model)

# Etki çarpanının hesaplanması.
coef(model)[[2]]

# Uzun dönem çarpanının hesaplanması.
coef(model)[[2]] + coef(model)[[3]]


## ----FDL.Models.2---------------------------------------
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.

data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.
data.ts <- ts(data, start = 1913) ## Data 1913 yılından itibaren başlamış.

# Model 1: Doğurganlık ve vergi muafiyeti arasındaki FDL(2) modeli.
model.1 <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2), singular.ok = FALSE)
summary(model.1) ## Tahmin özeti.

# Model 1: Parametre tahminleri.
coef(model.1)

# Model 1: Etki çarpanının hesaplanması.
coef(model.1)[[2]]

# Model 1: Uzun dönem çarpanının hesaplanması.
coef(model.1)[[2]] + coef(model.1)[[3]] + coef(model.1)[[4]]

# Model 2: Doğurganlık, vergi muafiyeti, savaş ve doğurganlık hapı arasındaki FDL(2) modeli.
model.2 <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2) + ww2 + pill, singular.ok = FALSE)
summary(model.2) ## Tahmin özeti.

# Model 2: Parametre tahminleri.
coef(model.2)

# Model 2: Etki çarpanının hesaplanması.
coef(model.2)[[2]]

# Model 2: Uzun dönem çarpanının hesaplanması.
coef(model.2)[[2]] + coef(model.2)[[3]] + coef(model.2)[[4]]

