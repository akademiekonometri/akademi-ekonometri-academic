## ----Settings.Functions, include = FALSE, cache = TRUE----
# Değiştirmeyin.
## Paketleri indirmek ve yüklemek için gerekli fonksiyon.
Load.Install <- function(Package.Names, Quiet = FALSE, Update.All = FALSE) {
    is_installed <- function(my.pkgs) is.element(my.pkgs, utils::installed.packages()[ ,1])
    github.pkgs <- grep("^.*?/.*?$", Package.Names, value = TRUE)
    github.bare.pkgs <- sub(".*?/", "", github.pkgs)
    cran.pkgs <- Package.Names[!(Package.Names %in% github.pkgs)]
    all.pkgs <- c(cran.pkgs, github.bare.pkgs)
    cran.missing <- cran.pkgs[which(!is_installed(cran.pkgs))]
    github.missing <- github.pkgs[which(!is_installed(github.bare.pkgs))]
    if (Update.All == TRUE) {
        cran.missing <- cran.pkgs
        github.missing <- github.pkgs
    } else {
        cran.missing <- cran.pkgs[which(!is_installed(cran.pkgs))]
        github.missing <- github.pkgs[which(!is_installed(github.bare.pkgs))]
    }
    if (length(cran.missing) > 0) {
        suppressWarnings(utils::install.packages(cran.missing, quiet = Quiet, dependencies = TRUE))
    }
    if (length(github.missing) > 0) {
        suppressWarnings(devtools::install_github(github.missing, quiet = Quiet, dependencies = TRUE))
    }
    failed.install <- all.pkgs[which(!is_installed(all.pkgs))]
    if (length(failed.install) > 0) {
        warning(paste0("Some packages failed to install: ", paste(failed.install, collapse = ", "), "."))
    }
    install.pkgs <- all.pkgs[which(is_installed(all.pkgs) == TRUE)]
    for (install.pkgs in install.pkgs) {
        suppressPackageStartupMessages(library(install.pkgs, character.only = TRUE, quietly = Quiet, verbose = FALSE))
    }
}


## ----Settings.Packages, cache = TRUE---------------------
# Devtools paketinin yüklenmesi
## Load.Install fonksiyonunun çalışması için devtools paketi gereklidir.
if("devtools" %in% rownames(installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools"))

# Gerekli paketlerin yüklenmesi.
## Paketleri yüklemeden önce Load.Install fonksiyonunun yüklenip çalıştığından emin olun.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge", "dynlm", "car", "FSA"))


## ----Settings.Seed---------------------------------------
set.seed(1234)


## ----Settings.Working.Directory--------------------------
# Değiştirmeyin.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının, yani üzerinde çalıştığınız dosyanın, bilgisayarınızda hangi lokasyonda olduğunu buluyor.
setwd(paste0(main.path)) ## Yeni çalışma klasörü (yani working directory) bu kaynak dosyasının lokasyonunda belirleniyor.


## ----t.test.Example.1------------------------------------
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.

data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Cari dönemde enflasyon ve işsizlik arasındaki statik model.
model <- lm(data = data, formula = inf ~ unem, singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# pt fonksiyonu ile çift ve tek kuyruklu t-testi. Hesaplanan p-değeridir.
2*pt(q = abs(coef(summary(model))[2, 3]), df = model$df, lower.tail = FALSE) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 != 0 
pt(q = coef(summary(model))[2, 3], df = model$df, lower.tail = TRUE) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta1 < 0
pt(q = coef(summary(model))[2, 3], df = model$df, lower.tail = FALSE) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 > 0

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testi.
hoCoef(model, term = 2, bo = 0, alt = c("two.sided")) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 != 0 
hoCoef(model, term = 2, bo = 0, alt = c("less")) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta1 < 0
hoCoef(model, term = 2, bo = 0, alt = c("greater")) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 > 0

# linearHypothesis fonksiyonu ile çift kuyruklu t-testi.
linearHypothesis(model, c("unem = 0")) ## linearHypothesis fonksiyonu F-istatistiği ve ona ait p-değerini veriyor. Tek bir kısıt için hesaplanan F-istatistiğinin, t-istatistiğine eşit olduğunu unutmayın.


## ----t.test.Example.2------------------------------------
data(intdef) ## Datayı yüklüyoruz.
?intdef ## Datanın metadatası.

data <- intdef ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Cari dönemde faiz, enflasyon ve bütçe açığı arasındaki statik model.
model <- lm(data = data, formula = i3 ~ inf + def, singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# pt fonksiyonu ile çift ve tek kuyruklu t-testi. Hesaplanan p-değeridir.
2*pt(q = abs(coef(summary(model))[3, 3]), df = model$df, lower.tail = FALSE) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta2 != 0 
pt(q = coef(summary(model))[3, 3], df = model$df, lower.tail = TRUE) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta2 < 0
pt(q = coef(summary(model))[3, 3], df = model$df, lower.tail = FALSE) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta2 > 0

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testi.
hoCoef(model, term = 3, bo = 0, alt = c("two.sided")) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta2 != 0 
hoCoef(model, term = 3, bo = 0, alt = c("less")) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta2 < 0
hoCoef(model, term = 3, bo = 0, alt = c("greater")) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta2 > 0

# linearHypothesis fonksiyonu ile çift kuyruklu t-testi.
linearHypothesis(model, c("def = 0")) ## linearHypothesis fonksiyonu F-istatistiği ve ona ait p-değerini veriyor. Tek bir kısıt için hesaplanan F-istatistiğinin, t-istatistiğine eşit olduğunu unutmayın.


## ----F.test.Example.1------------------------------------
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.

data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
data.ts <- ts(data, start = 1948) ## Data 1948 yılından itibaren başlamış.

# Model: Enflasyon ve işsizlik arasındaki FDL(2) modeli.
model <- dynlm(data = data.ts, formula = inf ~ unem + L(unem, 1) + L(unem, 2), singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# Modelin istatistiki olarak genel anlamlılığı için F-testi ve serbestlik dereceleri.
summary(model)$fstatistic

# Hipotez Testi 1: pf fonksiyonu ile modelin istatistiki olarak genel anlamlılığının testi. Hesaplanan p-değeridir.
pf(q = summary(model)$fstatistic[1], df1 = summary(model)$fstatistic[2], df2 = summary(model)$fstatistic[3], lower.tail = FALSE)

# Hipotez Testi 1: linearHypothesis fonksiyonu ile modelin istatistiki olarak genel anlamlılığının testi.
linearHypothesis(model, c("unem = 0", "L(unem, 1) = 0", "L(unem, 2) = 0"))

# Hipotez Testi 2: linearHypothesis fonksiyonu ile gecikmeli değişken parametrelerinin birlikte istatistiki olarak anlamlı olup olmadığının testi.
linearHypothesis(model, c("L(unem, 1) = 0", "L(unem, 2) = 0")) ## p-değeri %5 anlamlılık düzeyinden küçük olduğundan boş hipotez reddedilemez ve statik model kullanılmalıdır.


## ----F.test.Example.2------------------------------------
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.

data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.
data.ts <- ts(data, start = 1913) ## Data 1913 yılından itibaren başlamış.

# Model : Vergi muafiyeti ve doğurganlık arasındaki 3. dereceden FDL modeli.
model <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2) + L(pe, 3), singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# Modelin istatistiki olarak genel anlamlılığı için F-testi ve serbestlik dereceleri.
summary(model)$fstatistic

# Hipotez Testi 1: pf fonksiyonu ile modelin istatistiki olarak genel anlamlılığının testi. Hesaplanan p-değeridir.
pf(q = summary(model)$fstatistic[1], df1 = summary(model)$fstatistic[2], df2 = summary(model)$fstatistic[3], lower.tail = FALSE)

# Hipotez Testi 1: linearHypothesis fonksiyonu ile modelin istatistiki olarak genel anlamlılığının testi.
linearHypothesis(model, c("pe = 0", "L(pe, 1) = 0", "L(pe, 2) = 0", "L(pe, 3) = 0"))

# Hipotez Testi 2: linearHypothesis fonksiyonu ile 2. ve 3. gecikmeli değişken parametrelerinin birlikte istatistiki olarak anlamlı olup olmadığının testi.
linearHypothesis(model, c("L(pe, 2) = 0", "L(pe, 3) = 0")) ## p-değeri %5 anlamlılık düzeyinden küçük olduğundan boş hipotez reddedilemez ve bu nedenle 2. ve 3. gecikmeli değişkenler modelde kullanılmamalıdır.

# Hipotez Testi 3: linearHypothesis fonksiyonu ile gecikmeli değişken parametrelerinin birlikte istatistiki olarak anlamlı olup olmadığının testi.
linearHypothesis(model, c("L(pe, 1) = 0", "L(pe, 2) = 0", "L(pe, 3) = 0")) ## p-değeri %5 anlamlılık düzeyinden küçük olduğundan boş hipotez reddedilemez ve statik model kullanılmalıdır.

