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


## ----Settings.Packages, cache = TRUE--------------
# Devtools paketinin yüklenmesi
## Load.Install fonksiyonunun çalışması için devtools paketi gereklidir.
if("devtools" %in% rownames(installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools"))

# Gerekli paketlerin yüklenmesi.
## Paketleri yüklemeden önce Load.Install fonksiyonunun yüklenip çalıştığından emin olun.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge", "dynlm", "car"))


## ----Settings.Seed--------------------------------
set.seed(1234)


## ----Settings.Working.Directory-------------------
# Değiştirmeyin.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının, yani üzerinde çalıştığınız dosyanın, bilgisayarınızda hangi lokasyonda olduğunu buluyor.
setwd(paste0(main.path, "/")) ## Yeni çalışma klasörü (yani working directory) bu kaynak dosyasının lokasyonunda belirleniyor.


## ----Static.Models.SLR----------------------------
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.

data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Cari dönemde enflasyon ve işsizlik arasındaki statik model.
model.1 <- lm(data = data, formula = inf ~ unem, singular.ok = FALSE)
summary(model.1)

# İlk farklar enflasyon ve işsizlik arasındaki statik model.
model.2 <- lm(data = data, formula = cinf ~ cunem, singular.ok = FALSE)
summary(model.2)


## ----Static.Models.MLR----------------------------
data(intdef) ## Datayı yüklüyoruz.
?intdef ## Datanın metadatası.

data <- intdef ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Cari dönemde faiz, enflasyon ve bütçe açığı arasındaki statik model.
model <- lm(data = data, formula = i3 ~ inf + def, singular.ok = FALSE)
summary(model)


## ----FDL.Model.1----------------------------------
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.

data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
data.ts <- ts(data, start = 1948) ## Data 1948 yılından itibaren başlamış.

# Cari dönemde enflasyon ve işsizlik arasındaki statik model.
model <- dynlm(data = data.ts, formula = inf ~ unem + L(unem, 1), singular.ok = FALSE)
summary(model)

# Parametre tahminleri.
coef(model)

# Etki çarpanının hesaplanması.
coef(model)[[2]]

# Uzun dönem çarpanının hesaplanması.
coef(model)[[2]] + coef(model)[[3]]

# Test: \delta_{1} = 0
linearHypothesis(model, c("L(unem, 1) = 0")) ## p-değeri %5 anlamlılık düzeyinden küçük olduğunda boş hipotez reddedilemez ve statik model kullanılmalıdır.


## ----FDL.Model.2----------------------------------
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.

data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.
data.ts <- ts(data, start = 1913) ## Data 1913 yılından itibaren başlamış.

# Model 1: Vergi muafiyeti ve doğurganlık arasındaki 2. dereceden FDL modeli.
model.1 <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2), singular.ok = FALSE)
summary(model.1)

# Model 1: Parametre tahminleri.
coef(model.1)

# Model 1: Etki çarpanının hesaplanması.
coef(model.1)[[2]]

# Model 1: Uzun dönem çarpanının hesaplanması.
coef(model.1)[[2]] + coef(model.1)[[3]] + coef(model.1)[[4]]

# Test: \delta_{1} = \delta_{2} = 0
linearHypothesis(model.1, c("L(pe, 1) = 0", "L(pe, 2) = 0")) ## p-değeri %5 anlamlılık düzeyinden küçük olduğunda boş hipotez reddedilemez ve statik model kullanılmalıdır.

# Model 2: Vergi muafiyeti ve doğurganlık arasındaki 2. dereceden FDL modeli.
model.2 <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2) + ww2 + pill, singular.ok = FALSE)
summary(model.2)

# Model 2: Parametre tahminleri.
coef(model.2)

# Model 2: Etki çarpanının hesaplanması.
coef(model.2)[[2]]

# Model 2: Uzun dönem çarpanının hesaplanması.
coef(model.2)[[2]] + coef(model.2)[[3]] + coef(model.2)[[4]]

# Test: \delta_{1} = \delta_{2} = 0
linearHypothesis(model.2, c("L(pe, 1) = 0", "L(pe, 2) = 0")) ## p-değeri %5 anlamlılık düzeyinden küçük olduğunda boş hipotez reddedilemez ve statik model kullanılmalıdır.

