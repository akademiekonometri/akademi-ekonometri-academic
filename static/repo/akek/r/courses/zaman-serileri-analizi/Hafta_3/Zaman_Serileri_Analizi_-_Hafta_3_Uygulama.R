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


## ----Settings.Packages, cache = TRUE-------------
# Devtools paketinin yüklenmesi
## Load.Install fonksiyonunun çalışması için devtools paketi gereklidir.
if("devtools" %in% rownames(installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools"))

# Gerekli paketlerin yüklenmesi.
## Paketleri yüklemeden önce Load.Install fonksiyonunun yüklenip çalıştığından emin olun.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge", "dynlm", "car", "FSA", "AER"))


## ----Settings.Seed-------------------------------
set.seed(1234)


## ----Settings.Working.Directory------------------
# Değiştirmeyin.
main.path <- dirname(rstudioapi::getActiveDocumentContext()$path) ## Bu kod otomatik olarak kaynak dosyasının, yani üzerinde çalıştığınız dosyanın, bilgisayarınızda hangi lokasyonda olduğunu buluyor.
setwd(paste0(main.path)) ## Yeni çalışma klasörü (yani working directory) bu kaynak dosyasının lokasyonunda belirleniyor.


## ----t.test.Example.1----------------------------
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
2*pt(q = abs(coef(summary(model))[2, 3]), df = model$df.residual, lower.tail = FALSE) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 != 0 
pt(q = coef(summary(model))[2, 3], df = model$df.residual, lower.tail = TRUE) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta1 < 0
pt(q = coef(summary(model))[2, 3], df = model$df.residual, lower.tail = FALSE) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 > 0

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testi.
hoCoef(model, term = 2, bo = 0, alt = c("two.sided")) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 != 0 
hoCoef(model, term = 2, bo = 0, alt = c("less")) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta1 < 0
hoCoef(model, term = 2, bo = 0, alt = c("greater")) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 > 0

# linearHypothesis fonksiyonu ile çift kuyruklu t-testi.
linearHypothesis(model, c("unem = 0")) ## linearHypothesis fonksiyonu F-istatistiği ve ona ait p-değerini veriyor. Tek bir kısıt için hesaplanan F-istatistiğinin, aynı tek kısıt için hesaplanan t-istatistiğinin karesine eşit olduğunu unutmayın (ayrıca her iki testin sonucu da aynıdır).


## ----t.test.Example.2----------------------------
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
2*pt(q = abs(coef(summary(model))[3, 3]), df = model$df.residual, lower.tail = FALSE) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta2 != 0 
pt(q = coef(summary(model))[3, 3], df = model$df.residual, lower.tail = TRUE) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta2 < 0
pt(q = coef(summary(model))[3, 3], df = model$df.residual, lower.tail = FALSE) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta2 > 0

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testi.
hoCoef(model, term = 3, bo = 0, alt = c("two.sided")) ## Çift kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 != 0 
hoCoef(model, term = 3, bo = 0, alt = c("less")) ## Sol kuyruk t-testi: H0: Beta1 = 0 vs H1: Beta1 < 0
hoCoef(model, term = 3, bo = 0, alt = c("greater")) ## Sağ kuyruklu t-testi: H0: Beta1 = 0 vs H1: Beta1 > 0

# linearHypothesis fonksiyonu ile çift kuyruklu t-testi.
linearHypothesis(model, c("def = 0")) ## linearHypothesis fonksiyonu F-istatistiği ve ona ait p-değerini veriyor. Tek bir kısıt için hesaplanan F-istatistiğinin, aynı tek kısıt için hesaplanan t-istatistiğinin karesine eşit olduğunu unutmayın (ayrıca her iki testin sonucu da aynıdır).


## ----F.test.Example.1----------------------------
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
linearHypothesis(model, c("L(unem, 1) = 0", "L(unem, 2) = 0")) ## p-değeri %5 anlamlılık düzeyinden büyük olduğundan boş hipotez reddedilemez ve statik model kullanılmalıdır.


## ----F.test.Example.2----------------------------
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.

data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.
data.ts <- ts(data, start = 1913) ## Data 1913 yılından itibaren başlamış.

# Model : Vergi muafiyeti ve doğurganlık arasındaki FDL(3) modeli.
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
linearHypothesis(model, c("L(pe, 2) = 0", "L(pe, 3) = 0")) ## p-değeri %5 anlamlılık düzeyinden büyük olduğundan boş hipotez reddedilemez ve bu nedenle 2. ve 3. gecikmeli değişkenler modelde kullanılmamalıdır.

# Hipotez Testi 3: linearHypothesis fonksiyonu ile gecikmeli değişken parametrelerinin birlikte istatistiki olarak anlamlı olup olmadığının testi.
linearHypothesis(model, c("L(pe, 1) = 0", "L(pe, 2) = 0", "L(pe, 3) = 0")) ## p-değeri %5 anlamlılık düzeyinden büyük olduğundan boş hipotez reddedilemez ve statik model kullanılmalıdır.


## ----Functional.Form.Example.1-------------------
data(prminwge) ## Datayı yüklüyoruz.
?prminwge ## Datanın metadatası.

data <- prminwge ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Amerika'daki asgari ücretin Porto Riko'daki istihdam üzerindeki etkisini araştıran statik model.
model <- lm(data = data, formula = log(prepop) ~ log(mincov) + log(usgnp), singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# linearHypothesis fonksiyonu ile Beta1 için çift kuyruklu t-testi.
linearHypothesis(model, c("log(mincov) = 0")) ## linearHypothesis fonksiyonu F-istatistiği ve ona ait p-değerini veriyor. Tek bir kısıt için hesaplanan F-istatistiğinin, aynı tek kısıt için hesaplanan t-istatistiğinin karesine eşit olduğunu unutmayın (ayrıca her iki testin sonucu da aynıdır).

# linearHypothesis fonksiyonu ile Beta2 için çift kuyruklu t-testi.
linearHypothesis(model, c("log(usgnp) = 0")) ## linearHypothesis fonksiyonu F-istatistiği ve ona ait p-değerini veriyor. Tek bir kısıt için hesaplanan F-istatistiğinin, aynı tek kısıt için hesaplanan t-istatistiğinin karesine eşit olduğunu unutmayın (ayrıca her iki testin sonucu da aynıdır).


## ----Functional.Form.Example.2.1-----------------
data(USMoney) ## Datayı yüklüyoruz.
?USMoney ## Datanın metadatası.

data.ts <- USMoney ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz. Yüklediğimiz datanın ts yani zaman serisi objesi olduğunu unutmayın.
str(data.ts) ## Datanın yapısını inceleyelim.
head(data.ts, 10) ## Datanın yapısını ts objesi olarak inceleyelim.
datatable(data.frame(Date = as.Date(time(data.ts)), as.matrix(data.ts), stringsAsFactors = FALSE), filter = "none", options = list(pageLength = 5, autoWidth = TRUE)) ## Datanın yapısını data frame olarak inceleyelim.

## ----Functional.Form.Example.2.2-----------------
# Amerika'daki GSMH'nin para arzı üzerindeki etkisini araştıran FDL(4) modeli.
model <- dynlm(data = data.ts, formula = log(m1) ~ log(gnp) + L(log(gnp), 1) + L(log(gnp), 2) + L(log(gnp), 3) + L(log(gnp), 4), singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# Parametre isimleri daha sonra kullanılmak üzere kaydediliyor.
coef.names <- names(coef(model))

# Etki çarpanının (kısa dönem esnekliği) hesaplanması.
coef(model)[[2]]

# Uzun dönem çarpanının (uzun dönem esnekliği) hesaplanması.
coef(model)[[2]] + coef(model)[[3]] + coef(model)[[4]] + coef(model)[[5]] + coef(model)[[6]]

# Hipotez Testi 1: linearHypothesis fonksiyonu ile modelin istatistiki olarak genel anlamlılığının testi.
linearHypothesis(model, c("log(gnp) = 0", "L(log(gnp), 1) = 0", "L(log(gnp), 2) = 0", "L(log(gnp), 3) = 0", "L(log(gnp), 4) = 0"))
# linearHypothesis(model, coef.names[grep("gnp", coef.names)]) ## Yukarıdaki sonucun aynısını verir.

# Hipotez Testi 2: linearHypothesis fonksiyonu ile gecikmeli değişken parametrelerinin birlikte istatistiki olarak anlamlı olup olmadığının testi.
linearHypothesis(model, c("L(log(gnp), 1) = 0", "L(log(gnp), 2) = 0", "L(log(gnp), 3) = 0", "L(log(gnp), 4) = 0")) ## p-değeri %5 anlamlılık düzeyinden büyük olduğundan boş hipotez reddedilemez ve statik model kullanılmalıdır.
# linearHypothesis(model, coef.names[grep("L", coef.names)]) ## Yukarıdaki sonucun aynısını verir.


## ----Dummy.Variables.Example.1-------------------
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.

data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.

# Bazı değişkenlere ait önemli değerler.
min(data$gfr); max(data$gfr)
min(data$pe); max(data$pe); mean(data$pe)

# Model : Doğurganlık, vergi muafiyeti, savaş ve doğurganlık hapı arasındaki statik model.
model <- lm(data = data, formula = gfr ~ pe + ww2 + pill, singular.ok = FALSE)
summary(model) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model))

# Hipotez Testi 1: linearHypothesis fonksiyonu ile Beta1 için çift kuyruklu t-testi.
linearHypothesis(model, c("pe = 0"))

# Hipotez Testi 2: linearHypothesis fonksiyonu ile Beta2 için çift kuyruklu t-testi.
linearHypothesis(model, c("ww2 = 0"))

# Hipotez Testi 3: linearHypothesis fonksiyonu ile Beta3 için çift kuyruklu t-testi.
linearHypothesis(model, c("pill = 0"))

# Hipotez Testi 4: linearHypothesis fonksiyonu ile modelin istatistiki olarak genel anlamlılığının testi.
linearHypothesis(model, c("pe = 0", "ww2 = 0", "pill = 0"))


## ----Dummy.Variables.Example.2-------------------
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.

data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.
str(data) ## Datanın yapısını inceleyelim.
data ## Datanın yapısını inceleyelim.
data.ts <- ts(data, start = 1913) ## Data 1913 yılından itibaren başlamış.

# Model 1 : Doğurganlık, vergi muafiyeti, savaş ve doğurganlık hapı arasındaki FDL(2) modeli.
model.1 <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2) + ww2 + pill, singular.ok = FALSE)
summary(model.1) ## Tahmin özeti.

# Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.
coef(summary(model.1))

# Cari ve gecikmeli pe değerleri arasındaki korelasyon.
cor(data$pe, data$pe_1, use = "complete.obs", method = c("pearson"))
cor(data$pe, data$pe_2, use = "complete.obs", method = c("pearson"))
cor(data$pe_1, data$pe_2, use = "complete.obs", method = c("pearson"))

# Hipotez Testi 1 (Model 1): linearHypothesis fonksiyonu ile cari ve gecikmeli dönemdeki pe değişkenlerinin beraberce istatistiki anlamlılığının testi.
linearHypothesis(model.1, c("pe = 0", "L(pe, 1) = 0", "L(pe, 2) = 0"))

# Hipotez Testi 2 (Model 1): linearHypothesis fonksiyonu ile gecikmeli değişken parametrelerinin birlikte istatistiki olarak anlamlı olup olmadığının testi.
linearHypothesis(model.1, c("L(pe, 1) = 0", "L(pe, 2) = 0")) ## p-değeri %5 anlamlılık düzeyinden büyük olduğundan boş hipotez reddedilemez ve statik model kullanılmalıdır.

# Model 1 tahmini kullanılarak uzun dönem çarpanının hesaplanması.
coef(model.1)[[2]] + coef(model.1)[[3]] + coef(model.1)[[4]]

# Model 2 : Doğurganlık, vergi muafiyeti, savaş ve doğurganlık hapı arasındaki FDL(2) modelinin (Model 1) uzun dönem çarpanını hesaplamak için dönüştürülmüş hali.
model.2 <- dynlm(data = data.ts, formula = gfr ~ pe + I(L(pe, 1) - pe) + I(L(pe, 2) - pe) + ww2 + pill, singular.ok = FALSE) ## Burada [pe(t-1) - pe(t)] bağımsız değişkeni modele koyabilmek için I() fonksiyonunu kullandığımıza dikkat edin.
summary(model.2) ## Tahmin özeti.

# Hipotez Testi 3 (Model 2): hoCoef fonksiyonu ile çift ve tek kuyruklu t-testi.
hoCoef(model.2, term = 2, bo = 0, alt = c("two.sided")) ## Çift kuyruklu t-testi: H0: Theta0 = 0 vs H1: Theta0 != 0 
linearHypothesis(model.1, c("pe + L(pe, 1) + L(pe, 2) = 0")) ## Model 1'i dönüştürmeden direkt olarak Model 1'in sonuçlarını kullanarak uzun dönem çarpanı 

# Uzun dönem çarpanı için %95'lik güven aralığı (%5 anlamlılık düzeyinde) hesaplanması.
conf.level <- 0.95
alpha <- 1 - conf.level

## confint fonksiyonu ile güven aralığı hesaplanması.
confint(object = model.2, parm = "pe", level = conf.level)

## Elle güven aralığı hesaplanması.
estimate.beta <- coef(summary(model.2))[2, 1] ## Parametre tahmini
estimate.se <- coef(summary(model.2))[2, 2] ## Parametre için standart hata tahmini.
t.critic <- qt(p = (1 - alpha/2), df = model.2$df.residual, lower.tail = TRUE, log.p = FALSE) ## t-kritik değeri.
estimate.beta - (t.critic * estimate.se) ## Güven aralığı alt limiti.
estimate.beta + (t.critic * estimate.se) ## Güven aralığı üst limiti.

