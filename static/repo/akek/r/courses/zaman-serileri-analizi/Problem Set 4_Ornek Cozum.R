#===============================================================================
# Dr. Omer Kara
# Faculty Member, Ph.D.
# Department of Economics | School of Economics and Business Administration
# Eskisehir Osmangazi University | Eskisehir, Turkey 26480
# Mobile: +90 (539) 794-0221 | omer.kara.ylsy@gmail.com
#===============================================================================

#========================== Zaman Serileri Analizi =============================
#================================= Set 2 =======================================
#============================== Problem Set 4 ==================================
#============================== Ornek Cozumler =================================

# Devtools ve okara paketlerinin yüklenmesi.
if("devtools" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(install.packages("devtools")))}
suppressWarnings(suppressMessages(library("devtools"))) ## devtools paketi, okara paketinin yüklenmesi için gereklidir.
if("okara" %in% rownames(utils::installed.packages()) == FALSE) {suppressWarnings(suppressMessages(devtools::install_github("omerkara/okara", force = FALSE)))}
suppressWarnings(suppressMessages(library("okara"))) ## okara paketi.

# Gerekli paketlerin yüklenmesi.
Load.Install(c("rstudioapi", "readxl", "plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc", "reshape2", "scales", "lubridate", "ggplot2", "xtable", "DT", "latex2exp", "forecast", "WDI", "fpp2", "fpp3", "datasets", "quantmod", "FinYang/tsdl", "ggseas", "slider", "ecm", "wooldridge", "dynlm", "car", "AER", "stargazer"))

#================================== Soru 1 =====================================
# Fonksiyonel Form - Esneklik Sorusu
data(USMoney) ## Datayı yüklüyoruz.
?USMoney ## Datanın metadatası.
data.ts <- USMoney ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz. Yüklediğimiz datanın ts yani zaman serisi objesi olduğunu unutmayın.

# Sorudaki model: log(m1_t) = log(gnp_t) + log(gnp_t-1) + log(gnp_t-2) + log(gnp_t-3) + u_t.
model <- dynlm(data = data.ts, formula = log(m1) ~ log(gnp) + L(log(gnp), 1) + L(log(gnp), 2) + L(log(gnp), 3), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# Soru "uzun donem esnekligini" soruyor. Bu nedenle t, t-1, t-2, t-3 zamanlarina ait degiskenin onundeki parametre tahminlerini toplamamiz lazim.

# Cevap:
coef(model)[[2]] + coef(model)[[3]] + coef(model)[[4]] + coef(model)[[5]] ## Ilk parametre tahmini kesim parametresine ait oldugu icin onu almadan geri kalan 4 parametreyi sira numarasina gore topluyoruz.

#================================== Soru 2 =====================================
# T-testi Sorusu 1
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.
data <- phillips ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.

# Sorudaki modelde: inf_t = unem_t + u_t.
model <- lm(data = data, formula = inf ~ unem, singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = 0.19 vs H1: B_1 > 0.19 hipotezini soruyor.
# hoCoef fonksiyonu ile 0.19'a gore sag kuyruk testi t-testi.
hoCoef(model, term = 2, bo = 0.19, alt = c("greater"))

#================================== Soru 3 =====================================
# T-testi Sorusu 2
data(intdef) ## Datayı yüklüyoruz.
?intdef ## Datanın metadatası.
data.ts <- ts(intdef, start = 1948) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: i3_t = inf_t + inf_t-1 + u_t.
model <- dynlm(data = data.ts, formula = i3 ~ inf + L(inf, 1), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = 0.83 vs H1: B_1 < 0.83 hipotezini soruyor.
# hoCoef fonksiyonu ile 0.83'e gore sol kuyruk testi t-testi.
hoCoef(model, term = 2, bo = 0.83, alt = c("less"))

#================================== Soru 4 =====================================
# T-testi Sorusu 3
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.
data <- fertil3 ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.

# Sorudaki model: gfr_t = pe_t + ww2_t + pill_t + u_t.
model <- lm(data = data, formula = gfr ~ pe + ww2 + pill, singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_2 = -0.82 vs H1: B_2 != -0.82 hipotezini soruyor.
# hoCoef fonksiyonu ile -0.82'ye gore cift kuyruklu testi t-testi.
hoCoef(model, term = 3, bo = -0.82, alt = c("two.sided"))

#================================== Soru 5 =====================================
# T-testi Sorusu 4
data(hseinv) ## Datayı yüklüyoruz.
?hseinv ## Datanın metadatası.
data.ts <- ts(hseinv, start = 1947) ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.

# Sorudaki model: invpc_t = price_t + price_t-1 + price_t-2 + u_t.
model <- dynlm(data = data.ts, formula = invpc ~ price + L(price, 1) + L(price, 2), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_3 = 0.89 vs H1: B_3 != 0.89 hipotezini soruyor.
# hoCoef fonksiyonu ile 0.89'a gore cift kuyruklu testi t-testi.
hoCoef(model, term = 4, bo = 0.89, alt = c("two.sided"))

#================================== Soru 6 =====================================
# T-testi Sorusu 5
data(USMoney) ## Datayı yüklüyoruz.
?USMoney ## Datanın metadatası.
data.ts <- USMoney ## Yüklediğimiz datayı "data" ismi ile kaydediyoruz.

# Sorudaki model: m1_t = gnp_t + gnp_t-1 + gnp_t-2 + gnp_t-3 + gnp_t-4 + u_t.
model <- dynlm(data = data.ts, formula = m1 ~ gnp + L(gnp, 1) + L(gnp, 2) + L(gnp, 3) + L(gnp, 4), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef fonksiyonu ile çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_4 = -0.11 vs H1: B_4 > -0.11 hipotezini soruyor.
# hoCoef fonksiyonu ile 0.89'a gore cift kuyruklu testi t-testi.
hoCoef(model, term = 5, bo = -0.11, alt = c("greater"))

#================================== Soru 7 =====================================
# F-testi Sorusu 1


#================================== Soru 8 =====================================
# F-testi Sorusu 2


#================================== Soru 9 =====================================
# F-testi Sorusu 3


#================================== Soru 10 =====================================
# F-testi Sorusu 4


#================================== Soru 11 =====================================
# F-testi Sorusu 5

