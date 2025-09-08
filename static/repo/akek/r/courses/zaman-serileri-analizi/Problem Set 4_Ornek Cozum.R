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
data.ts <- USMoney ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

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
data.ts <- phillips ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki modelde: inf_t = unem_t + u_t.
model <- lm(data = data.ts, formula = inf ~ unem, singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef() fonksiyonu ile çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = 0.47 vs H1: B_1 > 0.47 hipotezini soruyor.
# hoCoef() fonksiyonu ile 0.47'a gore sag kuyruklu testi t-testi.
hoCoef(model, term = 2, bo = 0.47, alt = c("greater")) ## Buyuktur ise: "greater"; kucuktur ise: "less"; esit degildir ise: "two.sided" kullanin.

#================================== Soru 3 =====================================
# T-testi Sorusu 2
data(intdef) ## Datayı yüklüyoruz.
?intdef ## Datanın metadatası.
data.ts <- intdef ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: i3_t = inf_t + u_t.
model <- dynlm(data = data.ts, formula = i3 ~ inf, singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef() fonksiyonu ile tek kisit icin çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = 0.33 vs H1: B_1 > 0.33 hipotezini soruyor.
# hoCoef() fonksiyonu ile 0.33'e gore sag kuyruklu testi t-testi.
hoCoef(model, term = 2, bo = 0.33, alt = c("greater")) ## Buyuktur ise: "greater"; kucuktur ise: "less"; esit degildir ise: "two.sided" kullanin.

#================================== Soru 4 =====================================
# T-testi Sorusu 3
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.
data.ts <- ts(fertil3, start = 1913) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: gfr_t = pe_t + pe_t-1 + ww2_t + pill_t + u_t.
model <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + ww2 + pill, singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef() fonksiyonu ile tek kisit icin çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_3 = 1.05 vs H1: B_3 != 1.05 hipotezini soruyor.
# hoCoef() fonksiyonu ile 1.05'ye gore cift kuyruklu testi t-testi.
hoCoef(model, term = 4, bo = 1.05, alt = c("two.sided")) ## Buyuktur ise: "greater"; kucuktur ise: "less"; esit degildir ise: "two.sided" kullanin.

#================================== Soru 5 =====================================
# T-testi Sorusu 4
data(hseinv) ## Datayı yüklüyoruz.
?hseinv ## Datanın metadatası.
data.ts <- ts(hseinv, start = 1947) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: invpc_t = price_t + price_t-1 + price_t-2 + price_t-3 + u_t.
model <- dynlm(data = data.ts, formula = invpc ~ price + L(price, 1) + L(price, 2) + L(price, 3), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef() fonksiyonu ile tek kisit icin çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_3 = -1.29 vs H1: B_3 > -1.29 hipotezini soruyor.
# hoCoef() fonksiyonu ile -1.29'a gore sag kuyruklu testi t-testi.
hoCoef(model, term = 4, bo = -1.29, alt = c("greater")) ## Buyuktur ise: "greater"; kucuktur ise: "less"; esit degildir ise: "two.sided" kullanin.

#================================== Soru 6 =====================================
# T-testi Sorusu 5
data(USMoney) ## Datayı yüklüyoruz.
?USMoney ## Datanın metadatası.
data.ts <- USMoney ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: m1_t = gnp_t + gnp_t-1 + u_t.
model <- dynlm(data = data.ts, formula = m1 ~ gnp + L(gnp, 1), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# hoCoef() fonksiyonu ile tek kisit icin çift ve tek kuyruklu t-testini istedigimiz degere karsi yapabiliriz. Burada hoCoef() fonksiyonu ile sadece tekil kisit testi yani t-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = 0.12 vs H1: B_1 != 0.12 hipotezini soruyor.
# hoCoef() fonksiyonu ile 0.12'a gore cift kuyruklu testi t-testi.
hoCoef(model, term = 2, bo = 0.12, alt = c("two.sided")) ## Buyuktur ise: "greater"; kucuktur ise: "less"; esit degildir ise: "two.sided" kullanin.

#================================== Soru 7 =====================================
# F-testi Sorusu 1
data(phillips) ## Datayı yüklüyoruz.
?phillips ## Datanın metadatası.
data.ts <- ts(phillips, start = 1948) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: inf_t = unem_t + unem_t-1 + unem_t-2 + unem_t-3 + unem_t-4 + u_t.
model <- dynlm(data = data.ts, formula = inf ~ unem + L(unem, 1) + L(unem, 2) + L(unem, 3) + L(unem, 4), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# linearHypothesis() fonksiyonu ile cogul kisit icin olasu tum F-testlerini yapabiliriz. Burada linearHypothesis() fonksiyonu ile cogul kisit testi yani F-testi yapabilecegimizi unutmayin.

# Soru H0: B_3 = B_4 = B_5 = 0 vs H1: H0 dogru degil hipotezini soruyor.
# linearHypothesis() fonksiyonu ile hipoteze gore F-testi.
linearHypothesis(model, c("L(unem, 2) = 0", "L(unem, 3) = 0", "L(unem, 4) = 0")) ## Test icinde parametrelere ait degisken isimleri model ozetinde nasil gorunuyorsa oyle yazilmali.

#================================== Soru 8 =====================================
# F-testi Sorusu 2
data(intdef) ## Datayı yüklüyoruz.
?intdef ## Datanın metadatası.
data.ts <- ts(intdef, start = 1948) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: i3_t = inf_t + inf_t-1 + inf_t-2 + inf_t-3 + u_t.
model <- dynlm(data = data.ts, formula = i3 ~ inf + L(inf, 1) + L(inf, 2) + L(inf, 3), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# linearHypothesis() fonksiyonu ile cogul kisit icin olasu tum F-testlerini yapabiliriz. Burada linearHypothesis() fonksiyonu ile cogul kisit testi yani F-testi yapabilecegimizi unutmayin.

# Soru H0: B_3 = B_4 = 0 vs H1: H0 dogru degil hipotezini soruyor.
# linearHypothesis() fonksiyonu ile hipoteze gore F-testi.
linearHypothesis(model, c("L(inf, 2) = 0", "L(inf, 3) = 0")) ## Test icinde parametrelere ait degisken isimleri model ozetinde nasil gorunuyorsa oyle yazilmali.

#================================== Soru 9 =====================================
# F-testi Sorusu 3
data(fertil3) ## Datayı yüklüyoruz.
?fertil3 ## Datanın metadatası.
data.ts <- ts(fertil3, start = 1913) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: gfr_t = pe_t + pe_t-1 + pe_t-2 + ww2_t + pill_t + u_t.
model <- dynlm(data = data.ts, formula = gfr ~ pe + L(pe, 1) + L(pe, 2) + ww2 + pill, singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# linearHypothesis() fonksiyonu ile cogul kisit icin olasu tum F-testlerini yapabiliriz. Burada linearHypothesis() fonksiyonu ile cogul kisit testi yani F-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = B_4 = 0 vs H1: H0 dogru degil hipotezini soruyor.
# linearHypothesis() fonksiyonu ile hipoteze gore F-testi.
linearHypothesis(model, c("pe = 0", "ww2 = 0")) ## Test icinde parametrelere ait degisken isimleri model ozetinde nasil gorunuyorsa oyle yazilmali.

#================================== Soru 10 ====================================
# F-testi Sorusu 4
data(hseinv) ## Datayı yüklüyoruz.
?hseinv ## Datanın metadatası.
data.ts <- ts(hseinv, start = 1947) ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: invpc_t = price_t + price_t-1 + price_t-2 + u_t.
model <- dynlm(data = data.ts, formula = invpc ~ price + L(price, 1) + L(price, 2), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# linearHypothesis() fonksiyonu ile cogul kisit icin olasu tum F-testlerini yapabiliriz. Burada linearHypothesis() fonksiyonu ile cogul kisit testi yani F-testi yapabilecegimizi unutmayin.

# Soru H0: B_2 = B_3 = 0 vs H1: H0 dogru degil hipotezini soruyor.
# linearHypothesis() fonksiyonu ile hipoteze gore F-testi.
linearHypothesis(model, c("L(price, 1) = 0", "L(price, 2) = 0")) ## Test icinde parametrelere ait degisken isimleri model ozetinde nasil gorunuyorsa oyle yazilmali.

#================================== Soru 11 ====================================
# F-testi Sorusu 5
data(USMoney) ## Datayı yüklüyoruz.
?USMoney ## Datanın metadatası.
data.ts <- USMoney ## Yüklediğimiz datayı "data.ts" ismi ile kaydediyoruz.

# Sorudaki model: m1_t = gnp_t + gnp_t-1 + gnp_t-2 + u_t.
model <- dynlm(data = data.ts, formula = m1 ~ gnp + L(gnp, 1) + L(gnp, 2), singular.ok = FALSE)
summary(model) ## Tahmin özeti.
coef(model) ## Tum parametre tahminlerini beraberce veriyor.
coef(summary(model)) ## Parametre tahminleri, standart hataları, t-istatistikleri ve p-değerleri.

# linearHypothesis() fonksiyonu ile cogul kisit icin olasu tum F-testlerini yapabiliriz. Burada linearHypothesis() fonksiyonu ile cogul kisit testi yani F-testi yapabilecegimizi unutmayin.

# Soru H0: B_1 = B_3 = 0 vs H1: H0 dogru degil hipotezini soruyor.
# linearHypothesis() fonksiyonu ile hipoteze gore F-testi.
linearHypothesis(model, c("gnp = 0", "L(gnp, 2) = 0")) ## Test icinde parametrelere ait degisken isimleri model ozetinde nasil gorunuyorsa oyle yazilmali.


#==================================== END ======================================
