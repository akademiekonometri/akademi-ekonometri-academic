#============================= Akademi Ekonometri ==============================
#================== Ekonometri, Ekonomi ve Kodlama Platformu ===================

#=============================== Bizi Takip Edin ===============================
# Web Sitemiz: https://akademiekonometri.rbind.io/
# YouTube: https://www.youtube.com/c/AkademiEkonometri
# Twitter: https://twitter.com/AEkonometri
# Instagram: https://www.instagram.com/akademiekonometri/
# E-mail: akademiekonometri@gmail.com
#===============================================================================

#======================== Cointegration Pairwise Tests =========================

# Notes:
#
## This script explains what the below funtions do in general.
### 1. Coint.Pairwise.Tests (Pairwise PO and Johansen)

#============================== General Information ============================
# Some notes:
## 1. Testing for cointegration is a very important step to prevent spurious regression problem. Without any cointegration between I(1) univariate series, the long-run relationship found in often used time series models are invalid although the estimates are statistically significant and R^2 is very high; and hence, we get spurious regression problem as was pointed out by Granger and Newbold (1974) and Phillips (1986). Thus, without testing for cointegration and statistically proving it exists, our interpretations for time series models will be invalid.
## 2. If two or more series are individually integrated but some linear combination of them has a lower order of integration, then the series are said to be cointegrated. For example, suppose we have two first-order integrated (e.g., I(1)) univariate non-stationary time series X and Y such that some linear combination of them is stationary (e.g., I(0)), then we say that X and Y are cointegrated. In other words, while neither X nor Y alone hovers around a constant value, some combination of them does, so we can think of cointegration as describing a particular kind of long-run equilibrium relationship. Also, note that to run a cointegration test, the univariate series should be unit root.
## 3. Before turning to the multidimensional system, cointegration tests on all pairs of series should be applied. It is because that cointegration tests tend to have relatively low power, especially in multidimensional systems. Thus, performing tests on bivariate systems first works as a check procedure for the overall consistency of the results obtained in the multidimensional system (Lütkepohl and Krätzig, 2004).

#================================ Lag Selection ================================
# General information about lag selection.
## 1. An important practical issue for the cointegration tests is the specification of the lag length p.
## 2. PO tests: pshort = trunc[4(N/100)^0.25] and plong = trunc[12(N/100)^0.25] where N = Length of series
## 3. JO tests: Lag length is selected with VARselect function from vars package and AIC (Akaike Information Criterion) is used as default to choose lag length since Kilian (2001) stated that even in finite-order VAR models in many cases of practical interest the AIC is likely to be preferable to the BIC or HQ because MSE is lower and impulse response confidence intervals are more accurate then others. However, other lag selection methods such as BIC, HQ and FPE can also be used by changing Johansen.Lag argument to desired methods. Note that in the literature, it appears that, in general, too few lags in the model lead to rejection of null hypothesis too easily, while too many lags in the model decrease the power of the tests.

#================== Comparison of Cointegration Pairwise Tests =================
## 1. Long-run relationship between univariate series are investigated by running two different cointegration tests: Phillips–Ouliaris (PO) and Johansen (JO) cointegration tests.

## 2. First type of cointegration pairwise test is PO cointegration (direction matters) test. It is used instead of using the common Enger-Granger 2-step method, since Phillips and Ouliaris (1990) shows that residual-based unit root tests applied to the estimated cointegrating residuals do not have the usual Dickey–Fuller distributions under the null hypothesis of no-cointegration and they provide the correct critical values in Phillips-Ouliaris cointegration test. Note that Phillips-Ouliaris Pu is asymmetric, direction matters. In other words, conclusion might change depending on which univariete series is on the right-hand side of the cointegration regression (1st step); and thus, two PO tests are conducted for each time series pairs. In auxiliary regression (2nd step) of PO, Phillips-Perron unit root test is used with either Short lag (use PO.Lag = "Short") or Long lag (use PO.Lag = "Long") version.

## 3. Second type of cointegration pairwise test is JO. Since it is widely mentioned in the economics literature, unit root tests have low power and some size distortion problems, JO cointegration test is used as a second option. JO test is symmetric, direction does not matter; and thus, only one JO test is conducted for each time series pairs. JO test is preferred more then PO test in the economics literature, since JO test is symmetric, can reveal number of cointegration relationhips in multivariate case, and can also identify if the variable system (bivariate in our case) is stationary or not. As default, Johansen Trace test is used since Lütkepohl (2000) stated that Trace tests has superior power in small sample sizes; however, one can use Maximum Eigenvalue test by changing Johansen.Type argument of to Eigen.

## 4. Finally, in all tests only constant is added to cointegration equation but trend can be added also by changing Model.Type argument to "Trend". Cointegration equation will be used without constant and trend, if "None" is selected.

#==================================== END ======================================
