install.packages("XLConnect")
library("XLConnect")

################################################################################
# Degistir.
file.path <- "./test.xls"
################################################################################
# Loading with excel file 1.
workbook <- loadWorkbook(filename = file.path, create = FALSE) ## Loading the gas_price.xls file.
data <- readWorksheet(object = workbook, sheet = 1, startRow = 4, startCol = 3, header = TRUE, colTypes = "character")

# Loading with excel file 2.
data <- readWorksheetFromFile(file = file.path, sheet = 1, startRow = 4, startCol = 3, header = TRUE, colTypes = "character")

################################################################################
# Degisken secimi icin gerekli. Istedigin degiskeni "data" adli data set icinden rakamla sec.

test <- data[row.names(data) %in% c(5, 11, 12), ]
test <- test[, -1]
test <- t(test)

test <- t(data[row.names(data) %in% c(5, 11, 12), -c(1)])
################################################################################

test <- cbind(rownames(test), test)
colnames(test) <- test[1, ]
test <- test[-1, ]
rownames(test) <- 1:nrow(test)
test[, 1] <- gsub("X", "", test[, 1])
test <- as.data.frame(test, stringsAsFactors = FALSE)

################################################################################
# Sutun isimlerini sectigin dataya gore degistir.=
##
colnames(test) <- c("Yıl", "Nufus.Artisi", "Goc", "Issizlik")
################################################################################

str(test)

test$Yıl <- as.numeric(test$Yıl)
test$Nufus.Artisi <- as.numeric(test$Nufus.Artisi)
test$Goc <- as.numeric(test$Goc)
test$Issizlik <- as.numeric(test$Issizlik)

str(test)
