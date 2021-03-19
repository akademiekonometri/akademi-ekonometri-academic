
################################################################################
# Degistir.
file.path <- "./CPI.csv"
################################################################################
# Loading with csv file.
data <- read.csv(file.path, header = TRUE, sep = ",", dec = ".", colClasses = "character", comment.char = "", na.string = "")

test <- data[, -c(1, 5)]
colnames(test) <- c("Yil", "Ay", "TUFE")
test$Ay <- gsub("(M0)|(M)", "", test$Ay)
test <- test[grep("(13)|(S01)|(S02)|(S03)", test$Ay, invert = TRUE), ]
unique(test$Ay)

test$Yil <- as.numeric(test$Yil)
test$Ay <- as.numeric(test$Ay)
test$TUFE <- as.numeric(test$TUFE)

str(test)


