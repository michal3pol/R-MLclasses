setwd("D:/APU/R-MLclasses/lab3")

normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

pamiec_RAM <- c(1, 1, 1, 1, 2, 1, 2, 16, 8, 2)
pamiec_wbudowana <- c(64, 64, 64, 256, 64, 64, 16, 256, 256, 128)
cena <- c(2719, 2399, 3649, 2749, 3799, 2999, 2749, 12599, 7399, 5299)
tablety <- data.frame(pamiec_RAM, pamiec_wbudowana, cena)

tablety_N <- as.data.frame(lapply(tablety, normalize))

trainData <- tablety_N
testData <- tablety_N[1:3, ]

nn_result <- neuralnet(cena ~ pamiec_RAM + pamiec_wbudowana, data=trainData,
                          hidden=c(3,2), threshold=0.01)

print(nn_result)
plot(nn_result)

results <- compute(nn_result, testData)

ls(results)
comparison <- data.frame(actual=testData[,c('cena')], prediction=results$net.result)
comparison

