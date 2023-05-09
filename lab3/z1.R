install.packages("neuralnet")
library(neuralnet)

normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

Input <-  as.data.frame(runif(100, min=1, max=100))
#f(x) = x^3 - 2x
Output <- Input^3 - 2*Input

data <- cbind(Input,Output)
colnames(data) <- c("Input","Output")
# dane musza zostac znormalizowane
normalized <- as.data.frame(lapply(data, normalize))

#podzial danych
traindata <- normalized[1:85,]
testdata <- normalized[86:100,]

#neuralnet -trenowanie
nn_output <- neuralnet(Output ~ Input, data=traindata, hidden=10, threshold=0.01, linear.output=TRUE)
print(nn_output)
plot(nn_output)

results <- compute(nn_output, testdata)
ls(results)
comparison <- data.frame(actual=testdata[,2], prediction=results$net.result)
comparison