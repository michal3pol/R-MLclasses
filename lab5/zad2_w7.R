install.packages("randomForest")
install.packages("e1071")
install.packages("party")
install.packages("mlr")
install.packages("rFerns")
library(randomForest)
library(party)
library(mlr)
library(rFerns)

nazwa <- c("Tablet Apple iPad (10 gen.) 2022 (srebrny)", 
           "Tablet Apple iPad 2021", 
           "Tablet Apple iPad (10 gen.) 2022 (rozowy)", 
           "Tablet Apple iPad Wi-Fi (gwiezdna szarosc)", 
           "Tablet Apple iPad Air 2022 Wi-Fi (gwiezdna szarosc)", 
           "Tablet Apple iPad Air 2022 Wi-Fi (ksiezycowa poswiata)", 
           "Tablet Apple iPad 2021 Wi-Fi 256GB (srebrny)", 
           "Tablet Apple iPad Pro 2021 (srebrny)", 
           "Tablet Apple iPad Pro (gwiezdna szarosc)", 
           "iPad10")
wyswietlacz <- c(10.9, 10.2, 10.2, 10.2, 10.9, 10.9, 10.2, 10.2, 11, 9.7)
pamiec_RAM <- c(1, 4, 64, 8, 2, 32, 1, 64, 8, 2)
pamiec_wbudowana <- c(64, 128, 64, 256, 64, 32, 16, 256, 256, 128)
cena <- c(2719, 2399, 3649, 2749, 3799, 2999, 2749, 12599, 7399, 5299)
ocena <- c(6,3.5,6,4,2.5,5,2,4,4.5,5)
ipady_rtv <- data.frame(nazwa, wyswietlacz, pamiec_RAM, pamiec_wbudowana, cena, ocena)
ipady_rtv$nazwa = factor(ipady_rtv$nazwa)
ipady_rtv$ocena = factor(ipady_rtv$ocena)
summarizeColumns(ipady_rtv)

rdesc = makeResampleDesc("CV", iters = 10)

task = makeClassifTask(id = deparse(substitute(ipady_rtv)), ipady_rtv, "ocena",
                       weights = NULL, blocking = NULL, coordinates = NULL,
                       positive = NA_character_, fixup.data = "warn", check.data = TRUE)
lrns <- makeLearners(c("rpart", "C50", "ctree", "naiveBayes", "randomForest"), type = "classif")

bmr <- benchmark(learners = lrns, tasks = task, rdesc, models = TRUE, measures = list(acc, ber))
p = getBMRPredictions(bmr)
plotBMRSummary(bmr)

ipady_task = makeClassifTask(data = ipady_rtv, target="ocena")
ipady_learner <- makeLearner("classif.C50")
ipady_model <- train(ipady_learner, ipady_task)

