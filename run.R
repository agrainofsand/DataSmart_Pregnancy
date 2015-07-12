library(randomForest)
library(ROCR)

# Loading the data
PregnancyData <- read.csv("data/Pregnancy.csv")
PregnancyData.Test <- read.csv("data/Pregnancy_Test.csv")

# Transforming the data
PregnancyData$PREGNANT <- as.factor(PregnancyData$PREGNANT)
PregnancyData.Test$PREGNANT <- as.factor(PregnancyData.Test$PREGNANT)

# Running the lm model
Pregnancy.lm <- glm(PREGNANT ~ ., data=PregnancyData, family=binomial("logit"))
summary(Pregnancy.lm)

Pregnancy.rf <- randomForest(PREGNANT ~ ., data=PregnancyData, importance=TRUE)
varImpPlot(Pregnancy.rf, type=2)

# Predicting the model
PregnancyData.Test.lm.Preds <- predict(Pregnancy.lm, PregnancyData.Test, type="response")
summary(PregnancyData.Test.lm.Preds)

PregnancyData.Test.rf.Preds <- predict(Pregnancy.rf, PregnancyData.Test, type="prob")
summary(PregnancyData.Test.rf.Preds)

t(PregnancyData.Test[1,])
t(PregnancyData.Test.lm.Preds[1])
PregnancyData.Test.rf.Preds[1,2]

pred.lm <- prediction(PregnancyData.Test.lm.Preds, PregnancyData.Test$PREGNANT)
perf.lm <- performance(pred.lm, "tpr", "fpr")

pred.rf <- prediction(PregnancyData.Test.rf.Preds[,2], PregnancyData.Test$PREGNANT)
perf.rf <- performance(pred.rf, "tpr", "fpr")

plot(perf.lm, xlim=c(0,1), ylim=c(0,1))
plot(perf.rf, xlim=c(0,1), ylim=c(0,1), lty=2, add=TRUE)