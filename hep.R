setwd("C:\\R_code\\hepatitis")
library(mice)


hep_data = read.csv("hepatitis.data", header = F,na.strings = c("?"))

colnames(hep_data)=c("Class","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE","ANOREXIA",
"LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS","ASCITES","VARICES", "BILIRUBIN",
"ALK PHOSPHATE","SGOT", "ALBUMIN","PROTIME","HISTOLOGY")


md.pattern(hep_data)

library(VIM)
aggr_plot <- aggr(hep_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(hep_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


indx <- which.max(apply(hep_data, 1, function(x) length(x[is.na(x)])))

# indx = 56,41
hep_data= hep_data[-57,]
hep_data= hep_data[-42,]

tempData <- mice(hep_data,m=5,maxit=50,seed=500)
summary(tempData)
completedData <- complete(tempData,1)

completedData[,1]= as.factor(completedData[,1])
train_index = sample(1:nrow(completedData),108)
train_data=completedData[train_index,]

test_data = completedData[-train_index,]
predictions = data.frame(matrix(NA, nrow = 45, ncol = 1))

fit <- lda(Class~., data=train_data)
# summarize the fit
summary(fit)
# make predictions
predictions$L <- (predict(fit, test_data[,-1]))$class
# summarize accuracy
table(predictions$L, test_data[,1])



# load the package
library(VGAM)
# load data
# fit modelClass~., data=train_data)
fit <- vglm(Class~., family=multinomial, data=train_data)
# summarize the fit
summary(fit)
# make predictions
probabilities <- predict(fit,  test_data[,-1], type="response")
predictions$LR <- apply(probabilities, 1, which.max)
# summarize accuracy
table(predictions$LR, test_data[,1])




# fit model
fit <- plsda(train_data[,-1], as.factor(train_data[,1]), probMethod="Bayes")
# summarize the fit
summary(fit)
# make predictions
predictions$PLSDA <- predict(fit,  test_data[,-1])
# summarize accuracy
table(predictions$PLSDA, test_data[,1])
