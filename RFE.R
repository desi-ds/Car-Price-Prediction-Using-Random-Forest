
library(caret)
x <- df[, -c(1, 3, 26)]
x$symboling <- as.numeric(x$symboling)
x$fueltype  <- as.numeric(x$fueltype)
x$aspiration<- as.numeric(x$aspiration)
x$doornumber<- as.numeric(x$doornumber)
x$carbody   <- as.numeric(x$carbody)
x$drivewheel<- as.numeric(x$drivewheel)
x$enginelocation<- as.numeric(x$enginelocation)
x$enginetype<- as.numeric(x$enginetype)
x$cylindernumber<- as.numeric(x$cylindernumber)
x$fuelsystem<- as.numeric(x$fuelsystem)
y <- df[, 26]

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

set.seed(5)
lmProfile2 <- rfe(x, y,
                  sizes = c(10:15, 20, 23),
                  rfeControl = rfeControl(functions = lmFuncs,
                                          rerank = TRUE,
                                          number = 200))

lmpImp <- data.frame(varImp(lmProfile))
lmpImp <- data.frame(variable = rownames(lmpImp), lmpImp)

ggplot(data=lmpImp, aes(x=reorder(variable, Overall), y=Overall)) +
  geom_bar(stat="identity") + coord_flip()


rownames(lmpImp) <- NULL
predictors(lmProfile2)
ggplot(lmProfile2)
ggplot(lmProfile2, metric = "Rsquared")





