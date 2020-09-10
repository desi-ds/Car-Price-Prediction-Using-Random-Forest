library(caret)
df <- read.csv("Vehicle_Retail_Price_Assignment.csv")

#exploring Data
dim(df) #number of rows and columns in data
head(df) #first 6 rows of data
str(df) #structure of data
summary(df) #statistical summary of data

#checking for NAs
colSums(is.na(df)) #there is no NA's in data

#convert character variables in factor
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)
df$symboling <- as.factor(df$symboling)
str(df)

#EDA
library(ggplot2)
library(gridExtra)

#effect of car dimensions on price
d1 <- ggplot(df, aes(x = wheelbase, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

d2 <- ggplot(df, aes(x = carlength, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

d3 <- ggplot(df, aes(x = carwidth,  y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

d4 <- ggplot(df, aes(x = carheight, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

d5 <- ggplot(df, aes(x = curbweight, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()


grid.arrange(d1, d2, d3, d4, d5, top = "Variation of Price with Car Dimensions")
 
#effect of car configuration
library(dplyr)
c1 <- df %>% group_by(doornumber) %>% summarise(mean_price = mean(price)) %>% {
      ggplot(., aes(x = doornumber, y = mean_price, fill = doornumber)) + 
      geom_bar(stat = "identity") +
      theme_minimal() + theme(legend.position = "none")
}

c2 <- df %>% group_by(carbody) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = carbody, y = mean_price, fill = carbody)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

c3 <- df %>% group_by(drivewheel) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = drivewheel, y = mean_price, fill = drivewheel)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

c4 <- df %>% group_by(enginelocation) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = enginelocation, y = mean_price, fill = enginelocation)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

grid.arrange(c1, c2, c3, c4, top = "Mean Price for Different Configuations")

#mean price based on engine specification
e1 <- df %>% group_by(fueltype) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = fueltype, y = mean_price, fill = fueltype)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

e2 <- df %>% group_by(aspiration) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = aspiration, y = mean_price, fill = aspiration)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

e3 <- df %>% group_by(enginetype) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = enginetype, y = mean_price, fill = enginetype)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

e4 <- df %>% group_by(cylindernumber) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = cylindernumber, y = mean_price, fill = cylindernumber)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}


e5 <- df %>% group_by(fuelsystem) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = fuelsystem, y = mean_price, fill = fuelsystem)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

grid.arrange(e1, e2, e3, e4, e5, 
             top = "Mean Price with Engine Specifications")

#engine technical specifications
et1 <- ggplot(df, aes(x = boreratio, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

et2 <- ggplot(df, aes(x = stroke, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

et3 <- ggplot(df, aes(x = compressionratio, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

et4 <- ggplot(df, aes(x = horsepower, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

et5 <- ggplot(df, aes(x = peakrpm, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

grid.arrange(et1, et2, et3, et4, et5, top = "Price with Technical Engine Specification")

#mpg with price
mpg1 <- ggplot(df, aes(x = citympg, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

mpg2 <- ggplot(df, aes(x = highwaympg, y = price)) + 
  geom_point() + 
  stat_smooth(method="lm", se=F) + theme_minimal()

grid.arrange(mpg1, mpg2, top = "Price with MPG")

#symboling
df %>% group_by(symboling) %>% summarise(mean_price = mean(price)) %>% {
  ggplot(., aes(x = symboling, y = mean_price, fill = symboling)) + 
    geom_bar(stat = "identity") +
    theme_minimal() + theme(legend.position = "none")
}

#boxplots
box1 <- df %>% group_by(symboling)  %>% {
        ggplot(., aes(x = symboling, y = price, fill = symboling)) + 
        geom_boxplot() +
        theme_minimal() + theme(legend.position = "none")
}


box2 <- df %>% group_by(fueltype)  %>% {
    ggplot(., aes(x = fueltype, y = price, fill = fueltype)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box3 <- df %>% group_by(aspiration)  %>% {
  ggplot(., aes(x = aspiration, y = price, fill = aspiration)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box4 <- df %>% group_by(doornumber)  %>% {
  ggplot(., aes(x = doornumber, y = price, fill = doornumber)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box5 <- df %>% group_by(carbody)  %>% {
  ggplot(., aes(x = carbody, y = price, fill = carbody)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box6 <- df %>% group_by(drivewheel)  %>% {
  ggplot(., aes(x = drivewheel, y = price, fill = drivewheel)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box7 <- df %>% group_by(enginelocation)  %>% {
  ggplot(., aes(x = enginelocation, y = price, fill = enginelocation)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box8 <- df %>% group_by(enginetype)  %>% {
  ggplot(., aes(x = enginetype, y = price, fill = enginetype)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box9 <- df %>% group_by(cylindernumber)  %>% {
  ggplot(., aes(x = cylindernumber, y = price, fill = cylindernumber)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

box10 <- df %>% group_by(fuelsystem)  %>% {
  ggplot(., aes(x = fuelsystem, y = price, fill = fuelsystem)) + 
    geom_boxplot() +
    theme_minimal() + theme(legend.position = "none")
}

grid.arrange(box4, box5, box6, box7, top = "Variation of Price for Different Configurations")

grid.arrange(box2, box3, box8, box9, box10, top = "Variation of Price for Different Engine Specifications")

#correlation Analysis
numericData <- df[,sapply(df, is.numeric)] #filter all numeric vars
numericData <- numericData[, -c(1, 15)] #drop the id column and dependent var

library(corrplot)
corMat <- cor(numericData) #correlation matrix
corrplot(corMat, method = "number", type = "lower") #plot of corr matrix
highlyCorrelated <- findCorrelation(corMat, cutoff = 0.7) #find highly correlated
highlyCorCol <- colnames(numericData)[highlyCorrelated]
highlyCorCol



set.seed(123)
df2 <- df[, -c(1, 3)]
sample <- sample.int(n = nrow(df2), 
                     size = floor(.75*nrow(df2)), 
                     replace = F)


train <- df2[sample, ]
test  <- df2[-sample, ]

x_train <- train[, -24]
y_train <- train[, 24]

x_test <- test[, -24]
y_test <- test[, 24]

#Random Forest
library(randomForest)
# for reproduciblity
set.seed(123)

# default RF model
set.seed(125)
model1 <- randomForest(price ~ ., 
                       data = train,
                       importance = T,
                       ntree = 500, 
                       maxnodes = 20
                       )
model1
plot(model1)









