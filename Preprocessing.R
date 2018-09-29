###########PREPROCESSING############
#loading required libraries
library(ggplot2) #plotting
library(dplyr) #data cleaning
library(gridExtra) #grid mapping
library(GGally) #correlation plot


#Reading the training data
getwd()
setwd("/Users/apple/Documents/github/PredictiveMaintainence")
data = read.csv('data.csv')
data$Engine = as.factor(data$Engine)
head(data)

#Plotting all 21 sensors
sensors = colnames(data[,6:26])
slist = list()
for(i in sensors){
  slist[[i]] = ggplotGrob(ggplot(data = data, aes_string(y = i)) + geom_point(aes(x = RUL, y = data[i]), color = "lightpink3"))
}
grid.arrange(slist[[1]],slist[[2]],slist[[3]],slist[[4]],slist[[5]],slist[[6]],slist[[7]],slist[[8]],slist[[9]],slist[[10]],slist[[11]],slist[[12]],slist[[13]],slist[[14]],slist[[15]],slist[[15]],slist[[16]],slist[[17]],slist[[18]],slist[[19]],slist[[20]],slist[[21]], ncol=3)

#Plotting 3 operational settings
ops = colnames(data[,3:5])
oplist = list()
for(j in ops){
  oplist[[j]] = ggplotGrob(ggplot(data = data, aes_string(y = j)) + geom_point(aes(x = RUL, y = data[j]), color = "lightpink3"))
}
grid.arrange(oplist[[1]],oplist[[2]],oplist[[3]], ncol=3)

#RUL summary
RUL_engine = aggregate(RUL~Engine, data, max)
summary(RUL_engine$RUL)

data$row = row.names(data)
data$HI = NA
row_engine_min = aggregate(row~Engine, data, min)
row_engine_min$row = as.numeric(row_engine_min$row)

#Assigning first 30 cycles a value of 1 i.e good health
for(k in row_engine_min$row){
  l = k+29
  if (row.names(data[k,])==k){
    data$HI[k:l] = 1
  }
}

row_engine_max = aggregate(row~Engine, data, max)
row_engine_max$row = as.numeric(row_engine_max$row)
row_engine_max$row[1] = 192
#Assigning last 30 cycles a value of 0 i.e bad health
for(n in row_engine_max$row){
  m = n-29
  if (row.names(data[m,])==m){
    data$HI[m:n] = 0
  }
}

data_final = na.omit(data) #6000x12
table(data$HI)

data_final = subset(data_final, select = c("Sensor2","Sensor3","Sensor4",
                                           "Sensor7","Sensor8","Sensor11",
                                           "Sensor12","Sensor13","Sensor15",
                                           "Sensor20","Sensor21","HI"))
head(data_final)
tail(data_final)

#Correlation plot
round(cor(data_final),2)
ggcorr(data_final,
       label = T,
       label_alpha = F)

#Using regression to obtain a health index#
regression = lm(HI~., data = data_final)

####TEST####
#Reading the test data
test = read.csv("test.csv")
test_s = subset(test, select = c("S2","S3","S4",
                                 "S7","S8","S11"
                                 ,"S12","S13","S15",
                                 "S20","S21"))
colnames(test_s) = c("Sensor2","Sensor3","Sensor4",
                     "Sensor7","Sensor8","Sensor11",
                     "Sensor12","Sensor13","Sensor15",
                     "Sensor20","Sensor21")
test_model = predict.lm(regression, test_s)
test_s$HI = test_model #Found HI
test = cbind(test, test_s$HI) #Cbind HI with original test

#creating a function to map engine to the ts.plot function
plot_hi = function(j){
  test_1 = test[test$Engine == j,]
  case1 = test_1$`test_s$HI`
  ts.plot(case1, col = 'lightpink3', xlab = 'Cycle', ylab = 'Health Index')
}

engines = 1:100

hilist = list()
for (j in engines){
  hilist[[j]] = plot_hi(j)
}

#subsetting HI and Engine into a df called final
final = subset(test, select = c('Engine', 'test_s$HI')) #13096 x 2
head(final)
#final - univariate time series with engine numbers
