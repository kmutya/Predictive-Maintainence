#Modelling
library(nnfor) #to model MLP
library(forecast) #to forecast MLP
library(astsa) #ARIMA 
library(dplyr) #data cleaning
library(tseries) #kpss test 
library(ggplot2)

#reading in the final csv file
getwd()
setwd("/Users/apple/Google Drive/A&M/Summer 2018/Project/626")
final = read.csv('final_data.csv')
#Convert df final to list
final_list = split(final, f = final$Engine)

#creating a list
final_list2 = final_list
str(final_list2)
final_list2 = lapply(final_list2, function(x) x[(names(x) %in% "HI")])
str(final_list2)

#Plotting the engines we will forecast
#reading in the test file and column binding RUL with final df
test = read.csv("test.csv")
temp = cbind(final, test$Unit)
colnames(temp)[3] = 'RUL'

#creating a function to map engine to the ts.plot function
plot_hi = function(j){
  temp_1 = temp[temp$Engine == j,]
  case1 = temp_1$HI
  ts.plot(case1, col = 'lightpink3', xlab = 'Cycle', ylab = 'Health Index')  
}

engines = c(56,36,61,84,3,16,44,65,83)
par(mfrow = c(3,3))
hilist = list()
for (j in engines){
  hilist[[j]] = plot_hi(j)
}

#Creating a list with the engines we will forecast
forecast_list = final_list2[c(56,36,61,84,3,16,44,65,83)]
forecast_list2 = lapply(forecast_list, ts)

#Performing kpss test to test stationarity 
stationary_result = lapply(forecast_list2, kpss.test, null = "Level") #Level means that the data is like white noise

#Applying required function to our list
#Our custom function uses the auto.arima() function from forecast package
#Forecasts till 10000 iterations or when our RUL reaches 0, whichever is earlier
RUL_list_arima = lapply(forecast_list, function(x){
  out = auto.arima(x, d = 1, max.p = 15, max.q = 15)
  p = out$arma[1]
  q = out$arma[2]
  for(i in 1:10000){
    out_for = sarima.for(x, i, p,1,q)
    if (out_for$pred[i] <= 0.0){
      RUL = i
      break
    }
  }
  return(RUL)
})

#Fitting a MLP to the 9 engines
RUL_list_mlp = lapply(forecast_list2, function(x){
  mlp_fit = mlp(x, difforder = 1) #passing the trained model as an argument
  for(i in 1:1000){
    out_for = forecast(mlp_fit, h = i)
    if (out_for$mean[i] <= 0.01){
      RUL = i
      break
    }
  }
  return(RUL)
})

#Results
#Creating a dataframe with Engine numbers and forecasted values
actual_RUL = read.csv('actual_RUL.csv')
original_RUL = actual_RUL %>% filter(Engine %in% c(56,36,61,84,3,16,44,65,83))
original_RUL = arrange(original_RUL, RUL)
forecasts_df = data.frame(cbind(matrix(unlist(RUL_list_arima)),matrix(unlist(RUL_list_mlp))))
colnames(forecasts_df) = c('ARIMA_Forecast', 'MLP_forecast')
forecasts_df = cbind(original_RUL, forecasts_df)
forecasts_df$ARIMA_residual = forecasts_df$ARIMA_Forecast - forecasts_df$RUL
forecasts_df$MLP_residual = forecasts_df$MLP_forecast - forecasts_df$RUL
write.csv(forecasts_df, 'forecast_small.csv', row.names = F)

#Visualizing our predictions
#creating a df to organize all residuals by model
forecast_plot = data.frame(model = c(rep('ARIMA', 9), rep('MLP', 9)))
forecast_plot$value = c(forecasts_df[,"ARIMA_Forecast"], forecasts_df[,"MLP_forecast"])
forecast_plot$residual = c(forecasts_df[,"ARIMA_residual"], forecasts_df[,"MLP_residual"])
forecast_plot$Engine_no = c(forecasts_df[,"Engine"], forecasts_df[,"Engine"])
forecast_plot$RUL = c(forecasts_df[,"RUL"], forecasts_df[,"RUL"])

#plotting
ggplot(data = forecast_plot, aes(x = value, y = residual, colour = model)) + geom_point(shape = forecast_plot$model) + 
ylab('residual')+xlab('fitted value') + geom_hline(yintercept=0, linetype="dashed", color = "red") + 
ggtitle('Visualization of Residuals') + scale_colour_discrete(name = 'Legend', labels = c('ARIMA Residuals','MLP Residuals')) + 
geom_text(aes(label=Engine_no) ,hjust=0, vjust=2) + scale_size_continuous(range = c(1, 9))


#Cost function
#x = actual #y = predicted #z= cost
cf = function(x,y){
  d = y-x
  if(d<0){
    s = exp(-(d/13))-1
  }
  else{
    s = exp(d/10)-1
  }
  return(s)
}



