#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 14:37:05 2019

@author: Kartik
"""

import os
import pandas as pd
import math 
import numpy as np
import matplotlib.pyplot as plt

os.getcwd()
os.chdir('/Users/apple/Google Drive/A&M/Summer 2018/Project/626')

#import the data
forecast_df = pd.read_csv('forecast_small.csv')

#Plotting the cost function  
x = np.arange(-50, 50, 1)
y = []
for i in x:
    if i<0:
        y.append(np.exp(-(i / 13)) - 1)
    else:
        y.append(np.exp(i/10) - 1)
        
plt.plot(x, y)
plt.ylabel('Score')
plt.xlabel('error')
plt.title('Plot of the Cost as a function of error')
plt.show()


#Create a function to calculate the cost function  
def cost_function(aplist):
    d = aplist[1] - aplist[0] #Estimated - True
    if d<0:
        s = math.exp(-(d/13)) - 1
    else:
        s = math.exp(d/10)-1
    return(s)
    

#First, for the ARIMA case
arima_cost = forecast_df[['RUL', 'ARIMA_Forecast']]
arima_cost = arima_cost.values
arima_cost = arima_cost.tolist()

arima_value = []
for i in arima_cost:
    arima_value.append(cost_function(i))

    
#Second, for the MLP case  
mlp_cost = forecast_df[['RUL', 'MLP_forecast']]
mlp_cost = mlp_cost.values
mlp_cost = mlp_cost.tolist()

mlp_value = []
for i in mlp_cost:
    mlp_value.append(cost_function(i))
    
#create a final scores df
score_df = pd.DataFrame({'ARIMA_score': arima_value,
              'MLP_score': mlp_value})
final_score_df = pd.concat([forecast_df[['Engine', 'RUL']], score_df], axis=1)
final_score_df.to_csv('final_score_df.csv', index = False)

    
    
