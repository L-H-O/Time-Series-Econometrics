# -*- coding: utf-8 -*-
"""
Created on Fri Mar 29 11:51:58 2024

@author: lider
"""
## setup

import pandas as pd
import numpy as np
import os

from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.stattools import adfuller, acf, pacf
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf

login = os.getlogin()
home = os.path.join(r"C:\Users\{}".format(login), "Downloads")
os.chdir(home)
#%% import data

def load_data():

    raw_inf = pd.read_csv("infant-mortality.csv").query("Entity == 'Brazil'")                                             
    raw_dem = pd.read_csv("V-Dem-CY-Full+Others-v14.csv", low_memory = False).query("country_name == 'Brazil' and 1934 <= year <= 2021")
    raw_pop = pd.read_csv("population.csv", low_memory = False).query("ISO3_code == 'BRA' and Variant == 'Medium'")
        
    raw_pwt = pd.read_excel("pwt1001.xlsx", sheet_name = 'Data').query("country == 'Brazil'")
    raw_urb = pd.read_csv("urb-growth.csv", header = 2).query("`Country Name` == 'Brazil'")
    raw_fer = pd.read_csv("fertility-rate.csv", header = 2).query("`Country Name` == 'Brazil'")    
    
    raw_gdppc = pd.read_excel("gdp_pc.xls", sheet_name = 'Data', header = 3).query("`Country Name` == 'Brazil'")
    
    return raw_inf, raw_dem, raw_pop, raw_pwt, raw_urb, raw_fer, raw_gdppc

raw_inf, raw_dem, raw_pop, raw_pwt, raw_urb, raw_fer, raw_gdppc = load_data()

#%% wrangling

def wrangle_data():

    df_inf = (raw_inf.drop(columns = ["Code", "Entity"])
                     .assign(Year = lambda df_ : pd.to_datetime(df_["Year"],
                                                                  format = "%Y"))
                     .sort_values(by = 'Year')
             )
    
    df_dem = raw_dem.copy()
    
    df_pop = (raw_pop[["Time", "PopTotal"]]
                    .assign(Year = lambda df_ : pd.to_datetime(df_["Time"],
                                                               format = "%Y"))
                    .drop(columns = 'Time')
                    .sort_values(by = 'Year')
                    )
    
    df_pwt = (raw_pwt[['year', 'hc']]
                     .assign(Year = lambda df_ : pd.to_datetime(df_["year"],
                                                                format = "%Y"))
                     .drop(columns = 'year')
                     .sort_values(by = 'Year')
                     )
    
    df_urb = (raw_urb.T
                     .reset_index()
                     .iloc[4:]
                     .dropna()
                     .assign(Year = lambda df_ : pd.to_datetime(df_["index"],
                                                                format = "%Y")
                     )
                     .drop(columns = 'index')
                     .rename(columns = {29 :'urb_growth'})
                     )
    
    df_fer = (raw_fer.T
                     .reset_index()
                     .iloc[4:]
                     .dropna()
                     .assign(Year = lambda df_ : pd.to_datetime(df_["index"],
                                                                format = "%Y")
                     )
                     .drop(columns = 'index')
                     .rename(columns = {29 :'fer_rate'})
                     )
    
    df_gdppc = (raw_gdppc.T
                             .reset_index()
                             .iloc[4:]
                             .dropna()
                             .assign(Year = lambda df_ : pd.to_datetime(df_["index"],
                                                                        format = "%Y")
                             )
                             .drop(columns = 'index')
                             .rename(columns = {29 :'gpd_pc'})
                             )
    
    
    data =  (df_dem
                    .assign(Year = lambda df_ : pd.to_datetime(df_["historical_date"]))
                    [["Year", "v2caviol", "v2x_accountability", "v2x_accountability_osp",
                      "v2x_rule", "v2x_corr", "v2xcs_ccsi" ]]
                    .set_index('Year')
                    .resample('YS')
                    .first()
                    .reset_index()
                    .merge(df_inf,
                           on = 'Year')
                    .merge(df_pwt,
                           on = 'Year')
                    .merge(df_urb,
                          on = 'Year')
                    .merge(df_gdppc,
                           on = 'Year')
                    .merge(df_fer,
                           on = 'Year')
                    .merge(df_pop,
                           on = 'Year')
                    .set_index('Year')
                    .set_axis(['pol_violence', 'accountability',
                               'accountability_scaled', "rule_of_law"
                               ,"corruption", 'civil_society', 'imr',
                               'human_capital', "urb_growth", "gdp_pc",
                               "fer_rate", "pop"]
                              , axis = 1)
                    )
    
    return data
#%% Load data          

data = wrangle_data()

data = (data.assign(pop = np.log(data["pop"]),
                    fer_rate = np.log(data["fer_rate"].astype(float)),
                    urb_growth = np.log(data["urb_growth"].astype(float)),
                    )
                )

#%% utils

def adf_test(timeseries):
    print("Results of Dickey-Fuller Test:")
    dftest = adfuller(timeseries, autolag="AIC")
    dfoutput = pd.Series(
        dftest[0:4],
        index=[
            "Test Statistic",
            "p-value",
            "#Lags Used",
            "Number of Observations Used",
        ],
    )
    for key, value in dftest[4].items():
        dfoutput["Critical Value (%s)" % key] = value
    print(dfoutput)

#%%

data["human_capital"].plot()
data["accountability"].plot()

#%% GDP_pc

data["gdp_pc"].plot(color = 'darkred', xlabel = 'Ano', ylabel = 'PIB per capita (U$)')

seasonal_decompose(data["gdp_pc"]).plot(color = 'red')

(round(data["gdp_pc"].mean(),2), round(data["gdp_pc"].var(),2))

data["gdp_pc"].diff(1).plot(color = 'darkred', xlabel = 'Ano', ylabel = 'PIB per capita (U$) - Primeira Diferença')

adf_test(data["gdp_pc"].diff(1).dropna())
adf_test(np.log(data["gdp_pc"].astype(float)))
adf_test((data["gdp_pc"].pct_change().dropna()))

plot_acf(data["gdp_pc"])
plot_pacf(data["gdp_pc"])

plot_acf(data["gdp_pc"].diff(1).dropna())
plot_pacf(data["gdp_pc"].diff(1).dropna())

#%% Human Capital

data["human_capital"].plot(color = 'green', xlabel = 'Ano', ylabel = 'Índice de Capital Humano')

seasonal_decompose(data["human_capital"]).plot()

(round(data["human_capital"].mean(),2), round(data["human_capital"].var(),2))

data["human_capital"].pct_change().diff(1).dropna().plot(color = 'green', xlabel = 'Ano', ylabel = '% Índice de Capital Humano - Primeira Diferença')

adf_test(data["human_capital"].pct_change().diff().dropna())
adf_test(np.log(data["human_capital"].astype(float)))
adf_test(data["human_capital"].pct_change().dropna())

plot_acf(data["human_capital"])
plot_pacf(data["human_capital"])

plot_acf(data["human_capital"].pct_change().diff(1).dropna())
plot_pacf(data["human_capital"].pct_change().diff(1).dropna())

#%% Accountability

data["accountability_scaled"].plot(color = 'darkblue', xlabel = 'Ano', ylabel = 'Accountability')

seasonal_decompose(data["accountability_scaled"]).plot()

(round(data["accountability_scaled"].mean(),2), round(data["accountability_scaled"].var(),2))

data["accountability_scaled"].diff(1).dropna().plot(color = 'green', xlabel = 'Ano', ylabel = 'Accountability - Primeira Diferença')
data["accountability_scaled"].pct_change().dropna().plot(color = 'green', xlabel = 'Ano', ylabel = '% Accountability')

adf_test(data["accountability_scaled"].diff().dropna())
adf_test(np.log(data["accountability_scaled"].astype(float)))
adf_test(data["accountability_scaled"].pct_change().dropna())

plot_acf(data["accountability_scaled"])
plot_pacf(data["accountability_scaled"])

plot_acf(data["accountability_scaled"].pct_change().dropna())
plot_pacf(data["accountability_scaled"].pct_change().dropna())

#%% ARMA Estimation

data.loc[:"2018","imr"].plot(color = 'darkblue', xlabel = 'Ano', ylabel = 'TMI')

plot_acf(data["imr"])
plot_pacf(data["imr"])

data.loc[:"2018", "imr"].pipe(adf_test)

log_imr = data.loc[:"2018", "imr"].pipe(np.log)
log_imr.pipe(adf_test)

plot_acf(log_imr)
plot_pacf(log_imr)

log_imr.plot(color = 'darkblue', xlabel = 'Ano', ylabel = 'Log(IMR', title = 'Infant Mortality Rate')

diff_imr = data.loc[:"2018", "imr"].diff().diff().dropna()
diff_imr.pipe(adf_test)

plot_acf(diff_imr)
plot_pacf(diff_imr)

model_1 = ARIMA(log_imr, order = (1,0,1)).fit()
model_2 = ARIMA(log_imr, order = (1,0,2)).fit()
model_3 = ARIMA(log_imr, order = (2,0,1)).fit()
model_4 = ARIMA(log_imr, order = (2,0,2)).fit()
    
model_1_diff = ARIMA(diff_imr, order = (1,0,1)).fit()
model_2_diff = ARIMA(diff_imr, order = (1,0,2)).fit()
model_3_diff = ARIMA(diff_imr, order = (2,0,1)).fit()
model_4_diff = ARIMA(diff_imr, order = (2,0,2)).fit()

def get_info_criteria(model): # build shitty table for IC's
    
  return(   {
            'aic' : model.info_criteria('aic'),
            'bic' : model.info_criteria('bic'),
            'hqic' : model.info_criteria('hqic')
            })

models_log = (pd.concat(
                    [pd.DataFrame.from_dict(get_info_criteria(model), orient = 'index') 
                     for model in [model_1, model_2, model_3, model_4]
                     ],
                    axis = 1)
                 .set_axis(["ARIMA (1,0,1)", "ARIMA (1,0,2)", "ARIMA (2,0,1)", "ARIMA (2,0,2)"],
                    axis = 1)
          )

models_diff = (pd.concat(
                    [pd.DataFrame.from_dict(get_info_criteria(model), orient = 'index') 
                     for model in [model_1_diff, model_2_diff, model_3_diff, model_4_diff]
                     ],
                    axis = 1)
                 .set_axis(["ARIMA (1,0,1)", "ARIMA (1,0,2)", "ARIMA (2,0,1)", "ARIMA (2,0,2)"],
                    axis = 1)
          )

models_diff

# Sending code to R because statsmodels is sus

with pd.ExcelWriter('Base.xlsx') as Writer:
     data.to_excel(Writer, sheet_name = 'Dados')
        