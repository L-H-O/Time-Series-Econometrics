# -*- coding: utf-8 -*-
"""
Created on Fri Mar 29 11:51:58 2024

@author: lider
"""
## setup

import pandas as pd
import numpy as np
import os
import requests as req

login = os.getlogin()
home = os.path.join(r"C:\Users\{}".format(login), "Downloads")
os.chdir(home)
#%% import data

raw_inf = pd.read_csv("infant-mortality.csv")
raw_dem = pd.read_csv("V-Dem-CY-Full+Others-v14.csv", low_memory = False)
raw_pop = pd.read_csv("population.csv", low_memory = False)

raw_pwt = pd.read_excel("pwt1001.xlsx", sheet_name = 'Data')
raw_urb = pd.read_csv("urb-growth.csv", header = 2)
raw_fer = pd.read_csv("fertility-rate.csv", header = 2)

raw_gdppc = pd.read_html("http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=38375")[2]

#%% wrangling

df_inf = (raw_inf
                   .query("Entity == 'Brazil'")
                   .drop(columns = ["Code", "Entity"])
                   .assign(Year = lambda df_ : pd.to_datetime(df_["Year"],
                                                              format = "%Y"))
                   .sort_values(by = 'Year')
        )

df_dem = raw_dem.query("country_name == 'Brazil' and 1934 <= year <= 2021")

df_pop = (raw_pop
                .query("ISO3_code == 'BRA' and Variant == 'Medium'")
                [["Time", "PopTotal"]]
                .assign(Year = lambda df_ : pd.to_datetime(df_["Time"],
                                                           format = "%Y"))
                .drop(columns = 'Time')
                .sort_values(by = 'Year')
                )

df_pwt = (raw_pwt
                 .query("country == 'Brazil'")
                 [['year', 'hc']]
                 .assign(Year = lambda df_ : pd.to_datetime(df_["year"],
                                                            format = "%Y"))
                 .drop(columns = 'year')
                 .sort_values(by = 'Year')
                 )

df_urb = (raw_urb
                 .query("`Country Name` == 'Brazil'")
                 .T
                 .reset_index()
                 .iloc[4:]
                 .dropna()
                 .assign(Year = lambda df_ : pd.to_datetime(df_["index"],
                                                            format = "%Y")
                 )
                 .drop(columns = 'index')
                 .rename(columns = {29 :'urb_growth'})
                 )

df_fer = (raw_fer
                 .query("`Country Name` == 'Brazil'")
                 .T
                 .reset_index()
                 .iloc[4:]
                 .dropna()
                 .assign(Year = lambda df_ : pd.to_datetime(df_["index"],
                                                            format = "%Y")
                 )
                 .drop(columns = 'index')
                 .rename(columns = {29 :'fer_rate'})
                 )

df_gdppc = (raw_gdppc
                     .iloc[1:]
                     .assign(Year = lambda df_ : pd.to_datetime(df_[0],
                                                                format = "%Y" ))
                     .drop(columns = [0])
                     .rename(columns = {1 : "gdp_pc"})
                     .assign(gdp_pc = lambda df_ : df_["gdp_pc"].astype(float)*10)
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
                


df_dem.f