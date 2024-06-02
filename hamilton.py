# -*- coding: utf-8 -*-
"""
Created on Sun Jun  2 00:03:35 2024

@author: lider
"""

import pandas as pd
import os
from quantecon import hamilton_filter

#%%

df = (pd.read_excel(r"D:\ECONOMIA\7º SEMESTRE 2024\Econometria III\Base.xlsx")
        .query("Year < '2019'")
        [["Year", "imr", "pol_violence", "gdp_pc"]]
        )

imr_hamilton = (pd.DataFrame(
                            df["imr"].pipe(hamilton_filter, h = 2, p = 1),
                            index = [["imr_cycle", "imr_trend"]]
                            )
                            .T
                            .assign(Year = df["Year"].values.flatten())
                            )

pol_violence_hamilton = (pd.DataFrame(
                            df["pol_violence"].pipe(hamilton_filter, h = 2, p = 1),
                            index = [["pol_violence_cycle", "pol_violence_trend"]]
                            )
                            .T
                          #  .assign(Year = df["Year"].values.flatten())
                            )

gdp_pc_hamilton = (pd.DataFrame(
                            df["gdp_pc"].pipe(hamilton_filter, h = 2, p = 1),
                            index = [["gdp_pc_cycle", "gdp_pc_trend"]]
                            )
                            .T
                          #  .assign(Year = df["Year"].values.flatten())
                            )

to_r = pd.concat([imr_hamilton, pol_violence_hamilton, gdp_pc_hamilton], axis = 1).dropna()

with pd.ExcelWriter(os.path.join(r"D:\ECONOMIA\7º SEMESTRE 2024\Econometria III", "Base_hamilton.xlsx" )) as writer:
    
    to_r.to_excel(writer)
    

