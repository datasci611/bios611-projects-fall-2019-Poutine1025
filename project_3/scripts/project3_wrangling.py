#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 29 02:20:23 2019

@author: jianchen
"""
import pandas as pd
from scipy import stats

path="https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project2_2019/"
CLIENT=pd.read_csv(path + "CLIENT_191102.tsv", sep="\t")
DISABILITY_ENTRY=pd.read_csv(path + "DISABILITY_ENTRY_191102.tsv", sep="\t")
INCOME_ENTRY=pd.read_csv(path + "INCOME_ENTRY_191102.tsv", sep="\t")

NEW_CLIENT=CLIENT[["Client ID","Client Age at Entry","Client Gender","Client Primary Race","Client Ethnicity"]].dropna()
NEW_CLIENT.columns=["ID","Age","Gender","Race","Ethnicity"]
NEW_CLIENT=NEW_CLIENT[NEW_CLIENT.Race.isin(["American Indian or Alaska Native (HUD)",
                                            "Asian (HUD)",
                                            "Black or African American (HUD)",
                                            "Native Hawaiian or Other Pacific Islander (HUD)",
                                            "White (HUD)"])]
NEW_CLIENT=NEW_CLIENT[NEW_CLIENT.Ethnicity.isin(["Hispanic/Latino (HUD)","Non-Hispanic/Non-Latino (HUD)"])]
NEW_CLIENT=pd.DataFrame(NEW_CLIENT.groupby(["ID","Gender","Race","Ethnicity"]).mean()).reset_index()
NEW_INCOME_ENTRY=INCOME_ENTRY[["Client ID","Monthly Amount (Entry)"]].dropna()
NEW_INCOME_ENTRY.columns=["ID","Income"]
NEW_INCOME_ENTRY=NEW_INCOME_ENTRY[NEW_INCOME_ENTRY.Income>0]
NEW_INCOME_ENTRY=pd.DataFrame(NEW_INCOME_ENTRY.groupby(["ID"]).mean()).reset_index()
NEW_DISABILITY_ENTRY=DISABILITY_ENTRY[["Client ID","Disability Determination (Entry)"]].dropna()
NEW_DISABILITY_ENTRY.columns=["ID","Disability"]
NEW_DISABILITY_ENTRY=NEW_DISABILITY_ENTRY.groupby("ID").agg(lambda x: stats.mode(x)[0][0]).reset_index()
NEW_DISABILITY_ENTRY=NEW_DISABILITY_ENTRY[NEW_DISABILITY_ENTRY.Disability.isin(["No (HUD)","Yes (HUD)"])]
Income=pd.merge(NEW_CLIENT, NEW_INCOME_ENTRY)
Disability=pd.merge(NEW_CLIENT, NEW_DISABILITY_ENTRY)

Income.to_csv("./scripts/Income.csv", index=False)
Disability.to_csv("./scripts/Disability.csv", index=False)
NEW_CLIENT.to_csv("./scripts/Client.csv", index=False)