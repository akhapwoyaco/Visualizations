#
import warnings
warnings.filterwarnings('ignore')

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

#reading the medals xlsx file
medals = pd.read_excel(r'./data/Medals.xlsx ',header=0)

medals_data = medals.copy()
medals_data = medals_data[(medals.Rank < 11)]
medals_data = medals_data.sort_values('Rank by Total')
medals_data = medals_data.drop(['Rank','Total','Rank by Total'], axis = 1)
medals_data 

medals_data.set_index(['Team/NOC'])[['Gold', 'Silver', 'Bronze']].stack().unstack().plot.barh(figsize=(9,10))
plt.ylabel('')
plt.title('Top 10: Medal Count')
plt.show()
plt.savefig('Top_10_Medal_Count.png', dpi = 560)

# # Gender Entries
#reading the gender xlsx file
entries_gender = pd.read_excel(r'./data/EntriesGender.xlsx',header=0)

entries_gender.head()
gender_datas = entries_gender[['Discipline','Female','Male','Total']].copy()
gender_datas = gender_datas.sort_values('Total')
#plot
gender_datas.set_index(['Discipline'])[['Female','Male']].stack().unstack().plot.barh(figsize=(9,10))
plt.ylabel('')
plt.title('Total Entries')
plt.show()
plt.savefig('entriesGender.png', dpi = 560)

gender_datas = entries_gender[['Discipline','Total']].copy()
#plot
gender_datas = gender_datas.sort_values('Total')
gender_datas.set_index(['Discipline'])[['Total']].stack().unstack().plot.barh(figsize=(9,10))
plt.ylabel('')
plt.title('Total Entries')
plt.show()
plt.savefig('entriesTotalCount.png', dpi = 560)
