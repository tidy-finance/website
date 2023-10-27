# NOTE: THIS DOES NOT WORK PROPERLY
import pandas as pd
import numpy as np
import pandas_datareader as pdr
import matplotlib.pyplot as plt
import wes

industries_raw = pdr.DataReader(
  name="10_Industry_Portfolios_daily",
  data_source="famafrench",
  start = "1927-01-01",
  end = "2022-12-31")[0]

industries = (industries_raw
  .divide(100)
  .reset_index(names="date")
  .rename(str.lower, axis="columns")
)

data_plot = industries.melt(id_vars='date', var_name='name', value_name='value')

data_plot['year'] = data_plot['date'].dt.to_period('Y')

data_plot = data_plot.groupby(['year', 'name'])

data_plot = data_plot.agg(total=('value', 'mean'), vola=('value', 'std')).reset_index()

data_plot['vola'] = pd.qcut(data_plot['vola'], q=48, labels=False) + 1

data_plot

levels = data_plot['vola'].drop_duplicates().sort_values().tolist()
data_plot['vola'] = data_plot['vola'].astype('category')

wes.set_palette('Zissou1')

unique_names = data_plot['name'].unique()
nrows = 2
fig, axes = plt.subplots(nrows=nrows, ncols=len(unique_names)//nrows, subplot_kw={'projection': 'polar'})
data_plot['year'] = data_plot['year'].astype("int64")

for idx, name in enumerate(unique_names):
    ax = axes.flatten()[idx]
    subset = data_plot[data_plot['name'] == name]
    
    # Polar bar plot
    bars = ax.bar(subset['year'], subset['total'])

    # Removing radial labels for clarity (similar to theme_void)
    ax.set_yticklabels([])
    ax.set_xticklabels([])
    ax.grid(False)
    ax.set_title(name, pad=20)

    # Adjusting the radius labels to make sure they don't overlap with the bars
    ax.set_rlabel_position(30)

# To hide axes
for ax in axes.flatten():
    ax.axis('off')

plt.tight_layout()
plt.show()
