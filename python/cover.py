# -*- coding: utf-8 -*-
"""
Created on Mon Nov 20 10:24:56 2023

@author: CFrey
"""

# In[Libraries]

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from mizani.formatters import date_format
from matplotlib.ticker import FuncFormatter
from datetime import datetime
from matplotlib.colors import LinearSegmentedColormap

colors = ["#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"]
colormap = LinearSegmentedColormap.from_list("custom_colormap", colors)

# In[Data]

industries_raw = pd.read_excel('ffindustries.xlsx', index_col=0).div(100)
industries_raw.index = pd.to_datetime(industries_raw.index, format="%Y%m%d")

industries = (industries_raw.copy()
              .reset_index()
              .query('date >= "1927-01-01" & date <= "2022-12-31"')
              )

industries_long = pd.melt(industries, id_vars='date',
                          var_name='name',
                          value_name='value')
industries_order = sorted(industries_long['name'].unique())

data_plot = (industries_long
             .assign(year=industries_long['date'].dt.to_period("Y"))
             .groupby(['year', 'name'])
             .aggregate(total=('value', 'mean'),
                        vola=('value', 'std')
                        )
             .reset_index()
             )
num_cols = 4
num_rows = int(len(industries_order) / num_cols)
fig, axs = plt.subplots(num_rows, num_cols,
                        constrained_layout=True,
                        subplot_kw={"projection": "polar"}
                        )
axs = axs.flatten()

for i in enumerate(industries_order):

    df = data_plot.copy().query(f'name == "{i[1]}"')
    min_value = df['total'].min()
    max_value = df['total'].max()
    df['total'] = 2 * (df['total'] - min_value) / (max_value - min_value) - 1

    angles = np.linspace(0, 2 * np.pi, len(df), endpoint=False)
    values = df["total"].values
    width = 2 * np.pi / len(values)

    # Determines where to place the first bar.
    # By default, matplotlib starts at 0 (the first bar is horizontal)
    # but here we say we want to start at pi/2 (90 deg)
    offset = np.pi / 2

    # Initialize Figure and Axis
    ax = axs[i[0]]

    # Specify offset

    ax.set_theta_offset(offset)

    # Set limits for radial (y) axis.
    ax.set_ylim(-1, 1)

    # Remove all spines
    ax.set_frame_on(False)

    # Remove grid and tick marks
    ax.xaxis.grid(False)
    ax.yaxis.grid(False)
    ax.set_xticks([])
    ax.set_yticks([])

    normalize = plt.Normalize(min(values), max(values))
    colors = colormap(normalize(values))

    # Add bars
    ax.bar(angles, values, width=width, color=colors, edgecolor="white")

plt.tight_layout()
plt.subplots_adjust(wspace=-0.7, hspace=0)
# plt.show()
plt.gcf().savefig('test.svg', dpi=200, bbox_inches='tight', pad_inches=0, transparent=False)