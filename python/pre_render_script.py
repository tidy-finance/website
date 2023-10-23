import warnings
warnings.simplefilter(action="ignore", category=FutureWarning)

from plotnine import theme_set, theme, theme_bw
theme_set(theme_bw() + theme(legend_position="bottom"))

from mizani.breaks import date_breaks
from mizani.formatters import date_format, percent_format
custom_palette = ["#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"]
