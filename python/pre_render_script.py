import warnings
warnings.simplefilter(action="ignore", category=FutureWarning)

from plotnine import theme_set, theme, theme_bw
theme_set(theme_bw() + theme(legend_position="bottom"))
