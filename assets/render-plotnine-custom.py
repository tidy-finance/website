from plotnine import theme_set, theme_bw, theme, element_rect

theme_set(
  theme_bw() + theme(
    legend_position="bottom",
    legend_key=element_rect(fill="white", color="white"),
    legend_box_spacing=0
  )
)
