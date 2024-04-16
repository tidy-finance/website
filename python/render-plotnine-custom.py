theme_set(
  theme_bw() + theme(
    legend_position="bottom", 
    legend_key=element_rect(fill="white", color="white"), 
    title=element_text(size=10), 
    legend_box_spacing=0,
    figure_size=(6.4, 4))
)

class ggplot(ggplot):
    def __init__(self, data = None, mapping = None, environment = None):
        super().__init__(data, mapping, environment)
        self.scales.append(scale_color_grey())
        self.scales.append(scale_fill_grey())
