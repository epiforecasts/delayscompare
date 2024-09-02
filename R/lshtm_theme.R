########################
### From Ciara Judge ###
########################

lshtm_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "#01454F", fill = NA, size = 0.5),
    # color background 2)
    panel.background = element_rect(fill = "white"),
    # modify grid 3)
    #panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_line(colour = "aliceblue"),
    #panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_line(colour = "aliceblue"),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "#01454F"),
    axis.title = element_text(colour = "#01454F"),
    axis.ticks = element_line(colour = "#01454F"),
    # legend at the bottom 6)
    #legend.position = "bottom"
    strip.text.x = element_text(colour = "white"),
    strip.text.y = element_text(colour = "white"),
    strip.background = element_rect(
      color="#01454F", fill="#01454F", size=1.5, linetype="solid"
    ),
    #legend.position = "bottom",
    legend.title = element_text(colour = "#01454F", face = "bold"),
    legend.text = element_text(colour = "#01454F")
  )
}
