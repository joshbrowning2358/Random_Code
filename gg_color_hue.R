#Reproduce the colors from ggplot2
gg_color_hue = function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
