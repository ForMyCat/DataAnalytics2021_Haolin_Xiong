View(diamonds)
help(diamonds)

df <- diamonds
ggplot(data = diamonds)
geom_histogram(mapping = aes(x = data$x),binwidth = 0.5)
