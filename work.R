library("ggplot2")
obj <- ggplot(diamonds, aes(color, fill=cut)) + geom_bar(position="dodge") 
obj
