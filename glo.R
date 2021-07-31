source("mylib.R")

globalPict <- function(df, n1, n2, idx) {
  yy = c()
  xx = c()
  for(i in idx) {
    y <- lessonPoints(df, i)
    y <- log(y)
    y <- y / max(y)
    x <- (1:length(y))
    #x <- x / max(x)
    xx <- c(xx, x[n1:n2])
    yy <- c(yy, y[n1:n2])
  }
  return(list(x=xx, y=yy))
}
g <- globalPict(df, 1, 7, c(10,16,17,20,21,22,24,25,26,29,31)) 
plot(g$x, g$y)
fit <- lm(g$y ~ g$x)
summary(fit)

