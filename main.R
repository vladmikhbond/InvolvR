source("mylib.R")
# library(ggplot2)

###

makeSecondFrame <- function(df, idx) {
  res <<- data.frame(df[what, c("Name","Id","Mark","La","Tut","Genr")])
  
  i2 <- 1
  for(i in idx) {

    course <- makeCourse(df, i)
    print(paste(course$name, "-", course$id), quote=F)
    
    xp <- course$problems$x
    yp <- course$problems$y
    
    xl <- course$lessons$x
    yl <- course$lessons$y
    
    appr <- appro3(course$lessons$ly)                          

    a1 <- -appr$a1
    b1 <- -appr$b1
    c1 <- -appr$c1
    
    res$A1[i2] <<- round(a1 - b1, 3)
    res$P1[i2] <<- round(length(appr$ax)/length(xl), 3)
    res$A2[i2] <<- round(b1 - c1, 3)
    res$P2[i2] <<- round((length(appr$ax)+length(appr$bx)-1)/length(xl), 3)
    
    res$StaN[i2] <<- max(yl)
    res$FinN[i2] <<- min(yl)
    res$FinP[i2] <<- as.integer(yl[length(yl)] * 100 / max(yl) )
    
    res$LesN[i2] <<- length(yl)
    res$ProN[i2] <<- length(yp)
    i2 <- i2 + 1
  }
  
}

makeCourse <- function(df, i) {
  p <- problemPoints(df, i)
  l <- lessonPoints(df, i)
  crs <- list(
    name = df$Name[i], 
    id = df$Id[i], 
    lessons = data.frame(y = l, ly = log(l), x=1:length(l)),
    problems = data.frame(y = p, lp = log(p), x=1:length(p))
  )
  return (crs)
}

printResume <- function(course, appr)  {
  
  yp <- course$problems$y
  finisProc = paste0(as.integer(yp[length(yp)] * 100 / max(yp)),"%")
  
  a1 <- -appr$a1
  b1 <- -appr$b1
  c1 <- -appr$c1

  code = paste0(breaking(a1, b1), breaking(b1, c1))
  
  print(paste(course$name, "-", course$id), quote=F)
  print(paste(yp[1], finisProc,
              "|", round(a1, 2), round(b1, 2), round(c1, 2),
              "|", code), quote=F)
}

showCourse <- function(course) {
  xp <- course$problems$x
  yp <- course$problems$y
  
  xl <- course$lessons$x
  lyl <- course$lessons$ly   
  
  appr3 <- appro3(lyl)                          
  printResume(course, appr3) 
  
  #
  par(mfrow=c(1,2))

  # log lessons
  
  plot(xl, lyl, type="l", col="black" )              
  
  # approximate lesson                                                 
  showApproximation3(appr3)
  
  appr2 <- appro2(lyl)  
  showApproximation2(appr2)
  
  # plot(xl, lyl, type="l", col="gray" ) 
  # par(new=T)
  y0 <- course$lessons$y
  y0[length(y0)] <- 0
  plot(xl, lyl, type="l", col="black" )              
  # showStamina(appr3)
}

showApproximation3 <- function(appr) {
  x1 <- appr$ax
  x2 <- appr$bx
  x3 <- appr$cx
  y1 <- appr$a1 * x1 + appr$a0  
  y2 <- appr$b1 * x2 + appr$b0  
  y3 <- appr$c1 * x3 + appr$c0
  lines(x1, y1, col="red")
  lines(x2, y2, col="red")
  lines(x3, y3, col="red")
}

showApproximation2 <- function(appr) {
  x1 <- appr$ax
  x2 <- appr$bx

  y1 <- appr$a1 * x1 + appr$a0  
  y2 <- appr$b1 * x2 + appr$b0  

  lines(x1, y1, col="blue")
  lines(x2, y2, col="blue")
}

# stamina distr on lessons
showStamina <- function(appr) {
  x1 <- appr$ax
  x2 <- appr$bx
  x3 <- appr$cx
  y1 <- appr$a1 * x1 + appr$a0  
  y2 <- appr$b1 * x2 + appr$b0  
  y3 <- appr$c1 * x3 + appr$c0
  y <- c(exp(y1), exp(y2[-1]), exp(y3[-1]))
  
  stam <- staminaDistr(nonIncrease(y))
  plot(stam, type="l", col="green" )
}

main <- function(idx) {

  for (i in idx)  
  {
    cat(paste(i, ". "))
    course <- makeCourse(df, i)
    showCourse(course)
    s <- readline()
    if (s == 'q') break 
    if (nchar(s) > 1) break 
  }
}

byId <- function(courseId) {
  i = which(df$Id==courseId)
  course <- makeCourse(df, i)
  showCourse(course)
}

##############################################################
df <- read.csv("file.txt")  
what <- c(10,16,17,20,21,22,24,25,26,29,31) 

makeSecondFrame(df, what)  
main(what)


