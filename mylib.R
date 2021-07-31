# Делает последовательность невозрастающей
# 
nonIncrease <- function(x) {
  y <- x
  i <- length(y)
  repeat {
    j <- i - 1
    while( y[j] < y[i] && j > 1) {  
      j <- j - 1
    }
    if (i - j > 1) {
      a <- y[i]
      b <- y[j]
      for (k in (i-1):(j+1) ) {
        y[k] <- (a + b) / 2
      }
      
    }
    i <- j
    if (i < 1) {
      break
    }
    
  }
  return (y)
  
}

# Число решений самой решаемой задачи урока
# Урок задается строкой вида "8929;8257;8074;7820;" 
# 
maxInLesson <- function(s) {
  lst <- strsplit(s, ";")
  ns =  as.numeric(unlist(lst))
  return (max(ns))
}

# Число слушателей всех уроков
# 
lessonPoints <- function(df, i) {
  dropF <- df$DropF[i]
  dropL <- df$DropL[i]
  data <- df$Data[i]
  #
  ss <- unlist(strsplit(data, "!"))             # строки всех уроков 
  ss <- ss[ss != ""]                            # строки уроков с задачами   
  ss <- ss[(1 + dropF) : (length(ss) - dropL)]  # строки уроков без квази задач  
  #
  ns <- unlist(lapply(ss, maxInLesson))
  ns <- nonIncrease(ns)
  
  # ns <- log(ns)
  
  return(ns)
}

# Число решателей всех задач
# 
problemPoints <- function(df, i) {
  dropF <- df$DropF[i]
  dropL <- df$DropL[i]
  data <- df$Data[i]
  #
  ss <- unlist(strsplit(data, "!"))             # строки всех уроков 
  ss <- ss[ss != ""]                            # строки уроков с задачами   
  ss <- ss[(1 + dropF) : (length(ss) - dropL)]  # строки уроков без квази задач  
  s <- paste(ss, collapse = '')                   
  #
  ns <- as.integer(unlist(strsplit(s, ";")))
  ### ns <- nonIncrease(ns)
  return(ns)
}

# Распределение выносливости
# 
staminaDistr <- function(y) {
  y1 <- c(y[-1], y[length(y)])
  d = y - y1  # число людей с выносливостью x шагов (x также номер шага)
  return(d)
}

appro3 <- function(y) { 

  n <- length(y)
  
  minRests <- 10^20
  opt <- NULL
  for (a in 2:(n-2)) {
    aModel <- lineModel(1:a, y)
    for (b in (a+1):(n-1)) {
      bModel <- lineModel(a:b, y)
      cModel <- lineModel(b:n, y)
      rests <- sum(aModel$rests + bModel$rests + cModel$rests)
      if (rests < minRests) {
        opt <- c(a=aModel, b=bModel, c=cModel)
        minRests <- rests
      }
    }
  }
  r <- list(a1=as.double(opt$a.b1), a0=as.double(opt$a.b0), 
            b1=as.double(opt$b.b1), b0=as.double(opt$b.b0), 
            c1=as.double(opt$c.b1), c0=as.double(opt$c.b0),
            ax=opt$a.x, bx=opt$b.x, cx=opt$c.x )
  
  # correct middle line -----------------
     
  r$b_1 <- r$b1    # save source 
  r$b_0 <- r$b0    # save source 
  
  x1 <- r$bx[1] 
  x2 <- r$cx[1] 
  y1 <- x1 * r$a1 + r$a0
  y2 <- x2 * r$c1 + r$c0
  
  r$b1 <- (y1 - y2) / (x1 - x2)
  r$b0 <- (x1 * y2 - x2 * y1) / (x1 - x2)
  # -------------------------------------
  
  return (r)
}

appro2 <- function(y) { 
  
  n <- length(y)
  
  minRests <- 10^20
  opt <- NULL
  for (a in 2:(n-1)) {
    aModel <- lineModel(1:a, y)
    bModel <- lineModel(a:n, y)
    rests <- sum(aModel$rests + bModel$rests)
    if (rests < minRests) {
      opt <- c(a=aModel, b=bModel)
      minRests <- rests
    }
  }
  r <- list(a1=as.double(opt$a.b1), a0=as.double(opt$a.b0), 
            b1=as.double(opt$b.b1), b0=as.double(opt$b.b0), 
            ax=opt$a.x, bx=opt$b.x)
  
  # correct middle line -----------------
  
  r$b_1 <- r$b1    # save source 
  r$b_0 <- r$b0    # save source 
  
  x1 <- r$bx[1] 
  x2 <- r$bx[length(r$bx)] 
  y1 <- x1 * r$a1 + r$a0
  y2 <- x2 * r$b1 + r$b0
  
  r$b1 <- (y1 - y2) / (x1 - x2)
  r$b0 <- (x1 * y2 - x2 * y1) / (x1 - x2)
  # -------------------------------------
  
  return (r)
}

lineModel <- function(x, y) {
  fit <- lm(y[x] ~ x)
  result <- list(
    b0=fit$coefficients[1],
    b1=fit$coefficients[2],
    rests=sum(fit$residuals^2),
    x = x,
    fit = fit
  )
  return(result)
}

breaking <- function(a, b) {
  if (abs(a-b) < 0.01) return ("O")
  return (ifelse(a > b, "S", "F"))
}


