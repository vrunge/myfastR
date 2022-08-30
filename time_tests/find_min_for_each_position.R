

##############################
######### find_min_1


find_min_1 <- function(info, n, p)
{
  setDF(info)
  res <- rep(Inf, p)
  for(i in 1:n)
  {
    if(res[info$k[i]] > info$m[i]){res[info$k[i]] <- info$m[i]}
  }
  return(res)
}

##############################
######### find_min_2


find_min_2 <- function(info)
{
  setDF(info)
  return(aggregate(info$m, list(info[,1]), min))
}

##############################
######### find_min_3


find_min_3 <- function(info)
{
  #library(data.table) solution
  setDT(info)
  return(info[ ,list(mean=min(m)), by=k])
}

##############################
######### find_min_4


find_min_4 <- function(info)
{
  # library(dplyr) solution
  setDT(info)
  return(info %>%
    group_by(k) %>%
    summarise_at(vars(m), list(name = min)))
}




find_min_1(infoGenerator(), 1000,10)
find_min_2(infoGenerator())
find_min_3(infoGenerator())
find_min_4(infoGenerator())



n <- 1000
p <- 10
info <- infoGenerator(n,p)
res <- microbenchmark(find_min_1(info, n,p),
                      find_min_2(info),
                      find_min_3(info),
                      find_min_4(info),
                    times = 100)
library(ggplot2)
autoplot(res)
res


########################################



n <- 10^2
p <- 10
info <- infoGenerator(n,p)

find_min_1234 <- function(n, p)
{
  info <- infoGenerator(n,p)
  start <-  Sys.time()
  res <- rep(Inf, p)
  for(i in 1:n)
  {
    if(res[info$k[i]] > info$m[i]){res[info$k[i]] <- info$m[i]}
  }
  end <-  Sys.time()
  r1 <- (end-start)[[1]]

  ###
  start <-  Sys.time()
  aggregate(info$m, list(info[,1]), min)
  end <-  Sys.time()
  r2 <- (end-start)[[1]]

  ###
  setDT(info)
  start <-  Sys.time()
  info[ ,list(mean=min(m)), by=k]
  end <-  Sys.time()
  r3 <- (end-start)[[1]]

  ###
  start <-  Sys.time()
  info %>%
    group_by(k) %>%
    summarise_at(vars(m), list(name = min))
  end <-  Sys.time()
  r4 <- (end-start)[[1]]


  return(c(r1,r2,r3,r4))
}


res <- find_min_1234(10^3,10)
res
order(res)


### CHOOSE 3

#n <- 10^2
# 3 2 1 4

#n <- 10^3
# 3 2 1 4

#n <- 10^4
# 3 2 4 1

#n <- 10^6
# 3 4 2 1

#n <- 10^7
# 4 3 2 1




