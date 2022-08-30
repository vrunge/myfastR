

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

