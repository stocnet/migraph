top3 <- function(res, dec = 4){
  unname(round(res, dec))[1:3]
}

bot3 <- function(res, dec = 4){
  lr <- length(res)
  unname(round(res, dec))[(lr-2):lr]
}

top5 <- function(res, dec = 4){
  unname(round(res, dec))[1:5]
}

bot5 <- function(res, dec = 4){
  lr <- length(res)
  unname(round(res, dec))[(lr-4):lr]
}
