top3 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:3]  
  } else unname(res)[1:3]
}

bot3 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-2):lr]
  } else unname(res)[(lr-2):lr]
}

top5 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:5]
  } else unname(res)[1:3]
}

bot5 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-4):lr]
  } else unname(res)[(lr-2):lr]
}
