lasso_rank <- function(data,pred='GRD_PNTS_PER_UNIT_NBR',indep=c('MAX_ACT_MATH_SCR','HS_GPA'))
{
  require(lars)
  
  temp <- data %>% select(pred,indep) %>% drop_na()
  y    <- pull(temp,pred)
  x    <- temp %>% select(indep)
  
  formi <- as.formula(paste('~',paste(indep,collapse='+'),sep=""))
  
  x <- model.matrix(formi,x)

  #x <- model.matrix(x)
  
  #colnames(x) <- indep
  out <- lars(x,y,type='lasso')
  plot(out)
  cv.lars(x,y)
  print(coef(out))
  return(out)
}