basic_grade_anomaly <- function(sc,sr,SBJCT_CD='PHYSICS',CATLG_NBR=140)
{
  
  #define all of our output variables
  MALES_FULL_MEAN_GP   <- mat.or.vec(1,1)
  MALES_FULL_MEAN_GP[] <- NA
  FULL_MEAN_GP         <- MALES_FULL_MEAN_GP
  FULL_SD_GP           <- MALES_FULL_MEAN_GP
  FULL_SE_GP           <- MALES_FULL_MEAN_GP
  MALES_FULL_SD        <- MALES_FULL_MEAN_GP
  MALES_FULL_SE        <- MALES_FULL_MEAN_GP
  MALES_FULL_N         <- MALES_FULL_MEAN_GP
  FEMALES_FULL_MEAN_GP <- MALES_FULL_MEAN_GP
  FEMALES_FULL_SD      <- MALES_FULL_MEAN_GP
  FEMALES_FULL_SE      <- MALES_FULL_MEAN_GP
  FEMALES_FULL_N       <- MALES_FULL_MEAN_GP
  
  #Make a binned GPA
  sc$GPAO_GRANULAR = round(floor(sc$EXCL_CLASS_CUM_GPA*3)/3, digits=1)
  
  #select the course we want
  wc = sc[sc$CATLG_NBR == CATLG_NBR & sc$SBJCT_CD == SBJCT_CD,]
  
  #merge the two tables
  wc = merge(sr,wc,by='STDNT_ID')
  View(wc)
  #compute aggregate statistics by bin, within gender
  out = aggregate(wc$GRD_PTS_PER_UNIT_NBR, by = list(wc$STDNT_GNDR_SHORT_DES,wc$GPAO_GRANULAR),
                  FUN = c("mean"))
  
  #give those statistics pretty names
  names(out) = c("Gender", "Adj_Cum_GPA","Course_Grade","sd", "se","N")
  e <- out$Adj_Cum_GPA >= 0 & out$Adj_Cum_GPA < 4.3
  out <- out[which(e),]
  
  #Calculate non-gendered summary stats. This is an overblown way to get the
  #standard deviation.
  out2 = aggregate(wc$GRD_PTS_PER_UNIT_NBR, by = list( wc$GPAO_GRANULAR ),
                   FUN = c("mean", "sd", "se", "count"))
  names(out2) = c("Adj_Cum_GPA","Course_Grade","sd", "se","N")
  e <- out2$Adj_Cum_GPA >= 0 & out2$Adj_Cum_GPA < 4.3
  out2 <- out2[which(e),]
  
  #Full Grade Penalty and Gender Comparisons by Grade Penalty
  wc$penalty = wc$GRD_PTS_PER_UNIT-wc$GPAO
  wc <- wc[is.finite(wc$penalty),]
  FULL_MEAN_GP[i] <- mean(wc$penalty)
  FULL_SD_GP[i]   <- sd(wc$penalty)
  FULL_SE_GP[i]   <- FULL_SD_GP[i]/length(wc$penalty)
  d = aggregate(wc$penalty, by = list(wc$SEX), FUN = c("mean","sd","se"))
  
  #Grade Penalty D Value
  t = t.test(wc$penalty~wc$SEX)
  dval = round (abs(2 * t$statistic * -1/ t$parameter^.5), digits = 2)
  
  t = t.test(wc$penalty~wc$SEX)
  dval = round (abs(2 * t$statistic * -1/ t$parameter^.5), digits = 2)
  
  #View(out)
  f <- out$Gender == 'F'
  outf <- out[which(f),]
  nf  <- sum(f,na.rm=TRUE)
  m <- out$Gender == 'M'
  outm <- out[which(m),]
  nm <- sum(m,na.rm=TRUE)
  nmale <- nm
  nfemale <- nf
  
  plot(outm$Adj_Cum_GPA,outm$Course_Grade,xlim=c(0,4),ylim=c(0,4),pch=19,
       xlab='GPAO',ylab='Grade',main=course_list[i])
  points(outf$Adj_Cum_GPA,outf$Course_Grade,pch=19,col='red')
  legend(0,4,c('males','females'),text.col=c('black','red'))
  lines(c(0,4),c(0,4))
  
  #Add in the error bars
  for (j in 1:nm)
  {
    arrows(outm$Adj_Cum_GPA[j],outm$Course_Grade[j]-outm$se[j],outm$Adj_Cum_GPA[j],outm$Course_Grade[j]+outm$se[j],code=0)
  }
  for (j in 1:nf)
  {
    arrows(outf$Adj_Cum_GPA[j],outf$Course_Grade[j]-outf$se[j],outf$Adj_Cum_GPA[j],outf$Course_Grade[j]+outf$se[j],code=0,col='red')
  }
  
  #Add in the dispersion on the overall grade penalty
  lines(out2$Adj_Cum_GPA,out2$Course_Grade-out2$sd,col='blue',lty=2)
  lines(out2$Adj_Cum_GPA,out2$Course_Grade+out2$sd,col='blue',lty=2)
  
  
}

se <- function(dat)
{
  a  <- sd(dat)
  sed <- a/sqrt(length(data))
  return(sed)
}


compute_statistics <- function(wc)
{
    
  
  
  
}
  
