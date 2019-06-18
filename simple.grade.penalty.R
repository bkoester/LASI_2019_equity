library(tidyverse)

load_sc_data <- function() {
  

  scname <- "C:/Users/tamckay/Documents/R Code/LASI_2019_equity-master/student_course.tab"
  sc <- read_tsv(scname)
  
  return(sc)
}

load_sr_data <- function() {
  
  srname <- "C:/Users/tamckay/Documents/R Code/LASI_2019_equity-master/student_record.tab"
  sr <- read_tsv(srname)
  
  return(sr)

}

all_course_statistics <- function(sr,sc) {
  
  crid<-paste(sc$SBJCT_CD,sc$CATLG_NBR)
  class_list <- unique(crid)
  nclasses <- length(class_list)  
  course_name <- rep("",nclasses)
  mean_grade <- rep(0.,nclasses)
  mean_gpao <- rep(0.,nclasses)
  mean_grade_penalty <- rep(0.,nclasses)
  mean_grade_male <- rep(0.,nclasses)
  mean_gpao_male <- rep(0.,nclasses)
  mean_grade_penalty_male <- rep(0.,nclasses)
  mean_grade_female <- rep(0.,nclasses)
  mean_gpao_female <- rep(0.,nclasses)
  mean_grade_penalty_female <- rep(0.,nclasses)
  div <- rep("",nclasses)
  
  for (i in 1:nclasses) {
    inclass <- which(crid == class_list[i] & sc$GRD_PNTS_PER_UNIT_NBR >= 0 &
                       sc$GRD_PNTS_PER_UNIT_NBR <= 4.4 & sc$EXCL_CLASS_CUM_GPA > 0 & sc$EXCL_CLASS_CUM_GPA <= 4.4)
    sct <- merge(sc[inclass,],sr,all.x=T)
    print(paste(sct$SBJCT_CD[1],sct$CATLG_NBR[1]))
    course_name[i] <- paste(sct$SBJCT_CD[1],sct$CATLG_NBR[1])
    mean_grade[i] <- mean(sct$GRD_PNTS_PER_UNIT_NBR)
    mean_gpao[i] <- mean(sct$EXCL_CLASS_CUM_GPA)
    mean_grade_penalty[i] <- mean_gpao[i]-mean_grade[i]
    male <- which(sct$STDNT_GNDR_SHORT_DES == 0)
    mean_grade_male[i] <- mean(sct$GRD_PNTS_PER_UNIT_NBR[male])
    mean_gpao_male[i] <- mean(sct$EXCL_CLASS_CUM_GPA[male])
    mean_grade_penalty_male[i] <- mean_gpao_male[i]-mean_grade_male[i]
    female <- which(sct$STDNT_GNDR_SHORT_DES == 1)
    mean_grade_female[i] <- mean(sct$GRD_PNTS_PER_UNIT_NBR[female])
    mean_gpao_female[i] <- mean(sct$EXCL_CLASS_CUM_GPA[female])
    mean_grade_penalty_female[i] <- mean_gpao_female[i]-mean_grade_female[i]
  }
  
  acsout <- data.frame(course_name,mean_grade,mean_gpao,mean_grade_penalty,
                       mean_grade_male,mean_gpao_male,mean_grade_penalty_male,
                       mean_grade_female,mean_gpao_female,mean_grade_penalty_female,div)
  
  
  return(acsout)
  
}


course_grade_penalty_plot <- function(sr,sc,subject,catalognbr) {
  
  e <- sc$SBJCT_CD == subject & sc$CATLG_NBR == catalognbr
  sc <- sc[which(e),]
  print(length(sc$EXCL_CLASS_CUM_GPA))

  #remove duplicates
  sc <- sc[!duplicated(sc$STDNT_ID),]
  print(length(sc$EXCL_CLASS_CUM_GPA))
  
  #merge with the student record to get characteristics of students
  pin <- merge(sc,sr,by='STDNT_ID',all.x=TRUE)  
  
  if (length(pin$EXCL_CLASS_CUM_GPA) == 0) {
    return("There are no students in this course")
  }
  
  tpout <- tprofile(pin$EXCL_CLASS_CUM_GPA,pin$GRD_PNTS_PER_UNIT_NBR,nxbins=40,xrange=c(1.0,4.))
  tpplot(tpout,xlab='GPAO',ylab='Grade',
         main=paste(subject," ",catalognbr),xlim=c(0,4),ylim=c(0,4))
  #Calculate the average grade penalty
  grade_penalty <- mean(pin$GRD_PNTS_PER_UNIT_NBR - pin$EXCL_CLASS_CUM_GPA,na.rm=T)
  legend(1.0,3.8,paste('Grade penalty =',format(grade_penalty,digits=2)))
}


course_grade_penalty_plot_by_gender <- function(sr,sc,subject,catalognbr,random=0) {
  
  e <- sc$SBJCT_CD == subject & sc$CATLG_NBR == catalognbr
  sc <- sc[which(e),]

  #remove duplicates
  sc <- sc[!duplicated(sc$STDNT_ID),]
  print(length(sc$EXCL_CLASS_CUM_GPA))
  
  #merge with the student record to get characteristics of students
  pin <- merge(sc,sr,by='STDNT_ID',all.x=TRUE)  
  
  if (length(pin$EXCL_CLASS_CUM_GPA) == 0) {
    return("There are no students in this course")
  }
  
  incourse_male <- which((pin$CATLG_NBR == catalognbr) & 
                           (pin$SBJCT_CD == subject) & (pin$STDNT_GNDR_SHORT_DES == 0))
  incourse_female <- which((pin$CATLG_NBR == catalognbr) & 
                             (pin$SBJCT_CD == subject) & (pin$STDNT_GNDR_SHORT_DES == 1))
  
  if (random != 0) {
    nmale <- length(incourse_male)
    nfemale <- length(incourse_female)
    allin <- c(incourse_male,incourse_female)
    resort <- sample(length(allin))
    incourse_male = allin[resort[1:nmale]]
    incourse_female <- allin[resort[(nmale+1):length(allin)]]
  }
  if (length(incourse_male) == 0) {
    return("There are no male students in this course")
  }
  pin_male=pin[incourse_male,]	
  if (length(incourse_female) == 0) {
    return("There are no female students in this course")
  }
  pin_female=pin[incourse_female,]
  
  tpout_male <- tprofile(pin_male$EXCL_CLASS_CUM_GPA,pin_male$GRD_PNTS_PER_UNIT_NBR,nxbins=20,xrange=c(1.0,4.))
  tpout_female <- tprofile(pin_female$EXCL_CLASS_CUM_GPA,pin_female$GRD_PNTS_PER_UNIT_NBR,nxbins=20,xrange=c(1.0,4.))
  tpplot(tpout_male,xlab='GPAO',ylab='Grade',
         main=paste(subject," ",catalognbr),xlim=c(0,4),ylim=c(0,4),col='blue')
  tpplot(tpout_female,oplot=1,col='red')
  #Calculate the average grade penalty
  grade_penalty_male <- mean(pin_male$GRD_PNTS_PER_UNIT_NBR - pin_male$EXCL_CLASS_CUM_GPA,na.rm=T)
  grade_penalty_female <- mean(pin_female$GRD_PNTS_PER_UNIT_NBR - pin_female$EXCL_CLASS_CUM_GPA,na.rm=T)
  
  if(random == 0) {
    legend(1,4,c(paste("Male GP = ",format(grade_penalty_male,digits=2)),
                 paste("Female GP = ",format(grade_penalty_female,digits=2))),pch=c(0,5))
  }
  if(random != 0) {
    legend(1,4,c(paste("Random 1: ",format(grade_penalty_male,digits=2)),
                 paste("Random 2: ",format(grade_penalty_female,digits=2))),pch=c(0,5))
  }
  
  #Report some simple group stats
  mean1<-formatC(mean(pin_male$MAX_ACT_MATH_SCR,na.rm=T),digits=3)
  mean2<-formatC(mean(pin_female$MAX_ACT_MATH_SCR,na.rm=T),digits=3)
  print(paste('ACT MATH (M,F):',mean1,mean2))
  mean1<-formatC(mean(pin_male$EXCL_CLASS_CUM_GPA,na.rm=T),digits=3)
  mean2<-formatC(mean(pin_female$EXCL_CLASS_CUM_GPA,na.rm=T),digits=3)
  print(paste('GPAO (M,F):',mean1,mean2))
  mean1<-formatC(mean(pin_male$GRD_PNTS_PER_UNIT,na.rm=T),digits=3)
  mean2<-formatC(mean(pin_female$GRD_PNTS_PER_UNIT,na.rm=T),digits=3)
  print(paste('Grade (M,F):',mean1,mean2))
  mean1<-formatC(mean(pin_male$HS_GPA,na.rm=T),digits=3)
  mean2<-formatC(mean(pin_female$HS_GPA,na.rm=T),digits=3)
  print(paste('HSGPA (M,F):',mean1,mean2))
}

#Custom generic routines for making the kinds of grade penalty plots which I like...


tprofile <- function(ar1,ar2,nxbins=10,xrange=c(0.,0.)) {
  
  if (xrange[1] == xrange[2]) {
    xrange[1] <- min(ar1,na.rm=T)
    xrange[2] <- max(ar1,na.rm=T)
  }
  
  xmean <- rep(0.,nxbins)
  ymean <- rep(0.,nxbins)
  xerr <- rep(0.,nxbins)
  yerr <- rep(0.,nxbins)
  xmeanerr <- rep(0.,nxbins)
  ymeanerr <- rep(0.,nxbins)
  tpout <- data.frame(xmean,ymean,xerr,yerr,xmeanerr,ymeanerr)
  
  xlow <- xrange[1]
  xbin <- (xrange[2]-xrange[1])/nxbins
  for (i in 1:nxbins) {
    ind <- which((ar1 >= xlow) & (ar1 < (xlow+xbin)))
    if (length(ind) > 0) {
      tpout$xmean[i] <- mean(ar1[ind])
      tpout$ymean[i] <- mean(ar2[ind])
      tpout$xerr[i] <- sd(ar1[ind])
      tpout$yerr[i] <- sd(ar2[ind])
      tpout$xmeanerr[i] <- sd(ar1[ind])/sqrt(length(ind))
      tpout$ymeanerr[i] <- sd(ar2[ind])/sqrt(length(ind))
    }
    xlow <- xlow+xbin
  }
  
  keep <- which((tpout$ymean != 0.) & (tpout$ymeanerr != 0.))
  tpout <- tpout[keep,]
  
  return(tpout)
}


tpplot <- function(tpout,ylab="",xlab="",main="",xlim=c(0.,0.),ylim=c(0.,0.),oplot=0,col='black') {
  
  if (xlim[1] == xlim[2]) {
    xlim[1] <- min(tpout$xmean)
    xlim[2] <- max(tpout$xmean)
  }
  if (ylim[1] == ylim[2]) {
    ylim[1] <- min(tpout$ymean)
    ylim[2] <- max(tpout$ymean)
  }
  
  if (oplot == 0) {
    plot(tpout$xmean,tpout$ymean,type='p',pch=0,lty=0,
         xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,main=main,col=col)
    lines(tpout$xmean,tpout$ymean+tpout$yerr,lty=2,col=col)
    lines(tpout$xmean,tpout$ymean-tpout$yerr,lty=2,col=col)
  }
  if (oplot != 0) {
    points(tpout$xmean,tpout$ymean,type='p',pch=5,col=col)
    lines(tpout$xmean,tpout$ymean+tpout$yerr,lty=3,col=col)
    lines(tpout$xmean,tpout$ymean-tpout$yerr,lty=3,col=col)
  }
  lines(c(-1.,5.),c(-1.,5.),lty=1)
  arrows(tpout$xmean, tpout$ymean-tpout$ymeanerr, 
         tpout$xmean, tpout$ymean+tpout$ymeanerr, length=0.05, 
         angle=90, code=3)	
}
