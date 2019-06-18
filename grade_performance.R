grade_performance <- function(sr,sc,SBJCT='PHYSICS',CATNUM=140,SBJCT2='NONE',CATNUM2='NONE',
                              EQUITY=FALSE,DIVERSITY=FALSE,INCL=FALSE,ISREAL=FALSE)
{
  library(tidyverse)
  #sr <- read_tsv('~/Google Drive/code/SEISMIC/LASI19code/LASI_2019_equity/student_record.tab')
  #sc <- read_tsv('~/Google Drive/code/SEISMIC/LASI19code/LASI_2019_equity/student_course.tab')
  
  if (ISREAL==TRUE)
  {
    sr <- sr %>% drop_na() %>% mutate(STDNT_GNDR_SHORT_DES=case_when(STDNT_GNDR_SHORT_DES == 'Male' ~ 0, 
                                                       STDNT_GNDR_SHORT_DES == 'Female' ~ 1))
    sc <- sc %>% filter(TERM_CD >= 1860)
  }
  
  #physics 140
  #physics 240
  
  hh <- sc %>% filter(SBJCT_CD == SBJCT & CATLG_NBR == CATNUM) %>% 
        select(STDNT_ID,GRD_PNTS_PER_UNIT_NBR,EXCL_CLASS_CUM_GPA,TERM_CD) %>% left_join(sr)
  
  if (EQUITY == TRUE)
  {
    equ    <- course_equity(hh)
  }  
  
  if (DIVERSITY == TRUE)  
  {
    div <- course_diversity(hh,sr,q=0.5)
  }
  
  if (INCL==TRUE)
  {
    tt <- sequence_inclusion(hh,sc,SBJCT=SBJCT2,CATNUM=CATNUM2)
    print(tt)
  }
  
  return()
}

#compute diversity by gender, race, ethnicity
course_diversity <- function(hh,sr,q=2)
{
  #for the course
  hh <- hh %>% mutate(DIVMTX=str_c(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION))
  CLASS_NBR <- hh %>% mutate(N=n()) %>% group_by(DIVMTX) %>% summarize(P=n()/N[1])
  print("course demographic table")
  print(CLASS_NBR)
  div        <- sum(CLASS_NBR$P^q)^(1/(1-q))
  print(paste('course diversity = ',div))
  
  #university-wide
  sr <- sr %>% mutate(DIVMTX=str_c(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION))
  CLASS_NBR <- sr %>% mutate(N=n()) %>% group_by(DIVMTX) %>% summarize(P=n()/N[1])
  print("university demographic table")
  print(CLASS_NBR)
  div       <- sum(CLASS_NBR$P^q)^(1/(1-q))
  print(paste('University diversity = ',div))
  #print(sr %>% mutate(N=n()) %>% group_by(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION) %>% summarize(n()/N[1]))
  
}

#dig into course equity
course_equity <- function(hh)
{
  kk <- lasso_rank(hh,indep=c('MAX_ACT_MATH_SCR','HS_GPA','STDNT_UNDREP_MNRTY_CD',
                              'FIRST_GENERATION','EXCL_CLASS_CUM_GPA','STDNT_GNDR_SHORT_DES',
                              'MEDIAN_INCOME'))
  
  gpd <- hh %>% mutate(GPEN=GRD_PNTS_PER_UNIT_NBR-EXCL_CLASS_CUM_GPA) %>% 
    group_by(STDNT_GNDR_SHORT_DES) %>%
    summarize(GPD=mean(GPEN,na.rm=TRUE),se=sd(GPEN,na.rm=TRUE)/sqrt(n()))
  print('first order course equity:')
  print(gpd)
  
  #run propensity score matching
  psm <- pp_match(as.data.frame(hh))
  
  #now compute the case and control statistics
  stats <- psm %>% summarize(mnCASE=mean(CASE_STATS1),seCASE=sd(CASE_STATS1)/sqrt(n()),
                             mnCONT=mean(CONT_STATS1),seCONT=sd(CONT_STATS1)/sqrt(n()),
                             mnGRAND=mean(CASE_STATS1-CONT_STATS1),
                             seGRAND=sd(CASE_STATS1-CONT_STATS1)/n())
  
  print('PSM-assessed equity: cases-controls:')
  print(stats)
  
  return(stats)
  
}

sequence_inclusion <- function(hh,sc,SBJCT2,CATNUM2)
{
  
  #the next course
  hh2 <- sc %>% filter(SBJCT_CD == SBJCT2 & CATLG_NBR == CATNUM2) %>% select(STDNT_ID,TERM_CD)
  
  #now match the two
  hh  <- hh %>% left_join(hh2,by='STDNT_ID') %>% mutate(CONTINUE=1)

  hh$CONTINUE[which(is.na(hh$TERM_CD.y))] <- 0
  
  ll <- hh %>% mutate(N=n()) %>% group_by(GRD_PNTS_PER_UNIT_NBR) %>% 
         summarize(nex=sum(CONTINUE)/n(),sqrt(nex*(1-nex)/n()))
  ll <- hh %>% mutate(N=n()) %>% group_by(STDNT_GNDR_SHORT_DES) %>% 
         summarize(nex=sum(CONTINUE)/n(),sqrt(nex*(1-nex)/n()))
  
  return(ll)
  
}

pp_match <- function(hh)
{
  library(optmatch)
  dir <- '/Users/bkoester/Google Drive/code/REBUILD/LARC.GITHUB/'
  source(paste(dir,'larc.matched.outcomes.R',sep=''))
  kk <- larc.matched.outcomes(hh,'STDNT_GNDR_SHORT_DES',
                              "0","1",
                              c('MAX_ACT_MATH_SCR','EXCL_CLASS_CUM_GPA','HS_GPA'),
                               type=c('N','N','N'),OUTCOME='GRD_PNTS_PER_UNIT_NBR')
  return(as_tibble(kk))
  
}


