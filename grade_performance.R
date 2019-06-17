grade_performance <- function(sr,sc,SBJCT='PHYSICS',CATNUM=140,SBJCT2='NONE',CATNUM2='NONE')
{
  library(tidyverse)
  sr <- read_tsv('~/Google Drive/code/SEISMIC/LASI19code/LASI_2019_equity/student_record.tab')
  sc <- read_tsv('~/Google Drive/code/SEISMIC/LASI19code/LASI_2019_equity/student_course.tab')
  
  #physics 140
  #physics 240
  
  hh <- sc %>% filter(SBJCT_CD == SBJCT & CATLG_NBR == CATNUM) %>% 
        select(STDNT_ID,GRD_PNTS_PER_UNIT_NBR,EXCL_CLASS_CUM_GPA,TERM_CD) %>% left_join(sr)
  
  equity    <- course_equity(hh)
  diversity <- course_diversity(hh,sr)
  
  if (SBJCT2 != 'NONE')
  {
    tt <- sequence_inclusion(hh,sc,SBJCT=SBJCT2,CATNUM=CATNUM2)
    print(tt)
  }
  
  return(tt)
}

course_diversity <- function(hh,sr,q=2)
{
  #for the course
  hh <- hh %>% mutate(DIVMTX=str_c(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION))
  CLASS_NBR <- hh %>% group_by(DIVMTX) %>% count()
  #div       <- CLASS_NBR %>% summarize(COURSE_DIV=(sum(n^2)/sum(n)^q)^(1/(1-q)))
  div       <- (sum(CLASS_NBR$n^2)/sum(CLASS_NBR$n)^q)^(1/(1-q))
  print(paste('course diversity = ',div))
  print(hh %>% mutate(N=n()) %>% group_by(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION) %>% summarize(n()/N[1]))
  
  #university-wide
  sr <- sr %>% mutate(DIVMTX=str_c(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION))
  CLASS_NBR <- sr %>% group_by(DIVMTX) %>% count()
  #div       <- CLASS_NBR %>% summarize(COURSE_DIV=(sum(n^2)/sum(n)^q)^(1/(1-q)))
  div       <- (sum(CLASS_NBR$n^2)/sum(CLASS_NBR$n)^q)^(1/(1-q))
  print(paste('University diversity = ',div))
  print(sr %>% mutate(N=n()) %>% group_by(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION) %>% summarize(n()/N[1]))
  
}

course_equity <- function(hh)
{
  kk <- lasso_rank(hh,indep=c('MAX_ACT_MATH_SCR','HS_GPA','STDNT_UNDREP_MNRTY_CD',
                              'FIRST_GENERATION','EXCL_CLASS_CUM_GPA','STDNT_GNDR_SHORT_DES',
                              'MEDIAN_INCOME'))
  
  gpd <- hh %>% mutate(GPEN=GRD_PNTS_PER_UNIT_NBR-EXCL_CLASS_CUM_GPA) %>% 
    group_by(STDNT_GNDR_SHORT_DES) %>%
    summarize(GPD=mean(GPEN,na.rm=TRUE),se=sd(GPEN,na.rm=TRUE)/sqrt(n()))
  print('course equity')
  print(gpd)
  
  #pp <- pp_match(as.data.frame(hh))
  
}

sequence_inclusion <- function(hh,sc,SBJCT2,CATNUM2)
{
  
  #the next course
  hh2 <- sc %>% filter(SBJCT_CD == SBJCT2 & CATLG_NBR == CATNUM2) %>% select(STDNT_ID,TERM_CD)
  
  #now match the two
  hh  <- hh %>% left_join(hh2,by='STDNT_ID') %>% mutate(CONTINUE=1)
  
  hh$CONTINUE[which(is.na(hh$TERM_CD.y))] <- 0
  
  return(hh)
  
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

grade_anonmaly_plot <- function(hh)
{
  hh <- hh %>% mutate(GENDER=as.character(STDNT_GNDR_SHORT_DES),
                      FG=as.character(FIRST_GENERATION),
                      URM=as.character(STDNT_UNDREP_MNRTY_CD)) %>%
    select(c(MAX_ACT_MATH_SCR,MAX_ACT_ENGL_SCR,HS_GPA,GRD_PNTS_PER_UNIT_NBR,
             URM,GENDER,FG,FIRST_GENERATION,STDNT_UNDREP_MNRTY_CD,EXCL_CLASS_CUM_GPA))
  
  bin  <- cut_width(data$EXCL_CLASS_CUM_GPA,0.33)
  bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
  data <- add_column(data,bin)
  
}


useful_checks <- function(sr,sc)
{
  aa <- sc %>% group_by(SBJCT_CD,CATLG_NBR) %>% tally()
  View(aa)
  
}

gpao.binned.gndr <- function(scRES)
{
  #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
  tt <- scRES %>% group_by(GENDER,bin) %>% 
    summarise(mnGRD=signif(mean(GRD_PNTS_PER_UNIT_NBR),3),seGRD=signif(sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)/sqrt(n()),3))
  maxSE <- max(tt$seGRD,na.rm=TRUE)
  tt <- tt %>% replace_na(list(seGRD=maxSE))
  tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
  return(tt)
}

gpao.binned.urm <- function(scRES)
{
  #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
  tt <- scRES %>% group_by(URM,bin) %>% 
    summarise(mnGRD=signif(mean(GRD_PNTS_PER_UNIT_NBR),3),seGRD=signif(sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)/sqrt(n()),3))
  maxSE <- max(tt$seGRD,na.rm=TRUE)
  tt <- tt %>% replace_na(list(seGRD=maxSE))
  tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
  return(tt)
}

gpao.binned.first.gen <- function(scRES)
{
  #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
  tt <- scRES %>% group_by(FG,bin) %>% 
    summarise(mnGRD=signif(mean(GRD_PNTS_PER_UNIT_NBR),3),seGRD=signif(sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)/sqrt(n()),3))
  maxSE <- max(tt$seGRD,na.rm=TRUE)
  tt <- tt %>% replace_na(list(seGRD=maxSE))
  tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
  return(tt)

}
