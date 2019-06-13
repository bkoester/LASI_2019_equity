grade_performance <- function(sr,sc,SBJCT='Physics, General',CATNUM=167)
{
  library(tidyverse)
  sr <- read_tsv('~/Google Drive/code/SEISMIC/LASI19code/LASI_2019_equity/student_record.tab')
  sc <- read_tsv('~/Google Drive/code/SEISMIC/LASI19code/LASI_2019_equity/student_course.tab')
  
  #physics 167
  #physics 205
  hh <- sc %>% filter(SBJCT_CD == SBJCT & CATLG_NBR == CATNUM) %>% 
        select(STDNT_ID,GRD_PNTS_PER_UNIT_NBR,EXCL_CLASS_CUM_GPA,TERM_CD) %>% left_join(sr)
  
  equity <- course_equity(hh)
  diversity <- course_diversity(hh,sr)
  
  #return(pp)
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
  
  pp <- pp_match(as.data.frame(hh))
  
  
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


useful_checks <- function(sr,sc)
{
  aa <- sc %>% group_by(SBJCT_CD,CATLG_NBR) %>% tally()
  View(aa)
  
}