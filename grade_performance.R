grade_performance <- function(sr,sc)
{
  #physics 138
  #physics 276
  hh <- sc %>% filter(SBJCT_CD == 'Physics, General' & CATLG_NBR == 138) %>% 
        select(STDNT_ID,GRD_PNTS_PER_UNIT_NBR,EXCL_CLASS_CUM_GPA,TERM_CD) %>% left_join(sr)
  
  kk <- lasso_rank(hh,indep=c('MAX_ACT_MATH_SCR','HS_GPA','STDNT_UNDREP_MNRTY_CD',
                              'FIRST_GENERATION','EXCL_CLASS_CUM_GPA','STDNT_GNDR_SHORT_DES'))
  View(hh)
  
  gpd <- hh %>% mutate(GPEN=GRD_PNTS_PER_UNIT_NBR-EXCL_CLASS_CUM_GPA) %>% 
                group_by(STDNT_GNDR_SHORT_DES) %>%
                summarize(GPD=mean(GPEN,na.rm=TRUE),se=sd(GPEN,na.rm=TRUE)/sqrt(n()))
  print(gpd)
  pp <- pp_match(as.data.frame(hh))
  
  return(pp)
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