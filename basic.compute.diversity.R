basic_compute_diversity <- function(sr,sc,SBJCT_CD='PHYSICS',CATLG_NBR=140)
{
  library(tidyverse)
  #select the course
  sc = sc[sc$CATLG_NBR == CATLG_NBR & sc$SBJCT_CD == SBJCT_CD,]
  
  #merge the two tables
  hh = merge(sr,sc,by='STDNT_ID')
  
  #compute the diversity for the course
  #1) create hot-codec vector for each student
  hh <- hh %>% mutate(DIVMTX=str_c(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION))
  
  #2) count the number of each type of student
  CLASS_COUNTS <- hh %>% mutate(N=n()) %>% group_by(DIVMTX) %>% summarize(P=n()/N[1])
  
  #3) Print the table and compute the diversity
  print(CLASS_COUNTS)
  div        <- sum(CLASS_COUNTS$P^q)^(1/(1-q))
  print(paste('course diversity = ',div))
  
  #Now do the same thing university-wide for comparison.
  sr <- sr %>% mutate(DIVMTX=str_c(STDNT_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES,FIRST_GENERATION))
  CLASS_COUNTS <- sr %>% mutate(N=n()) %>% group_by(DIVMTX) %>% summarize(P=n()/N[1])
  print(CLASS_COUNTS)
  div       <- sum(CLASS_COUNTS$P^q)^(1/(1-q))
  print(paste('University diversity = ',div))
  
  
}