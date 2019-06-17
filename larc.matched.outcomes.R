#####################################################################################
#match two groups for comparison of outcomes.Data set should
#be cleaned before entry
#FUNCTION: larc.matched.outcomes
#PURPOSE : This matches two groups.
#INPUTS  : A data frame containing at least
#          1) covariates 
#          2) the test group (variate), and the two comparison groups within this test.
#         The identities of these columns MUST be specified as arguments
#         variate: which column will be the test column. should be ordinal for now.
#         group1 : the control group from the variate column
#         gropu2 : the case group from the same column
#         covariates: what to match on.
#         OUTCOME: the variable to compare between case and control, no NAs.
#         type: character fields in the covariates, or numerical?
#         verbose: if TRUE, report matching statistics.
#         multiple: if TRUE, allows return of multiple control matches per case.
#OUTPUTS : Two sets of statistics:
#         1) IF the outcome is numeric: both case/control sets are the same
#         2) IF the outcome is non-numeric: a) 0/1 indicator that outcome is filled, 2) outcome itself
#         3) the indices of each case and control in the original dataset.
#NOTES: - This only analyzes COMPLETE fields, i.e. NA in any column will cause the record be discarded!
#       - this requires library(optmatch). To install: > install.packages('optmatch')
#Ex: The ex compares graduating majors of white and black students, matching SAT test scorees and HS_GPA.
#Ex: kk <- larc.matched.outcomes(data,'STDNT_ETHNC_GRP_SHORT_DES',"White","Black",
#                                    c('MAX_SATI_MATH_SCR','MAX_SATI_VERB_SCR','HS_GPA'),
#                                     type=c('N','N','N'),OUTCOME='CUM_GPA')   
#
#####################################################################################  
larc.matched.outcomes <- function(data,variate,group1,group2,covariates,
                                  OUTCOME='UM_DGR_1_MAJOR_1_DES',type=c('C','C'),verbose=FALSE,multiple=FALSE)
{
  
  require(optmatch)
  require(RItools)
  
  ncov <- length(covariates)
  
  #Format the OUTCOME variable first
  type_outcome <- outcome.type(data,OUTCOME)
  data <- outcome.reformat(data,OUTCOME,type_outcome)
  
  #add in an index variable that will allow us to join all these results with the original dataset
  IND <- c(1:dim(data)[1])
  data <- data.frame(data,IND)
  
  #only keep what we want, don't chuck incomplete outcomes
  data <- data[,names(data) %in% c(variate,covariates,'OUTTEST','IND')]
  e    <- complete.cases(data) #only do the matching for complete records. 
  data <- data[which(e),]
  
  #pick out and clean up the case-control pairing
  temp <- as.character(data[,names(data) %in% variate])
  cln  <- temp == group1 | temp == group2
  lvl  <- levels(as.factor(temp[cln]))
  data <- data[which(cln),]
  nst  <- length(data[,1])
  
  #fill in the standard test columns depending on the OUTCOME type.
  print(paste('OUTCOME type = ',type_outcome,sep=""))
  if (type_outcome == 'factor' | type_outcome == 'character')
  {
    OUTBIN <- mat.or.vec(nst,1)
    e      <- data$OUTTEST != "NONE"
    OUTBIN[e] <- 1
    
    OUTCAT <- mat.or.vec(nst,1)
    OUTCAT <- data$OUTTEST
  }
  else 
  {
    OUTBIN <- data$OUTTEST
    OUTCAT <- data$OUTTEST
  }
  
  IND <- data$IND
  
  #now read the case vector and standardize it.
  assign(variate,as.numeric(data[,names(data) %in% variate]))
  temp <- eval(as.symbol(variate))
  jj   <- levels(as.factor(temp))
  e    <- temp == jj[1]
  e2   <- temp == jj[2]
 
  case <- mat.or.vec(length(temp),1)
  
  if (sum(e)  >  sum(e2))
  {
    case[e2] <- 1
    print(paste('using',lvl[2],'as cases'))
    pcasename  <- lvl[2]
    pctrlname  <- lvl[1]
  }
  if (sum(e2) >= sum(e))
  {
    case[e]  <- 1
    print(paste('using',lvl[1],'as cases'))
    pcasename  <- lvl[1]
    pctrlname  <- lvl[2]
  }
  
  datalm <- data.frame(case,OUTBIN,OUTCAT,IND)
  
  for (i in 1:ncov)
  {
    assign(covariates[i],as.character(data[,names(data) %in% covariates[i]]))  
    if (i == 1){clist <- covariates[i]}
    if (i > 1) {clist <- paste(clist,covariates[i],sep="+")}
    tt <- eval(as.symbol(covariates[i]))
    if (type[i] == 'N'){tt <- as.numeric(tt)}
    if (type[i] == 'C'){tt <- as.character(tt)}
    datalm <- data.frame(datalm,tt)
    names(datalm)[i+4] <- covariates[i]
  }
  
  #make the formula
  mod <- paste('case',"~",clist)
  model <- do.call("glm",list(as.formula(mod),data=as.name("datalm"),family=binomial()))
  
  #...and execute the matching with a caliper set at 0.2 to increase speed...
  m1    <- fullmatch(match_on(model,caliper=0.1),data=datalm)
  
  #comptue the post match balance statistics if VERBOSE
  if (verbose == TRUE)
  {
    mbalance <- xBalance(as.formula(mod), 
                         data = datalm, 
                         report = "z.scores",#c("chisquare.test", "std.diffs"), 
                         strata = data.frame(original = factor("none"), m1))
    print(mbalance)
  }
  
  #Now attach the matching structure to the data
  datalm <- cbind(datalm,matches=as.numeric(substr(m1,3,7)))
  datalm <- datalm[!is.na(datalm$matches),]
  
  #And sort once for speed, computing mean grades for the matched groups
  datalm       <- datalm[order(datalm$matches,datalm$case),] #This sort is crucial. Keeping the SEX makes sure that case is always index 1.
  datalm$count <- sequence(rle(as.vector(datalm$matches))$lengths)
  
  nid    <- length(datalm$matches[!duplicated(datalm$matches)])
  nstart <- which(datalm$count == 1)
  ntot   <- length(datalm$matches)
  
  outsize <- nid
  if (multiple == TRUE){outsize <- ntot}
  
  CASE_STATS1 <- mat.or.vec(outsize,1)
  CONT_STATS1 <- CASE_STATS1
  CASE_STATS2 <- CASE_STATS1
  CONT_STATS2 <- CASE_STATS1
  CONT_IND    <- CASE_STATS1
  CASE_IND    <- CASE_STATS1 
  NCONT       <- CASE_STATS1
  
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
  
    #for the default single control 
    case_range <- stop_ind
    ref_ind    <- i
    cont_range  <- start_ind
    ccount      <- 1
    
    #for multiple controls
    if (multiple == TRUE)
    {
      ref_ind     <- ind
      ccount      <- length(ind)-1
      cont_lrange <- start_ind
      cont_hrange <- stop_ind-1
      cont_range  <- c(cont_lrange:cont_hrange)
      #if (ccount == 1) #if we allow multiple matches, need to account differently for single matches.
      #{
      #  cont_range <- start_ind
      #  ref_ind    <- i
      #}
    }
    #print(paste('i = ',i,sep=""))
    #print(cont_range)
    NCONT[ref_ind]       <- ccount
      
    CONT_IND[ref_ind]    <- as.numeric(datalm$IND[cont_range])
    CASE_IND[ref_ind]    <- as.numeric(datalm$IND[case_range])
    
    CASE_STATS1[ref_ind] <- as.numeric(datalm$OUTBIN[case_range])
    CONT_STATS1[ref_ind] <- as.numeric(datalm$OUTBIN[cont_range])
    
    CASE_STATS2[ref_ind] <- as.character(datalm$OUTCAT[case_range])
    CONT_STATS2[ref_ind] <- as.character(datalm$OUTCAT[cont_range])
    
  }
  
  N <- nid
  out <- data.frame(N,NCONT,CASE_IND,CONT_IND,CASE_STATS1,CONT_STATS1,CASE_STATS2,CONT_STATS2)
  
  #finally, enforce that the same control can't be used twice. this is a minor bug that
  #i haven't been able to chase down.
  out <- out[!duplicated(out$CONT_IND),]
  
  return(out)
}

#This looks for designated outcome variable and gets its type
outcome.type <- function(data,OUTCOME)
{
  type <- class(data[,names(data) %in% OUTCOME])
  return(type)
}

#Replace the OUTCOME variable with a cleanly formatted 
#column that we will use in our statistics.
outcome.reformat <- function(data,OUTCOME,type)
{
  OUTTEST <- data[,names(data) %in% OUTCOME]
  
  #clean out NAs if it's a factor, replace with 'NONE', so OUTCOME is a complete case.
  if (type == 'factor')
  {
    e <- is.na(OUTTEST)
    if (sum(e,na.rm=TRUE) > 0){OUTTEST[e] <- 'NONE'}
  }
  
  #if OUTCOME is numeric or integer, we won't do anything.
  data <- data.frame(OUTTEST,data)
  return(data)
  
}
