get_census <- function()
{
  #load/install this library
  library(censusapi)
  #https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
  
  #go here to register for a key:
  #https://api.census.gov/data/key_signup.html
  Sys.setenv(CENSUS_KEY="<yourapi>")

  #apis <- listCensusApis()
  #View(apis)
  
  #vars <- listCensusMetadata(name = "acs/acs5", 
  #                           type = "variables")
  #View(vars)
  
  #this pulls zipcode median income
  zip_income <- getCensus(name = "acs/acs5", vars="B24091_001E",
                           region="zip code tabulation area:*",vintage=2017)
  
  return(zip_income)
}