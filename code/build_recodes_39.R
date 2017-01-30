
build_recode_39<-function() {
  require(dplyr,quietly = T,warn.conflicts = FALSE)
  require(stringr,quietly = T,warn.conflicts = FALSE)


  recode39<-read.delim("./misc/recode39.tab",stringsAsFactors = F)

  recode39$ICD.10<-gsub("[()]","",recode39$ICD.10)
  colnames(recode39)<-tolower(colnames(recode39))

  recode39_codes<-data.frame()

  invisible(
    mapply(function(index,codes) {
      ranges<-strsplit(codes,split = ",")[[1]]
      starts<-gsub("-.*","",ranges)
      ends<-gsub(".*-","",ranges)
      df<-data.frame(recode=index,start=starts,end=ends)
      recode39_codes<<-rbind(recode39_codes,df)
    }, recode39$recode, recode39$icd.10)
  )

  recode39_codes<-inner_join(recode39_codes,recode39)
  recode39_codes$icd.10<-NULL
  recode39_codes$start<-str_trim(gsub("[*]","",recode39_codes$start))
  recode39_codes$end<-str_trim(gsub("[*]","",recode39_codes$end))

  recode39<-recode39_codes[,c("recode","t","parent","sex","age","cause")]
  recode39<-unique(recode39)
  recode39_codes<-recode39_codes[,c("recode","start","end")]


  save(recode39,file="./data/recode39.RData")
  save(recode39_codes,file="./data/recode39_codes.RData")
}

build_code39_categories<-function() {
  # category39<-data.frame(name=character(),regexp=character())
  # category39<-rbind(category39,data.frame(title="Cancer", name="cancer",regexp="eoplasm|lymphoma|Leuk"))
  # category39<-rbind(category39,data.frame(title="Human immunodeficiency virus (HIV) disease", name="hiv",regexp="HIV"))
  # category39<-rbind(category39,data.frame(title="Diabetes", name="diabetes",regexp="Diabetes"))
  # category39<-rbind(category39,data.frame(title="Alzheimer's Disease", name="alzheimers",regexp="Alzheim"))
  # category39<-rbind(category39,data.frame(title="Diseases of heart", name="heart_disease",regexp="[Hh]eart"))
  # category39<-rbind(category39,data.frame(title="Essential (primary) hypertension and hypertensive renal disease", name="hypertension",regexp="Essent.*[Hh]ypertens"))
  # category39<-rbind(category39,data.frame(title="Cerebrovascular diseases", name="cerebrovascular",regexp="Cerebrovasc"))
  # category39<-rbind(category39,data.frame(title="Influenza and pneumonia", name="influenza",regexp="Influenz"))
  # category39<-rbind(category39,data.frame(title="Chronic lower respiratory diseases", name="respiratory",regexp="respiratory"))
  # category39<-rbind(category39,data.frame(title="Chronic liver disease and cirrhosis", name="liver",regexp="liver.*cirrho"))
  # category39<-rbind(category39,data.frame(title="Nephritis, nephrotic syndrome, and nephrosis", name="nephritis",regexp="[Nn]ephrit"))
  # category39<-rbind(category39,data.frame(title="Intentional self-harm (suicide)", name="suicide",regexp="[Ss]uicide"))
  # category39<-rbind(category39,data.frame(title="Assault (homicide)", name="assault",regexp="[Aa]ssault"))

  category39<-read.csv("./misc/category39.csv")
  save(category39,file = "./data/category39.RData")
}


#' Cause of Death Codes - NVSS
#'
#' The National Vital Statistics System publishes annual reports on Mortality. Statistics are provided for categories based on the
#' 39 Selected Causes of Death. This function maps the categories to the 39 Selected Causes of Death.
#'
#' @param name character giving the name of the category. If omitted, returns a vector of valid names.This function uses 'fuzzy' matching, so using any of "cerebro","cerevasc","cvasc", for example, will match "cerebrovascular".
#'
#' @return
#' returns an integer vector of recode values used to identify cases for inclusion in this category or a character vector of valid names.
#'
#' @export
#'
#'
#' @examples
#' nvss_cause_of_death_39()
#'
#' recodes<-nvss_cause_of_death_39("cancer")
#' recodes<-nvss_cause_of_death_39("resp")
#'
#'
nvss_cause_of_death_39<-function(name) {
  data(category39,envir = environment())
  if (missing("name")) {
    return (as.character(category39$name))
  }

  fuzzy<-as.vector(adist(name,category39$name,partial=T))
  index<-which.min(fuzzy)

  data(recode39,envir = environment())
  parent<-category39[index,"recode_id"]
  res<-recode39[recode39$parent==parent,"recode"]
  return(res)

}


#' ICD-10 Index Of 39 Selected Causes of Death
#'
#' @param icd10 character ICD-10 code
#'
#' @return Recode value for 39 Selected Causes of Death
#' @export
#'
#' @examples
#' recode.cause.39("C45")
#'
recode.cause.39<-function(icd10) {
  data("recode39_codes",envir = environment())
  data("recode39",envir = environment())
  res<-recode39_codes[icd10>=recode39_codes$start&icd10<=recode39_codes$end,"recode"]
  recode39[recode39$recode%in%res & recode39$recode!=recode39$parent,"recode"]
}


