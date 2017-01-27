

build_recode_113<-function() {

  require(dplyr,quietly = T,warn.conflicts = FALSE)

  #recode113<-read.delim("./misc/recode113.tab",stringsAsFactors = F)
  recode113<-read.csv("./misc/recode113.csv",stringsAsFactors = F)

  recode113$icd10<-gsub("[()]","",recode113$icd10)
  colnames(recode113)<-tolower(colnames(recode113))

  recode113_codes<-data.frame()

  invisible(
    mapply(function(index,codes) {
      ranges<-strsplit(codes,split = ",")[[1]]
      starts<-gsub("-.*","",ranges)
      ends<-gsub(".*-","",ranges)
      df<-data.frame(recode=index,start=starts,end=ends)
      recode113_codes<<-rbind(recode113_codes,df)
    }, recode113$recode, recode113$icd10)
  )


  recode113_codes<-inner_join(recode113_codes,recode113)
  recode113_codes$icd.10<-NULL
  recode113_codes$start<-gsub("[*]","",recode113_codes$start)
  recode113_codes$end<-gsub("[*]","",recode113_codes$end)

  recode113<-recode113_codes[,c("recode","t","parent","sex","age","cause")]

  recode113<-unique(recode113)
  recode113[recode113$recode==recode113$parent,"parent"]<-NA

  recode113_codes<-recode113_codes[,c("recode","start","end")]

  save(recode113,file="./data/recode113.RData")
  save(recode113_codes,file="./data/recode113_codes.RData")

}
