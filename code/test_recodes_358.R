


recode<-385

indices<-which(recode358_codes$recode==recode)

codes<-sapply(indices,function(index) {
  x<-inside_icd(as.character(recode358_codes[index,"start"]),
                as.character(recode358_codes[index,"end"]),
                recode358_codes$start,
                recode358_codes$end)

  recodes<-unlist(recode358_codes[x,"recode"])
  icd10s<-paste0(recode358_codes[index,"start"],"-",
                 recode358_codes[index,"end"],":")
  lenc<-nchar(icd10s)
  over<-paste(rep(" ",15-lenc),collapse="")
 # cat(paste(icd10s,over,paste(x,collapse=","),"\n"))
  cat(paste(icd10s,over,paste(recodes,collapse=","),"\n"))


  x
})




# me_min<-recode358_codes[index,"start"]
# me_max<-recode358_codes[index,"end"]
# mins<-recode358_codes$start
# maxs<-recode358_codes$end
#
#
# which(me_min>=mins & me_min<=maxs & me_max>=mins & me_max<=maxs)


# index2<-389
#
#   recode358_codes[index,"start"]>=recode358_codes[index2,"start"] &
#   recode358_codes[index,"start"]<=recode358_codes[index2,"end"] &
#   recode358_codes[index,"end"]>=recode358_codes[index2,"start"] &
#   recode358_codes[index,"end"]<=recode358_codes[index2,"end"])
#


