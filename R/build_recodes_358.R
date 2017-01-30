require(tibble)

get_recode358<-function() {
  lines<-readLines(con = "./misc/recode358.txt")


  wrapped_lines<-grep("^[0-9]{3} ",lines,invert = T)
  wrapped_lines<-wrapped_lines[order(wrapped_lines,decreasing = T)]

  invisible(
    sapply(wrapped_lines,function(wrap) {
      lines[wrap-1]<<-paste(lines[wrap-1],lines[wrap])
      lines[wrap]<<-""
    })
  )
  lines<-lines[grep("^[0-9]{3} ",lines)]

  recode<-as.integer(substring(text = lines,first = 1,last = 3))
  icd10<-gsub(".*[(]([A-Z].*)[)].*","\\1",lines)
  icd10<-gsub(" ","",icd10)


  recode358<-tibble(recode,icd10)
  #recode358$icd10<-as.character(recode358$icd10)
  recode358_codes<-tibble()

  invisible(
    mapply(function(index,codes) {
      ranges<-strsplit(codes,split = ",")[[1]]
      starts<-gsub("-.*","",ranges)
      ends<-gsub(".*-","",ranges)
      df<-tibble(recode=index,start=starts,end=ends)
      recode358_codes<<-rbind(recode358_codes,df)
    }, recode358$recode, recode358$icd10)
  )
  recode358_codes_end<-recode358_codes$end
  recode358_codes_end<-gsub("(^...$)","\\1.99",recode358_codes_end)
  recode358_codes_end<-gsub("(^...[.][0-9]$)","\\19",recode358_codes_end)

  parent<-sapply(1:nrow(recode358_codes), function(index){
    x<-inside_icd(as.character(recode358_codes[index,"start"]),
                  as.character(recode358_codes[index,"end"]),
                  recode358_codes$start,
                  recode358_codes_end)

    x<-x[recode358_codes$recode[x]<recode358_codes$recode[index]]
    x<-x[!is.na(x)]

    n<-length(x)

    if(n>0) {
      x<-max(x)
    } else  {
      x<-NA
    }
    x
  })

  recode358_codes$parent<-recode358_codes$recode[parent]
  unique_358<-unique(recode358_codes[,c(1,4)])
}

inside_icd<-function(me_min,me_max,mins,maxs) {
  which(me_min>=mins & me_min<=maxs & me_max>=mins & me_max<=maxs)
}

gsub("(^...$)","\\1.99",recode358_codes$end)
gsub("(^...[.][0-9]$)","\\19",recode358_codes$end)
