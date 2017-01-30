require(tibble)
require(stringr)

#get_recode358<-function() {
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

  ##
  ##  each line one recode at this point
  ##
  recode<-as.integer(substring(text = lines,first = 1,last = 3))
  icd10<-gsub(".*[(]([A-Z].*)[)].*","\\1",lines)
  icd10<-gsub(" ","",icd10)

  cause<-str_trim(gsub(".{16}(.*)[(]([A-Z].*)[)].*","\\1",lines))

  chapter<-character(length(lines))
  ch<-grep("^[IVX]{1,4}[.]",cause)
  chapter[ch]<-gsub("^([IVX]{1,4}[.])(.*)","\\1",cause[ch])

  cause<-str_trim(gsub("^[IVX]{1,4}[.](.*)","\\1",cause))
  t<-substring(text = lines,first=9,last=9)
  sex<-substring(text = lines,first=12,last=12)
  age<-substring(text = lines,first=16,last=16)


  recode358<-tibble(recode,t,sex,age,cause,chapter,icd10)
  #recode358$icd10<-as.character(recode358$icd10)
  recode358_codes<-tibble()

  invisible(
    mapply(function(index,codes) {
      ranges<-strsplit(codes,split = ",")[[1]]
      starts<-gsub("-.*","",ranges)
      ends<-gsub(".*-","",ranges)
      ##
      ##  at least 1 file has 1 item with a 0 (zero) instead of an O
      ##  in the first position ...
      ##  at least 1 file has asterisks with the icd10 code
      ##  have to take care of it here in case I read the file in from
      ## it's source again
      ##
      starts<-gsub("^0","O",starts)
      ends<-gsub("^0","O",ends)
      starts<-gsub("[*]","",starts)
      ends<-gsub("[*]","",ends)

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
#}


  recode358_codes_end<-recode358_codes$end
  recode358_codes_end<-gsub("(^...$)","\\1.99",recode358_codes_end)
  recode358_codes_end<-gsub("(^...[.][0-9]$)","\\19",recode358_codes_end)

  parent<-sapply(recode358$recode, function(recode){

    indices<-which(recode358_codes$recode==recode)

    codes<-sapply(indices,function(index) {


      x<-inside_icd(as.character(recode358_codes[index,"start"]),
                    as.character(recode358_codes[index,"end"]),
                    recode358_codes$start,
                    recode358_codes_end)

      recodes<-unlist(recode358_codes[x,"recode"])
      recodes<-as.integer(recodes[recodes<recode])
      recodes
    })
    un<-unlist(codes)
    dim(un)<-NULL
    un<-unique(un)

    dims<-dim(codes)


    if(recode==385) {
      cat("X")
    }
    if(is.list(codes) && length(codes[[1]])==0){
      p<-NA
    } else if (is.list(codes)) {
      tbl<-table(unlist(codes))
      m<-max(tbl)
      tbl<-tbl[tbl==m]
      p<-max(as.integer(names(tbl)))

    } else  if (is.null(dims) || length(dims)==1) {
      p<-max(codes)
    } else if (dims[2]==1) {
      p<-max(codes)
    } else {
      tbl<-table(codes)
      m<-max(tbl)
      tbl<-tbl[tbl==m]
      p<-max(as.integer(names(tbl)))

    }
    p

  })

  recode358$parent<-parent


  save(recode358,file="./data/recode358.RData")
  save(recode358_codes,file="./data/recode358_codes.RData")

inside_icd<-function(me_min,me_max,mins,maxs) {
  which(me_min>=mins & me_min<=maxs & me_max>=mins & me_max<=maxs)
}

