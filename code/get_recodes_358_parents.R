require(tibble)

#get_recode358<-function() {

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

