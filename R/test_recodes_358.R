


index<-393

cat(paste0(recode258_codes[index,"start"],"-",
           recode258_codes[index,"end"],"\n"))

x<-inside_icd(as.character(recode258_codes[index,"start"]),
           as.character(recode258_codes[index,"end"]),
           recode258_codes$start,
           recode258_codes$end)

x<-x[length(x)-1]

x

# me_min<-recode258_codes[index,"start"]
# me_max<-recode258_codes[index,"end"]
# mins<-recode258_codes$start
# maxs<-recode258_codes$end
#
#
# which(me_min>=mins & me_min<=maxs & me_max>=mins & me_max<=maxs)


# index2<-389
#
#   recode258_codes[index,"start"]>=recode258_codes[index2,"start"] &
#   recode258_codes[index,"start"]<=recode258_codes[index2,"end"] &
#   recode258_codes[index,"end"]>=recode258_codes[index2,"start"] &
#   recode258_codes[index,"end"]<=recode258_codes[index2,"end"])
#


