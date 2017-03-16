library(XML)
library(data.table)
library(rio)
library(dplyr)
library(sqldf)

feldolgozo<- function(nev){
print(nev)

doc = htmlParse(nev, encoding = "UTF-8")
node.set = getNodeSet(doc, "//table", sessionEncoding = "UTF-8")

col1 = getNodeSet(node.set[[2]], "./tbody/tr/td[1]", sessionEncoding = "UTF-8")
col2 = getNodeSet(node.set[[2]], "./tbody/tr/td[2]", sessionEncoding = "UTF-8")
col3 = getNodeSet(node.set[[2]], "./tbody/tr/td[3]", sessionEncoding = "UTF-8")

col1 <- col1[-1]
col2 <- col2[-1]
col3 <- col3[-1]

df = lapply(col1, function(node) {
  res = getChildrenStrings(node, encoding = "UTF-8")
  forras = res[1]
  varos = res[3]
  res = getChildrenStrings(getNodeSet(node, "./a", sessionEncoding = "UTF-8")[[1]], encoding = "UTF-8")
  nyertes = res[1]
  leiras = res[3]
  data.frame(forras, varos, nyertes, leiras, stringsAsFactors = F)
}

)	
df = do.call("rbind", df)


df2 = lapply(col2, function(node) {
  res = getChildrenStrings(node, encoding = "UTF-8")
  datum = res[1]
  data.frame(datum, stringsAsFactors = F)
}
)	
df_2 = do.call("rbind", df2)

df3 = lapply(col3, function(node) {
  res = getChildrenStrings(node, encoding = "UTF-8")
  osszeg = res[1]
  data.frame(osszeg, stringsAsFactors = F)
}
)	
df_3 = do.call("rbind", df3)
my_df <- cbind(df, df_2, df_3)
my_df$forras <- gsub('\n', '', my_df$forras)
my_df$varos <- gsub('\n', '', my_df$varos)
my_df$leiras <- gsub('\n', '', my_df$leiras)
my_df$nyertes <- gsub('\n', '', my_df$nyertes)
my_df$datum<- as.Date(my_df$datum, format = '%Y.%m.%d')
my_df$osszeg <- as.numeric(gsub(' ', '', my_df$osszeg))/1000000
my_df$program <- sapply(strsplit(my_df$forras, ' - '), "[", 2)
my_df$operativ_program <- sapply(strsplit(sapply(strsplit(my_df$forras,' '), "[", 1), '-'),"[", 1)
my_df$forras <- sapply(strsplit(my_df$forras, ' - '), "[", 1)

my_df <- my_df[,c(1,8,7,2:6)] 
my_df<- data.table(my_df)
return(my_df)

}

full_adat<- data.frame()

my_list <-paste('adat', paste(1:76, '.html', sep=''), sep = '/')


for (i in my_list) {
  adat<- feldolgozo(nev = i)
  full_adat<- rbind(full_adat,adat )
}
rm(adat)
full_adat$telep_help <- sapply(strsplit(full_adat$varos,' \\('), "[", 1)

# http://www.ksh.hu/docs/hun/hnk/hnk_2016.xls
telepules_adatok <- read.csv("telepules_info_ksh_2016.csv", stringsAsFactors = F, sep = ",", header = T)
telepules_adatok$roma_onkormanyzat <- ifelse(telepules_adatok$roma_onkormanyzat==1, 'van', 'nincs')
telepules_adatok$Lako_nepesseg <- gsub(',', '', telepules_adatok$Lako_nepesseg)
telepules_adatok <- telepules_adatok[telepules_adatok$Telepules!='',]

# nem tudom honnan van az adat
ttipus <-  data.table(read.csv("telepules_tipus.csv", stringsAsFactors = F, sep = ",", header = T))

telepules_adatok <- data.table(merge(telepules_adatok, ttipus, by.x = 'Telepules', by.y = 'telepules',all.x = T, all.y = F))
telepules_adatok$tipus <- ifelse(is.na(telepules_adatok$tipus),'nem_hatranyos_helyzetu',telepules_adatok$tipus) 


sz2020 <- data.table(merge(full_adat, telepules_adatok, by.x = 'telep_help', by.y = 'Telepules', all.x = T ))
sz2020 <- sz2020[,-1, with=F]

sz2020 <- sz2020[!duplicated(sz2020),]

sz2020$ev <-year(sz2020$datum) 


sz2020$Kisterseg <- ifelse(sz2020$Kisterseg=='' & sz2020$varos=='Budapest', 'Budapest', sz2020$Kisterseg)


write.csv(sz2020, '../2020shiny/szechenyi2020_adatok.csv', row.names = F)




