library(sp)
library(maptools)
library(raster)
library(rgdal)
library(foreign)
filter<-read.csv('file:///C:/Users/birde/Dropbox/ebird/filters/TexasFilterDump_20160114')
ababase<-read.csv('file:///C:/Users/birde/Dropbox/r_programs/ababase.csv')
counties<-read.csv('file:///C:/Users/birde/Dropbox/ebird/filters/county_list.csv')



sub.f<-merge(filter, ababase, by.x='taxon',by.y='species')
##
fname<-counties[,3]
dd<-strsplit(as.character(fname), split="--")
dd1<-strsplit(sapply(dd, function(x) x[2]),',')
dd2<-strsplit(sapply(dd1, function(x)paste0(x,collapse='')),'\\,')
dd3<-strsplit(sapply(dd2, function(x)paste0(x,collapse='')),'&')
dd4<-strsplit(sapply(dd3, function(x)paste0(x,collapse='')),' ')
dd5<-strsplit(sapply(dd4, function(x)paste0(x,collapse='')),'County')
dd6<-strsplit(sapply(dd5, function(x)paste0(x,collapse='')),' ')
dd7<-strsplit(sapply(dd6, function(x)paste0(x,collapse='')),' ')
dd8<-sapply(dd7, paste0,collapse='')
dd8

unq.filter<-data.frame(filtername=unique(sub.f$filter))
unq.filter$filtername<-tolower(unq.filter$filtername)
unq.filter$filtername1<-unq.filter$filtername
df1<-data.frame(countynames=dd8)
df1$countynames<-tolower(df1$countynames)
df2<-merge(df1, unq.filter, by.x='countynames',by.y='filtername',all=T)
#### no merge the new name with the counties
counties$mergename<-df1$countyname

sub.f$filter<-tolower(sub.f$filter)
sub.t<-merge(sub.f, counties[,c('COUNTY','mergename')], by.x='filter',by.y='mergename',all.x=T)
sub.t<-sub.t[,c('filter','dayOfYear','count','SpeciesID','COUNTY')]
sub.t$COUNTY[tolower(sub.t$COUNTY)=='dewitt']<-'de witt'
write.csv(sub.t,'C:/Users/birde/Dropbox/ebird/eBird_TX_filters_small.csv',row.names=F)

