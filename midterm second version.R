## load libraries
library(data.table)
library(magrittr)
require(foreign)
library(ggplot2)
require(scales)
library(lubridate)
library(tidyr)
library(dplyr)
## read in and glimpse
## first look: accid
accid<-read.dbf("Accid.dbf")
acc1<-read.dbf("lookups/acc.dbf")
ncol(accid)
nrow(accid)
head(accid)
#first step: check NAs for each column
indi = rep(0,ncol(accid))
for(i in 1: ncol(accid)){indi[i] = sum(!is.na(accid[,i]))}
which(indi==0)
# no column all NA



#########################################################
#stable information of this dataset
#default setup:1 
#0 means stable information for this column
#Since there is no column that is all NA, it only two cases: First some NAs and some other stuff
#Second, all other stuff(no NAs at all)
#Our goal is to find out which column has stable information(only has one level when converted to factor)
index=rep(1, ncol(accid))

for(i in 1:ncol(accid))
{
  if(length(levels(factor(accid[[i]])))==1)
  {index[i]=0}
  for(j in 1:nrow(accid))
  {
    if(is.na(accid[j,i])==TRUE)
    {index[i]=1
    break}
  }  
}
which(index==0)
colnames(accid[which(index==0)])
tidyaccid<-accid[,-c(which(index==0))]
ncol(tidyaccid)
## So we remove sitestate column since they are all MAs



#######################################################
# replace the numbers in Nature, bodypart, source, event, environ, human in tidyaccid with codes in Acc 
sum(acc1$CATEGORY=="PART-BODY")
parts<-acc1[(acc1$CATEGORY=="PART-BODY"),]
parts<-select(parts, CODE, VALUE)
head(parts)
colnames(parts)<-c("BODYPART", "VALUE")
str(parts)
tidyaccid<-left_join(tidyaccid, parts, by="BODYPART")
## remove the BODYPART column since they are all 
dr = function(name){return(which(colnames(tidyaccid)==name))}
tidyaccid<-tidyaccid[,-c(dr("BODYPART"))]
ncol(tidyaccid)
colnames(tidyaccid)[15]<-c("BODYPART")
head(tidyaccid)

## repeat the process for other columns
nature<-acc1[(acc1$CATEGORY=="NATUR-INJ"), ]
nature<-select(nature, CODE, VALUE)
head(nature)
colnames(nature)<-c("NATURE", "VALUE")
str(nature)
tidyaccid<-left_join(tidyaccid, nature, by="NATURE")
tidyaccid<-tidyaccid[, -c(dr("NATURE"), dr("VALUE.y"))]
ncol(tidyaccid)
colnames(tidyaccid)[15]<-c("NATURE")

event<-acc1[(acc1$CATEGORY=="EVENT-TYP"), ]
event<-select(event, CODE, VALUE)
colnames(event)<-c("EVENT", "VALUE")
str(event)
tidyaccid<-left_join(tidyaccid, event, by="EVENT")
tidyaccid<-tidyaccid[, -c(dr("EVENT"))]
ncol(tidyaccid)
colnames(tidyaccid)[15]<-c("Event")

human<-acc1[(acc1$CATEGORY=="HUMAN-FAC"), ]
human<-select(human, CODE, VALUE)   
colnames(human)<-c("HUMAN", "VALUE")
str(human)
tidyaccid<-left_join(tidyaccid, human, by="HUMAN")
tidyaccid<-tidyaccid[, -c(dr("HUMAN"))]
ncol(tidyaccid)
colnames(tidyaccid)[15]<-c("HUMAN")

source<-acc1[(acc1$CATEGORY=="SOURC-INJ"),]
source<-select(source, CODE, VALUE)
colnames(source)<-c("SOURCE", "VALUE")
str(source)
tidyaccid<-left_join(tidyaccid, source, by="SOURCE")
tidyaccid<-tidyaccid[, -c(dr("SOURCE"))] 
colnames(tidyaccid)[15]<-c("SOURCE")

environ<-acc1[(acc1$CATEGORY=="ENVIR-FAC"),]
environ<-select(environ, CODE, VALUE)
colnames(environ)<-c("ENVIRON", "VALUE")
str(environ)
tidyaccid<-left_join(tidyaccid, environ, by="ENVIRON")
tidyaccid<-tidyaccid[, -c(dr("ENVIRON"))] 
colnames(tidyaccid)[15]<-c("ENVIRON")

# Now we have now convert all codes in ACCID with ACC
# Then we can convert codes in HAZSUB with HZS

hzs<-read.dbf("lookups/hzs.dbf")
colnames(hzs)[1]<-c("HAZSUB")
str(hzs)
tidyaccid<-left_join(tidyaccid, hzs, by="HAZSUB")
ncol(tidyaccid) 
tidyaccid<-tidyaccid[, -c(dr("HAZSUB"))]
colnames(tidyaccid)[15]<-c("HAZSUB")

# Then we convert codes in OCC_CODE with OCC
occ<-read.dbf("lookups/occ.DBF")
colnames(occ)[1]<-c("OCC_CODE")
tidyaccid<-left_join(tidyaccid, occ, by="OCC_CODE")
tidyaccid<-tidyaccid[, -c(dr("OCC_CODE"))] 
# decode degree type 
tidyaccid$DEGREE<-gsub(1, "fatality", tidyaccid$DEGREE)
tidyaccid$DEGREE<-gsub(2, "hospitalized", tidyaccid$DEGREE)
tidyaccid$DEGREE<-gsub(3, "nonhospitalized", tidyaccid$DEGREE)
# decode task type
tidyaccid$TASK<-gsub(1, "regularly assigned task", tidyaccid$TASK) 
tidyaccid$TASK<-gsub(2, "task other than regularly assigned", tidyaccid$TASK)
## above ACCID finish converting
tidydata<-tidyaccid[, c(dr("ACTIVITYNO"), dr("DEGREE"), dr("BODYPART"), dr("NATURE"), dr("EVENT"), dr("HUMAN"), dr("SOURCE"), dr("ENVIRON"), dr("HAZSUB"), dr("OCCUPATION"))]
# extract some column to prepare for the final data



#########################################
osha<-read.dbf("osha.DBF")
head(osha)
fda<-read.dbf("lookups/fda.dbf")
# extract some useful column
tidyosha<-data.frame(osha$ACTIVITYNO, osha$JOBTITLE, osha$ESTABNAME, osha$OWNERCODE, osha$EMPCOUNT, osha$NATEMPCNT, osha$CLOSECASE, osha$NAICS, osha$SIC, osha$SITECITY, osha$SITECNTY)
ncol(tidyosha)
head(tidyosha)
head(fda)
# convert codes in ownercode in tidyosha with fda
colnames(fda)[1]<-c("osha.OWNERCODE")
tidyosha<-left_join(tidyosha, fda, by="osha.OWNERCODE")
head(tidyosha)
tidyosha$osha.OWNERCODE<-NULL
# convert codes in sitecity and sitecnty with scc
scc<-read.dbf("lookups/scc.dbf")
head(scc)
head(filter(scc, STATE=="MA"))
sccma<-filter(scc, STATE=="MA")
sccma$TYPE<-NULL
sccma$STATE<-NULL
sccma<-rename(sccma, osha.SITECITY=CITY, osha.SITECNTY=COUNTY)
tidyosha<-left_join(tidyosha, sccma, by="osha.SITECITY")
tidyosha$osha.SITECNTY.x<-NULL
tidyosha<-rename(tidyosha, osha.SITECNTY=osha.SITECNTY.y)
tidyosha$osha.SITECITY<-NULL
tidyosha$osha.SITECNTY<-NULL
tidyosha<-rename(tidyosha, PLACE=NAME)
# convert SIC code with sic.dbf
sic<-read.dbf("lookups/sic.dbf")
sic<-rename(sic, osha.SIC=SIC)
tidyosha<-left_join(tidyosha, sic, bu="osha.SIC")
tidyosha$osha.SIC=NULL
# convert code in osha.JOBTITLE by following guidelines in osha.txt
# A = area director C = safety officer I = health officer L = safety trainee M = health trainee
# N = national offic e management O = area office support staff P = compliance program manager R = 
# S = supervisor T = safety and health technician U = area office analyst V = discrim. invest'r
# W = regional mgt. X = regional FSO Y = regional tech. supp. Z = regional management
x<-c("A", "C", "I", "L", "M", "N", "O", "P", "S", "T", "U", "V", "W", "X", "Y", "Z")
r<-c("area director", "safety officer", "health officer", "safety trainee",
"health trainee", "national office management", "area office support staff", "compliance program manager",
"supervisor", "safety and health technician", "area office analyst", "discrim. invest'r", "regional mgt.",
"regional FSO", "regional tech. supp.", "regional management")
jobtitle<-data.frame(osha.JOBTITLE=x, JOBTITLE=r)
tidyosha<-left_join(tidyosha, jobtitle, by="osha.JOBTITLE")
tidyosha$osha.JOBTITLE<-NULL
# convert code in NAICS by following guidelines in naics.dbf
naics<-read.dbf("lookups/naics.dbf")
naics<-rename(naics, osha.NAICS=NAICS)
tidyosha<-left_join(tidyosha, naics, by="osha.NAICS")
tidyosha$osha.NAICS<-NULL
names(tidyosha) = sub("osha.","",names(tidyosha)) 
head(tidyosha)
## combine all data together
head(tidydata)
head(tidyosha)
tidydata<-left_join(tidyosha, tidydata, by="ACTIVITYNO")
## check stable information
## only two columns
index=rep(0, 2)
for(i in 1:nrow(tidydata))
{
  if(tidydata[i,3]!="0")
  {
    index[1]=1
  }
  if(tidydata[i,4]!="0")
  {
    index[2]=1
  }
}
index
## these two columns are stable information
tidydata$EMPCOUNT<-NULL
tidydata$NATEMPCNT<-NULL
## decode closecase
closecase<-data.frame(CLOSECASE=c("X", NA), CLOSE=c("Yes", "NO"))
tidydata<-left_join(tidydata, closecase, by="CLOSECASE")
tidydata$CLOSECASE<-NULL



############################################################
# violation
viol<-read.dbf("viol.DBF")
# extract some columns
tidyviol<-data.frame(viol$ACTIVITYNO, viol$EMPHASIS, viol$GRAVITY, viol$VIOLTYPE, viol$STD_LOOKUP, viol$INSTANCES, viol$REC, viol$NUMEXPOSED, viol$ABATEDONE, viol$HAZCAT)
names(tidyviol) = sub("viol.","",names(tidyviol)) 
# decode emphasis
levels(tidyviol$EMPHASIS)
tidyviol$EMPHASIS=gsub("X", "egregious", tidyviol$EMPHASIS)
# glimpse at gravity
levels(tidyviol$GRAVITY)
colnames(tidyviol)[3]<-c("GRAVITYLEVEL")
head(tidyviol)
# decode at violtype
violtype<-data.frame(VIOLTYPE=c("O", "R", "S", "U", "W"), CODE=c("other", "repeat", "serious", "unclassified", "willful"))
head(violtype)
tidyviol<-left_join(tidyviol, violtype, by="VIOLTYPE")
head(tidyviol)
tidyviol$VIOLTYPE<-NULL
tidyviol<-rename(tidyviol, VIOLTYPE=CODE)
head(tidyviol)
# decode for STD_LOOKUP
std<-read.dbf("lookups/STD.dbf")
colnames(std)[2]<-c("STD_LOOKUP")
tidyviol<-left_join(tidyviol, std, by="STD_LOOKUP")
tidyviol$STD_LOOKUP<-NULL
tidyviol<-rename(tidyviol, STD_LOOKUP=TEXT)
head(tidyviol)
# decode for related event
rec<-data.frame(REC=c("A", "C", "I", "R", "V"), RELATEDEVENT=c("FAT/CAT (fatality/catastrophe), accident", "complaint", "imminent danger", "related event code", "variance"))
tidyviol<-left_join(tidyviol, rec, by="REC")
head(tidyviol)
tidyviol$REC<-NULL
tidyviol$ABATEDONE<-gsub("X", "abatement, PPE, report completed", tidyviol$ABATEDONE)%>%
gsub("E", "abatement, PPE, plan, report not completed, employer out of business")%>%
gsub("W", "abatement, PPE, plan, report not completed, worksite changed")%>%
gsub("S", "abatement, PPE, plan, report not complete, ad discretion")%>%
gsub("N", "national indicator (older files)")%>%
gsub("I", "abatement completed immediately upon receipt of citation")%>%
gsub("Q", "quick fix (fixed during the walkaround)")%>%
gsub("A", "abatement, PPE, plan, report not completed, ad Discretion")




############################################################
# combine with tidydata above
tidydata<-left_join(tidydata, tidyviol, by="ACTIVITYNO")
head(tidydata)
ncol(tidydata)
nrow(tidydata)
# So I combine scc sic fda naics osha accid hazsub acc occ viol std


############################################################
# checking duplicated rows
head(duplicated(tidydata, incomparables = FALSE))
# remove all duplicated rows
tidydata<-distinct(tidydata)
# save them
save(tidydata, file="tidydata.Rdata")



###########################################################
load("tidydata.Rdata")
###Play with data
library(data.table)
library(ggplot2)
###########################################################
require(knitr)
require(ggthemes)
## Find the top 5 estabname and places associated with it
names(summary(tidydata$ESTABNAME))[1:5]
# Find the top 1 estabname in tidydata and which places consists it
sub<-filter(tidydata, ESTABNAME%in%names(summary(tidydata$ESTABNAME))[1:5])
g<-ggplot(sub, aes(ESTABNAME, fill=PLACE))+theme(axis.text.y=element_text(size=8))+coord_flip()+theme_economist()+scale_colour_economist()
g+geom_bar(position = "stack") + ggtitle("Top 5 companies~PLACE-MA.jpg")
png(filename="Top 5 companies consists of places in MA")
plot(g+geom_bar(position = "stack") + ggtitle("Top 5 companies~PLACE-MA"))
dev.off()
## ZOOM IN: if we only look at top 5 places in top 5 companies:
names(summary(sub$PLACE))[1:5]
subsub<-filter(sub, PLACE%in%names(summary(sub$PLACE))[1:5])
s<-ggplot(subsub, aes(PLACE), fill=..count..)+theme(axis.text=element_text(size=10))+theme_economist()+scale_colour_economist()
s+geom_bar(fill="skyblue1")+ggtitle("Top 5 most common places in top 5 companies")
png(filename="Top 5 most common places in top 5 companies")
plot(s+geom_bar(fill="skyblue1")+ggtitle("Top 5 most common places in top 5 companies"))
dev.off()
## Quincy Boston Lynn Springfield Worcester
## Let's look at a bigger picture
## How about the top 5 most common places for all companies
names(summary(tidydata$PLACE))[1:5]
sub2<-filter(tidydata, PLACE%in%names(summary(tidydata$PLACE))[1:5])
a<-ggplot(sub2, aes(PLACE), fill=..count..)+theme(axis.text=element_text(size=8))+theme_economist()+scale_colour_economist()
a+geom_bar(fill="skyblue1") + ggtitle("Top 5 most common places for all companies")
png(filename="Top 5 most common places for all companies in osha")
plot(a+geom_bar(fill="skyblue1") + ggtitle("Top 5 most common places for all companies"))
dev.off()
# Boston Worcester Springfield New Bedford Cambridge

# Boston is the most dangerous place in MA
# So let's look at which industries(jobs) are most dangerous(easy to have accidents) for MA and Boston

## Top 10 most accid prone industries in MA 
names(summary(tidydata$INDUSTRY))[1:10]
tmptmp<-filter(tidydata, INDUSTRY%in%names(summary(tidydata$INDUSTRY))[1:10])
t<-ggplot(tmptmp, aes(INDUSTRY))+theme(axis.text = element_text(size=8))+coord_flip()+theme_economist()+scale_colour_economist()
t+geom_bar(fill="skyblue3")+ggtitle("Top 10 most accident prone industries in MA")
png(filename="Top 10 most accident prone industries in MA")
plot(t+geom_bar(fill="skyblue3")+ggtitle("Top 10 most accident prone industries in MA"))
dev.off()

## Top 10 industry in Boston that is most dangerous
tmp<-filter(tidydata, PLACE=="BOSTON")
tmp<-filter(tmp, INDUSTRY%in%names(summary(tmp$INDUSTRY)[1:10]))
m<-ggplot(tmp, aes(INDUSTRY))+coord_flip()+theme_economist()+scale_colour_economist()
m+geom_bar(fill="skyblue2")+ggtitle("Top 10 most industry in Boston that are most dangerous")
png(filename="Top 10 most industry in Boston that are most dangerous")
dev.off()
# As we can see, nonresidential construction in boston and the whole MA is most dangerous

## degree(extent of injury) type for source of injury in tidyaccid faced by BODYPART
accid2<-subset(tidyaccid, TASK!=0, DEGREE!=0)
ggplot(accid2, aes(DEGREE, fill=TASK))+geom_bar()+ facet_wrap(~ BODYPART)+ theme(axis.text = element_text(size=6))+coord_flip()+ggtitle("Extent of injury for source of injury faceted by body part")+theme_economist()+scale_colour_economist()
png(filename="Extent of injury for source of injury faceted by body part")
plot(ggplot(accid2, aes(DEGREE, fill=TASK))+geom_bar()+ facet_wrap(~ BODYPART)+ theme(axis.text = element_text(size=6))+coord_flip()+ggtitle("Extent of injury for source of injury faceted by body part")+theme_economist()+scale_colour_economist())
dev.off()
# As we can see regularly assigned test causes more injury, no matter fatality, hospitalized, nonhospitalized, 
# which I didn't expect since my common sense is always that task other than regularly assigned causes injury, 
# since people are always good at what they usually do.


## Extent of injury for source of injury faceted by hazardous substance
accid3<-filter(tidyaccid, is.na(HAZSUB)==F, is.na(SOURCE)==F, is.na(DEGREE)==F)
ggplot(accid3, aes(DEGREE, fill=SOURCE))+geom_bar()+facet_wrap(~HAZSUB)+theme(axis.text = element_text(size=6))+ggtitle("Extent of injury for source of injury faceted by hazardous substance")+theme_economist()+scale_colour_economist()
png(filename="Extent of injury for source of injury faceted by hazardous substance")
plot(ggplot(accid3, aes(DEGREE, fill=SOURCE))+geom_bar()+facet_wrap(~HAZSUB)+theme(axis.text = element_text(size=6))+ggtitle("Extent of injury for source of injury faceted by hazardous substance")+theme_economist()+scale_colour_economist())
dev.off()
## As we can see, METHYLENE CHLORIDE (DICHLOROMETHANE) causes most nonhospitalized injury.


