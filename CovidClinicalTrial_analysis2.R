rm(list=ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Nour Hawila/COVID clinical trials/Second resubmission/software/clean")
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/COVID clinical trials/Second resubmission/software")

fn=c("studies_20191201.txt","studies_20201201.txt")


fn2=paste(unlist(strsplit(fn,".txt")),".RData",sep="")


for(i in 1:length(fn)){
	

d1 <- read.table(file = fn[i],
                 header = TRUE,
                 sep = "|",
                 na.strings = "",
                 comment.char = "",
                 quote = "\"",
                 stringsAsFactors=FALSE,
                 fill = FALSE)

rownames(d1)=d1$nct_id
        

	
	
############	
        d2=d1[,c("nct_id","source","study_first_submitted_date","study_first_submitted_qc_date","completion_date","study_type","completion_date_type","enrollment","enrollment_type","phase","overall_status","start_date","start_date_type","primary_completion_date_type","primary_completion_date")]
        
save(d2,file=fn2[i])

}


fn4=paste(unlist(strsplit(fn2,".RData")),"_subset.RData",sep="")

d.source=read.csv("source4.csv",header=T,na.strings=c("","NA"))
d.source[128,"Source"]="Boston Children’s Hospital"


for(i in 1:length(fn2)){
	load(fn2[i])
	d1=d2
d.source2=d.source[!is.na(d.source$Country),]

rownames(d.source2)=d.source2$Source


d2=d1[is.element(d1$source,d.source2$Source),]
d2$sector=d.source2[d2$source,c("Class")]
d2$country=d.source2[d2$source,c("Country")]
d2$cancer=d.source2[d2$source,c("Cancer")]

d.region=read.csv("continents2.csv")
rownames(d.region)=d.region$name

countries=unique(d2$country)
#countries[!is.element(countries,d.region$name)]

d2$region=d.region[d2$country,"region"]
d2$subregion=d.region[d2$country,"sub.region"]

save(d2,file=fn4[i])

}




#############################################

source("functions4.R")

load("studies_20191201_subset.RData")
d2.2019=d2

load("studies_20201201_subset.RData")
d2.2020=d2

d.covid=read.csv("20201204030641_covid-19.tsv",sep="\t",stringsAsFactors=FALSE)

#dat=mytabfun2(2020,d2.2020,Measure="Submitted",Country="United States")

#sum(is.element(dat$nct_id,d.covid$nct_id))
#mean(is.element(dat$nct_id,d.covid$nct_id))


library(patchwork)
options(scipen=999)
library(scales)


#####################################
################# Table #################
####################################

#### Table with COVID trials removed

# remove COVID trials from d2.2020
d2.2020.sub=d2.2020[!is.element(d2.2020$nct_id,d.covid$nct_id),]


## 2019

overall.2019.submitted=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Submitted")$sector,levels=c("Academic","Pharma"))))
us.2019.submitted=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Submitted",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2019.submitted=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Submitted",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2019.submitted=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Submitted",Region="Asia")$sector,levels=c("Academic","Pharma"))))


overall.2019.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Completed")$sector,levels=c("Academic","Pharma"))))
us.2019.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Completed",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2019.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Completed",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2019.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Completed",Region="Asia")$sector,levels=c("Academic","Pharma"))))

overall.2019.primary.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Primary.Completed")$sector,levels=c("Academic","Pharma"))))
us.2019.primary.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Primary.Completed",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2019.primary.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Primary.Completed",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2019.primary.completed=addmargins(table(factor(mytabfun(2019,d2.2019,Measure="Primary.Completed",Region="Asia")$sector,levels=c("Academic","Pharma"))))


## 2020

overall.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Submitted")$sector,levels=c("Academic","Pharma"))))
us.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Submitted",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Submitted",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Submitted",Region="Asia")$sector,levels=c("Academic","Pharma"))))


overall.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Completed")$sector,levels=c("Academic","Pharma"))))
us.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Completed",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Completed",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Completed",Region="Asia")$sector,levels=c("Academic","Pharma"))))


overall.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Primary.Completed")$sector,levels=c("Academic","Pharma"))))
us.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Primary.Completed",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Primary.Completed",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020.sub,Measure="Primary.Completed",Region="Asia")$sector,levels=c("Academic","Pharma"))))


tab=cbind(rbind(overall.2019.submitted,us.2019.submitted,europe.2019.submitted,asia.2019.submitted,overall.2019.completed,us.2019.completed,europe.2019.completed,asia.2019.completed
,overall.2019.primary.completed,us.2019.primary.completed,europe.2019.primary.completed,asia.2019.primary.completed),
rbind(overall.2020.submitted,us.2020.submitted,europe.2020.submitted,asia.2020.submitted,overall.2020.completed,us.2020.completed,europe.2020.completed,asia.2020.completed
,overall.2020.primary.completed,us.2020.primary.completed,europe.2020.primary.completed,asia.2020.primary.completed
))

tab2=cbind(
round((tab[,4]-tab[,1])/tab[,1]*100,1),
round((tab[,5]-tab[,2])/tab[,2]*100,1),
round((tab[,6]-tab[,3])/tab[,3]*100,1))


mytable=cbind(tab,tab2)


mytable2=mytable
mytable2[,4]=paste(mytable[,4]," (",mytable[,7],"%)",sep="")
mytable2[,5]=paste(mytable[,5]," (",mytable[,8],"%)",sep="")
mytable2[,6]=paste(mytable[,6]," (",mytable[,9],"%)",sep="")

write.csv(mytable2,file="new-table1-covid-removed.csv")


################## Table without COVID trials removed

## 2020

overall.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Submitted")$sector,levels=c("Academic","Pharma"))))
us.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Submitted",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Submitted",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2020.submitted=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Submitted",Region="Asia")$sector,levels=c("Academic","Pharma"))))


overall.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Completed")$sector,levels=c("Academic","Pharma"))))
us.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Completed",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Completed",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2020.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Completed",Region="Asia")$sector,levels=c("Academic","Pharma"))))


overall.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Primary.Completed")$sector,levels=c("Academic","Pharma"))))
us.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Primary.Completed",Country="United States")$sector,levels=c("Academic","Pharma"))))
europe.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Primary.Completed",Region="Europe")$sector,levels=c("Academic","Pharma"))))
asia.2020.primary.completed=addmargins(table(factor(mytabfun(2020,d2.2020,Measure="Primary.Completed",Region="Asia")$sector,levels=c("Academic","Pharma"))))


tab=cbind(rbind(overall.2019.submitted,us.2019.submitted,europe.2019.submitted,asia.2019.submitted,overall.2019.completed,us.2019.completed,europe.2019.completed,asia.2019.completed
,overall.2019.primary.completed,us.2019.primary.completed,europe.2019.primary.completed,asia.2019.primary.completed),
rbind(overall.2020.submitted,us.2020.submitted,europe.2020.submitted,asia.2020.submitted,overall.2020.completed,us.2020.completed,europe.2020.completed,asia.2020.completed
,overall.2020.primary.completed,us.2020.primary.completed,europe.2020.primary.completed,asia.2020.primary.completed
))



tab2=cbind(
round((tab[,4]-tab[,1])/tab[,1]*100,1),
round((tab[,5]-tab[,2])/tab[,2]*100,1),
round((tab[,6]-tab[,3])/tab[,3]*100,1))


mytable=cbind(tab,tab2)


mytable2=mytable
mytable2[,4]=paste(mytable[,4]," (",mytable[,7],"%)",sep="")
mytable2[,5]=paste(mytable[,5]," (",mytable[,8],"%)",sep="")
mytable2[,6]=paste(mytable[,6]," (",mytable[,9],"%)",sep="")

write.csv(mytable2,file="new-table1-covid-not-removed.csv")




##########################################################
##########################################################


###################################
##########################################
##########################################

Measure="Submitted"
Type="Interventional"
Country="United States"
#Sector="Academic"
Sector=NULL

dat=mydatfun2(Data=d2.2020,Measure=Measure,Type=Type,Country=Country,Sector=Sector,Year=2020)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

mycols=gg_color_hue(2)[2:1]

mymonths=seq(as.Date("2017-03-01"), as.Date("2020-10-01"), "months")
library(lubridate)

#as.Date(c("2017-03-01","2017-04-01","2018-03-01","2019-03-01","2020-03-01","2020-11-01"))
#c("Mar 2017","Apr 2017","Mar 2018","Mar 2019","Mar 2020","Nov 2020")



pdf(file="gr1.pdf",width=8.5,height=4)
par(mar=c(4,3,1,1))
plot(dat$date.month,dat$value2,type="n",xlab="",ylab="",xaxt="n")
points(dat$date.month,dat$value2,pch=16,col=mycols[dat$postcovid+1],cex=1.3)
points(dat$date.month,dat$value2.seasonal,pch=3,col="magenta",cex=1,type="b")
#mtext("Month",side=1,line=2)
mtext("log(Num Submitted Trials)",side=2,line=2)
abline(v=as.Date("2020-03-01"),col="goldenrod",lwd=6,lty=1)
axis(1,at=mymonths,labels=as.character(month(mymonths,label=T)),las=3)
axis(1,at=as.Date(c("2017-06-01","2018-06-01","2019-06-01","2020-06-01")),labels=c("2017","2018","2019","2020"),line=2,tick=FALSE)

dev.off()

########################################
######################################

Measure="Submitted"
Type="Interventional"
#Country="United States"
Country=NULL
Region="Asia"
Region=NULL
#Sector="Academic"
Sector=NULL
seasonal.correction=TRUE

dat=mydatfun2(Data=d2.2020,Measure=Measure,Type=Type,Country=Country,Sector=Sector,Year=2020,seasonal.correction=seasonal.correction,Region=Region)

dat.2019=mydatfun2(Data=d2.2019,Measure=Measure,Type=Type,Country=Country,Sector=Sector,Year=2019,seasonal.correction=seasonal.correction,Region=Region)

dat.diff=dat
dat.diff$value3=dat.diff$value3-dat.2019$value3


pdf("gr2.pdf",width=9,height=3)
ylim=NULL
#ylim=c(-.3,.3)
gr1=mygraphfun(Data=dat,Year=2020,ylim=ylim) + ggtitle("2020 Data") 
gr2=mygraphfun(Data=dat.2019,Year=2019,ylim=ylim)+ ggtitle("2019 Data") 
gr3=mygraphfun(Data=dat.diff,Year=2020,ylim=ylim)+ ggtitle("Difference") 

gr1+gr2+gr3 + theme(plot.margin = margin(0, 0, 0, 0))

dev.off()

######################################################
################## gr3new ####################################
######################################################



myMeasure=c("Submitted","Completed","Primary.Completed")
#myCountry=c("United States","France","China","All")
myRegion=c("Europe","Asia","All")
Type="Interventional"
seasonal.correction=TRUE

info=array(NA,dim=c(length(myMeasure)*4,3))

counter=1

mylist=list(NA)
for(var.measure in myMeasure){
#	for(var.country in myCountry){
	for(var.region in myRegion){

#var.country2=var.country
#if(var.country=="All"){var.country=NULL}

var.region2=var.region
if(var.region=="All"){var.region=NULL}
		#dat=mydatfun2(Data=d2.2020,Measure=var.measure,Type=Type,Country=var.country,Year=2020,seasonal.correction=seasonal.correction)

#dat.2019=mydatfun2(Data=d2.2019,Measure=var.measure,Type=Type,Country=var.country,Year=2019,seasonal.correction=seasonal.correction)

dat=mydatfun2(Data=d2.2020,Measure=var.measure,Type=Type,Region=var.region,Year=2020,seasonal.correction=seasonal.correction)

dat.2019=mydatfun2(Data=d2.2019,Measure=var.measure,Type=Type,Region=var.region,Year=2019,seasonal.correction=seasonal.correction)


dat$value.2019=dat.2019$value
dat$value3.2019=dat.2019$value3
dat2=dat
dat2$value3=dat2$value3-dat2$value3.2019

#fit=lm(value3~date.num2 + postcovid:date.num2 ,data=dat2)
#pval=signif(summary(fit)$coef[3,4],3)

fit1=lm(value3~date.num2*postcovid ,data=dat2)
fit2=lm(value3~date.num2,data=dat2)
#pval=format.pval(anova(fit1,fit2)$Pr[2],eps=.0001)
pval=scales::pvalue(anova(fit1,fit2)$Pr[2],accuracy=.0001,decimal.mark=".",add_p=T)

mylist[[counter]]=mygraphfun2(Data=dat2,Year=2020) + #ggtitle(paste("p-value=",pval,sep="")) 
ggtitle(pval) 
#info[counter,]=c(counter,var.measure,var.country2)
info[counter,]=c(counter,var.measure,var.region2)

counter=counter+1
print(counter)
flush.console()
	}}


dat=mydatfun2(Data=d2.2020,Measure="Submitted",Type=Type,Country="United States",Year=2020,seasonal.correction=seasonal.correction)

dat.2019=mydatfun2(Data=d2.2019,Measure="Submitted",Type=Type,Country="United States",Year=2019,seasonal.correction=seasonal.correction)


dat$value.2019=dat.2019$value
dat$value3.2019=dat.2019$value3
dat2=dat
dat2$value3=dat2$value3-dat2$value3.2019

#fit=lm(value3~date.num2 + postcovid:date.num2 ,data=dat2)
#pval=signif(summary(fit)$coef[3,4],3)

fit1=lm(value3~date.num2*postcovid ,data=dat2)
fit2=lm(value3~date.num2,data=dat2)
#pval=format.pval(anova(fit1,fit2)$Pr[2],eps=.0001)
pval=scales::pvalue(anova(fit1,fit2)$Pr[2],accuracy=.0001,decimal.mark=".",add_p=T)


mylist[[length(myMeasure)*3+1]]=mygraphfun2(Data=dat2,Year=2020) + ggtitle(pval) 


dat=mydatfun2(Data=d2.2020,Measure="Completed",Type=Type,Country="United States",Year=2020,seasonal.correction=seasonal.correction)

dat.2019=mydatfun2(Data=d2.2019,Measure="Completed",Type=Type,Country="United States",Year=2019,seasonal.correction=seasonal.correction)


dat$value.2019=dat.2019$value
dat$value3.2019=dat.2019$value3
dat2=dat
dat2$value3=dat2$value3-dat2$value3.2019

#fit=lm(value3~date.num2 + postcovid:date.num2 ,data=dat2)
#pval=signif(summary(fit)$coef[3,4],3)

fit1=lm(value3~date.num2*postcovid ,data=dat2)
fit2=lm(value3~date.num2,data=dat2)
#pval=format.pval(anova(fit1,fit2)$Pr[2],eps=.0001)
pval=scales::pvalue(anova(fit1,fit2)$Pr[2],accuracy=.0001,decimal.mark=".",add_p=T)


mylist[[length(myMeasure)*3+2]]=mygraphfun2(Data=dat2,Year=2020) + ggtitle(pval) 


##########


dat=mydatfun2(Data=d2.2020,Measure="Primary.Completed",Type=Type,Country="United States",Year=2020,seasonal.correction=seasonal.correction)

dat.2019=mydatfun2(Data=d2.2019,Measure="Primary.Completed",Type=Type,Country="United States",Year=2019,seasonal.correction=seasonal.correction)


dat$value.2019=dat.2019$value
dat$value3.2019=dat.2019$value3
dat2=dat
dat2$value3=dat2$value3-dat2$value3.2019

#fit=lm(value3~date.num2 + postcovid:date.num2 ,data=dat2)
#pval=signif(summary(fit)$coef[3,4],3)

fit1=lm(value3~date.num2*postcovid ,data=dat2)
fit2=lm(value3~date.num2,data=dat2)
#pval=format.pval(anova(fit1,fit2)$Pr[2],eps=.0001)
pval=scales::pvalue(anova(fit1,fit2)$Pr[2],accuracy=.0001,decimal.mark=".",add_p=T)


mylist[[length(myMeasure)*3+3]]=mygraphfun2(Data=dat2,Year=2020) + ggtitle(pval) 

###########





pdf("gr3new.pdf",width=12,height=9)	
((mylist[[length(myMeasure)*3+1]] | mylist[[1]] | mylist[[2]] | mylist[[3]])/(mylist[[length(myMeasure)*3+2]] | mylist[[4]] | mylist[[5]] | mylist[[6]])/(mylist[[length(myMeasure)*3+3]] | mylist[[7]] | mylist[[8]] | mylist[[9]]))
dev.off()






#######################
#########gr4new###########
#######################


myMeasure=c("Submitted","Completed","Primary.Completed")
mySector=c("Academic","Pharma")
#myRegion=c("Americas","Europe","Asia","All")
Type="Interventional"
seasonal.correction=TRUE

info=array(NA,dim=c(length(myMeasure)*length(mySector),3))

counter=1

mylist=list(NA)
for(var.measure in myMeasure){
	for(var.sector in mySector){

		dat=mydatfun2(Year=2020,Data=d2.2020,Measure=var.measure,Type=Type,Sector=var.sector,seasonal.correction=seasonal.correction)

dat.2019=mydatfun2(Year=2019,Data=d2.2019,Measure=var.measure,Type=Type,Sector=var.sector,seasonal.correction=seasonal.correction)

dat$value.2019=dat.2019$value
dat$value3.2019=dat.2019$value3
dat2=dat
dat2$value3=dat2$value3-dat2$value3.2019

#fit=lm(value3~date.num2 + postcovid:date.num2 ,data=dat2)
#pval=signif(summary(fit)$coef[3,4],3)

fit1=lm(value3~date.num2*postcovid ,data=dat2)
fit2=lm(value3~date.num2,data=dat2)
#pval=format.pval(anova(fit1,fit2)$Pr[2],eps=.0001)
pval=scales::pvalue(anova(fit1,fit2)$Pr[2],accuracy=.0001,decimal.mark=".",add_p=T)

mylist[[counter]]=mygraphfun2(Data=dat2,Year=2020) + ggtitle(pval) 
info[counter,]=c(counter,var.measure,var.sector)

counter=counter+1
print(counter)
flush.console()
	}}

pdf("gr4new.pdf",width=12,height=9)	
((mylist[[1]] | mylist[[3]] | mylist[[5]])/(mylist[[2]] | mylist[[4]] | mylist[[6]]))
dev.off()




########### World Map #################

Year=2019
Data=d2.2019
start.date1=as.Date(paste(Year,"04-01",sep="-"))
end.date1=as.Date(paste(Year,"11-01",sep="-"))

dat.submitted=data.frame(date=as.Date(Data$study_first_submitted_date),type=Data$study_type,country=Data$country) %>% filter((date>=start.date1 & date<end.date1) & type=="Interventional")

dat.completed=data.frame(date=as.Date(Data$completion_date),country=Data$country,completion_date_type=Data$completion_date_type,type=Data$study_type) %>% filter((date>=start.date1 & date<end.date1) & completion_date_type=="Actual" & type=="Interventional")


mycountries.submitted=names(which(sort(table(dat.submitted$country),decreasing=T)>=5))

mycountries.completed=names(which((sort(table(dat.completed$country),decreasing=T))>=5))

res.submitted=array(NA,dim=c(length(mycountries.submitted),2))

for(i in 1:length(mycountries.submitted)){

dat=mytabfun(2020,d2.2020,Measure="Submitted",Country=mycountries.submitted[i])
res.submitted[i,1]=ifelse(is.null(dim(dat)),0,dim(dat)[1])

dat=mytabfun(2019,d2.2019,Measure="Submitted",Country=mycountries.submitted[i])
res.submitted[i,2]=ifelse(is.null(dim(dat)),0,dim(dat)[1])

print(i)
flush.console()
}

rownames(res.submitted)=mycountries.submitted

res.completed=array(NA,dim=c(length(mycountries.completed),2))

for(i in 1:length(mycountries.completed)){


dat=mytabfun(2020,d2.2020,Measure="Completed",Country=mycountries.completed[i])
res.completed[i,1]=ifelse(is.null(dim(dat)),0,dim(dat)[1])

dat=mytabfun(2019,d2.2019,Measure="Completed",Country=mycountries.completed[i])
res.completed[i,2]=ifelse(is.null(dim(dat)),0,dim(dat)[1])

print(i)
flush.console()
}

rownames(res.completed)=mycountries.completed


#save(res.submitted,res.completed,file="world_map_data.RData")

#load("world_map_data.RData")

library(maps) #map_data
library(dplyr)
library(scales)
library(viridis)
world_map <- map_data("world")

#grep("Cyprus",world_map$subregion,value=T)
#grep("Cyprus",world_map$region,value=T)

world_map$region[world_map$region=="UK"]="United Kingdom"
world_map$region[world_map$region=="USA"]="United States"
world_map$region[world_map$region=="Guinea-Bissau"]="Guinea Bissau"

unique(d2.2020[!is.element(d2.2020$country,world_map$region),"country"])
unique(d2.2019[!is.element(d2.2019$country,world_map$region),"country"])


df.submitted=data.frame(res.submitted)
colnames(df.submitted)=c("Submitted.2020","Submitted.2019")
df.submitted$sub.tot=rescale(log(df.submitted$Submitted.2020+df.submitted$Submitted.2019),to=c(0,1))
df.submitted$sub.pd=(df.submitted$Submitted.2020-df.submitted$Submitted.2019)/(df.submitted$Submitted.2019)*100

df.submitted$sub.pd.cut=cut(df.submitted$sub.pd,breaks=c(-100,-30,-15,0,15,30,100),right=F)
min(df.submitted$sub.pd)
max(df.submitted$sub.pd)
df.submitted$region=rownames(res.submitted)
df.submitted.map=right_join(df.submitted,world_map, by = "region")

rownames(df.submitted)[!is.element(rownames(df.submitted),unique(world_map$region))]



df.completed=data.frame(res.completed)
colnames(df.completed)=c("Completed.2020","Completed.2019")
df.completed$com.tot=rescale(log(df.completed$Completed.2020+df.completed$Completed.2019),to=c(0,1))
df.completed$com.pd=(df.completed$Completed.2020-df.completed$Completed.2019)/(df.completed$Completed.2019)*100

df.completed$com.pd.cut=cut(df.completed$com.pd,breaks=c(-100,-30,-15,0,15,30,100),right=F)

#df.completed$com.pd.cut=cut(df.completed$com.pd,breaks=c(-100,-50,-40,-30,-20,-10,0,10,20,100),right=F)

table(df.completed$com.pd.cut)

min(df.completed$com.pd)
max(df.completed$com.pd)
df.completed$region=rownames(res.completed)
df.completed.map=right_join(df.completed,world_map, by = "region")

rownames(df.completed)[!is.element(rownames(df.completed),unique(world_map$region))]





dev.new(width=7, height=4, unit="in")

theme_set(theme_void())
ggplot(df.submitted.map, aes(long, lat, group = group,alpha=sub.tot))+
  geom_polygon(aes(fill = sub.pd.cut), color = "gray",size = 0.05)  + scale_fill_brewer(palette = "RdBu",name = "% Change",na.value="gray97",labels=c("< -30%","-30% to -15%","-15% to 0%","0% to 15%","15% to 30%","> 30%","NA"))  + guides(alpha = F) +scale_alpha(range=c(1,1))+ theme(text=element_text(size=13,family="Comic Sans MS"),plot.margin = unit(c(0, 0, 0, -1), "cm"),plot.title = element_text(hjust = 0.5,face = "bold")) + ggtitle("Submitted Interventional Trials")


ggsave("fig5.png",type="cairo",dpi=600)
 

dev.new(width=7, height=4, unit="in")

#mycol=rgb(0,1,0,.1)
mycol="white"

theme_set(theme_void())
ggplot(df.completed.map, aes(long, lat, group = group,alpha=com.tot))+
  geom_polygon(aes(fill = com.pd.cut), color = "gray",size = 0.05)  + scale_fill_brewer(palette = "RdBu",name = "% Change",na.value="gray97",labels=c("< -30%","-30% to -15%","-15% to 0%","0% to 15%","15% to 30%","> 30%","NA"))  + guides(alpha = F) +scale_alpha(range=c(1,1))+ theme(text=element_text(size=13,family="Comic Sans MS"),plot.margin = unit(c(0, 0, 0, -1), "cm"),plot.title = element_text(hjust = 0.5,face = "bold")) + ggtitle("Completed Interventional Trials")


ggsave("fig6.png",type="cairo",dpi=600)
 



ord1=order(df.submitted$sub.pd)
df1=df.submitted[ord1,]

ord2=order(df.completed$com.pd)
df2=df.completed[ord2,]



################################
####### MODELING ###############
################################


dat=mytabfun2(2020,d2.2020,Measure="Submitted",Country="United States")

sum(is.element(dat$nct_id,d.covid$nct_id))
mean(is.element(dat$nct_id,d.covid$nct_id))


###################################



Measure="Submitted"
Type="Interventional"
Sector="Academic"
Region="Americas"
Data=d2
Year=2020
Subregion=NULL
Country=NULL


mydat=mydatfun2(Data=d2,Measure=Measure,Type=Type,Sector=Sector,Region=Region,Year=Year)


count=1
myMeasure=c("Submitted","Completed","Primary.Completed")
Type=c("Interventional")
mySector=c("Academic", "Pharma")
myRegion=c("Americas", "Europe", "Asia")
Country=NULL

for(var.measure in myMeasure){
#for(var.type in myType){
for(var.sector in mySector){
for(var.region in myRegion){

if(var.region!="Americas"){Country=NULL}

if(var.region=="Americas"){
	Country="United States"
	var.region=NULL}

	
dat=mydatfun2(Data=d2.2020,Measure=var.measure,Type=Type,Region=var.region,Sector=var.sector,Year=2020,Country=Country)

dat.2019=mydatfun2(Data=d2.2019,Measure=var.measure,Type=Type,Region=var.region,Sector=var.sector,Year=2019,Country=Country)


if(is.null(var.region)){var.region="United States"}
		
if(!is.null(dim(dat)) & !is.null(dim(dat.2019))){
dat$measure=var.measure
dat$sector=var.sector
dat$region=var.region
dat$group=count
dat$value.2019=dat.2019$value
dat$value3.2019=dat.2019$value3

}

if(is.null(dim(dat)) | is.null(dim(dat.2019))){
dat=array(NA,dim=c(43,dim(mydat)[2]))
colnames(dat)=colnames(mydat)
dat=as.data.frame(dat)

dat$measure=var.measure
dat$type=var.type
dat$sector=var.sector
dat$region=var.region
dat$group=count

dat$value.2019=NA
dat$value3.2019=NA

}


if(count==1){mega=dat}
if(count>1){mega=bind_rows(mega,dat)}


count=count+1
print(count-1)
flush.console()

}}}

mega2=mega[!is.na(mega$value3),]
mega2$region=relevel(factor(mega2$region),ref="United States")
mega2$sector=relevel(factor(mega2$sector),ref="Pharma")

mega_region=mega2

#save(mega_region,file="mega_region_new.RData")

#load("mega_region_new.RData")


library(gtsummary)
library(gt)

dat = mega_region %>% filter(measure=="Submitted")

fit.submitted=lm(value3~value3.2019+scale(date.num2)+postcovid*sector+region,data=dat) %>% tbl_regression() %>% bold_labels() %>% bold_p()

dat = mega_region %>% filter(measure=="Completed")

fit.completed=lm(value3~value3.2019+scale(date.num2)+postcovid*sector+region,data=dat) %>% tbl_regression() %>% bold_labels() %>% bold_p()

dat = mega_region %>% filter(measure=="Primary.Completed")

fit.primary.completed=lm(value3~value3.2019+scale(date.num2)+postcovid*sector+region,data=dat) %>% tbl_regression() %>% bold_labels() %>% bold_p()


tbl=tbl_merge(tbls=list(fit.submitted,fit.completed,fit.primary.completed),tab_spanner=c("**Submitted**","**Completed**","**Primary.Completed**"))

tbl %>% as_gt() %>% gtsave(filename="newmodel.rtf")

####################
########################################
########################################

Data=d2.2019
Measure="Primary.Completed"
Type="Interventional"
Sector=NULL
Region=NULL
Subregion=NULL
Country=NULL

mydate=as.Date("2019-11-01")

dt1=data.frame(date=as.Date(Data$primary_completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$primary_completion_date_type) %>% filter(completion_date_type=="Anticipated") %>% arrange(date) %>% filter((date>mydate ))



dt2=dt1
if(!is.null(Country)){dt2=(dt2 %>% filter(country %in% Country))}	
if(!is.null(Region)){dt2=(dt2 %>% filter(region==Region))}
if(!is.null(Subregion)){dt2=(dt2 %>% filter(subregion==Subregion))}
if(!is.null(Type)){dt2=(dt2 %>% filter(type==Type))}
if(!is.null(Sector)){dt2=(dt2 %>% filter(sector==Sector))}

res.2019=log(as.numeric((dt2$date-as.Date("2019-11-01"))))

######### 2020

Data=d2.2020
Measure="Primary.Completed"
Type="Interventional"
Sector=NULL
Region=NULL
Subregion=NULL
Country=NULL

mydate=as.Date("2020-11-01")

dt1=data.frame(date=as.Date(Data$primary_completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$primary_completion_date_type) %>% filter(completion_date_type=="Anticipated") %>% arrange(date) %>% filter((date>mydate ))



dt2=dt1
if(!is.null(Country)){dt2=(dt2 %>% filter(country %in% Country))}	
if(!is.null(Region)){dt2=(dt2 %>% filter(region==Region))}
if(!is.null(Subregion)){dt2=(dt2 %>% filter(subregion==Subregion))}
if(!is.null(Type)){dt2=(dt2 %>% filter(type==Type))}
if(!is.null(Sector)){dt2=(dt2 %>% filter(sector==Sector))}


res.2020=log(as.numeric((dt2$date-as.Date("2020-11-01"))))
df=data.frame(days=c(res.2019,res.2020),year=c(rep(2019,length(res.2019)),rep(2020,length(res.2020))))

boxplot(df$days~df$year)

wilcox.test(df$days~df$year)

