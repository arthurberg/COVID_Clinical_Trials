library(readr)        
library(plyr)
library(chron)
library(ggplot2)
library(dplyr)

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

library(forecast)



mydatfun2=function(Data,Measure,Type,Sector=NULL,Region=NULL,Subregion=NULL,Country=NULL,Year=NULL,seasonal.correction=TRUE){

if(!is.null(Year)){
start.date1=as.Date(paste(Year-3,"03-01",sep="-"))
end.date1=as.Date(paste(Year,"03-01",sep="-"))

start.date2=as.Date(paste(Year,"04-01",sep="-"))
end.date2=as.Date(paste(Year,"11-01",sep="-"))

covid.date=as.Date(paste(Year,"03-20",sep="-"))
}

if(is.null(Year)){
start.date1=as.Date("2017-03-01")
end.date1=as.Date("2020-03-01")

start.date2=as.Date("2020-04-01")
end.date2=as.Date("2020-11-01")

covid.date=as.Date("2020-03-20")
}

if(Measure=="Submitted"){
dt1=data.frame(date=as.Date(Data$study_first_submitted_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country) %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1) | (date>=start.date2 & date<end.date2))
}

if(Measure=="Submitted_qc"){
dt1=data.frame(date=as.Date(Data$study_first_submitted_qc_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country) %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1) | (date>=start.date2 & date<end.date2))
}

if(Measure=="Completed"){
dt1=data.frame(date=as.Date(Data$completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$completion_date_type) %>% filter(completion_date_type=="Actual") %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1) | (date>=start.date2 & date<end.date2))	
}	

if(Measure=="Primary.Completed"){
dt1=data.frame(date=as.Date(Data$primary_completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$primary_completion_date_type) %>% filter(completion_date_type=="Actual") %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1) | (date>=start.date2 & date<end.date2))	
}



dt2=dt1
if(!is.null(Country)){dt2=(dt2 %>% filter(country %in% Country))}	
if(!is.null(Region)){dt2=(dt2 %>% filter(region==Region))}
if(!is.null(Subregion)){dt2=(dt2 %>% filter(subregion==Subregion))}
if(!is.null(Type)){dt2=(dt2 %>% filter(type==Type))}
if(!is.null(Sector)){dt2=(dt2 %>% filter(sector==Sector))}

if(dim(dt2)[1]==0){return(NA);break}

dt2$date.month=as.Date(as.character(cut(dt2$date,breaks="month")))

dt3=dt2[,c("date.month","value")] %>% group_by(date.month) %>% summarize_if(is.numeric,sum)
dt3=as.data.frame(dt3)

if(dim(dt3)[1]<43){return(NA); break}

dt3$postcovid=dt3$date.month>covid.date


dt3$value2=log(dt3$value+1)
ts.precovid=ts(dt3$value2[!dt3$postcovid], frequency = 12, start = c(year(start.date1), 3))

if(seasonal.correction==TRUE){
fit=Arima(ts.precovid,xreg=fourier(ts.precovid,K=4))
fit2=forecast(fit,xreg=fourier(ts.precovid,K=4,h=8))

ts.correction=ts(c(fit2$fitted,fit2$mean),start=start(fit2$fitted),frequency=frequency(fit2$fitted))
}


if(seasonal.correction==FALSE){

#ts.correction=ts(c(dt3$value2[1:36],NA,dt3$value2[37:43]),start=c(year(start.date1), 3),frequency=12)
ts.correction=ts(rep(0,44),start=c(year(start.date1), 3),frequency=12)
}


dt3$value2.seasonal=as.numeric(ts.correction)[-37]

dt3$value3=dt3$value2-dt3$value2.seasonal

dt3$date.num=as.numeric(dt3$date.month)
dt3$date.num2=dt3$date.num-as.numeric(covid.date)

return(dt3)
}


#######################
########################


mytabfun=function(Year,Data,Measure,Type="Interventional",Sector=NULL,Region=NULL,Subregion=NULL,Country=NULL){

start.date1=as.Date(paste(Year,"04-01",sep="-"))
end.date1=as.Date(paste(Year,"11-01",sep="-"))


if(Measure=="Submitted"){
dt1=data.frame(date=as.Date(Data$study_first_submitted_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country) %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1))
}

if(Measure=="Completed"){
dt1=data.frame(date=as.Date(Data$completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$completion_date_type) %>% filter(completion_date_type=="Actual") %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1))	
}	


if(Measure=="Primary.Completed"){
dt1=data.frame(date=as.Date(Data$primary_completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$primary_completion_date_type) %>% filter(completion_date_type=="Actual") %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1))	
}	



dt2=dt1
if(!is.null(Country)){dt2=(dt2 %>% filter(country %in% Country))}	
if(!is.null(Region)){dt2=(dt2 %>% filter(region==Region))}
if(!is.null(Subregion)){dt2=(dt2 %>% filter(subregion==Subregion))}
if(!is.null(Type)){dt2=(dt2 %>% filter(type==Type))}
if(!is.null(Sector)){dt2=(dt2 %>% filter(sector==Sector))}

if(dim(dt2)[1]==0){return(NA);break}

return(dt2)
}




mytabfun2=function(Year,Data,Measure,Type="Interventional",Sector=NULL,Region=NULL,Subregion=NULL,Country=NULL){

start.date1=as.Date(paste(Year,"04-01",sep="-"))
end.date1=as.Date(paste(Year,"11-01",sep="-"))


if(Measure=="Submitted"){
dt1=data.frame(nct_id=Data$nct_id,date=as.Date(Data$study_first_submitted_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country) %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1))
}

if(Measure=="Completed"){
dt1=data.frame(nct_id=Data$nct_id,date=as.Date(Data$completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$completion_date_type) %>% filter(completion_date_type=="Actual") %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1))	
}	


if(Measure=="Primary.Completed"){
dt1=data.frame(date=as.Date(Data$primary_completion_date),value=1,type=Data$study_type,sector=Data$sector,region=Data$region,subregion=Data$subregion,country=Data$country,completion_date_type=Data$primary_completion_date_type) %>% filter(completion_date_type=="Actual") %>% arrange(date) %>% filter((date>=start.date1 & date<end.date1))	
}	

dt2=dt1
if(!is.null(Country)){dt2=(dt2 %>% filter(country %in% Country))}	
if(!is.null(Region)){dt2=(dt2 %>% filter(region==Region))}
if(!is.null(Subregion)){dt2=(dt2 %>% filter(subregion==Subregion))}
if(!is.null(Type)){dt2=(dt2 %>% filter(type==Type))}
if(!is.null(Sector)){dt2=(dt2 %>% filter(sector==Sector))}

if(dim(dt2)[1]==0){return(NA);break}

return(dt2)
}





######################







gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}




mygraphfun=function(Data,Year,ylim=NULL){
	fit=lm(value3~date.num2 + postcovid:date.num2 ,data=Data)
	mat=predict(fit,newdata=Data,interval="confidence")
	myspline1=as.data.frame(spline(Data$date.num, mat[,"lwr"]))
	myspline2=as.data.frame(spline(Data$date.num, mat[,"upr"]))
	myspline3=as.data.frame(spline(Data$date.num, mat[,"fit"]))
	myspline=data.frame(x=as.Date(myspline1$x), lwr=myspline1$y, upr=myspline2$y, value3=myspline3$y)


if(!is.null(Year)){
start.date1=as.Date(paste(Year-3,"03-01",sep="-"))
end.date1=as.Date(paste(Year,"03-01",sep="-"))

start.date2=as.Date(paste(Year,"04-01",sep="-"))
end.date2=as.Date(paste(Year,"10-01",sep="-"))

covid.date=as.Date(paste(Year,"03-20",sep="-"))

	
	x1=as.Date(paste(Year-3,"03-01",sep="-"))
	x2=as.Date(paste(Year,"03-01",sep="-"))
	x3=as.Date(paste(Year,"03-30",sep="-"))
	x4=as.Date(paste(Year,"10-01",sep="-"))

covid.num = as.numeric(as.Date(paste(Year,"03-20",sep="-")))

}

if(is.null(Year)){
	
	x1=as.Date("2016-03-01")
	x2=as.Date("2020-03-01")
	x3=as.Date("2020-03-31")
	x4=as.Date("2020-10-01")

covid.num = as.numeric(as.Date("2020-03-20"))

}

	y1=fit$coef[1]+fit$coef[2]*(as.numeric(x1)-covid.num)
	y2=fit$coef[1]+fit$coef[2]*(as.numeric(x2)-covid.num)
	y3=fit$coef[1]+fit$coef[2]*(as.numeric(x3)-covid.num) + fit$coef[3]*(as.numeric(x3)-covid.num)
	y4=fit$coef[1]+fit$coef[2]*(as.numeric(x4)-covid.num)+ fit$coef[3]*(as.numeric(x4)-covid.num)

lines_df1=data.frame(x=c(x1,x2),y=c(y1,y2))
lines_df2=data.frame(x=c(x3,x4),y=c(y3,y4))

mycols=gg_color_hue(2)[2:1]

gr=ggplot(data=Data, aes(x=date.month, y=value3) ) +  geom_point(aes(color=postcovid))+scale_color_manual(values=mycols) + xlab("Year") + ylab("") + geom_line(aes(x=x,y=y),data=lines_df1, linetype="solid",color=mycols[1],size=1.5,lineend="round")+geom_line(aes(x=x,y=y),data=lines_df2,linetype="solid",color=mycols[2],size=1.5,lineend="round")+geom_line(data=myspline[myspline$x<=x2,],aes(x=as.Date(x),y=lwr),color=mycols[1])+geom_line(data=myspline[myspline$x<=x2,],aes(x=as.Date(x),y=upr),color=mycols[1])+geom_ribbon(data=myspline[myspline$x<=x2,],aes(x=x,ymin=lwr,ymax=upr),fill=mycols[1],alpha=.1)+geom_line(data=myspline[myspline$x>=x3,],aes(x=as.Date(x),y=lwr),color=mycols[2])+geom_line(data=myspline[myspline$x>=x3,],aes(x=as.Date(x),y=upr),color=mycols[2])+geom_ribbon(data=myspline[myspline$x>=x3,],aes(x=x,ymin=lwr,ymax=upr),fill=mycols[2],alpha=.1) + theme_minimal()+theme(text = element_text(size = 14),legend.position = "none")

if(!is.null(ylim)){gr=gr+ylim(ylim)}

gr
}











mygraphfun2=function(Data,Year,ylim=NULL){
	fit=lm(value3~date.num2 + postcovid:date.num2 ,data=Data)
	mat=predict(fit,newdata=Data,interval="confidence")
	myspline1=as.data.frame(spline(Data$date.num, mat[,"lwr"]))
	myspline2=as.data.frame(spline(Data$date.num, mat[,"upr"]))
	myspline3=as.data.frame(spline(Data$date.num, mat[,"fit"]))
	myspline=data.frame(x=as.Date(myspline1$x), lwr=myspline1$y, upr=myspline2$y, value3=myspline3$y)


if(!is.null(Year)){
start.date1=as.Date(paste(Year-3,"03-01",sep="-"))
end.date1=as.Date(paste(Year,"03-01",sep="-"))

start.date2=as.Date(paste(Year,"04-01",sep="-"))
end.date2=as.Date(paste(Year,"10-01",sep="-"))

covid.date=as.Date(paste(Year,"03-20",sep="-"))

	
	x1=as.Date(paste(Year-3,"03-01",sep="-"))
	x2=as.Date(paste(Year,"03-01",sep="-"))
	x3=as.Date(paste(Year,"03-30",sep="-"))
	x4=as.Date(paste(Year,"10-01",sep="-"))

covid.num = as.numeric(as.Date(paste(Year,"03-20",sep="-")))

}

if(is.null(Year)){
	
	x1=as.Date("2016-03-01")
	x2=as.Date("2020-03-01")
	x3=as.Date("2020-03-31")
	x4=as.Date("2020-10-01")

covid.num = as.numeric(as.Date("2020-03-20"))

}

	y1=fit$coef[1]+fit$coef[2]*(as.numeric(x1)-covid.num)
	y2=fit$coef[1]+fit$coef[2]*(as.numeric(x2)-covid.num)
	y3=fit$coef[1]+fit$coef[2]*(as.numeric(x3)-covid.num) + fit$coef[3]*(as.numeric(x3)-covid.num)
	y4=fit$coef[1]+fit$coef[2]*(as.numeric(x4)-covid.num)+ fit$coef[3]*(as.numeric(x4)-covid.num)

lines_df1=data.frame(x=c(x1,x2),y=c(y1,y2))
lines_df2=data.frame(x=c(x3,x4),y=c(y3,y4))

mycols=gg_color_hue(2)[2:1]

gr=ggplot(data=Data, aes(x=date.month, y=value3) ) +  geom_point(aes(color=postcovid))+scale_color_manual(values=mycols) + xlab("") + ylab("") + geom_line(aes(x=x,y=y),data=lines_df1, linetype="solid",color=mycols[1],size=1.5,lineend="round")+geom_line(aes(x=x,y=y),data=lines_df2,linetype="solid",color=mycols[2],size=1.5,lineend="round")+geom_line(data=myspline[myspline$x<=x2,],aes(x=as.Date(x),y=lwr),color=mycols[1])+geom_line(data=myspline[myspline$x<=x2,],aes(x=as.Date(x),y=upr),color=mycols[1])+geom_ribbon(data=myspline[myspline$x<=x2,],aes(x=x,ymin=lwr,ymax=upr),fill=mycols[1],alpha=.1)+geom_line(data=myspline[myspline$x>=x3,],aes(x=as.Date(x),y=lwr),color=mycols[2])+geom_line(data=myspline[myspline$x>=x3,],aes(x=as.Date(x),y=upr),color=mycols[2])+geom_ribbon(data=myspline[myspline$x>=x3,],aes(x=x,ymin=lwr,ymax=upr),fill=mycols[2],alpha=.1) + theme_minimal()+theme(text = element_text(size = 14),legend.position = "none")

if(!is.null(ylim)){gr=gr+ylim(ylim)}

gr
}
