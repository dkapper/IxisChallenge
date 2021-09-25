# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# import data sets
sessionCounts <- read.csv("DataAnalyst_Ecom_data_sessionCounts.csv")
addsToCart <- read.csv("DataAnalyst_Ecom_data_addsToCart.csv")

# reformat data

sessionCounts$dim_browser = as.factor(sessionCounts$dim_browser)
sessionCounts$dim_deviceCategory = as.factor(sessionCounts$dim_deviceCategory)
sessionCounts$dim_date = as.Date(sessionCounts$dim_date,"%m/%d/%y")

# load necessary packages
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

reftable1 = sessionCounts %>%
  select(dim_date,dim_deviceCategory,sessions,transactions,QTY) %>% #select desired vars
  mutate(YearMonth = format(dim_date,"%y-%m")) %>% #extract date vars
  group_by(YearMonth,dim_deviceCategory) %>% # group for aggregation
  summarize_at(vars(sessions:QTY),sum,na.rm=TRUE) %>% # variables to aggregate
  mutate(ECR = transactions/sessions) %>% # generate ECR var
  rename(device = dim_deviceCategory)

addsToCart = addsToCart%>%
  mutate(YearMonth = format(as.Date(paste(dim_year,dim_month,1,sep="-"),"%Y-%m-%d"),"%y-%m")) # add YearMonth to addsToCart# add YearMonth to addsToCart
addtable = reftable1 %>%
  group_by(YearMonth)%>%
  summarize_at(vars(sessions:ECR),sum,na.rm=TRUE) # aggregate device totals
  
  
addtable = right_join(addtable,addsToCart[,3:4],by="YearMonth") # merge addstoCart to other metrics
  

length = nrow(addtable) # find out size of data
reftable2 = addtable[(length-1):length,] %>% #select most recent 2 months
  gather(variable, value, -YearMonth)%>%
  spread(YearMonth, value) #Transpose data

names(reftable2)=c("Metric","PriorMonth","RecentMonth") #rename vars
reftable2 = reftable2[,c(1,3,2)]%>% #reorder vars
  mutate(AbsDiff = abs(RecentMonth-PriorMonth), RelDiff = RecentMonth-PriorMonth, PctChg = (RecentMonth-PriorMonth)/PriorMonth*100)


#port reference tables as sheets to Excel file
if (!require('openxlsx')) install.packages('openxlsx'); library('openxlsx')


wb = createWorkbook()
addWorksheet(wb,"RT1")
addWorksheet(wb,"RT2")
writeData(wb,"RT1",reftable1)
writeData(wb,"RT2",reftable2)
saveWorkbook(wb,file="IDC_Kapper.xlsx",overwrite=TRUE)

# begin visualization process
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('ggpubr')) install.packages('ggpubr'); library('ggpubr')

options(scipen = 100)

reftable1=reftable1%>%
  mutate(date=paste(substr(YearMonth,4,5),"/20",substr(YearMonth,1,2),sep=""))%>% #generate time variable for figures
  mutate(date=as.factor(date))

#plot each key metric across devices to see emerging trends 
sess = ggplot(data=reftable1,mapping=aes(x=date,y=sessions,group=device,color=device))+
  geom_point()+geom_line()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  ylab("Number of Sessions")+xlab("Month")+
  scale_color_manual(name="Device",labels=c("Desktop","Mobile","Tablet"),values=c("darksalmon","darkseagreen","cornflowerblue"))

trans = ggplot(data=reftable1,mapping=aes(x=date,y=transactions,group=device,color=device))+
  geom_point()+geom_line()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  ylab("Number of Transactions")+xlab("Month")+
  scale_color_manual(name="Device",labels=c("Desktop","Mobile","Tablet"),values=c("darksalmon","darkseagreen","cornflowerblue"))

quant = ggplot(data=reftable1,mapping=aes(x=date,y=QTY,group=device,color=device))+
  geom_point()+geom_line()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  ylab("Quantity Transacted")+xlab("Month")+
  scale_color_manual(name="Device",labels=c("Desktop","Mobile","Tablet"),values=c("darksalmon","darkseagreen","cornflowerblue"))

ecr = ggplot(data=reftable1,mapping=aes(x=date,y=ECR,group=device,color=device))+
  geom_point()+geom_line()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(angle=45,hjust=1))+
  ylab("E-commerce Conversion Rate (ECR)")+xlab("Month")+
  scale_color_manual(name="Device",labels=c("Desktop","Mobile","Tablet"),values=c("darksalmon","darkseagreen","cornflowerblue"))

ggarrange(trans,quant,sess,ecr,nrow=2,ncol=2,common.legend = TRUE,legend="bottom")
ggsave("Trends.png",width=11,height=8.5,units="in")

sesh=reftable1$sessions
trans=reftable1$transactions
qty=reftable1$QTY
eccr = reftable1$ECR

# manova analysis to detect which variables differ across devices
res.man = manova(cbind(sesh,trans,qty,eccr)~device,data=reftable1)
summary.aov(res.man) # all variables significantly differ


# build individual models and use Tukey to see what the significant diffs are
sess_aov = aov(sessions~device, reftable1)
TukeyHSD(sess_aov)

trans_aov = aov(transactions~device, reftable1)
TukeyHSD(trans_aov)

qty_aov = aov(QTY~device,reftable1)
TukeyHSD(trans_aov)

ecr_aov = aov(ECR~device,reftable1)
TukeyHSD(ecr_aov)

addtable$YearMonth = as.factor(addtable$YearMonth)
addtable$month = 1:12

sess_lm = lm(sessions~month,addtable)
summary(sess_lm)

trans_lm = lm(transactions~month,addtable)
summary(trans_lm)

addtable=addtable%>%
  mutate(date=paste(substr(YearMonth,4,5),"/20",substr(YearMonth,1,2),sep=""))%>% #generate time variable for figures
  mutate(date=as.factor(date))

trans_plot = ggplot(data=addtable,mapping=aes(x=month,y=transactions))+
  geom_point(aes(fill="black"))+geom_smooth(formula=y~x,method=lm,color="darkgreen",se=TRUE)+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.position = "none")+
  scale_x_continuous(breaks=1:12,labels=addtable$date)+
  ylab("Number of Transactions")+xlab("Month")

qty_lm = lm(QTY~month,addtable)
summary(qty_lm)

ecr_lm = lm(ECR~month,addtable)
summary(ecr_lm)

ecr_plot = ggplot(data=addtable,mapping=aes(x=month,y=ECR))+
  geom_point(aes(fill="black"))+geom_smooth(formula=y~x,method=lm,color="darkgreen",se=TRUE)+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.position = "none")+
  scale_x_continuous(breaks=1:12,labels=addtable$date)+
  ylab("ECR")+xlab("Month")

ggarrange(trans_plot,ecr_plot,nrow=1,ncol=2)
ggsave("LinearFits.png",width=8,height=4,units="in")

cor.test(reftable1$transactions,reftable1$QTY)#informs a near perfect positive correlation
cor.test(addtable$transactions,addtable$QTY) #informs a near perfect positive correlation
