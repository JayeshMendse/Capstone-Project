# Fetching data from excel file (Raw data from Market data file)
library(readxl)
dfraw<-read_xlsx("C:/PGCBA project/Market Data.xlsx",sheet="Combo")
dfraw
head(dfraw)
tail(dfraw)

# --------------------------------

getwd()
str(dfraw)
dfraw<-read_xlsx("C:/PGCBA project/Market Data.xlsx")
str(dfraw)

library(dplyr)
names(dfraw) # displays name of column from dataframe
colnames(dfraw)

df_sel_co<-dfraw[,1:5] # filter only selected columns
colnames(df_sel_co)
dim(df_sel_co) # displays no of rows and column in data frame



                      
df_sel_co<-select(dfraw,1,2,5,11,16)

colnames(df_sel_co)




#----------------------------

# Removing rows from dfraw where there is no value
dfraw<-dfraw[complete.cases(dfraw),]
dfraw

# Exporting clean data  from data frame  dfraw to excel file
install.packages("writexl")
library(writexl)
write_xlsx(dfraw,"C:/PGCBA project/Testing file.xlsx")

# Fetching data from excel file (Clean data from Testing file)
library(readxl)
dfclean<-read_xlsx("C:/PGCBA project/Testing file.xlsx")
dfclean

# Reading only large cap data
df_large<-dfclean[dfclean$LCMC == "LC",c("Name","Sector")]
df_large

# Bar Plot for large cap company name and its market cap
df_marcap<-dfclean[dfclean$LCMC == "LC",c("Name","MarketCap")]
df_marcap
head(df_marcap,10)
tail(df_marcap,10)
library(ggplot2)
plot<-ggplot(df_marcap,aes(Name,MarketCap))+geom_bar(stat="identity",color='steelblue4',fill='steelblue4')
plot





# Ascending Order of bar plot having vertical columns
plot<-ggplot(df_marcap,aes(reorder(Name,MarketCap),y=+MarketCap))
plot<-plot+geom_bar(stat="identity",color='blue',fill='blue')
plot<-plot+theme(axis.text.x=element_text(angle=35,hjust=0.9))
plot

# Ascending order of bar plot having horizontal bars
barplot(df_marcap$MarketCap,names.arg=df_marcap$Name,horiz=TRUE,xlab="MarketCap",ylab="Name",col='blue',main="Market cap of companies")




# Bottom 10 bars of Ascending order chart


# Descending Order
plot<-ggplot(df_marcap,aes(Name,-MarketCap),y=MarketCap)
plot<-plot+geom_bar(stat="identity",color='steelblue4',fill='steelblue4')
plot<-plot+theme(axis.text.x=element_text(angle=25,hjust=0.9))
plot

# Descending order of having bar plot having horizontal bars
library(ggplot2)
plot<-ggplot(df_marcap,aes(x=reorder(Name,-MarketCap),y=MarketCap))
plot<-plot+geom_bar(stat="identity",color="steelblue4",fill="steelblue4")
plot<-plot+theme(axis.text.x=element_text(angle=25,hjust=0.9))
plot

#Reading large cap data with CMP and Sales
df_lc_s_c<-dfclean[dfclean$LCMC == "LC",c("CMP","Sales")]
df_lc_s_c

# Histogram of large cap - CMP Vs Sales using ggplot method
hist(df_lc_s_c$Sales,xlab="CMP",main="Correlation between CMP and Sales",ylab="Sales",col='blue',border='black')
library(ggplot2)
ggplot(df_lc_s_c, aes(x = Sales)) +
  geom_histogram(color = "black", fill = "steelblue") +
  labs(x = "Sales", y = "CMP") +
  ggtitle("Correlation between CMP and Sales") +
  theme_minimal()
# Scatterplot
ggplot(df_lc_s_c)+
  geom_point(aes(x=Sales,y=CMP),size=4,color="blue")+
  ggtitle("Correlation between CMP and Sales")+
  xlab("sales")+
  ylab("CPM")
# Boxplot
plot<-ggplot(df_lc_s_c,aes(x=Sales,y=CMP))+geom_boxplot()
plot
ggplot(df_lc_s_c, aes(x = Sales, y = CMP, fill = 'label')) + 
  geom_boxplot() +
  theme(legend.position = "top")
# Heapmap
library(ggplot2)
install.packages("reshape2")
library(reshape2)
df_lc_s_c<-cor(df_lc_s_c[sapply(df_lc_s_c,is.numeric)])
df_lc_s_c<-melt(df_lc_s_c)
library(ggplot2)
ggplot(df_lc_s_c,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+scale_fill_gradient(high="blue",low="white")+
  geom_tile()
labs(title = "Correlation Heatmap",
     x = "Sales",
     y="CMP")




#Reading Mid cap data with CMP and Sales
df_mc_s_c<-dfclean[dfclean$LCMC == "MC",c("CMP","Sales")]
df_mc_s_c

# Histogram
hist(df_mc_s_c$Sales,xlab="CMP",main="Correlation between CMP and Sales",ylab="Sales",col='blue',border='black')
hist(df_mc_s_c$Sales,col="blue",border="black")

# Scatterplot
ggplot(df_mc_s_c)+
  geom_point(aes(x=Sales,y=CMP),size=4,color="blue")+
  ggtitle("Correlation between CMP and Sales")+
  xlab("sales")+
  ylab("CPM")

# Boxplot
plot<-ggplot(df_mc_s_c,aes(x=Sales,y=CMP))+geom_boxplot()
plot

# Heapmap
library(ggplot2)
install.packages("reshape2")
library(reshape2)
df_mc_s_c<-cor(df_mc_s_c[sapply(df_mc_s_c,is.numeric)])
df_mc_s_c<-melt(df_mc_s_c)
library(ggplot2)
ggplot(df_mc_s_c,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+scale_fill_gradient(high="blue",low="white")+
  geom_tile()
labs(title = "Correlation Heatmap",
     x = "Sales",
     y="CMP")


# Reading only mid cap data
df_mid_marcap<-dfclean[dfclean$LCMC == "MC",c("Name","MarketCap")]
df_mid_marcap
head(df_mid_marcap,10)
tail(df_mid_marcap,10)
library(ggplot2)
plot<-ggplot(df_mid_marcap,aes(Name,MarketCap))+geom_bar(stat="identity",color="steelblue4",fill="steelblue4")
plot

# Ascending Order of bar plot having vertical columns
plot<-ggplot(df_mid_marcap,aes(reorder(Name,MarketCap),y=+MarketCap))
plot<-plot+geom_bar(stat="identity",color='blue',fill='blue')
plot<-plot+theme(axis.text.x=element_text(angle=35,hjust=0.9))
plot
# Ascending order of bar plot having horizontal bars
barplot(df_mid_marcap$MarketCap,names.arg=df_mid_marcap$Name,horiz=TRUE,xlab="MarketCap",ylab="Name",col='blue',main="Market cap of companies")
# Descending order of having bar plot having horizontal bars
plot<-ggplot(df_mid_marcap,aes(x=reorder(Name,-MarketCap),y=MarketCap))
plot<-plot+geom_bar(stat="identity",color='steelblue4',fill='steelblue4')
plot<-plot+theme(axis.text.x=element_text(angle=25,hjust=0.9))
plot

# Correlation between CMP and EPS
df_large_c_e<-dfclean[dfclean$LCMC == "LC",c("CMP","EPS")]
df_large_c_e
# Scatter Plot
library(ggplot2)
ggplot(df_large_c_e)+geom_point(aes(x=EPS,y=CMP),size=5,color='blue')+ggtitle("Correlation between CMP & EPS(Large Cap)")+xlab("CMP")+ylab("EPS")

# Mid cap correlation
df_midcap_c_e<-dfclean[dfclean$LCMC == "MC",c("CMP","EPS")]
df_midcap_c_e
ggplot(df_midcap_c_e)+geom_point(aes(x=EPS,y=CMP),size=5,color='blue')+ggtitle("Correlation between CMP & EPS(Mid Cap)")+xlab("CMP")+ylab("EPS")
# Market Cap & Ind_PE(LC)
df_large_m_ip<-dfclean[dfclean$LCMC == "LC",c("MarketCap","Ind_PE")]
df_large_m_ip
library(ggplot2)
ggplot(df_large_m_ip)+geom_point(aes(x=Ind_PE,y=MarketCap),size=5,color='blue')+ggtitle("Correlation between MarketCap & Ind_PE")+xlab("Ind_PE")+ylab("MarketCap")
#  Market Cap & Ind_PE(MC)
df_midcap_m_ip<-dfclean[dfclean$LCMC == "MC",c("MarketCap","Ind_PE")]
df_midcap_m_ip
ggplot(df_midcap_m_ip)+geom_point(aes(x=Ind_PE,y=MarketCap),size=5,color='blue')+ggtitle("Correlation between MarketCap & Ind_PE")+xlab("Ind_PE")+ylab("MarketCap")
# Market Cap and OPM_Percent (LC)
df_large_m_op<-dfclean[dfclean$LCMC == "LC",c("MarketCap","OPM_Percent")]
df_large_m_op
ggplot(df_large_m_op)+geom_point(aes(x=OPM_Percent,y=MarketCap),size=5,color='blue')+ggtitle("Correlation between MarketCap & OPM_Percent")+xlab("OPM_Percent")+ylab("MarketCap")
# Market Cap and OPM_Percent (MC)
df_midcap_m_op<-dfclean[dfclean$LCMC == "MC",c("MarketCap","OPM_Percent")]
df_midcap_m_op
ggplot(df_midcap_m_op)+geom_point(aes(x=OPM_Percent,y=MarketCap),size=5,color='blue')+ggtitle("Correlation between MarketCap & OPM_Percent")+xlab("OPM_Percent")+ylab("MarketCap")
# Sales and Ind_PE (LC)
df_large_s_pe<-dfclean[dfclean$LCMC == "LC",c("Sales","Ind_PE")]
df_large_s_pe
library(ggplot2)
ggplot(df_large_s_pe)+geom_point(aes(x=Ind_PE,y=Sales),size=7,color="blue")+ggtitle("Correlation betweem Sales and Ind_PE")+xlab("Ind_PE")+ylab("Sales")
# Sales and Ind_PE (MC)
df_midcap_s_pe<-dfclean[dfclean$LCMC == "MC",c("Sales","Ind_PE")]
df_midcap_s_pe
ggplot(df_midcap_s_pe)+geom_point(aes(x=Ind_PE,y=Sales),size=7,color="blue")+ggtitle("Correlation betweem Sales and Ind_PE")+xlab("Ind_PE")+ylab("Sales")
?ggplot
# Sales and Market Cap(LC)
df_large_s_mc<-dfclean[dfclean$LCMC == "LC",c("Sales","MarketCap")]
df_large_s_mc
ggplot(df_large_s_mc)+geom_point(aes(x=MarketCap,y=Sales),size=7,color="blue")+ggtitle("Correlation betweem Sales and MarketCap")+xlab("MarketCap")+ylab("Sales")
# Sales and Market Cap(MC)
df_midcap_s_mc<-dfclean[dfclean$LCMC == "MC",c("Sales","MarketCap")]
df_midcap_s_mc
ggplot(df_midcap_s_mc)+geom_point(aes(x=MarketCap,y=Sales),size=7,color="blue")+ggtitle("Correlation betweem Sales and MarketCap")+xlab("MarketCap")+ylab("Sales")
# CMP and debt/Equity(LC)
df_large_cm_d<-dfclean[dfclean$LCMC == "LC",c("CMP","Debt/Eq")]
df_large_cm_d
ggplot(df_large_cm_d)+geom_point(aes(x=`Debt/Eq`,y=CMP),size=7,color="blue")+ggtitle("Correlation betweem CMP and Debt/eq")+xlab("Debt/eq")+ylab("CMP")
# CMP and debt/Equity(MC)
df_midcap_cm_d<-dfclean[dfclean$LCMC == "MC",c("CMP","Debt/Eq")]
df_midcap_cm_d
ggplot(df_midcap_cm_d)+geom_point(aes(x=`Debt/Eq`,y=CMP),size=7,color="blue")+ggtitle("Correlation betweem CMP and Debt/eq")+xlab("Debt/eq")+ylab("CMP")
# CMP and Sales Growth in 5 years(LC)
df_large_cm_sg<-dfclean[dfclean$LCMC == "LC",c("CMP","Sales_ Growth_5yrs")]
df_large_cm_sg
library(ggplot2)
ggplot(df_large_cm_sg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=CMP),size=7,color="blue")+ggtitle("Correlation between CMP and Sales growth in 5 years")+xlab("Sales_ Growth_5yrs")+ylab("CMP")
# CMP and Sales Growth in 5 years(MC)
df_midcap_cm_sg<-dfclean[dfclean$LCMC == "MC",c("CMP","Sales_ Growth_5yrs")]
df_midcap_cm_sg
ggplot(df_midcap_cm_sg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=CMP),size=7,color="blue")+ggtitle("Correlation between CMP and Sales growth in 5 years")+xlab("Sales_ Growth_5yrs")+ylab("CMP")
# CMP and Profit growth in 5years(LC)
df_large_cm_pg<-dfclean[dfclean$LCMC == "LC",c("CMP","Profit_Growth_5yrs ")]
df_large_cm_pg
ggplot(df_large_cm_pg)+geom_point(aes(x=`Profit_Growth_5yrs `,y=CMP),size=7,color="blue")+ggtitle("Correlation between CMP and Sales growth in 5 years")+xlab("Profit_Growth_5yrs")+ylab("CMP")
# CMP and Profit growth in 5years(MC)
df_midcap_cm_pg<-dfclean[dfclean$LCMC == "MC",c("CMP","Profit_Growth_5yrs ")]
df_midcap_cm_pg
ggplot(df_midcap_cm_pg)+geom_point(aes(x=`Profit_Growth_5yrs `,y=CMP),size=7,color="blue")+ggtitle("Correlation between CMP and Sales growth in 5 years")+xlab("Profit_Growth_5yrs")+ylab("CMP")
# Stock price growth in 5 years and EPS growth in 5 years (LC)
df_large_pg_epc<-dfclean[dfclean$LCMC == "LC",c("Profit_Growth_5yrs ","EPS_Change_5yrs")]
df_large_pg_epc
ggplot(df_large_pg_epc)+geom_point(aes(x=EPS_Change_5yrs,y=`Profit_Growth_5yrs `),size=7,color="blue")+ggtitle("Correlation between profit and EPS growth in 5 years")+xlab("EPS_Change_5yrs")+ylab("Profit_Growth_5yrs")
# Stock price growth in 5 years and EPS growth in 5 years (MC)
df_midcap_pg_epc<-dfclean[dfclean$LCMC == "MC",c("Profit_Growth_5yrs ","EPS_Change_5yrs")]
df_midcap_pg_epc
ggplot(df_midcap_pg_epc)+geom_point(aes(x=EPS_Change_5yrs,y=`Profit_Growth_5yrs `),size=7,color="blue")+ggtitle("Correlation between Profit and EPS growth in 5 years")+xlab("EPS_Change_5yrs")+ylab("Profit_Growth_5yrs")
# Stock price growth and sales price growth(LC)
df_large_pg_spg<-dfclean[dfclean$LCMC == "LC",c("Profit_Growth_5yrs ","Sales_ Growth_5yrs")]
df_large_pg_spg
library(ggplot2)
ggplot(df_large_pg_spg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=`Profit_Growth_5yrs `),size=4,color="blue")+ggtitle("Correlation between Sales and Stock Price growth in 5 years")+xlab("Sales_ Growth_5yrs")+ylab("Profit_Growth_5yrs")

hist(df_large_pg_spg$`Sales_ Growth_5yrs`,xlab="Sales_ Growth_5yrs",main="Histogram for Sales Growth 5 years",ylim=c(0,60),col="steelblue4",border='white')
# Stock price growth and sales price growth(MC)
df_midcap_pg_spg<-dfclean[dfclean$LCMC == "MC",c("Profit_Growth_5yrs ","Sales_ Growth_5yrs")]
df_midcap_pg_spg

ggplot(df_midcap_pg_spg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=`Profit_Growth_5yrs `),size=4,color="blue")+ggtitle("Correlation between Sales and Stock Price growth in 5 years")+xlab("Sales_ Growth_5yrs")+ylab("Profit_Growth_5yrs")
hist(df_midcap_pg_spg$`Sales_ Growth_5yrs`,xlab="Sales_ Growth_5yrs",main="Histogram for Sales Growth 5 years",ylimc=c(0,60),col="steelblue4",border="white")
# EPS 5 years growth and Debt/eq (LC)
df_large_epfi_deeq<-dfclean[dfclean$LCMC == "LC",c("Debt/Eq","EPS_Change_5yrs")]
df_large_epfi_deeq
library(ggplot2)
ggplot(df_large_epfi_deeq)+geom_point(aes(x=`Debt/Eq`,y=EPS_Change_5yrs),size=7,color="blue")+ggtitle("Correlation between EPS_Change_5yrs and Debt/Eq")+xlab("EPS_Change_5yrs")+ylab("Debt/Eq")                                                                                                                                                                                                              

# EPS 5 years growth and Debt/eq (MC)
df_midcap_epfi_deeq<-dfclean[dfclean$LCMC == "MC",c("Debt/Eq","EPS_Change_5yrs")]
df_midcap_epfi_deeq
ggplot(df_midcap_epfi_deeq)+geom_point(aes(x=`Debt/Eq`,y=EPS_Change_5yrs),size=7,color="blue")+ggtitle("Correlation between EPS_Change_5yrs and Debt/Eq")+xlab("EPS_Change_5yrs")+ylab("Debt/Eq")                                                                                                                                                                                                              


# PE and EPS change 5 years (LC)
pe_epfi<-dfclean[dfclean$LCMC == "LC",c("P/E","EPS_Change_5yrs")]
pe_epfi
library(ggplot2)
ggplot(pe_epfi)+geom_point(aes(x=EPS_Change_5yrs,y= `P/E` ),size=7,color='blue')+ggtitle("Correlation between P/E and EPS_Change_5yrs")+xlab("EPS_Change_5yrs")+ylab("P /E")

#  PE and EPS change 5 years (MC)
pe_epfi<-dfclean[dfclean$LCMC == "MC",c("P/E","EPS_Change_5yrs")]
pe_epfi
ggplot(pe_epfi)+geom_point(aes(x=EPS_Change_5yrs,y= `P/E` ),size=7,color='blue')+ggtitle("Correlation between P/E and EPS_Change_5yrs")+xlab("EPS_Change_5yrs")+ylab("P /E")

# Stock Price and Sales Growth(LC)
sp_sg<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sp_sg
ggplot(sp_sg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Sales Growth in 5years")+xlab("Sales _Growth_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and Sales Growth(MC)

sp_sg<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sp_sg
ggplot(sp_sg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Sales Growth in 5years")+xlab("Sales _Growth_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and Average PAT (LC)
sp_ap<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Avg _PAT_5yrs")]
sp_ap
ggplot(sp_ap)+geom_point(aes(x=`Avg _PAT_5yrs`,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Avg PAT in 5years")+xlab("Avg _PAT_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and Average PAT (MC)
sp_ap<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Avg _PAT_5yrs")]
sp_ap
ggplot(sp_ap)+geom_point(aes(x=`Avg _PAT_5yrs`,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Avg PAT in 5years")+xlab("Avg _PAT_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and OPM Percent (LC)
sp_op<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","OPM_Percent")]
sp_op
ggplot(sp_op)+geom_point(aes(x=OPM_Percent,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and OPM Percent in 5years")+xlab("OPM_Percent")+ylab("Stock_Price_Return_5yrs")

# Stock Price and OPM Percent (MC)
sp_op<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","OPM_Percent")]
sp_op
ggplot(sp_op)+geom_point(aes(x=OPM_Percent,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and OPM Percent in 5years")+xlab("OPM_Percent")+ylab("Stock_Price_Return_5yrs")

# Stock price 5 yrs and Cash flow 5yrs (LC)
sp_cf<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","CF_ Opr _5Yrs ")]
sp_cf
library(ggplot2)
ggplot(sp_cf)+geom_point(aes(x=`CF_ Opr _5Yrs `,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price and CF Opr 5Years")+xlab("CF_ Opr _5Yrs ")+ylab("Stock_Price_Return_5yrs")

# Stock price and Cash flow (MC)
sp_cf<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","CF_ Opr _5Yrs ")]
sp_cf
ggplot(sp_cf)+geom_point(aes(x=`CF_ Opr _5Yrs `,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price and CF Opr 5Years")+xlab("CF_ Opr _5Yrs ")+ylab("Stock_Price_Return_5yrs")

# Stock Price and Profit Growth (LC)
sp_pg<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Profit_Growth_5yrs ")]
sp_pg
ggplot(sp_pg)+geom_point(aes(x=`Profit_Growth_5yrs `,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Profit Growth in 5Years")+xlab("Profit_Growth_5yrs ")+ylab("Stock_Price_Return_5yrs")

# Stock Price and Profit Growth (MC)
sp_pg<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Profit_Growth_5yrs ")]
sp_pg
ggplot(sp_pg)+geom_point(aes(x=`Profit_Growth_5yrs `,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Profit Growth in 5Years")+xlab("Profit_Growth_5yrs ")+ylab("Stock_Price_Return_5yrs")

# Stock Price and EBIDT5yrs (LC)
sp_eb<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","EBIDT_5yrs")]
sp_eb
ggplot(sp_eb)+geom_point(aes(x=EBIDT_5yrs,y=Stock_Price_Return_5yrs),size=7,color="blue")+ggtitle("Correlation betwen Stock Price Return and EBIDT in 5Years")+xlab("EBIDT_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and EBIDT5yrs (MC)
sp_eb<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","EBIDT_5yrs")]
sp_eb
ggplot(sp_eb)+geom_point(aes(x=EBIDT_5yrs,y=Stock_Price_Return_5yrs),size=7,color="blue")+ggtitle("Correlation betwen Stock Price Return and EBIDT in 5Years")+xlab("EBIDT_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and free cash flow (LC)
sp_fcf<-dfclean[dfclean$LCMC == "LC", c("Stock_Price_Return_5yrs","Free _Cash_ Flow _5yrs")]
sp_fcf
ggplot(sp_fcf)+geom_point(aes(x=`Free _Cash_ Flow _5yrs`,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Free Cash flow in 5Years")+xlab("Free _Cash_ Flow _5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and free cash flow (MC)
sp_fcf<-dfclean[dfclean$LCMC == "MC", c("Stock_Price_Return_5yrs","Free _Cash_ Flow _5yrs")]
sp_fcf
ggplot(sp_fcf)+geom_point(aes(x=`Free _Cash_ Flow _5yrs`,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price Return and Free Cash flow in 5Years")+xlab("Free _Cash_ Flow _5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price and ROE 5yrs (LC)
sp_roe<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","ROE_5Yr ")]
sp_roe
ggplot(sp_roe)+geom_point(aes(x=`ROE_5Yr `,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price and ROE in 5years")+xlab("ROE_5Yr ")+ylab("Stock_Price_Return_5yrs")

# Stock Price and ROE 5yrs (MC)
sp_roe<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","ROE_5Yr ")]
sp_roe
ggplot(sp_roe)+geom_point(aes(x=`ROE_5Yr `,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price and ROE in 5years")+xlab("ROE_5Yr ")+ylab("Stock_Price_Return_5yrs")

# Stock Price and 5 years PE (LC)
sp_5ype<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","PE_5yrs")]
sp_5ype
library(ggplot2)
ggplot(sp_5ype)+geom_point(aes(x=PE_5yrs,y=Stock_Price_Return_5yrs),size=7,color='blue')+ggtitle("Correlation between Stock Price and PE5yrs in 5years")+xlab("PE_5yrs")+ylab("Stock_Price_Return_5yrs")

#Stock Price and 5 years PE (MC)
sp_5ype<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","PE_5yrs")]
sp_5ype
ggplot(sp_5ype)+geom_point(aes(x=PE_5yrs,y=Stock_Price_Return_5yrs),size=4,color='blue')+ggtitle("Correlation between Stock Price and PE5yrs in 5years")+xlab("PE_5yrs")+ylab("Stock_Price_Return_5yrs")

# Stock Price in 5 Years and PEG (LC)
sp_peg<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","PEG")]
sp_peg
ggplot(sp_peg)+geom_point(aes(x=PEG,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price and PEG")+
  xlab("PEG")+
  ylab("Stock_Price_Return_5yrs")

# Stock Price in 5 Years and PEG (MC)
sp_peg<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","PEG")]
sp_peg
ggplot(sp_peg)+geom_point(aes(x=PEG,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price and PEG")+
  xlab("PEG")+
  ylab("Stock_Price_Return_5yrs")

# Stock Price in 5 Years and M.Cap/Sales (LC)
sp_mcap_sale<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
sp_mcap_sale
ggplot(sp_mcap_sale)+geom_point(aes(x=`M.Cap/Sales`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price and M.Cap/Sales")+
  xlab("M.Cap/Sales")+
  ylab("Stock_Price_Return_5yrs")


# Stock Price in 5 Years and M.Cap/Sales (MC)
sp_mcap_sale<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
sp_mcap_sale
ggplot(sp_mcap_sale)+geom_point(aes(x=`M.Cap/Sales`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price and M.Cap/Sales")+
  xlab("M.Cap/Sales")+
  ylab("Stock_Price_Return_5yrs")

# Stock Price in 5 years and Debt/Profit (LC)
sp_dp<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Debt/Profit")]
sp_dp
ggplot(sp_dp)+geom_point(aes(x= `Debt/Profit`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price and Debt/Profit")+
  xlab("Debt/Profit")+
  ylab("Stock_Price_Return_5yrs")


# Stock Price in 5 years and Debt/Profit (MC)
sp_dp<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Debt/Profit")]
sp_dp
ggplot(sp_dp)+geom_point(aes(x= `Debt/Profit`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price and Debt/Profit")+
  xlab("Debt/Profit")+
  ylab("Stock_Price_Return_5yrs")


# Stock Price in 5 years and Change in FIIs (LC)
sp_fii<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fii
library(ggplot2)
ggplot(sp_fii)+geom_point(aes(x=`Chg_ in FII _Hold _3Yr`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price in 5 years and FII hold 3yr")+
  xlab("Chg_ in FII _Hold _3Yr")+
  ylab("Stock_Price_Return_5yrs")


# Stock Price in 5 years and Change in FIIs (MC)
sp_fii<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fii
ggplot(sp_fii)+geom_point(aes(x=`Chg_ in FII _Hold _3Yr`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price in 5 years and FII hold 3yr")+
  xlab("Chg_ in FII _Hold _3Yr")+
  ylab("Stock_Price_Return_5yrs")

# Stock Price,Sales growth, ROE and Profit growth(LC)
n_sp_sg_roe_pg<-dfclean[dfclean$LCMC == "LC",c("Name","Stock_Price_Return_5yrs","Sales_ Growth_5yrs",
                                               "Profit_Growth_5yrs ","ROE_5Yr ","EBIDT_5yrs","EPS_Change_5yrs",
                                               "5Yr_ OPM _Percent")]
n_sp_sg_roe_pg<- colMeans(n_sp_sg_roe_pg[sapply(n_sp_sg_roe_pg,is.numeric)],na.rm=TRUE)
round(n_sp_sg_roe_pg,digits=2)

# Barplot for stock price 5years, Sales growth 5years, Profit growth 5years......for LC.
barplot(n_sp_sg_roe_pg,names.arg=c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs",
                                   "Profit_Growth_5yrs ","ROE_5Yr ","EBIDT_5yrs","EPS_Change_5yrs",
                                   "5Yr_ OPM _Percent"),legend=rownames(n_sp_sg_roe_pg),horiz=FALSE,
        xlab="Average Comparison",main="Large Cap - Comparision of Parameters")


#?round
?barplot


# Stock Price,Sales growth, ROE and Profit growth(MC)
n_sp_sg_roe_pg<-dfclean[dfclean$LCMC == "MC",c("Name","Stock_Price_Return_5yrs","Sales_ Growth_5yrs",
                                               "Profit_Growth_5yrs ","ROE_5Yr ","EBIDT_5yrs","EPS_Change_5yrs",
                                               "5Yr_ OPM _Percent")]
n_sp_sg_roe_pg<- colMeans(n_sp_sg_roe_pg[sapply(n_sp_sg_roe_pg,is.numeric)],na.rm=TRUE)
round(n_sp_sg_roe_pg,digits=2)

# Barplot for stock price 5years, Sales growth 5years, Profit growth 5years......for MC.
barplot(n_sp_sg_roe_pg,names.arg=c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs",
                                   "Profit_Growth_5yrs ","ROE_5Yr ","EBIDT_5yrs","EPS_Change_5yrs",
                                   "5Yr_ OPM _Percent"),legend=rownames(n_sp_sg_roe_pg),horiz=FALSE,
        xlab="Average Comparison",main="Mid Cap - Comparision of Parameters")

# Average Stock Price increase Vs FII holding percentage.(LC)
sp_fi<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fi<- colMeans(sp_fi[sapply(sp_fi,is.numeric)],na.rm=TRUE)
round(sp_fi,digits=2)
barplot(sp_fi,names.org=c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr "),legend=rownames(sp_fi),horiz=FALSE,
        xlab="Average Comparison",main="Large Cap - Average Stock increase Vs FII holding")

# Average Stock Price increase Vs FII holding percentage.(MC)
sp_fi<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fi<- colMeans(sp_fi[sapply(sp_fi,is.numeric)],na.rm=TRUE)
round(sp_fi,digits=2)
barplot(sp_fi,names.org=c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr "),legend=rownames(sp_fi),horiz=FALSE,
        xlab="Average Comparison",main="Mid Cap - Average Stock increase Vs FII holding")

# Change in FII holding (LC)
fii<-dfclean[dfclean$LCMC == "LC",c("Chg_ in FII _Hold _3Yr")]
fii
hist(fii$`Chg_ in FII _Hold _3Yr`,ylim=c(0,50),col="blue",border='white')

# Change in FII holding (MC)
fii<-dfclean[dfclean$LCMC == "MC",c("Chg_ in FII _Hold _3Yr")]
fii
hist(fii$`Chg_ in FII _Hold _3Yr`,ylim=c(0,60),col="blue",border='white')

# Stock Price and Debt/Profit (LC)
sp_dp<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Debt/Profit")]
sp_dp
library(ggplot2)
ggplot(sp_dp)+geom_point(aes(x=`Debt/Profit`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price Return 5yrs and Debt/Profit")+
  xlab("Debt/Profit")+ylab("Stock_Price_Return_5yrs")

# Stock Price and Debt/Profit (MC)
sp_dp<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Debt/Profit")]
sp_dp
library(ggplot2)
ggplot(sp_dp)+geom_point(aes(x=`Debt/Profit`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price Return 5yrs and Debt/Profit")+
  xlab("Debt/Profit")+ylab("Stock_Price_Return_5yrs")

# Stock Price and M.Cap/Sales (LC)
sp_mcs<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
sp_mcs
library(ggplot2)
ggplot(sp_mcs)+geom_point(aes(x=`M.Cap/Sales`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price Return 5yrs and M.Cap/Sales")+
  xlab("M.Cap/Sales")+ylab("Stock_Price_Return_5yrs")

# Stock Price and M.Cap/Sales (MC)
sp_mcs<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
sp_mcs
ggplot(sp_mcs)+geom_point(aes(x=`M.Cap/Sales`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Stock Price Return 5yrs and M.Cap/Sales")+
  xlab("M.Cap/Sales")+ylab("Stock_Price_Return_5yrs")

# Sector wise stock price in percentage (LC)
sp_sec<-dfclean[dfclean$LCMC == "LC",c("Sector","Stock_Price_Return_5yrs")]
lbls<-c("Stock_Price_Return_5yrs","Sector")
pie(sp_sec$Stock_Price_Return_5yrs,lbls)
pie(sp_sec$Stock_Price_Return_5yrs,lbls,main="Sector wise Stock Price",col=rainbow(length(sp_sec)))
piepercent=round(100*sp_sec$Stock_Price_Return_5yrs/sum(sp_sec$Stock_Price_Return_5yrs),1)
legend("topright",c("Sector","Stock_Price_Return_5yrs"),cex=0.8,fill=rainbow(length(sp_sec)))

# Sector wise stock price in percentage (MC)
sp_sec<-dfclean[dfclean$LCMC == "MC",c("Sector","Stock_Price_Return_5yrs")]
lbls<-c("Stock_Price_Return_5yrs","Sector")
pie(sp_sec$Stock_Price_Return_5yrs,lbls)

piepercent=round(100*sp_sec$Stock_Price_Return_5yrs/sum(sp_sec$Stock_Price_Return_5yrs),1)
legend("topright",c("Sector","Stock_Price_Return_5yrs"),cex=0.8,fill=rainbow(length(sp_sec)))
# Stock Price and Sales Growth (LC)
sp_sg<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sp_sg
library(ggplot2)
ggplot(sp_sg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Sales Growth and Stock Price in 5 years")+
  xlab("Sales_ Growth_5yrs")+ylab("Stock_Price_Return_5yrs")
hist(sp_sg$`Sales_ Growth_5yrs`,xlab="Sales_ Growth_5yrs",ylab="frequency",main="Histogram of Sales Growth",col='steelblue4',border='white')

# Stock Price and Sales Growth (MC)
sp_sg<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sp_sg
library(ggplot2)
ggplot(sp_sg)+geom_point(aes(x=`Sales_ Growth_5yrs`,y=Stock_Price_Return_5yrs),size=4,color='blue')+
  ggtitle("Correlation between Sales Growth and Stock Price in 5 years")+
  xlab("Sales_ Growth_5yrs")+ylab("Stock_Price_Return_5yrs")
hist(sp_sg$`Sales_ Growth_5yrs`,xlab="Sales_ Growth_5yrs",ylab="frequency",main="Histogram of Sales Growth",col='steelblue4',border='white')

# CMP and Debt / Equity Ratio (LC)
cm_de<-dfclean[dfclean$LCMC == "LC",c("CMP","Debt/Eq")]
cm_de
library(ggplot2)
ggplot(cm_de)+geom_point(aes(x=`Debt/Eq`,y= CMP),size=4,color='blue')+
  ggtitle("Correlation between CMP and  Debt/Equity")+
  xlab("Debt/Eq")+ylab("CMP")

# CMP and Debt / Equity Ratio (MC)
cm_de<-dfclean[dfclean$LCMC == "MC",c("CMP","Debt/Eq")]
cm_de
ggplot(cm_de)+geom_point(aes(x=`Debt/Eq`,y= CMP),size=4,color='blue')+
  ggtitle("Correlation between CMP and  Debt/Equity")+
  xlab("Debt/Eq")+ylab("CMP")

# Stock Price and sales with p values, rsquared and regression equation on ggplot (LC)
sps<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sps
lmod=lm(Stock_Price_Return_5yrs~`Sales_ Growth_5yrs`,sps)
summary(lmod)
library(ggplot2)
ggplot(sps,aes(`Sales_ Growth_5yrs`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(20,20),y=c(70,65),label=c(" p-value: 0.0082",
                                                      "Adjusted R-squared:  0.06627"))



# Switch the sales growth from x to y axis and stock price from y to x axis to see the effect on
# R squares
ggplot(sps,aes(Stock_Price_Return_5yrs,`Sales_ Growth_5yrs`))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(40,40),y=c(55,60),label=c(" p-value: 0.0082",
                                                "Adjusted R-squared:  0.06627"))

# Stock Price and sales with p values, rsquared and regression equation on ggplot (MC)
sps<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sps
lmod=lm(Stock_Price_Return_5yrs~`Sales_ Growth_5yrs`,sps)
summary(lmod) 
summary(lmod)
library(ggplot2)
ggplot(sps,aes(`Sales_ Growth_5yrs`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(25,25),y=c(185,175),label=c(" p-value:  0.223",
                                                "Adjusted R-squared: 0.005658  "))
#? theme_classic()
#? geom_smooth

# Stock Price Growth in 5 years and EPS Growth in 5 years with p values and adjusted rsquared on 
# ggplot (LC)
sp_ep<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","EPS_Change_5yrs")]
sp_ep
lmod=lm(Stock_Price_Return_5yrs~EPS_Change_5yrs,sp_ep)
summary(lmod)
library(ggplot2)
ggplot(sp_ep,aes(EPS_Change_5yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(40,40),y=c(75,80),label=c(" p-value: 0.001352","Adjusted R-squared:  0.1006 "))

# Stock Price Growth in 5 years and EPS Growth in 5 years with p values and adjusted rsquared on 
# ggplot (MC)
sp_ep<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","EPS_Change_5yrs")]
sp_ep
lmod=lm(Stock_Price_Return_5yrs~EPS_Change_5yrs,sp_ep)
summary(lmod)
library(ggplot2)
ggplot(sp_ep,aes(EPS_Change_5yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(50,50),y=c(185,200),label=c(" p-value:  3.193e-09","Adjusted R-squared:  0.1006 "))

# Stock Price Growth in 5 years and Average PAT 5 years with p values 
# and adjusted r squared on ggplot (LC)
sp_ap<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Avg _PAT_5yrs")]
sp_ap
lmod=lm(Stock_Price_Return_5yrs~`Avg _PAT_5yrs`,sp_ap)
summary(lmod)
ggplot(sp_ap,aes(`Avg _PAT_5yrs`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(20000,20000),y=c(75,80),label=c("p-value: 0.005366",
                                                      "Adjusted R-squared:  0.07438 "))


# Stock Price Growth in 5 years and Average PAT 5 years with p values 
# and adjusted r squared on ggplot (MC)
sp_ap<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Avg _PAT_5yrs")]
sp_ap
lmod=lm(Stock_Price_Return_5yrs~`Avg _PAT_5yrs`,sp_ap)
summary(lmod)
ggplot(sp_ap,aes(`Avg _PAT_5yrs`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(250,250),y=c(185,200),label=c("p-value: 0.0005515
","Adjusted R-squared:  0.1176 "))

# Stock Price Growth in 5 years and 5 yrs PE with p values
# and adjusted r squared on ggplot (LC)
sp_pe<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","PE_5yrs")]
sp_pe
lmod=lm(Stock_Price_Return_5yrs~PE_5yrs,sp_pe)
summary(lmod)
library(ggplot2)
ggplot(sp_pe,aes(PE_5yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(200,200),y=c(75,80),label=c("p-value: 0.06418","Adjusted R-squared:  0.02747 "))

# Stock Price Growth in 5 years and 5 yrs PE with p values
# and adjusted r squared on ggplot (MC)
sp_pe<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","PE_5yrs")]
sp_pe
lmod=lm(Stock_Price_Return_5yrs~PE_5yrs,sp_pe)
summary(lmod)
ggplot(sp_pe,aes(PE_5yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(50,50),y=c(185,200),label=c(" p-value: 0.6191","Adjusted R-squared:  -0.008512 "))

# Stock Price Growth in 5 years and EBIDT 5years with p values
# and adjusted r squared on ggplot (LC)
sp_eb<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","EBIDT_5yrs")]
sp_eb
lmod=lm(Stock_Price_Return_5yrs~EBIDT_5yrs,sp_eb)
summary(lmod)
ggplot(sp_eb,aes(EBIDT_5yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(20,20),y=c(75,80),label=c(" p-value: 3.569e-05","Adjusted R-squared:  0.168 "))

# Stock Price Growth in 5 years and EBIDT 5years with p values
# and adjusted r squared on ggplot (MC)
sp_eb<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","EBIDT_5yrs")]
sp_eb
lmod=lm(Stock_Price_Return_5yrs~EBIDT_5yrs,sp_eb)
summary(lmod)
ggplot(sp_eb,aes(EBIDT_5yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(50,50),y=c(185,200),label=c("p-value: 6.976e-05 ","Adjusted R-squared:  0.1559  "))


# Stock Price Growth in 5 years and ROE in 5 years  with p values
# and adjusted r squared on ggplot (LC)
sp_roe<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs", "ROE_5Yrs")]
sp_roe
lmod=lm( Stock_Price_Return_5yrs~  ROE_5Yrs,sp_roe)
summary(lmod)
library(ggplot2)
ggplot(sp_roe,aes(ROE_5Yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(30,30),y=c(75,80),label=c("p-value: 0.3083","Adjusted R-squared:  0.0005622 "))

# Stock Price Growth in 5 years and ROE in 5 years  with p values
# and adjusted r squared on ggplot (MC)
sp_roe<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs", "ROE_5Yrs")]
sp_roe
lmod=lm( Stock_Price_Return_5yrs~  ROE_5Yrs,sp_roe)
summary(lmod)
library(ggplot2)
ggplot(sp_roe,aes(ROE_5Yrs,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(0,0),y=c(185,200),label=c(" p-value: 0.06499","Adjusted R-squared:  0.02724 "))

# Stock Price Growth in 5 years and Debt / Eq with p values and adjusted r squared
# on ggplot (LC)
sp_de<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Debt/Eq")]
sp_de
lmod=lm(Stock_Price_Return_5yrs~`Debt/Eq`,sp_de)
summary(lmod)
ggplot(sp_de,aes(`Debt/Eq`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(5,5),y=c(75,80),label=c("p-value: 0.5613","Adjusted R-squared:  -0.00747"))
  
# Stock Price Growth in 5 years and Debt / Eq with p values and adjusted r squared
# on ggplot (MC)
sp_de<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Debt/Eq")]
sp_de
lmod=lm(Stock_Price_Return_5yrs~`Debt/Eq`,sp_de)
summary(lmod)
ggplot(sp_de,aes(`Debt/Eq`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(5.0,5.0),y=c(185,200),label=c("p-value: 0.1688","Adjusted R-squared:  0.01028 "))

# Stock Price Growth in 5 years and PEG with p values and adjusted r squared on ggplot (LC)
sp_peg<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","PEG")]
sp_peg
lmod=lm(Stock_Price_Return_5yrs~PEG,sp_peg)
summary(lmod)
ggplot(sp_peg,aes(PEG,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(25,25),y=c(75,80),label=c("p-value: 0.432","Adjusted R-squared:  -0.004253"))

# Stock Price Growth in 5 years and PEG with p values and adjusted r squared on ggplot (MC)
sp_peg<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","PEG")]
sp_peg
lmod=lm(Stock_Price_Return_5yrs~PEG,sp_peg)
summary(lmod)
ggplot(sp_peg,aes(PEG,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(0,0),y=c(185,200),label=c("p-value: 0.411","Adjusted R-squared:  -0.00358 "))

# Stock Price Growth in 5 years and M.Cap / Sales with p values and adjusted r squared on
# ggplot (LC)
sp_mcap<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
sp_mcap
lmod=lm(Stock_Price_Return_5yrs~`M.Cap/Sales`,sp_mcap)
summary(lmod)
library(ggplot2)
ggplot(sp_mcap,aes(`M.Cap/Sales`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(20,20),y=c(75,80),label=c("p-value: 0.2973","Adjusted R-squared:  0.001115 "))

# Stock Price Growth in 5 years and M.Cap / Sales with p values and adjusted r squared on
# ggplot (MC)
sp_mcap<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
sp_mcap
lmod=lm(Stock_Price_Return_5yrs~`M.Cap/Sales`,sp_mcap)
summary(lmod)
library(ggplot2)
ggplot(sp_mcap,aes(`M.Cap/Sales`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=0)+
  theme_classic()+
  annotate("text",x=c(100,100),y=c(185,200),label=c("p-value: 0.004515","Adjusted R-squared:  0.07767 "))
 
# Stock Price Growth in 5 years and FII holding percentage with p values and adjusted r squared on
# ggplot (LC)
sp_fii<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fii
lmod=lm(Stock_Price_Return_5yrs~`Chg_ in FII _Hold _3Yr`,sp_fii)
summary(lmod)
library(ggplot2)
ggplot(sp_fii,aes(`Chg_ in FII _Hold _3Yr`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=1)+
  theme_classic()+
  annotate("text",x=c(10,10),y=c(75,80),label=c("p-value: 0.01369","Adjusted R-squared:  0.0565 "))
  
# Stock Price Growth in 5 years and FII holding percentage with p values and adjusted r squared on
# ggplot (MC)
sp_fii<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fii
lmod=lm(Stock_Price_Return_5yrs~`Chg_ in FII _Hold _3Yr`,sp_fii)
summary(lmod)
ggplot(sp_fii,aes(`Chg_ in FII _Hold _3Yr`,Stock_Price_Return_5yrs))+
  geom_point()+
  geom_smooth(method="lm",se=1)+
  theme_classic()+
  annotate("text",x=c(0,0),y=c(185,200),label=c("p-value: 0.1549","Adjusted R-squared:  0.01176  "))


sp_fii<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fii
library(ggplot2)
ggplot(sp_fii,aes(Stock_Price_Return_5yrs,`Chg_ in FII _Hold _3Yr`))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()

sp_fii<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Chg_ in FII _Hold _3Yr")]
sp_fii
library(ggplot2)
ggplot(sp_fii,aes(Stock_Price_Return_5yrs,`Chg_ in FII _Hold _3Yr`))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()

eps<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","EPS_Change_5yrs")]
eps
library(ggplot2)
ggplot(eps,aes(Stock_Price_Return_5yrs,EPS_Change_5yrs))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()

eps<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","EPS_Change_5yrs")]
eps
library(ggplot2)
ggplot(eps,aes(Stock_Price_Return_5yrs,EPS_Change_5yrs))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()

sg<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sg
ggplot(sg,aes(`Sales_ Growth_5yrs`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
 
  theme_bw()




sg<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Sales_ Growth_5yrs")]
sg
ggplot(sg,aes(`Sales_ Growth_5yrs`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  
  theme_bw()

avgpat<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Avg _PAT_5yrs")]
avgpat
ggplot(avgpat,aes(`Avg _PAT_5yrs`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()

avgpat<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Avg _PAT_5yrs")]
avgpat
ggplot(avgpat,aes(`Avg _PAT_5yrs`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()

ebidt<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","EBIDT_5yrs")]
ebidt
ggplot(ebidt,aes(EBIDT_5yrs,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()


ebidt<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","EBIDT_5yrs")]
ebidt
ggplot(ebidt,aes(EBIDT_5yrs,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()

roe<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","ROE_5Yrs")]
roe
ggplot(roe,aes(ROE_5Yrs,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()


roe<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","ROE_5Yrs")]
roe
ggplot(roe,aes(ROE_5Yrs,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()


debt<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","Debt/Eq")]
debt
ggplot(debt,aes(`Debt/Eq`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()


debt<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","Debt/Eq")]
debt
ggplot(debt,aes(`Debt/Eq`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()

mcap<-dfclean[dfclean$LCMC == "LC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
mcap
ggplot(mcap,aes(`M.Cap/Sales`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()

mcap<-dfclean[dfclean$LCMC == "MC",c("Stock_Price_Return_5yrs","M.Cap/Sales")]
mcap
ggplot(mcap,aes(`M.Cap/Sales`,Stock_Price_Return_5yrs))+
  geom_boxplot()+
  theme_bw()
#------------------------- Prediction
# Loading the necessary libraries
library(dplyr)
install.packages("lattice")
library(lattice)
library(ggplot2)
library(corrplot)
install.packages("foreach")
library(foreach)
install.packages("Matrix")
install.packages("glmnet")
library(glmnet)
install.packages("rpart")
library(rpart)
install.packages("randomForest")
library(randomForest)
library(caret)

# Read Data
library(readxl)
dfclean<-read_xlsx("C:/PGCBA project/Testing file.xlsx")
dfclean
dim(dfclean)

# Transforming Data

dfclean$Stock_Price_Return_5yrs<-as.numeric(dfclean$Stock_Price_Return_5yrs)
dfclean$Stock_Price_Return_5yrs<-as.factor(dfclean$Stock_Price_Return_5yrs)
str(dfclean$Stock_Price_Return_5yrs)

# Validation dataset
library(caret)
set.seed(175)

Validation_index<-createDataPartition(dfclean$Stock_Price_Return_5yrs,p=.2,list=FALSE
                                      )

#------------------------------------------ End of Prediction
#Describe data
install.packages("Hmisc")
library(Hmisc)
install.packages("psych")
library(psych)
describe(dfclean,na.rm=TRUE)
colMax<-function(dfclean)sapply(dfclean,max,na.rm=T)
colMax(dfclean)
colMin<-function(dfclean)sapply(dfclean,min,na.rm=T)
colMin(dfclean)
colSort<-function(dfclean)sapply(dfclean,sort)
colnames(dfclean)
rownames(dfclean)



# Bar graph
barplot(dfclean$`MarCap _Rs.CR`,col="blue",border="black")
barplot(dfclean,xlab="`MarCap _Rs.CR`",ylab="`Sales Rs.Cr.`")
barplot(dfclean$`Sales Rs.Cr.`,col="blue",border="black")

# LinePlot
plot(dfclean$CMPRs.,type='l')
boxplot(dfclean$CMPRs.,col="red")

# Scatter plot
plot(dfclean$CMPRs.,pch=1,col="orange")

# Histo gram
hist(dfclean$`OPM %`,col="pink",border="black")

plot(dfclean$`OPM %`,dfclean$`Sales Rs.Cr.`,type='l',xlab="`OPM %`",ylab="`Sales Rs.Cr.`")
plot(dfclean$`OPM %`,dfclean$`P/E`,type='l',xlab="`OPM %`",ylab="`P/E`")

library(tidyverse)
library(ggplot2)
dfclean<-ggplot(dfclean,aes(LCMC))
dfclean+geom_bar(aes(group=LCMC,color=LCMC))+theme(legend.position="None")
ggplot(dfclean,aes(x=Name,y=`MarCap _Rs.CR`))+
  geom_bar(stat="identity",position="dodge")

  
# Reading specific columnns from dataframe (Correct code)
dfclean[c(2,4,5)]

# Reading specific rows from data frame by reading row name (s.no 1,2,3,4,....) (Correct code)
dfclean[c('1','3','106','205'),]





# Reading maximum values company wise

# Exporting data to specific worsheet in excel file (not working)
#install.packages("writexl")
#library(writexl)
#write_xlsx(dfclean,"C:/PGCBA project/Testing file.xlsx",sheetName="CD",append=TRUE)

# Reading all worksheeet from excel file (Correct code)
#allworksheet<-"C:/PGCBA project/Market Data.xlsx"
#allworksheet
#install.packages("rio")
#library(rio)
#data<-import_list(allworksheet)
#data

# Readling from seperate file tlc and tmc


library(readxl)
df_tlc<-read_xlsx("C:/PGCBA project/TLC_data.xlsx")
df_tlc
df_tmc<-read_xlsx("C:/PGCBA project/TMC_data.xlsx")
df_tmc

# Bar Plot for TLC having Market Cap
barplot(df_tlc$`MarCap _Rs.CR`,col="blue",border="black")


# Bar Plot for TMC having Market Cap
barplot(df_tmc$`MarCap _Rs.CR`,col="blue",border="black")

# Bar Plot for TLC having Sales
barplot(df_tlc$`Sales Rs.Cr.`,col="blue",border="black")

# Bar Plot for TMC having Sales
barplot(df_tmc$`Sales Rs.Cr.`,col="blue",border="black")

# Histogram for TLC having PE
hist(df_tlc$`P/E`,col="pink",border="black")
 
# Histogram for TMC having PE
hist(df_tmc$`P/E`,col="pink",border="black")

# Line plot for TLC having PE
plot(df_tlc$`P/E`,type='l')

# Line plot for TMC having PE
plot(df_tmc$`P/E`,type='l')

# Box plot for TLC having Ind PE
boxplot(df_tlc$`Ind PE`,col="red")

## Box plot for TMC having Ind PE
boxplot(df_tmc$`Ind PE`,col="red")

# plot Scatter plot for TLC having IND PE
plot(df_tlc$`Ind PE`,pch=1,col="red")

# plot Scatter plot for TMC having IND PE
plot(df_tmc$`Ind PE`,pch=1,col="red")

# How to read topics from R: Use ? before the subject matter. See below example.
#? barplot 

