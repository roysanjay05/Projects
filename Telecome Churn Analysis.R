#----------------------------------------------------
# Codes are below and Answers at the Bottom ################

#------------------------------------------------------------------------


telecomfinal <- read.csv("C:/Data Science with R/Assignments/Graded Assignments/Topic 13 -  Final Case Study Course Wrap up/telecomfinal.csv")
summary(telecomfinal)

#- removing variables with very high missing values-----------

data<-telecomfinal[,-c(12,33,34,36,46,47,48,49,52,53,54,55,61,62,63,64,66,72)]


summary(data)


# creating subset of vaiables containing "mou" (similar gropus for better understanding) to shortlist varaibles for model

sub_mou<-select(data,contains("mou"),churn)

summary(sub_mou)

# removing NA values 
data1<-na.omit(data)
summary(data1)

# removing non relavant variables car_buy, CSA, cust id

data1<-data1[-49,-50,-61]

summary(sub_mou)

#============================================================================================
#=============================================================================================

# mou_mean- data cleaning
summary(data1$mou_Mean)
quantile(data1$mou_Mean,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_mean1
plot(mou_mean1$dec,mou_mean1$n) 


# 95% of values are below below 1582 which is near Q3+1.5*IQR (Inter Quartile Range)
# imputing values with Q3+1.5*IQR

sub<-715+1.5*IQR(data1$mou_Mean,na.rm=TRUE) # 715 is Q3 value
data1$mou_Mean[data1$mou_Mean>2469]<-sub
# imputing the extreem value (1% in the population) with Q3+1.5*IQR

summary(data1$mou_Mean)
hist(data1$mou_Mean)

quantile(data1$mou_Mean,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_mean1
plot(mou_mean1$dec,mou_mean1$n)

# this shows trend, hence selected "mou_mean"for model ********

#======================================================================================

# selecting more variables which contain "mou" using correlation and VIF values
# Then applying the data imputation to save effort in data preparation

# correlation matrix

mou_cor<-cor(sub_mou[,c(1:11)])
mou_cor<-round(mou_cor,3)

# since mou_Mean is already identified variable which is useful for model, checking other mou
# variables for correlation and multicollinearity

# linear model with Mou_Mean and other variables determine correlation
# vif will determine the magnitude of correlation and dropping the variables >10 vif values
mou_reg<-lm(data=sub_mou,mou_Mean~mou_Range+change_mou+mou_opkv_Range+ccrndmou_Range+ovrmou_Mean+avg3mou+
              avgmou+avg6mou+mou_pead_Mean+adjmou)
vif(mou_reg)


#=======================================================================

# mou_Range- data cleaning
summary(data1$mou_Range)
quantile(data1$mou_Range,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_Range1
plot(mou_Range1$dec,mou_Range1$n) 


sub<-471+1.5*IQR(data1$mou_Range,na.rm=TRUE) # 1008 
data1$mou_Range[data1$mou_Range>2033]<-sub 
# only 1% value is extreemly high and higher than 2033, hence imputing it with Q3+1.5*IQR=1008

summary(data1$mou_Range)
hist(data1$mou_Range)


quantile(data1$mou_Range,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_Range1
plot(mou_Range1$dec,mou_Range1$n) 

#====================================================================================


# change_mou- data cleaning
summary(data1$change_mou)
quantile(data1$change_mou,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->change_mou1
plot(change_mou1$dec,change_mou1$n) 

sub<-64.5+1.5*IQR(data1$change_mou,na.rm=TRUE) # 282 
data1$change_mou[data1$change_mou>724]<-sub 
# only 1% value is extreemly high and higher than 724(99% values below this), hence imputing it with Q3+1.5*IQR=282

summary(data1$change_mou)
hist(data1$change_mou)


quantile(data1$change_mou,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->change_mou1
plot(change_mou1$dec,change_mou1$n)

#=====================================================================================


# mou_opkv_Range- data cleaning
summary(data1$mou_opkv_Range)
quantile(data1$mou_opkv_Range,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_opkv_Range1
plot(mou_opkv_Range1$dec,mou_opkv_Range1$n) 

sub<-136.4+1.5*IQR(data1$mou_opkv_Range,na.rm=TRUE) # 317.3 
data1$mou_opkv_Range[data1$mou_opkv_Range>857]<-sub 
# only 1% value is extreemly high and higher than 857(99% values below this), hence imputing it with Q3+1.5*IQR=317.3

summary(data1$mou_opkv_Range)
hist(data1$mou_opkv_Range)


quantile(data1$mou_opkv_Range,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_opkv_Range1
plot(mou_opkv_Range1$dec,mou_opkv_Range1$n)

#=======================================================================================


# ccrndmou_Range- data cleaning
summary(data1$ccrndmou_Range)
hist(data1$ccrndmou_Range)
quantile(data1$ccrndmou_Range,p=c(1:100)/100,na.rm=TRUE)

# 55% data is having value zero, hence covrting this value in dummy binary variable
# ccr_dummy=0 if 0 and ccr_dummy=1 if >0

data1$ccrndmou_Range<-ifelse(data1$ccrndmou_Range==0,0,1)
head(data1$ccrndmou_Range)

#=====================================================================================


# ovrmou_Mean- data cleaning
summary(data1$ovrmou_Mean)
hist(data1$ovrmou_Mean)
quantile(data1$ovrmou_Mean,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->ovrmou_Mean1
plot(ovrmou_Mean1$dec,ovrmou_Mean1$n) 

sub<-40+1.5*IQR(data1$ovrmou_Mean,na.rm=TRUE) # 100 
data1$ovrmou_Mean[data1$ovrmou_Mean>427.3]<-sub 
# only 1% value is extreemly high and higher than 427.3(99% values below this), hence imputing it with Q3+1.5*IQR=100

summary(data1$ovrmou_Mean)
hist(data1$ovrmou_Mean)


quantile(data1$mou_opkv_Range,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_opkv_Range1
plot(mou_opkv_Range1$dec,mou_opkv_Range1$n)

#======================================================================================


# avgmou- data cleaning
summary(data1$avgmou)
hist(data1$avgmou)
quantile(data1$avgmou,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->avgmou1
plot(avgmou1$dec,avgmou1$n) 

sub<-659+1.5*IQR(data1$avgmou,na.rm=TRUE) # 1382 
data1$avgmou[data1$avgmou>2060]<-sub 
# only 1% value is extreemly high and higher than 2060(99% values below this), hence imputing it with Q3+1.5*IQR=1382

summary(data1$avgmou)
hist(data1$avgmou)


quantile(data1$avgmou,p=c(1:100)/100,na.rm=TRUE)

data1%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->avgmou1
plot(avgmou1$dec,avgmou1$n)

#======================================================================================

summary(data1$adjmou)
hist(data1$adjmou)

quantile(data1$adjmou,p=c(1:100)/100,na.rm=TRUE)

new_Adjmou<-sqrt(data1$adjmou)
summary(new_Adjmou)
hist(log(new_Adjmou))

quantile(new_Adjmou,p=c(1:100)/100,na.rm=TRUE)



# transformation of data
SQ_ovrmou<-sqrt(data1$ovrmou_Mean)

summary(SQ_ovrmou)
hist(SQ_ovrmou)

log_ovrmou<-sqrt(log(data1$ovrmou_Mean))
summary(log_ovrmou)
hist(log_ovrmou)




#=======================================================================================
#=======================================================================================

par(mfrow=c(4,3))
hist(sub_mou$mou_Mean)
hist(sub_mou$mou_Range)
hist(sub_mou$change_mou)
hist(sub_mou$mou_opkv_Range)
hist(sub_mou$ccrndmou_Range)
hist(sub_mou$ovrmou_Mean)
hist(sub_mou$avg3mou)
hist(sub_mou$avgmou)
hist(sub_mou$avg6mou)
hist(sub_mou$mou_pead_Mean)
hist(sub_mou$adjmou)
dev.off()

# all are skewed varaiables and need data transformation

summary(data1$mou_Mean)
hist(data1$mou_Mean)

hist(sqrt(data1$mou_Mean))
summary(sqrt(data1$mou_Mean))
data1$mou_Mean1<-(sqrt(data1$mou_Mean))
#------------------------------------
summary(data1$mou_Range)
hist(data1$mou_Range)

hist(log10(data1$mou_Range+1))
summary(log10(data1$mou_Range+1))
data1$mou_Range1<-log10(data1$mou_Range+1)

#------------------------------------

summary(sub_mou$change_mou)
hist(sub_mou$change_mou)

hist(log10(sub_mou$change_mou))# this creates lot of 'NaNs" values hence converting into binary
# is value <0 as '0' and >0 as '1'this 
# this shows if any change in mou in current month as compare to last 3 months average

data1$change_mou1<-ifelse(data1$change_mou>=0,1,0)
#-------------------------------------

summary(sub_mou$mou_opkv_Range)
hist(sub_mou$mou_opkv_Range)

hist(log(sub_mou$mou_opkv_Range+1),breaks = 50)
data1$mou_opkv_Range1<-log(data1$mou_opkv_Range+1)
summary(data1$mou_opkv_Range1)

#----------------------------------------

summary(sub_mou$ccrndmou_Range)
hist(sub_mou$ccrndmou_Range)
hist(log(sub_mou$ccrndmou_Range+1)) # values are skewed right and not normal due to very high zero values

quantile(data1$ccrndmou_Range,p=c(1:100)/100,na.rm=TRUE)
# 55 % data values are zero, thic can be converted into dummy variable 0 means 0 and 1 means >0

data1$ccrndmou_Range1<-as.factor(ifelse(data1$ccrndmou_Range>0,1,0))

#---------------------------------------------------------------

summary(sub_mou$ovrmou_Mean)
hist(sub_mou$ovrmou_Mean)
hist(log(sub_mou$ovrmou_Mean+1),breaks=50)

quantile(data1$ovrmou_Mean,p=c(1:100)/100,na.rm=TRUE)

data1$ovrmou_Mean1<-log(data1$ovrmou_Mean+1)
summary(data1$ovrmou_Mean1)

quantile(data1$ovrmou_Mean1,p=c(1:100)/100,na.rm=TRUE)
# very high number of zero values and still data not normal, converting into dummy

data1$ovrmou_Mean1<-ifelse(data1$ovrmou_Mean>100,"high",ifelse(data1$ovrmou_Mean>0,"medium","NIL"))
#-------------------------------------------------------------

summary(sub_mou$avg3mou)
hist(sub_mou$avg3mou)
hist(sqrt(sub_mou$avg3mou))

data1$avg3mou1<-sqrt(data1$avg3mou)

summary(data1$avg3mou1)

#------------------------------------------------------------------

summary(sub_mou$avgmou)
hist(sub_mou$avgmou)
hist(sqrt(sub_mou$avgmou))

data1$avgmou1<-sqrt(data1$avgmou)
#-----------------------------------------------------------------
summary(sub_mou$avg6mou)
hist(sub_mou$avg6mou)

hist(sqrt(sub_mou$avg6mou))
data1$avg6mou1<-sqrt(data1$avg6mou)
#--------------------------------------------------

summary(sub_mou$mou_pead_Mean)
hist(sub_mou$mou_pead_Mean)
hist(log(sub_mou$mou_pead_Mean+1)) # infine values with log and skewed will +1 to log transformation

quantile(data1$mou_pead_Mean,p=c(1:100)/100,na.rm=TRUE)
# 92 % observations have zero value, hence converting into dummy categorical variable
# peak mean call mou = 1 means called during peak, 0 means not called suring peak

data1$mou_pead_Mean1<-ifelse(data1$mou_pead_Mean>1,1,0)
#---------------------------------------------------
summary(sub_mou$adjmou)
hist(sub_mou$adjmou)
hist(log(sub_mou$adjmou+1))
data1$adjmou1<-log(data1$adjmou+1)
summary(data1$adjmou1)
#--------------------------------------------------

# 
# variables for correlation and multicollinearity

# vif will determine the magnitude of correlation and dropping the variables >10 vif values

vif_mod<-glm(data=data1,churn~mou_Mean1+mou_Range1+change_mou1+mou_opkv_Range1+ccrndmou_Range1+ovrmou_Mean1+avg3mou1+
               avgmou1+avg6mou1+mou_pead_Mean1+adjmou1, family="binomial")


vif(vif_mod)

# VIF values shows the variables to drop due to multicollinearity are mou_Mean1, avg3mou1,avg6mou1 & avgmou1
################=====================##############################============================
summary(data1)
# subsetting varaibles related to sepnd, revenue together for ease of handling and shortlisting

sub_rev<-select(data1,contains("rev"),churn,totmrc_Mean)

summary(sub_rev)

par(mfrow=c(2,4))
hist(sub_rev$rev_Range)
hist(sub_rev$ovrrev_Mean)
hist(sub_rev$rev_Mean)
hist(sub_rev$totrev)
hist(sub_rev$adjrev)
hist(sub_rev$avgrev)
hist(sub_rev$totmrc_Mean)
dev.off()

# all the variables under subset sub_rev are skewed and need transformation

summary(sub_rev$rev_Range)
hist(sub_rev$rev_Range)

hist(log10(sub_rev$rev_Range+1))
summary(log10(sub_rev$rev_Range+1))

data1$rev_Range1<-log10(data1$rev_Range+1)
hist(data1$rev_Range1)
#-----------------------------------

summary(sub_rev$ovrrev_Mean)
hist(sub_rev$ovrrev_Mean)

hist(log10(sub_rev$ovrrev_Mean+1)) # histogram still not near normal

quantile(data1$ovrrev_Mean,p=c(1:100)/100,na.rm=TRUE)
# 53% of data have zero values, hence converting into dummy binary variable
# ovrrev is 0 means no ovrage Revenue and 1 means ovrage revenue >0
data1$ovrrev_Mean1<-ifelse(data1$ovrrev_Mean>0,1,0)

#----------------------------------

summary(sub_rev$rev_Mean)
hist((sub_rev$rev_Mean),breaks=50)

hist(log10(sub_rev$rev_Mean^2)+1)

data1$rev_Mean1<- log10((data1$rev_Mean^2)+1)

summary(data1$rev_Mean1)
hist(data1$rev_Mean1)

#----------------------------------
summary(sub_rev$totrev)
hist(sub_rev$totrev)

hist(log10(sub_rev$totrev),breaks=20)
data1$totrev1<-log10(data1$totrev)
hist(data1$totrev1)
#----------------------------------

summary(sub_rev$adjrev)
hist(sub_rev$adjrev)

hist(log10(sub_rev$adjrev))
data1$adjrev1<-log10(data1$adjrev)
hist(data1$adjrev1)
summary(data1$adjrev1)
#------------------------------

summary(sub_rev$avgrev)
hist(sub_rev$avgrev)

hist(log10(sub_rev$avgrev))
data1$avgrev1<-log10(data1$avgrev)
hist(data1$avgrev1)
summary(data1$avgrev1)
#----------------------------

summary(sub_rev$totmrc_Mean)
hist(sub_rev$totmrc_Mean)

#--------------------------

# check variables for correlation and multicollinearity

# vif will determine the magnitude of correlation and dropping the variables >10 vif values

vif_mod1<-glm(data=data1,churn~totmrc_Mean+avgrev1+adjrev1+totrev1+rev_Mean1+ovrrev_Mean1+rev_Range1, family="binomial")


vif(vif_mod1)

# VIF values shows the variables to drop due to multicollinearity are mou_Mean1, avg3mou1,avg6mou1 & avgmou1
# drop variables  adjrev1,totrev1,rev_Mean1
################===============####################===================

summary(data1)
sub_call<-data1[,c(6:8,13:16,18,22,23,26,27,29,44,48,50,51,55)]

par(mfrow=c(4,4))
hist(sub_call$drop_blk_Mean)
hist(sub_call$drop_vce_Range)
hist(sub_call$owylis_vce_Range)
hist(sub_call$custcare_Mean)
hist(sub_call$callwait_Mean)
hist(sub_call$iwylis_vce_Mean)
hist(sub_call$callwait_Range)
hist(sub_call$adjqty)
hist(sub_call$comp_vce_Mean)
hist(sub_call$plcd_vce_Mean)
hist(sub_call$avg3qty)
hist(sub_call$avg6qty)
hist(sub_call$avgqty)
hist(sub_call$roam_Mean)
hist(sub_call$da_Mean)
hist(sub_call$da_Range)
hist(sub_call$drop_vce_Mean)
dev.off()
# all the variables are skewed right and need transformation
#-----------------------

summary(sub_call$drop_blk_Mean)
hist(sub_call$drop_blk_Mean)

hist(log10(sub_call$drop_blk_Mean+1))
summary((log10(sub_call$drop_blk_Mean+1)))
data1$drop_blk_Mean1<-log10(data1$drop_blk_Mean+1)
summary(data1$drop_blk_Mean1)

#------------------------

summary(sub_call$drop_vce_Range)
hist(sub_call$drop_vce_Range)

hist(log10(sub_call$drop_vce_Range),breaks=50) # no option is making data normal
summary(log10(sub_call$drop_vce_Range+1))

# looking at percentile distribution of data
quantile(sub_call$drop_vce_Range,p=c(1:100)/100,na.rm=TRUE)
# converting data into dummy varaibles
# Low>=10, Med=11-50, high=>50
data1$drop_vce_Range1<-ifelse(data1$drop_vce_Range<=10,"Low",ifelse(data1$drop_vce_Range>11&data1$drop_vce_Range<50,"Medium","High"))

table(data1$drop_vce_Range1,data1$churn)
#-------------------------

summary(sub_call$owylis_vce_Range)
hist(sub_call$owylis_vce_Range)

hist(log10(sub_call$owylis_vce_Range)^3,breaks=50) # transformation not making data normal
quantile(sub_call$owylis_vce_Range,p=c(1:100)/100,na.rm=TRUE)
data1$owylis_vce_Range1<-ifelse(data1$owylis_vce_Range<=10,"Low",ifelse(data1$owylis_vce_Range>11&data1$owylis_vce_Range<50,"Medium","High"))

table(data1$owylis_vce_Range1,data1$churn)
#-----------------------

summary(sub_call$custcare_Mean)
hist(sub_call$custcare_Mean)

hist(log10(sub_call$custcare_Mean+1))  # data not normal after transformatio
quantile(sub_call$custcare_Mean,p=c(1:100)/100,na.rm=TRUE)
# 61% data have value zero, converting into dummy variable
# custcare_Mean 0 if value '0' and '1' if > 0

data1$custcare_Mean1<-ifelse(data1$custcare_Mean>0,1,0)
table(data1$custcare_Mean1,data1$churn)
#------------------------

summary(sub_call$callwait_Mean)
hist(sub_call$callwait_Mean)
quantile(sub_call$callwait_Mean,p=c(1:100)/100,na.rm=TRUE)
# 56% data values are zero, converting into dummy
# call wait 0 = 0 and >0 = 1
data1$callwait_Mean1<-ifelse(data1$callwait_Mean>0,1,0)
table(data1$callwait_Mean1,data1$churn)
#----------------------------

summary(sub_call$iwylis_vce_Mean)
hist(sub_call$iwylis_vce_Mean)
quantile(sub_call$iwylis_vce_Mean,p=c(1:100)/100,na.rm=TRUE)

hist(log10(sqrt(sub_call$iwylis_vce_Mean)+1),breaks=50)
data1$iwylis_vce_Mean1<-ifelse(data1$iwylis_vce_Mean<=10,"Low",ifelse(data1$iwylis_vce_Mean>10&data1$iwylis_vce_Mean<40,"Medium","High"))

table(data1$iwylis_vce_Mean1,data1$churn)
#----------------------------

summary(sub_call$callwait_Range)
hist(sub_call$callwait_Range)
quantile(sub_call$callwait_Range,p=c(1:100)/100,na.rm=TRUE)
# 50 % data do show call waiting zero, converting data into dummy categorical
# call wait mean = 0 when no wait and 1 if wait >0

data1$callwait_Range1<-ifelse(data1$callwait_Range>0,1,0)

table(data1$callwait_Range1,data1$churn)

#----------------------

summary(sub_call$adjqty)
hist(sub_call$adjqty)

hist(log10(sub_call$adjqty+1))
data1$adjqty1<-log10(data1$adjqty+1)

summary(data1$adjqty1)
hist(data1$adjqty1)
#-----------------------
# defining new variables as % call completed.

sub_call$perc_failcall<-ifelse(sub_call$plcd_vce_Mean==0,0,(1-(sub_call$comp_vce_Mean/sub_call$plcd_vce_Mean)))

summary(sub_call$perc_failcall)
hist(sub_call$perc_failcall)

hist(log10(sub_call$perc_failcall+1))
summary(log10(sub_call$perc_failcall+1))

# taking clue & creating new variable in original data set
# transforming new variables

data1$perc_failcall<-ifelse(data1$plcd_vce_Mean==0,0,(1-(data1$comp_vce_Mean/data1$plcd_vce_Mean)))
data1$perc_failcall1<-log10(data1$perc_failcall+1)

summary(data1$perc_failcall1)
#-----------------------------------


summary(sub_call$avg3qty)
hist(sub_call$avg3qty)

hist(log10(sub_call$avg3qty+1))

summary(log10(sub_call$avg3qty+1))
data1$avg3qty1<-log10(data1$avg3qty+1)
summary(data1$avg3qty1)
#-------------------------------

summary(sub_call$avg6qty)
hist(sub_call$avg6qty)

hist(log10(sub_call$avg6qty+1))
data1$avg6qty1<-log10(data1$avg6qty+1)
summary(data1$avg6qty1)
hist(data1$avg6qty1)
#------------------------------

summary(sub_call$avgqty)
hist(sub_call$avgqty)

hist(log10(sub_call$avgqty+1))

data1$avgqty1<-log10(data1$avgqty+1)
hist(data1$avgqty1)
#------------------------------
summary(sub_call$roam_Mean)
hist(sub_call$roam_Mean)

quantile(sub_call$roam_Mean,p=c(1:100)/100,na.rm=TRUE)
# 77% data points have zero value
# converting variable into dummy categorical variable
# roam_Mean1= 0 when value is zero and 1 when value >0

data1$roam_Mean1<-ifelse(data1$roam_Mean>0,1,0)

table(data1$roam_Mean1,data1$churn)
#------------------------------

summary(sub_call$da_Mean)
hist(sub_call$da_Mean, breaks=50)

hist(log10(sub_call$da_Mean))
quantile(sub_call$da_Mean,p=c(1:100)/100,na.rm=TRUE)

# 58% data is zero value and 99% data is below 10, hence converting into dummy variable

data1$da_Mean1<-ifelse(data1$da_Mean>0,1,0)
#----------------------------

summary(sub_call$da_Range)
hist(sub_call$da_Range)

hist(log10(sub_call$da_Range+1))

quantile(sub_call$da_Range,p=c(1:100)/100,na.rm=TRUE)
# 63% data is zero value, converting into dummy variables
data1$da_Range1<-ifelse(data1$da_Range>0,1,0)

table(data1$da_Mean1,data1$da_Range1) # both variables are giving nearly same information, can be dropped

#-------------------------

summary(sub_call$drop_vce_Mean)
hist(sub_call$drop_vce_Mean)

hist(log10(sub_call$drop_vce_Mean+1))
summary(log10(sub_call$drop_vce_Mean+1))

data1$drop_vce_Mean1<-log10(sub_call$drop_vce_Mean+1)
summary(data1$drop_vce_Mean1)
#-----------------------------

# check variables for correlation and multicollinearity

# vif will determine the magnitude of correlation and dropping the variables >10 vif values

vif_mod2<-glm(data=data1,churn~drop_blk_Mean1+drop_vce_Range1+owylis_vce_Range1+custcare_Mean1+
                callwait_Mean1+iwylis_vce_Mean1+callwait_Range1+adjqty1+perc_failcall1+
              avg3qty1+avg6qty1+avgqty1+roam_Mean1+da_Mean1+da_Range1+drop_vce_Mean1, family="binomial")
                


vif(vif_mod2)

# as per vif value, variables to drop are da_Mean1, da_Range1
#=========================================================================

rest_num<-data1[,c(10:12,35:40,45)]

summary(rest_num)

par(mfrow=c(4,3))
hist(rest_num$months)
hist(rest_num$totcalls)
hist(rest_num$eqpdays)
hist(rest_num$age1)
hist(rest_num$age2)
hist(rest_num$models)
hist(rest_num$hnd_price)
hist(rest_num$actvsubs)
hist(rest_num$uniqsubs)
hist(rest_num$recv_sms_Mean)

dev.off()

#--------------------
summary(rest_num$months)
hist((rest_num$months),breaks=50)

hist(log10(rest_num$months),breaks=50)

summary(log10(rest_num$months))

data1$months1<-log10(data1$months)

summary(data1$months1)

#-------------------------

summary(rest_num$totcalls)
hist(rest_num$totcalls)

hist(log10(rest_num$totcalls+1))
summary(log10(rest_num$totcalls+1))

data1$totcalls1<-log10(data1$totcalls+1)
summary(data1$totcalls1)
#----------------------------

summary(rest_num$eqpdays)
hist(rest_num$eqpdays)

rest_num$eqpdays<-ifelse(rest_num$eqpdays<0,mean(rest_num$eqpdays),rest_num$eqpdays)

summary(rest_num$eqpdays)
hist(rest_num$eqpdays)

hist(sqrt(rest_num$eqpdays))
summary(sqrt(rest_num$eqpdays)) # taking clue and making changes in main data

data1$eqpdays<-ifelse(data1$eqpdays<0,mean(data1$eqpdays),data1$eqpdays)
data1$eqpdays1<-sqrt(data1$eqpdays)

summary(data1$eqpdays1)

#---------------------------


summary(rest_num$age1) # age1= 0 is visible 
hist(rest_num$age1)


quantile(rest_num$age1,p=c(1:100)/100,na.rm=TRUE)

# 27 % data shows with zero age and using phone not possible
# imputing with mean age

rest_num$age1<-ifelse(rest_num$age1<=0,mean(rest_num$age1),rest_num$age1)

summary(rest_num$age1)
hist(rest_num$age1)

hist(sqrt(rest_num$age1))
summary(sqrt(rest_num$age1))

# taking clue from here and updating main data

data1$age1<-ifelse(data1$age1<=0,mean(data1$age1),data1$age1)
data1$age11<-sqrt(data1$age1)
summary(data1$age11)
#----------------------------

summary(rest_num$age2)
hist(rest_num$age2)

quantile(rest_num$age2,p=c(1:100)/100,na.rm=TRUE)
# 51 % data has zero value, imputing with Mean
# dropping this variable as do not see any business logic which lead churn
#-----------------------------

table(data1$churn,data1$models)
class(data1$models)

summary(rest_num$models)
hist(rest_num$models)
# since these are sicreet variables, converting dummy variables
# models issued 1-3 - Models_Low, 4-6- Models_Med, >6 Models_High

data1$models1<-ifelse(data1$models<=3,"Models_Low",ifelse(data1$models>3 & rest_num$models<=6,"Models_Med","Model_High"))

table(data1$models1,data1$churn)
#--------------------------------

summary(rest_num$hnd_price)
hist(rest_num$hnd_price,breaks=20)

hist(sqrt(rest_num$hnd_price))
quantile(rest_num$hnd_price,p=c(1:100)/100,na.rm=TRUE)

# data shows the values are in buckets 29.9/ 59.9/ 79.9/ 99.9/ 129.9/ 149.9/ 199.9/ 499.9 
# creating dummy variables

data1$hnd_price1<-ifelse(data1$hnd_price<80,"Low_price",ifelse(data1$hnd_price>80 & data1$hnd_price<=150,"Med_price","High_price"))

table(data1$hnd_price1, data1$churn)

#------------------------------

summary(rest_num$actvsubs)
hist(rest_num$actvsubs)
quantile(rest_num$actvsubs,p=c(1:100)/100,na.rm=TRUE)
# data shows 71% values are '1' 14% values are '2' and rest above '3' converting into dummy variables

data1$actvsubs1<-ifelse(data1$actvsubs==1,"One",ifelse(data1$actvsubs==2,"two","3&above"))

table(data1$actvsubs1,data1$churn)
#--------------------------------

summary(rest_num$uniqsubs)
quantile(data1$uniqsubs,p=c(1:100)/100,na.rm=TRUE)
data1$uniqsubs1<-ifelse(data1$uniqsubs>=3,">3",data1$uniqsubs)

#-----------------------------

summary(rest_num$recv_sms_Mean)
quantile(data1$recv_sms_Mean,p=c(1:100)/100,na.rm=TRUE)
# only 1% values are above zero, hence dropping variable.
#-----------------------------------

char_data<-data1[,c(30:34,49,48)]

table(char_data$crclscod,char_data$churn)

#-------------------------

summary(char_data$crclscod)


char_data$crclass<-substr(char_data$crclscod,1,1)
table(char_data$crclass,char_data$churn)
char_data$crcls<-ifelse(char_data$crclass== "A"|char_data$crclass=="B"|char_data$crclass=="C","crcl_best",ifelse(char_data$crclass=="D"|char_data$crclass=="E"|char_data$crclass=="G","crcl_good","crcl_bad"))

# converting data into three levels
# credic class code A is best and Z is worst, clubbing A to C as best, D to G as good and rest all as bad
data1$crclass<-substr(data1$crclscod,1,1)
data1$crclass<-ifelse(data1$crclass== "A"|data1$crclass=="B"|data1$crclass=="C","crcl_best",ifelse(data1$crclass=="D"|data1$crclass=="E"|data1$crclass=="G","crcl_good","crcl_bad"))

table(data1$crclass,data1$churn)

#----------------

table(char_data$marital,char_data$churn) # retaining 5 levels

#-----------------

table(char_data$ethnic,char_data$churn) # retaining the levels

#-------------

table(char_data$car_buy,char_data$churn) # retaining the levels

#-------------
# creating seperate data set for model

str(data1)
data2<-data1[,-c(1:30,35:47)]
data3<-data2[,-c(6:19,51,52,45)]

# looking data if factors to be created for binary variables
str(data3)

data3$churn<-as.factor(data3$churn)
data3$change_mou1<-as.factor(data3$change_mou1)
data3$mou_pead_Mean1<-as.factor(data3$mou_pead_Mean1)
data3$ovrrev_Mean1<-as.factor(data3$ovrrev_Mean1)
data3$custcare_Mean1<-as.factor(data3$custcare_Mean1)
data3$callwait_Mean1<-as.factor(data3$callwait_Mean1)
data3$roam_Mean1<-as.factor(data3$roam_Mean1)

boxplot(data3$churn)
summary(data3$churn)

#-------------------------------------
# spliting data into train and test data

set.seed(200)
index<-sample(nrow(data3),0.70*nrow(data3),replace=F)
train<-data3[index,]
test<-data3[-index,]
#--------------------------------------

train_mod1<-glm(data=train,churn~.,family="binomial")
summary(train_mod1)

step(train_mod1,direction = "both")

# using the variables suggested by step function run

train_mod10<-glm(formula = churn ~ asl_flag + refurb_new + marital + ethnic + 
      mou_Mean1 + mou_Range1 + mou_opkv_Range1 + ovrmou_Mean1 + 
      avg3mou1 + avgmou1 + rev_Range1 + rev_Mean1 + totrev1 + adjrev1 + 
      avgrev1 + drop_blk_Mean1 + drop_vce_Range1 + custcare_Mean1 + 
      callwait_Mean1 + iwylis_vce_Mean1 + callwait_Range1 + adjqty1 + 
      avg3qty1 + avgqty1 + drop_vce_Mean1 + months1 + totcalls1 + 
      eqpdays1 + age11 + hnd_price1 + actvsubs1 + uniqsubs1 + crclass, 
    family = "binomial", data = train)
summary(train_mod10)

# creating dummy variables for partaially significant factor variables and dropping insignificant

train$ethnic_C<-ifelse(train$ethnic=="C",1,0)
train$ethnic_N<-ifelse(train$ethnic=="N",1,0)
train$ethnic_S<-ifelse(train$ethnic=="S",1,0)
train$ethnic_Z<-ifelse(train$ethnic=="Z",1,0)

train$drop_vce_Range1_low<-ifelse(train$drop_vce_Range1=="Low",1,0)

train$uniqsubs1_1<-ifelse(train$uniqsubs1=="1",1,0)

train$crclass_good<-ifelse(train$crclass=="crcl_good",1,0)

train$actvsubs1_one<-ifelse(train$actvsubs1=="One",1,0)

# run model with replacing new dummy variables


train_mod11<-glm(formula = churn ~ asl_flag + refurb_new  + ethnic_C+ ethnic_N +ethnic_S +ethnic_Z + 
                   mou_Mean1 + mou_Range1 + mou_opkv_Range1 + ovrmou_Mean1 + 
                   avg3mou1 + avgmou1 + rev_Range1 + rev_Mean1 + totrev1 + adjrev1 + 
                   avgrev1 + drop_blk_Mean1 + drop_vce_Range1_low + custcare_Mean1 + 
                   callwait_Mean1  + callwait_Range1 + adjqty1 + 
                   avg3qty1 + avgqty1 + drop_vce_Mean1 + months1 + totcalls1 + 
                   eqpdays1 + age11 + hnd_price1 + actvsubs1_one + uniqsubs1_1 + crclass_good, 
                 family = "binomial", data = train)
summary(train_mod11)

train_mod12<-glm(formula = churn ~ asl_flag + 
                   mou_Mean1   + ovrmou_Mean1 + 
                     avgmou1+ rev_Range1 + rev_Mean1   + 
                   avgrev1  +
                   perc_failcall1+
                   avg3qty1   +   
                   + age11  + actvsubs1_one + uniqsubs1_1, 
                 family = "binomial", data = train)
summary(train_mod12) 


train_mod13<-glm(formula = churn ~ asl_flag +  mou_Mean1+ 
                     avgmou1+ rev_Range1 + rev_Mean1   +  
                   avgrev1  +perc_failcall1+avg3qty1   + age11  +
                   actvsubs1_one + uniqsubs1_1, family = "binomial", data = train)
                   
                      
summary(train_mod13)                     
                 


#------------------------------------------------------
# creating same variables for test data as well

test$ethnic_C<-ifelse(test$ethnic=="C",1,0)
test$ethnic_N<-ifelse(test$ethnic=="N",1,0)
test$ethnic_S<-ifelse(test$ethnic=="S",1,0)
test$ethnic_Z<-ifelse(test$ethnic=="Z",1,0)

test$drop_vce_Range1_low<-ifelse(test$drop_vce_Range1=="Low",1,0)

test$uniqsubs1_1<-ifelse(test$uniqsubs1=="1",1,0)

test$crclass_good<-ifelse(test$crclass=="crcl_good",1,0)

test$actvsubs1_one<-ifelse(test$actvsubs1=="One",1,0)



###########################---------------


####################################################################################3
#####################################################################################


train_mod14<-glm(formula = churn ~  asl_flag+ refurb_new  +  mou_opkv_Range1+ 
                   mou_Mean1   + ovrmou_Mean1 + mou_Range1+rev_Range1+
                   avg3mou1 + avgmou1  + rev_Mean1 + totrev1 + adjrev1 + 
                   avgrev1    + custcare_Mean1+
                      adjqty1 + drop_vce_Mean1+
                   avg3qty1 + avgqty1  + months1  + totcalls1+
                      eqpdays1+ actvsubs1_one +crclass_good+uniqsubs1_1 , 
                 family = "binomial", data = train)
summary(train_mod14)

################################3
pred1<-predict(train_mod14,type="response",newdata=test)

head(pred1)

table(data3$churn)/nrow(data3)

pred1<-ifelse(pred1>0.241,1,0)

kappa2(data.frame(test$churn,pred1))

confusionMatrix(pred1,test$churn,positive="1")




###########

# ROCR- Area under curve

predB<-prediction(pred1,test$churn)
pref<-performance(predB,measure="tpr",x.measure = "fpr")
plot(pref)

auc<-performance(predB,measure="auc")
auc<-auc@y.values[[1]]
auc


#------------
# calculating the probability of churn 
# creating data fram with churn probability and total revenue to determine target customers
prob1<-predict(train_mod14,type="response",newdata=test)
head(prob1)
target<-data.frame(prob1,test$totrev1)

target$totrev<-10^target$test.totrev1
target<-target[,-2]

target$prob<-ifelse(target$prob1>0.65,"High",ifelse(target$prob1>0.4,"Medium","Low"))
summary(target$totrev)
target$rev<-ifelse(target$totrev>1000,"High",ifelse(target$totrev>400,"Medium","Low"))
table(target$prob,target$prob)


##############################################################################
## ANSWERS ---------------------------...............................

# 1.  What are the top five factors driving likelihood of churn at Mobicom?

# Top five factors are-
# Total Calls, AvgRev,Totalrev, Avg3qty,months

#-------------------------------------------------------------------------------------

# 2.  Validation of survey findings. 
# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  
# b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

# Ans a- Analysis shows the cost and billing is causing churn as mou overage is contributin to churn
#       Network & Service quality is also found to be impating churn as call drop is an issue as shown in analysis
#       drop voice call and customer care call variable shows the impact

# Ans b-Data usage connectivity is not causing significan impact on churn

#-------------------------------------------------------------------------------------

# 3. Would you recommend rate plan migration as a proactive retention strategy?

# Ans- Rate plan migration will be recommended for customers who have high usage and 
#      showing the "mouoverage" which means the minutes of use spilling over the plan and they are spending high
#      This is one of the contributing factor which drives churn

#---------------------------------------------------------------------------------------
# 4. What would be your recommendation on how to use this churn model for prioritisation of customers for a proactive retention campaigns in the future?
# Ans- Analysis shows the customet with high churn probability and high value of total revenue 
#      should be targetd for retention. from test data 794 such customers which are potential
#      (codes above, same way can be calculated from train data)

#5. What would be the target segments for proactive retention campaigns? 
#   Falling ARPU forecast is also a concern and therefore, 
#   Mobicom would like to save their high revenue customers besides managing churn. 
#   Given a budget constraint of a contact list of 20% of the subscriber pool, 
#   which subscribers should prioritized if "revenue saves" is also a priority besides 
#   controlling churn. In other words, controlling churn is the primary objective and
#   revenue saves is the secondary objective.
# Ans- targetd segments of customers will be 
#        - High churn probability of customers with high total revenue
#        - Hi churn probability of customers with overage MOU, such customers to me migrated to new plan
#        - improving the network issues and call drops to improve customer experience







