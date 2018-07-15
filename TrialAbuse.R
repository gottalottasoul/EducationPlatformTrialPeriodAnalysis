###########################################################################
# Trial Abuse analysis
# 6/20/17
###########################################################################

require(CengageFunctions)

#unload all previously loaded packages
#detachAllPackages()

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(readr)
}
if (!require(dplyr)) {
  install.packages('dplyr') # consistent data.frame operations
  require(dplyr)
}
if (!require(purrr)) {
  install.packages('purrr') # consistent & safe list/vector munging
  require(purrr)
}
if (!require(tidyr)) {
  install.packages('tidyr') # consistent data.frame cleaning
  require(tidyr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # date manipulation
  require(lubridate)
}
if (!require(forcats)) {
  install.packages('forcats') # working with factors
  require(forcats)
}
if (!require(ggplot2)) {
  install.packages('ggplot2') # date manipulation
  require(ggplot2)
}
if (!require(scales)) {
  install.packages('scales') # date manipulation
  require(scales)
}
if (!require(here)) {
  install.packages("here") #relative working directory
  require(here)
}

#set working directory
setwd(here())

#######################################################################################
### below packages and code are needed to connect to snowflake                      ###
### snowflake package is deprecated, and relies on older version of dplyr           ###
#######################################################################################
if (!require(rJava)) {
  install.packages("rJava") #relative working directory
  require(rJava)
}

if (!require(devtools)) {
  install.packages('devtools') # date manipulation
  require(devtools)
}
if (!require(dplyr-snowflakedb)) {
  devtools::install_github("snowflakedb/dplyr-snowflakedb") #relative working directory
  require(dplyr-snowflakedb)
}


devtools::install_github("snowflakedb/dplyr-snowflakedb",ref="v0.1.0") #relative working directory

#need an older version of dplyr since db connect stuff was abstracted out to
#dbplyr in more recent versions
install_version("dplyr", version = "0.4.3", repos = "http://cran.us.r-project.org")


options(dplyr.jdbc.classpath = "c:/users/babbenante/downloads/snowflake-jdbc-3.0.21.jar")
options(java.parameters = "-Xmx8048m")

my_db <- src_snowflakedb(user = "babbenante",
                         password = "iD3DH27eNE4M",
                         account = "cengage",
                         opts = list(warehouse = "TEMP_WH",
                                     db = "PROD",
                                     schema = "ZPG"))


abuse_data<-tbl.src_snowflakedb(my_db,"ACTIVITIES_PER_WEEK")

#abuse_data<-tbl.src_snowflakedb(my_db,my_sql)
b<-head(abuse_data)
colnames(abuse_data)

b<-filter(abuse_data,between(HED_ACADEMICYEAR,2015,2017))
c<-collect(b)

#c %>% 
#  group_by(HED_ACADEMICYEAR,HED_ACADEMICTERMOFYEAR,COUR) %>% 
#  summarise(courses=n_distinct(COURSEID))

#d<-c %>% filter(PRODUCTID==180997,COURSEID==695608,PARTYID==7121226)


write_delim(c,"C:/Users/babbenante/OneDrive - Cengage Learning/Documents/rawdata/ActivityUsageRaw.txt",delim="\t")


#######################################################################################
### end snowflake connection                                                        ###
#######################################################################################

#read in the weekly student data and do some data manipulation
ActivityUsageData<- read_delim("C:/Users/babbenante/OneDrive - Cengage Learning/Documents/rawdata/ActivityUsageRaw.txt"
                               ,delim="\t"
                               ,col_types = cols(PRODUCTID=col_character()
                                                 ,COURSEID=col_character()
                                                 ,PARTYID=col_character()
                                                 ,HED_ACADEMICYEAR=col_character()))
ActivityUsageData$YearTerm<-paste0(ActivityUsageData$HED_ACADEMICTERMOFYEAR,'_',ActivityUsageData$HED_ACADEMICYEAR)
ActivityUsageData$YearTerm=factor(ActivityUsageData$YearTerm,levels=c("Winter_2015", "Spring_2015","Fall_2015","Winter_2016", "Spring_2016","Fall_2016","Winter_2017","Spring_2017","Fall_2017"))
ActivityUsageData$YearTerm<-fct_recode(ActivityUsageData$YearTerm,Wi_15='Winter_2015'
                                       ,Sp_15='Spring_2015'
                                       ,Fa_15='Fall_2015'
                                       ,Wi_16='Winter_2016'
                                       ,Sp_16='Spring_2016'
                                       ,Fa_16='Fall_2016'
                                       ,Wi_17='Winter_2017'
                                       ,Sp_17='Spring_2017'
                                       ,Fa_17='Fall_2017')


GracePeriodWindow<-ActivityUsageData %>% 
  filter(IN_GRACEPERIOD==1) %>% 
  group_by(YearTerm,COURSEID) %>% 
  summarise(EndGracePeriod=max(WEEK_OF_COURSE))

CourseSummary<-ActivityUsageData %>% 
  group_by(YearTerm,COURSEID) %>% 
  summarise(Students=max(USERS_ON_COURSE)
            ,CourseWeeks=max(WEEK_OF_COURSE)
#            ,Activations=ifelse(is.na(max(TOTAL_ACTIVATIONS)),0,max(TOTAL_ACTIVATIONS))
            ,TotalActivites=max(TOTAL_ACTIVITIES_IN_COURSE)
            ,AssignedActivities=max(TOTAL_ASSIGNED_ACTIVITIES_IN_COURSE)) %>% 
  inner_join(.,GracePeriodWindow,by=c('YearTerm','COURSEID')) %>% 
  mutate(GracePeriodPercent=EndGracePeriod/CourseWeeks)

#Roll up data to by student by course
UsagebyStudent<-ActivityUsageData %>% 
  mutate(CourseWeeksCalc=ceiling(DAYS_IN_COURSE/7)) %>% 
  group_by(YearTerm,COURSEID,PARTYID) %>% 
  summarise(ACTIVATED=max(ACTIVATED)
            ,PAID=max(PAID)
            ,GracePeriodActivities=sum(ASSIGNED_ACTIVITIES_COMPLETED_BY_STUDENT_IN_GRACEPERIOD)
            ,TwoWeekActivities=sum(ASSIGNED_ACTIVITIES_COMPLETED_BY_STUDENT[WEEK_OF_COURSE<=2])
            ,TotalActivities=sum(ASSIGNED_ACTIVITIES_COMPLETED_BY_STUDENT)
            ,AssignedActivities=max(TOTAL_ASSIGNED_ACTIVITIES_IN_COURSE)
            ,TotalActivitiyCompletion=TotalActivities/AssignedActivities
            ,GracePeriodShare=GracePeriodActivities/AssignedActivities
            ,TwoWeekShare=TwoWeekActivities/AssignedActivities
            ,CourseWeeks=max(WEEK_OF_COURSE)
            ,CourseWeeksCalc=max(CourseWeeksCalc))

CourseUsage<-UsagebyStudent %>% 
  group_by(YearTerm,COURSEID,PAID) %>% 
  summarise(Students=n()
#            ,Paid=sum(PAID)
            ,avgTwoWeekActivites=mean(TwoWeekActivities)
            ,medTwoWeekActivities=median(TwoWeekActivities)) %>% 
  inner_join(.,CourseSummary,by=c('YearTerm','COURSEID'))
CourseUsage$isPaid<-factor(CourseUsage$PAID)
CourseUsage$isPaid<-fct_recode(CourseUsage$isPaid,NonPayingStudents='0',PayingStudents='1')

#summarise data by paid/not paid
UsageSummarybyStudent<-UsagebyStudent %>% 
  group_by(YearTerm,COURSEID,PAID) %>% 
  summarise(ACTIVATED=sum(ACTIVATED)
            ,avgGracePeriodActivities=mean(GracePeriodActivities)
            ,medGracePeriodActivites=median(GracePeriodActivities)
            ,avgTotalActivities=mean(TotalActivities)
            ,medTotalActivities=median(TotalActivities)
            ,avgGracePeriodShare=mean(GracePeriodShare)
            ,medGracePeriodShare=median(GracePeriodShare))

#convert paid indicator to factor
UsagebyStudent$isPaid<-factor(UsagebyStudent$PAID)
UsagebyStudent$isPaid<-fct_recode(UsagebyStudent$isPaid,NonPayingStudents='0',PayingStudents='1')

#graph students by term
CourseUsage %>% 
  group_by(YearTerm) %>% 
  summarise(Students=sum(Students.x)) %>% 
  ggplot(aes(x=YearTerm,y=Students)) +
  geom_bar(stat='identity',fill="#063961") +
  cengage_fte_theme() +
#  scale_fill_manual(values=cengage_color_palette(1,2))+
  scale_y_continuous(labels=comma)+
  labs(title="Count of Disciplines per Term", x="Term", y="Disciplines") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 

#graph courses by term
CourseSummary %>% 
  group_by(YearTerm) %>% 
  summarise(Course=length(unique(COURSEID))) %>% 
  ggplot(aes(x=YearTerm,y=Course)) +
  geom_bar(stat='identity',fill="#063961") +
  cengage_fte_theme() +
#  scale_fill_manual(values=cengage_color_palette(1,2))+
  scale_y_continuous(labels=comma)+
  labs(title="Count of Courses per Term", x="Term", y="Courses") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')


#graph courses by length
CourseSummary %>% 
  filter(CourseWeeks<=25) %>% 
  group_by(CourseWeeks) %>% 
  summarise(Courses=n()) %>% 
  ggplot(aes(x=CourseWeeks,y=Courses)) +
  geom_bar(stat='identity',fill="#063961") +
  cengage_fte_theme() +
#  scale_fill_manual(values=cengage_color_palette(1,2))+
  scale_y_continuous(labels=comma)+
  labs(title="Count of Courses by Course Length (weeks)", x="Weeks", y="Courses") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')

#graph courses by # assignments
CourseSummary %>% 
  filter(CourseWeeks<=25 & AssignedActivities<=40) %>% 
#  group_by(AssignedActivities) %>% 
#  summarise(Courses=n()) %>% 
  ggplot(aes(AssignedActivities)) +
  geom_histogram(stat='bin',fill="#063961",binwidth = 1) +
  cengage_fte_theme() +
#  scale_fill_manual(values=cengage_color_palette(1,2))+
  scale_y_continuous(labels=comma)+
  labs(title="Count of Courses by Number of Course Assignments", x="Courses", y="Assignments") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom')


#graph activities complete in two weeks by length of course by Paid/not paid
CourseUsage %>% 
  filter(CourseWeeks<=25) %>% 
  mutate(avgCompletion=avgTwoWeekActivites/TotalActivites) %>% 
  ggplot(aes(CourseWeeks,avgCompletion)) +
  geom_bar(aes(fill=isPaid),stat='summary',fun.y='mean',position='dodge') +
  cengage_fte_theme() +
  scale_fill_manual(values=cengage_color_palette(2,2))+
  scale_y_continuous(labels=percent)+
  labs(title="Share of Courses Completed In First Two Weeks by Length of Course", x="Course Length (Weeks)", y="Percent Course Completed") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 


#graph total activities completed by Paid/not paid
UsagebyStudent %>% 
  ggplot(aes(TotalActivitiyCompletion)) +
  geom_density(aes(fill=isPaid),alpha=0.75,adjust=5) +
  cengage_fte_theme() +
  scale_fill_manual(values=cengage_color_palette(2,2))+
  labs(title="Distribution of Lessons completed by Paying/Not Paying Students (Full Course)", x="Percent of Activities Completed During Course", y="Density") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 

#graph  activities completed in first two weeks by Paid/not paid
UsagebyStudent %>% 
  filter(CourseWeeksCalc>=6 & CourseWeeksCalc<=25) %>% 
  ggplot(aes(TwoWeekShare)) +
  geom_density(aes(fill=isPaid),alpha=0.75,adjust=5) +
  cengage_fte_theme() +
  scale_fill_manual(values=cengage_color_palette(2,2))+
  labs(title="Distribution of Lessons completed by Paying/Not Paying Students (Two Weeks)", x="Percent of Activities Completed in First Two Weeks", y="Density") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 

UsagebyStudent %>% 
  group_by(YearTerm) %>% 
  summarise(percent.50=quantile(TwoWeekShare,probs=.5)
            ,percent.60=quantile(TwoWeekShare,probs=.6)
            ,percent.70=quantile(TwoWeekShare,probs=.7)
            ,percent.80=quantile(TwoWeekShare,probs=.8)
            ,percent.90=quantile(TwoWeekShare,probs=.9)
            ,percent.95=quantile(TwoWeekShare,probs=.95))

#graph  activities completed in grace period by Paid/not paid
UsagebyStudent %>% 
#  filter(PAID==1) %>% 
ggplot(aes(GracePeriodShare)) +
  geom_density(aes(fill=isPaid),alpha=0.75,adjust=5) +
  cengage_fte_theme() +
  scale_fill_manual(values=cengage_color_palette(2,2))+
  labs(title="Distribution of Lessons completed by Paying/Not Paying Students (Trial Period)", x="Activities Completed in Trial Period", y="Density") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 

course.length<-CourseSummary %>% 
  select(COURSEID,YearTerm,CourseWeeks)


my.den.non<-density(UsagebyStudent[which(UsagebyStudent$isPaid=="NonPayingStudents" & UsagebyStudent$CourseWeeksCalc>2 & UsagebyStudent$CourseWeeksCalc<=25),]$TwoWeekShare)
my.den.pd<-density(UsagebyStudent[which(UsagebyStudent$isPaid=="PayingStudents" & UsagebyStudent$CourseWeeksCalc>2 & UsagebyStudent$CourseWeeksCalc<=25),]$TwoWeekShare)
sum(my.den.non$y[256:477]-my.den.pd$y[256:477])/2

curr.stud.pop<-CourseUsage %>% 
  filter(YearTerm=='Fa_16' & CourseWeeks>=3) %>% 
  group_by(YearTerm) %>% 
  summarise(tot.students=sum(Students.x)) %>% 
  .$tot.students
  



# TrialAbuseData.raw <- read_delim("~/rawdata/TrialAbuseData.txt"
#                              ,"\t"
#                              , escape_double = FALSE
#                              , col_types = cols(ACADEMIC_YEAR = col_character())
#                              , trim_ws = TRUE)
# 
# #rename some columns to be friendlier
# TrialAbuseData<-TrialAbuseData.raw %>% 
#   rename(greater_than_90_perc_count=`more_than_90%`
#          ,greater_than_80_perc_count=`more_than_80%`
#          ,greater_than_70_perc_count=`more_than_70%`
#          ,greater_than_60_perc_count=`more_than_60%`) %>% 
#   mutate(perc_share_90=greater_than_90_perc_count/TOTAL_STUDENTS
#          ,perc_share_80=greater_than_80_perc_count/TOTAL_STUDENTS
#          ,perc_share_70=greater_than_70_perc_count/TOTAL_STUDENTS
#          ,perc_share_60=greater_than_60_perc_count/TOTAL_STUDENTS
#          ,Term=ifelse(ACADEMIC_TERM=="Winter","Wi",
#                      ifelse(ACADEMIC_TERM=="Spring","Sp","Fa"))) %>%
#   mutate(YearTerm=paste0(ACADEMIC_YEAR,"_",Term)) %>% 
#   mutate(YearTerm=factor(YearTerm,levels=c("2015_Wi", "2015_Sp","2015_Fa","2016_Wi", "2016_Sp","2016_Fa","2017_Wi","2017_Sp")))
#   
# TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new") %>% 
#   dplyr::select(COURSEKEY,perc_share_90:perc_share_60) %>% 
#   gather(cohort,perc_share,perc_share_90:perc_share_60) %>% 
#   ggplot(.,aes(perc_share)) +
#   geom_density(aes(fill=cohort), alpha=0.75,adjust=5) +
#   xlim(0, .25) +
#   cengage_fte_theme() +
#   scale_fill_manual(values=cengage_color_palette(4,2))+
#   labs(title="Share of Students/Course Potential Trial Abuse", x="Percent of Class Achieving Threshold", y="Density") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new" & MAX_WEEKS>=8) %>% 
#   dplyr::select(COURSEKEY,perc_share_90:perc_share_60) %>% 
#   gather(cohort,perc_share,perc_share_90:perc_share_60) %>% 
#   ggplot(.,aes(perc_share)) +
#   geom_density(aes(fill=cohort), alpha=0.75,adjust=5) +
#   xlim(0, .25) +
#   cengage_fte_theme() +
#   scale_fill_manual(values=cengage_color_palette(4,2))+
#   labs(title="Share of Students/Course Potential Trial Abuse", x="Percent of Class Achieving Threshold", y="Density") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
#   
# TrialAbuseData %>% 
# #  filter(SEGMENT=="2week" & DATASET=="new") %>%
#   filter(SEGMENT=="3week") %>%   
#   group_by(YearTerm) %>% 
#   summarise(tot_perc_share_90=sum(perc_share_90)/sum(TOTAL_STUDENTS)
#             ,tot_perc_share_80=sum(perc_share_80)/sum(TOTAL_STUDENTS)
#             ,tot_perc_share_70=sum(perc_share_70)/sum(TOTAL_STUDENTS)
#             ,tot_perc_share_60=sum(perc_share_60)/sum(TOTAL_STUDENTS)) %>% 
#   gather(sensitivity_level,perc_share,tot_perc_share_90:tot_perc_share_60) %>% 
# #  filter(sensitivity_level=="tot_perc_share_90") %>% 
#   ggplot() +
#   geom_line(aes(x=YearTerm,y=perc_share,group=sensitivity_level,colour=sensitivity_level),size=1) +
#   cengage_fte_theme() +
#   scale_color_manual(values=cengage_color_palette(4,2))+
#   scale_y_continuous(labels=percent)+
#   labs(title="Share of Students/Course Potential Trial Abuse", x="Term", y="Percent of Students") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="3week" & DATASET=="new") %>% 
#   group_by(YearTerm) %>% 
#   summarise(Disciplines=length(unique(DISCIPLINE))) %>% 
#   ggplot(aes(x=YearTerm,y=Disciplines,fill=YearTerm)) +
#   geom_bar(stat='identity') +
#   cengage_fte_theme() +
#   scale_fill_manual(values=cengage_color_palette(5,2))+
#   scale_y_continuous(labels=comma)+
#   labs(title="Count of Disciplines per Term", x="Term", y="Disciplines") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="3week" & DATASET=="new") %>% 
#   group_by(YearTerm) %>% 
#   summarise(CourseArea=length(unique(COURSEAREA))) %>% 
#   ggplot(aes(x=YearTerm,y=CourseArea,fill=YearTerm)) +
#   geom_bar(stat='identity') +
#   cengage_fte_theme() +
#   scale_fill_manual(values=cengage_color_palette(5,2))+
#   scale_y_continuous(labels=comma)+
#   labs(title="Count of Course Areas per Term", x="Term", y="Course Area") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new") %>% 
#   group_by(YearTerm) %>% 
#   summarise(Students=sum(TOTAL_STUDENTS)) %>% 
#   ggplot(aes(x=YearTerm,y=Students,fill=YearTerm)) +
#   geom_bar(stat='identity') +
#   cengage_fte_theme() +
#   scale_fill_manual(values=cengage_color_palette(5,2))+
#   scale_y_continuous(labels=comma)+
#   labs(title="Count of Students per Term", x="Term", y="Students") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new") %>% 
#   group_by(MAX_WEEKS) %>% 
#   summarise(courses=n()
#             ,abuse=sum(greater_than_60_perc_count)/sum(TOTAL_STUDENTS)) %>% 
#   ggplot(aes(x=MAX_WEEKS,y=courses,fill=MAX_WEEKS)) +
#   geom_bar(stat='identity',fill="#063961") +
#   cengage_fte_theme() +
#   scale_y_continuous(labels=comma)+
#   labs(title="Count of Courses by Max Course Weeks", x="Course Weeks", y="Courses") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new") %>% 
#   group_by(MAX_WEEKS) %>% 
#   summarise(courses=n()
#             ,abuse=sum(greater_than_60_perc_count)/sum(TOTAL_STUDENTS)) %>% 
#   ggplot(aes(x=MAX_WEEKS,y=abuse,fill=MAX_WEEKS)) +
#   geom_bar(stat='identity',fill="#007db8") +
#   cengage_fte_theme() +
#   scale_y_continuous(labels=percent)+
#   labs(title="Percent of Students Exceeding Threshold by Max Course Weeks", x="Course Weeks", y="% Students") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# 
# TrialAbuseData %>% 
#   dplyr::select(DISCIPLINE,greater_than_60_perc_count:greater_than_90_perc_count,TOTAL_STUDENTS) %>%
#   group_by(DISCIPLINE) %>% 
#   summarise(total_60_perc=sum(greater_than_60_perc_count)
#             ,total_70_perc=sum(greater_than_70_perc_count)
#             ,total_80_perc=sum(greater_than_80_perc_count)
#             ,total_90_perc=sum(greater_than_90_perc_count)
#             ,total_students=sum(TOTAL_STUDENTS)) %>% 
#   ggplot(aes(DISCIPLINE,total_students)) +
#   geom_bar(stat = 'sum') +
#   cengage_fte_theme() +
#   scale_fill_manual(values=cengage_color_palette(55))+
#   labs(title="Share of Students/Course Potential Trial Abuse", x="Percent of Class Achieving Threshold", y="Density") +
#   theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
# 
# 
# results.df<-TrialAbuseData %>% 
#   filter(SEGMENT=="3week" & DATASET=="new") %>% 
#   group_by(YearTerm) %>% 
#   summarise(tot_perc_share_90=percent(sum(perc_share_90)/sum(TOTAL_STUDENTS))
#             ,tot_perc_share_80=percent(sum(perc_share_80)/sum(TOTAL_STUDENTS))
#             ,tot_perc_share_70=percent(sum(perc_share_70)/sum(TOTAL_STUDENTS))
#             ,tot_perc_share_60=percent(sum(perc_share_60)/sum(TOTAL_STUDENTS)))
# 
# results.df.contingency<-TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new") %>% 
#   group_by(YearTerm) %>% 
#   summarise(tot_perc_share_90=sum(greater_than_90_perc_count)
#             ,non_perc_share_90=sum(TOTAL_STUDENTS)-tot_perc_share_90
#             ,tot_perc_share_80=sum(greater_than_80_perc_count)
#             ,non_perc_share_80=sum(TOTAL_STUDENTS)-tot_perc_share_80
#             ,tot_perc_share_70=sum(greater_than_70_perc_count)
#             ,non_perc_share_70=sum(TOTAL_STUDENTS)-tot_perc_share_70
#             ,tot_perc_share_60=sum(greater_than_60_perc_count)
#             ,non_perc_share_60=sum(TOTAL_STUDENTS)-tot_perc_share_60)
# 
# 
# b<-chisq.test(results.df.contingency[c(1,4),4:5])$p.value
# #90% YoY Win
# paste0("Winter 90% Change: ",percent(as.numeric(sub("%","",results.df[4,2]))-as.numeric(sub("%","",results.df[1,2]))))
# paste0("p value: ",round(chisq.test(results.df.contingency[c(1,4),4:5])$p.value,3))
# #90% YoY Spr
# paste0("Spring 90% Change: ",percent(as.numeric(sub("%","",results.df[5,2]))-as.numeric(sub("%","",results.df[2,2]))))
# paste0("p value: ",round(chisq.test(results.df.contingency[c(2,5),4:5])$p.value,3))
# #60% YoY Win
# paste0("Winter 60% Change: ",percent(as.numeric(sub("%","",results.df[4,5]))-as.numeric(sub("%","",results.df[1,5]))))
# paste0("p value: ",round(chisq.test(results.df.contingency[c(1,4),8:9])$p.value,3))
# #60% YoY Spr
# paste0("Spring 60% Change: ",percent(as.numeric(sub("%","",results.df[5,5]))-as.numeric(sub("%","",results.df[2,5]))))
# paste0("p value: ",round(chisq.test(results.df.contingency[c(2,5),8:9])$p.value,3))
# 
# TrialAbuseData %>% 
#   filter(SEGMENT=="2week" & DATASET=="new" & MAX_WEEKS>=8) %>% 
#   summarise(var= var(greater_than_90_perc_count))



save(CourseUsage,CourseSummary,UsagebyStudent,file="C:/Users/babbenante/OneDrive - Cengage Learning/Documents/rawdata/TrialAbuseUsageData.RData")