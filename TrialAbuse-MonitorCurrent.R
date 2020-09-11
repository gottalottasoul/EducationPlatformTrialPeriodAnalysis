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
#if (!require(dplyr)) {
#  install.packages('dplyr') # consistent data.frame operations
#  require(dplyr)
#}

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
#if (!require(dplyr-snowflakedb)) {
#  devtools::install_github("snowflakedb/dplyr-snowflakedb") 
#  require(dplyr-snowflakedb)
#}
library(dplyr, lib.loc = "C:\\Users\\babbenante\\OneDrive - Cengage Learning\\Documents\\R\\win-library\\3.4\\old_packages")
library(dplyr.snowflakedb, lib.loc = "C:\\Users\\babbenante\\OneDrive - Cengage Learning\\Documents\\R\\win-library\\3.4\\old_packages")

#######################################################################################
### end one of package loading                                                      ###
#######################################################################################


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
if (!require(RODBC)) {
  install.packages('RODBC') # date manipulation
  require(RODBC)
}


#set working directory
setwd(here())

#######################################################################################
### below packages and code are needed to connect to snowflake                      ###
### snowflake package is deprecated, and relies on older version of dplyr           ###
#######################################################################################
# if (!require(rJava)) {
#   install.packages("rJava") #relative working directory
#   require(rJava)
# }
# 
# if (!require(devtools)) {
#   install.packages('devtools') # date manipulation
#   require(devtools)
# }
# if (!require(dplyr-snowflakedb)) {
#   devtools::install_github("snowflakedb/dplyr-snowflakedb") 
#   require(dplyr-snowflakedb)
# }
# 
# 
# devtools::install_github("snowflakedb/dplyr-snowflakedb",ref="v0.1.0") 
# 
# #need an older version of dplyr since db connect stuff was abstracted out to
# #dbplyr in more recent versions
# install_version("dplyr", version = "0.4.3", repos = "http://cran.us.r-project.org")


options(dplyr.jdbc.classpath = "c:/users/babbenante/downloads/snowflake-jdbc-3.0.21.jar")
options(java.parameters = "-Xmx8048m")

snowflake_conn <- src_snowflakedb(user = "******",
                         password = "******",
                         account = "******",
                         opts = list(warehouse = "******",
                                     db = "******",
                                     schema = "******"))


datamart_conn<-odbcConnect("******",uid="******",pwd="******",believeNRows = FALSE)
course_data<-sqlQuery(datamart_conn,"select enr.course_key
                  ,enr.product_type
                  ,enr.course_name
                  ,enr.institution_name
                  ,enr.instructor_name
                  ,tr.sales_rep_nm
                  from dw.dm_course_enrollments enr
                  join dw.dm_territories tr
                  on enr.territory_skey=tr.territory_skey
                  where course_begin_date > date '2017-08-01'",stringsAsFactors=FALSE)




abuse_data<-tbl.src_snowflakedb(snowflake_conn,sql("select apw.* 
                                          ,dt.datevalue as course_end_date
                                          ,c.coursekey
                                          from MARKETING_ANALYTICS.ACTIVITIES_PER_WEEK apw
                                          join dw_ga.dim_course c
                                          on apw.courseid=c.courseid
                                          join dw_ga.dim_date dt ON c.enddatekey = dt.datekey 
                                          "))


Date_range<-Sys.Date()+5

#SnowflakeQuery<-filter(abuse_data,HED_ACADEMICYEAR==2017 & HED_ACADEMICTERMOFYEAR=='Fall')
SnowflakeQuery<-filter(abuse_data,COURSE_START_DATE>='2017-08-01',COURSE_START_DATE<=Date_range)
ActivityUsageData<-collect(SnowflakeQuery)
ActivityUsageData$COURSE_START_DATE<-as.Date(ActivityUsageData$COURSE_START_DATE)
ActivityUsageData$COURSE_END_DATE<-as.Date(ActivityUsageData$COURSE_END_DATE)
ActivityUsageData$Current_Date<-ActivityUsageData$COURSE_START_DATE+ActivityUsageData$DAY_OF_COURSE
#ActivityUsageData$End_Date<-ActivityUsageData$COURSE_START_DATE+ActivityUsageData$DAYS_IN_COURSE
ActivityUsageSummary<-ActivityUsageData %>% 
  group_by(COURSEKEY,PARTYID,PRODUCTID) %>% 
  summarise(DAY_OF_COURSE=max(DAY_OF_COURSE)
                   ,COURSE_START_DATE=max(COURSE_START_DATE)
                   ,Students_in_course=max(USERS_ON_COURSE)
                   ,Activations=mean(TOTAL_ACTIVATIONS)
                   ,Current_Date=max(Current_Date)
                   ,End_Date=max(COURSE_END_DATE)
                   ,Completed_Assignments=sum(ASSIGNED_ACTIVITIES_COMPLETED_BY_STUDENT)
                   ,Total_Activities=max(TOTAL_ASSIGNED_ACTIVITIES_IN_COURSE)
                   )

reportDate<-Sys.Date()-5
  
RecentUsageData<-ActivityUsageSummary %>% 
  filter(Current_Date>=reportDate & DAY_OF_COURSE<=21) %>% 
  group_by(DAY_OF_COURSE) %>% 
  mutate(PERCENT_ACTIVITIES_COMPLETED=Completed_Assignments/Total_Activities) %>% 
  summarise(Courses=n_distinct(COURSEKEY)
                   ,Total_students=sum(Students_in_course)
                   ,StudentsGreaterthanHalf=sum(PERCENT_ACTIVITIES_COMPLETED>=.5)
                   ,StudentsMoreThanThree=sum(Completed_Assignments>=3))


RecentUsageData_Detail<-ActivityUsageSummary %>% 
  filter(Current_Date>=reportDate & DAY_OF_COURSE<=14) %>% 
  mutate(PERCENT_ACTIVITIES_COMPLETED=Completed_Assignments/Total_Activities) %>% 
  filter(PERCENT_ACTIVITIES_COMPLETED>=.5 | Completed_Assignments>=3) %>% 
  arrange(PARTYID,COURSEKEY,DAY_OF_COURSE)

RecentUsageData_Detail_Filtered<-ActivityUsageSummary %>% 
  filter(Current_Date>=reportDate & DAY_OF_COURSE<=14 & Total_Activities>=7 & End_Date>Sys.Date()) %>% 
  mutate(PERCENT_ACTIVITIES_COMPLETED=Completed_Assignments/Total_Activities) %>% 
  filter(PERCENT_ACTIVITIES_COMPLETED>=.5)  %>% 
  arrange(PARTYID,COURSEKEY,DAY_OF_COURSE)


Report_List_detail<-RecentUsageData_Detail_Filtered %>% 
  left_join(.,course_data,by=c("COURSEKEY"="COURSE_KEY")) %>% 
  select(Current_Date,SALES_REP_NM,INSTITUTION_NAME,INSTRUCTOR_NAME,PRODUCT_TYPE,COURSE_NAME,COURSEKEY,COURSE_START_DATE,End_Date,Completed_Assignments,Total_Activities,PERCENT_ACTIVITIES_COMPLETED)

Report_List_Summary<-Report_List_detail %>% 
  group_by(SALES_REP_NM,INSTITUTION_NAME,INSTRUCTOR_NAME,PRODUCT_TYPE,COURSE_NAME,COURSE_START_DATE,End_Date,Total_Activities) %>% 
  summarise(PotentialAbuse=n())

Report_List_detail %>% 
  ggplot(aes(Total_Activities)) +
  geom_density(alpha=0.75,adjust=5) +
  cengage_fte_theme() +
  scale_fill_manual(values=cengage_color_palette(1,2))+
  labs(title="Activities Completed by Paying/Not-Paying (Full Course)", x="Percent of Activities Completed During Course", y="Density") +
  theme(legend.title=element_blank(),legend.text=element_text(size=5),legend.position='bottom') 
