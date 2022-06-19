#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard) 
library(shiny)
library(h2o)
library(dplyr)
library(ggplot2)
library('dplyr')
library(reshape)
require(shinydashboard)
library(data.table)

options(java.parameters = "-Xmx64048m") # 64048 is 64 GB

df_1 <- as.data.frame(read.csv("https://github.com/CarrieZhao0822/PredictAirflightDelay/raw/main/2018-1.csv",header=T, na.strings=c("","NA")))
df_2 <- as.data.frame(read.csv("https://github.com/CarrieZhao0822/PredictAirflightDelay/raw/main/2018-2.csv",header=T, na.strings=c("","NA")))
df_3 <- as.data.frame(read.csv("https://github.com/CarrieZhao0822/PredictAirflightDelay/raw/main/2018-3.csv",header=T, na.strings=c("","NA")))
df_4 <- as.data.frame(read.csv("https://github.com/CarrieZhao0822/PredictAirflightDelay/raw/main/2018-4.csv",header=T, na.strings=c("","NA")))
df_5 <- as.data.frame(read.csv("https://github.com/CarrieZhao0822/PredictAirflightDelay/raw/main/2018-5.csv",header=T, na.strings=c("","NA")))
df_6 <- as.data.frame(read.csv("https://github.com/CarrieZhao0822/PredictAirflightDelay/raw/main/2018-6.csv",header=T, na.strings=c("","NA")))

df <- rbind(df_1,df_2)
df <- rbind(df,df_3)
df <- rbind(df,df_4)
df <- rbind(df,df_5)
df <- rbind(df,df_6)
rm(df,df_1,df_2,df_3,df_4,df_5,df_6)

####################################For Introduction
text1 <- c("Abstract")
text2 <- c("The main goal of this project is to create a shiny app in R using descriptive and predictive analytics knowledge we gained in our class. Our goal is to provide American Airline companies with insights on the impact of flight delays on economy and consumers and how to make better business decisions that can save airline companies millions of dollars due to flight delays. To provide useful insights, we created a model using shiny app that provides descriptive analysis and visualizations that shows the number of canceled flights for the chosen year and cancelations and delays reasons. Moreover, the shiny app provides a predictive analysis to make prediction on future delays which can provide Airline companies of important and useful insight to analyze their performance, make decisions to mitigate these delays and increase customer satisfaction. ")
text3 <- c("Business Problem")
text4 <- c("Everyday thousands of flights are either delayed or canceled all over the United Sates. These delays do not only effect travelers but also Airline companies. The average cost of delay for U.S. passenger airlines is around $74.24 per minute which can translates to billions of dollars of cost to U.S Airline companies. Analyzing the reasons and number of delays can give a better understanding in predicting future delays and how they can be mitigated. There are some delay reasons that can’t be controlled but most of the reasons are known and can be identified to help reduce the number of delays. Predicating and understanding the reasons of delays will help Airline companies save time, money and increase customer satisfaction. ")
text5 <- c("Data")
text6 <- c("To create a reliable model that can help do future predictions a reliable data set with all needed information is needed. The data used for our shiny app and analysis was originally collected from U.S DOT Bureau of Transportation Statistics. For our project, we had to clean the data we have as the original data set includes millions of observations. Thus, we only used the flight delays data from 2018 for our analysis and prediction due to the limitation of the free version of shiny app.")

####################################For Conclusion
text_con_1 <- c("Root Cause")
text_con_2 <- c("Cancellation Reason")
text_con_3 <- c("Weather is the main reason that cause airflight canceled.")
text_con_4 <- c("Delay Reason")
text_con_5 <- c("Weather: Raining and thunder season, and snowing season will are the main reason cause of a airflight delay.")
text_con_6 <- c("Month: Holiday season and summer break, winter break are the period that has the most delay airflight.")
text_con_7 <- c("Suggestion")
text_con_8 <- c("Business Suggestion for Airline Company")
text_con_9 <- c("Decrease number of the flights during raining and thunder season and snowing season of each origin and destination, change airplane model from 737 to 777 during these season, so that helps to merge the passengers into same aircraft from multiple smaller aircraft, and the change doesn’t reduce the revenue.")
text_con_10 <- c("Travel Tips for passengers")
text_con_11 <- c("Avoid these high delay/cancellation rate months to go travelling. If you have to, make sure you have a plan B travel plan. For example, for short trip, driving is a good plan, sometimes even faster than waiting for the delayed airplane.")

######################################################################
#Part 1
#Descriptive visualizations for 2018 Flight cancellation and delay
######################################################################
flight_2018 <- df

###############Calculation for Cancellation
net_flights <- subset(flight_2018, flight_2018$CANCELLED==0) # We focus on no Cancelled flight

net_flights <- subset(net_flights, !(is.na(net_flights$AIR_TIME))) #Amount of time spent in the air,delete NA

#CANCELLED FLIGHT ANALYSIS
cancelled_flights <- subset(flight_2018, flight_2018$CANCELLED==1)
nrow(cancelled_flights)/nrow(flight_2018)*100 #Percentage of cancelled flights among all flights is 1.616
#[1] 1.616204

#CANCELLATION CODES
#A-Carrier Caused
#B-Weather
#C-National Aviation System
#D-Security
cancellation_reason <- summary(factor(cancelled_flights$CANCELLATION_CODE))
#A     B     C     D 
#29484 61984 25072    44 
Cancel_reason <- c("Carrier","Weather","NAS","Security")
xa<-c(cancellation_reason[[1]],cancellation_reason[[2]],cancellation_reason[[3]],cancellation_reason[[4]])
percentage<-round(xa/sum(xa)*100)

labels_new<-paste(Cancel_reason,percentage)

#concatenates the above output with the '%' symbol 
final_labels<-paste(labels_new,'%',sep = "")

cancellation_reason[1]/sum(cancellation_reason)*100 #Carrier Caused cancellation - 25.28992%
cancellation_reason[2]/sum(cancellation_reason)*100 #Weather caused cancellation - 53.16682%
cancellation_reason[3]/sum(cancellation_reason)*100 #NAS caused cancellation - 21.50552%
cancellation_reason[4]/sum(cancellation_reason)*100 #Security caused cancellation - 0.03774103%

#Cancellation rates are calculated for each airline
all_cancellation_fq <- summary(factor(cancelled_flights$OP_CARRIER))
all_flight_fq <- summary(factor(flight_2018$OP_CARRIER))
cancel_rate <- sort(all_cancellation_fq / all_flight_fq *100, decreasing = TRUE)
cancel_rate_name <- names(cancel_rate)
names(cancel_rate) <- NULL
cancel_rate_table <- data.frame(cancel_rate_name, cancel_rate) 
colnames(cancel_rate_table) <- c("Airline_Company_Code","Cancelation_Rate")

#rm(flight_2018)

###############Calculation for Delay
#Input new column with month
net_flights$FL_DATE <- as.Date(net_flights$FL_DATE)
net_flights$Month <- as.numeric(format(net_flights$FL_DATE,'%m'))

attach(net_flights)
#Calculate mean of Arrived delay time per month per Airline
arrive_delay <- net_flights[ , c("Month","OP_CARRIER","ARR_DELAY")]
arrive_mean_delay <- aggregate(arrive_delay,by=list(Month = Month, AirLine = OP_CARRIER ), FUN=mean,na.rm = TRUE)
arrive_mean_delay$Month <- NULL

arrive_mean_delay$OP_CARRIER <- NULL

#Calculate mean of Arrived delay time per month
arrive_delay <- net_flights[ , c("Month","OP_CARRIER","ARR_DELAY")]
arrive_mean_delay_m <- aggregate(arrive_delay,by=list(Month = Month), FUN=mean,na.rm = TRUE)
arrive_mean_delay_m$Month <- NULL
arrive_mean_delay_m$OP_CARRIER <- NULL


#Calculate mean of 5 delay reason per month onlyl
flights_delay <- net_flights[ , c("Month","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")]
mean_delay_month <- aggregate(flights_delay,by=list(Month = Month), FUN=mean,na.rm = TRUE)
mean_delay_month$Month <- NULL

#Calculate mean of 5 delay reason per month per Airline
flights_delay <- net_flights[ , c("Month","OP_CARRIER","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")]
mean_delay <- aggregate(flights_delay,by=list(Month = Month, AirLine = OP_CARRIER ), FUN=mean,na.rm = TRUE)
mean_delay$Month <- NULL
mean_delay$OP_CARRIER <- NULL

###################################
#Plot for cancellation and delay
###################################
#source("multiplot.R")

#par(mfrow=c(1,2), bg="lightgrey")

#####Plot for Cancellation
#pie3D(x,labels = final_labels,explode = 0.1,main='Pie chart of Cancelization Reason',labelcex = 1.5,radius = 0.7)
#pie3D(cancel_rate_table$Cancelation_Rate,labels=cancel_rate_table$Airline_Company_Code,explode=0.1,
#      main="Pie Chart of Airline Cancellation Rate",radius = 0.7)
p1 <- ggplot(data.frame(xa,final_labels), aes(x = "", y = xa, fill = final_labels)) +
  geom_col(color = "black") +
  geom_label(aes(label = final_labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")
#p1
#p1 <- pie(x, labels = final_labels, main = "Pie chart of Cancellation Reason",col = rainbow(length(x)))
p2 <- pie(cancel_rate_table$Cancelation_Rate, labels = cancel_rate_table$Airline_Company_Code, main = "Pie chart of Cancelization Reason",col = rainbow(length(cancel_rate_table$Cancelation_Rate)))
p2 <- ggplot(cancel_rate_table, aes(x = "", y = Cancelation_Rate, fill = Airline_Company_Code)) +
  geom_col(color = "black") +
  geom_label(aes(label = Airline_Company_Code),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")
#p2
#Plot delay reason by month for selected airline 
#ggplot(arrive_mean_delay_m, aes(x=factor(Month),y=ARR_DELAY)) + geom_line()
p3 <- ggplot(arrive_mean_delay_m, aes(x=Month,y=ARR_DELAY)) + geom_point() + geom_line(size=2,color="blue")
p3 + labs(
  title = "Flight Arrived Dealy by Month",
  subtitle = "2018",
  x = "Month",
  y = "Arrived Delay"
)

p4 <- ggplot(arrive_mean_delay, aes(x=factor(Month),y=ARR_DELAY,fill=AirLine)) + geom_col() + labs(title = "Flight Arrived Dealy by Month for all Airlines", subtitle = "2018", x = "Month", y = "Arrived Delay")

# AA arrive delay per month
p5 <-ggplot(arrive_mean_delay[arrive_mean_delay$AirLine=="AA",c(1,3)], aes(x=Month,y=ARR_DELAY)) + geom_point() + geom_line(size=2,color="blue")
# DL arrive delay per month
p6 <-ggplot(arrive_mean_delay[arrive_mean_delay$AirLine=="DL",c(1,3)], aes(x=Month,y=ARR_DELAY)) + geom_point() + geom_line(size=2,color="red")


# For 5 reason plot
my_data <- melt(mean_delay_month,id = "Month")   
p7 <- ggplot(my_data, aes(x=factor(Month),y=value,fill=variable)) + geom_col(position = "dodge")
p7 + labs(
  title = "Flight Dealy Reason by Month",
  subtitle = "2018",
  x = "Month",
  y = "Delay Time"
)
# Show AA for 5 reason plot
my_data1 <- melt(mean_delay,id = c("Month","AirLine"))
p8 <- ggplot(my_data1[my_data1$AirLine=="AA",c(1,3,4)], aes(x=Month,y=value,fill=variable)) + geom_col(position = "dodge")
# Show DL for 5 reason plot
my_data1 <- melt(mean_delay,id = c("Month","AirLine"))
p9 <- ggplot(my_data1[my_data1$AirLine=="DL",c(1,3,4)], aes(x=Month,y=value,fill=variable)) + geom_col(position = "dodge")


#rm(arrive_delay,cancelled_flights,flight_2018,flights_delay,net_flights)
#rm(arrive_mean_delay,arrive_mean_delay_m,cancel_rate_table,mean_delay,mean_delay_month,my_data,my_data1)
#rm(all_cancellation_fq,all_flight_fq,cancel_rate,cancel_rate,cancel_rate_name,cancel_rate_table)
#rm(Cancel_reason,cancellation_reason,final_labels,labels_new,percentage,x)
#rm(multiplot)

######################################################################
#Part 2
#predictive for 2018 Flight cancellation and delay
######################################################################
# Create subset to only keep the columns that we need.
subset <- net_flights %>%
  select(Month, OP_CARRIER , ORIGIN ,DEST,DEP_DELAY, CRS_ELAPSED_TIME ,ACTUAL_ELAPSED_TIME, ARR_DELAY,DISTANCE )
#change column type

#subset$FL_DATE <- as.factor(subset$FL_DATE)
subset$OP_CARRIER <- as.factor(subset$OP_CARRIER)
subset$ORIGIN <- as.factor(subset$ORIGIN)
subset$DEST <- as.factor(subset$DEST)
#str(subset)


h2o.init(nthreads=12, max_mem_size="64g")

# load d data frame into h20 cluster

#carsData <- h2o.importFile(f)
data <- as.h2o(subset)

#str(data)
#head(data)
#gc() 

# prepare the data
y <- "ARR_DELAY"                                # target variable to learn
x <- setdiff(names(subset), y)                # feature variables are all other columns

parts <- h2o.splitFrame(data, 0.8, seed=99) # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs
test <- parts[[2]]                          # random set of testing obs


# Train a Deep Learning model

# train$y <- log(train$y)
# test$y <- log(test$y)
#rm(df)
h2oDepLearn <- h2o.deeplearning(x, y, train)

#h2oDepLearn

#check training performance

h2o.performance(h2oDepLearn, test)
#MSE:  0.2330426
#RMSE:  0.4827449
#MAE:  0.1813015
#RMSLE:  NaN
#Mean Residual Deviance :  0.2330426

#MSE:  0.2852991
#RMSE:  0.5341339
#MAE:  0.1899758
#RMSLE:  NaN
#Mean Residual Deviance :  0.2852991

p <- h2o.predict(h2oDepLearn, test)

p <-  as.data.frame(p)
predDelay <- cbind(as.data.frame(test$Month), as.data.frame(p$predict), as.data.frame(test$ARR_DELAY) )

#temp_predDelay <- predDelay
#head(p)
#head(arrive_delay)
#head(predDelay)

#head(predDelay)
#write.table(x=predDelay, sep=",", file="pred_delay_results.csv",row.names = T)
summary(predDelay)
#month          predict           ARR_DELAY       
#Min.   : 1.00   Min.   : -79.202   Min.   :-117.000  
#1st Qu.: 4.00   1st Qu.: -14.060   1st Qu.: -14.000  
#Median : 7.00   Median :  -5.841   Median :  -6.000  
#Mean   : 6.58   Mean   :   5.080   Mean   :   5.007  
#3rd Qu.:10.00   3rd Qu.:   7.822   3rd Qu.:   8.000  
#Max.   :12.00   Max.   :1778.819   Max.   :1778.000  
#NA's   :560       

#predict           ARR_DELAY       
#Min.   :-200.358   Min.   :-120.000  
#1st Qu.: -14.089   1st Qu.: -14.000  
#Median :  -5.456   Median :  -6.000  
#Mean   :   5.593   Mean   :   5.117  
#3rd Qu.:   8.640   3rd Qu.:   8.000  
#Max.   :1994.825   Max.   :2023.000  
#NA's   :27542     
predPlot <- ggplot(data=predDelay, aes(x=p$predict, y=ARR_DELAY)) + ggtitle("Plot of Arrival DELAY") + geom_point(colour="blue") + geom_abline(intercept = 0, slope = 2,color = "red", size = 1)

# train$FL_DATE = as.factor(train$FL_DATE)
# train$OP_CARRIER = as.factor(train$OP_CARRIER)
# train$ORIGIN = as.factor(train$ORIGIN)
# train$DEST = as.factor(train$DEST)
# train$ARR_TIME = as.factor(train$ARR_TIME)
# train$CANCELLED = as.factor(train$CANCELLED)
# train$ARR_DELAY = as.factor(train$ARR_DELAY)

my_data_test <- predDelay
names(my_data_test) <- c("Month","Predict_Delay","Actual_Delay")
my_data_test <- melt(my_data_test,id = "Month")   

p10 <- ggplot(my_data_test, aes(x=factor(Month),y=value,fill=variable)) + geom_col(position = "dodge")
p10 + labs(
  title = "Predict Vs Actual Arrived Delay",
  subtitle = "2018",
  x = "Month",
  y = "Delay Time"
)
p10


Predict_Delay <- c("Min.   : -79.202","1st Qu.: -14.060","Median :  -5.841","Mean   :   5.080","3rd Qu.:   7.822","Max.   :1778.819")
Actual_Delay <- c("Min.   :-117.000","1st Qu.: -14.000","Median :  -6.000","Mean   :   5.007","3rd Qu.:   8.000","Max.   :1778.000")
p12 <- data.frame(Predict_Delay, Actual_Delay)

h20_DeepLearn <- c("MSE:","RMSE:","MAE:","Mean Residual Deviance :")
h2o.performance <- c("0.2330426","0.4827449","0.1813015","0.2330426")
p11 <- data.frame(h20_DeepLearn, h2o.performance)

#rm(arrive_delay,df,flight_2018,flights_delay,net_flights,subset)

# Define UI for application that draws a histogram
library(shinydashboard)
library(shiny)
header <- dashboardHeader(title = "2018 Flight Analysis")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Introdcution", tabName = "Introdcution", icon = icon("dashboard")),
  menuItem("Data Analysis", tabName = "EDA", icon = icon("calendar")),
  menuItem("Predict", tabName = "predict", icon = icon("book")),
  menuItem("Conclusion",tabName = "Conclusion",icon = icon("dashboard"))
))

tbs <- tabItems(
  # First tab content
  tabItem(
    tabName = "Introdcution",
    fluidRow(textOutput("text1"),width = 12,tags$head(tags$style("#text1{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text2"),width = 12,tags$head(tags$style("#text2{color: black;font-size: 20px;font-style: Arial;}")),
             textOutput("text3"),width = 12,tags$head(tags$style("#text3{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text4"),width = 12,tags$head(tags$style("#text4{color: black;font-size: 20px;font-style: Arial;}")),
             textOutput("text5"),width = 12,tags$head(tags$style("#text5{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text6"),width = 12,tags$head(tags$style("#text6{color: black;font-size: 20px;font-style: Arial;}"))
    )
  ),
  ######################################################
  #### Second tab content #####
  tabItem(
    tabName = "EDA",
    fluidRow(
      box(title = "Flight Cancellation Rate by Reason",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p1", height = "300px")),
      box(title = "Cancellation Rate by Airlines",status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p2", height = "300px"))
    ), #End of Fluid Row 1
    fluidRow(
      box(title = "Arrived Delay by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p3", height = "300px")),
      box(title = "Arrived Delay by Month of Airlines",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p4", height = "300px"))
    ), # End of fluid row 2
    fluidRow(
      box(title = "AA arrive delay by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p5", height = "300px")),
      box(title = "DL arrive delay by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p6", height = "300px"))
    ), # End of fluid Row 3
    fluidRow(
      box(width = 12,title = "Flight Delay Reason by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p7", height = "300px"))
    ), #End of fluid row 4
    fluidRow(
      box(width = 6,title = "AA Delay Reason by Month",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p8", height = "300px")),
      box(width = 6,title = "DL Delay Reason by Month" ,status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p9", height = "300px"))
    ) #End of fluid
  ),
  #######################################################
  # Third tab content
  tabItem(
    tabName = "predict",
    fluidRow(
      box(title = "h2o.performance of Deep Learning",status = "primary",solidHeader = TRUE,collapsible = TRUE,tableOutput("flight_p11")),
      box(title = "Summary for Prediction and Actual",status = "primary",solidHeader = TRUE,collapsible = TRUE,tableOutput("flight_p12"))
    ), #End of fluid Row1
    fluidRow(
      box(title = "Flight Prediction vs Actual Arrived delay",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p", height = "300px")),
      box(title = "Cancellation Rate by Airlines",status = "primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("flight_p10", height = "300px"))
    ) #End of fluid
  ),
  # Fourth tab content
  tabItem(
    tabName = "Conclusion",
    fluidRow(textOutput("text_con_1"),width = 12,tags$head(tags$style("#text_con_1{color: purple;font-size: 50px;font-style: italic;font-weight: bold;}")),
             textOutput("text_con_2"),width = 12,tags$head(tags$style("#text_con_2{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text_con_3"),width = 12,tags$head(tags$style("#text_con_3{color: black;font-size: 20px;font-style: Arial;}")),
             textOutput("text_con_4"),width = 12,tags$head(tags$style("#text_con_4{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text_con_5"),width = 12,tags$head(tags$style("#text_con_5{color: black;font-size: 20px;font-style: Arial;}")),
             textOutput("text_con_6"),width = 12,tags$head(tags$style("#text_con_6{color: black;font-size: 20px;font-style: Arial;}")),
             textOutput("text_con_7"),width = 12,tags$head(tags$style("#text_con_7{color: purple;font-size: 50px;font-style: italic;font-weight: bold;}")),
             textOutput("text_con_8"),width = 12,tags$head(tags$style("#text_con_8{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text_con_9"),width = 12,tags$head(tags$style("#text_con_9{color: black;font-size: 20px;font-style: Arial;}")),
             textOutput("text_con_10"),width = 12,tags$head(tags$style("#text_con_10{color: blue;font-size: 40px;font-style: italic;font-weight: bold;}")),
             textOutput("text_con_11"),width = 12,tags$head(tags$style("#text_con_11{color: black;font-size: 20px;font-style: Arial;}"))
    )
  )
  
)


# combine the two fluid rows to make the body
body <- dashboardBody(tbs,
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ))

#completing the ui part with dashboardPage
library(shinydashboard) 
library(shiny)
ui <-
  dashboardPage(title = 'This is my Page title', header, sidebar, body, skin =
                  'purple')

# Define server logic required to draw a histogram
library(shinydashboard) 
library(shiny)
server <- function(input, output) {
  output$text1 <- renderText({paste(text1)})
  output$text2 <- renderText({paste(text2)})
  output$text3 <- renderText({paste(text3)})
  output$text4 <- renderText({paste(text4)})
  output$text5 <- renderText({paste(text5)})
  output$text6 <- renderText({paste(text6)})
  
  output$flight_p1 <- renderPlot({p1})
  output$flight_p2 <- renderPlot({p2})
  output$flight_p3 <- renderPlot({p3})
  output$flight_p4 <- renderPlot({p4})
  output$flight_p5 <- renderPlot({p5})
  output$flight_p6 <- renderPlot({p6})
  output$flight_p7 <- renderPlot({p7})
  output$flight_p8 <- renderPlot({p8})
  output$flight_p9 <- renderPlot({p9})
  
  output$flight_p <- renderPlot({predPlot})    
  output$flight_p10 <- renderPlot({p10}) 
  #result of h2o
  output$flight_p11 <- renderTable({p11}) 
  output$flight_p12 <- renderTable({p12}) 
  
  
  output$text_con_1 <- renderText({paste(text_con_1)})
  output$text_con_2 <- renderText({paste(text_con_2)})
  output$text_con_3 <- renderText({paste(text_con_3)})
  output$text_con_4 <- renderText({paste(text_con_4)})
  output$text_con_5 <- renderText({paste(text_con_5)})
  output$text_con_6 <- renderText({paste(text_con_6)})
  output$text_con_7 <- renderText({paste(text_con_7)})
  output$text_con_8 <- renderText({paste(text_con_8)})
  output$text_con_9 <- renderText({paste(text_con_9)})
  output$text_con_10 <- renderText({paste(text_con_10)})
  output$text_con_11 <- renderText({paste(text_con_11)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)