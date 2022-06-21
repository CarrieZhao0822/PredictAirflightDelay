Flight Delays and Cancellation Analysis & Prediction
Yan Zhao, Mohamed Ahmed, Gang Wu
zhao1146@purdue.edu; ahmed179@purdue.edu; wu1865@purdue.edu
Abstract
The main goal of this project is to create a shiny app in R using descriptive and predictive 
analytics knowledge we gained in our class. Our goal is to provide U.S Airline companies with 
insights on the impact of flight delays on economy and consumers and how to make better 
business decisions that can save airline companies millions of dollars due to flight delays. To 
provide useful insights, we created a model using shiny app that provides descriptive analysis 
and visualizations that shows the number of canceled flights for the chosen year and cancelations 
and delays reasons. Moreover, the shiny app provides a predictive analysis to make prediction on 
future delays which can provide Airline companies of important and useful insight to analyze 
their performance, make decisions to mitigate these delays and increase customer satisfaction. 
Business Problem: 
Everyday thousands of flights are either delayed or canceled all over the United Sates. These 
delays do not only effect travelers but also Airline companies. The average cost of delay for U.S. 
passenger airlines is around $74.24 per minute which can translates to billions of dollars of cost 
to U.S Airline companies. Analyzing the reasons and number of delays can give a better 
understanding in predicting future delays and how they can be mitigated. There are some delay 
reasons that can’t be controlled but most of the reasons are known and can be identified to help 
reduce the number of delays. Predicating and understanding the reasons of delays will help 
Airline companies save time, money and increase customer satisfaction. 
Analytics Problem
Help traveler find actual flight delay reasons (Carriers, NAS, Security, Weather) and light 
cancellations rate. This can benefit Airline companies to improve their operations. Moreover, we 
can predict the flight delay for next year or future years to let travels and Airline companies have 
future years forecast.
Data
To create a reliable model that can help do future predictions a reliable data set with all 
needed information is needed. The data used for our shiny app and analysis was originally 
collected from U.S DOT Bureau of Transportation Statistics. For our project, we had to 
clean the data we have as the original data set includes millions of observations. Thus, we 
only used the flight delays data from 2018 for our analysis and prediction due to the 
limitation of the free version of shiny app.
Methodology Selection
• R: Using R is very important as it is free and an open source. Moreover, R provides many 
useful packages and advanced visualizations to do predictions. 
• Shiny: To have a better understanding of our data and business problem we first used 
descriptive analytics to analyze what information we have, causes and effects and to make 
informed decisions. Thus, we used Visualization and Exploratory Data Analysis (EDA) and 
some statistical summarization. Since we have multiple visualizations output to show to our 
audience, Shiny is the best tool to use in this case since we can use Shiny dashboard to 
communicate result interactive charts, visualizations, text, or tables.
• H2O: In addition, we used predictive analytics with H2O for our model to create prediction 
that can help our client to predict delays and mitigate them.
Model Building
For prediction, we used Neural Network method in R since we are comparing multiple models
and neural network is the best fit for this scenario which has the lowest MSE. The benefit of 
using the neural network is because it draws from the parallel processing of information.
Functionality 
We have used several useful R packages for our analysis and prediction such that helped our 
DDS to create the functionalities below: 
• Analytics Airflight Cancellation Rate by Reason
• Analytics Airflight Delay by Month
• Analytics Cancellation Percentage per Airline
• Analytics Airflight Delay per Airline
• Predict Delay Per Month
Conclusion
We found there are multiple factors for flight delays that makes a positive impact on the delay 
time. Some are external factors, such as, the month of the year, the specific operation carrier 
company, the departing airport, and the arrival airport. Other factors are internal that can be used 
to calculate the actual delay time such as, CRS Elapsed Time of Flight, actual elapsed time of the 
flight, arrival delay time, and the distance between departure airport and the arrival airport. For 
our airline operation management, we suggest having the data engineering department to 
measure the need for each air route, do not over add flight as that could cause unnecessary delays 
and unsold seats which lower the company revenue. For our airline passengers, we suggest 
avoiding busy traveling months with high delay and cancelation rates and to have a plan b in 
these months in case of delay of cancellation.

References
https://www.kaggle.com/yuanyuwendymu/airline-delay-and-cancellation-data-2009-2018
https://www.transtats.bts.gov/Homepage.asp
https://www.airlines.org/dataset/u-s-passenger-carrier-delay-costs/
https://github.com/thepiratex/AttritionAnalysisRShiny
Group5 final project links
GitHub link: https://github.com/CarrieZhao0822/PredictAirflightDelay
ShinyApp link: https://purduegwu.shinyapps.io/final_project_group5/
Video presentation link: https://app.box.com/s/5n3bddpq5q8fdx41fgvxmf4omwwk7mf0
