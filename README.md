# Mobile-Price-Predictor
UNDERSTANDING DATA :

Our problem statment is based on Mobile Price Classification which is Crucial aspect in smartphone industry . In this we will help 


1.) First of all we will analyze the dataset completly and understand the meaning of every      column , dimentions of the data , data types of the columns ,  understanding the insights in it describe(summary of the columns) .

2.) To understand numerical or categorical better it is good to use visualization , it's better to use pie chart and for analyzing our target column again we use pie chart to understand the distributions in efficient way 

3.) To understand the pattrens in the dataset we will be doing EDA(EXPLORATORY DATA ANALYSIS)
    * UNI VARIATE ANALYSIS   
           EXPLORING CATEGORICAL DATA :
           ---- USING COUNT PLOTS
           EXPLORING NUMERICAL DATA :
           ---- USING BOX-PLOTS
           ---- USING KDE-PLOTS
    * BI VARIATE ANALYSIS
           ---- USING SCATTER-PLOTS
           ---- GROUPED COUNT-PLOTS
    * MULTI VARIATE ANALYSIS
           ---- USING SCATTER PLOTS

4.) (i)   Checking the missing values , Removing missing values or imputing them according to distribution 
    (ii)  Checking the DUPLICATED DATA , Removing the DUPLICATES in data
    (iii) Checking Redundancy and removing it 
    (iv)  OUTLIER DETECTION by BOX-PLOTS and Removing them by INTER QUARTILE RANGE 
    (v)   Checking NOISE IN THE DATA , by using PAIR-PLOTS
 
5.)  CORRELATION : Used to identify the similar behaviour among the data in between different columns. Values lie in between -1 to 1. We are plotting the heatmap to find CORRELATION . There will be a thrushold value for data we are dealing if the value of correlation is greater than thrushold range we will be removing those features . 


6.) (i)  Splitting the data into train and test in specified ratio . Finding the dimentions of the train data accordingly and test data accordingly . 
    (ii) MODEL IMPLEMENTATION : We can use the model based on our datset we are using DECISION TREE - CART ALGORITHM . 
         USES ARE :-
     








7.) Calculating the ACCURACY , PRECISION , RECALL , F1-SCORE based on our model . 
    VISUALIZE the ROC CURVE 


