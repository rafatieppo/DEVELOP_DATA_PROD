#=================================================================
                                             #Rafael Tieppo
                                             #tiepporc@unemat.br
                                             #02-12-2014
#=================================================================



#Week 1- Developing Data Products

#Getting stared
#install.packages("shiny")
#library(shiny)
#library(devtools)
#library(shinyapps)
#library(RCurl)
# 
#shinyUI(pageWithSidebar(
#    headerPanel("Histogram Plot"),
#     sidebarPanel(
#         h1('Side bar panel'),
#         h1('H1 Text'), 
#         h2('H2 Text'), 
#         h3('H3 Text'),
#         h4('H4 Text'),
#         sliderInput('mu', 'Guess a the mean', value = 70, min = 62, max
#                     = 74, step = 0.05)
#         
#         ),
#    mainPanel(
#        h3('Main Panel text'),
#        code('some code'),
#        p('some text'),
#        plotOutput('newHist')
#        )
#        ))

################

shinyUI(pageWithSidebar(
    
  headerPanel("AGCOM - Combine Module"),
    
    sidebarPanel(
        
    h6("This model estimates the operational cost of
         combines (grains). It a part of a full model,
         more information is availabel on :
         www.https://sites.google.com/site/com4agriculture"),
    h6("Choose the values to prediction"),

    sliderInput('AREA_', 'Select Area Size',value = 1000,
                min = 200, max = 10000, step = 250),

    sliderInput('TIME_', 'Select Available Time',value = 250,
                min = 100, max = 1000, step = 25),

    sliderInput('WIDTH_', 'Select Width Work (m)',value = 6,
                min = 5, max = 17, step = 1),
        
    sliderInput('SPEED_', 'Select Speed (km /h)',value = 6,
                min = 1, max = 15, step = 1),

    sliderInput('POWER_', 'Select Engine Power (cv) ',value = 200,
                min =100 , max = 750, step = 50,),

    sliderInput('PP_', 'Select Purchase Price',value = 300000,
                min = 200000, max = 1000000, step = 50000),

    sliderInput('FP_', 'Select Fuel Price $/L',value = 2 ,
                min =1 , max = 5, step = .50),

    sliderInput('OUTSOURC_', 'Select Outsourcing Cost',value = 110 ,
                min =50 , max = 200, step = 10)
        
              ),
    
  mainPanel(
    h5('Cost Prediction of Combines'),
    plotOutput('Plot_Cost')
    #verbatimTextOutput('teste')
           ) 
                        ) )



#  plotOutput('newHist')
#    sliderInput('FC', 'Select Fuel Price $/L  ',value = 2 , min =1 , max = 5, step = .50,)
#              ),
#    sliderInput('PP', 'Select Purchase Price',value = 300000,
 #               min = 200000, max = 1000000, step = 50000)
