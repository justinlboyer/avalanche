#This is a script that creates an ensmeble forecast for predicting avlanches with 13 diff models, 10 ridge regression models, 1 linear, 1 logistic, and 1 lasso

# Load models
load("ensembleBestLam.R")
load("ensembleRRFit.rda")
load("ensembleLasBestLam.R")
load("ensembleLasso.rda")
load("ensembleLinear.rda")
load("ensembleLogistic.rda")

#Generate current weather data
source("currentWeather.R")
currWeat <- currentWeather()


# This model uses the daily precipitation (.1mm), snowdepth (in), snowfall(in), MaxTemp(F), MinTemp(F)


#library(dplyr)
library(glmnet)
library(shiny)

# Define UI for application 
ui <- shinyUI(fluidPage(#
  # Application title
  titlePanel("Avalanche Forecaster"),
  sidebarLayout(#
    sidebarPanel("The default inputs are the current 24 hour weather conditions taken from the", a("NOAA's Alta Guard station", href = "http://www.wrh.noaa.gov/mesowest/getobext.php?wfo=slc&graph=off&sid=agd", target="_blank"),br(),br(), strong("Important")," This models predictions are based on observations of avalanches from people like you.  This is a tool, just like an ECT.  Be conservative in your judgement.  Play with the model, you will notice that an increase in snowfall leads to a decrease in the probability of an avalanche occuring, this is likely due to a bias in the sampling.  In other words, the forecaster is not perfect.", br(), br(), strong("Use at your own risk... skull and crossbones!"),
                 br(), br(), br(),#
                 numericInput("preci", "24 hr Precipitation - Water Equivalency (in)", currWeat[4],  min = 0, max = 10, step = 0.1),
                 numericInput("snwd","Snow depth (in)", currWeat[3], min = 0, max = 300, step = 1),
                 numericInput("snf"," 24 hr. Snowfall (in)", currWeat[5], min = 0, max = 150, step = 1),
                 numericInput("mxt","Maximum Temperature (degrees Farenheit)", currWeat[2], min = -40, max = 100, step = 1),
                 numericInput("mint","Minimum Temperature (degrees Farenheit)", currWeat[1], min = -40, max = 100, step = 1),width = 8),
    mainPanel(br(),br(),br(), br(),br(),br(),br(),br(),br(),br(),br(), br(),br(),"Based on reported historical observations and the inputs supplied:", br(), h2(textOutput("results1")), strong("of the models predict the occurence of an avalanche."), width = 4)),
  "This probability was created to provide another tool in evaluating danger in the backcountry.  The probability is generated by datasets provided by the Utah Avalanche Center, and the NOAA, which populate 13 different models.  The forecast has a 95% confidence interval between 77% and 85%, the sensitivity is 60%, the specificity is 91%, with 0 (no avalanches occuring) being positive, 1 (avalanches occuring) being negative.  Only use this tool to supplement your information, this tool does not replace common sense, in field oberservations, or freak occurences.   If you would like to contribute please visit the project page at ", 
  a("https://github.com/justinlboyer/avalanche", href = "https://github.com/justinlboyer/avalanche",target="_blank"), br(), br()
  )#
)
   
#Conversions
#(f-32)*5/9
#.45meps = 1 mph
#1in = 25.4mm
   
   # Define Server
   server <- function(input, output, session){
     value <- reactive(#
       {x.in <- t(c(input$preci*25.4, input$snwd*25.4, input$snf*25.4, (input$mxt-32)*5/9, (input$mint-32)*5/9))
       x.inDF <- data.frame(x.in)
       colnames(x.inDF) <- c("Precipitation","Snow_Depth","Snowfall","Max_Temperature","Min_Temperature")
       #Initialize Prediction vector
       pred <- numeric(13)
       #Predict logistic
       pred[1] <- predict(mdl,newdata = x.inDF)
       #Predict Lasso
       pred[2] <- predict(las.fit, s = las.bestlam, newx = x.in)
       #Predict Linear
       pred[3] <- predict.lm(lm.fit1, newdata = x.inDF)
       #Predict Intercept
       #pred[4] <- interceptFit
       #Predict Using 10 RR models
       for (l in 4:13) {
         pred[l] <- predict(fit[[l-3]],s=bestlam[[l-3]],newx = x.in)
       }
       #Restrict values between 0 and 1
       pred[pred<0] <- 0
       pred <- tanh(pred)
       pred})
     output$results1 <- renderText(paste({round(sum(round(value()))/length(value())*100,2)},"%"))
   }
   
shinyApp(ui = ui, server = server)  
