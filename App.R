#setwd("H:/USER/JGenser/Framingham Study/R Shiny/Framingham 2.0")
library(shiny)
library(ggplot2)
library(ggthemes)

df_scores = read.table("df3_scores.csv", header=T, sep=",")
riskTable = read.table("Framingham Scores.csv", header=T, sep=",")

##function to calculate risk scores based on health variables

calcRisk = function(AGE, SEX, TOTCHOL, HDLC, SYSBP, CURSMOKE, riskTable){
  
  
  men_risk = subset(riskTable, SEX==1)
  wmn_risk = subset(riskTable, SEX==2)
  
  ##calculate risk points if MALE
  if(SEX == 1){
    
    if(AGE <=34) age_pts <- -1
    else if (AGE <= 39) age_pts <- 0
    else if (AGE <= 44) age_pts <- 1
    else if (AGE <= 49) age_pts <- 2
    else if (AGE <= 54) age_pts <- 3
    else if (AGE <= 59) age_pts <- 4
    else if (AGE <= 64) age_pts <- 5
    else if (AGE <= 69) age_pts <- 6
    else age_pts <- 7
    
    if(TOTCHOL < 160) chol_pts <- -3
    else if (TOTCHOL <= 199) chol_pts <- 0
    else if (TOTCHOL <= 239) chol_pts <- 1
    else if (TOTCHOL <= 279) chol_pts <- 2
    else chol_pts <- 3
    
    if (HDLC <= 35) hdl_pts <- 2
    else if (HDLC <=44) hdl_pts < -1
    else if (HDLC <=59) hdl_pts <- 0
    else if (HDLC >=60) hdl_pts <- -2
    
    if (SYSBP < 129) bp_pts <- 0
    else if (SYSBP <= 139) bp_pts <- 1
    else if (SYSBP <= 159) bp_pts <- 2
    else if (SYSBP >= 160) bp_pts <- 3
    
    if (CURSMOKE == F) smoke_pts = 0
    else if (CURSMOKE == T) smoke_pts = 2
    
  }
  
  ##calculate risk points if FEMALE
  else if (SEX == 2){
    
    if(AGE <=34) age_pts <- -9
    else if (AGE <= 39) age_pts <- -4
    else if (AGE <= 44) age_pts <- 0
    else if (AGE <= 49) age_pts <- 3
    else if (AGE <= 54) age_pts <- 6
    else if (AGE <= 59) age_pts <- 7
    else if (AGE >= 60) age_pts <- 8
    
    if(TOTCHOL < 160) chol_pts <- -2
    else if (TOTCHOL <= 199) chol_pts <- 0
    else if (TOTCHOL <= 239) chol_pts <- 1
    else if (TOTCHOL <= 279) chol_pts <- 1
    else if (TOTCHOL >= 280) chol_pts <- 3
    
    if (HDLC <= 35) hdl_pts <- 5
    else if (HDLC <=44) hdl_pts <- 2
    else if (HDLC <=49) hdl_pts <- 1
    else if (HDLC <= 59) hdl_pts <- 0
    else if (HDLC >=60) hdl_pts <- -3
    
    if (SYSBP < 120) bp_pts <- -3
    else if (SYSBP <= 139) bp_pts <- 0
    else if (SYSBP <= 159) bp_pts <- 2
    else if (SYSBP >= 160) bp_pts <- 3
    
    if (CURSMOKE == F) smoke_pts = 0
    else if (CURSMOKE == T) smoke_pts = 2
    
  }
  
  total_pts = age_pts + chol_pts + hdl_pts + bp_pts + smoke_pts
  
  if (SEX==1){
    risk_pct = men_risk$risk[match(total_pts, men_risk$tot_pts)]
  }
  else if (SEX==2){
    risk_pct = wmn_risk$risk[match(total_pts, wmn_risk$tot_pts)]
  }
  
  
  return(risk_pct)
}




# ui = fluidPage(
#   titlePanel("Framingham Study Population"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       
#       radioButtons("sex", label = h4("Gender"),
#                    choices = list("Male" = 1, "Female" = 2), 
#                    selected = 1),
#       
#       sliderInput("age", label = h4("Age"), min = 30, 
#                              max = 75, value = 50),
#     
#       sliderInput("totchol", label = h4("Total Cholesterol"), min = 100,
#                   max = 600, value = 175),
#       
#       sliderInput("hdl", label = h4("HDL Cholesterol"), min = 10, max = 200, value = 35),
#       
#       sliderInput("sysbp", label = h4("Systolic Blood Pressure"), min = 80, max = 300, value = 120),
#       
#       checkboxInput("smoke",  label = "Smoker", value = F),
#       
#       fluidRow(column(3, verbatimTextOutput("value")))
#       
#     ),
#   
#     
#     mainPanel("Risk of Heart Attack within 10 years - Density Distribution",
#               plotOutput("plot"))
#   )
# )


ui = fluidPage(
  titlePanel("Risk of Heart Attack within 10 years - Density Distribution (Framingham Study Population)"),
  absolutePanel(
    top=20, right=20, width=200,
    draggable=T,
    radioButtons("sex", label=h6("Gender"), choices=list("Male"=1, "Female"=2),selected=1),
    sliderInput("age", label=h6("Age"), min=30, max=75, value=50),
    sliderInput("totchol", label=h6("Total Cholesterol"), min=100, max=600, value=175),
    sliderInput("hdl", label=h6("HDL Cholesterol"), min=10, max=200, value=35),
    sliderInput("sysbp", label=h6("Systolic Blood Pressure"), min=80, max=300, value=120),
    checkboxInput("smoke", label="Current Smoker", value=F),
    style = "opacity: 0.92"
    
  ),
  plotOutput("plot", height="40em"),
  hr()
  
#   fluidRow(
#     
#     column(3,
#           radioButtons("sex", label=h6("Gender"), choices=list("Male"=1, "Female"=2),selected=1),
#           sliderInput("age", label=h6("Age"), min=30, max=75, value=50)
#           
#     ),
#     
#     column(3,
#            sliderInput("totchol", label=h6("Total Cholesterol"), min=100, max=600, value=175),
#            sliderInput("hdl", label=h6("HDL Cholesterol"), min=10, max=200, value=35)
# 
#     ),
#     column(3,
#            sliderInput("sysbp", label=h6("Systolic Blood Pressure"), min=80, max=300, value=120),
#            checkboxInput("smoke", label="Current Smoker", value=F)
#            )
  # )
  
  
  
  
  
)
  
server <- function(input, output) {
  

#getRisk <- reactive({
 #   risk <-  calcRisk(input$age, input$sex, input$totchol, input$hdl, input$sysbp, input$smoke, riskTable)
  
 # })  


  
output$plot <- renderPlot({ 
    
  risk = calcRisk(input$age, input$sex, input$totchol, input$hdl, input$sysbp, input$smoke, riskTable)
  p = ggplot(df_scores, aes(x = risk)) + 
    geom_histogram(aes(y=..density..), color="snow2", fill="grey80")  +
    geom_density(fill = "lightblue3", alpha=0.3, color="snow2") +
    scale_x_continuous(breaks = seq(0,0.6,0.05)) 

  
  
    risk = calcRisk(input$age, input$sex, input$totchol, input$hdl, input$sysbp, input$smoke, riskTable)
    # abline = geom_abline(intercept = , color = "red", linetype = "dashed", size = 0.5)
    line = geom_vline(xintercept = risk, color = "red", linetype ="dashed", size = 0.5) 
    #scale = scale_fill_economist()
    labs = labs(x="Risk", y = "Density")
  print(p + line +labs)
  
  })

  output$value <- renderPrint({
    
    risk = calcRisk(input$age, input$sex, input$totchol, input$hdl, input$sysbp, input$smoke, riskTable)
    print(risk)
  })
}


shinyApp(ui = ui, server = server)



