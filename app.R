library(shiny)
library(ggplot2)

unitcode <-"HPS104"
assignment1_name <- "Assignment Part A"
assignment1_weight <- 10
assignment1_due <-7

assignment2_name <- "Assignment Part B"
assignment2_weight <- 10
assignment2_due <-10

assignment3_name <- "Assignment Part C"
assignment3_weight <- 20
assignment3_due <-12

assignment4_name <- "Assignment Part D"
assignment4_weight <- 10
assignment4_due <-12

ui <- navbarPage(
  paste(unitcode, "Grade Tracker"),
  
  
  tabPanel("Tracker",
           # Sidebar ----
           sidebarLayout(
             sidebarPanel(
               h4("Target Grade"),
               numericInput("target",
                            "I am aiming for a score of x %",
                            value = 65),
               sliderInput("week",
                            "What is the current week of trimester?",
                            min = 0,
                            max = 12,
                            value = 0),
               p("This tool makes assumptions about the marks available based on the week of trimester, so will provide misleading estimates prior to the release of grades."),
               hr(),
               h4("Assignments"),
               numericInput(
                 "assignment1",
                 paste(assignment1_name," % Grade"),
                 min = 0,
                 max = 100,
                 value = 0
               ),
               numericInput(
                 "assignment2",
                 paste(assignment2_name," % Grade"),
                 min = 0,
                 max = 100,
                 value = 0
               ),
               numericInput(
                 "assignment3",
                 paste(assignment3_name," % Grade"),
                 min = 0,
                 max = 100,
                 value = 0
               ),
               numericInput(
                 "assignment4",
                 paste(assignment4_name," % Grade"),
                 min = 0,
                 max = 100,
                 value = 0
               )
             ),
             
             # Main panel ----
             mainPanel(
               textOutput("statement1"),
               textOutput("statement2"),
               plotOutput("targetPlot")
             )
           )),
  # About tab ----
  tabPanel(
    "About",
    fluidPage(
      "This was built by Mathew Ling",
      a("(@lingtax)", href = "https://twitter.com/lingtax"),
      " to help students better understand their study targets.",
      p(),
      "It is powered by ",
      strong("R"),
      " and the ",
      strong("ggplot2"),
      " package, and is built in ",
      strong("Shiny"),
      ".",
      
      p(),
      "This tool makes assumptions about the marks available based on the week of trimester, so may provide misleading estimates immediately prior to the release of grades.",
      p(),
      "For bug reports and feature requests, please raise an issue at ",
      a("this github repository", href = "https://github.com/Lingtax/GradeTracker/issues/new")
    )
  )
)


# Server logic ----
server <- function(input, output) {
  
  week <- reactive({ifelse(input$week>=assignment4_due, 5,
                           ifelse(input$week>=assignment4_due, 4,
                           ifelse(input$week>=assignment2_due, 3,
                                  ifelse(input$week>=assignment1_due, 2, 1
                                  ))))
  })
  
  denom <- reactive({
    ifelse(input$week>=assignment4_due, assignment1_weight + assignment2_weight + assignment3_weight + assignment4_weight,
           ifelse(input$week>=assignment3_due, assignment1_weight + assignment2_weight + assignment3_weight,
                  ifelse(input$week>=assignment2_due, assignment1_weight + assignment2_weight,
                         ifelse(input$week>=assignment1_due, assignment1_weight, 0
                         ))))
  })
  

  
  weights <-
    reactive({
      list(
        assignment1weight = round(input$assignment1 / 100 * assignment1_weight, 2),
        assignment2weight = round(input$assignment2 / 100 * assignment2_weight, 2),
        assignment3weight = round(input$assignment3 / 100 * assignment3_weight, 2),
        assignment4weight = round(input$assignment4 / 100 * assignment4_weight, 2)
      )
    })
  
  weightsum <- reactive({sum(
    weights()$assignment1weight,
    weights()$assignment2weight,
    weights()$assignment3weight,
    weights()$assignment4weight
  )})
  
  output$statement1 <- renderText({
    paste(
      "The entered grades amount to ",
      weightsum(),
      "% out of the ",
      denom(),
      "% available so far",
      ifelse(denom()>0, 
             paste(", which makes your running average ",
                   round(weightsum()/denom()*100, 2),
                   "%.", sep =""),
             "."), 
      sep ="" 
    )
  })
  output$statement2 <- renderText({
    if((input$target - weightsum())/(100-denom())<1){
      paste0(
        "To exceed your target of ", 
        input$target, 
        "%, you will require an average of over ",
        round((input$target - weightsum())/(100-denom())*100, 2), 
        "% across the remaining assessments.")
    } else {
      "You cannot reach the specified target with the remaining marks available."
    }
  }) 
  
  output$targetPlot <- renderPlot({
    
    df <- data.frame(Week = c(0,
                              assignment1_due,
                              assignment2_due,
                              assignment3_due,
                              assignment4_due),
                     Score = c(0,
                               weights()$assignment1weight, 
                               weights()$assignment1weight + weights()$assignment2weight, 
                               weights()$assignment1weight + weights()$assignment2weight + weights()$assignment3weight,
                               weights()$assignment1weight + weights()$assignment2weight + weights()$assignment3weight + weights()$assignment4weight
                     )
    )
    df <- df[1:week(), ]
     
    ggplot(df, aes(Week, Score)) +
      scale_x_continuous(limits = c(0,13), breaks =  1:12, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
      geom_line(cex = 1) +
      geom_hline(yintercept = input$target, colour = "red", linetype="dashed") +
      theme_classic()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

