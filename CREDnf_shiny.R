library(shiny)

if (interactive()) {

  ui <- fluidPage(
    titlePanel("CRED-nf Checklist"),

    navlistPanel(
      "Domains",
      tabPanel("Pre-experiment",
               h3("Pre-Experiment"),
               wellPanel(
                 selectInput("checklist1a", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new1a"),
                 textOutput("text1a")
               ),
               wellPanel(
                 selectInput("checklist1b", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new1b"),
                 textOutput("text1b")
               )
               ),

      tabPanel("Control groups",
               h3("Control groups"),
               wellPanel(
                 selectInput("checklist2a", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new2a"),
                 textOutput("text2a")
               ),
               wellPanel(
                 selectInput("checklist2b", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new2b"),
                 textOutput("text2b")
               ),
               wellPanel(
                 selectInput("checklist2c", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new2c"),
                 textOutput("text2c")
               ),
               wellPanel(
                 selectInput("checklist2d", h4(),
                             choices = list("Not at all" = 1, "Provide details" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new2d"),
                 textOutput("text2d")
               ),
               wellPanel(
                 selectInput("checklist2e", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new2e"),
                 textOutput("text2e")
               )
               ),

      tabPanel("Control measures"),
      tabPanel("Feedback specifications"),
      tabPanel("Outcome measures - Brain"),
      tabPanel("Outcome measures - Behaviour"),
      tabPanel("Data storage"),
      tabPanel("Checklist summary",
               h3("CRED-nf checklist summary output"),
               tags$div(
                 tags$ol(
                   tags$li("Pre-experiment"),
                   tags$ol(
                     tags$li(textOutput("summary1a")),
                     tags$li(textOutput("summary1b")),
                     type="a"
                   ),

                   tags$li("Control groups"),
                   tags$ol(
                     tags$li(textOutput("summary2a")),
                     tags$li(textOutput("summary2b")),
                     tags$li(textOutput("summary2c")),
                     tags$li(textOutput("summary2d")),
                     tags$li(textOutput("summary2e")),
                     type="a"
                   )
                 )
               )
               # textOutput("summary1a"),
               # br(),
               # textOutput("summary1b")
               ),
      widths=c(2,10)
    )

  )





  server <- function(input, output, session) {

    # A vector of pre-existing choices:
    mychoices <- c("No", "Yes", "Not applicable")

    observe({
      updateSelectInput(session, inputId = "checklist1a", label = "Pre-register experiment protocol and planned analysis",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist1b", label = "Justify sample size",
                        choices = mychoices)
    })


    observe({
      updateSelectInput(session, inputId = "checklist2a", label = "Employ control group(s) or control condition(s)",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist2b", label = "When leveraging experimental designs where a double-blind is possible, use a double-blind",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist2c", label = "Were those who rate the outcomes (including statisticians where relevant) blinded?",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist2d", label = "To what extent did participants and experimenters remain blinded?",
                        choices = c("Not at all", "Provide details", "Not applicable"))
    })
    observe({
      updateSelectInput(session, inputId = "checklist2e", label = "If this is a clinical efficacy study, was a standard-of-care intervention group used as a benchmark for improvement?",
                        choices = mychoices)
    })


    # Open end box to enter text - if the user has selected "yes":
    output$new1a <- renderUI({
      if (!input$checklist1a == "Yes") return(NULL) else {
        textInput("response1a", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })

    output$new1b <- renderUI({
      if (!input$checklist1b == "Yes") return(NULL) else {
        textInput("response1b", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })

    output$new2a <- renderUI({
      if (!input$checklist2a == "Yes") return(NULL) else {
        textInput("response2a", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new2b <- renderUI({
      if (!input$checklist2b == "Yes") return(NULL) else {
        textInput("response2b", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new2c <- renderUI({
      if (!input$checklist2c == "Yes") return(NULL) else {
        textInput("response2c", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new2d <- renderUI({
      if (!input$checklist2d == "Provide details") return(NULL) else {
        textInput("response2d", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new2e <- renderUI({
      if (!input$checklist2e == "Yes") return(NULL) else {
        textInput("response2e", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })


    # Return text
    output$text1a <- renderText({
      if (input$checklist1a == "Yes") {
        return(input$response1a)
      }
    })

    output$text1b <- renderText({
      if (input$checklist1b == "Yes") {
        return(input$response1b)
      }
    })


    output$text2a <- renderText({
      if (input$checklist2a == "Yes") {
        return(input$response2a)
      }
    })
    output$text2b <- renderText({
      if (input$checklist2b == "Yes") {
        return(input$response2b)
      }
    })
    output$text2c <- renderText({
      if (input$checklist2c == "Yes") {
        return(input$response2c)
      }
    })
    output$text2d <- renderText({
      if (input$checklist2d == "Provide details") {
        return(input$response2d)
      }
    })
    output$text2e <- renderText({
      if (input$checklist2e == "Yes") {
        return(input$response2e)
      }
    })



    output$summary1a <- renderText({
      if (input$checklist1a=="Yes") {
        return(input$response1a)
      } else if (input$checklist1a=="No") {
        return("Protocol and planned analyses were not pre-registered")
      } else if (input$checklist1a=="Not applicable") {
        return("Protocol and planned analyses were not applicable")
      }
    })

    output$summary1b <- renderText({
      if (input$checklist1b=="Yes") {
        return(input$response1b)
      } else if (input$checklist1b=="No") {
        return("No justification was given for the sample size")
      } else if (input$checklist1b=="Not applicable") {
        return("A sample size was not applicable for this study")
      }
    })


    output$summary2a <- renderText({
      if (input$checklist2a=="Yes") {
        return(input$response2a)
      } else if (input$checklist2a=="No") {
        return("No control group(s)/condition(s) were employed in this study")
      } else if (input$checklist2a=="Not applicable") {
        return("Control group(s)/condition(s) were not applicable for this study")
      }
    })

    output$summary2b <- renderText({
      if (input$checklist2b=="Yes") {
        return(input$response2b)
      } else if (input$checklist2b=="No") {
        return("This study did not use a double-blind design")
      } else if (input$checklist2b=="Not applicable") {
        return("A double-blind design was not applicable for this study")
      }
    })

    output$summary2c <- renderText({
      if (input$checklist2c=="Yes") {
        return(input$response2c)
      } else if (input$checklist2c=="No") {
        return("Those who rated the outcomes or those who performed the analysis were not blinded")
      } else if (input$checklist2c=="Not applicable") {
        return("Blinding of those who rated the outcomes or those who performed the analysis was not applicable for this study")
      }
    })

    output$summary2d <- renderText({
      if (input$checklist2d=="Provide details") {
        return(input$response2d)
      } else if (input$checklist2d=="Not at all") {
        return("Participants and experimenters did not remain blinded")
      } else if (input$checklist2d=="Not applicable") {
        return("Blinding of participants and experimenters was not applicable for this study")
      }
    })

    output$summary2e <- renderText({
      if (input$checklist2e=="Yes") {
        return(input$response2e)
      } else if (input$checklist2e=="No") {
        return("A standard-of-care intervention group was not used as a benchmark for improvement")
      } else if (input$checklist2e=="Not applicable") {
        return("A standard-of-care intervention group was not applicable for this study, or this was not a clinical efficacy study")
      }
    })

    # Add option to export to PDF and/or Docx
    # Add warning (and specify number) of any responses marked as "NO" at start of summary before output

  }

  shinyApp(ui, server)
  }
