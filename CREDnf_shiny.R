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

      tabPanel("Control measures",
               h3("Control measures"),
               wellPanel(
                 selectInput("checklist3a", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new3a"),
                 textOutput("text3a")
               ),
               wellPanel(
                 selectInput("checklist3b", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new3b"),
                 textOutput("text3b")
               ),
               wellPanel(
                 selectInput("checklist3c", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new3c"),
                 textOutput("text3c")
               ),
               wellPanel(
                 selectInput("checklist3d", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new3d"),
                 textOutput("text3d")
               ),
               wellPanel(
                 selectInput("checklist3e", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new3e"),
                 textOutput("text3e")
               )
      ),

      tabPanel("Feedback specifications",
               h3("Feedback specifications"),
               wellPanel(
                 selectInput("checklist4a", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new4a"),
                 textOutput("text4a")
               ),
               wellPanel(
                 selectInput("checklist4b", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new4b"),
                 textOutput("text4b")
               ),
               wellPanel(
                 selectInput("checklist4c", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new4c"),
                 textOutput("text4c")
               ),
               wellPanel(
                 selectInput("checklist4d", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new4d"),
                 textOutput("text4d")
               ),
               wellPanel(
                 selectInput("checklist4e", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new4e"),
                 textOutput("text4e")
               )
               ),

      tabPanel("Outcome measures - Brain",
               h3("Outcome measures - Brain"),
               wellPanel(
                 selectInput("checklist5a", h4(),
                             choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
                 uiOutput("new5a"),
                 textOutput("text5a")
               ),
               wellPanel(
                 selectInput("checklist5b", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new5b"),
                 textOutput("text5b")
               ),
               wellPanel(
                 selectInput("checklist5c", h4(),
                             choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
                 uiOutput("new5c"),
                 textOutput("text5c")
               )
               ),
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
                   ),

                   tags$li("Control measures"),
                   tags$ol(
                     tags$li(textOutput("summary3a")),
                     tags$li(textOutput("summary3b")),
                     tags$li(textOutput("summary3c")),
                     tags$li(textOutput("summary3d")),
                     tags$li(textOutput("summary3e")),
                     type="a"
                   ),

                   tags$li("Feedback specifications"),
                   tags$ol(
                     tags$li(textOutput("summary4a")),
                     tags$li(textOutput("summary4b")),
                     tags$li(textOutput("summary4c")),
                     tags$li(textOutput("summary4d")),
                     tags$li(textOutput("summary4e")),
                     type="a"
                   ),

                   tags$li("Outcome measures - Brain"),
                   tags$ol(
                     tags$li(textOutput("summary5a")),
                     tags$li(textOutput("summary5b")),
                     tags$li(textOutput("summary5c")),
                     type="a"
                   )
                 )
               ),

               br(), br(),
               downloadButton("reportpdf", "Download summary")
               ),
      widths=c(2,10)
    )

  )





  server <- function(input, output, session) {

    # A vector of pre-existing choices:
    mychoices <- c("No", "Yes", "Not applicable")
    mychoices2 <- c("Not applicable", "Provide details")

    observe({
      updateSelectInput(session, inputId = "checklist1a", label = "Does the study have a pre-registered protocol including planned analyses?",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist1b", label = "Is there a justification for the sample size used?",
                        choices = mychoices)
    })


    observe({
      updateSelectInput(session, inputId = "checklist2a", label = "Were control group(s) or control condition(s) employed?",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist2b", label = "When leveraging experimental designs where a double-blind is possible, was a double-blind design used?",
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


    observe({
      updateSelectInput(session, inputId = "checklist3a", label = "Was data collected on psychosocial factors?",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist3b", label = "Were participants provided with a strategy?",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist3c", label = "Report the strategies participants used",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist3d", label = "Report methods used for online data-processing and artifact collection",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist3e", label = "Report condition and group effects for artifacts",
                        choices = mychoices2)
    })


    observe({
      updateSelectInput(session, inputId = "checklist4a", label = "Report how the online-feature extraction was defined",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist4b", label = "Report and justify the reinforcement schedule",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist4c", label = "Report the feedback modality and content",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist4d", label = "Collect and report all brain activity variable(s) and/or contasts used for feedback, as displayed to experimental participants",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist4e", label = "Report the hardware and software used",
                        choices = mychoices2)
    })


    observe({
      updateSelectInput(session, inputId = "checklist5a", label = "Report neurofeedback regulation success based on the feedback signal",
                        choices = mychoices2)
    })
    observe({
      updateSelectInput(session, inputId = "checklist5b", label = "Have within-session and between-session regulation blocks of feedback variable(s) been plotted, as well as pre-to-post resting baselines or contrasts?",
                        choices = mychoices)
    })
    observe({
      updateSelectInput(session, inputId = "checklist5c", label = "Has the experimental condition/group been statistically compared to the control condition/group (not only each group to baseline variables)?",
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


    output$new3a <- renderUI({
      if (!input$checklist3a == "Yes") return(NULL) else {
        textInput("response3a", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new3b <- renderUI({
      if (!input$checklist3b == "Yes") return(NULL) else {
        textInput("response3b", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new3c <- renderUI({
      if (!input$checklist3c == "Provide details") return(NULL) else {
        textInput("response3c", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new3d <- renderUI({
      if (!input$checklist3d == "Provide details") return(NULL) else {
        textInput("response3d", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new3e <- renderUI({
      if (!input$checklist3e == "Provide details") return(NULL) else {
        textInput("response3e", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })


    output$new4a <- renderUI({
      if (!input$checklist4a == "Provide details") return(NULL) else {
        textInput("response4a", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new4b <- renderUI({
      if (!input$checklist4b == "Provide details") return(NULL) else {
        textInput("response4b", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new4c <- renderUI({
      if (!input$checklist4c == "Provide details") return(NULL) else {
        textInput("response4c", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new4d <- renderUI({
      if (!input$checklist4d == "Provide details") return(NULL) else {
        textInput("response4d", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new4e <- renderUI({
      if (!input$checklist4e == "Provide details") return(NULL) else {
        textInput("response4e", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })


    output$new5a <- renderUI({
      if (!input$checklist5a == "Provide details") return(NULL) else {
        textInput("response5a", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
      }
    })
    output$new5b <- renderUI({
      if (!input$checklist5b == "Yes") return(NULL) else {
        textInput("response5b", label=NULL, placeholder="Report page/figure number(s) in which plots are shown")
      }
    })
    output$new5c <- renderUI({
      if (!input$checklist5c == "Yes") return(NULL) else {
        textInput("response5c", label=NULL, placeholder="Copy text from manuscript that meets checklist criteria")
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


    output$text3a <- renderText({
      if (input$checklist3a == "Yes") {
        return(input$response3a)
      }
    })
    output$text3b <- renderText({
      if (input$checklist3b == "Yes") {
        return(input$response3b)
      }
    })
    output$text3c <- renderText({
      if (input$checklist3c == "Provide details") {
        return(input$response3c)
      }
    })
    output$text3d <- renderText({
      if (input$checklist3d == "Provide details") {
        return(input$response3d)
      }
    })
    output$text3e <- renderText({
      if (input$checklist3e == "Provide details") {
        return(input$response3e)
      }
    })


    output$text4a <- renderText({
      if (input$checklist4a == "Provide details") {
        return(input$response4a)
      }
    })
    output$text4b <- renderText({
      if (input$checklist4b == "Provide details") {
        return(input$response4b)
      }
    })
    output$text4c <- renderText({
      if (input$checklist4c == "Provide details") {
        return(input$response4c)
      }
    })
    output$text4d <- renderText({
      if (input$checklist4d == "Provide details") {
        return(input$response4d)
      }
    })
    output$text4e <- renderText({
      if (input$checklist4e == "Provide details") {
        return(input$response4e)
      }
    })


    output$text5a <- renderText({
      if (input$checklist5a == "Provide details") {
        return(input$response4a)
      }
    })
    output$text5b <- renderText({
      if (input$checklist5b == "Yes") {
        return(input$response5b)
      }
    })
    output$text5c <- renderText({
      if (input$checklist5c == "Yes") {
        return(input$response5c)
      }
    })




    ############# Report summary #############

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


    output$summary3a <- renderText({
      if (input$checklist3a=="Yes") {
        return(input$response3a)
      } else if (input$checklist3a=="No") {
        return("Data was not collected on psychosocial factors")
      } else if (input$checklist3a=="Not applicable") {
        return("Data on psychosocial factors was not applicable for this study")
      }
    })

    output$summary3b <- renderText({
      if (input$checklist3b=="Yes") {
        return(input$response3b)
      } else if (input$checklist3b=="No") {
        return("Participants were not provided with a strategy")
      } else if (input$checklist3b=="Not applicable") {
        return("Providing a strategy to participants was not applicable for this study")
      }
    })

    output$summary3c <- renderText({
      if (input$checklist3c=="Provide details") {
        return(input$response3c)
      } else if (input$checklist3c=="Not applicable") {
        return("Participant strategies were not applicable for this study")
      }
    })

    output$summary3d <- renderText({
      if (input$checklist3d=="Provide details") {
        return(input$response3d)
      } else if (input$checklist3d=="Not applicable") {
        return("Online data-processing and artifact collection was not applicable for this study")
      }
    })

    output$summary3e <- renderText({
      if (input$checklist3e=="Provide details") {
        return(input$response3e)
      } else if (input$checklist3e=="Not applicable") {
        return("Condition and group effects for artifacts were not applicable for this study")
      }
    })


    output$summary4a <- renderText({
      if (input$checklist4a=="Provide details") {
        return(input$response4a)
      } else if (input$checklist4a=="Not applicable") {
        return("Online-feature extraction was not applicable for this study")
      }
    })

    output$summary4b <- renderText({
      if (input$checklist4b=="Provide details") {
        return(input$response4b)
      } else if (input$checklist4b=="Not applicable") {
        return("A reinforcement schedule was not applicable for this study")
      }
    })

    output$summary4c <- renderText({
      if (input$checklist4c=="Provide details") {
        return(input$response4c)
      } else if (input$checklist4c=="Not applicable") {
        return("Feedback modality and content was not applicable for this study")
      }
    })

    output$summary4d <- renderText({
      if (input$checklist4d=="Provide details") {
        return(input$response4d)
      } else if (input$checklist4d=="Not applicable") {
        return("Brain activity variable(s) and/or contrasts used for feedback were not applicable for this study")
      }
    })

    output$summary4e <- renderText({
      if (input$checklist4e=="Provide details") {
        return(input$response4e)
      } else if (input$checklist4e=="Not applicable") {
        return("No hardware of software were used in this study")
      }
    })


    output$summary5a <- renderText({
      if (input$checklist5a=="Provide details") {
        return(input$response5a)
      } else if (input$checklist5a=="Not applicable") {
        return("Neurofeedback regulation success based on the feedback signal was not applicable for this study")
      }
    })
    output$summary5b <- renderText({
      if (input$checklist5b=="Yes") {
        return(input$response5b)
      } else if (input$checklist5b=="No") {
        return("Within-session and between-session regulation blocks of feedback variable(s) were not plotted in this study")
      } else if (input$checklist5b=="Not applicable") {
        return("Plots of within-session and between-session regulation blocks of feedback variable(s) were not applicable for this study")
      }
    })
    output$summary5c <- renderText({
      if (input$checklist5c=="Yes") {
        return(input$response5c)
      } else if (input$checklist5c=="No") {
        return("The experimental condition/group and the control condition(s)/group(s) were not statistically compared")
      } else if (input$checklist5c=="Not applicable") {
        return("Statistical comparison of the experimental condition/group and the control condition(s)/group(s) was not applicable for this study")
      }
    })


    # Add option to export to PDF and/or Docx

    output$reportpdf <- downloadHandler(
      filename = "checklist.pdf",
      content = function(file) {
        # Copy report file to temp directory before processing it to avoid permission issues
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list()
        params$domain1 <- c(output$summary1a, output$summary1b)
        params$domain2 <- c(output$summary2a, output$summary2b, output$summary2c, output$summary2d, output$summary2e)
        params$domain3 <- c(output$summary3a, output$summary3b, output$summary3c, output$summary3d, output$summary3e)
        params$domain4 <- c(output$summary4a, output$summary4b, output$summary4c, output$summary4d, output$summary4e)
        params$domain5 <- c(output$summary5a, output$summary5b, output$summary5c)

        # Knit the document using params
        rmarkdown::render(tempReport, output_file = file,
                          params=params,
                          envir=new.env(parent = globalenv()) # Eval in child of global env to isolate rmd code from app code
                          )
      }
    )


    # Add warning (and specify number) of any responses marked as "NO" at start of summary before output

  }

  shinyApp(ui, server)
  }
