library(shiny)

if (interactive()) {

  ui <- fixedPage(
    # titlePanel("CRED-nf Checklist"),
    # sidebarLayout(position = "left",
    #               sidebarPanel("Domain"),
    #               mainPanel(
    #                 h1("First level title")
    #               )),

    h3("Pre-Experiment"),
    wellPanel(
      selectInput("checklist1a", h4(),
                  choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = 1),
      uiOutput("new1a"),
      textOutput("text1a")
    ),
    wellPanel(
      selectInput("checklist1b", h4(),
                  choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = 1),
      uiOutput("new1b"),
      textOutput("text1b")
    )

    # fixedRow(
    #   column(3,
    #          h3("Pre-Experiment"),
    #          selectInput("checklist1a", h4(),
    #                      choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = 1),
    #          selectInput("checklist1b", h4(),
    #                      choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = 1)
    #          ),
    #
    #   column(3,
    #          br(),
    #          uiOutput("new1a"),
    #          uiOutput("new1b")
    #   ),
    #
    #   column(3,
    #          br(),
    #          textOutput("text1a"),
    #          textOutput("text1b")
    #   )
    # )
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
  }

  shinyApp(ui, server)
  }
