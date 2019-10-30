library(shiny)

if (interactive()) {
  
  ncheck <- 24 # Number of checklist items
  checkIDs <- c("1a", "1b",
                "2a", "2b", "2c.rater", "2c.stat", "2d", "2e",
                "3a", "3b", "3c", "3d", "3e",
                "4a", "4b", "4c", "4d", "4e",
                "5a", "5b", "5c",
                "6a", "6b",
                "7a"
  )
  
  # Vectors of IDs
  inputIDs <- paste0("checklist", checkIDs)  # Generate inputIDs
  newIDs <- paste0("new", checkIDs)  # Generate newIDs
  textIDs <- paste0("text", checkIDs)  # Generate textIDs
  summaryIDs <- paste0("summary", checkIDs)  # Generate summaryIDs
  sumIDs <- paste0("sum", checkIDs)
  responseIDs <- paste0("response", checkIDs)
  
  # An index of choices and vector of pre-existing choices
  choicecode <- c(1,1,
                  1,2,2,2,2,2,
                  1,1,1,1,1,
                  1,1,1,1,1,
                  1,1,2,
                  3,2,
                  1
  )
  choicelist <- list(c("No", "Yes"),
                     c("No", "Yes", "Not applicable"),
                     c("No", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori", "Not applicable")
  )
  # choicelist <- list(list("No"=1, "Yes"=2),
  #                    list("No"=1, "Yes"=2, "Not applicable"=3),
  #                    list("No"=1, "Yes, and the measure was defined a priori"=2, "Yes, and the measure was not defined a priori"=3, "Not applicable"=4)
  # )
  
  # Text vectors
  labels <- c("Was the protocol or analysis preregistered?",
              "Does the manuscript describe the sampling plan and/or justify the sample size used?",
              "Did the experiment include at least one control group or control condition?",
              "Was a double-blind appropriate in the present experiment?",
              "Were those who rate the outcomes blinded to group assignment?",
              "Were those who analysed the data blinded to group assignment?",
              "Were any measures taken to examine to what extend participants and experimenters remained blind?",
              "Is this a clinical efficacy study?",
              "Were data collected on psychosocial factors?",
              "Does the manuscript report whether participants were provided with a strategy?",
              "Are the strategies that participants reported using included in the manuscript?",
              "Does the manuscript report the methods used for online-data processing and artifact correction?",
              "Does the manuscript report condition and/or group effects for artifacts?",
              "Does the manuscript report how the online-feature extraction was defined?",
              "Does the manuscript report and justify the reinforcement schedule?",
              "Does the manuscript report the feedback modality and content?",
              "Does the manuscript report all brain activity variable(s) and/or contrasts used for feedback, as displayed to experimental participants?",
              "Does the manuscript report the hardware and software used?",
              "Does the manuscript report neurofeedback regulation success based on the feedback signal?",
              "Does the manuscript plot within-session and between-session regulation blocks of feedback variable(s), as well as pre-to-post resting baselines or contrasts?",
              "Does the manuscript statistically compare the experimental condition/group to the control condition(s)/group(s) (not only each group to baseline measures)?",
              "Does the manuscript report measures of clinical or behavioural significance and describe whether they were reached?",
              "Does the manuscript compare regulation success and behavioural outcomes?",
              "Does the manuscript include a link to any open access materials, analysis scripts, code, raw data, or final values?"
  )
  placeholders <- list("Copy the text from your manuscript that identifies the preregistration and includes a link to it",
                       "Copy the text from your manuscript that describes the sampling plan and justifies the sample size",
                       "Copy the text from your manuscript that describes the control group(s) and/or condition(s)",
                       "Copy the text from your manuscript that describes the double-blind and how it was implemented",
                       "Copy the text from your manuscript that describes the blind and how it was implemented",
                       "Copy the text from your manuscript that describes the blind and how it was implemented",
                       "Copy the text from your manuscript that describes the measures taken to examine whether participants and experimenters remained blind",
                       "Copy the text from your manuscript that describes the standard-of-care intervention group",
                       "Copy the text from your manuscript that describes the data collected on psychosocial factors",
                       "Copy the text from your manuscript that identifies whether one or more strategy was provided. If one or more strategy was provided, include the text describing the strategy or strategies.",
                       "Copy the text from your manuscript that describes the strategies participants used (note, these are not necessarily the same as the strategies provided)",
                       "Copy the text from your manuscript that describes the methods used for online-data processing and artifact correction",
                       "Copy the text from your manuscript that describes condition and/or group level artifacts",
                       "Copy the text from your manuscript that describes how the online-feature extraction was defined",
                       "Copy the text from your manuscript that describes and justifies the reinforcement schedule",
                       "Copy the text from your manuscript that describes the feedback modality and content",
                       "Copy the text from your manuscript that reports the brain activity variable(s) and/or contrasts used for feedback, as displayed to experimental participants",
                       "Copy the text from your manuscript that describes the hardware and software used",
                       "Copy the text from your manuscript that describes neurofeedback regulation success based on the feedback signal",
                       "Copy the text from your manuscript and/or insert the figure number(s) that plot within-session and between-session regulation blocks of feedback variable(s), as well as pre-to-post resting baselines or contrasts",
                       "Copy the text from your manuscript that describes the statistical comparison of the experimental condition/group to the control condition(s)/group(s)",
                       "Copy the text from your manuscript that reports measures of clinical or behavioural significance and describes whether they were reached. Ensure this text includes the URL where the measure of clinical or behavioural significance was preregistered or enter the source where this clinical or behavioural significance value has been previously established",
                       "Copy the text from your manuscript and/or the figure numbers that compare regulation success and behavioural outcomes",
                       "Copy the text from your manuscript detailing which items are available. Ensure this text includes a link to access the documents"
  )
  noboilers <- c("This experiment was not preregistered",
                 "The manuscript does not describe the sampling plan or justify the sample size used",
                 "This experiment did not include a control group or control condition",
                 "The experiment did not include a double-blind",
                 "Those who rated the outcome were not blind to group assignment",
                 "Those who analysed the data were not blind to group assignment",
                 "No measures were taken to examine whether participants and experimenters remained blind",
                 "The present study is a clinical efficacy study. There was no standard-of-care intervention group",
                 "Psychosocial factors were not measured",
                 "The manuscript does not report whether strategies were provided",
                 "The strategies participants used were not recorded or not reported in the manuscript",
                 "The manuscript does not report the methods used for online-data processing and artifact correction",
                 "Condition and group effects for artifacts were not measured, or not reported in the manuscript",
                 "The manuscript does not report how the online-feature extraction was defined",
                 "The manuscript does not report or justify the reinforcement schedule",
                 "The manuscript does not report the feedback modality and content",
                 "All brain activity variable(s) and/or contrasts used for feedback, as displayed to experimental participants were not collected or are not reported in the manuscript",
                 "The manuscript does not report the hardware and software used",
                 "The manuscript does not report neurofeedback regulation success based on the feedback signal",
                 "The manuscript does not plot within-session and between-session regulation blocks of feedback variable(s), as well as pre-to-post resting baselines or contrasts",
                 "The manuscript does not statistically compare the experimental condition/group to the control condition(s)/group(s)",
                 "The manuscript does not include measures of clinical or behavioural significance",
                 "This manuscript does not compare regulation success and behavioural outcomes",
                 "No additional documents related to the materials, analysis scripts, code, raw data, or final values are available for this manuscript"
  )
  naboilers <- c(NA, NA, 
                 NA, "NA: A double-blind was not appropriate for this experiment",
                 "NA: There was only one participant group",
                 "NA: There was only one participant group",
                 "NA: There was only one participant group",
                 "NA: This is not a clinical efficacy study",
                 NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA,
                 NA, NA, "NA: There was only one participant group",
                 "NA:  the study does not take cognitive or behavioural measures",
                 "NA:  the study does not take cognitive or behavioural measures",
                 NA
  )
  strblank <- "This field has been left blank"
  
  
  # Run checks on vector lengths
  if (length(checkIDs)!=ncheck) {
    stop("checkIDs not equal to length of ncheck")
  }
  if (length(labels)!=ncheck) {
    stop("labels not equal to length of ncheck")
  }
  if (length(choicecode)!=ncheck) {
    stop("choicecode not equal to length of ncheck")
  }
  if (length(placeholders)!=ncheck) {
    stop("placeholders not equal to length of ncheck")
  }
  if (length(responseIDs)!=ncheck) {
    stop("responseIDs not equal to length of ncheck")
  }
  if (length(noboilers)!=ncheck) {
    stop("noboilers not equal to length of ncheck")
  }
  if (length(naboilers)!=ncheck) {
    stop("naboilers not equal to length of ncheck")
  }
  if (length(summaryIDs)!=ncheck) {
    stop("summaryIDs not equal to length of ncheck")
  }
  if (length(sumIDs)!=ncheck) {
    stop("sumIDs not equal to length of ncheck")
  }
  
  
  
  ########################## START UI #############################
  
  ui <- fluidPage(
    titlePanel("CRED-nf Checklist"),

    navlistPanel(
      "Domains",
      tabPanel("Pre-experiment",
               h3("Pre-Experiment"),
               lapply(1:2, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               
               # wellPanel(
               #   selectInput("checklist1a", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new1a"),
               #   textOutput("text1a")
               # ),
               # wellPanel(
               #   selectInput("checklist1b", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new1b"),
               #   textOutput("text1b")
               # )
               ),

      tabPanel("Control groups",
               h3("Control groups"),
               lapply(3:8, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               
               # wellPanel(
               #   selectInput("checklist2a", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new2a"),
               #   textOutput("text2a")
               # ),
               # wellPanel(
               #   selectInput("checklist2b", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new2b"),
               #   textOutput("text2b")
               # ),
               # wellPanel(
               #   selectInput("checklist2c", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new2c"),
               #   textOutput("text2c")
               # ),
               # wellPanel(
               #   selectInput("checklist2d", h4(),
               #               choices = list("Not at all" = 1, "Provide details" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new2d"),
               #   textOutput("text2d")
               # ),
               # wellPanel(
               #   selectInput("checklist2e", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new2e"),
               #   textOutput("text2e")
               # )
               ),

      tabPanel("Control measures",
               h3("Control measures"),
               lapply(9:13, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               
               # wellPanel(
               #   selectInput("checklist3a", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new3a"),
               #   textOutput("text3a")
               # ),
               # wellPanel(
               #   selectInput("checklist3b", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new3b"),
               #   textOutput("text3b")
               # ),
               # wellPanel(
               #   selectInput("checklist3c", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new3c"),
               #   textOutput("text3c")
               # ),
               # wellPanel(
               #   selectInput("checklist3d", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new3d"),
               #   textOutput("text3d")
               # ),
               # wellPanel(
               #   selectInput("checklist3e", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new3e"),
               #   textOutput("text3e")
               # )
      ),

      tabPanel("Feedback specifications",
               h3("Feedback specifications"),
               lapply(14:18, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               
               # wellPanel(
               #   selectInput("checklist4a", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new4a"),
               #   textOutput("text4a")
               # ),
               # wellPanel(
               #   selectInput("checklist4b", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new4b"),
               #   textOutput("text4b")
               # ),
               # wellPanel(
               #   selectInput("checklist4c", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new4c"),
               #   textOutput("text4c")
               # ),
               # wellPanel(
               #   selectInput("checklist4d", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new4d"),
               #   textOutput("text4d")
               # ),
               # wellPanel(
               #   selectInput("checklist4e", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new4e"),
               #   textOutput("text4e")
               # )
               ),

      tabPanel("Outcome measures - Brain",
               h3("Outcome measures - Brain"),
               lapply(19:21, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               
               # wellPanel(
               #   selectInput("checklist5a", h4(),
               #               choices = list("Not applicable" = 1, "Provide details" = 2), selected = NULL),
               #   uiOutput("new5a"),
               #   textOutput("text5a")
               # ),
               # wellPanel(
               #   selectInput("checklist5b", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new5b"),
               #   textOutput("text5b")
               # ),
               # wellPanel(
               #   selectInput("checklist5c", h4(),
               #               choices = list("No" = 1, "Yes" = 2, "Not applicable"=3), selected = NULL),
               #   uiOutput("new5c"),
               #   textOutput("text5c")
               # )
               ),
      tabPanel("Outcome measures - Behaviour",
               h3("Outcome measures - Behaviour"),
               lapply(22:23, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               ),
      tabPanel("Data storage",
               h3("Outcome measures - Brain"),
               lapply(24, function(i) {
                 wellPanel(
                   selectInput(inputIDs[i], h4(),
                               choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
                   uiOutput(newIDs[i]),
                   textOutput(textIDs[i])
                 )
               })
               ),
      tabPanel("Checklist summary",
               h3("CRED-nf checklist summary output"),
               tags$div(
                 tags$ol(
                   tags$li("Pre-experiment"),
                   tags$ol(
                     lapply(1:2, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary1a")),
                     # tags$li(textOutput("summary1b")),
                     type="a"
                   ),

                   tags$li("Control groups"),
                   tags$ol(
                     lapply(3:4, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     tags$li("Blinding of those who rate the outcome and those who analyse the data:",
                       tags$ul(
                         tags$li(textOutput(summaryIDs[5])),
                         tags$li(textOutput(summaryIDs[6]))
                       )
                     ),
                     lapply(7:8, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary2a")),
                     # tags$li(textOutput("summary2b")),
                     # tags$li(textOutput("summary2c")),
                     # tags$li(textOutput("summary2d")),
                     # tags$li(textOutput("summary2e")),
                     type="a"
                   ),

                   tags$li("Control measures"),
                   tags$ol(
                     lapply(9:13, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary3a")),
                     # tags$li(textOutput("summary3b")),
                     # tags$li(textOutput("summary3c")),
                     # tags$li(textOutput("summary3d")),
                     # tags$li(textOutput("summary3e")),
                     type="a"
                   ),

                   tags$li("Feedback specifications"),
                   tags$ol(
                     lapply(14:18, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary4a")),
                     # tags$li(textOutput("summary4b")),
                     # tags$li(textOutput("summary4c")),
                     # tags$li(textOutput("summary4d")),
                     # tags$li(textOutput("summary4e")),
                     type="a"
                   ),

                   tags$li("Outcome measures - Brain"),
                   tags$ol(
                     lapply(19:21, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary5a")),
                     # tags$li(textOutput("summary5b")),
                     # tags$li(textOutput("summary5c")),
                     type="a"
                   ),
                   
                   tags$li("Outcome measures - Behaviour"),
                   tags$ol(
                     lapply(22:23, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary6a")),
                     # tags$li(textOutput("summary6b")),
                     type="a"
                   ),
                   
                   tags$li("Data storage"),
                   tags$ol(
                     lapply(24, function(i) {
                       tags$li(textOutput(summaryIDs[i]))
                     }),
                     
                     # tags$li(textOutput("summary7a")),
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




  ########################## START SERVER #############################

  server <- function(input, output, session) {
    
    ############ Observe inputs to react to selections made by user ############
    
    observe({
      for (i in 1:ncheck) {
        updateSelectInput(session, inputId = inputIDs[i], label = labels[i],
                          choices = choicelist[[choicecode[i]]])
      }
    })
    
    
    ############ Add open end box to enter text if they user has selected "yes############
    
    output[[newIDs[1]]] <- renderUI({
      if (!input[[inputIDs[1]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
        return(NULL) 
      } else {
        textInput(responseIDs[1], label=NULL, placeholder=placeholders[[1]])
      }
    })
    output[[newIDs[2]]] <- renderUI({
      if (!input[[inputIDs[2]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
        return(NULL) 
      } else {
        textInput(responseIDs[2], label=NULL, placeholder=placeholders[[2]])
      }
    })
    
    lapply(3:ncheck, function(i) {
      output[[newIDs[i]]] <- renderUI({
        if (!input[[inputIDs[i]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
          return(NULL)
        } else {
          textInput(responseIDs[i], label=NULL, placeholder=placeholders[[i]])
        }
      })
    })



    ########### Return text ############
    
    # for (i in 1:ncheck) {
    #   output[[paste0("text", checkIDs[i])]] <- renderText({
    #     if (input[[inputIDs[i]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
    #       return(input[[responseIDs[i]]])
    #     }
    #   })
    # }
    
    lapply(1:ncheck, function(i) {
      output[[paste0("text", checkIDs[i])]] <- renderText({
        if (input[[inputIDs[i]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
          return(input[[responseIDs[i]]])
        }
      })
    })


    ############# Report summary #############
    
    # assign(sumIDs[1],
    #        reactive({
    #          if (input[[inputIDs[1]]] == "Yes") {
    #            return(input[[responseIDs[1]]])
    #          } else if (input[[inputIDs[1]]] == "No") {
    #            return(noboilers[1])
    #          } else if (input[[inputIDs[1]]] == "Not applicable") {
    #            return(naboilers[1])
    #          } else if (input[[inputIDs[1]]] == "Yes, and the measure was defined a priori") {
    #            return(input[[responseIDs[1]]])
    #          } else if (input[[inputIDs[1]]] == "Yes, and the measure was not defined a priori") {
    #            temp <- input[[responseIDs[1]]]
    #            
    #            # Remove leading/trailing whitespace and add period if not at end
    #            temp <- trimws(temp)
    #            if (!grepl(".+\\.$", temp)) {
    #              temp <- paste0(temp, ".")
    #            }
    #            return(paste(temp, "This clinical or behavioural significance value was not defined a priori", sep=" "))
    #          }
    #        })
    # )
    # 
    # output[[summaryIDs[1]]] <- renderText({eval(parse(text=paste0(sumIDs[1], "()")))})
    
    # for (i in 2:ncheck) {
    #   assign(sumIDs[i],
    #          reactive({
    #            if (input[[inputIDs[i]]] == "Yes") {
    #              return(input[[responseIDs[i]]])
    #            } else if (input[[inputIDs[i]]] == "No") {
    #              return(noboilers[i])
    #            } else if (input[[inputIDs[i]]] == "Not applicable") {
    #              return(naboilers[i])
    #            } else if (input[[inputIDs[i]]] == "Yes, and the measure was defined a priori") {
    #              return(input[[responseIDs[i]]])
    #            } else if (input[[inputIDs[i]]] == "Yes, and the measure was not defined a priori") {
    #              temp <- input[[responseIDs[i]]]
    #              
    #              # Remove leading/trailing whitespace and add period if not at end
    #              temp <- trimws(temp)
    #              if (!grepl(".+\\.$", temp)) {
    #                temp <- paste0(temp, ".")
    #              }
    #              return(paste(temp, "This clinical or behavioural significance value was not defined a priori", sep=" "))
    #            }
    #          })
    #          )
    #   
    #   output[[summaryIDs[i]]] <- renderText({eval(parse(text=paste0(sumIDs[i], "()")))})
    # }
    
    lapply(1:ncheck, function(i) {
      assign(sumIDs[i],
             reactive({
               
               if (input[[inputIDs[i]]] == "Yes") {
                 if (input[[responseIDs[i]]]=="") {
                   return(strblank)
                 } else {
                   return(input[[responseIDs[i]]])
                 }
               } else if (input[[inputIDs[i]]] == "No") {
                 return(noboilers[i])
               } else if (input[[inputIDs[i]]] == "Not applicable") {
                 return(naboilers[i])
               } else if (input[[inputIDs[i]]] == "Yes, and the measure was defined a priori") {
                 if (input[[responseIDs[i]]]=="") {
                   return(strblank)
                 } else {
                   return(input[[responseIDs[i]]])
                 }
               } else if (input[[inputIDs[i]]] == "Yes, and the measure was not defined a priori") {
                 temp <- input[[responseIDs[i]]]
                 
                 if (temp=="") {
                   temp <- strblank
                 }
                 
                 # Remove leading/trailing whitespace and add period if not at end
                 temp <- trimws(temp)
                 if (!grepl(".+\\.$", temp)) {
                   temp <- paste0(temp, ".")
                 }
                 return(paste(temp, "This clinical or behavioural significance value was not defined a priori.", sep=" "))
               }
             })
      )
      
      output[[summaryIDs[i]]] <- renderText({eval(parse(text=paste0(sumIDs[i], "()")))})
    })
    
    # sum1a <- reactive({
    #   #ifelse(input$checklist1a=="Yes", input$response1a, "NOOOOOOO")
    #   if (input$checklist1a=="Yes") {
    #       return(input$response1a)
    #     } else if (input$checklist1a=="No") {
    #       return("Protocol and planned analyses were not pre-registered")
    #     } else if (input$checklist1a=="Not applicable") {
    #       return("Protocol and planned analyses were not applicable")
    #     }
    # })
    # sum1b <- reactive({
    #   #ifelse(input$checklist1b=="Yes", input$response1b, "NOOOOOOO")
    #   if (input$checklist1b=="Yes") {
    #       return(input$response1b)
    #     } else if (input$checklist1b=="No") {
    #       return("No justification was given for the sample size")
    #     } else if (input$checklist1b=="Not applicable") {
    #       return("A sample size was not applicable for this study")
    #     }
    # })
    # 
    # output$summary1a <- renderText({sum1a()})
    # output$summary1b <- renderText({sum1b()})
    # 
    # 
    # # output$summary1a <- renderText({
    # #   if (input$checklist1a=="Yes") {
    # #     return(input$response1a)
    # #   } else if (input$checklist1a=="No") {
    # #     return("Protocol and planned analyses were not pre-registered")
    # #   } else if (input$checklist1a=="Not applicable") {
    # #     return("Protocol and planned analyses were not applicable")
    # #   }
    # # })
    # 
    # # output$summary1b <- renderText({
    # #   if (input$checklist1b=="Yes") {
    # #     return(input$response1b)
    # #   } else if (input$checklist1b=="No") {
    # #     return("No justification was given for the sample size")
    # #   } else if (input$checklist1b=="Not applicable") {
    # #     return("A sample size was not applicable for this study")
    # #   }
    # # })
    # 
    # 
    # output$summary2a <- renderText({
    #   if (input$checklist2a=="Yes") {
    #     return(input$response2a)
    #   } else if (input$checklist2a=="No") {
    #     return("No control group(s)/condition(s) were employed in this study")
    #   } else if (input$checklist2a=="Not applicable") {
    #     return("Control group(s)/condition(s) were not applicable for this study")
    #   }
    # })
    # 
    # output$summary2b <- renderText({
    #   if (input$checklist2b=="Yes") {
    #     return(input$response2b)
    #   } else if (input$checklist2b=="No") {
    #     return("This study did not use a double-blind design")
    #   } else if (input$checklist2b=="Not applicable") {
    #     return("A double-blind design was not applicable for this study")
    #   }
    # })
    # 
    # output$summary2c <- renderText({
    #   if (input$checklist2c=="Yes") {
    #     return(input$response2c)
    #   } else if (input$checklist2c=="No") {
    #     return("Those who rated the outcomes or those who performed the analysis were not blinded")
    #   } else if (input$checklist2c=="Not applicable") {
    #     return("Blinding of those who rated the outcomes or those who performed the analysis was not applicable for this study")
    #   }
    # })
    # 
    # output$summary2d <- renderText({
    #   if (input$checklist2d=="Provide details") {
    #     return(input$response2d)
    #   } else if (input$checklist2d=="Not at all") {
    #     return("Participants and experimenters did not remain blinded")
    #   } else if (input$checklist2d=="Not applicable") {
    #     return("Blinding of participants and experimenters was not applicable for this study")
    #   }
    # })
    # 
    # output$summary2e <- renderText({
    #   if (input$checklist2e=="Yes") {
    #     return(input$response2e)
    #   } else if (input$checklist2e=="No") {
    #     return("A standard-of-care intervention group was not used as a benchmark for improvement")
    #   } else if (input$checklist2e=="Not applicable") {
    #     return("A standard-of-care intervention group was not applicable for this study, or this was not a clinical efficacy study")
    #   }
    # })
    # 
    # 
    # output$summary3a <- renderText({
    #   if (input$checklist3a=="Yes") {
    #     return(input$response3a)
    #   } else if (input$checklist3a=="No") {
    #     return("Data was not collected on psychosocial factors")
    #   } else if (input$checklist3a=="Not applicable") {
    #     return("Data on psychosocial factors was not applicable for this study")
    #   }
    # })
    # 
    # output$summary3b <- renderText({
    #   if (input$checklist3b=="Yes") {
    #     return(input$response3b)
    #   } else if (input$checklist3b=="No") {
    #     return("Participants were not provided with a strategy")
    #   } else if (input$checklist3b=="Not applicable") {
    #     return("Providing a strategy to participants was not applicable for this study")
    #   }
    # })
    # 
    # output$summary3c <- renderText({
    #   if (input$checklist3c=="Provide details") {
    #     return(input$response3c)
    #   } else if (input$checklist3c=="Not applicable") {
    #     return("Participant strategies were not applicable for this study")
    #   }
    # })
    # 
    # output$summary3d <- renderText({
    #   if (input$checklist3d=="Provide details") {
    #     return(input$response3d)
    #   } else if (input$checklist3d=="Not applicable") {
    #     return("Online data-processing and artifact collection was not applicable for this study")
    #   }
    # })
    # 
    # output$summary3e <- renderText({
    #   if (input$checklist3e=="Provide details") {
    #     return(input$response3e)
    #   } else if (input$checklist3e=="Not applicable") {
    #     return("Condition and group effects for artifacts were not applicable for this study")
    #   }
    # })
    # 
    # 
    # output$summary4a <- renderText({
    #   if (input$checklist4a=="Provide details") {
    #     return(input$response4a)
    #   } else if (input$checklist4a=="Not applicable") {
    #     return("Online-feature extraction was not applicable for this study")
    #   }
    # })
    # 
    # output$summary4b <- renderText({
    #   if (input$checklist4b=="Provide details") {
    #     return(input$response4b)
    #   } else if (input$checklist4b=="Not applicable") {
    #     return("A reinforcement schedule was not applicable for this study")
    #   }
    # })
    # 
    # output$summary4c <- renderText({
    #   if (input$checklist4c=="Provide details") {
    #     return(input$response4c)
    #   } else if (input$checklist4c=="Not applicable") {
    #     return("Feedback modality and content was not applicable for this study")
    #   }
    # })
    # 
    # output$summary4d <- renderText({
    #   if (input$checklist4d=="Provide details") {
    #     return(input$response4d)
    #   } else if (input$checklist4d=="Not applicable") {
    #     return("Brain activity variable(s) and/or contrasts used for feedback were not applicable for this study")
    #   }
    # })
    # 
    # output$summary4e <- renderText({
    #   if (input$checklist4e=="Provide details") {
    #     return(input$response4e)
    #   } else if (input$checklist4e=="Not applicable") {
    #     return("No hardware or software were used in this study")
    #   }
    # })
    # 
    # 
    # output$summary5a <- renderText({
    #   if (input$checklist5a=="Provide details") {
    #     return(input$response5a)
    #   } else if (input$checklist5a=="Not applicable") {
    #     return("Neurofeedback regulation success based on the feedback signal was not applicable for this study")
    #   }
    # })
    # output$summary5b <- renderText({
    #   if (input$checklist5b=="Yes") {
    #     return(input$response5b)
    #   } else if (input$checklist5b=="No") {
    #     return("Within-session and between-session regulation blocks of feedback variable(s) were not plotted in this study")
    #   } else if (input$checklist5b=="Not applicable") {
    #     return("Plots of within-session and between-session regulation blocks of feedback variable(s) were not applicable for this study")
    #   }
    # })
    # output$summary5c <- renderText({
    #   if (input$checklist5c=="Yes") {
    #     return(input$response5c)
    #   } else if (input$checklist5c=="No") {
    #     return("The experimental condition/group and the control condition(s)/group(s) were not statistically compared")
    #   } else if (input$checklist5c=="Not applicable") {
    #     return("Statistical comparison of the experimental condition/group and the control condition(s)/group(s) was not applicable for this study")
    #   }
    # })


    ########## Add option to export to PDF and/or Docx ##########

    output$reportpdf <- downloadHandler(
      filename = "checklist.pdf",
      content = function(file) {
        # Copy report file to temp directory before processing it to avoid permission issues
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list("domain1"=c(sum1a(), sum1b()))

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
