#---
#output: html_document
#runtime: shiny
#---
library(shiny)

# Number of checklist items
ncheck <- 7

# List of item ID
checkIDs <- c("1a", "2a", "3a", "4a", "5a", "6a", "6b")

# Copy of checklist IDs vectors
inputIDs <- paste0("checklist", checkIDs)	# Generate inputIDs
newIDs <- paste0("new", checkIDs)					# Generate newIDs
textIDs <- paste0("text", checkIDs)				# Generate textIDs
summaryIDs <- paste0("summary", checkIDs)	# Generate summaryIDs
sumIDs <- paste0("sum", checkIDs)
responseIDs <- paste0("response", checkIDs)

# Lisdt of possible choice in formular
choicelist <- list(c("No", "Yes"), c("No", "Partially", "Yes"))

# An index of choices and vector of pre-existing choices see choiceList
choicecode <- c(1,1,1,1,1,1,1)

# Domains
domains <- c("Recruitment phase", 
						 "Beginning of the first NF/BCI training session",
						 "Beginning of each NF/BCI session", 
						 "During NF/BCI sessions",
						 "End of the last NF/BCI session",
						 "Additional phase")

# Questions
labels <- c("Did you provide any instructions during the recruitment phase?",
						"Did you provide any instructions at the very beginning of the NF/BCI training procedure (first session)?",
						"Did you provide any instructions at the beginning of each session of the NF/BCI training procedure?",
						"Did you provide any instructions during the NF/BCI training sessions?",
						"Did you provide any instructions at the end of the NF/BCI training procedure (end of the last session)?",
						"Did your experiment include an additional phase (e.g., transfer phase, at-home training, without NF) during which different instructions were given?",
						"Did you provide any instructions before or during this additional phase?")

# Default message (in grey) in the text area when ye is selected
placeholders <- list("Please indicate what kind of instructions you provided during the recruitment phase.",
										 "Please indicate what kind of instructions you provided at the very beginning of the NF/BCI training procedure.",
										 "Please indicate what kind of instructions you provided at the beginning of each session.",
										 "Please indicate what kind of instructions you provided during the sessions.",
										 "Please indicate what kind of instructions you provided at the end of the training procedure.",
										 NA,
										 "Please indicate what kind of instructions you provided before or during this additional phase."
)

# Default Message If No is selected
noboilers <- c(NA,NA,NA,NA,NA,NA,NA)

# Default Message If choice is'nt no but don't need test to justifiy. 
naboilers <- c(NA,NA,NA,NA,NA, "Different instructions given during additional phase.",NA)

# Message if a field need to be fill but nothing is define
strblank <- "This field has been left blank"

# Run checks on vector lengths
if (length(checkIDs)!=ncheck) { stop("checkIDs not equal to length of ncheck") }
if (length(labels)!=ncheck) { stop("labels not equal to length of ncheck") }
if (length(choicecode)!=ncheck) { stop("choicecode not equal to length of ncheck") }
if (length(placeholders)!=ncheck) { stop("placeholders not equal to length of ncheck") }
if (length(responseIDs)!=ncheck) { stop("responseIDs not equal to length of ncheck") }
if (length(noboilers)!=ncheck) { stop("noboilers not equal to length of ncheck") }
if (length(naboilers)!=ncheck) { stop("naboilers not equal to length of ncheck") }
if (length(summaryIDs)!=ncheck) { stop("summaryIDs not equal to length of ncheck") }
if (length(sumIDs)!=ncheck) { stop("sumIDs not equal to length of ncheck") }



########################## START UI #############################
ui <- fluidPage(
	titlePanel("NF/BCI Instructions Checklist"),
	
	navlistPanel("Domains",
		
		# About Tab
		tabPanel("About",
						 tags$div(HTML("<h1><u>N</u>euro<u>F</u>eedback/<u>B</u>rain <u>C</u>omputer <u>I</u>nterface <u>I</u>nstruction Checklist</h1>")),
						 tags$div(p("This webpage serves as an online tool to standardize reporting of the", a(href="https://psyarxiv.com/nyx84/", "CRED-nf checklist."), 
						 					 "Please select the tabs on the left and answer the questions provided. When you respond ‘Yes’ to an item, you will be prompted to copy-paste the text from your manuscript that addresses the item. We recommend you also save this copy-pasted text in a text document in case this webpage has a timeout issue.", style = "font-size:15px")),
						 br(),
						 p("When completed, click the ‘Download summary’ button from the ‘Checklist summary’ tab. This will produce a table which you can include in your manuscript submission as supplementary material.", style = "font-size:15px"),
						 br(),
						 tags$div(p("For full details about this checklist, the criteria regarding each item, and the motivation for its development, please see", 
						 					 a(href="https://psyarxiv.com/nyx84/", "the associated manuscript here."), style = "font-size:15px")),
						 br(),
						 tags$div(p("This tool is currently a Beta version, and has been created by Hugo Pedder and Robert Thibault of the University of Bristol. The content is taken from the published version of the CRED-nf checklist. If you encounter any bugs when using it or have any feedback, please email robert.thibault@bristol.ac.uk with the subject `CRED-nf Shiny App` or raise an issue on",
						 					 a(href="https://github.com/tmonseigne/CRED-nf", "GitHub"), style = "font-size:15px"))
		),
		
		# Manuscript information Tab
		tabPanel("Manuscript information",
						 textInput("title", label="Manuscript title", width="80%"),
						 textInput("author", label="Corresponding author name", width="80%"),
						 textInput("email", label="Corresponding author email", width="80%")),
		
		# Recruitment phase Tab
		tabPanel(paste("1.", domains[1]),
						 h2(domains[1]),
						 lapply(1, function(i) {
						 	wellPanel(
						 		selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
						 		uiOutput(newIDs[i]),
						 		textOutput(textIDs[i])
						 	)
						 })
		),
		
		# Beginning of the first NF/BCI training session Tab		
		tabPanel(paste("2.", domains[2]),
						 h2(domains[2]),
						 lapply(2, function(i) {
						 	wellPanel(
						 		selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
						 		uiOutput(newIDs[i]),
						 		textOutput(textIDs[i])
						 	)
						 })
		),
		
		# Beginning of each NF/BCI session Tab
		tabPanel(paste("3.", domains[3]),
						 h2(domains[3]),
						 lapply(3, function(i) {
						 	wellPanel(
						 		selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
						 		uiOutput(newIDs[i]),
						 		textOutput(textIDs[i])
						 	)
						 })
		),
		
		# During NF/BCI sessions Tab
		tabPanel(paste("4.", domains[4]),
						 h2(domains[4]),
						 lapply(4, function(i) {
						 	wellPanel(
						 		selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
						 		uiOutput(newIDs[i]),
						 		textOutput(textIDs[i])
						 	)
						 })
		),
		
		# End of the last NF/BCI session Tab
		tabPanel(paste("5.", domains[5]),
						 h2(domains[5]),
						 lapply(5, function(i) {
						 	wellPanel(
						 		selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
						 		uiOutput(newIDs[i]),
						 		textOutput(textIDs[i])
						 	)
						 })
		),
		
		# Additional phase Tab
		tabPanel(paste("6.", domains[6]),
						 h2(domains[6]),
						 lapply(6:7, function(i) {
						 	wellPanel(
						 		selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL),
						 		uiOutput(newIDs[i]),
						 		textOutput(textIDs[i])
						 	)
						 })
		),
		
		# Checklist Tab
		tabPanel("Checklist summary", 
						 tags$span(style="color:red", strong(em(textOutput("warningtext")))),
						 h2("CRED-nf checklist summary output"),
						 tags$div(
						 	tags$ol(
						 		tags$li(domains[1]),
						 		tags$ol(lapply(1, function(i) { tags$li(textOutput(summaryIDs[i])) }), type="a"),
						 		
						 		tags$li(domains[2]),
						 		tags$ol(lapply(2, function(i) { tags$li(textOutput(summaryIDs[i])) }), type="a"),
						 		
						 		tags$li(domains[3]),
						 		tags$ol(lapply(3, function(i) { tags$li(textOutput(summaryIDs[i])) }), type="a"),
						 		
						 		tags$li(domains[4]),
						 		tags$ol(lapply(4, function(i) { tags$li(textOutput(summaryIDs[i])) }), type="a"),
						 		
						 		tags$li(domains[5]),
						 		tags$ol(lapply(5, function(i) { tags$li(textOutput(summaryIDs[i])) }), type="a"),
						 		
						 		tags$li(domains[6]),
						 		tags$ol(lapply(6:7, function(i) { tags$li(textOutput(summaryIDs[i])) }), type="a")
						 	)
						 ),
						 
						 br(), br(),
						 downloadButton("reportpdf", "Download summary")
		),
		widths=c(3,9)
	)
	
)




########################## START SERVER #############################
server <- function(input, output, session) {
	
	############ Observe inputs to react to selections made by user ############
	
	observe({
		for (i in 1:ncheck) {
			updateSelectInput(session, inputId = inputIDs[i], label = labels[i], choices = choicelist[[choicecode[i]]])
		}
	})
	
	############ Add text boxes for manuscript info, etc #########
	assign("title", renderText({input$title}))
	assign("author", renderText({input$author}))
	assign("email", renderText({input$email}))
	
	
	############ Add open end box to enter text if they user has selected "yes" ############
	
	lapply(1:ncheck, function(i) {
		output[[newIDs[i]]] <- renderUI({
			if (!input[[inputIDs[i]]] %in% c("Yes", 
																			 "Yes, and the measure was defined a priori", 
																			 "Yes, and the measure was not defined a priori", 
																			 "Yes, and a double-blind was used", 
																			 "Yes, and a standard-of-care intervention group was used as a benchmark for improvement",
																			 "Partially")) {
				return(NULL)
			} else { textAreaInput(responseIDs[i], label=NULL, placeholder=placeholders[[i]]) }
		})
	})
	
	
	
	########### Return text ############
	
	lapply(1:ncheck, function(i) {
		output[[paste0("text", checkIDs[i])]] <- renderText({
			if (input[[inputIDs[i]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
				return(input[[responseIDs[i]]])
			}
		})
	})
	
	
	############# Generate report summary #############
	
	params <- list()
	lapply(1:ncheck, function(i) {
		assign(sumIDs[i],
					 reactive({
					 	
					 	if (input[[inputIDs[i]]] %in% c("Yes", 
					 																	"Yes, and the measure was defined a priori",
					 																	"Yes, and a double-blind was used",
					 																	"Yes, and a standard-of-care intervention group was used as a benchmark for improvement",
					 																	"Partially")) {
					 		if (input[[responseIDs[i]]]=="") { return(strblank) 
					 		} else { return(input[[responseIDs[i]]]) }
					 	} else if (input[[inputIDs[i]]] == "No") { return(noboilers[i])
					 	} else if (input[[inputIDs[i]]] %in% c("Not applicable",
					 																				 "Not applicable, the study does not take cognitive or behavioural measures",
					 																				 "Not applicable, there was only one participant group")) {
					 		return(naboilers[i])
					 	} else if (input[[inputIDs[i]]] == "Yes, and the measure was not defined a priori") {
					 		temp <- input[[responseIDs[i]]]
					 		
					 		if (temp=="") { temp <- strblank }
					 		
					 		# Remove leading/trailing whitespace and add period if not at end
					 		temp <- trimws(temp)
					 		if (!grepl(".+\\.$", temp)) { temp <- paste0(temp, ".") }
					 		return(paste(temp, "This clinical or behavioural significance value was not defined a priori.", sep=" "))
					 	} else if (input[[inputIDs[i]]] %in% c("Yes, but a double-blind was not used",
					 																				 "Yes, and a standard-of-care intervention group was not used as a benchmark for improvement")) {
					 		return(naboilers[i])
					 	}
					 }),
					 envir=globalenv()
		)
		
		output[[summaryIDs[i]]] <- renderText({eval(parse(text=paste0(sumIDs[i], "()")))})
		
	})
	
	
	########## Add warning text for items left blank ##########
	
	warningtext <- reactive({
		blankindex <- vector()
		
		for (i in 1:ncheck) {
			if (input[[inputIDs[i]]] %in% c("Yes", "Yes, and the measure was defined a priori", "Yes, and the measure was not defined a priori")) {
				if (input[[responseIDs[i]]]=="") { blankindex <- append(blankindex, i) }
			}
		}
		
		if (length(blankindex)>0) {
			return(paste0("Warning: Checklist item(s) ", paste(checkIDs[blankindex], collapse=", "), " have been left blank."))
		} else {return(NULL)}
	})
	output$warningtext <- renderText(warningtext())
	
	
	########## Add option to export to PDF and/or Docx ##########
	
	output$reportpdf <- downloadHandler(
		filename = "checklist.pdf",
		content = function(file) {
			# Copy report file to temp directory before processing it to avoid permission issues
			tempReport <- file.path(tempdir(), "report.Rmd")
			file.copy("report.Rmd", tempReport, overwrite = TRUE)
			
			# Set up parameters to pass to Rmd document
			params <- list("title"=title(), "author"=author(), "email"=email(),
										 "domain1"=c(sum1a()),
										 "domain2"=c(sum2a()),
										 "domain3"=c(sum3a()),
										 "domain4"=c(sum4a()),
										 "domain5"=c(sum5a()),
										 "domain6"=c(sum6a(), sum6b()),
										 "boilers"=c(noboilers, naboilers, strblank)
			)
			
			# Knit the document using params
			# Eval in child of global env to isolate Rmd code from app code
			rmarkdown::render(tempReport, output_file = file, params=params, envir=new.env(parent = globalenv()))
		}
	)
	
}

shinyApp(ui, server)