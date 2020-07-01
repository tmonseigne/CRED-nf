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
newIDs <- paste0("new", checkIDs)			# Generate newIDs
textIDs <- paste0("text", checkIDs)			# Generate textIDs			
summaryIDs <- paste0("summary", checkIDs)	# Generate summaryIDs		Id of each text output for checklist summary 
responseIDs <- paste0("response", checkIDs)	# Generate responseIDs		Id of each text area
tickIDs <- paste0("tick", checkIDs)			# Generate tickIDs			Id of each Checkgroupbox
sumResponseIDs <- paste0("sumR", checkIDs)	# Generate sumResponseIDs	Response on each ID in string for PDF
sumTickIDs <- paste0("sumT", checkIDs)		# Generate sumTickIDs		All ticks selected on each ID in string for PDF


# Lisdt of possible choice in formular
ticklist <- c("Avoiding EEG artefacts",
			  "Environment and feedback",
			  "Mental strategies",
			  "User motivation",
			  "Transfer tasks",
			  "Trainers' behaviour",
			  "Ethics",
			  "Other"
)

# Lisdt of possible choice in formular
choicelist <- list(c("No", "Yes"))

# An index of choices and vector of pre-existing choices see choiceList
choicecode <- c(1, 1, 1, 1, 1, 1, 1)

# Domains
domains <- c("Recruitment phase",
			 "Beginning of the first NF/BCI training session",
			 "Beginning of each NF/BCI session",
			 "During NF/BCI sessions",
			 "End of the last NF/BCI session",
			 "Additional phase"
)

# Questions
labels <- c("Did you provide any instructions during the recruitment phase?",
			"Did you provide any instructions at the very beginning of the NF/BCI training procedure (first session)?",
			"Did you provide any instructions at the beginning of each session of the NF/BCI training procedure?",
			"Did you provide any instructions during the NF/BCI training sessions?",
			"Did you provide any instructions at the end of the NF/BCI training procedure (end of the last session)?",
			"Did your experiment include an additional phase (e.g., transfer phase, at-home training, without NF) during which different instructions were given?",
			"If Yes, Did you provide any instructions before or during this additional phase?"
)

# Default message (in grey) in the text area when ye is selected
placeholders <- list("Please indicate what kind of instructions you provided during the recruitment phase.",
					 "Please indicate what kind of instructions you provided at the very beginning of the NF/BCI training procedure.",
					 "Please indicate what kind of instructions you provided at the beginning of each session.",
					 "Please indicate what kind of instructions you provided during the sessions.",
					 "Please indicate what kind of instructions you provided at the end of the training procedure.",
					 "",
					 "Please indicate what kind of instructions you provided before or during this additional phase."
)

checkItem <- c("Instructions during the recruitment phase.",
			   "Instructions at the very beginning of the NF/BCI training procedure (first session).",
			   "Instructions at the beginning of each session of the NF/BCI training procedure.",
			   "Instructions during the NF/BCI training sessions.",
			   "Instructions at the end of the NF/BCI training procedure (end of the last session).",
			   "Additional phase (e.g., transfer phase, at-home training, without NF) with different instructions.",
			   "Instructions before or during this additional phase.")

# Default Message If choice is'nt no but don't need test to justifiy.
naboilers <- c("", "", "", "", "", "Different instructions given during additional phase.", "")

# Default Message If  choice is no but the classical no str can't be used
noboilers <- c("", "", "", "", "", "No Additionnal phases.", "No Additionnal phases.")

# Message if a field need to be fill but nothing is define
strblank <- "This field has been left blank"

# Message if No is selected
strNo <- "No instructions provided"

# Message if No type is selected
strNoType <- "No instruction type selected"

# Run checks on vector lengths
if (length(checkIDs) != ncheck) { 		stop("checkIDs not equal to length of ncheck") }
if (length(labels) != ncheck) {			stop("labels not equal to length of ncheck") }
if (length(choicecode) != ncheck) {		stop("choicecode not equal to length of ncheck") }
if (length(placeholders) != ncheck) {	stop("placeholders not equal to length of ncheck") }
if (length(responseIDs) != ncheck) {	stop("responseIDs not equal to length of ncheck") }
if (length(noboilers) != ncheck) {		stop("noboilers not equal to length of ncheck") }
if (length(naboilers) != ncheck) {		stop("naboilers not equal to length of ncheck") }
if (length(summaryIDs) != ncheck) {		stop("summaryIDs not equal to length of ncheck") }
if (length(sumResponseIDs) != ncheck) {	stop("sumResponseIDs not equal to length of ncheck") }

simpleItem <- function(i) { wellPanel(selectInput(inputIDs[i], h4(), choices = as.list(choicelist[[choicecode[i]]]), selected = NULL ), uiOutput(newIDs[i]), textOutput(textIDs[i])) }

checkboxes <- function(i) { checkboxGroupInput(tickIDs[i], "What was(were) the type(s) of this(these) instruction(s)? (check all that apply)", choices = ticklist ) }

########################## START UI #############################
ui <- fluidPage(
	titlePanel("RETINA"),
	
	navlistPanel("Domains",
				 
				 # About Tab
				 tabPanel("About",
				 		 tags$div(HTML("<h1><u>RE</u>por<u>T</u>ing <u>I</u>nstructions for <u>N</u>eurofeedb<u>A</u>ck</h1>")),
				 		 tags$div(p("Instructions and guidance can have a substantial impact on learning, 
				 		 		   but little is known regarding the current practice and the reasoning behind specific instructions in the context of neurofeedback (NF) and brain-computer interfaces (BCI). 
				 		 		   We thus proposed a framework intended to enable both literature-based decision making for an optimal learning environment in NF/BCI and systematic research on the influence of instructions. 
				 		 		   The current online app is a digital version of the framework, to be filled-in by NF/BCI scientists in order to support the scientific community and to encourage rigorous design and reporting of instructions. ", style = "font-size:15px")),
				 		 br(),
				 		 p("When completed, click the ‘Download summary’ button from the ‘Checklist summary’ tab. This will produce a table which you can include in your manuscript submission as supplementary material.", style = "font-size:15px"),
				 		 br(),
				 		 tags$div(p("For full details about this checklist, the criteria regarding each item, and the motivation for its development, please see", 
				 		 		   a(href = "https://URLOFPAPER", "the associated manuscript here."), style = "font-size:15px")),
				 		 br(),
				 		 tags$div(p("This tool is currently a Beta version, and has been created by Thibaut Monseigne with the work of Hugo Pedder and Robert Thibault of the University of Bristol (",
				 		 		   a(href = "https://psyarxiv.com/nyx84/", "manuscript here"),"). The content is taken from the published version of the RETINA. 
				 		 		   If you encounter any bugs when using it or have any feedback, please email thibaut.monseigne@inria.fr with the subject `RETINA Shiny App` or raise an issue on", a(href = "https://github.com/tmonseigne/RETINA", "GitHub"), style = "font-size:15px"))
				 ),
				 
				 # Manuscript information Tab
				 tabPanel(
				 	"Manuscript information",
				 	textInput("title", label = "Manuscript title", width = "80%"),
				 	textInput("author", label = "Corresponding author name", width = "80%"),
				 	textInput("email", label = "Corresponding author email", width = "80%")
				 ),
				 
				 # Recruitment phase Tab
				 tabPanel(paste("1.", domains[1]), h2(domains[1]), simpleItem(1)),
				 
				 # Beginning of the first NF/BCI training session Tab
				 tabPanel(paste("2.", domains[2]), h2(domains[2]), simpleItem(2)),
				 
				 # Beginning of each NF/BCI session Tab
				 tabPanel(paste("3.", domains[3]), h2(domains[3]), simpleItem(3)),
				 
				 # During NF/BCI sessions Tab
				 tabPanel(paste("4.", domains[4]), h2(domains[4]), simpleItem(4)),
				 
				 # End of the last NF/BCI session Tab
				 tabPanel(paste("5.", domains[5]), h2(domains[5]), simpleItem(5)),
				 
				 # Additional phase Tab
				 tabPanel(paste("6.", domains[6]), h2(domains[6]), simpleItem(6), simpleItem(7)),
				 
				 # Checklist Tab
				 tabPanel("Checklist summary", 
				 		 tags$span(style = "color:red", strong(em( textOutput("warningtext")))), 
				 		 tags$span(style = "color:red", strong(em( textOutput("warningChoice")))),
				 		 h2("RETINA summary output"),
				 		 tags$div(
				 		 	tags$ol(
				 		 		tags$li(domains[1]), tags$ol(lapply(1, function(i) { tags$li(textOutput(summaryIDs[i]))}), type = "a"),
				 		 		tags$li(domains[2]), tags$ol(lapply(2, function(i) { tags$li(textOutput(summaryIDs[i]))}), type = "a"),
				 		 		tags$li(domains[3]), tags$ol(lapply(3, function(i) { tags$li(textOutput(summaryIDs[i]))}), type = "a"),
				 		 		tags$li(domains[4]), tags$ol(lapply(4, function(i) { tags$li(textOutput(summaryIDs[i]))}), type = "a"),
				 		 		tags$li(domains[5]), tags$ol(lapply(5, function(i) { tags$li(textOutput(summaryIDs[i]))}), type = "a"),
				 		 		tags$li(domains[6]), tags$ol(lapply(6:7, function(i) { tags$li(textOutput(summaryIDs[i]))}), type = "a")
				 		 	)
				 		 ),
				 		 
				 		 br(), br(),
				 		 downloadButton("reportpdf", "Download summary")
				 ),
				 widths = c(3, 9)
	)
	
)


########################## START SERVER #############################
server <- function(input, output, session) {
	############ Observe inputs to react to selections made by user ############
	observe({ for (i in 1:ncheck) { updateSelectInput(session, inputId = inputIDs[i], label = labels[i], choices = choicelist[[choicecode[i]]]) } })
	
	############ Add text boxes for manuscript info, etc #########
	assign("title", renderText({ input$title }))
	assign("author", renderText({ input$author }))
	assign("email", renderText({ input$email }))
	
	
	############ Add element if "Yes" is selected ############
	lapply(1:ncheck, function(i) {
		output[[newIDs[i]]] <- renderUI({
			if (input[[inputIDs[i]]] == "No") { return(NULL) }
			else if (i != 6) { list(textAreaInput(responseIDs[i], label = NULL, placeholder = placeholders[[i]]), checkboxes(i)) }
		})
	})
	
	########### Return text ############
	lapply(1:ncheck, function(i) { output[[paste0("text", checkIDs[i])]] <- renderText({ if (input[[inputIDs[i]]] == "Yes") { return(input[[responseIDs[i]]]) } }) })
	
	############# Generate report summary #############
	
	getResponse <- function(i) {
		if (i == 6 && input[[inputIDs[i]]] == "Yes") {	return(naboilers[i]) }	# If Yes at Item 6
		if (i == 6 && input[[inputIDs[i]]] == "No") {	return(noboilers[i]) }	# If No at Item 6
		if (i == 7 && input[[inputIDs[6]]] == "No") {	return(noboilers[i]) }	# If No at Item 6 and we are at item 7
		if (input[[inputIDs[i]]] == "Yes") { if (input[[responseIDs[i]]] == "") { return(strblank) } else { return(input[[responseIDs[i]]]) } }	# Response
		return(strNo)
	}
	
	getTicks <- function(i) {
		if ((i == 6) || (i == 7 && input[[inputIDs[6]]] == "No")) { return("") }
		if(input[[inputIDs[i]]] == "Yes") { 
			res <- toString(input[[tickIDs[i]]])
			if(res == "") {return(strNoType)}
			return (res)
		}
		return("")
	}
	
	getSum <- function(i) {
		r <- reactive({ getResponse(i) })
		t <- reactive({ getTicks(i) })
		return(paste(t(), r(), sep="\n"))
	}
	
	params <- list()
	
	lapply(1:ncheck, function(i) { 
		
		assign(sumResponseIDs[i], reactive({ getResponse(i) }), envir = globalenv())
		assign(sumTickIDs[i], reactive({ getTicks(i) }), envir = globalenv())
		output[[summaryIDs[i]]] <- renderText({ eval(parse(text = paste0(sumResponseIDs[i], "()"))) })
		
	})
	
	
	########## Add warning text for items left blank ##########
	warningtext <- reactive({
		blankindex <- vector()
		for (i in 1:ncheck) { if (i != 6 && input[[inputIDs[i]]] == "Yes" && input[[responseIDs[i]]] == "") { blankindex <- append(blankindex, i) } }
		if (length(blankindex) > 0) { return(paste0("Warning: Checklist item(s) ", paste(checkIDs[blankindex], collapse = ", "), " have been left blank.")) }
		return(NULL)
	})
	output$warningtext <- renderText(warningtext())
	
	warningChoice <- reactive({
		blankindex <- vector()
		for (i in 1:ncheck) { if (i != 6 && input[[inputIDs[i]]] == "Yes" && toString(input[[tickIDs[i]]]) == "") { blankindex <- append(blankindex, i) } }
		if (length(blankindex) > 0) { return(paste0("Warning: Checklist item(s) ", paste(checkIDs[blankindex], collapse = ", "), " have no choice.")) }
		return(NULL)
	})
	output$warningChoice <- renderText(warningChoice())
	
	
	
	########## Add option to export to PDF and/or Docx ##########
	
	output$reportpdf <- downloadHandler(
		filename = "checklist.pdf",
		content = function(file) {
			# Copy report file to temp directory before processing it to avoid permission issues
			tempReport <- file.path(tempdir(), "report.Rmd")
			file.copy("report.Rmd", tempReport, overwrite = TRUE)
			
			# Set up parameters to pass to Rmd document
			params <- list("title" = title(), "author" = author(), "email" = email(),
						   "checkIDs" = checkIDs, "checkItem" = checkItem, "domains" = domains,
						   "response" = list(sumR1a(), sumR2a(), sumR3a(), sumR4a(), sumR5a(), sumR6a(), sumR6b()),
						   "ticks" = list(sumT1a(), sumT2a(), sumT3a(), sumT4a(), sumT5a(), sumT6a(), sumT6b()),
						   "boilers" = c(naboilers, noboilers, strblank, strNo, strNoType)
			)
			
			# Knit the document using params
			# Eval in child of global env to isolate Rmd code from app code
			rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv())
			)
		}
	)
}

shinyApp(ui, server)