---
bibliography: NF-BCI-instructions-checklist.bib
---

# NF-BCI-instructions-checklist online checklist tool


The NF-BCI-instructions-checklist checklist [@Ros2020] was developed to promote consistency and to standardise the design and reporting of neurofeedback studies. There is currently considerable heterogeneity in the reporting of these studies, which make it challenging to reliably assess the efficacy of neurofeedback techniques. By providing a set of standardised items the NF-BCI-instructions-checklist checklist provides a framework to address these issues.

This NF-BCI-instructions-checklist online tool is an R Shiny app that accompanies the development of the checklist. It makes it easy for users to complete the checklist via a web browser, providing prompting questions for each of the items and highlighting those where users have left responses blank. It can also be used to generate a PDF report of responses which can be included alongside a manuscript submission to journals so that reviewers can have access to the completed checklist.


## Usage

The checklist includes 24 items grouped into 7 different domains:

1. Pre-experiment
2. Control groups
3. Control measures
4. Feedback specifications
5. Outcome measures - brain
6. Outcome measures - behaviour
7. Data storage

After inputting details regarding the manuscript in question in `Manucript Information` (used for generating the PDF report), users then navigate through each domain using the tabs on the left-hand panel. Within each domain, they answer questions regarding the design/reporting of their neurofeedback experiment. Answering `Yes` to many of the quetsions will prompt a more detailed response in which users are asked to copy relevant sections of their manuscript that justify their answer to the question.

The final tab contains a `Checklist Summary` which is populated with boiler-plate responses depending on the user's answers to the preceeding questions. At the top of this page, a warning in red is given for items to which the user responsed `Yes` but did not provide the required additional information. At the bottom of this page, the `Download summary` button can be used to generate and download a PDF of responses to the different items, which will also include details that have been written in the `Manuscript Information` tab.


## Installation

The majority of users can run the app in a web broswer via the shiny.io server it is hosted on (https://NF-BCI-instructions-checklist.shinyapps.io/NF-BCI-instructions-checklist/). This is the easiest way to use the software, as all dependencies are installed on the server, and generated PDF reports can easily be downloaded from here without the need to separately install LaTeX and associated packages.

The app can also be run locally in R, for which two files `app.R` and `report.Rmd` are required. The following dependencies are also required:

R (>= 3.0.0)
shiny (>= 1.3.2)
rmarkdown (>= 1.16)

A TeX distribution (operating system-dependent) with the following packages:
fancyhdr (>= 3.10)
longtable (>= 4.12)
array (>= 2.41)


## Testing

Automatic testing can be performed using the `shinytest` R package. Two test files exist - one for testing checklist domains 1-3 and one for testing checklist domains 4-7. Both sets of tests can be performed simultaneously by running the `RunTests.R` script.

One component of the app that cannot be tested automatically is the generation of the PDF report. Testing for this must be performed manually. To do so, run the app and navigate to the `Checklist Summary` tab. Clicking the `Download Summary` button should generate a PDF saved in a temporary location that will open automatically. Responses in the PDF should match those entered into the app and shown in the `Checklist Summary` tab.


## References
