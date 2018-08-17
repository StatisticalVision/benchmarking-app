library(data.table)
library(readxl)
library(curl)
library(shiny)
library(DT)
library(dplyr)


## All tables are from here. Files were downloaded as txt files from this link, written to csv locally,
## then uploaded to the r server because the initial txt files were large and slow.
## There's probably a way to unzip and fread at once somewhere.
# https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
# https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data/42008111
## https://census.gov/data/datasets/2016/econ/cbp/2016-cbp.html

# https://www2.census.gov/programs-surveys/cbp/datasets/2016/zbp16detail.zip
zip <- fread("curl https://www2.census.gov/programs-surveys/cbp/datasets/2016/zbp16detail.zip | funzip",
             colClasses = c(zip = "character", naics = "character"))
setnames(zip, "naics", "NAICS")
# head(zip)

# https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16co.zip
county <- fread("curl https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16co.zip | funzip",
             colClasses = c(fipstate = "character", fipscty = "character", cencty = "character", naics = "character"))
setnames(county, "naics", "NAICS")
# head(county)

# https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16msa.zip
# metro <- fread("curl https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16msa.zip | funzip",
#                 colClasses = c(msa = "character", naics = "character"))
# head(metro)


closeAllConnections()
## This is the backup data
## https://www.census.gov/data/tables/2015/econ/susb/2015-susb-annual.html

# # Congressional district = cd
# cd <- data.table(read_excel("~/HoldingApp/cd_naicssector_2015.xlsx", skip = 4))
# cd <- cd[-c(1,2) ]
# 
# # Aggregate table by fips code and cd code
# cd[, StateCd := paste(`STATE DESCRIPTION`, `CD Code`)]
# setnames(cd, c("FIPS   STATE CODE", "NAICS CODE"), c("State Fips", "NAICS Code"))
# cd <- cd[, list(Name = StateCd[1],
#                             Description = `NAICS   DESCRIPTION`[1], NumberOfFirms = sum(`NUMBER OF FIRMS`),
#                             NumberOfEstablishments = sum(`NUMBER OF ESTABLISHMENTS`), Employment = sum(EMPLOYMENT), 
#                             AnnualPayroll = sum(`ANNUAL PAYROLL ($1,000)`)), by = c("State Fips", "CD Code",
#                                                                                     "NAICS Code")]
# 
# # County totals = t
# ct <- data.table(read_excel("~/HoldingApp/county_totals_2015.xlsx", skip = 5))
# ct <- ct[-c(1,2) ]
# 
# # Aggregate table by fips code and cd code
# setnames(ct, c("FIPS STATE   CODE", "FIPS COUNTY   CODE", "NAICS CODE"), c("State Fips", "County Fips", "NAICS Code"))
# ct <- ct[, list(Name = StateCd[1],
#                 Description = `NAICS   DESCRIPTION`[1], NumberOfFirms = sum(`NUMBER OF FIRMS`),
#                 NumberOfEstablishments = sum(`NUMBER OF ESTABLISHMENTS`), Employment = sum(EMPLOYMENT), 
#                 AnnualPayroll = sum(`ANNUAL PAYROLL ($1,000)`)), by = c("State Fips", "County Fips", "NAICS Code")]
# 
# 
# 
# 
# Metro and micro areas
# https://www.census.gov/data/tables/2015/econ/susb/2015-susb-annual.html
# https://www2.census.gov/programs-surveys/susb/tables/2015/msa_naicssector_2015.xlsx
msaNAICS <- data.table(
  read_excel("~/zters-app/Data/msa_naicssector_2015.xlsx", skip = 4))
msaNAICS <- msaNAICS[-c(1,2) ]

# Aggregate table by fips code
setnames(msaNAICS, c("FIPS    CODE", "NAICS   CODE"), c("Fips", "NAICS"))
msaNAICS <- msaNAICS[, list(Name = `MSA DESCRIPTION`[1],
           Description = `NAICS DESCRIPTION`[1], NumberOfFirms = sum(`NUMBER OF FIRMS`),
           NumberOfEstablishments = sum(`NUMBER OF ESTABLISHMENTS`), Employment = sum(EMPLOYMENT),
           AnnualPayroll = sum(`ANNUAL PAYROLL ($1,000)`)), by = c("Fips", "NAICS")]
# Sepearate macro and micro into two tables
metro <- msaNAICS[grepl("Metro", Name)]
micro <- msaNAICS[grepl("Micro", Name)]
rm(msaNAICS)
#nrow(msaNAICS[grepl("Metro", Name)]) + nrow(msaNAICS[grepl("Micro", Name)]) == nrow(msaNAICS)
# 
# 

# Set NAICS for zip and county to the same format as metro and micro
unique(metro$NAICS)
ziph <- copy(zip)
# Total for each zip
ziph[NAICS == "------", NAICS := "Total"]
ziph[, NAICS := gsub("--", "", NAICS)]
head(ziph)
# Sum to match metro areas.

# Example used to help with shinay app
# https://deanattali.com/blog/building-shiny-apps-tutorial/
# http://rstudio.github.io/shiny/tutorial/#shiny-text

ui <- fluidPage(
  # Title
  titlePanel("NAICS Establishments by Geography"),
  
  # Sidebar panel
  sidebarPanel(
    # Select data set
    selectInput("dtinput", "Select data:", choices = c("micro", "metro")),
    # Select naics input
    # http://shiny.rstudio.com/gallery/checkbox-group.html
    # https://gist.github.com/wch/4211337
    checkboxGroupInput("industryinput", "Select NAICS", choices = unique(metro$`NAICS`))
    # Select num of obs?
    # numericInput("obs", "Number of observations:", 10)
  ),
  
  # Output dataset?
  mainPanel(DT::dataTableOutput("view"))
)

server <- function(input, output) {
  # Get selected data
  dtChoiceInput <- reactive({
    # hold table selection
    # https://stackoverflow.com/questions/36585956/r-shiny-multiple-inputs-in-reactive
    switch(input$dtinput, "micro" = micro, "metro" = metro)
  })
  # Show n observations?
  # https://stackoverflow.com/questions/39367369/how-to-select-certain-rows-in-a-reactive-dataset-in-r-shiny
  output$view <- DT::renderDataTable({
    #dtChoiceInput()
    dtChoiceInput() %>% filter(NAICS %in% input$industryinput)
  })
}


shinyApp(ui = ui, server = server)
