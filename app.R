# introduction
# start with upload files seperately
# advanced: error, dose not recieve selinexor, the exposure plot. 
# downloading reports


# Load packages and data ----
library(shiny)
library(tidyverse)
library(clipr)
library(haven)
library(magrittr)
library(lubridate)
library(Hmisc)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(haven)
library(DT)
library(writexl)
#setwd("~/Work/Project/Patients Profile/Code")


# df_suppfa <- read_sas(paste0(path,'/SDTM/suppfa.sas7bdat'))
# df_fa <- read_sas(paste0(path,'/SDTM/fa.sas7bdat'))
source("Shiny-PP-DataCleaning-Dec052022.R")
source("Shiny-PP-plotting-Dec052022.R")

# safety_pp <- compute_safety_pp(df_dm, df_ex)
# ex_dfs <- process_ex(df_ex)
# ex <- ex_dfs[[1]]
# ex_rgm <- ex_dfs[[2]]
# 
# ae_dfs <- process_ae(df_ae, safety_pp)
# ae <- ae_dfs[[1]]
# ae_oneday <- ae_dfs[[2]]
# 
# df_ptinfo <- compute_ptinfo(df_ds, df_dm, df_fa, df_suppfa, safety_pp)

# need to upload ex,ae,ds, dm, fa, fasuppa, 
#user interface ------------------------------------
ui <- fluidPage(
  titlePanel('Patient Profile'),
  shinyjs::useShinyjs(), 
  fluidRow(
    column(4,
           selectInput(
             'pt_id', 
             "Select a PatientID to examine.",
             choices = NULL
           ),
           dateInput('cut_off_d', "Please select the data cut-off date"),
           shinyFeedback::useShinyFeedback(),

           fileInput(
             'df_ae', 
             label = "Please upload SDTM AE domain"
           ),
           fileInput(
             'df_dm', 
             label = "Please upload SDTM DM domain"
           ),
           fileInput(
             'df_ds', 
             label = "Please upload SDTM DS domain"
           ),
           fileInput(
             'df_dd', 
             label = "Please upload STDM DD domain"
           ),
           
           fileInput(
             'df_ex', 
             label = "Please upload SDTM EX domain"
           ),
           fileInput(
             'df_lb', 
             label = "Please upload STDM LB domain"
           ),

           actionButton('reset', "Reset All"),
           
           
           tableOutput("upload_files"),
           
           conditionalPanel(
             condition = "input.tabselected == 'fig'",
             downloadButton('download_figure', 'Download Figures .pdf')
           ),
           conditionalPanel(
             condition = 'input.tabselected == "ae"',
             downloadButton('download_ae', 'Download Listing .xlsx')
           ),
           conditionalPanel(
             condition = 'input.tabselected == "ex"',
             downloadButton('download_ex', 'Download Listing .xlsx')
           )
           
    ),
    column(
      8,
      tabsetPanel(
        id = 'tabselected',
        tabPanel(
          "Figures",
          value = 'fig',
          verbatimTextOutput('pt_info'),
          plotOutput("ex_p"),
          plotOutput("ae_p"),
          plotOutput("lb_p"),
          #! further make download button in siderbar, and change with selecting different tabpanel.
          #column(3, downloadButton('download_figure', 'Download Figures .pdf'), offset = 8)
          
        ),
        tabPanel(
          "AE Listing",
          value = 'ae',
          DT::dataTableOutput('ae_listing'), 
          #downloadButton('download_ae',  'Download Listing .xlsx')
          ),
        tabPanel(
          "EX Listing",
          value = 'ex',
          DT::dataTableOutput('ex_listing'),
          #downloadButton('download_ex','Download Listing .xlsx')
          )
      )
      )
  )
)
  






server <- function(input, output, session){
  options(shiny.maxRequestSize=30*1024^2)
  # read data---------------------------------- 
  data <- reactiveValues(
    df_ex = NULL,
    df_ae = NULL,
    df_dm = NULL,
    df_ds = NULL,
    df_dd = NULL,
    df_lb = NULL
  )
  
  df_ex <- reactive({
    req(input$df_ex)
    ext <- tools::file_ext(input$df_ex$name)
    valid_type <- ext %in% c('csv', 'sas7bdat')
    valid_domain <-tools::file_path_sans_ext(input$df_ex$name) == "ex"
    shinyFeedback::feedbackDanger("df_ex", !valid_type, "Invalid file; Please upload a .csv or .sas7bdat file")
    shinyFeedback::feedbackWarning("df_ex", !valid_domain, "Please upload correct domain")
    
    req(valid_type, valid_domain)
    switch(ext,
           csv = read_csv(input$df_ex$datapath),
           sas7bdat = haven::read_sas(input$df_ex$datapath)
    )
    
  })
  

  df_ae <- reactive({
    req(input$df_ae)
    ext <- tools::file_ext(input$df_ae$name)
    valid_type <- ext %in% c('csv', 'sas7bdat')
    valid_domain <-tools::file_path_sans_ext(input$df_ae$name) == "ae"

    shinyFeedback::feedbackDanger("df_ae", !valid_type, "Invalid file; Please upload a .csv or .sas7bdat file")
    shinyFeedback::feedbackWarning("df_ae", !valid_domain, "Please upload correct domain")
    req(valid_type,valid_domain)
    switch(ext,
           csv = read_csv(input$df_ae$datapath),
           sas7bdat = haven::read_sas(input$df_ae$datapath)
    )
  })
  #! may I turn this into function?
  
  df_dm <- reactive({
    req(input$df_dm)
    ext <- tools::file_ext(input$df_dm$name)
    valid_type <- ext %in% c('csv', 'sas7bdat')
    valid_domain <-tools::file_path_sans_ext(input$df_dm$name) == "dm"
    
    shinyFeedback::feedbackDanger("df_dm", !valid_type, "Invalid file; Please upload a .csv or .sas7bdat file")
    shinyFeedback::feedbackWarning("df_dm", !valid_domain, "Please upload correct domain")
    
    req(valid_type,valid_domain, cancelOutput = TRUE)
    switch(ext,
           csv = read_csv(input$df_dm$datapath),
           sas7bdat = haven::read_sas(input$df_dm$datapath)
    )
  })
  
  df_ds <- reactive({
    req(input$df_ds)
    ext <- tools::file_ext(input$df_ds$name)
    valid_type <- ext %in% c('csv', 'sas7bdat')
    valid_domain <-tools::file_path_sans_ext(input$df_ds$name) == "ds"

    shinyFeedback::feedbackDanger("df_ds", !valid_type, "Invalid file; Please upload a .csv or .sas7bdat file")
    shinyFeedback::feedbackWarning("df_ds", !valid_domain, "Please upload a correct domain")
    req(valid_type,valid_domain)
    switch(ext,
           csv = read_csv(input$df_ds$datapath),
           sas7bdat = haven::read_sas(input$df_ds$datapath)
    )
  })
  
  df_dd <- reactive({
    req(input$df_dd)
    ext <- tools::file_ext(input$df_dd$name)
    valid_type <- ext %in% c('csv', 'sas7bdat')
    valid_domain <-tools::file_path_sans_ext(input$df_dd$name) == "dd"

    shinyFeedback::feedbackDanger("df_dd", !valid_type, "Invalid file; Please upload a .csv or .sas7bdat file")
    shinyFeedback::feedbackWarning("df_dd", !valid_domain, "Please upload a correct domain")
    req(valid_type,valid_domain)
    switch(ext,
           csv = read_csv(input$df_dd$datapath),
           sas7bdat = haven::read_sas(input$df_dd$datapath)
    )
  })
  
  df_lb <- reactive({
    req(input$df_lb)
    ext <- tools::file_ext(input$df_lb$name)
    valid_type <- ext %in% c('csv', 'sas7bdat')
    valid_domain <-tools::file_path_sans_ext(input$df_lb$name) == "lb"

    shinyFeedback::feedbackDanger("df_lb", !valid_type, "Invalid file; Please upload a .csv or .sas7bdat file")
    shinyFeedback::feedbackWarning("df_lb", !valid_domain, "Please upload a correct domain")
    req(valid_type,valid_domain, cancelOutput = TRUE)
    switch(ext,
           csv = read_csv(input$df_lb$datapath),
           sas7bdat = haven::read_sas(input$df_lb$datapath)
    )
  })
  
# make the warining of wrong dataset type and wrong domain response immediately
  observeEvent(input$df_ex, {data$df_ex <- df_ex()})
  observeEvent(input$df_ae, {data$df_ae <- df_ae()})
  observeEvent(input$df_dm, {data$df_dm <- df_dm()})
  observeEvent(input$df_ds, {data$df_ds <- df_ds()})
  observeEvent(input$df_dd, {data$df_dd <- df_dd()})
  observeEvent(input$df_lb, {data$df_lb <- df_lb()})


  # data clean-----------------
  safety_pp <- reactive({
    req(input$df_dm, input$df_ex)
    compute_safety_pp(df_dm(), df_ex())
  })
  
  # ptinfo
  df_ptinfo <- reactive({
    compute_ptinfo(df_ds(), df_dm(), df_dd(), safety_pp())
  })
  
  # ex
  clean_ex <- reactive({
    req(input$df_ex)
    process_ex(df_ex())
  })
  ex <- reactive({clean_ex()[[1]]})
  ex_rgm <- reactive({clean_ex()[[2]]})
  
  clean_ae <- reactive({
    req(input$df_ae, input$df_dm, input$df_ex)
    process_ae(df_ae(), df_ptinfo(),input$cut_off_d)
  })
  ae <- reactive({clean_ae()[[1]]})
  ae_oneday <- reactive({clean_ae()[[2]]})
  
  lb <- reactive({process_lb(df_lb(), safety_pp())})
  # update pt ID in UI select Input
  observeEvent(safety_pp(),{
    choices <- unique(safety_pp()$USUBJID)
    freezeReactiveValue(input, 'pt_id')
    updateSelectInput(inputId = 'pt_id', choices = choices)
  })

  #output-------------------------
  # Print uploaded dataset
  output$upload_files <- renderTable(
    rbind(
      input$df_ex[c("name", "size", 'type')], 
      input$df_ae[c("name", "size", 'type')], 
      input$df_dm[c("name", "size", 'type')], 
      input$df_ds[c("name", "size", 'type')], 
      input$df_dd[c("name", "size", 'type')],
      input$df_lb[c("name", "size", 'type')]
    )
  )
  # output$preview <- renderTable({
  #   head(df_ptinfo(), 3)
  # })
  
  # Print out pt's information
  output$pt_info <-
    renderText({
      as.character(
        df_ptinfo()[df_ptinfo()$USUBJID == input$pt_id, 'title']
      )
    })
  
  #get the x-axis max
  lim_x <-  reactive({
    lim_ex = max(ex()[ex()$USUBJID == input$pt_id, "EXSTDY"], na.rm = TRUE)
    lim_ae = max(ae()[ae()$USUBJID == input$pt_id, "AEENDY2"], na.rm = TRUE)
    lim_lb = max(lb()[lb()$USUBJID == input$pt_id, "LBDY"], na.rm = TRUE)
    max(c(lim_ex, lim_ae, lim_lb), na.rm = TRUE)
  })
  

  # explosure figure
  output$ex_p <- renderPlot({
    req(input$df_ex, input$df_dm)
    if(!input$pt_id %in% unique(ex()$USUBJID)){
      #! Can change the apperance of validate message?
      validate('The patient didn\'t take any Selinexor.' )
    }
    else{
      make_exposure_p(ex(), ex_rgm(), input$pt_id) + xlim(0, lim_x())
    }
  })

  output$ae_p <- renderPlot({
    req(input$df_ex, input$df_ae)
    make_AE_p(ae(), ae_oneday(), input$pt_id) + xlim(0, lim_x())
  })
  
  output$lb_p <- renderPlot({
    req(input$df_lb, input$df_ex, input$df_dm)
    make_lb_p(lb(), input$pt_id) + xlim(0, lim_x())
  })
  
  #output Listing---------------------------
  output$ae_listing <-DT::renderDataTable(
    # req(input$df_ex, input$df_dm, input$df_ae)
    make_ae_listing(ae(),input$pt_id),
    options = list(scrollX = TRUE)
  )
  
  output$ex_listing <- DT::renderDataTable(
    make_ex_listing(df_ex(), df_dm(), input$pt_id),
    options = list(scrollX = TRUE)
  )
  
  #download -----------------------------
  #! can change latter, switch bettwn pdf and html
  output$download_figure <- downloadHandler(
    filename = 'report.pdf',
    content = function(file) {
      res <- rmarkdown::render(
        "report.rmd",
        params = list(
          pt_id = input$pt_id
        )
      )
      file.rename(res, file)
    }
      )
  #download listing
  output$download_ae <- downloadHandler(
    filename = 'ae_listing.xlsx',
    content = function(file){
      write_xlsx(make_ae_listing(ae(), df_dm(),input$pt_id), file)
    }
  )
  output$download_ae <- downloadHandler(
    filename = 'ex_listing.xlsx',
    content = function(file){
      write_xlsx(make_ex_listing(df_ex(), df_dm(), input$pt_id), file)
    }
  )
  
  #reset all uploaded files
  observeEvent(input$reset, {
    shinyjs::reset()
    #reset('pt_info')
    # shinyjs::reset('ex_listing')
    # shinyjs::reset(output$ex_listing)
    # output$ex_p = NULL
    # output$lb_p = NULL
    # output$ae_p = NULL
    # output$ae_listing = NULL
    output$ex_listing =  DT::renderDataTable({})
    # output$upload_files = NULL
  })
  

  
}



# Run the app
shinyApp(ui, server)

