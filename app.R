#setwd("~/Dropbox/Santa Clara Shiny")
source("functions.R")
library(shinythemes)
library(shinyjs)
library(shinymanager)
library(DT)

####################


server <- function(input, output, session) {
  
  # restore button
  observeEvent(input$restore_all | input$restore_all2, {
    vars = c("N", "S", "E", "I_m", "R_mu", "R_md",
             "I_s", "H_su", "H_sd", "R_su", "R_sd",
             "I_c", "H_cu", "H_cd", "C_cu", "C_cd", "R_cu",
             "R_cd", "contact_mod_1", "contact_mod_2", "t",
             "lambda", "pmbase", "pmint1", "pmint2", "v", "f_c",
             "f_s", "gamma", "p_s", "p_c", "delta_s", "delta_c", "epsilon_c", "alpha", "per2", "per3")
    for(i in 1:length(vars)) reset(vars[i])
    })
  
  # local save
  observeEvent(input$save | input$save2, {
    
      # Define inputs to save
      inputs_to_save <- c("contact_mod_1", "contact_mod_2", "t",
                          "lambda", "pmbase", "pmint1", "pmint2", "v", "f_c", "alpha",
                          "f_s", "gamma", "p_s", "p_c", "delta_s", "delta_c",
                          "epsilon_c", "per2", "per3",
                          "N", "E", "I_m", "R_mu", "R_md", 
                          "I_s", "H_su", "H_sd", "R_su", "R_sd",
                          "I_c", "H_cu", "H_cd", "C_cu", "C_cd", "R_cu",
                          "R_cd")
      if(!is.null(input[["N"]])){
      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for(input.i in inputs_to_save){
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      # Save Inputs
      write.csv(inputs_data_frame, file = paste0(tempdir(), "/temp.csv"), row.names = FALSE)
      }
      
  })
  
  # calibration data 
  data = reactive({
    inFile = input$file1
    if (is.null(inFile)) {
      return(read.csv("calib.csv"))
    } else{ return(read.csv(inFile$datapath) %>% rename(time = 1, cum_cases = 2, hosp_ongoing = 3, hosp_new = 4))}
     })
  
  # input data
  data_input = reactive({
    inFile = input$file2
    if (is.null(inFile)) {
      return(read.csv("inputs.csv"))
    } else{ return(read.csv(inFile$datapath))}
  })
  
  # restore saved
  observeEvent(input$restore_saved | input$restore_saved2, {
    
    if(file.exists(paste0(tempdir(), "/temp.csv"))){
      # Load inputs
      uploaded_inputs <- read.csv(paste0(tempdir(), "/temp.csv"))
      # Update each input
      for(i in 1:nrow(uploaded_inputs)){
        if(i < 18) { 
          updateSliderInput(session,
                            inputId = uploaded_inputs$inputId[i],
                            value = uploaded_inputs$value[i])
        }else{
          updateNumericInput(session,  inputId = uploaded_inputs$inputId[i],
                            value = uploaded_inputs$value[i])
        }
      }
    }
    
  })
  
  # restore saved
  observeEvent(input$file2, {

    inFile = input$file2
    if(!is.null(inFile)){
        # Load inputs
        uploaded_inputs <- read.csv(inFile$datapath)
      # Update each input
      for(i in 1:nrow(uploaded_inputs)){
        if(i < 20) { 
          updateSliderInput(session,
                            inputId = uploaded_inputs$inputId[i],
                            value = uploaded_inputs$value[i])
        }else{
          updateNumericInput(session,  inputId = uploaded_inputs$inputId[i],
                             value = uploaded_inputs$value[i])
        }
      }
    }
    
  })
  
  
  # run data
  out <- reactive({
    # put slider control values here as arguments
    process_params(N = input$N, E = input$E, I_m = input$I_m, R_mu = input$R_mu, R_md = input$R_md, 
                   I_s = input$I_s, H_su = input$H_su, H_sd = input$H_sd, R_su = input$R_su, R_sd = input$R_sd,
                   I_c = input$I_c, H_cu = input$H_cu, H_cd = input$H_cd, C_cu = input$C_cu, C_cd = input$C_cd, R_cu = input$R_cu,
                   R_cd =input$R_cd, per2 = input$per2, per3 = input$per3,
                   contact_mod_1 = input$contact_mod_1, contact_mod_2 = input$contact_mod_2, 
                              t = input$t, alpha = input$alpha,
                              lambda = input$lambda,
                              pmbase = input$pmbase, pmint1 = input$pmint1, pmint2 = input$pmint2, 
                              v = 1/input$v, f_c = input$f_c, f_s = input$f_s, gamma = 1/input$gamma,
                              p_s = input$p_s, p_c = input$p_c, delta_s = 1/input$delta_s, delta_c = 1/input$delta_c,
                              epsilon_c = 1/input$epsilon_c)
  })
  
  # main plots
  plots = reactive({make_plots(out())})
  output$Plot <- renderPlot({
    plot_grid(plots()[[1]], plots()[[2]], nrow=2, ncol=1, align="hv")
  })
  
  output$Plot_No_NPI <- renderPlot({
    plot_grid(plots()[[3]], plots()[[4]], nrow=2, ncol=1, align="hv")
  })
  
  # main plots
  cPlots = reactive({calib_plots(out(), data() )})
  output$Calib <- renderPlot({
    plot_grid(cPlots(), nrow=1, ncol=1, align="hv")
  })
  
  # table
  output$tbl = renderTable({ data() })
  output$tbl2 = renderTable({ data_input() })
  
  # make report
  output$download_Data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(out(), file)
    }  
  )
  
  # make report
  output$download_Inputs <- downloadHandler(
    filename = function() {
      paste("inputs_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Define inputs to save
      inputs_to_save <- c("contact_mod_1", "contact_mod_2", "t",
                          "lambda", "pmbase", "pmint1", "pmint2", "v", "f_c", "alpha",
                          "f_s", "gamma", "p_s", "p_c", "delta_s", "delta_c", "epsilon_c",
                          "N", "E", "I_m", "R_mu", "R_md",
                          "I_s", "H_su", "H_sd", "R_su", "R_sd",
                          "I_c", "H_cu", "H_cd", "C_cu", "C_cd", "R_cu",
                          "R_cd")
      
      # Declare inputs
      inputs <- NULL
      # Append all inputs before saving to folder
      for(input.i in inputs_to_save){
        inputs <- append(inputs, input[[input.i]])
      }
      # Inputs data.frame
      inputs_data_frame <- data.frame(inputId = inputs_to_save, value = inputs)
      # Save Inputs
      write.csv(inputs_data_frame, file, row.names = FALSE)
    }  
  )
  

}

####################

ui <- fluidPage(theme=shinytheme("simplex"),
                useShinyjs(),
                titlePanel("Simple COVID-19 Model"),
                sidebarLayout(
                  sidebarPanel(
                    tabsetPanel(
                      tabPanel("Main", fluid=TRUE,
                               #includeMarkdown("content/instructions.md"),
                               
                               h4("Social distancing:"),
                               
                               sliderInput("t", "Time Horizon (days)",     min=32, max=180, value=50),
                               sliderInput("per2", "Period 2 start day",     min=1, max=180, value=20),
                               sliderInput("contact_mod_1", "Proportion of usual contacts (Period 2)", min=0, max=1, value=.4),
                               sliderInput("per3", "Period 3 start day",     min=1, max=180, value=40),
                               sliderInput("contact_mod_2", "Proportion of usual contacts (Period 3)", min=0, max=1, value=.2),
                               
                               h4("Testing:"),
                               sliderInput("pmbase", "Detection probability (Period 1)", min=0, max=1, value=.05),
                               sliderInput("pmint1", "Detection probability (Period 2)", min=0, max=1, value=.1),
                               sliderInput("pmint2", "Detection probability (Period 3)", min=0, max=1, value=.2),
                               
                               h4("Transmission:"),
                               sliderInput("lambda", HTML("&beta;"), min=.4, max=1.4, value=.8),
                               actionButton("save", "Save inputs"),
                               actionButton("restore_saved", "Restore saved inputs"),
                               h4(""),
                               actionButton("restore_all", "Restore original inputs")
                      ),
                      tabPanel("Epidemiology", fluid=TRUE,
                            #   includeMarkdown("content/parameters.md"),
                            h4("Transition rates:"),
                            
                               sliderInput("v", "Latent period (days)", min=3, max=8, value=6),
                               sliderInput("gamma", "Recovery rate (days)", min=3, max=8, value=5),
                            
                            h4("Severity:"),
                            sliderInput("alpha", "Relative infectiousness of mild cases", min=0, max=1, value=1),
                            
                              sliderInput("f_s", "Proportion of severe cases", min=0, max=1, value=0.1),
                               sliderInput("f_c", "Proportion of critical cases", min=0, max=1, value=0.04),
                            
                              sliderInput("p_s", "Proportion of severe cases detected", min=0, max=1, value=0.95),
                              sliderInput("p_c", "Proportion of critical cases detected", min=0, max=1, value=0.95),
                            
                            h4("Care:"),
                            
                              sliderInput("delta_s", "Hospital exit rate (days)", min=5, max=12, value=10),
                               sliderInput("delta_c", "Rate of progression to ICU from hospital (days)", min=3, max=9, value=4),
                            sliderInput("epsilon_c", "Exit rate from ICU (days)", min=7, max=13, value=10),
                            actionButton("save2", "Save inputs"),
                            actionButton("restore_saved2", "Restore saved inputs"),
                            h4(""),
                            actionButton("restore_all2", "Restore original inputs")
                      ),
                      
                      tabPanel("Initial conditions", fluid=TRUE,
                        numericInput("N", "N", 1000000, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("E", "E", 100, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("I_m", HTML("I<sub>m</sub>"), 40, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("R_md", HTML("R<sub>md</sub>"), 4, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("R_mu", HTML("R<sub>mu</sub>"), 40, min = NA, max = NA, step = NA, width = NULL),
                        
                        numericInput("I_s", HTML("I<sub>s</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("H_su", HTML("H<sub>su</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("H_sd", HTML("H<sub>sd</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("R_su", HTML("R<sub>su</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("R_sd", HTML("R<sub>sd</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        
                        numericInput("I_c", HTML("I<sub>c</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("H_cu", HTML("H<sub>cu</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("H_cd", HTML("H<sub>cd</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("C_cu", HTML("C<sub>cu</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("C_cd", HTML("C<sub>cd</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("R_cu", HTML("R<sub>cu</sub>"), 0, min = NA, max = NA, step = NA, width = NULL),
                        numericInput("R_cd", HTML("R<sub>cd</sub>"), 0, min = NA, max = NA, step = NA, width = NULL)
                      )),
                    width=3),

                  
                  mainPanel(tabsetPanel(type = "tabs",
                                        tabPanel("NPI", plotOutput("Plot", height = "700px")),
                                        tabPanel("No intervention",
                                                 plotOutput("Plot_No_NPI", height = "700px")),
                                        tabPanel("Calibration", 
                                                 plotOutput("Calib", height = "1000px", width = "1200px")),
                                        tabPanel("Upload/Download",
                                                 column(4,
                                                   h4("Calibration data"),
                                                   fileInput("file1", "Choose CSV File",
                                                             multiple = FALSE,
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")),
                                                   tableOutput("tbl")),
                                                  column(4,
                                                         h4("Model inputs"),
                                                         fileInput("file2", "Choose CSV File",
                                                                   multiple = FALSE,
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv")),
                                                         tableOutput("tbl2")
                                                 ),
                                                 column(2,
                                                        h4("Downloads"),
                                                        downloadButton(outputId = "download_Inputs", 
                                                                       label = 'Download inputs',
                                                                       class= "mybutton"),
                                                        downloadButton(outputId = "download_Data", 
                                                                       label = 'Download projection',
                                                                       class= "mybutton")
                                                        )
                                                 ),
                                        tabPanel("Documentation",
                                                 h4(""),
                                                 tags$iframe(style="height:500px; width:100%; scrolling=no; border: none", 
                                                             src="model_diagram.pdf"),
                                                 includeMarkdown("content/documentation.md")
                                                 )
                  ))
                ),
                hr(),
                includeMarkdown("content/footer.md")
                )

shinyApp(ui = ui, server = server)
