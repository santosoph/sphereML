library(shiny)
library(shinyalert)
library(shinydashboard)
library(spheredata)
library(DT)
library(lavaan)
library(semPlot)
library(CTT)
library(mirt)
library(shinycssloaders)
library(FSelectorRcpp)
library(randomForest)
library(caret)
library(caTools)
library(pROC)
library(GA)
library(genalg)
library(shinyFiles)
library(readxl)

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "sphereML"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "introduction", icon = icon("home")),
      menuItem("Data preparation", tabName = "data", icon = icon("file")),
      menuItem("Data validation", tabName = "validation", icon = icon("circle-nodes"),
               menuSubItem("Content validity", tabName = "conval"),
               menuSubItem("Factor analysis", tabName = "factan"),
               menuSubItem("Item analysis", tabName = "iteman"),
               menuSubItem("Reliability analysis", tabName = "reliability")),
      menuItem("ML Model training", tabName = "training", icon = icon("code-compare"),
               menuSubItem("Feature selection", tabName = "fselect"),
               menuSubItem("Hyperparameter tuning", tabName = "hypetune"),
               menuSubItem("Train the model", tabName = "trainml")),
      menuItem("Learning analytics", tabName = "analytics", icon = icon("magnifying-glass-chart")),
      menuItem("About", tabName = "about", icon = icon("mixer")),
      menuItem("References", tabName = "reference", icon = icon("readme"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              fluidPage(
                
                tags$style(HTML("
                body {font-size: 16px;}
                h1 {font-size: 30px; font-weight: bold;}
                h2 {font-size: 25px;}
                strong {font-size: 20px;}
                p {font-size: 20px;}
                ul.custom-bullets li {font-size: 20px;}")),
                
                h1("Introduction"),
                h2("Welcome to the sphereML package"),
                p("The sphereML is a free open-source statistical software packaged for applying machine learning (ML) techniques in physics education research (PER). It has been developed within the R programming language, facilitated by the", a("Shiny", href = "http://shiny.rstudio.com"), "package."),
                
                strong("Link to the", a("sphereML's quick-start user guide", href = "https://github.com/santosoph/sphereML/")),
                br(),
                
                h3("An evolving software"),
                
                p("The main goal of sphereML is to fit the needs for machine learning implementation in physics education research (PER). If any feature you find useful/ would like to use on sphereML is not available yet, you can ask for its implementation on the", a("sphereML Github Repository.", href = "https://github.com/santosoph/sphereML/issues")),
                
                h3("Citing the sphereML package in your work"),
                p("If you found sphereML useful and used it for your research, please cite the", a("paper published in the Scientific Data journal.",href="https://joss.theoj.org/papers/10.21105/joss.02630" )),
                
                h2("Current Features version 0.1.0"),
                p("The features facilitated by the sphereML package will be regurlarly updated:"),
                
                tags$ul(
                  class = "custom-bullets",
                  tags$li("Data preparation: a showcase of dataset facilitated by the spheredata package."),
                  tags$li("Data validation: content validity using the method of Lawshe and Aiken."),
                  tags$li("Data validation: confirmatory factor analysis (CFA)."),
                  tags$li("Data validation: classical test theory (CTT) and item response theory (IRT)."),
                  tags$li("Data validation: reliability analysis based on Cronbach's method."),
                  tags$li("ML model training: filter-based feature selection using item response theory (IRT)."),
                  tags$li("ML model training: hyperparameteer tuning using genetic algorithm (GA)."),
                  tags$li("ML model training: train the random forest to predict student's performance in physics."))
                
              ) # Fluidpage
              
      ),
      
      # Second tab content
      tabItem(tabName = "data",
              fluidPage(
                tags$head(
                  tags$style(HTML(".indented-paragraph {text-indent: 30px;}
                                  "))
                ),
                
                h1("Data preparation"),
                p("The data has been made available in the CRAN repository through the", a("spheredata",href="https://cran.r-project.org/package=spheredata"), "package. SPHERE stands for Students' Performance in Physics Education Research (PER). The students are the eleventh graders learning physics at high school curriculum. We follow the stream of multidimensional students' assessment as probed by some research based assessments (RBAs) in PER. The goal is to predict the students' performance at the end of the learning process. Three learning domains are measured including conceptual understanding, scientific ability, and scientific attitude. Furthermore, demographic backgrounds and potential variables influencing students' performance on physics are also demonstrated. We provide teachers' judgment data as our baseline to compare the predictive results of students' performance between machine learning (ML) based analysis and teacher (human) based judgment. Click on the Tab below to explore the detail of each data further.", class = "indented-paragraph"),
                
                tabsetPanel(
                  id = "data_tabs",
                  tabPanel("Demographic", p(""), DTOutput("table1")),
                  tabPanel("Literacy", p(""), DTOutput("table2")),
                  tabPanel("Physics identity", p(""), DTOutput("table3")),
                  tabPanel("Teachers judgment", p(""),  DTOutput("table4")),
                  tabPanel("FCI", p(""), DTOutput("table5")),
                  tabPanel("FMCE", p(""), DTOutput("table6")),
                  tabPanel("RRMCS", p(""), DTOutput("table7")),
                  tabPanel("FMCI", p(""), DTOutput("table8")),
                  tabPanel("MWCS", p(""), DTOutput("table9")),
                  tabPanel("TCE", p(""), DTOutput("table10")),
                  tabPanel("STPFASL", p(""), DTOutput("table11")),
                  tabPanel("SAAR", p(""), DTOutput("table12")),
                  tabPanel("CLASS", p(""), DTOutput("table13"))
                )
              )
      ),
      
      # Third tab content
      # First sub of the third tab content
      tabItem(tabName = "conval",
              h1("Content validity evidence"),
              p("The content validity is a useful statistical technique to determine the validity of individual instrument items, as rated by a panel of content experts. In this study, five experts from physics education scholars were involved."),
              column(
                width = 4,
                selectInput("convaldata_choice", "Choose the data:", 
                            choices = list("Please choose" = "",
                                           "FCI" = "table14",
                                           "FMCE" = "table15", 
                                           "RRMCS" = "table16",
                                           "FMCI" = "table17",
                                           "MWCS" = "table18",
                                           "TCE" = "table19",
                                           "STPFASL" = "table20",
                                           "SAAR" = "table21",
                                           "CLASS" = "table22"),
                            selected = NULL),
                selectInput("conval_choice", "Choice of the index:", 
                            choices = list("Please choose" = "",
                                           "Aiken's V" = "aikenV",
                                           "Lawshe's CVR"     = "lawshecvr"),
                            selected = NULL)
              ),
              
              column(
                width = 8,
                box(
                  width = NULL, 
                  status = "primary", 
                  dataTableOutput("output_table_conval")
                )
              )
      ),
      
      # Second sub of the third tab content
      tabItem(tabName = "factan",
              h1("Validity evidence from factor analysis"),
              p("The method of factor analysis is intended to provide construct validity evidence of assessment constructs. It can be used to explain how the internal structure has been confirmed by the data empirically. In this package, one of the widely used factor analysis methods, confirmatory factor analysis (CFA), is performed."),
              br(),
              column(
                width = 5,
                selectInput("factandata_choice", "Choose the data:", 
                            choices = list("Please choose" = "",
                                           "FCI" = "table23",
                                           "FMCE" = "table24", 
                                           "RRMCS" = "table25",
                                           "FMCI" = "table26",
                                           "MWCS" = "table27",
                                           "TCE" = "table28",
                                           "STPFASL" = "table29",
                                           "SAAR" = "table12",
                                           "CLASS" = "table13"),
                            selected = NULL, width = 250),
                
                uiOutput("factan_var_select_ui"),
                
                textAreaInput("factan_spec", "Model Specification:", rows = 10, width = 500, 
                              placeholder = "Copy the abovementioned theoretical model here. Example: \nF1 =~ x1 + x2 + x3 \nF2 =~ x4 + x5 + x6"),
                
                actionButton("factan_run_model", "Run Analysis")
              ),
              
              column(
                width = 7,
                
                tabsetPanel(
                  id = "factan_tabs",
                  tabPanel("Model Fit", dataTableOutput("factan_modelfit")),
                  tabPanel("Unstandardized Estimate", dataTableOutput("factan_modelunstandard")),
                  tabPanel("Standardized Estimate", dataTableOutput("factan_modelstandard")),
                  tabPanel("Modification Indices", dataTableOutput("factan_modindices")),
                  tabPanel("Plot", div(style = "height: 500px; overflow: auto; ", plotOutput("factan_plot", height = "3000px")))
                  )
              )
      ),
      
      # Third sub of the third tab content
      tabItem(tabName = "iteman",
              h1("Validity evidence from item analysis"),
              p("Some analyses from the perspective of CTT and IRT are performed. You could directly compare the results to understand the measurement of both methods."),
              
              column(
                width = 6,
                h2("Classical test theory (CTT) analysis"),
                tabsetPanel(
                  id = "itemantabs_ctt",
                  tabPanel("Item Analysis", 
                           selectInput("iteman_choice_ctt_1", "Choose the data",
                                       choices = list("Please choose" = "",
                                                      "FCI" = "table30",
                                                      "FMCE" = "table31", 
                                                      "RRMCS" = "table32",
                                                      "FMCI" = "table33",
                                                      "MWCS" = "table34",
                                                      "TCE" = "table35",
                                                      "STPFASL" = "table36"),
                                       selected = NULL, width = 150),
                           dataTableOutput("iteman_ctt_1")),
                  
                  tabPanel("Distractor Analysis",
                           fluidRow(
                             column(4,selectInput("iteman_choice_ctt_2", "Choose the data",
                                                  choices = list("Please choose" = "",
                                                                 "FCI" = "table37",
                                                                 "FMCE" = "table38",
                                                                 "RRMCS" = "table39",
                                                                 "FMCI" = "table40",
                                                                 "MWCS" = "table41",
                                                                 "TCE" = "table42",
                                                                 "STPFASL" = "table43"),
                                                  selected = NULL, width = 150)),
                             column(8,uiOutput("ctt_column_select_ui"))
                           ),
                           
                           dataTableOutput("iteman_ctt_2")),
                  
                  tabPanel("Plot ICC",
                           fluidRow(
                             column(4, selectInput("iteman_choice_ctt_3", "Choose the data",
                                         choices = list("Please choose" = "",
                                                        "FCI" = "table23",
                                                        "FMCE" = "table24",
                                                        "RRMCS" = "table25",
                                                        "FMCI" = "table26",
                                                        "MWCS" = "table27",
                                                        "TCE" = "table28",
                                                        "STPFASL" = "table29"),
                                         selected = NULL, width = 150)),
                             column(8, uiOutput("ctt_plot_select_ui"))
                           ),
                           
                           plotOutput("iteman_ctt_3")),
                  
                  tabPanel("Ability Distribution", 
                           selectInput("iteman_choice_ctt_4", "Choose the data",
                                       choices = list("Please choose" = "",
                                                      "FCI" = "table23",
                                                      "FMCE" = "table24",
                                                      "RRMCS" = "table25",
                                                      "FMCI" = "table26",
                                                      "MWCS" = "table27",
                                                      "TCE" = "table28",
                                                      "STPFASL" = "table29"),
                                       selected = NULL, width = 150),
                           plotOutput("iteman_ctt_4"))
                )
              ),
              
              column(
                width = 6,
                h2("Item response theory (IRT) analysis"),
                tabsetPanel(
                  id = "itemantabs_irt",
                  tabPanel("Model Fit",
                           fluidRow(
                             column(4, selectInput("iteman_choice_irt_1", "Choose the data",
                                                choices = list("Please choose" = "",
                                                               "FCI" = "table23",
                                                               "FMCE" = "table24",
                                                               "RRMCS" = "table25",
                                                               "FMCI" = "table26",
                                                               "MWCS" = "table27",
                                                               "TCE" = "table28",
                                                               "STPFASL" = "table29"),
                                                selected = NULL, width = 150)),
                             column(8, actionButton("run_irt_modelfit_ui", "Run Analysis", style = "margin-top: 29px; margin-left: -50px"))
                           ),
                           
                           dataTableOutput("iteman_irt_1")),
                  
                  tabPanel("Item Analysis",
                           fluidRow(
                             column(4, selectInput("iteman_choice_irt_2", "Choose the data",
                                                   choices = list("Please choose" = "",
                                                                  "FCI" = "table23",
                                                                  "FMCE" = "table24",
                                                                  "RRMCS" = "table25",
                                                                  "FMCI" = "table26",
                                                                  "MWCS" = "table27",
                                                                  "TCE" = "table28",
                                                                  "STPFASL" = "table29"),
                                                   selected = NULL, width = 150)),
                             
                             column(8, selectInput("iteman_model_irt_2", "Select the model",
                                                   choices = list("Please choose" = "",
                                                                  "1PL" = "table44",
                                                                  "2PL" = "table45",
                                                                  "3PL" = "table46"),
                                                   selected = NULL, width = 150))
                           ),
                           dataTableOutput("iteman_irt_2")),
                  
                  tabPanel("Distractor Analysis",
                           fluidRow(
                             column(4, selectInput("iteman_choice_irt_3", "Choose the data",
                                                   choices = list("Please choose" = "",
                                                                  "FCI" = "table5",
                                                                  "FMCE" = "table6",
                                                                  "RRMCS" = "table7",
                                                                  "FMCI" = "table8",
                                                                  "MWCS" = "table9",
                                                                  "TCE" = "table10",
                                                                  "STPFASL" = "table11"),
                                                   selected = NULL, width = 150)),
                             
                             column(4, uiOutput("irt_distractor_select_ui"))
                           ),
                           
                           plotOutput("iteman_irt_3")),
                  
                  tabPanel("Plot ICC",
                           fluidRow(
                             column(4, selectInput("iteman_choice_irt_4", "Choose the data",
                                                   choices = list("Please choose" = "",
                                                                  "FCI" = "table23",
                                                                  "FMCE" = "table24",
                                                                  "RRMCS" = "table25",
                                                                  "FMCI" = "table26",
                                                                  "MWCS" = "table27",
                                                                  "TCE" = "table28",
                                                                  "STPFASL" = "table29"),
                                                   selected = NULL, width = 150)),
                             
                             column(4, selectInput("iteman_model_irt_4", "Select the model",
                                                   choices = list("Please choose" = "",
                                                                  "1PL" = "table44",
                                                                  "2PL" = "table45",
                                                                  "3PL" = "table46"),
                                                   selected = NULL, width = 150)),
                             
                             column(4, uiOutput("irt_plot_select_ui"))
                           ),
                           
                           plotOutput("iteman_irt_4")),
                  
                  tabPanel("Ability Distribution",
                           fluidRow(
                             column(4, selectInput("iteman_choice_irt_5", "Choose the data",
                                                   choices = list("Please choose" = "",
                                                                  "FCI" = "table23",
                                                                  "FMCE" = "table24",
                                                                  "RRMCS" = "table25",
                                                                  "FMCI" = "table26",
                                                                  "MWCS" = "table27",
                                                                  "TCE" = "table28",
                                                                  "STPFASL" = "table29"),
                                                   selected = NULL, width = 150)),
                             
                             column(8, selectInput("iteman_model_irt_5", "Select the model",
                                                   choices = list("Please choose" = "",
                                                                  "1PL" = "table44",
                                                                  "2PL" = "table45",
                                                                  "3PL" = "table46"),
                                                   selected = NULL, width = 150))
                           ),
                           plotOutput("iteman_irt_5"))
                )
              )
      ),
      
      # Fourth sub of the third tab content
      tabItem(tabName = "reliability",
              h1("Reliability estimation"),
              p("The method of reliability estimation based on Cronbach's method performed here is calculated using the", strong("alpha"), " function provided by the ", strong("psych"),"package. As you can see, some reliability coefficients are reported. The ", strong("raw_alpha"),"is Cronbach's alpha calculated using unstandardized data. Conversely, the ", strong("std_alpha"),"is calculated using the standardized data. The", strong("G6")," coefficient is Guttman's Lambda that considers the amount of variance in each item. The ", strong("average_r"),"is an average of interitem correlation. The ", strong("S/N"),"(signal/ noise) ratio is a useful index of quality of the test that is linear with the number of items and the average correlation. The ", strong("ase"),"stands for the alpha's standard error."),
              br(),
              
              column(
                width = 3,
                selectInput("reliabilitydata_choice", "Choose the data:", 
                            choices = list("Please choose" = "",
                                           "FCI" = "table23",
                                           "FMCE" = "table24", 
                                           "RRMCS" = "table25",
                                           "FMCI" = "table26",
                                           "MWCS" = "table27",
                                           "TCE" = "table28",
                                           "STPFASL" = "table29",
                                           "SAAR" = "table12",
                                           "CLASS" = "table13"),
                            selected = NULL, width = 250),
                
                uiOutput("reli_var_select_ui"),
                
                actionButton("reliability_run", "Run Analysis")),
              
              column(
                width = 9,
                tabsetPanel(
                  id = "reli_tabs",
                  tabPanel("Cronbach's Alpha", dataTableOutput("reliability_alpha")),
                  tabPanel("If an item is dropped", dataTableOutput("item_drop_alpha"))
                )
                
              )
      ),
      
      # Fourth tab content
      # First sub of the fourth tab content
      tabItem(tabName = "fselect",
              h1("Feature selection using filter-based method"),
              p("On the numerical data, we implemented a filter based method using the IRT approach to study the importance of each variable for a machine learning implementation. Using the IRT approach, features are thought as latent constructs that can be psychometrically explored to produce a prediction of student's performance in physics."),
              br(),
              column(
                width = 6,
                tabsetPanel(
                  id = "num_data_fselect",
                  tabPanel("Numerical data",
                           h5("Based on the fitted factor analysis models formerly, we used them to extract factor scores of each test. The structure of our data features is shown below. This will take a few seconds for computation."),
                           withSpinner(dataTableOutput("fselect_data"), type = 7)),
                  
                  tabPanel("Encode to 2",
                           h5("The features data should be encoded to dichotomous format based on the median splitting method. This is aimed for three-parameter logistic (3-PL) model analysis. The code of 1 denotes the higher student's performance on each feature."),
                           withSpinner(dataTableOutput("dichotomous_data"), type = 7)),
                  
                  tabPanel("Encode to 4",
                           h5("The features data should be encoded to polytomous format based on the quartile splitting method. This is aimed for graded response model (GRM) analysis. The higher categories correspond to the higher student's performance on each feature."),
                           withSpinner(dataTableOutput("polytomous_data"), type = 7)),
                  
                  tabPanel("IRT-based feature selection results",
                           h5("The a_d, b_d, and g_d contain the discriminatory power, difficulty, and guessing factor from the 3-PL model respectively. The a_p is a discrimination parameter shown by the GRM. All those values are scaled based on the z-score distribution. The feature importance is calculated using the multi-objective optimization (MOO) formula."),
                           withSpinner(dataTableOutput("IRT_fselect"), type = 7))
                )
              ),
              
              column(
                width = 6,
                tabsetPanel(
                  id = "cat_data_fselect",
                  tabPanel("Categorical data", br(), br(),
                           withSpinner(dataTableOutput("fselect_data_cat"), type = 7)),
                  
                  tabPanel("IRT-based feature selection results", br(), br(),
                           withSpinner(dataTableOutput("IRT_fselect_data_cat"), type = 7))
                )
              )
      ),
      
      # Second sub of the fourth tab content
      tabItem(tabName = "hypetune",
              h1("Hyperparameter tuning using genetic algorithm (GA)"),
              p("There are some hyperparameters that should be optimized during the training of the RF model. They include number of trees to grow (", strong("ntree"),"), number of variables randomly sampled as candidates at each split (", strong("mtry"),"), size(s) of sample to draw (", strong("sampsize"), "), minimum size of terminal nodes (", strong("nodesize"), "), maximum number of terminal nodes trees in the forest can have (", strong("maxnodes"), "), splitting ratio used for training and testing data (", strong("splitratio"), "), and number of features selected (", strong("num_features"), "). "),
              
              column(
                width = 3,
                sliderInput("popSize", "Population Size", min = 10, max = 50, value = 15, step = 1),
                sliderInput("maxiter", "Max Generations", min = 5, max = 100, value = 30, step = 1),
                sliderInput("pcrossover", "Crossover Probability", min = 0.1, max = 1, value = 0.8, step = 0.1),
                sliderInput("pmutation", "Mutation Probability", min = 0.01, max = 0.5, value = 0.1, step = 0.01),
              ),
              
              column(
                width = 3,
                selectInput("objective", "Optimization objective",
                            choices = c("Accuracy", "Kappa", "Sensitivity", "Specificity", "Recall", "F1"),
                            selected = "F1"),
                uiOutput("GA_var_select_ui"),
                actionButton("runGA", "Run GA Optimization")
              ),
              
              column(
                width = 6,
                
                tabsetPanel(
                  id = "hypetune_results",
                  tabPanel("Summary", br(), verbatimTextOutput("summary_gaResults")),
                  tabPanel("Plot of fitness value", br(), plotOutput("plot_gaResults", height = "500px")),
                  tabPanel("Optimized hyperparameter", br(), dataTableOutput("hype_gaResults"))
                )
              )
      ),
      
      # Third sub of the fourth tab content
      tabItem(tabName = "trainml",
              h1("Training the ML model content"),
              column(width = 3,
                     uiOutput("RF_var_select_ui"),
                     numericInput("rf_ntree", "ntree:", value = 500, min = 1, max = 1000),
                     numericInput("rf_mtry", "mtry:", value = 5, min = 1, max = 50),
                     numericInput("rf_sampsize", "sampsize:", value = 235, min = 1, max = 300),
                     numericInput("rf_nodesize", "nodesize:", value = 5, min = 1, max = 50),
                     numericInput("rf_maxnodes", "maxnodes:", value = 10, min = 1, max = 50),
                     actionButton("train", "Train the model")
              ),
              
              column(
                width = 9,
                tabsetPanel(
                  id = "trainrf_results",
                  tabPanel("Summary", br(), verbatimTextOutput("summary_rfResults")),
                  tabPanel("Performance metrics", br(), verbatimTextOutput("train_rfResults")),
                  tabPanel("ROC plot", br(), plotOutput("plot_roc_rf", height = "500px")),
                  tabPanel("Error rate plot", br(), plotOutput("plot_error_rf", height = "500px")),
                  tabPanel("Save the model", 
                           h4("Now you can download your trained ML model saved for the next section (Learning Analytics)."), 
                           br(), downloadButton("saveML", "Save ML model"))
                )
              )
      ),
      
      # Fifth tab content
      tabItem(tabName = "analytics",
              h1("Learning analytics content"),
              p("Learning analytics is a digital platform made for enhancing educational process driven by our analysis through students' data collected from learning environment. One of the analytical task is a prediction of students' performance at the end of the semester. You can use the trained model to make this predictive information to your own data."),
              
              tabsetPanel(
                id = "LA_results",
                tabPanel("Raw data", 
                         column(3, fileInput("datafile", "Upload your data (.XLSX)", accept = c(".xlsx"))),
                         column(3, hr(style = "margin: 13.5px"), actionButton("open_raw_LA", "Show the data")),
                         dataTableOutput("raw_LA_data")),
                
                tabPanel("Prediction result",
                         column(3, fileInput("modelfile", "Upload the model (.RDS)", accept = c(".rds"))),
                         column(3, hr(style = "margin: 13.5px"), actionButton("predict_LA", "Predict")),
                         DTOutput("predict_LA_results"))
                         
              )
      ),
      
      # Sixth tab content
      tabItem(tabName = "about",
              h1("About the author"),
              p("My name is ", a("Purwoko Haryadi Santoso", href = "https://www.scopus.com/authid/detail.uri?authorId=57214234882/"),"
                and I am a lecturer at the Department of Physics Education, Universitas Sulawesi Barat in Majene, INDONESIA.
                I am currently doing a doctoral program in educational measurement at the", strong("Department of Educational Research and
                Evaluation, Universitas Negeri Yogyakarta, INDONESIA"), "developing a novel approach to implement machine learning (ML)
                techniques for enhancing physics education practices and studies."),
              
              h1("Supervisors"),
              p("Prof. Dr. Edi Istiyono, M.Si and Dr. Haryanto, M.Pd., M.T."),
              
              h1("Development"),
              p(
                "This software package has been developed with the R language using the ", a("Shiny package", href = "http://shiny.rstudio.com"),
                "under the", a("RStudio", href = "https://rstudio.com/"), " environment."
              ),
              
              h3("Packages and function used"),
              p("The sphereML package is based on multiple packages and function to provide you an easy-to-use interface for statistical analysis.
                Random forest modelling is performed mainly with ", strong("randomForest"), "combined with the ", strong("caret"), " and ", strong("caTools"), " packages. Item Response Theory (IRT) and Genetic Algorithm (GA) are modelled using the",
                strong("mirt"), "and", strong("GA"), " packages respectively. The ",strong("tidyr"), ", ",strong("dplyr"), ", ",strong("tibble"), " and ",strong("broom"), " packages are used for data tidying and formatting."
              ),
              
              
              h1("Contact"),
              p(
                "To report a bug or request/suggest a new feature, please open a",
                a("Github issue.", href = "https://github.com/santosoph/sphereML/issues"),
                "You can also contact me by ", a("mail.", href = "mailto:purwokoharyadisantoso@unsulbar.ac.id")
              )
      ),
      
      # Eighth tab content
      tabItem(tabName = "reference",
              h1("References used in this package"),
              br(),
              p("Breiman, L., Cutler, A., Liaw, A., & Wiener, M. (2022).", em("randomForest: Breiman and Cutler’s Random Forests for Classification and Regression."), "R package version 4.7-1.1.", a("https://cran.r-project.org/web/packages/randomForest", href = "https://cran.r-project.org/web/packages/randomForest")),
              p("Chalmers, R. P. (2012). mirt: A multidimensional item response theory package for the R environment.", em("Journal of Statistical Software, 48."), "(6).", a("https://doi.org/10.18637/jss.v048.i06", href = "https://doi.org/10.18637/jss.v048.i06")),
              p("Kline, A. S., Kline, T. J. B., & Lee, J. (2021). Item response theory as a feature selection and interpretation tool in the context of machine learning.", em("Medical and Biological Engineering and Computing, 59"),"(2).", a("https://doi.org/10.1007/s11517-020-02301-x", href = "https://doi.org/10.1007/s11517-020-02301-x")),
              p("R Core Team. (2023).", em("R: A language and environment for statistical computing."), "R Foundation for Statistical Computing.", a("https://www.R-project.org/", href = "https://www.R-project.org/")),
              p("Scrucca, L. (2013). GA: A package for genetic algorithms in R.", em("Journal of Statistical Software, 53"), "(4).", a("https://doi.org/10.18637/jss.v053.i04", href = "https://doi.org/10.18637/jss.v053.i04"))
      )
    )
  )
)

