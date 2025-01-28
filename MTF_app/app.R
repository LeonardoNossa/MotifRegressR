library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(plotly)
library(igraph)
library(monaLisa)
library(pls)
library(reshape2)
library(dplyr)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(MotifRegressR)


ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "logo.jpg", style = "height: 150px;")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("How to use?", tabName = "Usage", icon = icon("info-circle")),
      menuItem("Data", tabName = "Data", icon = icon("upload")),
      menuItem("Scoring & Filtering", tabName = "Scoring_Filtering", icon = icon("star")),
      menuItem("Regression", tabName = "Regression", icon = icon("chart-line")),
      menuItem("Consensus & Clustermap", tabName = "Consensus_Clustermap", icon = icon("project-diagram"))
    ),
    tags$div(
      style = "position: relative; bottom: 20px; left: 79px;",
      actionButton("Github", label = NULL, icon = icon("github"),
                   style = "font-size: 20px; color: #2C3E50; border: none; background: none;")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(
        "
        .main-sidebar .sidebar .sidebar-menu > li > a {
          display: block;
          margin: 0px;
          padding: 50px;
          font-size: 12px;
          background-color: #C8C8E8;
          color: #4B0082;
          text-align: center;
          position: relative;
          transition: all 0.3s ease-in-out;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li:first-child > a {
          clip-path: polygon(50% 100%, 
                             100% 70%, 
                             100% 0%, 
                             0% 0%, 
                             0% 70%);
          height: 50px;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li:nth-child(2) > a,
        .main-sidebar .sidebar .sidebar-menu > li:nth-child(3) > a,
        .main-sidebar .sidebar .sidebar-menu > li:nth-child(4) > a,
        .main-sidebar .sidebar .sidebar-menu > li:nth-child(5) > a {
          clip-path: polygon(50% 40%, 
                             100% 10%, 
                             100% 55%, 
                             50% 90%, 
                             0% 55%, 
                             0% 10%);
          margin-top: -35px;
          height: 50px;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li:first-child.active > a {
          background-color: #2C3E50;
          color: white;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li.active > a {
          color: white;
        }
        
        .main-sidebar .sidebar .sidebar-menu > li a:hover {
          background-color: #2C3E50;
          color: white;
        }
        
        .skin-blue .main-header .logo {
          background-color: #2C3E50;
          color: white;
          height: 150px !important;
        }
        
        .skin-blue .main-header .logo:hover {
          background-color: #2C3E50;
        }
        
        .skin-blue .main-header .navbar {
          background-color: #C8C8E8;
          height: 50px;
        }
        
        .skin-blue .main-sidebar {
          background-color: #C8C8E8;
          margin-top: 60px;
        }
        
        .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #2C3E50;
          border-left: #2C3E50;
        }
        
        .main-sidebar .sidebar .sidebar-menu a {
          color: #4B0082;
        }
        
        .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #2C3E50;
          border-left: #2C3E50;
        }
        
        .skin-blue .sidebar-toggle {
          display: none;
        }
        
        .skin-blue .box.box-primary .box-header {
          background-color: #2C3E50 !important;
          color: white;
        }
        
        .box.box-primary {
          border: 1px solid #2C3E50 !important;
        }
        
        .box.box-success .box-header {
          background-color: #FF9900 !important;
        }
        
        .box.box-success {
          border: 1px solid #FF9900 !important;
        }
        
        .hidden-box {
          display: none !important; 
        }
        
        .small-download-btn {
          font-size: 10px; 
          padding: 3px 7px; 
          background-color: #f8f9fa;
          border: 1px solid #ced4da;
          color: #495057;
          border-radius: 4px; 
          text-align: center;
          cursor: pointer;
          margin-bottom: 10px;
        }
        
        .small-download-btn:hover {
          background-color: #e2e6ea;
        }
        "
      ))
    ),
    tabItems(
      tabItem(
        tabName = "Usage",
        fluidPage(
          
          div(class = "text-center", h1("Discover MotifRegressR!")),
          
          h4("Enjoy the trailer: "),
          tags$video(src = "tutorial video.mp4", type = "video/mp4", controls = TRUE, style = "width: 100%; max-width: 800px; height: auto;"),  
          
          hr(),
          
          mainPanel(
            
            h3("Welcome to MotifRegressR application!"),
            p("MotifRegressR is a tool that integrates several regression approaches in order to perform regression over DNA binding motifs, in order to discover the most impactful motifs in different gene expression samples/conditions."),
            p("This page aims at giving the user a tutorial on how to use the Shiny application, which is built for non-R users. All of the functionalities available in the Application will be performed by the MotifRegressR R package. (look for the GitHub repository clicking the logo on the left!)."),
            
            hr(),
            
            h3("Data Upload"),
            p("The first step of the pipeline will be Data upload. In the panel \"Data\" you will have different boxes allowing the user to provide input data."),
            p("In the ", span("red box", style = "color: red;"), ", the user will be able to select a way to provide motifs' PFMs (either from JASPAR specifying the taxon id, or by providing an input MEME file). By clicking on the \"Conversion PFM\" the PFMs will be converted to PWMs."),
            p("When PFMs are loaded, details regarding them and genomic logos for each motif can be displayed by clicking on the elements of the list at the bottom of the page."),
            p("The user will also be able to load the FASTA and GFF file for the chosen organism, and select the size of the upstream regions to use to perform the analysis (default is 300 bp upstream of each gene)."),
            p("If the user already has a matrix of Scores (see next section for details), a direct upload is allowed."),
            p("In the ", span("green box", style = "color: green;"), ", the user will need to specify the organism on which analyses are performed. Right now, analyses can be performed on: "),
            tags$ul(
              tags$li("Bacteria"),
              tags$li("S. cerevisiae"),
              tags$li("D. melanogaster"),
              tags$li("C. elegans")
            ),
            p("In the ", span("blue box", style = "color: blue;"), ", the user will be able to load, inspect and modify the gene expression compendium."),
            p("In order to inspect the most similar samples, the user can visualize the compendium in a lower dimensional 2D space."),
            p("Different dimensionality reduction techniques can be selected and employed by the user. The user is also allowed to look up to ENA browser for metadata regarding samples in the compendium, by clicking on \"Retrieve Metadata\"."),
            p("Dimensionality reduction can be performed by specifying the dimensionality reduction approach and the feature of Metadata (if present) for which to color points in the 2D space. An interactive plotly plot is generated. If the user is interested in specific samples, the selection tool provided by plotly can be employed to select only the chosen motifs. Samples names will then appear in a box below the plot and can be easily copied using the \"Copy to clipboard\" button."),
            p("The user will also be allowed to perform PCA on the columns of the compendium, and then choose to perform regression on the reduced dataset, instead of the initial one. This step is completely optional. In order to perform this step, the user can click in the PCA tab panel and select the % of explained variance that wants to be reached (influences the number of dimensions) and whether or not to scale the data."),
            p("The metadata table and the PCA result of the compendium will be shown in the panel."),
            p("In the ", span("purple box", style = "color: purple;"), ", the user will be able to upload files, that will be used to retain only first genes for each operon, if the user specified \"Bacteria\" in the green box."),
            p("Files to be uploaded are TUS file (containing information of first genes of each operon) and the Protein Info file (containing information of gene names / gene IDs correspondance)."),
            
            tags$img(src = "Immagine3.png", height = "400px"),
            
            tags$img(src = "PFMs data.png", height = "400px"),
            tags$img(src = "Metadata dimensionality reduction.png", height = "400px"),
            
            hr(),
            
            h3("Scoring & Filtering"),
            p("In the panel \"Scoring & Filtering\", scoring and filtering steps can be performed."),
            p("Scoring procedure implies sliding PWMs on the upstream sequences and assigning a score at each sliding step. The final score for a particular motif on a particular upstream sequence is defined as the maximum score (most likely binding site)."),
            p("In the ", span("red box", style = "color: red;"), ", the user can choose whether or not to perform the scoring step considering also the reverse strand, or just the forward one. Since the procedure is very time consuming, parallelization is supported. The number of cores to use can be specified and the scoring step can be started by clicking \"Start Scoring\"."),
            p("If the user specified \"Bacteria\" in the previous panel, filtering can be performed, using TUS file and protein info file uploaded in the previous panels."),
            p("In the ", span("green box", style = "color: green;"), ", the user can click on \"Start filtering\" in order to perform it."),
            p("Tabular data for the Scores matrix and a plot of the score distribution are shown as a result."),
            
            tags$img(src = "Immagine5.png", height = "200px"),
            tags$img(src = "scores.png", height = "300px"),
            
            hr(),
            
            h3("Regression"),
            p("In the panel \"Regression\", we can perform 7 different types of regression, using the data we generated in the previous steps. In the ", span("red box", style = "color: red;"), ", the user will be able to select among different regression models, select the conditions on which to perform regression."),
            p("NOTE: If you visualized the compendium in a 2D space before, you can directly paste the \"Copy to Clipboard\" output."),
            p("The user will also be able to choose the K value, used to select the top K most important motifs for each condition. Several regression types employ randomness: by toggling \"Reproducible results\" to ON, the user can set a seed to ensure reproducibility."),
            p("By clicking \"Start Regression\", the regression step can be started and graphical outputs can be visualized, by selecting the condition in the bottom curtain menu."),
            p("Before starting the regression, the user can select parameters for the different regression types, as shown in the ", span("green box", style = "color: green;"), "."),
            p("The graphical output will be similar for each regression type: We will obtain a digraph representing the most important motifs in the selected conditions, as well as diagnostic plots to evaluate regression models (and tabular output of data used to generate them)."),
            
            tags$img(src = "Immagine7.png", height = "400px"),
            tags$img(src = "digraph_rand_lasso.png", height = "200px", style = "margin-top: 20px;"),
            tags$img(src = "rand_lasso_output.png", height = "400px", style = "margin-top: 20px;"),
            
            hr(),
            
            h3("Clustermap & Consensus"),
            p("The last panel (\"Clustermap & Consensus\") contains the last downstream steps: clustermap and consensus generation."),
            p("In the panel, the user will be able to select the regression data to use to perform the analyses."),
            p("For what concerns the clustermap generation, at the center of the image, the user will be able to select both the distance metric and the clustering approach for both columns and rows. By clicking \"Perform Clustermap\" the user will be able to visualize the graphical output."),
            p("For what concerns the consensus, the user can select \"by regression\" field:"),
            tags$ul(
              tags$li("If ", span("by regressor = TRUE", style = "color: purple;"), ", then consensus is generated by looking at the number of regression model that find a particular motif among the top K most important motifs for a particular condition."),
              tags$li("If ", span("by regressor = FALSE", style = "color: purple;"), ", the consensus will look at one regression type at a time, by considering the number of conditions for which the motif is among the top K most important motifs.")
            ),
            
            p("The consensus will be visualized as a heatmap."),
            p("In order to statistically validate the consensus (i.e. test the association between motifs and conditions), the user will be allowed to perform a Fisher's exact test. In the top right corner, the user will be able to select the significance value for the test. A textual output will be given to the user, providing a conclusion on the significance of the results."),
            p("The graphical outputs of both the clustermap and the consensus are interactive. The user is allowed to drag the cursor over an area of interest of the heatmap, to visualize a sub-heatmap on the side."),
            p("Moreover, by clicking on a specific cell, information regarding the specific cell will be displayed."),
            
            tags$img(src = "schermata input consensus ecc.png", height = "250px", style = "margin-bottom: 20px;"),
            fluidRow(
              column(6,
                     tags$img(src = "clustermap_lm.png", height = "250px", style = "margin-right: 10px;")
                     ),
              column(6,
                     tags$img(src = "consensus1.png", height = "250px"))
            ),
            tags$img(src = "fisher_test_output.png", height = "100px", style = "margin-top: 20px;"),
            
            hr(),
            
            h3("Project Pipeline"),
            tags$div(
              style = "display: flex; justify-content: center; align-items: center; height: 60vh; overflow: hidden; width: 70vw;",
              tags$img(src = "MotifRegressR.png", id = "zoomable-img", 
                       style = "width: 100%; max-width: 25000px; height: auto; cursor: grab;")
            ),
            
            tags$script(HTML("\n      
                             var offsetX = 0, offsetY = 0; \n      
                             var scaleValue = 1;\n\n      
                             $(document).on('mousedown', '#zoomable-img', 
                             function(event) {\n        
                             var img = $(this);\n\n        
                             if (event.button === 0) {\n         
                             var startX = event.pageX - offsetX;\n          
                             var startY = event.pageY - offsetY;\n          
                             $(this).css('cursor', 'grabbing');\n\n          
                             $(document).on('mousemove', function(event) {\n            
                             offsetX = event.pageX - startX;\n            
                             offsetY = event.pageY - startY;\n            
                             img.css({\n              
                             'transform': 'scale(' + scaleValue + ') translate(' + offsetX + 'px, ' + offsetY + 'px)'\n            
                             });\n          
                             });\n\n          
                             $(document).on('mouseup', function() {\n            
                             $(document).off('mousemove');\n            
                             $(document).off('mouseup');\n           
                             img.css('cursor', 'grab');\n          
                             });\n        
                             }\n      
                             });\n\n      
                             $(document).on('keydown', function(event) {\n        
                             if (event.keyCode === 32) {\n          
                             scaleValue = Math.max(1, scaleValue - 0.2);\n          
                             $('#zoomable-img').css({\n            
                             'transform': 'scale(' + scaleValue + ') translate(' + offsetX + 'px, ' + offsetY + 'px)'\n          
                             });\n          
                             $('#zoomable-img').css('cursor', 'zoom-out');\n        
                             }\n      
                             });\n\n      
                             $(document).on('mousedown', '#zoomable-img', function(event) {\n        
                             var img = $(this);\n\n        
                             if (event.button === 2) {\n          
                             event.preventDefault();\n          
                             scaleValue += 0.2;\n          
                             img.css({\n            
                             'transform': 'scale(' + scaleValue + ') translate(' + offsetX + 'px, ' + offsetY + 'px)'\n          
                             });\n          
                             img.css('cursor', 'zoom-in');\n        
                             }\n      
                             });\n\n      
                             $(document).on('contextmenu', '#zoomable-img', function(event) 
                             {\n        
                             event.preventDefault();\n      
                             });\n    
                             ")),    
            
            hr(),
            
            h3("Github Repository"),
            p("Find the code and additional resources on the MotifRegressR GitHub repository.")
          )
        )
      ),
      tabItem(
        tabName = "Data",
        fluidRow(
          box(
            title = "Upload files for scoring", status = "primary", solidHeader = TRUE, width = 6,
            div(
              fluidRow(
                column(6,
                       radioButtons("database_choice", "Choose Database:",
                                    choices = c("JASPAR" = "jaspar", "MyData" = "mydata"),
                                    selected = NULL)),
                column(6,
                       conditionalPanel(
                         condition = "input.database_choice == 'jaspar'",
                         textInput("Tax_ID", "Tax_ID")
                       ))
              )
            ),
            conditionalPanel(
              condition = "input.database_choice == 'mydata'",
              fileInput("meme_file", "Add .meme file", multiple = FALSE, accept = c(".meme"))
            ),
            fileInput("fna_file", "Add .fna file", multiple = FALSE, accept = c(".fna")),
            actionButton("Conversion_PFM", "Conversion PFM", icon = icon("bolt")),
            hr(),
            div(
              fluidRow(
                column(6,
                       fileInput("gff_file", "Add .gff file", multiple = FALSE, accept = c(".gff"))
                       ),
                column(6,
                        textInput("upstream","Upstream bp for each genes: ")
                        )
              )
            ),
            hr(),
            p("If you already have the scores file upload it: "),
            fileInput("score_csv", "Add score file (.csv)", multiple = FALSE, accept = c(".csv"))
          ),
          box(
            title = "DNA characteristics", status = "primary", solidHeader = TRUE, width = 6,
            div(
              fluidRow(
                column(6,
                       radioButtons(
                         "Species",
                         label = "Select the species:",
                         choices = c("Bacteria", "C. elegans", "D. melanogaster", "S. cerevisiae"),
                         selected = "Bacteria"
                       )),
                column(6,
                       style = "margin-left: -100px;",
                       div(
                         conditionalPanel(
                           condition = "input.Conversion_PFM",
                           tags$p(tags$strong("Nucleotide Frequencies:"))), 
                         tableOutput("freq")
                       ))
            
              )
            )
          ),
          box(
            title = "Upload file for regression", status = "primary", solidHeader = TRUE, width = 6,
            tabsetPanel(
              tabPanel(
                "Compendium",
                div(
                  fluidRow(
                    column(6,
                           fileInput("compendium_csv", "Add compendium (.csv) file", multiple = FALSE, accept = c(".csv"))
                    ),
                    column(6,
                           selectInput(
                             "Reduction",
                             label = "Dimensionality reduction method: ",
                             choices = c("PCA", "MDS", "t-SNE", "UMAP"),
                             selected = "PCA"))
                    )
                  ),
                div(
                  fluidRow(
                    column(6,
                           style = "margin-top: 75px;",
                           actionButton("Retrive_metadata", "Retrive metadata", icon = icon("bolt"))
                    ),
                    column(6,
                           textInput("colorby", "Visualize by: " ),
                           actionButton("reduction", "Dimensionality reduction", icon = icon("bolt")))
                    )
                  )
                ),
                tabPanel(
                  "PCA",
                  div(
                    fluidRow(
                      column(6,
                             textInput("percentage_variance","Percentage of variance:")),
                      column(6,
                             radioButtons("scale", "You want scale the data: ",
                                          choices = c("True" = "True", "False" = "False"),
                                          selected = "True")
                             )
                    )
                  ),
                  actionButton("start_pca","Start PCA", icon = icon("bolt"))
                )
              )
            ),
          box(
            title = "Upload files for filtering", id = "filtering_files", status = "primary", solidHeader = TRUE, width = 6,
            fileInput("TUS_file", "Add TUS (.tsv) file", multiple = FALSE, accept = c(".tsv")),
            fileInput("PT_info_file", "Add protein info (.txt) file", multiple = FALSE, accept = c(".txt"))
          )
        ),
        div(
          conditionalPanel(
            condition = "input.Conversion_PFM",
          id = "list_pfms_plot",
          tabsetPanel(
            tabPanel("PFMs data",
                     fluidRow(
                       box(
                         title = "List of PFMs", status = "success", solidHeader = TRUE, width = 3,
                         DTOutput("List_of_PFMs"),
                         id = "list_pfms"
                       ),
                       box(
                         title = "Selected PFM", status = "success", solidHeader = TRUE, width = 4,
                         DTOutput("Selected_PFM"),
                         id = "selected"
                       ),
                       box(
                         title = "Logo", status = "success", solidHeader = TRUE, width = 5,
                         plotOutput("logo_plot"),
                         id = "Logo_plot"
                       )
                     )
              )
            )
          )
        ),
        div(
          conditionalPanel(
            condition = "input.Retrive_metadata",
            tabsetPanel(
              tabPanel("Metadata & Dimensionality reduction",
                       fluidRow(
                         box(
                           title = "Metadata", status = "success", solidHeader = TRUE, width = 6,
                           DTOutput("Metadata_tab")
                         ),
                         box(
                           title = "Dimensionality reduction", status = "success", solidHeader = TRUE, width = 6,
                           plotlyOutput("reduction_plot"),
                           hr(),
                           p("Select in the plot points that you want study: "),
                           uiOutput("selected_points_ui"),
                           tags$script(HTML("
                                $(document).on('click', '#copy_button', function() {
                                var copyText = document.getElementById('selected_ids');
                                var range = document.createRange();
                                range.selectNode(copyText);
                                window.getSelection().removeAllRanges();
                                window.getSelection().addRange(range);
                                document.execCommand('copy');
                                window.getSelection().removeAllRanges();
                              });
                            ")
                           )
                         )
                       )
                  )
                )
              )
            ),
        div(
          conditionalPanel(
            condition = "input.start_pca",
            tabsetPanel(
              tabPanel("PCA result",
                       fluidRow(
                        box(
                          title = "PCA matrix", status = "success", solidHeader = TRUE, width = 12,
                          DTOutput("pca_matrix")
                        )
                      )
                    )
                  )
                )
              )
            ),
      tabItem(
        tabName = "Scoring_Filtering",
        fluidRow(
          column(6,  
                 box(
                   title = "Execute scores", status = "primary", solidHeader = TRUE, width = NULL, 
                   p("Number of motifs: ", strong(textOutput("num_motifs", inline = TRUE))),
                   p("Number of sequences: ", strong(textOutput("num_sequences", inline = TRUE))),
                   p("Total scores: ", strong(textOutput("operations", inline = TRUE))),
                   radioButtons("strand", "On which strand: ",
                                choices = c( "Single (forward)"= "true", "Double (forward and reverse)" = "false"),
                                selected = "true"),
                   div(
                     fluidRow(
                       column(6,
                              textInput("core", "Core number for the calculation: ")
                              ),
                       column(6,
                              actionButton("Start_scoring", "Start scoring", icon = icon("star"), style = "margin-top: 25px;")
                              )
                     )
                   ),
                   p("\n"),
                   conditionalPanel(
                     condition = "input.Start_scoring",
                     p("Running the scoring process..."),
                     p("Please wait ...")
                   )
                 )
          ),
          column(6, 
                 box(
                   title = "Execute filtering", status = "primary", solidHeader = TRUE, width = NULL, 
                   p("Initial number of genes: ",strong(textOutput("num_genes", inline = TRUE))),
                   actionButton("Start_filtering", "Start filtering", icon = icon("star")),
                   p("\n"),
                   conditionalPanel(
                     condition = "input.Start_filtering",
                     p("Final number of genes: ", strong(textOutput("num_genes2", inline = TRUE)))
                   )
                 )
          )
        ),
        fluidRow(
          column(6, 
                 box(
                   title = "Score for all motifs", status = "success", solidHeader = TRUE, width = NULL, 
                   downloadButton(
                     outputId = "downloadData1", 
                     label = "Download",
                     class = "small-download-btn" 
                   ),
                   DTOutput("Scores")
                 )
          ),
          column(6, 
                 box(
                   title = "Score distribution", status = "success", solidHeader = TRUE, width = NULL,
                   plotOutput("Score_plot")
                 )
            )
          )
      ),
      tabItem(
        tabName = "Regression",
        fluidRow(
          box(
            title = "Execute regression", status = "primary", solidHeader = TRUE, width = 6,
            div(
              fluidRow(
                column(6,
                       checkboxGroupInput("regressor_choice", "Choose Regressor:",
                                          choices = c("SVM" = "SVM","KNN" = "KNN", "Randomized Lasso" = "Randomized_lasso","RF" = "RF", "PLS" = "PLS", "O2PLS" = "O2PLS", "LM" = "LM"),
                                          selected = "SVM")),
                column(6,
                       conditionalPanel(
                         condition = "input.start_pca",
                         radioButtons("compe_or_pca", "Select the file for the regression:",
                                      choices = c("Compendium" = "Comp", "PCA data" = "PCA_data"),
                                      selected = "Comp")
                         
                       ))
              )
            ),
            textInput("condition", "Condition(s)"),
            textInput("K", "Choose the top K motifs to consider"),
            div(
              fluidRow(
                column(6,
                       switchInput(
                         inputId = "seed",
                         label = "Reproducible results",  
                         value = FALSE,                  
                         onLabel = "ON",                 
                         offLabel = "OFF",               
                         size = "mini"                  
                       )),
                column(6,
                       style = "margin-left: -100px",
                       actionButton("Start_regression", "Start regression", icon = icon("chart-line"))
                       )
              )
            ),
            hr(),
            p("Select here the condition whose results you want to display"),
            selectInput("condition_result", "Results for the condition:",
                        choices = NULL)
          ),
          conditionalPanel(
            condition = "input.regressor_choice.includes('LM')",
            box(
              title = "Parameter/s for LM regression", status = "primary", solidHeader = TRUE,
            textInput("Alpha", "Alpha:")
            )
          ),
          conditionalPanel(
            condition = "input.regressor_choice.includes('PLS')",
            box(
              title = "Parameter/s for PLS regression", status = "primary", solidHeader = TRUE,
              textInput("Component", "Component:")
            )
          ),
          conditionalPanel(
            condition = "input.regressor_choice.includes('KNN')",
            box(
              title = "Parameter/s for KNN regression" ,status = "primary", solidHeader = TRUE,
              textInput("core_knn", "Select core number"),
              textInput("cv", "Select number of cross validation")
            )
          ),
          conditionalPanel(
            condition = "input.regressor_choice.includes('SVM')",
            box(
            title = "Parameter/s for SVM regression", status = "primary", solidHeader = TRUE,
            selectInput("Kernel", "Select the kernel: ",
                        choices = c("linear", "radial", "polynomial", "sigmoid"),
                        selected = "radial")
            )
          ),
          conditionalPanel(
            condition = "input.regressor_choice.includes('Randomized_lasso')",
            box(
              title = "Parameter/s for Randomized lasso regression", status = "primary", solidHeader = TRUE,
              textInput("cutoff", "Enter the cutoff")
            )
          ),
          conditionalPanel(
            condition = "input.regressor_choice.includes('RF')",
            box(
              title = "Parameter/s for RF regression", status = "primary", solidHeader = TRUE,
              textInput("ntree", "Enter the number of tree"),
              textInput("rand_var", "Enter the random variables as candidates")
            )
          )
        ),
        fluidRow(
          tabsetPanel(
            id = "regression_tabs", type = "tabs",
            tabPanel(
              "SVM",
              conditionalPanel(
                condition = "input.regressor_choice.includes('SVM')",
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("svm_single")
                  ),
                  box(
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData2", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("Weight_tab")
                  ),
                  box(
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData3", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("Metrics")
                  ),
                  box(
                    status = "success", width = 6,
                    plotOutput("Res_plot")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('SVM')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            ),
            tabPanel(
              "KNN",
              conditionalPanel(
                condition = "input.regressor_choice.includes('KNN')",
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("knn_single")
                  ),
                  box(
                    title = "RMSE matrix",
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData4", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("RMSE_data")
                  ),
                  box(
                    title = "R2 matrix",
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData5", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("R2_data")
                  ),
                  box(
                    title = "MAE matrix",
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData6", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("MAE_data")
                  ),
                  box(
                    title = "Importance matrix",
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData7", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("importance_data")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("RMSE_plot")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("R2_plot")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("MAE_plot")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("importance_plot")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('KNN')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            ),
            tabPanel(
              "Randomized Lasso",
              conditionalPanel(
                condition = "input.regressor_choice.includes('Randomized_lasso')",  
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("randomized_lasso_single")
                  ),
                  box(
                    title = "Probability table", status = "primary", width = 6,
                    downloadButton(
                      outputId = "downloadData8", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("prob_tab")
                  ),
                  box(
                    status = "success", width = 6,
                    plotOutput("Lasso_plot")
                  ),
                  box(
                    status = "success", width = 6,
                    plotOutput("Prob_plot")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('Randomized_lasso')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            ),
            tabPanel(
              "RF",
              conditionalPanel(
                condition = "input.regressor_choice.includes('RF')",  
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("rf_single")
                  ),
                  box(
                    title = "Importance matrix",
                    status = "primary", width = 6,
                    downloadButton(
                      outputId = "downloadData9", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("importance_df")
                  ),
                  box(
                    status = "success", width = 6,
                    plotOutput("importance_dotplot")
                  )
                ),
                fluidRow(
                  box(
                    status = "success", width = 6,
                    plotOutput("mse_elbow_plot")
                  ),
                  box(
                    status = "primary", width = 6,
                    plotOutput("percentage_Var_explained_elbow_plot")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('RF')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            ),
            tabPanel(
              "PLS",
              conditionalPanel(
                condition = "input.regressor_choice.includes('PLS')",  
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("pls_single")
                  ),
                  box(
                    title = "Coefficients table", status = "primary", width = 6,
                    downloadButton(
                      outputId = "downloadData10", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("Coefficients")
                  ),
                  box(
                    status = "success", width = 6,
                    plotOutput("Heatmap_correlation")
                  )
                ),
                fluidRow(
                  box(
                    status = "success", width = 6,
                    plotlyOutput("PLS_plot")
                  ),
                  box(
                    status = "success", width = 6,
                    plotOutput("Validation_plot")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('PLS')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            ),
            tabPanel(
              "O2PLS",
              conditionalPanel(
                condition = "input.regressor_choice.includes('O2PLS')",  
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("o2pls_single")
                  ),
                  box(
                    title = "Joint X matrix",
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData11", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("joint_X_loadings")
                  ),
                  box(
                    status = "success", width = 4,
                    plotOutput("joint_X_loadings_dotchart")
                  ),
                  box(
                    status = "primary", width = 5,
                    plotOutput("scatterplot_joint_scores")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('O2PLS')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            ),
            tabPanel(
              "LM",
              conditionalPanel(
                condition = "input.regressor_choice.includes('LM')",  
                fluidRow(
                  box(
                    status = "success", width = 12,
                    plotlyOutput("lm_single")
                  ),
                  box(
                    title = "Coefficients table",
                    status = "primary", width = 3,
                    downloadButton(
                      outputId = "downloadData12", 
                      label = "Download",
                      class = "small-download-btn" 
                    ),
                    DTOutput("Coef_red_tab")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("Res_Fit")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("Q_Q")
                  ),
                  box(
                    status = "success", width = 3,
                    plotOutput("Scale_Loc")
                  )
                ),
                fluidRow(
                  box(
                    status = "success", width = 4,
                    plotOutput("Cooks_dist")
                  ),
                  box(
                    status = "success", width = 4,
                    plotOutput("Res_Lev")
                  ),
                  box(
                    status = "success", width = 4,
                    plotOutput("Cooks_lev")
                  )
                )
              ),
              conditionalPanel(
                condition = "!input.regressor_choice.includes('LM')",
                HTML("<span style='color:red;'>Not runned --> You have not selected this type of regression.<br>Please select it to see the results.</span>")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "Consensus_Clustermap",
        fluidRow(
          box(
            title = "Choose the regression data", status = "primary", width = 12,
            div(
              fluidRow(
                column(2,
                       checkboxGroupInput("choose_data", "Data:",
                                          choices = c("data SVM" = "data_SVM","data_KNN" = "data_KNN", "data Randomized Lasso" = "data_Randomized_lasso", "data_RF" = "data_RF", "data PLS" = "data_PLS", "data_O2PLS" = "data_O2PLS", "data LM" = "data_LM"),
                                          selected = "data_SVM")),
                column(3,
                       style = "margin-left: 100px;",
                       selectInput(
                         "DistanceRow",
                         label = "Row's distance metric: ",
                         choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
                         selected = "euclidean")
                       ),
                column(3,
                       selectInput(
                         "DistanceCol",
                         label = "Column's distance metric: ",
                         choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
                         selected = "euclidean")
                ),
                column(2,
                       style = "margin-left: 80px;",
                       textInput("alpha_fisher", "Define the alpha value: "))
              )
            ),
            div(
              fluidRow(
                column(2,
                       radioButtons("by_regressor", "By regressor: ",
                                    choices = c("YES" = "YES", "NO" = "NO"),
                                    selected = "YES")),
                column(3,
                       style = "margin-left: 100px;",
                       selectInput(
                         "MethodRow",
                         label = "Row's clustering method: ",
                         choices = c("ward.D", "ward.D2", "single", "complete", "average (UPGMA)", "mcquitty (WPGMA)", "median (WPGMC)", "centroid (UPGMC)"),
                         selected = "complete")
                       ),
                column(3,
                       selectInput(
                         "MethodCol",
                         label = "Column's clustering method: ",
                         choices = c("ward.D", "ward.D2", "single", "complete", "average (UPGMA)", "mcquitty (WPGMA)", "median (WPGMC)", "centroid (UPGMC)"),
                         selected = "complete")
                )
              )
            ),
            hr(),
            div(
              fluidRow(
                column(3,
                      actionButton("Perform_consensus", "Perform consensus", icon = icon("project-diagram"))
                      ),
                column(5,
                       style = "margin-left: 180px;",
                       actionButton("Perform_clustermap", "Perform clustermap", icon = icon("project-diagram"))
                       ),
                column(1,
                       style = "margin-left: 30px;",
                       actionButton("Perform_fisher", "Perform fisher", icon = icon("calculator")))
              )
            )
          )
        ),
        tabsetPanel(
          tabPanel(
            "Consensus heatmap",
            box(
              status = "success", width = 12,
              uiOutput("Consensus_heatmap")
            )
          ),
          tabPanel(
            "Clustermap",
            tabsetPanel(
              tabPanel(
                "SVM",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_svm")
                )
              ),
              tabPanel(
                "KNN",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_knn")
                )
              ),
              tabPanel(
                "Randomized lasso",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_rand_lasso")
                )
              ),
              tabPanel(
                "RF",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_rf")
                )
              ),
              tabPanel(
                "PLS",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_pls")
                )
              ),
              tabPanel(
                "O2PLS",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_o2pls")
                )
              ),
              tabPanel(
                "LM",
                box(
                  status = "success", width = 12,
                  uiOutput("Clustermap_lm")
                )
              )
            )
          ),
          tabPanel(
            "Fisher test",
            box(
              status = "success", width = 12,
              uiOutput("fisher_results")
            )
          )
        )
      )
    )
  )
)

options(shiny.maxRequestSize = 50 * 1024^2) 

server <- function(input, output, session) {
  
  PFMs <- reactiveVal(NULL)
  PWMs <- reactiveVal(NULL)
  dataSeq <- reactiveVal(NULL)
  Scores <- reactiveVal(NULL)
  TPM <- reactiveVal(NULL)
  Metadata <- reactiveVal(NULL)
  dim_reduction <- reactiveVal(NULL)
  PCA <- reactiveVal(NULL)
  firstGenes <- reactiveVal(NULL)
  initial_score <- reactiveVal(NULL)
  file_for_regression <- reactiveVal(NULL) 
  set_seed <- reactiveVal(NULL) 
  param_svm <- reactiveVal(NULL)
  Regr_svm <- reactiveVal(NULL)
  df_svm_all <- reactiveVal(NULL)
  param_knn <- reactiveVal(NULL)
  Regr_knn <- reactiveVal(NULL)
  df_knn_all <- reactiveVal(NULL)
  param_rand_lasso <- reactiveVal(NULL)
  Regr_rand_lasso <- reactiveVal(NULL)
  df_rand_lasso_all <- reactiveVal(NULL)
  param_rf <- reactiveVal(NULL)
  Regr_rf <- reactiveVal(NULL)
  df_rf_all <- reactiveVal(NULL)
  param_pls <- reactiveVal(NULL)
  Regr_pls <- reactiveVal(NULL)
  df_pls_all <- reactiveVal(NULL)
  param_o2pls <- reactiveVal(NULL)
  Regr_o2pls <- reactiveVal(NULL)
  df_o2pls_all <- reactiveVal(NULL)
  param_lm <- reactiveVal(NULL)
  Regr_lm <- reactiveVal(NULL)
  df_lm_all <- reactiveVal(NULL)
  consensus <- reactiveVal(NULL)
  distanceMetricsRow <- reactiveVal(NULL)
  clusterMethodRow <- reactiveVal(NULL)
  distanceMetricsCol <- reactiveVal(NULL)
  clusterMethodCol <- reactiveVal(NULL)
  fisher <- reactiveVal(NULL)
  
  observeEvent(input$Github, {
    browseURL("https://github.com/LeonardoNossa/MotifRegressR.git")
  })
  
  observeEvent(input$score_csv, {
    req(input$score_csv)
    shinyjs::hide("list_pfms_plot")
    disable("database_choice")
    disable("Tax_ID")
    disable("meme_file")
    disable("gff_file")
    disable("fna_file")
    disable("upstream")
    disable("Conversion_PFM")
    disable("Start_scoring")
    disable("strand")
    
    csv_data <- read.csv(input$score_csv$datapath, header = TRUE)
    rownames(csv_data) <- csv_data[, 1]
    Scores(csv_data[, -1])
  
    initial_score(nrow(Scores()))
  })
  
  
  observeEvent(input$Conversion_PFM, {
    disable("score_csv")
  })
  
  observeEvent(input$Conversion_PFM, {
    
    if (input$database_choice == "jaspar" && input$Tax_ID != "") {
      PFMs(PFM_loader(path = NULL, tax_id = input$Tax_ID))
    } else if (input$database_choice == "mydata") {
      req(input$meme_file)
      PFMs(PFM_loader(path = input$meme_file$datapath, tax_id = NULL))
    }
    
    req(PFMs(), input$fna_file)
    background_freq <- retrieve_frequencies(input$fna_file$datapath)
    freq_data  <- t(data.frame(background_freq))
    colnames(freq_data) <- c("A", "C", "G", "T")
    rownames(freq_data) <- NULL
    
    output$freq <-  renderTable({
      freq_data
    }, digits = 7)
    
    PWMs(PFM2PWM(PFMs(), background = background_freq))

    
    output$List_of_PFMs <- renderDT({
      data.frame(Motifs = names(PFMs()))
    }, selection = "single")
  
  })
    
  observeEvent(input$List_of_PFMs_rows_selected, {
    req(input$List_of_PFMs_rows_selected)
    motif_name <- names(PFMs())[input$List_of_PFMs_rows_selected]
    output$Selected_PFM <- renderDT({
      datatable(data.frame(PFMs()[[motif_name]]),options = list(scrollX = TRUE))
    })
  })
  
  observeEvent(input$List_of_PFMs_rows_selected, {
    req(input$List_of_PFMs_rows_selected)
    motif_name <- names(PFMs())[input$List_of_PFMs_rows_selected]
    output$logo_plot <- renderPlot(retrieve_logos(PFMs()[motif_name]))
  })
  
  observeEvent(input$compendium_csv, {
    req(input$compendium_csv)
    TPM(compendium_loader(input$compendium_csv$datapath, row.names = 1))
    file_for_regression(TPM())
  })
  
  observeEvent(input$Retrive_metadata, {
    tryCatch({
      Metadata(retrieve_metadata(TPM()))
      output$Metadata_tab <- renderDT(
        datatable(Metadata(), options = list(scrollX = TRUE))
      )
    }, error = function(e) {
      showNotification("No metadata could be fetched!", type = "error")
    })
  })
  
  observeEvent(input$reduction, {
    
    color_by <- if (input$colorby == "") NULL else input$colorby
    
    valid_methods <- c("PCA", "MDS", "t-SNE", "UMAP")
    
    if (input$Reduction %in% valid_methods) {
      dim_reduction(
        plot_dimensionality_reduction(TPM(), Metadata(), color_by, method = input$Reduction)
      )
    } 
    
    output$reduction_plot <- renderPlotly({
      dim_reduction()
    })

    formatted_ids <- reactive({
      if (!is.null(input$selected_run_accession)) {
        paste0(input$selected_run_accession, collapse = ",")
      } else {
        NULL
      }
    })
    
    output$selected_points_ui <- renderUI({
      ids <- formatted_ids()
      if (!is.null(ids)) {
        tagList(
          verbatimTextOutput("selected_ids"),
          actionButton("copy_button", "Copy to Clipboard")
        )
      } else {
        "No points selected"
      }
    })
    
    output$selected_ids <- renderText({
      formatted_ids()
    })
    
  })
  
  observeEvent(input$start_pca, {
    req(input$percentage_variance)
    
    if (input$scale == "True"){
      PCA(principal_comps(as.matrix(TPM()), explained_var = as.numeric(input$percentage_variance), scale = TRUE ))
    } else {
      PCA(principal_comps(as.matrix(TPM()), explained_var = as.numeric(input$percentage_variance), scale = FALSE ))
    }
    output$pca_matrix <- renderDT({
      datatable(PCA(), options = list(scrollX = TRUE))
    })
  })
  
  observeEvent(input$Species, {
    req(input$score_csv)
    
    if (input$Species == "Bacteria"){
      enable("filtering_files")
      enable("Start_filtering")
    } else {
      disable("filtering_files") 
      disable("Start_filtering")
    }
  })
  
  observeEvent(input$Species, {
    req(input$gff_file, input$fna_file, input$upstream)
    
    if (input$Species == "Bacteria") {
      enable("filtering_files") 
      enable("Start_filtering")
      dataSeq(get_sequences(input$gff_file$datapath, input$fna_file$datapath, species = input$Species, upstream = as.numeric(input$upstream)))
    } else {
      disable("filtering_files") 
      disable("Start_filtering")
      dataSeq(get_sequences(input$gff_file$datapath, input$fna_file$datapath, species = input$Species, upstream = as.numeric(input$upstream)))
    }
  })
  
  output$num_motifs <- renderText({ length(PFMs()) })
  output$num_sequences <- renderText({ nrow(dataSeq()) })
  output$operations <- renderText({ length(PWMs()) * nrow(dataSeq()) })
  
  observeEvent(input$Start_scoring, {
    req(PWMs(), dataSeq(), input$core)
    if (input$strand == "true"){
      Scores(scorer(PWMs(), dataSeq(),both = TRUE, workers = as.numeric(input$core)))
    } else if (input$strand == "false"){
      Scores(scorer(PWMs(), dataSeq(),both = FALSE, workers = as.numeric(input$core)))
    }

    initial_score(nrow(Scores()))
  })
  
  output$num_genes <- renderText({ initial_score() })
  
  observeEvent(input$Start_filtering, {
    req(input$TUS_file, input$PT_info_file, Scores(), TPM())
    
    if (input$Species == "Bacteria") {
      firstGenes(table_first_genes(input$TUS_file$datapath, input$PT_info_file$datapath))
      filtered_scores <- MotifRegressR::filter(Scores(), firstGenes())
      filtered_TPM <- MotifRegressR::filter(TPM(), firstGenes())
      Scores(filtered_scores)
      TPM(filtered_TPM)
    }
    output$num_genes2 <- renderText({ nrow(filtered_scores) })
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Scores_data", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Scores(), file)
    })
  
  output$Scores <- renderDT({
    req(Scores())
    datatable(Scores(), options = list(scrollX = TRUE))
  })
  
  output$Score_plot <- renderPlot({
    req(Scores())
    plot(sort(as.matrix(Scores())))
  })
  
  
  observeEvent(input$compe_or_pca, {
    if (input$compe_or_pca == "Comp"){
      file_for_regression(TPM())
    } else {
      file_for_regression(PCA())
    }
  })
  
  observeEvent(input$seed, {
    if (input$seed) {
      set_seed(set.seed(123))
    }
  })
  
  observeEvent(input$Start_regression, {
    req(input$Kernel)
    
    set_seed()
    
    conditions <- unlist(strsplit(input$condition, ","))
    updateSelectInput(session, "condition_result", choices = c(conditions, "All conditions"))
    
    if ("SVM" %in% input$regressor_choice) {
      param_svm(retrieve_params_list(Regression_svm_kernel = input$Kernel))
      Regr_svm(regressor(Scores(), file_for_regression(), conditions, "SVM", param_svm()))
    }
    if ("KNN" %in% input$regressor_choice){
      param_knn(retrieve_params_list(Regression_KNN_num_cores = as.numeric(input$core_knn), Regression_KNN_cv = as.numeric(input$cv)))
      Regr_knn(regressor(Scores(), file_for_regression(), conditions, "KNN", param_knn()))
    }
    if ("Randomized_lasso" %in% input$regressor_choice) {
      param_rand_lasso(retrieve_params_list(Regression_lasso_cutoff = as.numeric(input$cutoff)))
      Regr_rand_lasso(regressor(Scores(), file_for_regression(), conditions, "Randomized_lasso", param_rand_lasso()))
    }
    if ("RF" %in% input$regressor_choice){
      param_rf(retrieve_params_list(Regression_RF_ntree = as.numeric(input$ntree), Regression_RF_mtry = as.numeric(input$rand_var)))
      Regr_rf(regressor(Scores(), file_for_regression(), conditions, "RF", param_rf()))
    }
    if ("PLS" %in% input$regressor_choice) {
      param_pls(retrieve_params_list())
      Regr_pls(regressor(Scores(), file_for_regression(), conditions, "PLS", param_pls()))
    }
    if ("O2PLS" %in% input$regressor_choice) {
      param_o2pls(retrieve_params_list())
      Regr_o2pls(regressor(Scores(), file_for_regression(), conditions, "O2PLS", param_o2pls()))
    }
    if ("LM" %in% input$regressor_choice) {
      param_lm(retrieve_params_list())
      Regr_lm(regressor(Scores(), file_for_regression(), conditions, "LM", param_lm()))
    }
  })
  
  observeEvent(input$condition_result, {
    conditions <- unlist(strsplit(input$condition, ","))
  
    for (cond in conditions){
      if ("SVM" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_svm <- Regr_svm()$SVM[cond]
        data_visualization_svm <- plot_SVM(data_cond_svm, as.data.frame(file_for_regression()), Scores())
        
        df_svm_all(models2dataframe(Regr_svm(), k = as.numeric(input$K)))
        df_svm_split <- list(subset(df_svm_all()$SVM, conditions == input$condition_result))
        names(df_svm_split) <- "SVM"
        
        
        output$svm_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_svm_split)
          })
        
        output$downloadData2 <- downloadHandler(
          filename = function() {
            paste("Importance_data_svm", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_svm$df, file)
          })
        
        output$Weight_tab <- renderDT({
          datatable(data_visualization_svm$df, options = list(scrollX = TRUE))
        })
        
        output$downloadData3 <- downloadHandler(
          filename = function() {
            paste("Metrics_svm", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_svm$metrics, file)
          })
        
        output$Metrics <- renderDT({
          datatable(data_visualization_svm$metrics, options = list(scrollX = TRUE))
        })
        
        output$Res_plot <- renderPlot({
          data_visualization_svm$plot
        })
      }
      if ("KNN" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_knn <- Regr_knn()$KNN[[cond]]
        data_visualization_knn <- plot_KNN(data_cond_knn)
        
        df_knn_all(models2dataframe(Regr_knn(), k = as.numeric(input$K)))
        df_knn_split <- list(subset(df_knn_all()$KNN, conditions == input$condition_result))
        names(df_knn_split) <- "KNN"
        
        output$knn_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_knn_split)
        })
        
        output$downloadData4 <- downloadHandler(
          filename = function() {
            paste("RMSE_knn", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_knn$RMSE_data, file)
          })
        
        output$RMSE_data <- renderDT({
          datatable(data_visualization_knn$RMSE_data, options = list(scrollX = TRUE))
        })
        
        output$downloadData5 <- downloadHandler(
          filename = function() {
            paste("R2_knn", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_knn$R2_data, file)
          })
        
        output$R2_data <- renderDT({
          datatable(data_visualization_knn$R2_data, options = list(scrollX = TRUE))
        })
        
        output$downloadData6 <- downloadHandler(
          filename = function() {
            paste("MAE_knn", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_knn$MAE_data, file)
          })
        
        output$MAE_data <- renderDT({
          datatable(data_visualization_knn$MAE_data, options = list(scrollX = TRUE))
        })
        
        output$downloadData7 <- downloadHandler(
          filename = function() {
            paste("Importance_knn", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_knn$importance_data, file)
          })
        
        output$importance_data <- renderDT({
          datatable(data_visualization_knn$importance_data, options = list(scrollX = TRUE))
        })
        
        output$RMSE_plot <- renderPlot({
          data_visualization_knn$RMSE_plot
        })
        
        output$R2_plot <- renderPlot({
          data_visualization_knn$R2_plot
        })
        
        output$MAE_plot <- renderPlot({
          data_visualization_knn$MAE_plot
        })
        
        output$importance_plot <- renderPlot({
          data_visualization_knn$importance_plot
        })
        
      }
      if ("Randomized_lasso" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_rand_lasso <- Regr_rand_lasso()$Randomized_lasso[[cond]]
        data_visualization_rand_lasso <- plot_RANDLASSO(data_cond_rand_lasso)
        
        df_rand_lasso_all(models2dataframe(Regr_rand_lasso(), k = as.numeric(input$K)))
        
        df_rand_lasso_split <- list(subset(df_rand_lasso_all()$Randomized_lasso, conditions == input$condition_result))
        names(df_rand_lasso_split) <- "Randomized lasso"
        
        output$randomized_lasso_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_rand_lasso_split)
        })
        
        output$downloadData8 <- downloadHandler(
          filename = function() {
            paste("Probabilities_randLasso", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_rand_lasso$prob_df, file)
          })
        
        output$prob_tab <- renderDT({
          datatable(data_visualization_rand_lasso$prob_df, options = list(scrollX = TRUE))
        })
        
        
        output$Lasso_plot <- renderPlot({
          data_visualization_rand_lasso$stability_paths
        })
        
        output$Prob_plot <- renderPlot({
          data_visualization_rand_lasso$selection_prob
        })
      }
      if ("RF" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_rf <- Regr_rf()$RF[[cond]]
        data_visualization_rf <- plot_RF(data_cond_rf)
        
        df_rf_all(models2dataframe(Regr_rf(), k = as.numeric(input$K)))
        
        df_rf_split <- list(subset(df_rf_all()$RF, conditions == input$condition_result))
        names(df_rf_split) <- "RF"
        
        output$rf_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_rf_split)
        })
        
        output$mse_elbow_plot <- renderPlot({
          data_visualization_rf$mse_elbow_plot
        })
        
        output$percentage_Var_explained_elbow_plot <- renderPlot({
          data_visualization_rf$percentage_Var_explained_elbow_plot
        })
        
        output$downloadData9 <- downloadHandler(
          filename = function() {
            paste("Importance_rf", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_rf$importance, file)
          })
        
        output$importance_df <- renderDT({
          datatable(data_visualization_rf$importance, options = list(scrollX = TRUE))
        })
        
        output$importance_dotplot <- renderPlot({
          data_visualization_rf$importance_dotplot
        })
      }
      if ("PLS" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_pls <- Regr_pls()$PLS[[cond]]
        data_visualization_pls <- plot_PLS(data_cond_pls)
        
        df_pls_all(models2dataframe(Regr_pls(), k = as.numeric(input$K)))
        
        df_pls_split <- list(subset(df_pls_all()$PLS, conditions == input$condition_result))
        names(df_pls_split) <- "PLS"
        
        output$pls_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_pls_split)
        })
        
        output$PLS_plot <- renderPlotly({
          data_visualization_pls[[input$Component]]
        })
        
        output$Validation_plot <- renderPlot({
          data_visualization_pls$validation_plot
        })
        
        output$Heatmap_correlation <- renderPlot({
          data_visualization_pls$heatmap
        })
        
        output$downloadData10 <- downloadHandler(
          filename = function() {
            paste("Coefficients_pls", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_pls$coefficients_df, file)
          })
        
        output$Coefficients <- renderDT({
          datatable(data_visualization_pls$coefficients_df, options = list(scrollX = TRUE))
        })
      }
      if ("O2PLS" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_o2pls <- Regr_o2pls()$O2PLS[[cond]]
        data_visualization_o2pls <- plot_O2PLS(data_cond_o2pls)
        
        df_o2pls_all(models2dataframe(Regr_o2pls(), k = as.numeric(input$K)))
        
        df_o2pls_split <- list(subset(df_o2pls_all()$O2PLS, conditions == input$condition_result))
        names(df_o2pls_split) <- "O2PLS"
        
        output$o2pls_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_o2pls_split)
        })
        
        output$downloadData11 <- downloadHandler(
          filename = function() {
            paste("Joint_X_loadings_o2pls", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_o2pls$joint_X_loadings, file)
          })
        
        output$joint_X_loadings <- renderDT({
          datatable(data_visualization_o2pls$joint_X_loadings, options = list(scrollX = TRUE))
        })
        
        output$joint_X_loadings_dotchart <- renderPlot({
          data_visualization_o2pls$joint_X_loadings_dotchart
        })
        
        output$scatterplot_joint_scores <- renderPlot({
          data_visualization_o2pls$scatterplot_joint_scores
        })
      }
      if ("LM" %in% input$regressor_choice && cond %in% input$condition_result){
        
        data_cond_lm <- Regr_lm()$LM[[cond]]
        data_visualization_lm <- plot_LM(data_cond_lm)
        
        df_lm_all(models2dataframe(Regr_lm(), k = as.numeric(input$K), alpha = as.numeric(input$Alpha)))
        
        df_lm_split <- list(subset(df_lm_all()$LM, conditions == input$condition_result))
        names(df_lm_split) <- "LM"
        
        output$lm_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_lm_split)
        })
        
        output$downloadData12 <- downloadHandler(
          filename = function() {
            paste("Coefficients_lm", Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(data_visualization_lm$coefficients_data, file)
          })
        
        output$Coef_red_tab <- renderDT({
          datatable(data_visualization_lm$coefficients_data, options = list(scrollX = TRUE))
        })
        
        output$Res_Fit <- renderPlot({
          data_visualization_lm$residual_vs_fitted_plot
        })
        
        output$Q_Q <- renderPlot({
          data_visualization_lm$qq_plot
        })
        
        output$Scale_Loc <- renderPlot({ 
          data_visualization_lm$scale_location_plot
        })
        
        output$Cooks_dist <- renderPlot({
          data_visualization_lm$cooks_distance_plot
        })
        
        output$Res_Lev <- renderPlot({
          data_visualization_lm$residual_vs_leverage_plot
        })
        
        output$Cooks_lev <- renderPlot({
          data_visualization_lm$cooks_distance_vs_leverage_plot
        })
      }
    }
    if ("SVM" %in% input$regressor_choice){
      if ("All conditions" %in% input$condition_result){
        shinyjs::hide("Weight_tab")
        shinyjs::hide("Res_plot")
        shinyjs::hide("Metrics")
        
        output$svm_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_svm_all())
        })
      } else {
        shinyjs::show("Weight_tab")
        shinyjs::show("Res_plot")
        shinyjs::show("Metrics")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("Weight_tab")
        shinyjs::hide("Res_plot")
        shinyjs::hide("Metrics")
        updateSelectInput(session, "condition_result", choices = conditions)
        
        output$svm_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_svm_all())
        })
      }
    }
    if ("KNN" %in% input$regressor_choice){
      if("All conditions" %in% input$condition_result){
        shinyjs::hide("RMSE_data")
        shinyjs::hide("R2_data")
        shinyjs::hide("MAE_data")
        shinyjs::hide("importance_data")
        shinyjs::hide("RMSE_plot")
        shinyjs::hide("R2_plot")
        shinyjs::hide("MAE_plot")
        shinyjs::hide("importance_plot")
        
        output$knn_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_knn_all())
        })
      } else {
        shinyjs::show("RMSE_data")
        shinyjs::show("R2_data")
        shinyjs::show("MAE_data")
        shinyjs::show("importance_data")
        shinyjs::show("RMSE_plot")
        shinyjs::show("R2_plot")
        shinyjs::show("MAE_plot")
        shinyjs::show("importance_plot")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("RMSE_data")
        shinyjs::hide("R2_data")
        shinyjs::hide("MAE_data")
        shinyjs::hide("importance_data")
        shinyjs::hide("RMSE_plot")
        shinyjs::hide("R2_plot")
        shinyjs::hide("MAE_plot")
        shinyjs::hide("importance_plot")
        
        output$knn_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_knn_all())
        })
      }
    }
    if ("Randomized_lasso" %in% input$regressor_choice){
      if ("All conditions" %in% input$condition_result){
        shinyjs::hide("prob_tab")
        shinyjs::hide("Lasso_plot")
        shinyjs::hide("Prob_plot")
        
        output$randomized_lasso_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_rand_lasso_all())
        })
      } else {
        shinyjs::show("prob_tab")
        shinyjs::show("Lasso_plot")
        shinyjs::show("Prob_plot")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("prob_tab")
        shinyjs::hide("Lasso_plot")
        shinyjs::hide("Prob_plot")
        updateSelectInput(session, "condition_result", choices = conditions)
        
        output$randomized_lasso_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_rand_lasso_all())
        })
      }
    }
    if ("RF" %in% input$regressor_choice){
      if ("All conditions" %in% input$condition_result){
        shinyjs::hide("mse_elbow_plot")
        shinyjs::hide("percentage_Var_explained_elbow_plot")
        shinyjs::hide("importance_df")
        shinyjs::hide("importance_dotplot")
        
        output$rf_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_rf_all())
        })
      } else {
        shinyjs::show("mse_elbow_plot")
        shinyjs::show("percentage_Var_explained_elbow_plot")
        shinyjs::show("importance_df")
        shinyjs::show("importance_dotplot")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("mse_elbow_plot")
        shinyjs::hide("percentage_Var_explained_elbow_plot")
        shinyjs::hide("importance_df")
        shinyjs::hide("importance_dotplot")
        
        output$rf_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_rf_all())
        })
      }
    }
    if ("PLS" %in% input$regressor_choice){
      if ("All conditions" %in% input$condition_result){
        shinyjs::hide("PLS_plot")
        shinyjs::hide("Validation_plot")
        shinyjs::hide("Heatmap_correlation")
        shinyjs::hide("Coefficients")
        
        output$pls_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_pls_all())
        })
      } else {
        shinyjs::show("PLS_plot")
        shinyjs::show("Validation_plot")
        shinyjs::show("Heatmap_correlation")
        shinyjs::show("Coefficients")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("PLS_plot")
        shinyjs::hide("Validation_plot")
        shinyjs::hide("Heatmap_correlation")
        shinyjs::hide("Coefficients")
        updateSelectInput(session, "condition_result", choices = conditions)
        
        output$pls_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_pls_all())
        })
      }
    }
    if ("O2PLS" %in% input$regressor_choice){
      if ("All conditions" %in% input$condition_result){
        shinyjs::hide("joint_X_loadings")
        shinyjs::hide("joint_X_loadings_dotchart")
        shinyjs::hide("scatterplot_joint_scores")
        
        output$o2pls_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_o2pls_all())
        })
      } else {
        shinyjs::show("joint_X_loadings")
        shinyjs::show("joint_X_loadings_dotchart")
        shinyjs::show("scatterplot_joint_scores")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("joint_X_loadings")
        shinyjs::hide("joint_X_loadings_dotchart")
        shinyjs::hide("scatterplot_joint_scores")
        updateSelectInput(session, "condition_result", choices = conditions)
        
        output$o2pls_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_o2pls_all())
        })
      }
    }
    if ("LM" %in% input$regressor_choice){
      if ("All conditions" %in% input$condition_result){
        shinyjs::hide("Coef_red_tab")
        shinyjs::hide("Res_Fit")
        shinyjs::hide("Q_Q")
        shinyjs::hide("Scale_Loc")
        shinyjs::hide("Res_Lev")
        shinyjs::hide("Cooks_dist")
        shinyjs::hide("Cooks_lev")
        
        output$lm_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_lm_all())
        })
      } else {
        shinyjs::show("Coef_red_tab")
        shinyjs::show("Res_Fit")
        shinyjs::show("Q_Q")
        shinyjs::show("Scale_Loc")
        shinyjs::show("Res_Lev")
        shinyjs::show("Cooks_dist")
        shinyjs::show("Cooks_lev")
      }
      if ("all" %in% input$condition && "all" %in% input$condition_result){
        shinyjs::hide("Coef_red_tab")
        shinyjs::hide("Res_Fit")
        shinyjs::hide("Q_Q")
        shinyjs::hide("Scale_Loc")
        shinyjs::hide("Res_Lev")
        shinyjs::hide("Cooks_dist")
        shinyjs::hide("Cooks_lev")
        updateSelectInput(session, "condition_result", choices = conditions)
        
        output$lm_single <- renderPlotly({
          plot_digraph(motif_names = colnames(Scores()), edges = df_lm_all())
        })
      }
    }
  })
  
  observeEvent(input$Perform_consensus, {
    
    selected_data <- c(
      if ("data_SVM" %in% input$choose_data) df_svm_all(),
      if ("data_KNN" %in% input$choose_data) df_knn_all(),
      if ("data_Randomized_lasso" %in% input$choose_data) df_rand_lasso_all(),
      if ("data_RF" %in% input$choose_data) df_rf_all(),
      if ("data_PLS" %in% input$choose_data) df_pls_all(),
      if ("data_O2PLS" %in% input$choose_data) df_o2pls_all(),
      if ("data_LM" %in% input$choose_data) df_lm_all()
    )
    
    if (length(selected_data) > 0) {
      if (input$by_regressor == "YES"){
        consensus(consensus_motifs(selected_data, by_regressor = TRUE))
      } else if (input$by_regressor == "NO"){
        consensus(consensus_motifs(selected_data, by_regressor = FALSE))
      }
      
      output$Consensus_heatmap <- renderUI({
        ht <- consensus_motifs_heatmap(consensus())
        ht <- draw(ht)
        htShiny(ht)
      })
    }
  })
  
  observeEvent(input$Perform_clustermap, {
    
    distanceMetricsRow(switch(input$DistanceRow,
                              "euclidean" = "euclidean",
                              "maximum" = "maximum",
                              "manhattan" = "manhattan",
                              "canberra" = "canberra",
                              "binary" = "binary",
                              "minkowski" = "minkowski",
                              "pearson" = "pearson",
                              "spearman" = "spearman",
                              "kendall" = "kendall"))
    
    distanceMetricsCol(switch(input$DistanceCol,
                              "euclidean" = "euclidean",
                              "maximum" = "maximum",
                              "manhattan" = "manhattan",
                              "canberra" = "canberra",
                              "binary" = "binary",
                              "minkowski" = "minkowski",
                              "pearson" = "pearson",
                              "spearman" = "spearman",
                              "kendall" = "kendall"))
    
    clusterMethodRow(switch(input$MethodRow,
                            "ward.D" = "ward.D",
                            "ward.D2" = "ward.D2",
                            "single" = "single",
                            "complete" = "complete",
                            "average (UPGMA)" = "average",
                            "mcquitty (WPGMA)" = "mcquitty",
                            "median (WPGMC)" = "median",
                            "centroid (UPGMC)" = "centroid"))
    
    clusterMethodCol(switch(input$MethodCol,
                            "ward.D" = "ward.D",
                            "ward.D2" = "ward.D2",
                            "single" = "single",
                            "complete" = "complete",
                            "average (UPGMA)" = "average",
                            "mcquitty (WPGMA)" = "mcquitty",
                            "median (WPGMC)" = "median",
                            "centroid (UPGMC)" = "centroid"))

    if ("data_SVM" %in% input$choose_data){
      output$Clustermap_svm <- renderUI({
        cm_svm <- plot_clustermap(df_svm_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_svm <- draw(cm_svm)
        htShiny(cm_svm)
      })
    }
    if ("data_KNN" %in% input$choose_data){
      output$Clustermap_knn <- renderUI({
        cm_knn <- plot_clustermap(df_knn_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_knn <- draw(cm_knn)
        htShiny(cm_knn)
      })
    }
    if ("data_Randomized_lasso" %in% input$choose_data){
      output$Clustermap_rand_lasso <- renderUI({
        cm_rand_lasso <- plot_clustermap(df_rand_lasso_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_rand_lasso <- draw(cm_rand_lasso)
        htShiny(cm_rand_lasso)
      })
    }
    if ("data_RF" %in% input$choose_data){
      output$Clustermap_rf <- renderUI({
        cm_rf <- plot_clustermap(df_rf_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_rf <- draw(cm_rf)
        htShiny(cm_rf)
      })
    }
    if ("data_PLS" %in% input$choose_data){
      output$Clustermap_pls <- renderUI({
        cm_pls <- plot_clustermap(df_pls_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_pls <- draw(cm_pls)
        htShiny(cm_pls)
      })
    }
    if ("data_O2PLS" %in% input$choose_data){
      output$Clustermap_o2pls <- renderUI({
        cm_o2pls <- plot_clustermap(df_o2pls_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_o2pls <- draw(cm_o2pls)
        htShiny(cm_o2pls)
      })
    }
    if ("data_LM" %in% input$choose_data){
      output$Clustermap_lm <- renderUI({
        cm_lm <- plot_clustermap(df_lm_all(), c(distanceMetricsRow(),distanceMetricsCol()), c(clusterMethodRow(), clusterMethodCol()))
        cm_lm <- draw(cm_lm)
        htShiny(cm_lm)
      })
    }
  })
  
  observeEvent(input$Perform_fisher, {
    fisher(consensus_fisher_test(consensus(), num_of_regressions = length(input$regressor_choice), alpha = as.numeric(input$alpha_fisher)))
   
    output$fisher_results <- renderUI({
      if (!is.null(fisher())) {
        HTML(paste0(
          "<b>Methods:</b> ", fisher()$method, "<br>",
          "<b>Data:</b> ", fisher()$data.name, "<br>",
          "<b>P-value:</b> ", formatC(fisher()$p.value, format = "e", digits = 2), "<br>",
          "<b>Alternative hypothesis:</b> ", fisher()$alternative
        ))
      } else {
        "Risultati non disponibili."
      }
    })
  })
}

shinyApp(ui, server)


