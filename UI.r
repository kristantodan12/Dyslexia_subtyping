library(networkD3)
library(dplyr)
library(igraph)
library(visNetwork)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(rintrojs)
library(ggplot2)
library(qdapTools)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)

source("helper.r")


# Define UI for application
ui <- shinyUI(navbarPage(

        ##########################
        ### Design Layout ###
        ##########################
        title = div(
            img(src = "LMU Klinikum.jpg", height = "50px", style = "margin-right: 15px;"),
            img(src = "UOL_blue.png", height = "50px", style = "margin-right: 15px;"),
            img(src = "DFG.jpg", height = "50px", style = "margin-right: 15px;"),
            img(src = "metarep.jpg", height = "50px", style = "margin-right: 15px;")
        ),
        id = "navBar",
        theme = "bootstrap.css",
        collapsible = TRUE,
        inverse = TRUE,
        windowTitle = "Interactive App for Mapping Dyslexia Subtyping Methods",
        position = "fixed-top",
        header = tags$style(
            HTML("
            .navbar {
                min-height: 70px; /* Adjust this value to increase the height */
                background-color: #FFFFFF; /* White background color */
                border-bottom: 4px solid #779ECB; /* Blue underline */
            }
            .navbar-brand {
                height: 70px; /* Adjust this value to match the navbar height */
                padding: 10px 15px; /* Adjust padding to center the images vertically */
            }
            .navbar-nav > li > a {
                padding-top: 25px; /* Adjust this value to center the text vertically */
                padding-bottom: 25px; /* Adjust this value to center the text vertically */
                color: #007bff; /* Blue color for the text */
                font-size: 14px; /* Increase font size */
                font-weight: bold; /* Make text bold */
            }
            .navbar-nav > li > a:hover {
                color: grey !important; /* Darker blue color for the text on hover */
            }
            body {
                padding-top: 100px;
            }
            ")
        ),
        tags$head(
            tags$style(HTML('
            .navbar-nav { float: right !important; }
            .hidden-tab { display: none !important; }
            .custom-home-icon {
                cursor: pointer;
                padding: 25px;
                display: flex;
                align-items: center;
            }
            .custom-home-icon i {
                margin-right: 5px;
            }
            .custom-home-icon span {
                font-weight: bold;
                color: dark blue; /* Blue color for the text */
                font-size: 14px; /* Increase font size */
            }
            .navbar-nav > li > a:hover {
                color: grey !important; /* Darker blue color for the text on hover */
            }
            body {
                padding-top: 100px;
            }
            ')),
            tags$script(HTML('
            $(document).ready(function() {
                $(".navbar-nav > li").addClass("hidden-tab");
                $(".navbar-nav > li:last-child").removeClass("hidden-tab");
                
                // Add custom home icon with text
                $(".navbar-nav").prepend(\'<li class="custom-home-icon"><i class="fa fa-home"></i><span>HOME</span></li>\');
                
                // Click event for custom home icon
                $(".custom-home-icon").click(function() {
                var homeTab = $(".navbar-nav > li > a[data-value=\'home\']");
                homeTab.click();
                });
            });
            '))
        ),

        # Add JavaScript to handle delete button clicks
        tags$script(HTML("
            Shiny.addCustomMessageHandler('addDeleteHandler', function(message) {
                $(document).on('click', '.delete', function() {
                    var id = $(this).attr('id');
                    Shiny.setInputValue('delete_row', id, {priority: 'event'});
                });
            });
        ")),

        
        ##########################
        ### Home ###
        ##########################       
        tabPanel("Home", value = "home",  icon = icon("home"),
            shinyjs::useShinyjs(),
            tags$head(tags$script(HTML('
            function fakeClick(tabName) {
                var dropdownList = document.getElementsByTagName("a");
                for (var i = 0; i < dropdownList.length; i++) {
                var link = dropdownList[i];
                if(link.getAttribute("data-value") == tabName) {
                    link.click();
                }
                }
            }

            var sendToShiny = function(label) {
                Shiny.onInputChange("node_clicked", label);
            };
            '))),

            fluidRow(
            align = "left",
            column(
                width = 12,
                wellPanel(
                style = "background-color: #f0f0f0; color: #000; padding: 10px; border: 1px solid #ddd;",
                shiny::tags$h4("The Multiverse of Developmental Dyslexia Subtyping Methods:", style = "color: #000;"),
                shiny::tags$h4("The 'Mapping Dyslexia Subtypes' - MAP-DyS - app for Analytical Decision-Making",
                                style = "color: #0d5f1fce; font-weight: normal;"),
                shiny::tags$h5("This MAP-DyS app allows you to explore the decision steps for subtyping developmental dyslexia based on a systematic review of the literature.",
                                style = "color: #000; font-weight: normal;"),
                shiny::tags$h5("Click on the node below to explore the features of this app:", style = "color: #000;"),
                shiny::HTML("<br>")
                )
            )
            ),

            fluidRow(
            column(8,
                visNetworkOutput("network_home", width = "100%", height = "600px")
            ),
            column(4,
                align = "left",
                uiOutput("node_description")  # Move this line here
            )
            )
        ),


        ##########################
        ### Dataset Exploration ###
        ##########################                 
        tabPanel("Dataset Exploration", value = "DB",
            fluidRow(
                column(12,
                    tabsetPanel(id = "nested_tabs_DB", type = "tabs",
                        tabPanel(
                            "PRISMA Diagram", value = "PRISMA",
                            fluidRow(
                                column(4, 
                                    shiny::HTML("<h5><b>Systematic literature review of articles that conducted the subtyping of developmental dyslexia.</b></h5> 
                                    <b>Literature search:</b> We used four databases (Embase, APA PsycInfo, ERIC, OSF Preprints) to search 
                                    for empirical papers that conducted the subtyping of developmental dyslexia. <br><br> 
                                    <b>Inclusion criteria:</b> We included empirical studies that identified the cognitive profiles, subtypes, or subgroups of readers with 
                                    reading difficulty. The items were screened by two coders independently, where the primary coder (Coder 1) screened all items, whereas 
                                    the subordinate coder (Coder 2) screened 20% of randomly selected items for reliability check.<br><br>
                                    <b>Data extraction:</b> All data related to the publications’ details were coded by one coder. The remaining data related to
                                    the subtyping details were coded by another coder.<br><br></h5>"),
                                ),
                                column(8, 
                                    shiny::HTML("<h3>Prisma Diagram</h3>"),
                                    img(src='PRISMA.png', align = "center", width = "600px", height = "700px")
                                )
                            )
                        ),
                        tabPanel(
                            "List of Included Articles", value = "list_paper",
                            shiny::HTML("<h5><b> Below is the full list of the included papers in our systematic review.</b></h5>"),
                            fluidRow(
                                column(12, DT::dataTableOutput("table_articles"))
                            )
                        ),
                        tabPanel(
                            "List of Variables", value = "list_decisions",
                            shiny::HTML("<h5><b>Below is an overview of all the variables available in our dataset.</b></h5>"),
                            fluidRow(
                                column(12, DT::dataTableOutput("table_decisions"))
                            )
                        ),
                        tabPanel("Create Your Table", value = "YT",
                            shiny::HTML("<h5><b>In this tab, you may create your own subset of the dataset.</b></h5>"),
                                sidebarLayout(
                                    sidebarPanel(
                                        # Description text for the checkboxes
                                        shiny::HTML("<h5>Select <b>one or more variable categories</b> to filter the available variables:</h5>"),
                                        # UI elements for selecting groups in Description
                                        checkboxGroupInput("group_selection", "Select Variable Categories:",
                                            choices = NULL, # Will be updated in server
                                            selected = NULL
                                        ),
                                        # Description text for the dropdown
                                        shiny::HTML("<h5>Select <b>one or more variable categories</b> from the dropdown list:</h5>"),
                                        uiOutput("name_dropdown"), # Dynamic dropdown based on group selection
                                        # Description text for the CLEAR button
                                        shiny::HTML("<h5><b>From the variable list generated on the right, click the red “Delete” 
                                        button if you wish to remove a selected variable. Click the CLEAR button below to reset your selection:</b> </h5>"),
                                        actionButton("clear_button", "DELETE", style = "color: red"),
                                        br(), br(),  # Add more space between buttons
                                        # Description text for the Display Articles button
                                        shiny::HTML("<h5>Click the <b>Generate Table</b> button below to general a table of the filtered articles</h5>"),
                                        actionButton("display_articles_button", "Generate Table"),
                                        br(), br(),  # Add more space between buttons
                                        downloadButton("download_article_table", "Download Article Table"),
                                    ),
                                    mainPanel(
                                        fluidRow(
                                            # column(12, h3("Description Table")),  # Title for the first table
                                            column(12, DTOutput("description_table"))
                                        ),
                                        br(), br(),  # Add more space between the two tables
                                        fluidRow(
                                            # column(12, h3("Article Table")),  # Title for the second table
                                            column(12, div(style = "width: 100%;", DTOutput("article_table")))  # Ensure full width for article output
                                        )
                                    )
                                )
                        ),
                    )
                )
            )
        ),


        ###########################
        ### Multiverse of Decision Steps ###
        ###########################
        tabPanel("Multiverse of Decision Steps", value = "EX",
            tabsetPanel(id = "nested_tabs_EX", type = "tabs",

                        tabPanel("Descriptive Data: Univariate", value = "AU",
                                sidebarLayout(
                                    sidebarPanel(
                                        shiny::HTML("<h5>In this tab, you may explore the descriptive data of each of the variable in our dataset. 
                                        First, select the variable categories of your interest, then select a variable in the dropdown list. 
                                        The frequency distribution plots will be displayed on the right.
                                        <br><br></h5>"),
                                        shiny::HTML("<h5>Select the <b>variable categories</b> to filter the available variables:</h5>"),
                                        checkboxGroupInput("group_selection_analysis", "Select Variable Categories:",
                                            choices = NULL, # Will be updated in server
                                            selected = NULL
                                        ),
                                        shiny::HTML("<h5>Select a <b>variable</b> from the dropdown list:</h5>"),
                                        uiOutput("name_dropdown_analysis") # Dynamic dropdown based on group selection
                                    ),
                                    mainPanel(
                                        uiOutput("univariate_output") # Dynamic output for plot or table
                                    )
                                )
                        ),
                    
                        tabPanel("Descriptive Data: Bivariate", value = "AB",
                            sidebarLayout(
                                sidebarPanel(
                                    shiny::HTML("<h5>In this tab, you may explore the frequency distribution between two of your selected 
                                    variables from our dataset. First, select the variable categories of your interest, 
                                    then select a variable from each dropdown list. A bivariate frequency distribution table will be 
                                    displayed on the left. The values of Variable 1 will be displayed row-wise, whereas the variables of 
                                    Variable 2 will be displayed column-wise.<br><br></h5>"),
                                    selectInput("group_selection1_bivariate", "Select Variable Category 1:",
                                        choices = NULL # Will be updated in server
                                    ),
                                    uiOutput("variable_dropdown1_bivariate"), # Dynamic dropdown based on group selection 1
                                    selectInput("group_selection2_bivariate", "Select Variable Category 2:",
                                        choices = NULL # Will be updated in server
                                    ),
                                    uiOutput("variable_dropdown2_bivariate") # Dynamic dropdown based on group selection 2
                                ),
                                mainPanel(
                                    tableOutput("bivariate_matrix_output") # Output for the bivariate matrix
                                )
                            )
                        ),


                        tabPanel("Subsetting the Multiverse", value = "AD",
                            sidebarLayout(
                                sidebarPanel(
                                    shiny::HTML("<h5>This tab helps you identify the papers which undertook your selected combination 
                                    of variables and options for dyslexia subtyping. To create your combination of variables and options, 
                                    first select a variable category and a variable from the first dropdown list. Then, select an option of 
                                    your chosen variable from the second dropdown list, and click “Add Selection”. For variables where
                                    multiple values were reported by each paper, such as, “Variation in Measure” and “Variation in Outcome”, you may choose to add the individual options one by 
                                    one by clicking “Select Single Value” as the mode. Otherwise, click “Select Value” as the mode to add 
                                    combinations of options under the chosen variable.<br><br></h5>"),
                                    shiny::HTML("<h5>The list of paper(s) that had reported your chosen variable and option will be displayed on the right. 
                                    You may repeat the above steps to add more variables and options to the list and filter the papers that 
                                    undertook your chosen combination of options. Click “Delete Last Row” if you wish to remove the last added 
                                    variable and option from the combination.<br><br></h5>"),
                                    shiny::HTML("<h5>Select the <b>variable categories</b> to filter the available variables:</h5>"),
                                    selectInput("group_selection_categorical", "Select Variable Category:",
                                        choices = NULL # Will be updated in server
                                    ),
                                    shiny::HTML("<h5>Select a <b>variable</b> from the dropdown list:</h5>"),
                                    uiOutput("variable_dropdown_categorical"), # Dynamic dropdown based on group selection
                                    uiOutput("value_selection_mode"), # Dynamic radio buttons for selection mode
                                    uiOutput("value_dropdown_categorical"), # Dynamic dropdown based on variable selection and mode
                                    actionButton("add_selection", "Add Selection"), # Button to add selection to the table
                                    tableOutput("selected_combinations"), # Table to display selected combinations
                                    actionButton("delete_last_row", "Delete Last Row"), # Button to delete the last row
                                ),
                                mainPanel(
                                    DTOutput("filtered_data_output") # Output for the filtered dataframe
                                )
                            )
                        ),


                        tabPanel("Decision Map of Individual Article", value = "IV",
                            sidebarLayout(
                                sidebarPanel(
                                    # Description text for the checkboxes
                                    p("Select the paper to visualize the decision map:"),
                                    selectInput("selected_paper", "Paper", choices = unique(article_data$Paper)),
                                    uiOutput("entry_no_ui")
                                ),
                                mainPanel(
                                    DTOutput("selected_table")  # Table to display selected items
                                    #visNetworkOutput("decision_map")  # Visualization output
                                )
                            )
                        )
                    )
          
        ),

        ###########################
        ###Additional Analyses ###
        ###########################
        tabPanel("Subtype Result Explorer", value = "TvS",
            tabsetPanel(id = "nested_tabs_TvS", type = "tabs",
                tabPanel("Theoretical Models and Subtypes", value = "TvS1",
                    fluidPage(
                    helpText(shiny::HTML("Here is an interactive plot of the number of data entries yielding specific subtypes (grouped by type presented in 
                                            <span style='color:blue;'>blue color</span>) per theoretical model. When you hover over a cell, it will show you the name of the specific subtype, the theoretical model, 
                                            and the corresponding number of data entries in the dataset. When you click the cell, the corresponding paper(s) 
                                            will be shown in the table below the interactive plot.")),
                        plotlyOutput("heatmap1", height = "1200px"),  # Adjust the height
                        DTOutput("filtered_data_theory")  # Ensure the ID matches the server logic
                    )
                ),
                tabPanel("Language and Subtypes", value = "TvS2",  # Corrected value to be unique
                    fluidPage(
                        helpText(shiny::HTML("Here is an interactive plot of the number of data entries yielding specific subtypes (grouped by type presented in 
                                            <span style='color:blue;'>blue color</span>) across the L1 
                                            of the sample of participants. When you hover over a cell, it will show you the name of the specific subtype, 
                                            the L1 of the sample of participants, and the corresponding number of data entries in the dataset. When you 
                                            click the cell, the corresponding paper(s) will be shown in the table below the interactive plot.")),
                        plotlyOutput("heatmap2", height = "1200px"),  # Adjust the height
                        DTOutput("filtered_data_lang")  # Ensure the ID matches the server logic
                    )
                ),
                # tabPanel("Subtype Prevalence", value = "TvS3",
                #     sidebarLayout(
                #         sidebarPanel(
                #             selectInput("prevalenceType", "Select Prevalence Type:", 
                #                         choices = c("Highest Prevalence", "Common Single Subtypes", "Common Subtype Sets")),
                #             width = 3 
                #         ),
                #         mainPanel(
                #             plotlyOutput("prevalencePlot", height = "800px"),
                #             DTOutput("paperTable"),
                #             width = 9 
                #         )
                #     )
                # )
            )
        ),

        ###########################
        ### Project Info ###
        ###########################
        tabPanel("About", value = "about",  icon = icon("info-circle"),
                
                column(1),
                column(10,
                        shiny::HTML("<h3>This project, “The Multiverse of Subtyping Developmental Dyslexia Methods”, is funded by Treasure Box Program for Early Career Researchers,
                        under priority program <a href='https://www.meta-rep.uni-muenchen.de'> META-REP</a> 
                        (SPP 2317).</h3><br>
                        <h2><center>Our team</center></h2><br>")
                ),
                
                    # TEAM BIO
                fluidRow(
                    
                    style = "height:50px;"),
                
                fluidRow(
                    column(12,
                        h3("Principal Investigators:"),
                        p("Anna Yi Leung (University Hospital, Ludwig-Maximilians-University of Munich, Germany)"),
                        p("Dr. Daniel Kristanto (Carl von Ossietzky Universität Oldenburg, Germany)")
                    ),
                    column(12,
                        h3("Collaborators:"),
                        p("Prof. Andrea Hildebrandt (Carl von Ossietzky Universität Oldenburg, Germany)"),
                        p("Dr. Xenia Schmalz (University Hospital, Ludwig-Maximilians-University of Munich, Germany)"),
                        p("Dr. Carsten Gießing (Carl von Ossietzky Universität Oldenburg, Germany)"),
                        p("Prof. John PA Ioannidis (Stanford University, USA)")
                    )
                ),
            
                fluidRow(style = "height:150px;")
        )                     

)
)