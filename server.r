library(shiny)
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
library(tibble)
library(htmlwidgets)
library(ggtext)
library(gridExtra)
library(plotly)
library(gtools)
library(later)
library(data.table)
library(tidyr)
source("helper.r")

# Define the server logic
shinyServer(function(input, output, session) {
    
    # Navbar ------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
    
    # DT Options --------------------------------------------------------------
    options(DT.options = list( lengthMenu = c(10, 20),
                                dom = 'tl'
    )) 

    ###########################
    ### Home ###
    ###########################

    main_nodes <- data.frame(id = 1:4, 
                            label = c("Home", "Dataset Exploration", "Multiverse of Decision Steps", "Subtype Result Explorer"), 
                            value = c(60, 60, 60, 60), 
                            title = "Click to see information", 
                            shape = "dot",
                            color = "#0d5f1fce")
    main_edges <- data.frame(from = c(1, 1, 1), 
                            to = c(2, 3, 4))

    output$network_home <- renderVisNetwork({
        visNetwork(main_nodes, main_edges, width = "100%") %>%
        visEvents(click = "function(properties) {
            var nodeId = properties.nodes[0];
            if(nodeId) {
            var label = this.body.data.nodes.get(nodeId).label;
            Shiny.onInputChange('node_clicked', label);
            }
        }")
    })

    observeEvent(input$node_clicked, {
        if (input$node_clicked == "Dataset Exploration") {
            subnodes <- data.frame(id = 5:8, 
                                    label = c("PRISMA Diagram", "List of Included Articles", "List of Variables", "Create Your Table"), 
                                    value = c(30, 30, 30, 30), 
                                    title = "Click to see information", 
                                    shape = "dot", 
                                    color = "#a2df9d")
            subedges <- data.frame(from = rep(2, 4), 
                                    to = 5:8)
            
            visNetworkProxy("network_home") %>%
                visUpdateNodes(nodes = subnodes) %>%
                visUpdateEdges(edges = subedges)
        } else if (input$node_clicked == "Multiverse of Decision Steps") {
            subnodes <- data.frame(id = 9:12, 
                                label = c("Descriptive Data: Univariate", "Descriptive Data: Bivariate", "Subsetting the Multiverse", "Decision Map of Individual Article"), 
                                value = c(30, 30, 30, 30), 
                                title = "Click to see information", 
                                shape = "dot", 
                                color = "#a2df9d")
            subedges <- data.frame(from = rep(3, 4), 
                                to = 9:12)
            
            visNetworkProxy("network_home") %>%
            visUpdateNodes(nodes = subnodes) %>%
            visUpdateEdges(edges = subedges)
        } else if (input$node_clicked == "Subtype Result Explorer") {
            subnodes <- data.frame(id = 13:14,#15, 
                                label = c("Theoretical Model and Subtypes", "Language and Subtypes"),# "Subtype Prevalence"), 
                                value = c(30, 30),# 30), 
                                title = "Click to see information", 
                                shape = "dot", 
                                color = "#a2df9d")
            subedges <- data.frame(from = rep(4, 2),#3), 
                                to = 13:14)#15)
            
            visNetworkProxy("network_home") %>%
            visUpdateNodes(nodes = subnodes) %>%
            visUpdateEdges(edges = subedges)
        }

        info <- switch(input$node_clicked,
            "Dataset Exploration" = "Check out the dataset of developmental dyslexia subtyping methods that were extracted from the papers included in the systematic literature review.",
            "Multiverse of Decision Steps" = "Explore the descriptive data of the variables, relationships between variables, and the multiverse of decision steps in developmental dyslexia subtyping.",
            "Subtype Result Explorer" = "Check out the overview of the developmental dyslexia subtypes, their prevalence, and their relationships with the theoretical models for subtyping and participants’ language.",
            "PRISMA Diagram" = "Check out the PRISMA diagram for the steps taken to identify the academic papers that applied dyslexia subtyping methods in the systematic literature review.",
            "List of Included Articles" = "Check out the full list of the included papers in our systematic review.",
            "List of Variables" = "Check out the overview of all the variables available in our dataset.",
            "Create Your Table" = "Create your own subset of the dataset.",
            "Descriptive Data: Univariate" = "Explore the descriptive data of each of the variables in the dataset.",
            "Descriptive Data: Bivariate" = "Select and explore the descriptive data between two of the variables in the dataset.",
            "Subsetting the Multiverse" = "Explore the predefined subset of decision steps and options in the multiverse of developmental dyslexia subtyping.",
            "Decision Map of Individual Article" = "Examine the decision map of subtyping dyslexia in a selected individual paper.",
            "Theoretical Model and Subtypes" = "Examine the relationship between the theoretical models for subtyping and the dyslexia subtypes and their prevalence.",
            "Language and Subtypes" = "Examine the relationship between the language of the participants and the respective dyslexia subtypes and their prevalence.",
            #"Subtype Prevalence" = "Examine the prevalence of each of the dyslexia subtypes in the dataset.",
            "Home" =  "Welcome to explore the multiverse of developmental dyslexia subtyping methods!"  # Default to home if no match
        )

        output$node_description <- renderUI({
            wellPanel(
                style = "background-color: #f0f0f0; padding: 10px; border: 1px solid #ddd;",  # Light background with padding and border
                div(style = "font-size: 18px; font-weight: bold;", input$node_clicked),  # Smaller title
                div(style = "font-size: 14px;", info),
                div(style = "margin-top: 20px;", actionButton("go_to_tab", "Go to Tab"))  # Space between text and button
            )
        })
    })

    observeEvent(input$go_to_tab, {
        tab_value <- switch(input$node_clicked,
            "Dataset Exploration" = "DB",
            "Multiverse of Decision Steps" = "EX",
            "Subtype Result Explorer" = "TvS",
            "PRISMA Diagram" = "PRISMA",
            "List of Included Articles" = "list_paper",
            "List of Variables" = "list_decisions",
            "Create Your Table" = "YT",
            "Descriptive Data: Univariate" = "AU",
            "Descriptive Data: Bivariate" = "AB",
            "Subsetting the Multiverse" = "AD",
            "Decision Map of Individual Article" = "IV",
            "Theoretical Model and Subtypes" = "TvS1",
            "Language and Subtypes" = "TvS2",
            #"Subtype Prevalence" = "TvS3",
            "home"  # Default to home if no match
        )
        
        main_tab <- NULL
        nested_tab <- NULL

        # Determine the main tab and nested tab based on tab_value
        if (tab_value %in% c("DB", "PRISMA", "list_paper", "list_decisions", "YT")) {
            main_tab <- "DB"
            nested_tab <- tab_value
        } else if (tab_value %in% c("EX", "AU", "AB", "AD", "IV")) {
            main_tab <- "EX"
            nested_tab <- tab_value
        } else if (tab_value %in% c("TvS", "TvS1", "TvS2", "TvS3")) {
            main_tab <- "TvS"
            nested_tab <- tab_value
        } else {
            main_tab <- tab_value
        }

        # Update the main tabset panel first
        updateTabsetPanel(session, "navBar", selected = main_tab)

        # If there's a nested tab to update, do it after the main tab is selected
        if (!is.null(nested_tab) && nested_tab != main_tab) {
            # Use a small delay to ensure the main tab is updated before the nested tab
            later::later(function() {
                if (nested_tab %in% c("PRISMA", "list_paper", "list_decisions", "YT")) {
                    updateTabsetPanel(session, "nested_tabs_DB", selected = nested_tab)
                } else if (nested_tab %in% c("AU", "AB", "AD", "IV")) {
                    updateTabsetPanel(session, "nested_tabs_EX", selected = nested_tab)
                } else if (nested_tab %in% c("TvS1", "TvS2", "TvS3")) {
                    updateTabsetPanel(session, "nested_tabs_TvS", selected = nested_tab)
                }
            }, delay = 0.1)
        }
    })


    ###########################
    ### Dataset Exploration ###
    ###########################

    ### Output table of articles ###
    output$table_articles <- DT::renderDataTable(
        article_data, escape = FALSE,
        #extensions = "FixedHeader",
        style="bootstrap",
        filter = 'top',
        options = list(
        dom = 'Bfrtip',
        pageLength = 20,
        scrollX=TRUE,
        autoWidth = TRUE,
        paging=TRUE,
        searching=TRUE,
        ordering=TRUE,
        #fixedHeader = TRUE,
         columnDefs = list(
            list(width = '1000px', targets = which(names(article_data) == "Abstract"))  # Set width for Abstract column
            )
        )
    )

    ### Output table of variables/decisions ###
    output$table_decisions <- DT::renderDataTable(
        description, escape = FALSE,
        #extensions = "FixedHeader",
        style="bootstrap",
        filter = 'top',
        options = list(
        dom = 'Bfrtip',
        pageLength = 20,
        scrollX=TRUE,
        autoWidth = TRUE,
        paging=TRUE,
        searching=TRUE,
        ordering=TRUE
        #fixedHeader = TRUE,
        )
    )

    ### Choosing the variables and displaying the descriptions and the article table ###
    # Update the choices for the group selection checkboxGroupInput
    observe({
        updateCheckboxGroupInput(session, "group_selection",
            choices = unique(description$Group)
        )
        updateCheckboxGroupInput(session, "group_selection_analysis",
            choices = unique(description$Group)
        )
    })
    
    # Dynamically generate the dropdown based on selected group
    output$name_dropdown <- renderUI({
        req(input$group_selection)
        selected_groups <- input$group_selection
            filtered_names <- description %>%
            filter(Group %in% selected_groups) %>%
            filter(!str_detect(Name_vis, "\\d")) %>%  # Exclude items containing numbers
            pull(Name_vis)
        selectInput("name_selection", "Select Name:", choices = filtered_names, multiple = TRUE)
    })
    
    # Reactive value to store selected names
    selected_names <- reactiveVal(character(0))
    
    # Update selected names when name_selection changes
    observeEvent(input$name_selection, {
        current_selection <- selected_names()
        new_selection <- input$name_selection
        selected_names(unique(c(current_selection, new_selection)))
    })
    
    # Display the descriptions based on selected names using DT::datatable
    output$description_table <- renderDT({
        req(selected_names())
        selected_descriptions <- description %>% filter(Name_vis %in% selected_names()) %>% select(Name_vis, Description, Data_type)
        colnames(selected_descriptions) <- c("Name of Variables", "Description", "Data Type")
        selected_descriptions <- cbind(selected_descriptions, Delete = sprintf('<button class="btn btn-danger delete" id="%s">Delete</button>', selected_descriptions$`Name of Variables`))
        datatable(selected_descriptions, escape = FALSE, selection = 'none', options = list(dom = 't'))
    }, server = FALSE)
    
    # Clear the main panel when the CLEAR button is clicked
    observeEvent(input$clear_button, {
        updateSelectInput(session, "name_selection", selected = character(0))
        selected_names(character(0))
        output$description_table <- renderDT({ NULL })
        output$article_table <- renderDT({ NULL })  # Clear article output as well
    })
    
    # Ensure the table is updated when the group selection changes
    observeEvent(input$group_selection, {
        updateSelectInput(session, "name_selection", choices = NULL)
    })
    
    # Handle row deletion
    observeEvent(input$delete_row, {
        name_to_delete <- input$delete_row
        current_selection <- selected_names()
        updated_selection <- setdiff(current_selection, name_to_delete)
        selected_names(updated_selection)
    })
    
    # Add JavaScript to handle delete button clicks
    session$sendCustomMessage(type = 'addDeleteHandler', message = list())

    # Handle download button click
    output$download_article_table <- downloadHandler(
        filename = function() {
            paste("article_table-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(article_data, file, row.names = FALSE)
        }
    )

    ### Displaying the articles based on the selected variables ###
    # Display the filtered article data when the Display Articles button is clicked
    observeEvent(input$display_articles_button, {
        req(selected_names())
        selected_names_vis <- selected_names()
        selected_names_or <- description %>% filter(Name_vis %in% selected_names_vis) %>% pull(Name_or)
        filtered_article_data <- article_data[, selected_names_or, drop = FALSE]
        output$article_table <- renderDT({
            datatable(filtered_article_data, escape = FALSE, options = list(pageLength = 10))
        })
    })


    #############################################################
    ###Multiverse of Decision Stepss###
    #############################################################

    ### Descriptive Data (Univariate) ###
    # Update variable dropdown based on selected group
    output$name_dropdown_analysis <- renderUI({
        req(input$group_selection_analysis)
        selected_groups <- input$group_selection_analysis
        filtered_names <- description %>%
            filter(Group %in% selected_groups) %>%
            filter(!str_detect(Name_vis, "\\d")) %>%  # Exclude items containing numbers
            pull(Name_vis)
        selectInput("name_selection_analysis", "Select Variable:", choices = filtered_names)
    })

     observeEvent(input$name_selection_analysis, {
        req(input$name_selection_analysis)
        selected_name_vis <- input$name_selection_analysis
        selected_data_type <- description %>% filter(Name_vis == selected_name_vis) %>% pull(Data_type)
        selected_name_or <- description %>% filter(Name_vis == selected_name_vis) %>% pull(Name_or)
        selected_column_data <- article_data[[selected_name_or]]
        
        output$univariate_output <- renderUI({
            if (selected_data_type == "Categorical_L") {
                    tagList(
                        plotOutput("univariate_plot", width = "100%", height = "500px"),
                        tableOutput("abbreviation_legend")
                    )
            } else if (selected_data_type == "Character") {
                DTOutput("character_table")
            } else {
                 plotOutput("univariate_plot", width = "100%", height = "500px")
            }
        })
        
        if (selected_data_type == "Categorical_L") {
            # Create a mapping of original labels to abbreviated labels
            unique_labels <- unique(article_data[[selected_name_or]])
            abbreviations <- c(LETTERS, letters, as.character(1:100))[1:length(unique_labels)]
            label_mapping <- setNames(abbreviations, unique_labels)

            # Convert the x-axis variable to a factor with levels in alphabetical order
            article_data[[selected_name_or]] <- factor(article_data[[selected_name_or]], levels = names(label_mapping))

            # Define custom colors for "Not reported" and "Not applicable"
            custom_colors <- c("Not reported" = "red", "Not applicable" = "orange", "Other" = "#32964b")

            output$univariate_plot <- renderPlot({
                ggplot(article_data, aes_string(x = selected_name_or)) +
                    geom_bar(stat = "count", aes(fill = article_data[[selected_name_or]]), alpha = 0.7) +
                    scale_x_discrete(labels = label_mapping) +  # Use abbreviated labels
                    scale_fill_manual(
                        values = custom_colors, 
                        na.value = "#32964b",  # Custom colors
                        drop = FALSE,  # Ensure all levels are considered
                        labels = function(x) {
                            # Only show labels for levels that are present in the data
                            present_levels <- intersect(x, c("Not reported", "Not applicable"))
                            setNames(present_levels, present_levels)
                        }
                    ) +
                    labs(title = paste("Bar Plot of", selected_name_vis),
                        x = selected_name_vis, y = "No. of Papers (Frequency Count)") +
                    theme_minimal(base_size = 15) +  # Increase base size for larger axes
                    theme(
                        plot.background = element_rect(fill = "white", color = NA),  # White background
                        panel.background = element_rect(fill = "white", color = NA),  # White panel background
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
                        plot.margin = margin(t = 10, r = 10, b = 100, l = 10),  # Increase bottom margin
                        panel.grid.major = element_line(color = "grey80"),  # Major grid lines
                        panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
                    ) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1))  # Ensure integer y-axis labels
            })
            
            output$abbreviation_legend <- renderTable({
                data.frame(
                    Abbreviation = abbreviations,
                    Original_Label = unique_labels
                ) %>%
                    setNames(c("Abbreviation", selected_name_vis))
            })
        } else if (selected_data_type == "Character") {
            output$character_table <- DT::renderDataTable({
                DT::datatable(
                    article_data %>% select(Paper, selected_name_or),
                    escape = FALSE,
                    style = "bootstrap",
                    filter = 'top',
                    options = list(
                        dom = 'Bfrtip',
                        pageLength = 20,
                        scrollX = TRUE,
                        autoWidth = TRUE,
                        paging = TRUE,
                        searching = TRUE,
                        ordering = TRUE
                    )
                )
            })
        } else if (selected_data_type == "Numeric") {
            custom_colors <- c("Not reported" = "red", "Not applicable" = "orange", "Other" = "#32964b")
            
            # Count occurrences of "Not reported" and "Not applicable"
            not_reported_count <- sum(article_data[[selected_name_or]] == "Not reported", na.rm = TRUE)
            not_applicable_count <- sum(article_data[[selected_name_or]] == "Not applicable", na.rm = TRUE)
            
            # Remove "Not reported" and "Not applicable" entries
            numeric_data <- article_data %>%
                filter(!(.data[[selected_name_or]] %in% c("Not reported", "Not applicable"))) %>%
                mutate_at(vars(selected_name_or), ~ as.numeric(as.character(.)))
            
            # Check if there are any numeric values left
            if (nrow(numeric_data) > 0) {
                # Calculate the number of bins using Sturges' formula
                num_bins <- nclass.Sturges(numeric_data[[selected_name_or]])
                bin_width <- (max(numeric_data[[selected_name_or]], na.rm = TRUE) - min(numeric_data[[selected_name_or]], na.rm = TRUE)) / num_bins
                
                # Determine the maximum y-axis limit for both plots
                max_y <- max(c(not_reported_count, not_applicable_count, hist(numeric_data[[selected_name_or]], breaks = num_bins, plot = FALSE)$counts))
                
                # Create the histogram plot
                p1 <- ggplot(numeric_data, aes_string(x = selected_name_or)) +
                    geom_histogram(aes(y = ..count..), binwidth = bin_width, fill = "#32964b", alpha = 0.7) +
                    labs(title = paste("Histogram of", selected_name_vis),
                        x = selected_name_vis, y = "No. of Papers (Frequency Count)") +
                    #scale_x_continuous(breaks = seq(min(numeric_data[[selected_name_or]], na.rm = TRUE), max(numeric_data[[selected_name_or]], na.rm = TRUE), by = 2)) +  # Ensure integer breaks
                    scale_x_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) + 
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) +
                    theme_minimal(base_size = 15) +  # Increase base size for larger axes
                    theme(
                        plot.background = element_rect(fill = "white", color = NA),  # White background
                        panel.background = element_rect(fill = "white", color = NA),  # White panel background
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
                        plot.margin = margin(t = 10, r = 10, b = 50, l = 10),  # Adjust bottom margin
                        panel.grid = element_blank(),  # Remove grid
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),  # Adjust y-axis title margin
                        panel.grid.major = element_line(color = "grey80"),  # Major grid lines
                        panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
                    ) +
                    ylim(0, max_y)  # Set the same y-axis range
            } else {
                max_y <- max(not_reported_count, not_applicable_count)
                p1 <- NULL
            }
            
            # Create the bar plot for "Not reported" and "Not applicable"
            if (not_reported_count > 0 || not_applicable_count > 0) {
                additional_data <- data.frame(
                    category = factor(c("Not reported", "Not applicable"), levels = c("Not reported", "Not applicable")),
                    count = c(not_reported_count, not_applicable_count)
                )
                
                p2 <- ggplot(additional_data, aes(x = category, y = count, fill = category)) +
                    geom_bar(stat = "identity", alpha = 0.7) +
                    scale_fill_manual(values = custom_colors, drop = FALSE) +
                    labs(title = "Not reported and Not applicable", x = NULL, y = "No. of Papers (Frequency Count)") +
                    theme_minimal(base_size = 15) +
                    theme(
                        plot.background = element_rect(fill = "white", color = NA),  # White background
                        panel.background = element_rect(fill = "white", color = NA),  # White panel background
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
                        plot.margin = margin(t = 10, r = 10, b = 50, l = 10),  # Adjust bottom margin
                        panel.grid = element_blank(),  # Remove grid
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),  # Adjust y-axis title margin
                        panel.grid.major = element_line(color = "grey80"),  # Major grid lines
                        panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
                    ) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1)) + # Ensure integer y-axis labels
                    ylim(0, max_y) # Set the same y-axis range 
                
                # Display the plots side by side
                output$univariate_plot <- renderPlot({
                    if (!is.null(p1)) {
                        grid.arrange(p1, p2, ncol = 2)
                    } else {
                        print(p2)
                    }
                })
            } else {
                output$univariate_plot <- renderPlot({
                    if (!is.null(p1)) {
                        print(p1)
                    } else {
                        # Handle case where there are no numeric values and no "Not reported" or "Not applicable"
                        plot.new()
                        text(0.5, 0.5, "No data available", cex = 1.5)
                    }
                })
            }
        } else { # Categorical or Ordinal
            custom_colors <- c("Not reported" = "red", "Not applicable" = "orange", "Other" = "#32964b")

            output$univariate_plot <- renderPlot({
                ggplot(article_data, aes_string(x = selected_name_or, fill = selected_name_or)) +
                    geom_bar(stat = "count", alpha = 0.7) +
                    scale_fill_manual(
                        values = custom_colors, 
                        na.value = "#32964b",  # Custom colors
                        drop = FALSE,  # Ensure all levels are considered
                        labels = function(x) {
                            # Only show labels for levels that are present in the data
                            present_levels <- intersect(x, c("Not reported", "Not applicable"))
                            setNames(present_levels, present_levels)
                        }
                        ) +
                    labs(title = paste("Bar Plot of", selected_name_vis),
                        x = selected_name_vis, y = "No. of Papers (Frequency Count)") +
                    theme_minimal(base_size = 15) +  # Increase base size for larger axes
                    theme(
                        plot.background = element_rect(fill = "white", color = NA),  # White background
                        panel.background = element_rect(fill = "white", color = NA),  # White panel background
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
                        plot.margin = margin(t = 10, r = 10, b = 100, l = 10),  # Increase bottom margin
                        panel.grid.major = element_line(color = "grey80"),  # Major grid lines
                        panel.grid.minor = element_line(color = "grey90")   # Minor grid lines
                    ) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::number_format(accuracy = 1))  # Ensure integer y-axis labels
            })
        }

        }
     )

    ### Subset Variables ###
    observe({
        updateSelectInput(session, "group_selection_categorical",
            choices = unique(description$Group),
            selected = unique(description$Group)[1]
        )
    })

    # Update variable dropdown based on selected group
    output$variable_dropdown_categorical <- renderUI({
        req(input$group_selection_categorical)
        selected_group <- input$group_selection_categorical

        # Filter description based on selected group
        filtered_description <- description[description$Group == selected_group, ]

        # Get variables with Data_type Categorical and Ordinal
        categorical_ordinal_vars <- filtered_description$Name_vis[
            filtered_description$Data_type %in% c("Categorical", "Categorical_L") &
            !str_detect(filtered_description$Name_vis, "\\d")  # Exclude items containing numbers
        ]

        selectInput("variable_selection_categorical", "Select Variable:",
            choices = categorical_ordinal_vars,
            selected = categorical_ordinal_vars[1]
        )
    })

    # Update value dropdown based on selected variable
    output$value_dropdown_categorical <- renderUI({
        req(input$variable_selection_categorical)
        selected_variable <- input$variable_selection_categorical

        # Get the corresponding Name_or for the selected Name_vis
        selected_name_or <- description$Name_or[description$Name_vis == selected_variable]

        # Get unique values of the selected variable in article_data
        unique_values <- unique(article_data[[selected_name_or]])

        # Check if selection_mode is available and not NULL
        if (!is.null(input$selection_mode) && input$selection_mode == "Select Single Value") {
            # Split unique values by semicolon and get unique values
            unique_values <- unique(unlist(strsplit(unique_values, "; ")))
        }

        selectInput("value_selection_categorical", "Select Option:",
            choices = unique_values,
            selected = unique_values[1]
        )
    })

    # Update radio buttons based on selected variable
    output$value_selection_mode <- renderUI({
        req(input$variable_selection_categorical)
        selected_variable <- input$variable_selection_categorical

        # Get the corresponding Name_or for the selected Name_vis
        selected_name_or <- description$Name_or[description$Name_vis == selected_variable]

        if (grepl("All", selected_name_or)) {
            radioButtons("selection_mode", "Select Mode:",
                choices = c("Select Value", "Select Single Value"),
                selected = "Select Value"
            )
        }
    })

    # Reactive values to store selected combinations
    selected_combinations <- reactiveValues(data = data.frame(Variable = character(), Option = character(), stringsAsFactors = FALSE))

    # Add selection to the table
    observeEvent(input$add_selection, {
        req(input$variable_selection_categorical, input$value_selection_categorical)
        mode <- ifelse(is.null(input$selection_mode), "Select Value", input$selection_mode)
        new_row <- data.frame(Variable = input$variable_selection_categorical, Option = input$value_selection_categorical, Mode = mode, stringsAsFactors = FALSE)
        selected_combinations$data <- rbind(selected_combinations$data, new_row)
    })

    # Delete the last row from the table
    observeEvent(input$delete_last_row, {
        if (nrow(selected_combinations$data) > 0) {
            selected_combinations$data <- selected_combinations$data[-nrow(selected_combinations$data), ]
        }
    })

    # Display selected combinations
    output$selected_combinations <- renderTable({
        selected_combinations$data
    })

    # Filter and display the dataframe based on selected combinations
    output$filtered_data_output <- renderDT({
        req(nrow(selected_combinations$data) > 0)
        filtered_data <- article_data

        for (i in 1:nrow(selected_combinations$data)) {
            variable <- selected_combinations$data$Variable[i]
            option <- selected_combinations$data$Option[i]
            mode <- selected_combinations$data$Mode[i]
            name_or <- description$Name_or[description$Name_vis == variable]

            if (mode == "Select Value") {
                filtered_data <- filtered_data[filtered_data[[name_or]] == option, ]
            } else if (mode == "Select Single Value") {
                filtered_data <- filtered_data[grepl(option, filtered_data[[name_or]]), ]
            }
        }

        # Select specific columns to display
        filtered_data <- filtered_data[, c("Entry_No", "Paper", "Title", "Journal", "Authors", "Year", "DOI")]

        # Render the table as HTML to support clickable links
        datatable(
            filtered_data, escape = FALSE,
            #extensions = "FixedHeader",
            style = "bootstrap",
            filter = 'top',
            options = list(
                dom = 'Bfrtip',
                pageLength = 20,
                scrollX = TRUE,
                autoWidth = TRUE,
                paging = TRUE,
                searching = TRUE,
                ordering = TRUE
                #fixedHeader = TRUE,
            )
        )
    })

    ### Desccripive Data (Bivariate) ###

    observe({
        updateSelectInput(session, "group_selection1_bivariate",
            choices = unique(description$Group),
            selected = unique(description$Group)[1]
        )
        updateSelectInput(session, "group_selection2_bivariate",
            choices = unique(description$Group),
            selected = unique(description$Group)[1]
        )
    })
    
    # Update variable dropdown based on selected group 1
    output$variable_dropdown1_bivariate <- renderUI({
        req(input$group_selection1_bivariate)
        selected_group <- input$group_selection1_bivariate
        
        # Filter description based on selected group
        filtered_description <- description[description$Group == selected_group, ]
        
        # Get variables with Data_type Categorical and Ordinal
        categorical_ordinal_vars <- filtered_description$Name_vis[
            filtered_description$Data_type %in% c("Categorical", "Categorical_L") &
            !str_detect(filtered_description$Name_vis, "\\d")  # Exclude items containing numbers
        ]
        
        selectInput("variable_selection1_bivariate", "Select Variable 1 (Row):",
            choices = categorical_ordinal_vars,
            selected = categorical_ordinal_vars[1]
        )
    })
    
    # Update variable dropdown based on selected group 2
    output$variable_dropdown2_bivariate <- renderUI({
        req(input$group_selection2_bivariate)
        selected_group <- input$group_selection2_bivariate
        
        # Filter description based on selected group
        filtered_description <- description[description$Group == selected_group, ]
        
        # Get variables with Data_type Categorical and Ordinal
        categorical_ordinal_vars <- filtered_description$Name_vis[
            filtered_description$Data_type %in% c("Categorical", "Categorical_L") &
            !str_detect(filtered_description$Name_vis, "\\d")  # Exclude items containing numbers
        ]
        
        selectInput("variable_selection2_bivariate", "Select Variable 2 (Column):",
            choices = categorical_ordinal_vars,
            selected = categorical_ordinal_vars[1]
        )
    })
    
    # Generate and display the bivariate matrix
    output$bivariate_matrix_output <- renderTable({
        req(input$variable_selection1_bivariate, input$variable_selection2_bivariate)
        
        # Get the corresponding Name_or for the selected Name_vis
        selected_name_or1 <- description$Name_or[description$Name_vis == input$variable_selection1_bivariate]
        selected_name_or2 <- description$Name_or[description$Name_vis == input$variable_selection2_bivariate]
        
        # Get unique values for the selected variables
        unique_values1 <- unique(article_data[[selected_name_or1]])
        unique_values2 <- unique(article_data[[selected_name_or2]])
        
        # Initialize the matrix
        matrix_data <- matrix(0, nrow = length(unique_values1), ncol = length(unique_values2),
                            dimnames = list(unique_values1, unique_values2))
        
        # Fill the matrix with counts
        for (i in seq_along(unique_values1)) {
            for (j in seq_along(unique_values2)) {
                val1 <- unique_values1[i]
                val2 <- unique_values2[j]
                # Filter out NA values before counting rows
                filtered_data_bi <- article_data[!is.na(article_data[[selected_name_or1]]) & !is.na(article_data[[selected_name_or2]]), ]
                matrix_data[i, j] <- nrow(filtered_data_bi[filtered_data_bi[[selected_name_or1]] == val1 & filtered_data_bi[[selected_name_or2]] == val2, ])
            }
        }
        
        # Convert matrix to dataframe for display
        matrix_df <- as.data.frame.matrix(matrix_data)
        
        # Ensure row and column names are correctly set
        if (nrow(matrix_df) == 1) {
            rownames(matrix_df) <- unique_values1
        }
        if (ncol(matrix_df) == 1) {
            colnames(matrix_df) <- unique_values2
        }

        # Convert matrix data to integers without commas
        matrix_df[] <- lapply(matrix_df, function(x) as.integer(x))

        # Sort column names using mixedsort if there are multiple columns
        if (ncol(matrix_df) > 1) {
            sorted_colnames <- mixedsort(colnames(matrix_df))
            matrix_df <- matrix_df[, sorted_colnames, drop = FALSE]
        }

        # Sort row names using mixedsort if there are multiple rows
        if (nrow(matrix_df) > 1) {
            sorted_rownames <- mixedsort(rownames(matrix_df))
            matrix_df <- matrix_df[sorted_rownames, , drop = FALSE]
        }
        
        # Return the dataframe to be rendered as a table
        matrix_df
    }, rownames = TRUE)


    ### Decision Map of Individual Article ###
    # Reactive to filter data by selected paper
    paper_data <- reactive({
        data <- article_data[article_data$Paper == input$selected_paper, ]
        data
    })
    
    # Render Entry_No dropdown UI
    output$entry_no_ui <- renderUI({
        data <- paper_data()
        
        # If there are multiple entries, show the dropdown
        if (nrow(data) > 1) {
            selectInput("selected_entry_no", "Select Entry No", 
                        choices = unique(data$Entry_No), 
                        selected = unique(data$Entry_No)[1])
        } else {
            # Return NULL if there's only one entry
            return(NULL)
        }
    })
    
    # Observe changes in selected paper and re-render the dropdown
    observeEvent(input$selected_paper, {
        data <- paper_data()
        
        # Explicitly trigger re-rendering of the Entry_No UI
        output$entry_no_ui <- renderUI({
            if (nrow(data) > 1) {
                selectInput("selected_entry_no", "Select Entry No", 
                            choices = unique(data$Entry_No), 
                            selected = unique(data$Entry_No)[1])
            } else {
                return(NULL)  # Clear the dropdown if single entry
            }
        })
    })
    
    # Reactive expression for the selected entry data
    selected_entry_data <- reactive({
        data <- paper_data()
        if (nrow(data) > 1) {
            req(input$selected_entry_no)  # Ensure input is available
            selected_data <- data[data$Entry_No == input$selected_entry_no, ]
            selected_data
        } else {
            data[1, ]
        }
    })

    # Define unique variables
    unique_vars <- list(
        TheoryName = unique(article_data$TheoryName),
        Subj_Lang = unique(article_data$Subj_Lang),
        Rep_Pop = unique(article_data$Rep_Pop),
        Dys_Severity = unique(article_data$Dys_Severity),
        Comorbidity = unique(article_data$Comorbidity),
        Total_N = unique(article_data$Total_N),
        Deficit_Cutoff_inSD = unique(article_data$Deficit_Cutoff_inSD),
        Normal_Cutoff_inSD = unique(article_data$Normal_Cutoff_inSD),
        Description_CutOff_Criteria = unique(article_data$Description_CutOff_Criteria),
        Software_PreProcess = unique(article_data$Software_PreProcess),
        Sample_ForClustering = unique(article_data$Sample_ForClustering),
        Results_Validated = unique(article_data$Results_Validated),
        Construct_All = unique(article_data$Construct_All),
        Standardised_All = unique(article_data$Standardised_All),
        Indicator_All = unique(article_data$Indicator_All),
        OutlierRemoval_All = unique(article_data$OutlierRemoval_All),
        MissingDataTreat_All = unique(article_data$MissingDataTreat_All),
        UnifiedLabel_All = unique(article_data$UnifiedLabel_All),
        Prevalence_All = unique(article_data$Prevalence_All)
    )

    # Exclude "Not Reported" from unique_vars
    # unique_vars <- lapply(unique_vars, function(x) x[!x %in% c("Not reported")])

    # Determine the maximum number of values in the lists
    max_length <- max(sapply(unique_vars, length))

    # Pad the columns with empty cells to match the maximum length
    padded_unique_vars <- lapply(unique_vars, function(x) {
        length(x) <- max_length
        x[is.na(x)] <- ""
        return(x)
    })
    
    # Convert the padded list to a data frame
    unique_vars_df <- as.data.frame(padded_unique_vars, stringsAsFactors = FALSE)

    # Render table showing unique_vars with highlighted selected items
    output$selected_table <- renderDT({
        data <- selected_entry_data()

        # Ensure only columns from unique_vars are used if they exist in data
        valid_columns <- intersect(names(unique_vars), colnames(data))
        selected_items <- data[, valid_columns, drop = FALSE]

        # Create a datatable
        dt <- datatable(unique_vars_df, escape = FALSE, options = list(dom = 't', pageLength = max_length))

        # Apply custom CSS to highlight the cells corresponding to the selected paper
        for (col in valid_columns) {
            dt <- dt %>%
                formatStyle(
                    columns = col,
                    valueColumns = col,
                    backgroundColor = styleEqual(
                        levels = selected_items[[col]],
                        values = rep("#8fec9f", length(selected_items[[col]]))
                    )
                )
        }

        dt
    })

    # Create and render the network visualization
    # output$decision_map <- renderForceNetwork({
    #     data <- selected_entry_data()

    #     # Create nodes
    #     nodes <- data.frame(id = integer(), label = character(), group = character(), color = character(), size = numeric(), stringsAsFactors = FALSE)
    #     node_id <- 0  # Start at 0 for zero-indexing
        
    #     for (i in seq_along(unique_vars)) {
    #         var_name <- names(unique_vars)[i]
    #         var_items <- unique_vars[[var_name]]
            
    #         # Count occurrences of each item in the corresponding column
    #         item_counts <- sapply(var_items, function(item) {
    #             sum(article_data[[var_name]] == item, na.rm = TRUE)
    #         })
            
    #         var_nodes <- data.frame(
    #             id = node_id:(node_id + length(var_items) - 1),
    #             label = var_items,
    #             group = var_name,
    #             size = item_counts * 2,  # Increase default size
    #             stringsAsFactors = FALSE
    #         )
    #         nodes <- rbind(nodes, var_nodes)
    #         node_id <- node_id + length(var_items)
    #     }

    #     # Ensure only columns from unique_vars are used if they exist in data
    #     valid_columns <- intersect(names(unique_vars), colnames(data))
    #     selected_items <- unlist(data[, valid_columns, drop = FALSE])

    #     nodes$color <- ifelse(nodes$label %in% selected_items, "#064206", "gray")

    #     # Create links only if there are at least 2 nodes
    #     if (nrow(nodes) > 1) {
    #         links <- data.frame(
    #             source = rep(0:(nrow(nodes) - 2), each = 1),  # Use zero-indexing for source
    #             target = 1:(nrow(nodes) - 1)  # Ensure targets are correctly indexed
    #         )
    #     } else {
    #         links <- data.frame(source = integer(0), target = integer(0))  # Keep links empty
    #     }

    #     # Prepare nodes and edges for visNetwork
    #     vis_nodes <- nodes %>%
    #         mutate(title = paste0("Name: ", label, "<br>Count: ", size)) %>%
    #         select(id, label, group, color, size, title)

    #     vis_edges <- links %>%
    #         rename(from = source, to = target)

    #     # Create visNetwork plot
    #     visNetwork(vis_nodes, vis_edges) %>%
    #         visNodes(size = 10) %>%
    #         visEdges(arrows = "to") %>%
    #         visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    #         visInteraction(hover = TRUE)
    # })



    
    #############################################################
    ###Additional Analyses###
    #############################################################
    ### Theory vs Subtype ###
    output$heatmap1 <- renderPlotly({
        # Add the Subtype1_Type column to count_df_theory based on Subtype_UniLabel
        filtered_count_df_theory <- count_df_theory %>%
            filter(Subtype_UniLabel != 'Not applicable') %>%
            mutate(Subtype1_Type = subtype_grouped$Subtype1_Type[match(Subtype_UniLabel, subtype_grouped$Subtype_UniLabel)])

        # Sort Subtype_UniLabel by group and alphabetically within each group
        filtered_count_df_theory <- filtered_count_df_theory %>%
            arrange(Subtype1_Type, Subtype_UniLabel)

        # Create a custom y-axis label with group names and add it to the dataframe
        filtered_count_df_theory <- filtered_count_df_theory %>%
            mutate(GroupedLabel = paste0(
                '<span style="color:blue;">', Subtype1_Type, ':</span> ', Subtype_UniLabel
            ))

        # Extract y-axis categories
        y_categories <- filtered_count_df_theory$GroupedLabel

        # Extract x-axis categories
        x_categories <- sort(unique(filtered_count_df_theory$TheoryName))

        # Create the heatmap
        plot_ly(
            data = filtered_count_df_theory,
            x = ~TheoryName,
            y = ~GroupedLabel,
            z = ~Count,
            type = "heatmap",
            colors = colorRamp(c("white", "#0c6322")),
            text = ~paste("Subtype: ", Subtype_UniLabel, "<br>Theory: ", TheoryName, "<br>Count: ", Count),
            hoverinfo = "text",
            source = "heatmap1",
            colorbar = list(
                title = list(
                    text = "No. of Data<br>Entries",
                    font = list(size = 13, color = "black", family = "Arial", bold = TRUE)
                )
            )
        ) %>%
        layout(
            title = "No. of Data Entries Yielding Specific Subtypes Per Theoretical Model",
            titlefont = list(size = 13, color = "black", family = "Arial", bold = TRUE),
            xaxis = list(
                title = "Theoretical Model",
                titlefont = list(size = 16, color = "black", family = "Arial", bold = TRUE),
                tickangle = 90,
                tickfont = list(size = 11, color = "black", family = "Arial", bold = TRUE),
                categoryorder = "array",
                categoryarray = x_categories,
                tickmode = "array",
                tickvals = x_categories,
                ticktext = x_categories
            ),
            yaxis = list(
                title = "Subtype (Grouped by Type)",
                titlefont = list(size = 16, color = "black", family = "Arial", bold = TRUE),
                tickfont = list(size = 11, color = "black", family = "Arial", bold = TRUE),
                categoryorder = "array",
                categoryarray = y_categories,
                tickmode = "array",
                tickvals = y_categories,
                ticktext = y_categories,
                tickformat = "html"  # Enable HTML formatting for y-axis labels
            ),
            margin = list(l = 300, r = 25, b = 100, t = 25)  # Adjust margins to give space for labels
        )
    })

    output$filtered_data_theory <- renderDT({
        # Get the clicked data from the heatmap
        event_data <- event_data("plotly_click", source = "heatmap1")
        if (is.null(event_data)) {
            return(datatable(data.frame(Message = "Click on a cell to see details."), options = list(dom = 't')))
        }
        
        # Extract the selected theory and subtype from the clicked cell
        selected_theory <- event_data$x
        selected_subtype <- event_data$y
        
        # Remove HTML tags from the selected_subtype
        selected_subtype <- str_replace_all(selected_subtype, "<[^>]+>", "")  # Remove HTML tags
        selected_subtype <- str_split(selected_subtype, ": ", simplify = TRUE)[, 2]  # Extract only the subtype part
        
        # Find the Entry_No in subtype_data that matches the clicked subtype
        matching_entry_nos <- subtype_data %>%
            filter(Subtype_UniLabel == selected_subtype) %>%
            pull(Entry_No)
        
        # Filter article_data based on the matching Entry_No and selected theory
        filtered_data <- article_data %>%
            filter(Entry_No %in% matching_entry_nos & TheoryName == selected_theory) %>%
            select(Paper, DOI, TheoryName, UnifiedLabel_All, Prevalence_All)
        
        # Define custom column names
        custom_colnames <- c("Paper", "DOI", "Theory Name", "Subtypes (All)", "Prevalence (All)")
        setnames(filtered_data, old = names(filtered_data), new = custom_colnames)
        
        # Render the filtered data as a datatable
        datatable(filtered_data, escape = FALSE,
            style = "bootstrap",
            filter = 'top',
            options = list(
                dom = 'Bfrtip',
                pageLength = 20,
                scrollX = TRUE,
                searching = TRUE,
                ordering = TRUE
            )
        )
    })


    ### Language vs Subtype ###
    output$heatmap2 <- renderPlotly({
        # Add the Subtype1_Type column to count_df_language based on Subtype_UniLabel
        filtered_count_df_language <- count_df_language %>%
            filter(Subtype_UniLabel != 'Not applicable') %>%
            mutate(Subtype1_Type = subtype_grouped$Subtype1_Type[match(Subtype_UniLabel, subtype_grouped$Subtype_UniLabel)])

        # Sort Subtype_UniLabel by group and alphabetically within each group
        filtered_count_df_language <- filtered_count_df_language %>%
            arrange(Subtype1_Type, Subtype_UniLabel)

        # Create a custom y-axis label with group names and add it to the dataframe
        filtered_count_df_language <- filtered_count_df_language %>%
            mutate(GroupedLabel = paste0(
                '<span style="color:blue;">', Subtype1_Type, ':</span> ', Subtype_UniLabel
            ))

        # Extract y-axis categories
        y_categories <- filtered_count_df_language$GroupedLabel

        # Extract x-axis categories
        x_categories <- sort(unique(filtered_count_df_language$Subj_Lang))

        # Create the heatmap
        plot_ly(
            data = filtered_count_df_language,
            x = ~Subj_Lang,
            y = ~GroupedLabel,
            z = ~Count,
            type = "heatmap",
            colors = colorRamp(c("white", "#0c6322")),
            text = ~paste("Subtype: ", Subtype_UniLabel, "<br>Language: ", Subj_Lang, "<br>Count: ", Count),
            hoverinfo = "text",
            source = "heatmap2",
            colorbar = list(
                title = list(
                    text = "No. of Data<br>Entries",
                    font = list(size = 13, color = "black", family = "Arial", bold = TRUE)
                )
            )
        ) %>%
        layout(
            title = "No. of Data Entries Yielding Specific Subtypes Across L1 of Participants",
            titlefont = list(size = 13, color = "black", family = "Arial", bold = TRUE),
            xaxis = list(
                title = "L1 of Sample of Participants",
                titlefont = list(size = 16, color = "black", family = "Arial", bold = TRUE),
                tickangle = 90,
                tickfont = list(size = 11, color = "black", family = "Arial", bold = TRUE),
                categoryorder = "array",
                categoryarray = x_categories,
                tickmode = "array",
                tickvals = x_categories,
                ticktext = x_categories
            ),
            yaxis = list(
                title = "Subtype (Grouped by Type)",
                titlefont = list(size = 16, color = "black", family = "Arial", bold = TRUE),
                tickfont = list(size = 11, color = "black", family = "Arial", bold = TRUE),
                categoryorder = "array",
                categoryarray = y_categories,
                tickmode = "array",
                tickvals = y_categories,
                ticktext = y_categories,
                tickformat = "html"  # Enable HTML formatting for y-axis labels
            ),
            height = 1200,
            margin = list(l = 300, r = 25, b = 100, t = 25)  # Adjust margins to give space for labels
        )
    })

    output$filtered_data_lang <- renderDT({
        # Get the clicked data from the heatmap
        event_data <- event_data("plotly_click", source = "heatmap2")
        if (is.null(event_data)) {
            return(datatable(data.frame(Message = "Click on a cell to see details."), options = list(dom = 't')))
        }
        
        # Extract the selected language and subtype from the clicked cell
        selected_language <- event_data$x
        selected_subtype <- event_data$y

        # Remove HTML tags from the selected_subtype
        selected_subtype <- str_replace_all(selected_subtype, "<[^>]+>", "")  # Remove HTML tags
        selected_subtype <- str_split(selected_subtype, ": ", simplify = TRUE)[, 2]  # Extract only the subtype part
        
        # Find the Entry_No in subtype_data that matches the clicked subtype
        matching_entry_nos <- subtype_data %>%
            filter(Subtype_UniLabel == selected_subtype) %>%
            pull(Entry_No)
        
        # Filter article_data based on the matching Entry_No and selected language
        filtered_data <- article_data %>%
            filter(Entry_No %in% matching_entry_nos & Subj_Lang == selected_language) %>%
            select(Paper, DOI, TheoryName, UnifiedLabel_All, Prevalence_All)
        
        # Define custom column names
        custom_colnames <- c("Paper", "DOI", "Theory Name", "Subtypes (All)", "Prevalence (All)")
        setnames(filtered_data, old = names(filtered_data), new = custom_colnames)
        
        # Render the filtered data as a datatable
        datatable(filtered_data, escape = FALSE,
            style = "bootstrap",
            filter = 'top',
            options = list(
                dom = 'Bfrtip',
                pageLength = 20,
                scrollX = TRUE,
                searching = TRUE,
                ordering = TRUE
            )
        )
    })

    ### Subtype Prevalence ###
    article_data_long <- reactive({
        article_data %>%
            rowwise() %>%
            mutate(UnifiedLabel_All = strsplit(as.character(UnifiedLabel_All), "; "),
                Prevalence_All = strsplit(as.character(Prevalence_All), "; ")) %>%
            unnest(cols = c(UnifiedLabel_All, Prevalence_All)) %>%
            mutate(Prevalence_All = as.numeric(Prevalence_All))
    })

    # Subtype with highest prevalence
    output$prevalencePlot <- renderPlotly({
        data <- article_data_long()
        
        # Function to wrap text
        wrap_text <- function(text, width = 20) {
            str_wrap(text, width = width)
        }
        
        # Dynamic filtering based on user selection
        if (input$prevalenceType == "Highest Prevalence") {
            plot_data <- data %>%
                group_by(UnifiedLabel_All) %>%
                summarise(mean_prevalence = mean(Prevalence_All)) %>%
                arrange(desc(mean_prevalence))
            
            plot_data$UnifiedLabel_All <- wrap_text(plot_data$UnifiedLabel_All)
            
            p <- ggplot(plot_data, aes(x = reorder(UnifiedLabel_All, mean_prevalence), y = mean_prevalence, text = paste("Subtypes:", UnifiedLabel_All, "<br>Mean Prevalence:", mean_prevalence))) +
                geom_col(fill = "darkgreen") +
                coord_flip() +
                labs(title = "Subtype with Highest Prevalence", x = "Subtypes", y = "Mean Prevalence") +
                theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
            
        } else if (input$prevalenceType == "Common Single Subtypes") {
            plot_data <- data %>%
                group_by(UnifiedLabel_All) %>%
                summarise(count = n()) %>%
                arrange(desc(count))
            
            plot_data$UnifiedLabel_All <- wrap_text(plot_data$UnifiedLabel_All)
            
            p <- ggplot(plot_data, aes(x = reorder(UnifiedLabel_All, count), y = count, text = paste("Subtypes:", UnifiedLabel_All, "<br>Count:", count))) +
                geom_col(fill = "darkgreen") +
                coord_flip() +
                labs(title = "Most Common Single Subtypes", x = "Subtypes", y = "Count") +
                theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
            
        } else if (input$prevalenceType == "Common Subtype Sets") {
            plot_data <- article_data %>%
                group_by(UnifiedLabel_All) %>%
                summarise(count = n(), avg_prevalence = mean(as.numeric(Prevalence_All))) %>%
                arrange(desc(count))
            
            plot_data$UnifiedLabel_All <- wrap_text(plot_data$UnifiedLabel_All)
            
            p <- ggplot(plot_data, aes(x = reorder(UnifiedLabel_All, count), y = count, text = paste("Subtype Set:", UnifiedLabel_All, "<br>Count:", count))) +
                geom_col(fill = "darkgreen") +
                coord_flip() +
                labs(title = "Most Common Subtype Sets", x = "Subtype Set", y = "Count", size = "Count") +
                theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        }
        
        # Calculate the required height based on the number of labels and their length
        ggplotly(p, tooltip = "text")
    })

    # # Papers using highest prevalence subtypes
    # output$paperTable <- renderDT({
    #     data <- article_data_long()
        
    #     # Get the selected data from the plotly plot
    #     selected_data <- event_data("plotly_click")
        
    #     if (is.null(selected_data)) {
    #         return(NULL)
    #     }
        
    #     selected_subtype <- selected_data$x
        
    #     filtered_data <- article_data_long %>%
    #         filter(str_detect(UnifiedLabel_All, selected_subtype)) %>%
    #         select(Paper, DOI) %>%
    #         distinct()
        
    #     datatable(filtered_data, escape = FALSE, options = list(pageLength = 10))
    # })

    
})