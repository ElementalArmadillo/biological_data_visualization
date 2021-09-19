library(shiny)
library(tidyverse)

# Initialize data and some variables for later use
data <- read.table("Metadata_structure_sample.txt", header = T)
data <- subset(data, select = -c(`X.`))
cols <- colnames(data)
X_cols <- cols[6:9]
Y_dropdown_cols <- tail(cols,-19)
ab_filters <- unique(data[c("AB")])$AB
ab_filters <- append(ab_filters, "No Filter", 0)
major_col_name <- cols[18]
minor_col_name <- cols[19]

ui <- fluidPage(h1("INSERT TITLE HERE"), # Change the title
                tabsetPanel(
                  # Tab 1: Y = MajorSatellite...
                  tabPanel(title = "Major Satellite Data",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("x_major", "X-axis:",
                                           choices = X_cols),
                               selectInput("ab_filter_major", "Filter Antibodies:",
                                           choices = ab_filters)
                             ),
                             mainPanel(plotOutput("major", height = "700px"))
                           )),
                  # Tab 2: Y = MinorSatellite...
                  tabPanel(title = "Minor Satellite Data",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("x_minor", "X-axis:",
                                           choices = X_cols),
                               selectInput("ab_filter_minor", "Filter Antibodies:",
                                           choices = ab_filters)
                             ),
                             mainPanel(plotOutput("minor", height = "700px"))
                           )),
                  # Tab 3: Y = DF000...
                  tabPanel(title = "DF000....",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("x_df", "X-axis:",
                                           choices = X_cols),
                               selectInput("ab_filter_df", "Filter Antibodies:",
                                           choices = ab_filters),
                               # Choose Y-axis
                               selectizeInput("y_df", "Y-axis:",
                                              choices = NULL)),
                             mainPanel(plotOutput("df", height = "700px"))
                           ))
                ))

server <- function(input, output, session) {
  updateSelectizeInput(session, "y_df", choices = Y_dropdown_cols, server = TRUE)
  
  # Tab 1 plot
  output$major <- renderPlot({
    # Filter data
    if (input$ab_filter_major == "No Filter") {
      plot_data_major <- data
    } else {
      plot_data_major <- data %>% filter(AB == input$ab_filter_major)
    }
    # Plot
    ggplot(plot_data_major, aes(
      x = get(input$x_major),
      y = get(major_col_name),
      fill = get(input$x_major)
    )) +
      geom_point(alpha = 0.4) +
      geom_boxplot(alpha = 0.3) +
      xlab(input$x_major) +
      ylab(major_col_name) +
      facet_grid(cols = vars(`Project.`)) +
      theme(
        legend.title = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 11),
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
  })
  
  # Tab 2 plot
  output$minor <- renderPlot({
    # Filter data
    if (input$ab_filter_minor == "No Filter") {
      plot_data_minor <- data
    } else {
      plot_data_minor <- data %>% filter(AB == input$ab_filter_minor)
    }
    # Plot
    ggplot(plot_data_minor, aes(
      x = get(input$x_minor),
      y = get(minor_col_name),
      fill = get(input$x_minor)
    )) +
      geom_point(alpha = 0.4) +
      geom_boxplot(alpha = 0.3) +
      xlab(input$x_minor) +
      ylab(minor_col_name) +
      facet_grid(cols = vars(`Project.`)) +
      theme(
        legend.title = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 11),
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
  })
  
  # Tab 3 plot
  output$df <- renderPlot({
    # Filter data
    if (input$ab_filter_df == "No Filter") {
      plot_data_df <- data
    } else {
      plot_data_df <- data %>% filter(AB == input$ab_filter_df)
    }
    # Plot
    ggplot(plot_data_df, aes(
      x = get(input$x_df),
      y = get(input$y_df),
      fill = get(input$x_df)
    )) +
      geom_point(alpha = 0.4) +
      geom_boxplot(alpha = 0.3) +
      xlab(input$x_df) +
      ylab(input$y_df) +
      facet_grid(cols = vars(`Project.`)) +
      theme(
        legend.title = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 11),
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
  })
}

shinyApp(ui = ui, server = server)