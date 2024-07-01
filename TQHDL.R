library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(shinythemes)

# Giao diện
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Exploratory Data Analysis (EDA) Web App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      h4("Navigation"),
      navlistPanel(
        "EDA Tabs",
        tabPanel("Data Info", icon = icon("info-circle")),
        tabPanel("Numeric Features", icon = icon("chart-bar")),
        tabPanel("Categorical Features", icon = icon("chart-pie")),
        tabPanel("Numeric & Categorical Features", icon = icon("box")),
        tabPanel("Summary Statistics", icon = icon("table")),
        tabPanel("Correlation Analysis", icon = icon("project-diagram")),
        tabPanel("Scatter Plot", icon = icon("dot-circle")),
        tabPanel("Data Filtering", icon = icon("filter"))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Data Info", 
                 tableOutput("dataInfo"),
                 tableOutput("colInfo")),
        
        tabPanel("Numeric Features", 
                 selectInput("numericFeature", "Choose a Numeric Feature", choices = NA),
                 tableOutput("featureStats"),
                 plotOutput("featureHist")),
        
        tabPanel("Categorical Features", 
                 selectInput("categoricalFeature", "Choose a Categorical Feature", choices = NA),
                 plotOutput("categoricalChart")),
        
        tabPanel("Numeric & Categorical Features",
                 selectInput("numericFeature2", "Choose a Numeric Feature", choices = NA),
                 selectInput("categoricalFeature2", "Choose a Categorical Feature", choices = NA),
                 plotOutput("modelPlots")),
        
        # New Tab: Summary Statistics
        tabPanel("Summary Statistics", 
                 tableOutput("summaryStats")),
        
        # New Tab: Correlation Analysis
        tabPanel("Correlation Analysis", 
                 plotOutput("correlationPlot")),
        
        # New Tab: Scatter Plot
        tabPanel("Scatter Plot", 
                 selectInput("xFeature", "Choose X Feature", choices = NA),
                 selectInput("yFeature", "Choose Y Feature", choices = NA),
                 plotOutput("scatterPlot")),
        
        # New Tab: Data Filtering
        tabPanel("Data Filtering", 
                 selectInput("filterFeature", "Choose a Feature to Filter", choices = NA),
                 uiOutput("filterValueUI"),
                 tableOutput("filteredData"))
      )
    )
  )
)

#-------------------------------------
server <- function(input, output, session) {
  # Reactive expression to read uploaded file
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # Return NULL if no file is uploaded
      return(NULL)
    }
    read_csv(inFile$datapath)
  })
  
  ## Tab 1 ##
  
  # Thông tin dữ liệu đầu ra dưới dạng bảng
  output$dataInfo <- renderTable({
    if(is.null(data())) {
      return(data.frame(Information = "Please upload a CSV file.", Value = NA))
    } else {
      num_rows <- nrow(data())
      num_cols <- ncol(data())
      num_duplicate_rows <- sum(duplicated(data()))
      num_rows_with_na <- sum(apply(data(), 1, function(x) any(is.na(x))))
      num_cols_with_na <- sum(apply(data(), 2, function(x) any(is.na(x))))
      
      info_df <- data.frame(
        Information = c("Number of Rows", "Number of Columns", "Number of Duplicate Rows", 
                        "Number of Rows with Missing Values", "Number of Columns with Missing Values"),
        Value = c(num_rows, num_cols, num_duplicate_rows, num_rows_with_na, num_cols_with_na)
      )
    }
  })
  
  # hiển thị cột và dữ liệu
  output$colInfo <- renderTable({
    if(is.null(data())) {
      return(data.frame(Column = "No data available. Please upload a CSV file.", DataType = NA))
    } else {
      df <- data()  # Get the current data frame
      col_types <- sapply(df, class)  # Get data type of each column
      col_info_df <- data.frame(Column = names(df), DataType = col_types, stringsAsFactors = FALSE)
      return(col_info_df)
    }
  })
  
  
  ## Tab 2 ##
  
  # Tự động cập nhật các lựa chọn cho selectInput dựa trên tệp đã tải lên
  observe({
    df <- data()
    if (is.null(df)) return()
    numericCols <- names(select_if(df, is.numeric))
    updateSelectInput(session, "numericFeature", choices = numericCols)
  })
  
  # Tính toán và hiển thị bảng thống kê cho đối tượng số đã chọn
  output$featureStats <- renderTable({
    df <- data()
    if (is.null(df) || is.null(input$numericFeature)) {
      return(data.frame(Statistic = "Please upload a file and select a feature.", Value = NA))
    }
    feature <- df[[input$numericFeature]]
    
    stats <- data.frame(
      Statistic = c("Number of Unique Values", "Number of Rows with Missing Values",
                    "Number of Rows with 0", "Number of Rows with Negative Values",
                    "Average Value", "Standard Deviation Value", 
                    "Minimum Value", "Maximum Value", "Median Value"),
      Value = c(length(unique(na.omit(feature))),
                sum(is.na(feature)),
                sum(feature == 0),
                sum(feature < 0),
                mean(feature, na.rm = TRUE),
                sd(feature, na.rm = TRUE),
                min(feature, na.rm = TRUE),
                max(feature, na.rm = TRUE),
                median(feature, na.rm = TRUE))
    )   
  }) 
  
  # Tạo và hiển thị biểu đồ cho tính năng số đã chọn
  output$featureHist <- renderPlot({
    df <- data()
    if (is.null(df) || is.null(input$numericFeature)) return()
    feature <- df[[input$numericFeature]]
    
    ggplot(data.frame(Feature = feature), aes(x = Feature)) +
      geom_histogram(bins = 50, fill = "blue", color = "black") +
      labs(x = input$numericFeature, y = "Count") +
      theme_minimal()
  })
  
  ## Tab 3 ##
  
  # Tự động cập nhật các lựa chọn để lựa chọn tính năng phân loại
  observe({
    df <- data()
    if (is.null(df)) return()
    categoricalCols <- names(select_if(df, Negate(is.numeric)))
    updateSelectInput(session, "categoricalFeature", choices = categoricalCols)
  })
  
  # Hiển thị biểu đồ thanh cho tính năng phân loại đã chọn
  output$categoricalChart <- renderPlot({
    df <- data()
    if (is.null(df) || is.null(input$categoricalFeature)) return()
    
    feature <- df[[input$categoricalFeature]]
    feature <- as.character(feature) # Ensure it's treated as character
    
    # Đếm tần số của từng giá trị duy nhất
    freq_table <- table(feature)
    
    # Plot bar chart
    bar_data <- data.frame(Value = names(freq_table), Frequency = as.numeric(freq_table))
    ggplot(bar_data, aes(x = Value, y = Frequency)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = input$categoricalFeature, y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Tab 4 ##
  
  # Tự động cập nhật các lựa chọn để lựa chọn tính năng số và phân loại
  observe({
    df <- data()
    if (is.null(df)) return()
    numericCols <- names(select_if(df, is.numeric))
    updateSelectInput(session, "numericFeature2", choices = numericCols)
    
    categoricalCols <- names(select_if(df, Negate(is.numeric)))
    updateSelectInput(session, "categoricalFeature2", choices = categoricalCols)
  })
  
  # Tạo và hiển thị các ô hộp lồng nhau cho các đối tượng số và phân loại đã chọn
  output$modelPlots <- renderPlot({
    df <- data()
    if (is.null(df) || is.null(input$numericFeature2) || is.null(input$categoricalFeature2)) return()
    
    numericFeature <- df[[input$numericFeature2]]
    categoricalFeature <- df[[input$categoricalFeature2]]
    
    # Tạo khung dữ liệu để vẽ đồ thị
    plot_data <- data.frame(Numeric = numericFeature, Category = categoricalFeature)
    
    # Vẽ các ô hộp lồng nhau
    ggplot(plot_data, aes(x = Category, y = Numeric, fill = Category)) +
      geom_boxplot() +
      labs(x = input$categoricalFeature2, y = input$numericFeature2) +
      theme_minimal()
  })
  
  ## Tab 5: Thống kê tóm tắt ##
  
  output$summaryStats <- renderTable({
    df <- data()
    if (is.null(df)) return(data.frame(Statistic = "No data available", Value = NA))
    summary_df <- summary(df)
    as.data.frame(summary_df)
  })
  
  ## Tab 6: Phân tích tương quan ##
  
  output$correlationPlot <- renderPlot({
    df <- data()
    if (is.null(df)) return()
    
    numericCols <- select_if(df, is.numeric)
    if (ncol(numericCols) < 2) return()
    
    cor_matrix <- cor(numericCols, use = "complete.obs")
    corrplot(cor_matrix, method = "color", tl.cex = 0.8)
  })
  
  ## Tab 7: Biểu đồ phân tán ##
  
  observe({
    df <- data()
    if (is.null(df)) return()
    numericCols <- names(select_if(df, is.numeric))
    updateSelectInput(session, "xFeature", choices = numericCols)
    updateSelectInput(session, "yFeature", choices = numericCols)
  })
  
  output$scatterPlot <- renderPlot({
    df <- data()
    if (is.null(df) || is.null(input$xFeature) || is.null(input$yFeature)) return()
    ggplot(df, aes_string(x = input$xFeature, y = input$yFeature)) +
      geom_point(alpha = 0.7) +
      labs(x = input$xFeature, y = input$yFeature) +
      theme_minimal()
  })
  
  ## Tab 8: Lọc dữ liệu ##
  
  observe({
    df <- data()
    if (is.null(df)) return()
    updateSelectInput(session, "filterFeature", choices = names(df))
  })
  
  output$filterValueUI <- renderUI({
    df <- data()
    if (is.null(df)) return()
    filter_col <- df[[input$filterFeature]]
    if (is.numeric(filter_col)) {
      sliderInput("filterValue", "Filter by Value", min = min(filter_col, na.rm = TRUE), 
                  max = max(filter_col, na.rm = TRUE), value = c(min(filter_col, na.rm = TRUE), max(filter_col, na.rm = TRUE)))
    } else {
      selectInput("filterValue", "Filter by Value", choices = unique(filter_col), selected = unique(filter_col)[1])
    }
  })
  
  output$filteredData <- renderTable({
    df <- data()
    if (is.null(df) || is.null(input$filterValue)) return()
    filter_col <- df[[input$filterFeature]]
    if (is.numeric(filter_col)) {
      filtered_df <- df %>% filter(filter_col >= input$filterValue[1] & filter_col <= input$filterValue[2])
    } else {
      filtered_df <- df %>% filter(filter_col == input$filterValue)
    }
    filtered_df
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
