library(shiny)
library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# ✅ DATA CLEANING FUNCTION
# ---------------------------------------------------------
clean_data <- function(df) {
  
  # Remove empty rows and columns
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  df <- df[, colSums(is.na(df)) != nrow(df)]
  
  # Trim whitespace
  df <- as.data.frame(lapply(df, function(x) {
    if (is.character(x)) trimws(x) else x
  }))
  
  # Convert numeric-looking columns to numeric
  df <- df %>% mutate(across(everything(), ~ {
    if (is.character(.x) && all(grepl("^[0-9.]+$", .x) | .x == "")) {
      suppressWarnings(as.numeric(.x))
    } else .x
  }))
  
  # Convert character columns to factors
  df <- df %>% mutate(across(where(is.character), as.factor))
  
  # Remove duplicate rows
  df <- distinct(df)
  
  # Fix column names
  names(df) <- make.names(names(df), unique = TRUE)
  
  return(df)
}

# ---------------------------------------------------------
# ✅ UI
# ---------------------------------------------------------
ui <- fluidPage(
  titlePanel("Auto Data Cleaning + Visualization (R Shiny)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      
      h4("Variable Selection"),
      uiOutput("var_ui"),
      
      selectInput("plot_type", "Choose Plot Type",
                  choices = c(
                    "Histogram" = "hist",
                    "Density Plot" = "density",
                    "Boxplot" = "box",
                    "Bar Chart" = "bar",
                    "Scatter Plot" = "scatter",
                    "Line Plot" = "line",
                    "Correlation Heatmap" = "corr"
                  )
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'hist'",
        sliderInput("bins", "Bins", min = 5, max = 100, value = 30)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cleaned Data Preview", tableOutput("clean_preview")),
        tabPanel("Visualization", plotOutput("plot", height = "500px"))
      )
    )
  )
)

# ---------------------------------------------------------
# ✅ SERVER
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  # Load + Clean Data
  data <- reactive({
    req(input$file)
    raw <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    clean_data(raw)
  })
  
  # Show cleaned data
  output$clean_preview <- renderTable({
    head(data(), 10)
  })
  
  # Dynamic variable selection
  output$var_ui <- renderUI({
    req(data())
    df <- data()
    
    tagList(
      selectInput("xvar", "X Variable", choices = names(df)),
      selectInput("yvar", "Y Variable (optional)", choices = c("None", names(df))),
      selectInput("colorvar", "Color Group (optional)", choices = c("None", names(df)))
    )
  })
  
  # Plotting logic
  output$plot <- renderPlot({
    req(data())
    df <- data()
    
    x <- input$xvar
    y <- if (input$yvar == "None") NULL else input$yvar
    cvar <- if (input$colorvar == "None") NULL else input$colorvar
    
    p <- NULL
    
    # Histogram
    if (input$plot_type == "hist") {
      validate(need(is.numeric(df[[x]]), "Histogram requires numeric X"))
      p <- ggplot(df, aes_string(x = x)) +
        geom_histogram(bins = input$bins, fill = "steelblue", color = "white") +
        theme_minimal()
    }
    
    # Density
    if (input$plot_type == "density") {
      validate(need(is.numeric(df[[x]]), "Density requires numeric X"))
      aes_map <- aes_string(x = x)
      if (!is.null(cvar)) aes_map$colour <- cvar
      p <- ggplot(df, aes_map) + geom_density() + theme_minimal()
    }
    
    # Boxplot
    if (input$plot_type == "box") {
      validate(need(!is.null(y), "Boxplot requires Y"))
      validate(need(is.numeric(df[[y]]), "Y must be numeric"))
      p <- ggplot(df, aes_string(x = x, y = y)) +
        geom_boxplot(fill = "lightblue") + theme_minimal()
    }
    
    # Bar chart
    if (input$plot_type == "bar") {
      p <- ggplot(df, aes_string(x = x)) +
        geom_bar(fill = "steelblue") + theme_minimal()
    }
    
    # Scatter
    if (input$plot_type == "scatter") {
      validate(need(is.numeric(df[[x]]) && is.numeric(df[[y]]),
                    "Scatter requires numeric X and Y"))
      aes_map <- aes_string(x = x, y = y)
      if (!is.null(cvar)) aes_map$colour <- cvar
      p <- ggplot(df, aes_map) + geom_point(alpha = 0.7) + theme_minimal()
    }
    
    # Line plot
    if (input$plot_type == "line") {
      validate(need(!is.null(y), "Line plot requires Y"))
      aes_map <- aes_string(x = x, y = y)
      if (!is.null(cvar)) aes_map$colour <- cvar
      p <- ggplot(df, aes_map) + geom_line() + theme_minimal()
    }
    
    # Correlation heatmap
    if (input$plot_type == "corr") {
      num_df <- df[, sapply(df, is.numeric), drop = FALSE]
      validate(need(ncol(num_df) > 1, "Need at least 2 numeric columns"))
      corr <- cor(num_df, use = "pairwise.complete.obs")
      corr_df <- as.data.frame(as.table(corr))
      
      p <- ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                             midpoint = 0, limit = c(-1, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    p
  })
}

# ---------------------------------------------------------
# ✅ RUN APP
# ---------------------------------------------------------
shinyApp(ui, server)