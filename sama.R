library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(sodium)
library(tibble)

# Your login data
user_base <- tibble::tibble(
  user = c("super_admin", "admin"),
  password = sapply(c("super_admin", "admin"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

# Load your CSV file
employee_data <- read.csv('./daat.csv')
case_study_2_data <- read.csv('./daats.csv')

# Define UI
ui <- fluidPage(
  # Logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # Login section
  shinyauthr::loginUI(id = "login"),
  
  titlePanel(uiOutput("dashboard_title")),
  
  # Add a sidebar for case study selection
  sidebarLayout(
    sidebarPanel(
      # Show selectInput only to logged-in users
      uiOutput("case_study_sidebar")
    ),
    mainPanel(
      uiOutput("main_content")
    )
  )
)

server <- function(input, output, session) {
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Reactive variable to track login status
  logged_in <- reactiveVal(FALSE)
  
  observe({
    # Update the login status
    logged_in(credentials()$user_auth)
    
    # Show/hide the sidebar based on login status
    if (logged_in()) {
      shinyjs::enable("case_study_sidebar")
    } 
  })
  
  selected_case_study <- reactive({
    input$case_study
  })
  
  output$dashboard_title <- renderUI({
    if (logged_in()) {
      titlePanel("Employee Performance Analysis")
    }
  })
  
  output$case_study_sidebar <- renderUI({
    if (logged_in()) {
      selectInput("case_study", "Select Case Study", c("Case Study 1", "Case Study 2"))
    } else {
      # If not logged in, return NULL (no sidebar)
      NULL
    }
  })
  required_columns <- c("Sub_project_ID", "Project_Required_Head_Count", "Total_Available_Headcount")
  missing_columns <- setdiff(required_columns, names(case_study_2_data))
  
  if (length(missing_columns) > 0) {
    stop(paste("Missing columns in case_study_2_data: ", paste(missing_columns, collapse = ", ")))
  }
  
  # Calculate the shortfall or surplus for each sub-project
  case_study_2_data$Shortfall_Surplus <- case_study_2_data$Total_Available_Headcount - case_study_2_data$Project_Required_Head_Count
  
  # Provide recommendations
  case_study_2_data$Advice <- ifelse(case_study_2_data$Shortfall_Surplus >= 0, 
                                     "The available headcount is sufficient for the project requirements.",
                                     "Allocate additional resources to meet the required headcount.")
  
  output$main_content <- renderUI({
    if (logged_in()) {
      fluidPage(
        titlePanel("Employee Performance Analysis"),
        mainPanel(
          fluidRow(
            column(12,
                   switch(selected_case_study(),
                          "Case Study 1" = tabsetPanel(
                            tabPanel("Summary Table", DTOutput("summary_table")),
                            tabPanel("Productivity", 
                                     plotlyOutput("productivity_violin"),
                                     textOutput("summary_text")
                            ),
                            tabPanel("Quality", 
                                     plotlyOutput("quality_violin"),
                                     textOutput("summarys_text")
                            ),
                            tabPanel("Attendance", 
                                     plotlyOutput("attendance_violin"),
                                     textOutput("summaryc_text")
                            ),
                            tabPanel("Conclusion", textOutput("conclusion_text")
                            )
                          ),
                          "Case Study 2" = tabsetPanel(
                            tabPanel("Capacity Planning", DTOutput("table")),
                
                          )
                   )
            )
          )
        )
      )
    }
  })
  
  output$summary_table <- renderDT({
    validate(
      need("Employee.No" %in% names(employee_data), "Employee.No column not found in employee_data"),
      need("Project" %in% names(employee_data), "Project column not found in employee_data"),
      need("Productivity_Score" %in% names(employee_data), "Productivity_Score column not found in employee_data"),
      need("Quality_Score" %in% names(employee_data), "Quality_Score column not found in employee_data"),
      need("Attendance_Score" %in% names(employee_data), "Attendance_Score column not found in employee_data")
    )
    
    datatable(employee_data[, c("Employee.No", "Project", "Productivity_Score", "Quality_Score", "Attendance_Score")],
              options = list(pageLength = 7),
              rownames = FALSE)
  })
  
  output$productivity_violin <- renderPlotly({
    validate(need("Project" %in% names(employee_data), "Project column not found in employee_data"))
    
    plot_ly(employee_data, x = ~Project, y = ~Productivity_Score, type = 'violin', box = list(visible = TRUE)) %>%
      layout(title = "Productivity Score Distribution by Project")
  })
  
  output$summary_text <- renderText({
    conclusion <- " "
    
    conclusion <- paste0(
      conclusion,
      "- The median productivity score for most employees is concentrated around 1.2 to 1.5, indicating a relatively consistent performance level among the majority of staff.\n",
      "- Project 'IIII' stands out with a significantly lower median productivity score compared to other projects. This suggests potential challenges or inefficiencies that may require attention and optimization.\n",
      "- Distinct outliers are observed in projects 'UPPP','HHHH' and 'IIII,' indicating variations in individual performance within these projects. These variations may warrant further investigation to understand the factors influencing such differences.\n",
      "\n Based on these findings, targeted strategies can be implemented to address challenges in Project 'IIII' and to provide additional support or training for individuals in projects 'UPPP','HHHH' and 'IIII' experiencing performance variations."
    )
    
    conclusion
  })
  output$quality_violin <- renderPlotly({
    validate(need("Project" %in% names(employee_data), "Project column not found in employee_data"))
    
    plot_ly(employee_data, x = ~Project, y = ~Quality_Score, type = 'violin', box = list(visible = TRUE)) %>%
      layout(title = "Quality Score Distribution by Project")
  })
  
  output$summarys_text <- renderText({
    conclusion <- "The analysis of Quality Scores provides the following key insights:\n"
    
    conclusion <- paste0(
      conclusion,
      "- The quality scores are generally higher, with medians ranging from 0.97 to 0.99, indicating a relatively consistent quality level in most projects.\n",
      "- Project 'XXXX'  shows lower median quality scores compared to other projects. This suggests potential challenges or inefficiencies that may require attention and optimization.\n",
      "- There are outliers in projects 'HHHH' and 'QQQQ,' indicating variations in individual quality performance. These variations may warrant further investigation to understand the factors influencing such differences.\n",
      "\n Based on these findings, targeted strategies can be implemented to address challenges in Project 'XXXX' and to provide additional support or training for individuals in projects 'QQQQ','HHHH' and 'XXXX' experiencing performance variations."
    )
    
    conclusion
  })
  
  output$attendance_violin <- renderPlotly({
    validate(need("Project" %in% names(employee_data), "Project column not found in employee_data"))
    
    plot_ly(employee_data, x = ~Project, y = ~Attendance_Score, type = 'violin', box = list(visible = TRUE)) %>%
      layout(title = "Attendance Score Distribution by Project")
  })
  output$summaryc_text <- renderText({
    conclusion <- "The analysis of Attendance Score provides the following key insights:\n"
    
    conclusion <- paste0(
      conclusion,
      "- The majority of employees have high attendance scores close to 1.\n",
      "\n There are distinct outliers in projects 'HHHH','UPPP','OOOO' and 'QQQQ,' indicating variations in individual attendance performance."
    )
    
    conclusion
  })
  output$conclusion_text <- renderText({
    conclusion <- "The analysis of employee performance provides the following key insights:\n"
    
    conclusion <- paste0(
      conclusion,
      "- Overally, employees tend to have high-quality scores and attendance scores, indicating a good level of commitment and performance.\n",
      "\n Project 'HHHH' consistently shows distinct outliers,which may indicate areas where individual performance deviates significantly from the norm and may need further investigation."
    )
    
    conclusion
  })
  
  output$table <- renderDT({
    datatable(case_study_2_data, options = list(pageLength = 5), 
              caption = "Optimal Capacity Planning Advice")
  })
}

shinyApp(ui = ui, server = server)

