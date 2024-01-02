# Employee Performance Analysis Shiny App

Welcome to the Employee Performance Analysis Shiny App! This interactive application allows you to analyze and visualize employee performance data. The app includes two case studies: Case Study 1 focuses on overall employee performance, while Case Study 2 provides insights into capacity planning.

## Features

- **Login System**: Secure login system with user authentication.
- **Case Study Selection**: Choose between Case Study 1 and Case Study 2.
- **Dashboard Title**: Display a dynamic dashboard title based on user login.
- **Case Study Sidebar**: Select the case study using a dropdown menu.
- **Data Analysis Tabs**: Explore different aspects of employee performance through tabs.
  - **Case Study 1**:
    - Summary Table
    - Productivity Distribution
    - Quality Distribution
    - Attendance Distribution
    - Conclusion
  - **Case Study 2**:
    - Capacity Planning Table

## Installation

To run this Shiny App, make sure you have the required R packages installed:

```
install.packages(c("shiny", "shinyjs", "plotly", "DT", "sodium", "tibble"))
```

## Usage
- Launch the app by running the runApp function in R.
- Log in using the provided credentials (super_admin or admin).
- Choose a case study from the sidebar.
- Explore the tabs for detailed visualizations and insights.

## Credits

This app uses the following R packages:

    shiny: Web application framework
    shinyjs: JavaScript interaction in Shiny apps
    plotly: Interactive plots
    DT: Interactive tables
    sodium: Password hashing
    tibble: Modern data frame
