library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Read the included sample data
expenses <- read.csv("hostel_expenses.csv", stringsAsFactors = FALSE)
money <- read.csv("money_tracker.csv", stringsAsFactors = FALSE)
subs <- read.csv("subscriptions.csv", stringsAsFactors = FALSE)
budget <- read.csv("monthly_budget.csv", stringsAsFactors = FALSE)

expenses$Date <- as.Date(expenses$Date)
money$Date <- as.Date(money$Date)
expenses$DayType <- ifelse(weekdays(expenses$Date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
expenses$Amount_INR <- as.numeric(expenses$Amount_INR)
money$Amount_INR <- as.numeric(money$Amount_INR)
subs$MonthlyCost_INR <- as.numeric(subs$MonthlyCost_INR)
budget$MonthlyBudget_INR <- as.numeric(budget$MonthlyBudget_INR)

ui <- fluidPage(
  tags$head(
    tags$title("Hostel Expense Analyzer Pro"),
    tags$style(HTML("
      body { background: #f3f6fb; color: #172033; font-family: 'Segoe UI', sans-serif; }
      .hero { background: linear-gradient(120deg, #111827, #4338ca); color: white; padding: 24px; border-radius: 18px; margin: 14px 0 18px; }
      .hero h1 { font-weight: 800; margin: 0; }
      .hero p { color: #dbeafe; margin: 8px 0 0; }
      .well { background: white; border: 1px solid #e2e8f0; border-radius: 15px; }
      .kpi { background: white; border: 1px solid #e2e8f0; border-radius: 15px; padding: 16px; margin-bottom: 14px; min-height: 88px; }
      .klabel { color: #64748b; font-size: 12px; text-transform: uppercase; letter-spacing: 1px; }
      .kvalue { font-size: 24px; font-weight: 800; margin-top: 8px; }
      .panel { background: white; padding: 16px; border: 1px solid #e2e8f0; border-radius: 15px; margin-bottom: 15px; }
      .nav-tabs > li > a { font-weight: 600; }
    "))
  ),
  div(
    class = "hero",
    h1("Hostel Expense Analyzer Pro"),
    p("A student lifestyle money dashboard: food, turf, movies, gaming, subscriptions and friends")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter spending"),
      dateRangeInput(
        "dates", "Date range",
        start = min(expenses$Date), end = max(expenses$Date),
        min = min(expenses$Date), max = max(expenses$Date),
        format = "dd M yyyy"
      ),
      selectInput(
        "category", "Category",
        choices = c("All categories", sort(unique(expenses$Category)))
      ),
      selectInput("daytype", "Day type", choices = c("All days", "Weekday", "Weekend")),
      sliderInput("allowance", "Monthly allowance (₹)", min = 2000, max = 15000, value = 6000, step = 500),
      actionButton("reset", "Reset filters", class = "btn-primary"),
      p(
        style = "color:#64748b;font-size:12px;margin-top:15px",
        "Sample INR data. Edit the CSV files to use your own records."
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          fluidRow(
            column(3, div(class = "kpi", div(class = "klabel", "Spending"), div(class = "kvalue", textOutput("total", inline = TRUE)))),
            column(3, div(class = "kpi", div(class = "klabel", "Transactions"), div(class = "kvalue", textOutput("count", inline = TRUE)))),
            column(3, div(class = "kpi", div(class = "klabel", "Average purchase"), div(class = "kvalue", textOutput("average", inline = TRUE)))),
            column(3, div(class = "kpi", div(class = "klabel", "Allowance left"), div(class = "kvalue", textOutput("left", inline = TRUE))))
          ),
          fluidRow(
            column(6, div(class = "panel", h4("Spending by category"), plotOutput("categoryPlot", height = 310))),
            column(6, div(class = "panel", h4("Daily spending trend"), plotOutput("dailyPlot", height = 310)))
          ),
          fluidRow(
            column(6, div(class = "panel", h4("Weekday vs weekend"), plotOutput("weekendPlot", height = 260))),
            column(6, div(class = "panel", h4("Quick insights"), uiOutput("insights")))
          )
        ),
        tabPanel("Transactions", br(), div(class = "panel", h4("Searchable expense records"), DTOutput("transactions"))),
        tabPanel(
          "Budget planner",
          br(),
          fluidRow(
            column(6, div(class = "panel", h4("Budget vs actual"), plotOutput("budgetPlot", height = 350))),
            column(6, div(class = "panel", h4("Budget status"), DTOutput("budgetTable")))
          ),
          div(class = "panel", h4("Savings suggestions"), uiOutput("suggestions"))
        ),
        tabPanel(
          "Lifestyle",
          br(),
          fluidRow(
            column(6, div(class = "panel", h4("Lifestyle spending"), plotOutput("lifePlot", height = 320))),
            column(6, div(class = "panel", h4("Subscriptions"), DTOutput("subsTable")))
          ),
          div(class = "panel", h4("Recurring subscription cost"), uiOutput("subSummary"))
        ),
        tabPanel(
          "Lend & borrow",
          br(),
          fluidRow(
            column(4, div(class = "kpi", div(class = "klabel", "Gross lent"), div(class = "kvalue", textOutput("lent", inline = TRUE)))),
            column(4, div(class = "kpi", div(class = "klabel", "Gross borrowed"), div(class = "kvalue", textOutput("borrowed", inline = TRUE)))),
            column(4, div(class = "kpi", div(class = "klabel", "Net outstanding"), div(class = "kvalue", textOutput("net", inline = TRUE))))
          ),
          div(class = "panel", p("Loans and repayments are tracked separately from expenses."), DTOutput("moneyTable"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateDateRangeInput(session, "dates", start = min(expenses$Date), end = max(expenses$Date))
    updateSelectInput(session, "category", selected = "All categories")
    updateSelectInput(session, "daytype", selected = "All days")
  })

  filtered_data <- reactive({
    data <- expenses %>% filter(Date >= input$dates[1], Date <= input$dates[2])
    if (input$category != "All categories") {
      data <- data %>% filter(Category == input$category)
    }
    if (input$daytype != "All days") {
      data <- data %>% filter(DayType == input$daytype)
    }
    data
  })

  money_format <- function(value) {
    paste0("₹", format(round(value), big.mark = ",", scientific = FALSE))
  }

  output$total <- renderText({
    money_format(sum(filtered_data()$Amount_INR))
  })
  output$count <- renderText({
    nrow(filtered_data())
  })
  output$average <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      "₹0"
    } else {
      money_format(mean(data$Amount_INR))
    }
  })
  output$left <- renderText({
    money_format(input$allowance - sum(filtered_data()$Amount_INR))
  })

  output$categoryPlot <- renderPlot({
    data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(Spend = sum(Amount_INR), .groups = "drop") %>%
      arrange(Spend)
    validate(need(nrow(data) > 0, "No records for these filters"))
    ggplot(data, aes(x = reorder(Category, Spend), y = Spend)) +
      geom_col(fill = "#4f46e5") +
      coord_flip() +
      labs(x = NULL, y = "Spend (₹)") +
      theme_minimal(base_size = 12)
  })

  output$dailyPlot <- renderPlot({
    data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(Spend = sum(Amount_INR), .groups = "drop")
    validate(need(nrow(data) > 0, "No records for these filters"))
    ggplot(data, aes(x = Date, y = Spend)) +
      geom_line(color = "#0d9488", linewidth = 1) +
      geom_point(color = "#0d9488", size = 1.5) +
      labs(x = NULL, y = "Spend (₹)") +
      theme_minimal(base_size = 12)
  })

  output$weekendPlot <- renderPlot({
    data <- filtered_data() %>%
      group_by(DayType) %>%
      summarise(Spend = sum(Amount_INR), .groups = "drop")
    ggplot(data, aes(x = DayType, y = Spend, fill = DayType)) +
      geom_col(width = 0.55) +
      scale_fill_manual(values = c("Weekday" = "#6366f1", "Weekend" = "#f97316")) +
      labs(x = NULL, y = "Spend (₹)") +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$insights <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(p("No data for selected filters."))
    }
    by_category <- data %>%
      group_by(Category) %>%
      summarise(Spend = sum(Amount_INR), .groups = "drop") %>%
      arrange(desc(Spend))
    tagList(
      p(tags$b("Top category: "), by_category$Category[1], " (", money_format(by_category$Spend[1]), ")"),
      p(tags$b("Largest purchase: "), data$Description[which.max(data$Amount_INR)], " — ", money_format(max(data$Amount_INR))),
      p(tags$b("Weekend spend: "), money_format(sum(data$Amount_INR[data$DayType == "Weekend"]))),
      p(tags$b("Purchases above ₹300: "), sum(data$Amount_INR > 300))
    )
  })

  output$transactions <- renderDT({
    datatable(
      filtered_data() %>% select(Date, Time, Category, Description, Amount_INR, PaymentMethod, ExpenseType),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      colnames = c("Date", "Time", "Category", "Description", "Amount (₹)", "Payment", "Type")
    )
  })

  budget_actual <- reactive({
    filtered_data() %>%
      group_by(Category) %>%
      summarise(Actual = sum(Amount_INR), .groups = "drop") %>%
      left_join(budget, by = "Category") %>%
      mutate(
        Status = ifelse(Actual > MonthlyBudget_INR, "Over budget", "Within budget"),
        Variance = MonthlyBudget_INR - Actual
      )
  })

  output$budgetPlot <- renderPlot({
    data <- budget_actual()
    validate(need(nrow(data) > 0, "No records for these filters"))
    ggplot(data, aes(x = reorder(Category, Actual), y = Actual, fill = Status)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Over budget" = "#ef4444", "Within budget" = "#10b981")) +
      labs(x = NULL, y = "Actual spend (₹)") +
      theme_minimal(base_size = 11)
  })

  output$budgetTable <- renderDT({
    data <- budget_actual() %>%
      select(Category, MonthlyBudget_INR, Actual, Variance, Status)
    datatable(
      data, rownames = FALSE, options = list(pageLength = 10),
      colnames = c("Category", "Budget (₹)", "Actual (₹)", "Budget left (₹)", "Status")
    )
  })

  output$suggestions <- renderUI({
    data <- budget_actual() %>% filter(Actual > MonthlyBudget_INR)
    if (nrow(data) == 0) {
      return(p("No category exceeds its reference budget in this filtered view."))
    }
    suggestions <- lapply(seq_len(nrow(data)), function(i) {
      tags$li(
        paste0(
          data$Category[i], " is above its reference budget by ",
          money_format(data$Actual[i] - data$MonthlyBudget_INR[i]),
          ". Review transactions and adjust your budget if needed."
        )
      )
    })
    tags$ul(suggestions)
  })

  lifestyle_categories <- c("Entertainment", "Turf & Sports", "Gaming", "Subscriptions", "Food Delivery", "Social & Friends")
  output$lifePlot <- renderPlot({
    data <- filtered_data() %>%
      filter(Category %in% lifestyle_categories) %>%
      group_by(Category) %>%
      summarise(Spend = sum(Amount_INR), .groups = "drop")
    validate(need(nrow(data) > 0, "No lifestyle data for these filters"))
    ggplot(data, aes(x = reorder(Category, Spend), y = Spend)) +
      geom_col(fill = "#8b5cf6") +
      coord_flip() +
      labs(x = NULL, y = "Spend (₹)") +
      theme_minimal()
  })

  output$subsTable <- renderDT({
    datatable(
      subs, rownames = FALSE, options = list(pageLength = 6, dom = "tip"),
      colnames = c("Subscription", "Renewal date", "Monthly cost (₹)", "Billing", "Status", "Notes")
    )
  })

  output$subSummary <- renderUI({
    monthly_cost <- sum(subs$MonthlyCost_INR[subs$Status != "Cancelled"])
    tagList(
      p(tags$b("Monthly recurring estimate: "), money_format(monthly_cost)),
      p(tags$b("Yearly estimate: "), money_format(monthly_cost * 12)),
      p("Review subscriptions marked Review if you no longer use them.")
    )
  })

  output$lent <- renderText({
    money_format(sum(money$Amount_INR[money$Type == "Lent"]))
  })
  output$borrowed <- renderText({
    money_format(sum(money$Amount_INR[money$Type == "Borrowed"]))
  })
  output$net <- renderText({
    owed_to_you <- sum(money$Amount_INR[money$Type == "Lent"]) -
      sum(money$Amount_INR[money$Type == "Repayment Received"])
    you_owe <- sum(money$Amount_INR[money$Type == "Borrowed"]) -
      sum(money$Amount_INR[money$Type == "Repayment Made"])
    money_format(owed_to_you - you_owe)
  })

  output$moneyTable <- renderDT({
    datatable(
      money, rownames = FALSE, options = list(pageLength = 10),
      colnames = c("ID", "Date", "Type", "Person", "Description", "Amount (₹)")
    )
  })
}

shinyApp(ui = ui, server = server)
