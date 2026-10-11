# Hostel Expense Analyzer Pro (R + VS Code)

A beginner-friendly local dashboard for realistic student expenses: food, snacks, delivery, transportation, theatres, turf/sports, gaming, subscriptions, laundry, stationery, personal care, mobile recharge, shopping, health, and social spending. It also tracks lending/borrowing separately.

## Run it
1. Extract the ZIP.
2. Open the extracted `Hostel_Expense_Analyzer_R` folder in VS Code.
3. Open the R terminal with Ctrl+Shift+P -> `R: Create R Terminal`.
4. Run `source("run_project.R")`.
5. On first run, missing packages (`shiny`, `dplyr`, `ggplot2`, `DT`) are installed. The dashboard then opens in your browser.

If the R terminal is not in the project folder, use `setwd("C:/your/actual/path/Hostel_Expense_Analyzer_R")` first.

## Files
- `app.R`: dashboard
- `run_project.R`: package check and browser launch
- `install_packages.R`: optional package installer
- `hostel_expenses.csv`: 680 sample transactions from July-September 2026
- `money_tracker.csv`: lending, borrowing, and repayments
- `subscriptions.csv`: recurring subscription examples
- `monthly_budget.csv`: editable category budgets
- `output/`: reserved for future exported reports

Sample values are illustrative INR amounts, not real personal records. The app runs locally and is not published online.
