# Run this file in the VS Code R terminal from the project folder.
required <- c("shiny","dplyr","ggplot2","DT")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)>0) install.packages(missing, repos="https://cloud.r-project.org")
still_missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(still_missing)>0) stop("Could not install: ",paste(still_missing,collapse=", "),". Check internet and run install_packages.R.")
shiny::runApp(appDir=getwd(), launch.browser=TRUE)
