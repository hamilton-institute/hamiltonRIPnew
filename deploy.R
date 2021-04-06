args <- commandArgs(trailingOnly = TRUE)

usethis::ui_todo("Deploying the app to shinyapps.io...")

rsconnect::setAccountInfo(
  name = 'apmuhamilton',
  token = args[1],
  secret= args[2]
)

files <- list.files('.')
files <- files[!stringr::str_detect(files, ".tsv$|^renv")]

rsconnect::deployApp(
  appFiles = files,
  appName = 'hamiltonRIPnew',
  forceUpdate = TRUE,
  account = 'apmuhamilton',
  logLevel = "normal"
)
