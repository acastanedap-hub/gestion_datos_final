library(rsconnect)
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "www/styles.css",".Renviron"),
  appName = "trabajo-final-gestion-datos",
  account = "anthonycast",
  forceUpdate = TRUE
)