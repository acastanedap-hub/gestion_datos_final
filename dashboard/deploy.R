library(rsconnect)
rsconnect::deployApp(
  appDir = "C:/Users/HP/Desktop/Gestion Datos/dashboard",
  appFiles = c("app.R", "www/styles.css",".Renviron"),
  appName = "trabajo-final-gestion-datos",
  account = "anthonycast",
  forceUpdate = TRUE
)