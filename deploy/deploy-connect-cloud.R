# deploy/deploy-shinyapps.R
# usethis::use_build_ignore("deploy")
if (!interactive()) {
  rsconnect::connectCloudClientCredentials(
    clientId = Sys.getenv("CONNECT_CLOUD_CLIENT_ID"),
    clientSecret = Sys.getenv("CONNECT_CLOUD_CLIENT_SECRET"),
    account = Sys.getenv("CONNECT_CLOUD_ACCOUNT")
  )
}
# Add here any additional files/directories the app needs
app_files <- c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/")
# Determine the ID of an existing deployment if one exists
app_name <- "rTRhexNG"
app_id <- rsconnect::applications() |> subset(name == app_name, id, drop = TRUE)
app_id <- if (length(app_id) == 0) NULL else app_id
rsconnect::deployApp(
  appName = app_name,
  appId = app_id,
  appFiles = app_files,
  forceUpdate = TRUE
)
