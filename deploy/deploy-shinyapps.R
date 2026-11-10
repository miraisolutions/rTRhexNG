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
app_files = c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/")
rsconnect::deployApp(
  appName = "rTRhexNG", appFiles = app_files, forceUpdate = TRUE
)
