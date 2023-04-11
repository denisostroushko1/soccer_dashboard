
# https://nrennie.rbind.io/blog/2022-10-05-automatically-deploying-a-shiny-app-for-browsing-rstats-tweets-with-github-actions/
rsconnect::setAccountInfo(name='kexite',
			  token='6CCB37F9420F50D615E3A222E8C297A1',
			  secret='61LtkxrGX+wr4y/0KyyL5KrB96quAxv0wAnXqHW1')
    # Authenticate
    setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
                   token = Sys.getenv("SHINY_TOKEN"),
                   secret = Sys.getenv("SHINY_SECRET_TOKEN"))
    # Deploy
    deployApp(appFiles = c("app.R", 'dash_df.fthr', "Master Packages.R"), 
              appName = 'soccer_dashboard', 
              forceUpdate = T,
              account = )
    