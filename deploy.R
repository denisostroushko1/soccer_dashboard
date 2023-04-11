
source("Master Packages.R")
# https://nrennie.rbind.io/blog/2022-10-05-automatically-deploying-a-shiny-app-for-browsing-rstats-tweets-with-github-actions/
    # Authenticate
    print("Starting deployment")

    setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
                   token = Sys.getenv("SHINY_TOKEN"),
                   secret = Sys.getenv("SHINY_SECRET_TOKEN"))
    # Deploy
    print("Connect Successfull")
    
    deployApp(appFiles = c("app.R", 'dash_df.fthr', "Master Packages.R"), 
              appName = 'soccer_dashboard', 
              forceUpdate = T)
    
    
    print("App deployed")
    