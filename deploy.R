
source("Master Packages.R")
# https://nrennie.rbind.io/blog/2022-10-05-automatically-deploying-a-shiny-app-for-browsing-rstats-tweets-with-github-actions/
    # Authenticate

    if(file.exists('keys.R') == T){
      
      source("Master Functions.R")
      
      print("Locally getting data for deployemnt")
      ## conditions for a local deployment 
      source('keys.R')
      rsconnect::setAccountInfo(name = SHINY_ACC_NAME,
               token = SHINY_TOKEN,
               secret = SHINY_SECRET_TOKEN)
      
       Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
                   "AWS_SECRET_ACCESS_KEY" = secret_key, 
                   "AWS_DEFAULT_REGION" =  aws_region)
    
        tempfile <- tempfile()  
        save_object(object = "s3://shiny-soccer-data/dashboard_data_csv_zip.zip", file = tempfile)
        zipped <- unzip(tempfile)
        older_data <- read_csv("dashboard_data.csv")
      
        colnames_to_remove <- colnames(older_data)[grep("[1-9]", colnames(older_data))]
        
        older_data <- older_data[, -c(which(colnames(older_data) %in% colnames_to_remove))]
        
        unlink('dashboard_data.csv')
        unlink('dashboard_data_csv_zip.zip')
        
        roll_up <- roll_up_data(big_data = older_data)
        write_feather(roll_up, 'dash_df_rollup.fthr')
        
    print("Connect Successfull")
    }
    
    if(file.exists('keys.R') == F){
      print("Connecting to account using git secrets")
        rsconnect::setAccountInfo(name = Sys.getenv("shiny_acc"),
                      token = Sys.getenv("shiny_token"),
                      secret = Sys.getenv("shiny_secret"))
       print("Connect Successfull")
    }
    
    # Deploy
    
     print("Starting deployment")
     # remove.packages("knitr"): removing a package this way causes a process to just terminate with no clear error code
    deployApp(
      appFiles = c( 
        # data files  for the app 
        'dash_df_rollup.fthr', 'FBref Advanced Soccer Data Disctionary.csv', 
        
        # scripts and R files 
        "Master Packages.R",  "app.R", 
        
        # other files 
        './www/boot.png' 
        ), 
      appName = 'fb_ref_dashboard', 
      forceUpdate = T
      )
    
    print("App deployed")
    