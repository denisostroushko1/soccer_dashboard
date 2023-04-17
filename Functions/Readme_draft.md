
1. Since March 31st I faced a number of technical challenges. I tried multiple ways of storing the data online online and 
pulling it into the Shiny App when the app laods, but neither Google nor AWS services worked. My application was able to load, 
but it took well over 5 minutes for each serivce, which would be terrible for user experience. 

I realized that using a feather data file and have an app deployed with data in the 'package' of files would significantly 
improve load time, which worked out great! At that point, I needed to find a way to re-deploy the app on the daily basis. 

Which leads me into another milestone: git actions. Using lecture and homework materials, I set up a workflow in my repository 
that runs scripts to append mew data from FBref, process the data, store backups in AWS, and deploys the app. My workflow 
also includes secrets to protect AWS keys from being publicly visible. 

The last hurdle I had to address concerns the scructure and size of my data. Initialy I wanted to have one row per-player-per-game. Such data had 650,000 rows and 120 varibles. It was able to load the app to shinyapp.io, but any interctive plot or the table would crash the app due to lack of memory resources. One way to handle it was to use a paid tier subscription. Maybe in the future. 

My solution was to reduce the size of data: make it one row per player-per-season. WHile losing details and granularity, at 
least this way I can stil impletemnt most of my ideas. 

2. My product runs and updates automatically every day at midnight. I need to work on the contern of the app and data visualizations.

3. I deployed my app [here](https://kexite.shinyapps.io/soccer_dashboard/), I am attaching the link instead of a screen shot. 