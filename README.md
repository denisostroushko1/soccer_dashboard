# Advanced Soccer Statistics
Denis Ostroushko

<!-- gfm -->

Last successful data update: 2025-07-25 00:54:46 CST

# Product Link

[Working version of the
App](https://kexite.shinyapps.io/fb_ref_dashboard/)

<!--
# Obvious Data Fixes: 
&#10;1. Add match week as a variable. During the initial design I completely forgot to include it...
&#10;1.1 Add opponents: currently it is unknown who the opponent on the day was... but the data is available. Will be added in version 2.0 
&#10;2. Europa League and Conference League are not in the data, while UCL is... need to fix this issue, these are some of the best competitions 
to scout players 
&#10;3. Add transfer/market values of players and their salaries 
-->

# Product Title: Advanced player-level soccer data analysis

I finished the final project by myself. I collected, stored, and
analyzed advanced soccer statistics. The final product involves two
major components: automatically refreshing data with intermediate
storage in AWS, and an interactive Shiny Dashboard application with
player-level advanced statistics, in large provided by Opta, and
available for websraping on [FBref.com](https://fbref.com/en/)

## Product type description

I used `shiny` and `shinydashboard` to make a dashboard with data
summary tables and visualizations and non-supervised learning to extract
insights about player types (i.e. finding players who have similar play
styles as underlined by their detailed metrics) and their performances.

The goal of the dashboard is to allow a user to search the database of
players from 12 selected soccer competitions. Detailed data for these
leagues and cups is available as early as 2017 for some leagues, and
2018 for all competitions.

The data set contains just over 100 metrics that track and count various
actions a player may do over the course of the game. The intention of
the app is to give user an ability to select actions of interest, and
get a ‘scouting’ report on the player, using a set of user-identified
metrics. Additionally, the back-end code will filter through the data
and tell user which actions the player is best at when compared to other
players in the data base.

Essentially, the goal of the product is to attempt and create a
‘scouting’ tool using modern statistical analysis methods, and data
visualization techniques.

# Data Sources

[FBref.com](https://fbref.com/en/) is a very popular website which
provides in depth match-by-match player-level performance data for each
player. An example of one game can be viewed
[here](https://fbref.com/en/matches/e62f6e78/Crystal-Palace-Arsenal-August-5-2022-Premier-League).
I an grabbing data from tables listed under “Crystal Palace Player
Stats” and “Arsenal Player Stats”.

<div id="fig-ex">

![](./Data%20sample.png)

Figure 1: Example of data from FBRef.com

</div>

Note that there are 6 tabs for each match, and I am scraping all of
those for the final data set. Some columns are repeated, so the final
data set is “narrower” than the full score of available data on FBref.
Data dictionary is available in the [github
repositiory](https://github.com/denisostroushko1/soccer_dashboard/blob/main/FBref%20Advanced%20Soccer%20Data%20Disctionary.csv)
for this project.

While these data is quite detailed, it is, unfortunately, not the most
advanced data available. However, these data can be obtained using `R`
code and available packages, so it fits the purpose of the exercise.
More advanced and detailed data, down to the one-record per play per
player is available through many providers and APIs, which is a paid
service.

In order to store the data I set up an AWS S3 bucket. Size and amount of
data I am working with qualifies for a free-tier storage plan.

My repository also contains scripts and functions to automate web
scraping of FBref. I set up a git workflow process to scrape the data
from FBref everyday at midnight, store a copy of the data in AWS, create
all subsequent data files in the temporary work environment, and
redeploy the app as part of the process.

# Main features and interactivity

Since I have the data for the past five season, for twelve competitions,
for each player that featured in that span of games, I want to allowed
the user to visualize and summarize every single bit of data. It was
important for me to collect data such that players’ numbers are recorded
to every game, in order to track players’ performance over time. It is
common to see a player on a “hot streak”, and tracking their performance
over time to compare against a longer term average can help us identify
such “hot streaks”.

I also want to be able to find players that are ‘similar’ in terms of
their play styles. My speculation is that players with similar play
styles should have similar underlying stats. For example, we can follow
this procedure:

1.  Identify top 10-15 attributes, or actions, a player does best (we
    can give user an ability to do so interactively). Top attributes can
    be identified using percentiles. For example, a player $A$ can be in
    the 90th percentile of all players in terms of shots per 90 minutes.
    Say, out of 100 possible actions that we track, this action is
    ranked \#7 out of 100, i.e.  there are 6 other metrics, or
    statistics, or numbers for actions, that are in 91st, or higher
    quantiles across all recorded players.

2.  Use only these 10-15 features to create a K-means clustering model,
    and use the distance metric to find players that are similar to the
    one we are interested in.

Additionally, I want to use a number of plotly graphs and datatable
tables to summarize player performance and their historical statistics.

# Future work

1.  I want to refine the data that powers my analysis. The data I used
    for analysis and final version for PUBH 7462 is quite raw. There are
    a lot of sparse metrics, such as red card, or own goals (which are
    rare events). There are also statistics that tally success and fails
    of an outcome (such a successful tackles and unsuccessful tackles),
    which should be turned into the total tackles and success %.
    Reducing the number of features should reduce the amount of noise in
    the analysis.

2.  I want to add more tabs into the dashboard: two-player side-by-side
    comparison, team level-comparison, standout players.

- team-level profile will attempt to compare teams in terms of
  statistics aggregated from the player level. In the process of
  scouting, it is not only important to find a player that is similar to
  a player you try to replace, but also a scouted player needs to be a
  able to fit in the system and play style that a given team has.

Essentially, this will allow us to evaluate how similar a scouted player
is to our current player, but also how well can they transition from
their old team to a new team

- Standout players would be a tab that sorts that the entire data set
  and list best players in attacking, defensive, etc.. categories. This
  is a tool to keep an eye on players who start to perform better and
  better every week, and therefore have stronger stats recorded.
