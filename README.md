# Running data

This project turns a rather unorganized kind of data, jotted down exercise notes about run times and laps, and uses R programming to scan the text and turn it into an actual dataset. The data is then manipulated to calculate metrics such as run speed, or total distance ran, taking into account the different sizes of laps on different tracks. Finally, using Tableau the data is visualized in a comprehensive and interactive dashboard. Therefore, this project goes through all the steps from data collection to cleaning, manipulation and presentation - in other words, showcases an ETL (extract, transform, load) process.

# The Tableau dashboard can be found HERE:
## https://public.tableau.com/app/profile/m.k4987/viz/RunningStats_16586070966120/Dashboard32?publish=ye

Try mousing over specific points to see more details about the data at that point in time. You can also filter out one of the cities runs took place in!


### The files
"trcanje info.txt" is a file with the original notes jotted down after exercise, written in Croatian. It's interesting to compare it to the Excel file which is created at the end of the R script to see how a large block of text gets transformed into a neat, understandable database.

If you want to run the R code yourself, simply download all the files in a single folder. Then, open the .Rproj file and run "running regex_comments.R".
This will also create an up-to-date Excel version of the database in the folder, as well as several images with graphs of the major calculated metrics over time!
