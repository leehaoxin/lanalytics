lanalytics
==========

Analysis of student data from quizzes

Files:

1. parsequizzes.R

- First data parsing step - this script is designed to be run exactly once at 
    the beginning of the data analysis process
- Takes xlsx file for each quiz as downloaded from Learning Catalytics
- Assigns each student a unique random identifier
- Creates 3 tables, all of which have one row per student ID, and one column per   
    quiz question. Data in those tables are 
    - Absolute time of answer
    - Content of answer
    - Correctness of answer (1 or 0)
    All three tables are saved into xlsx files
- Creates an additional (confidential) table of student IDs and names, to be
    stored in a safe place


2. order_time.R

- Depends on parsequizzes.R
- Takes absolute time stamp data as computed using parsequizzes.R
- For each quiz and each student, computes 
    - the order in which questions were answered
    - the time it took to answer each question


3. easiness_time.R

- Depends on order_time.R, parsequizzes.R 
- Takes time per question and correctness information
- For each quiz question, compute
    - item easiness (% of sdtudents who g)
    - time to answeer item (median across all students)
- scatter plots easiness vs time


4. guessers.R

- TODO: This function is still buggy and incomplete
- Depends on order_time.R, parsequizzes.R
- Assumes that the first two and last two questions of each quiz are easy 
    (in our case, because they are about study style and preparaation)
- Uses time for those easy question as a threshold (i.e. fastest possible)
- Computes questions answered below this threshold and classifies them as
    - cheating, if the answer was correct
    - guessing, if the answer was wrong
- Plots overall occurence of guessing and cheating across quizzes and students

5. easiness_time_level.R

- Depends on easiness_time.R
- Also depends on a rating of cognitive level for each question (as provided by
    the instructor, 1=low, 3=high)
- Plots scatter plot of easiness vs time, colour-coded by cognitive level

6. easiness_time_level_two_years.R

- Depends on easiness_time.R
- Also depends on a rating of cognitive level for each question (as provided by
    the instructor, 1=low, 3=high)
- Does the same as easiness_time_level.R, but for two successive years
- Plots data from both years side-by-side

7. import_time_easiness.R

- Helper script to import data from excel for further processing
- Written in order to be able to use earlier data we processed in MATLAB
- Not part of the analysis workflow if everything is done in lanalytics 


