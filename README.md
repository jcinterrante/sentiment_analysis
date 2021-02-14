# Data Skills 2 - R
## Winter Quarter 2021

## Homework 3
## Due: Sunday February 21st before midnight on GitHub Classroom

Note that there is a lot of flexibility in how you approach these questions and what your final results will look like.  Being comfortable with that sort of assignment is an explicit course goal; real-world research is much more likely to come with open-ended assignments rather than explicit goals to start with X and accomplish exactly Y.  Use short comments (1-3 lines max) to explain any choices that you think need explaining.

__Question 1 (50%):__ You are working as a research assistant at a policy think tank.  The senior researcher you work for is preparing a study into global economic sentiment, and has assigned you the task of parsing a quarterly report from the Bank of International Settlements (BIS), a financial institution in Switzerland that produces research and reports focusing on central banks and the global economy.

Unfortunately, reports from the BIS are very dry, and the senior researcher doesn't actually want to read any of them.  As a kind boss, they also don't want to make YOU read through them, so they ask you to instead analyze the December 2020 quarterly overview (text file in the homework repo) using natural language processing.

Parse the text using the tidytext library, and perform sentiment analysis on it.  Produce summary statistics and 1-2 plots that help describe the sentiment of the article.

Output to save to your repo for this question:
  * question1.R file with the code - summary statistics can be displayed with print or View
  * question1_plot.png files for the plots you generate

__Question 2 (50%):__ Your senior researcher is very happy with the results you achieved on the December 2020 report, so they ask you to generalize your code to parse more BIS reports.  

Collect both the [December 2019](https://www.bis.org/publ/qtrpdf/r_qt1912a.htm) and [December 2020](https://www.bis.org/publ/qtrpdf/r_qt2012a.htm) reports directly from the web, parsing the text out of the html.  Produce new summary statistics and 1-2 new plots that show how sentiment has changed over time.

Do not use the text file from question 1, but do reuse as much code as you can from question 1 (i.e. copy and paste any relevant code from question 1 and then change it to generalize).  Keep in mind that your code for question 2 needs to potentially be able to generalize to more than the two URLs specified.  For example, you might be asked by your hypothetical boss to parse BIS quarterly reports going back ten years.  Note that any slight variation in results from the text file in question 1 and the parsed html content for 2020 is acceptable, due to differences in the web formatting.

Output to save to your repo for this question:
  * question2.R file with the code - summary statistics can be displayed with print or View
  * question2_plot.png files for the plots you generate
  * A csv document in tidy format of sentiment results for the two articles, which could be expanded to include more than these two quarterly reports.  You can assume there will be only one BIS report per quarter.