# Visualizing covid-19 data with R with main focus on China vs worldwide.

# If not installed, uncomment and execute the following lines below in console.
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("xslx")
# install.packages("magrittr)

# Loading required libraries.
library("ggplot2")
library("magrittr")
library("dplyr")
library("xlsx")

# Note: Magrittr package offers a set of semantics 
# to improve your code by avoiding nested function calls.

# Reading data directly to avoid manual updating.
# If there's preference for dynamic data in lieu of static or inode directory,
# below line can be un-commented but the data set is highly didactic as compared
# the COVID_cases.xslx. There will also be further changes needed to be introduced
# in the code after loading data sets which, completely lies in one's interest.
# myData <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Loading data.
Confirmed_cases_in_the_world <- read.xlsx("./Data/COVID_cases.xlsx", sheetIndex = 1)

# Variable containing the cases confirmed worldwide.
Cases_worldwide <- Confirmed_cases_in_the_world

# Plotting the data for cases confirmed worldwide.
Cases_worldwide_plotted <- ggplot(data = Cases_worldwide, aes(x = Date, y = Cuml_cases)) + 
                           geom_line(col="red") + 
                           ylab("Cumulative confirmed") + 
                           xlab("Dates")
Cases_worldwide_plot <- Cases_worldwide_plotted
Cases_worldwide_plot

# Covid-19 cases in China juxtaposed with the world.
Cases_in_china_compared_to_the_world <- read.xlsx("./Data/COVID_cases.xlsx", sheetIndex = 2)

# Variable containing the juxtaposed data of china vs world.
China_vs_World <- Cases_in_china_compared_to_the_world

# Plotting the data for cases confirmed in china vs worldwide.
China_vs_world_plotted <- ggplot(data = China_vs_World, aes(x = Date, y = Cuml_cases)) + 
                          geom_line(aes(x = Date, y = Cuml_cases, group = Is_china, color = Is_china)) + 
                          ylab("Cumulative cases in China vs Worldwide") +
                          xlab("Dates")
Plotting_Confirmed_cases_China_Vs_world <- China_vs_world_plotted
Plotting_Confirmed_cases_China_Vs_world

# WHO based data - World health organization.
# Mutate adds new variables and preserves existing. 
WHO_events <- tribble(
  ~ Date, ~ Event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting change",
) %>% mutate(Date = as.Date(Date))

# Using WHO_events, add a vertical dashed lines with an X-intercept
# at Date and text at Date, labelled by event & at 100,000 on Y-axis.x
WHO_data_based_plotting <- Plotting_Confirmed_cases_China_Vs_world + 
                           geom_vline(aes(xintercept = Date), data = WHO_events, linetype = "dashed") + 
                           geom_text(aes(x = Date, label = Event), data = WHO_events, y = 100000)
WHO_data_plot <- WHO_data_based_plotting
WHO_data_plot

# China reporting increase after February 15th, 2020.
China_cases_surge_after_feb15th <- China_vs_World %>% 
                                   filter(Is_china == "China", Date >= "2020-02-15")

# Using above variable,generate a line plot Cuml_cases vs Date.
# Add a smooth trend line using linear regression, no error ribbons.
# 'se' argument inside geom_smooth display confidence interval 
# around smooth, TRUE by default.
# 'lm()' or "linear model" function can be used to 
# create a simple regression model. China_cases_surge_after_feb15th, aes(x = Date, y = Cuml_cases)

Plotting_Cuml_cases_vs_Date_for_China_cases_surge_lm <- ggplot(data = China_cases_surge_after_feb15th,
                                                     aes(x = Date, y = Cuml_cases)) + 
                                                     geom_line() +
                                                     geom_smooth(formula = y ~ x, method="lm", se=FALSE) +
                                                     ylab("Cumulative confirmed cases") +
                                                     xlab("Dates")
China_cases_surge_plot <- Plotting_Cuml_cases_vs_Date_for_China_cases_surge_lm
China_cases_surge_plot

# Filter cases of 'not China' for China vs world.
Cases_not_China_for_China_vs_worldwide <- China_vs_World %>% filter(Is_china == "Not China")

# Plotting a line plot using not china for Cuml_cases vs Date.
# Similar like above, add a smooth trend line 
# using linear regression, with no error ribbons.
Plotting_for_data_not_china_lm <- ggplot(data = Cases_not_China_for_China_vs_worldwide, 
                                         aes(x = Date, y = Cuml_cases)) +
                                         geom_line() + 
                                         geom_smooth(formula = y ~ x, method = "lm", se=FALSE) + 
                                         ylab("Cumulative confirmed cases") + 
                                         xlab("Dates")

# The rest of the world can is growing much faster
# than linearly since, from the plot, it can be deduced
# that straight line does not fit well (at all).
# Adding a logarithmic scale can preclude such disasters'.
# With logarithmic scale, we get a much close fit to the data.
Modified_plot_for_not_china_lm <- Plotting_for_data_not_china_lm + scale_y_log10()
Not_China_cases_plot_with_log_scale <- Modified_plot_for_not_china_lm
Not_China_cases_plot_with_log_scale

# Countries outside china that have been disrupted abruptly or
# say, where in the world, countries are highly effected by SARS-COVID, 
# be superior or commensurate to that of china.
Cases_by_country <- read.xlsx("./Data/COVID_cases.xlsx", sheetIndex = 3)
Cases_by_country_cuml_cases <- Cases_by_country$Cuml_cases
Cases_by_country_cuml_cases
# Create a data frame (df) of Cases_by_country_cuml_cases
# But, before that sort that in descending order.
Cuml_cases_by_country_df <- Cases_by_country[order(-Cases_by_country_cuml_cases),]
Top_countries_with_most_cases <- Cuml_cases_by_country_df[1:30,]
Top_countries_with_most_cases

# Plotting a bar plot for most effected countries.
# Cuml_cases vs Province (or use Country).
# The barplots below are just created randomly, just to experiment.
# The boxplots' below can be modified as per needs or for experimenting further.
# Bar_plot_for_countries_hardest_hit_province <- barplot(Top_countries_with_most_cases$Cuml_cases,
#                                               names.arg = Top_countries_with_most_cases$Country,
#                                               col = "gold",)
# Bar_plot_for_countries_hardest_hit_date <- barplot(Top_countries_with_most_cases$Cuml_cases,
#                                                    names.arg = Top_countries_with_most_cases$Date,
#                                                    col = "purple",)

Country_wise_cases_plot <- ggplot(data = Top_countries_with_most_cases, 
       aes(x = Date, y = Cuml_cases)) +
  geom_line(aes(x = Date, y = Cuml_cases, group = Country, color = Country)) + 
  # geom_smooth(formula = y ~ x, method = "lm", se=FALSE) + 
  ylab("Cases count") + 
  xlab("Dates")
Final_countrywise_cases_line_plot <- Country_wise_cases_plot
Final_countrywise_cases_line_plot