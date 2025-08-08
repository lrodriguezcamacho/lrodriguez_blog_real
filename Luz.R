library(tidyverse)
salarydata <- read.csv("salary_bras.csv")

#To clean and transform the data for better use

salarydata <- gather(
  salarydata,
  group,
  salary,
  Asian.Men:White.Women,
  factor_key = TRUE
)


salarydata$ethnicity <- str_split_i(
  salarydata$group,
  "\\.",
  1
)

salarydata$gender <- str_split_i(
  salarydata$group,
  "\\.",
  2
)

#To filter a data set without the total values

salary_data_filter <- salarydata |> filter(ethnicity != "Total")
??salarydata

#To create a graph that shows how are women and man pay across racial groups

 ggplot(salary_data_filter, mapping = aes (x = Year, y = salary, group = gender,
                                   color = gender)) +
   geom_line()+
   geom_point()+
   facet_wrap(~ethnicity) +
   labs(title = "Gender Pay Gap by Racial/Ethnic Groups in the U.S. 1988 - 2023",
        y = "Salary",
        x = "Year", color = "Gender")+
   theme_minimal()

ggplot(salary_data_filter, mapping = aes( x= Year, y = salary,
                                          color=gender))+
  geom_line()+
  geom_point()+
  facet_wrap(~ethnicity)

# To create a filter of Non White Males vs White women 

salarydata_whitewomen <- salary_data_filter |> filter((
  gender == "Men" & ethnicity != "White") | (
    gender == "Women" & ethnicity == "White"))

# The creation of the plot of the comparison

ggplot(salarydata_whitewomen, aes(
  x=Year, y=salary, color = interaction(gender, ethnicity)))+
  geom_line()+
  geom_point()+
  labs(title = "Salary Comparison: Men (Non-White) vs White Women", x = "Year",
       y = "Salary", color = "Gender & Race/Ethnicity")+
  theme_linedraw()

#To create a filter of Women only

salarydata_women <- salary_data_filter |> filter(gender == "Women")
salarydata_women

#To create a filter of Men only

salarydata_men <- salary_data_filter |> filter(gender == "Men")
salarydata_men

#To create a plot for women pay gap across groups 

ggplot(salarydata_women, aes(
  x = Year, y = salary, color = ethnicity))+
  geom_line()+
  geom_point()+
  labs(title ="Race/Ethnicity Pay Gap Within Women", x = "Year", y = "Salary",
       color = "Race/Ethnicity")+
  theme_linedraw()

#To make a graph to show the pay gap within men 

ggplot(salarydata_men, aes(
  x = Year, y = salary, color = ethnicity))+
  geom_line()+
  geom_point()+
  labs(title ="Race/Ethnicity Pay Gap Within Men", x = "Year", y = "Salary",
       color = "Race/Ethnicity")+
  theme_linedraw()


# To create a filter of Men vs Asian women 

salarydata_asianwomen <- salary_data_filter |> filter((
  gender == "Men") | (
    gender == "Women" & ethnicity == "Asian"))

# The creation of the plot of the comparison

ggplot(salarydata_asianwomen, aes(
  x=Year, y=salary, color = interaction(gender, ethnicity)))+
  geom_line()+
  geom_point()+
  labs(title = "Salary Comparison: Men vs Asian Women", x = "Year",
       y = "Salary", color = "Gender & Race/Ethnicity")+
  theme_linedraw()
  
  
  
       