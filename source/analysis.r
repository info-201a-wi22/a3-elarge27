# analysis.r
# A3 Source Code File
#
# ------------------------------------------------------------------------------
#
# Eric Large
# INFO 201 AF
# 2022-02-17
#
# ------------------------------------------------------------------------------
#
# Libraries
library("tidyverse")
library("usmap")
#
# ------------------------------------------------------------------------------
#
# Loading in the `incarceration_trends.csv` data frame
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
               header = TRUE, stringsAsFactors = FALSE)
View(df)
#
# ------------------------------------------------------------------------------
#
# Summary Information (5 statistics calculated from data set)
#
## 1. As of the most recent year, Which county has the highest population of
##    incarcerated Black people in prison (black_prison_pop)?
df_2 <- df %>%
  select(county_name, year, black_prison_pop, total_prison_pop) %>%
  replace_na(list(black_prison_pop = 0)) %>%
  replace_na(list(total_prison_pop = 0))


county_highest_pop <- df_2 %>%
  filter(black_prison_pop > 0) %>%
  filter(year == max(year)) %>%
  filter(black_prison_pop == max(black_prison_pop)) %>%
  pull(county_name)

# Returns "Los Angeles County"

## 2. In Los Angeles County, what is the ratio of incarcerated Black people (black_prison_pop)
##    to the total incarcerated population in prison (total_prison_pop)?
total_black_prison_pop <- df_2 %>%
  filter(black_prison_pop > 0) %>%
  filter(year == max(year)) %>%
  filter(black_prison_pop == max(black_prison_pop)) %>%
  pull(black_prison_pop)

# Returns 15,853 incarcerated Black people

total_prison_pop <- df_2 %>%
  filter(grepl("Los Angeles County", county_name)) %>%
  filter(total_prison_pop > 0) %>%
  filter(year == max(year)) %>%
  pull(total_prison_pop)

# Returns 42,940 incarcerated people

ratio_black_total_pop <- (total_black_prison_pop / total_prison_pop)

# Returns 0.369 or ~37%

### Data Wrangling for Chart2
black_pop <- df_2 %>%
  filter(black_prison_pop > 0) %>%
  group_by(year) %>%
  summarize(mean(black_prison_pop))

total_pop <- df_2 %>%
  filter(total_prison_pop > 0) %>%
  group_by(year) %>%
  summarize(mean(total_prison_pop))

prison_pop <- left_join(total_pop, black_pop, by = "year")

colnames(prison_pop) <- c("year", "avg_total_pop", "avg_black_pop")

## 3. According to the most recent year, how many Black people were sent to 
##    prison in Los Angeles County (black_prison_adm) as compared to the total number 
##    of admitted prisoners (total_prison_adm), regardless of race? (ratio)

# Creating data frame with prison admission information
df_3 <- df %>%
  select(county_name, year, black_prison_adm, total_prison_adm) %>%
  replace_na(list(black_prison_adm = 0)) %>%
  replace_na(list(total_prison_adm = 0))

# Black prison admissions in Los Angeles County
total_black_prison_adm <- df_3 %>%
  filter(grepl("Los Angeles County", county_name)) %>%
  filter(black_prison_adm > 0) %>%
  filter(year == max(year)) %>%
  filter(black_prison_adm == max(black_prison_adm)) %>%
  pull(black_prison_adm)

# Returns 3,188 admissions of Black people into prison

# Total prison admissions in Los Angeles County
total_prison_adm <- df_3 %>%
  filter(grepl("Los Angeles County", county_name)) %>%
  filter(total_prison_adm > 0) %>%
  filter(year == max(year)) %>%
  filter(total_prison_adm == max(total_prison_adm)) %>%
  pull(total_prison_adm)

# Returns 9,957 total admissions into prison

# Ratio of Black prison admissions to total prison admissions for Los Angeles
# County in the most recent year
ratio_black_total_adm <- (total_black_prison_adm / total_prison_adm)

# Returns 0.32 or ~32% of all admitted prisoners in Los Angeles County were Black

## 4. How have the average admissions of incarcerated Black people in prison
##    (black_prison_adm) changed since data collection on the variable began in 1970?
yearly_black_avg_adm <- df_3 %>%
  filter(black_prison_adm > 0) %>%
  group_by(year) %>%
  summarize(mean(black_prison_adm))

colnames(yearly_black_avg_adm) <- c("Year", "Avg_Admissions")

## 5. Average Yearly Admissions for Black and White Males

# Creating data frame with variables of interest
df_4 <- df %>%
  select(year, county_name, black_male_prison_adm, white_male_prison_adm) %>%
  replace_na(list(black_male_prison_adm = 0)) %>%
  replace_na(list(white_male_prison_adm = 0))

b_a_m <- df_4 %>%
  filter(black_male_prison_adm > 0) %>%
  group_by(year) %>%
  summarize(mean(black_male_prison_adm))

 
w_a_m <- df_4 %>%
  filter(white_male_prison_adm > 0) %>%
  group_by(year) %>%
  summarize(mean(white_male_prison_adm))

male_racial_avg <- left_join(b_a_m, w_a_m, by = "year")

colnames(male_racial_avg) <- c("year", "black_male_adm", "white_male_adm")

# ------------------------------------------------------------------------------
# Chart of average Black & WHite male admissions over time (1970-2016):
theme_set(theme_bw())


# plot
chart1 <- ggplot(male_racial_avg, aes(x=year)) + 
  geom_line(aes(y=black_male_adm, col="Black Males")) + 
  geom_line(aes(y=white_male_adm, col="White Males")) + 
  labs(title="Yearly Average Male Prison Admissions", 
       subtitle="Average Number of Male Prison Admissions Across All U.S. Counties", 
       caption="Source: Vera Institute",
       y="# of Admissions") +
  scale_color_manual(name="", 
                     values = c("Black Males"="#00ba38", "White Males"="#f8766d")) +
  theme(panel.grid.minor = element_blank())

#
# ------------------------------------------------------------------------------
# Chart comparing Black prison population to the total prison population:
chart2 <- ggplot(prison_pop) + 
  geom_point(mapping = aes(x = avg_total_pop, y = avg_black_pop),
             color = "red",
             alpha = .3) +
  labs(subtitle="Average Black & Total Prison Population Across All Counties", 
       y="Black Population", 
       x="Total Population", 
       title="Total Prison Population v.s. Black Prison Population", 
       caption = "Source: Vera Institute")

#
# ------------------------------------------------------------------------------
# Map of Black prison incarceration rates in U.S. (by state): 
## dplyr data wrangling
pop_map <- df %>%
  select(year, county_name, state, black_prison_pop) %>%
  replace_na(list(black_prison_pop = 0)) 
  
black_pop_map <- pop_map %>%
  filter(black_prison_pop > 0) %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(mean(black_prison_pop))

colnames(black_pop_map) <- c("state", "avg_black_pop")


map <- plot_usmap(data = black_pop_map, values = "avg_black_pop", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Imprisoned Black Population (2016)", 
    label = scales::comma
  ) + theme(legend.position = "right") +
  labs(
    title = "Imprisoned Black Population Across the U.S.",
    subtitle = "2016 Averages by State",
    caption = "Source: Vera Institute"
  )

#
# ------------------------------------------------------------------------------