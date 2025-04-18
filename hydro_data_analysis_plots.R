### Load Libraries

### Read in Collated Data

# X. Project-specific modifications----
### add week ###
dat_wse_ld3tail <- dat_wse_ld3tail %>%
  mutate(week = week(date))

### add season ###
dat_wse_ld3tail$season <- with(dat_wse_ld3tail, 
                               ifelse(month == 12 | month == 1 | month == 2, 'Winter',
                                      ifelse(month == 3 | month == 4 | month == 5, 'Spring',
                                             ifelse(month == 6 | month == 7 | month == 8,'Summer',
                                                    ifelse(month == 9 | month == 10 | month == 11, 'Fall', 'Fall')))))
### time period ###
dat_wse_ld3tail$tperiod <- with(dat_wse_ld3tail,
                                ifelse(year < 1990, 'Early',
                                       ifelse(year >= 1990, 'Late', 'Late')))


#__1. WSE Data Summaries----

#____a. mean daily elev----

#______i) by year----
wse_stats_years <- dat_wse_ld3tail %>%
  group_by(year) %>%
  summarise(mean = mean(wse))

#______ii) by month----
wse_stats_months <- dat_wse_ld3tail %>%
  group_by(year, month) %>%
  summarise(mean = mean(wse))

#______iii) by year and season----
wse_stats_season <- dat_wse_ld3tail %>%
  group_by(year, season) %>%
  summarise(mean = mean(wse), min_wse = min(wse), max_wse = max(wse)) %>%
  mutate(range = max_wse - min_wse)


#______iv) by season and year----
wse_stats_season_yr <- dat_wse_ld3tail %>%
  group_by(season, year, tperiod) %>%
  summarise(mean = mean(wse))

#__2. WSE Plots----

#____a) mean daily wse ----

#______i) by year----
plot_year <- ggplot(wse_stats_years, aes(x=year, y=mean)) +
  geom_bar(stat='identity') +
  labs(x="Year", y="WSE (ft)")+
  coord_cartesian(ylim=c(665, 675))

plot_year + ggtitle("Mean Daily WSE: Year (1934-2025)")

#______ii) for select month(s) by year----

#July since 1990
wse_stats_july <- wse_stats_months %>%
  filter(year >= 1990 & month == 7)

plot_july <-ggplot(wse_stats_july , aes(x=year, y=mean)) + 
  geom_bar(stat='identity') +
  labs(x = "Year", y="WSE (ft)") +
  coord_cartesian(ylim=c(662,682))

plot_july + ggtitle("Mean Daily WSE: July (1990-2024)")

#Oct since 1990
wse_stats_oct <- wse_stats_months %>%
  filter(year >= 1990 & month == 10)

plot_oct <-ggplot(wse_stats_oct , aes(x=year, y=mean)) + 
  geom_bar(stat='identity') +
  labs(x = "Year", y="WSE (ft)") +
  coord_cartesian(ylim=c(666,682))

plot_oct + ggtitle("Mean Daily WSE: October (1990-2024)")

#______iii) for select season(s) by year----

#bar chart wse fluctuation by season

bar_wse_range_season <- ggplot(wse_stats_season, aes(x=year, y=range)) +
  geom_bar(stat='identity') +
  facet_wrap(~season) +
  labs(x= "Year", y = "WSE range (ft)") 

bar_wse_range_season + ggtitle("Annual WSE Range by Season (1934-2025)")

#scatterplot

#seasons as series
scatter_wse_season <- ggplot(wse_stats_season, aes(x=year, y=mean, color=season, shape=season)) +
  geom_point() +
  geom_smooth(method=loess) +
  labs(x="Year", y="Water Surface Elevation (ft)") +
  coord_cartesian(ylim=c(664, 680))

scatter_wse_season + ggtitle("Mean Daily WSE at L&D3 Tailwaters by Season")

#facet wrap by season
scatter_wse_season4 <- ggplot(wse_stats_season, aes(x=year, y=mean)) +
  geom_point() +
  geom_smooth(method=loess) +
  facet_wrap(~season) +
  labs(x="Year", y="Water Surface Elevation (ft)") +
  coord_cartesian(ylim=c(664, 680))

scatter_wse_season4 + ggtitle("Mean Daily WSE at L&D3 Tailwaters by Season")

#early (1935-1989) and late (since 1990) time period as series and 
#facet wrap by season

scatter_wse_season_period <- ggplot(wse_stats_season_yr, aes(year, y=mean, color=tperiod, shape=tperiod)) +
  geom_point() +
  geom_smooth(method=lm) +
  facet_wrap(~season) +
  labs(x="Year", y="Water Surface Elevation (ft)") +
  coord_cartesian(ylim=c(664, 680))

scatter_wse_season_period + ggtitle("Mean Daily WSE at L&D3 Tailwaters by Season")

#__3. Discharge Data Summaries----

#______vi) week----
dat_q_ld4 <- dat_q_ld4 %>%
  mutate(week = week(date))

#______vii) season----
dat_q_ld4$season <- with(dat_q_ld4, 
                         ifelse(month == 12 | month == 1 | month == 2, 'Winter',
                                ifelse(month == 3 | month == 4 | month == 5, 'Spring',
                                       ifelse(month == 6 | month == 7 | month == 8,'Summer',
                                              ifelse(month == 9 | month == 10 | month == 11, 'Fall', 'Fall')))))
#______viii) time period----
dat_q_ld4$tperiod <- with(dat_q_ld4,
                          ifelse(year < 1990, 'Early',
                                 ifelse(year >= 1990, 'Late', 'Late')))

#__4. Discharge Data Summaries----

#____a. mean daily discharge----

#______i) by year----
q_stats_years <- dat_q_ld4 %>%
  group_by(year) %>%
  summarise(mean = mean(cfs))

#______ii) by month----
q_stats_months <- dat_q_ld4 %>%
  group_by(year, month) %>%
  summarise(mean = mean(cfs))

#______iii) by year and season----
q_stats_season <- dat_q_ld4 %>%
  group_by(year, season) %>%
  summarise(mean = mean(cfs))

#______iv) by season and year----
q_stats_season_yr <- dat_q_ld4 %>%
  group_by(season, year, tperiod) %>%
  summarise(mean = mean(cfs))

#______v) by day----
q_stats_day <- dat_q_ld4 %>%
  group_by(julian) %>%
  summarise(q10 = quantile(cfs, probs = 0.10), q25 = quantile(cfs, probs = 0.25), q50 = quantile(cfs, probs = 0.50),
            q75 = quantile(cfs, probs = 0.75), q90 = quantile(cfs, probs = 0.90))

### pivot longer ###

q_stats_day_long <- q_stats_day %>% 
  pivot_longer(cols = !julian,
               names_to = "quantile",
               values_to = "value")

#______vi) by week----
q_stats_week <- dat_q_ld4 %>%
  group_by(week) %>% 
  summarise(q10 = quantile(cfs, probs = 0.10), q25 = quantile(cfs, probs = 0.25), 
            q50 = quantile(cfs, probs = 0.50), q75 = quantile(cfs, probs = 0.75), 
            q90 = quantile(cfs, probs = 0.90))

q_stats_week_long <- q_stats_week %>% 
  pivot_longer(cols = !week,
               names_to = "quantile",
               values_to = "value")

#__4. Discharge Plots----

#____a) mean daily discharge ----

#______i) by year----
plot_year_q <- ggplot(q_stats_years, aes(x=year, y=mean)) +
  geom_bar(stat='identity') +
  labs(x="Year", y="WSE (ft)")

plot_year_q + ggtitle("Mean Daily Discharge: Year (1934-2025)")

#______ii) for select month(s) by year----

#July since 1990
q_stats_july <- q_stats_months %>%
  filter(year >= 1990 & month == 7)

plot_july_q <-ggplot(q_stats_july , aes(x=year, y=mean)) + 
  geom_bar(stat='identity') +
  labs(x = "Year", y="Discharge (cfs)")

plot_july_q + ggtitle("Mean Daily Discharge: July (1990-2024)")


#Oct since 1990
q_stats_oct <- q_stats_months %>%
  filter(year >= 1990 & month == 10)

plot_oct_q <-ggplot(q_stats_oct , aes(x=year, y=mean)) + 
  geom_bar(stat='identity') +
  labs(x = "Year", y="Discharge (cfs)")

plot_oct_q + ggtitle("Mean Daily Discharge: October (1990-2024)")

#______iii) by year and season----
#______iv) by season and year----

#______v) by day----

### line plots - multiple quantiles ###
line_plot_quantiles_day <- ggplot(q_stats_day_long, aes(x=julian, y= value, color = quantile))  +
  geom_line() +
  labs(x="Julian Day", y="Discharge (cfs)")

line_plot_quantiles_day + ggtitle("Daily Discharge at L&D4 (1934-2025)")

### line plots with shading in between ###
line_plot_quantiles_day2 <- ggplot(q_stats_day, aes(x=julian, y=q50, line = "black")) +
  geom_ribbon(aes(x=julian, ymin=q10, ymax=q25, color="lightblue", fill = "low (10th - 25th percentile)")) +
  geom_ribbon(aes(x=julian, ymin=q25, ymax=q50, fill = "low-mod (25th - 50th percentile)")) +
  geom_ribbon(aes(x=julian, ymin=q50, ymax=q75, fill = "mod-high (50th - 75th percentile)")) +
  geom_ribbon(aes(x=julian, ymin=q75, ymax=q90, fill = "high (75th - 90th percentile)")) +
  geom_line() +
  ylim(0, 200000)

line_plot_quantiles_day2 + ggtitle("Historical Daily Flow at L&D4 (1934-2024)")

### select individual year and combine with historical quantiles

### left_join appears to require converting julian to character, so...

# add new julian 'jd' column to 2024 data filtered from historical data
dat_q_ld4_2024 <- dat_q_ld4 %>% filter (year == 2024) %>%
  select(year, julian, cfs) %>%
  mutate(jd = as.character(julian))

# add new julian 'jd' column to daily quantiles dataframe
q_stats_jd <- q_stats_day %>%
  mutate(jd = as.character(julian))

# join 2024 daily data to historical daily quantiles
historical_2024 <- left_join(q_stats_jd, dat_q_ld4_2024)

plot_historical_2024 <- ggplot(historical_2024, aes(x=julian, y=cfs, line = "black")) +
  geom_ribbon(aes(x=julian, ymin=q10, ymax=q25, fill = "low(10th - 25th percentile)")) +
  geom_ribbon(aes(x=julian, ymin=q25, ymax=q50, fill = "low-mod(25th - 50th percentile)")) +
  geom_ribbon(aes(x=julian, ymin=q50, ymax=q75, fill = "mod-high(50th - 75th percentile)")) +
  geom_ribbon(aes(x=julian, ymin=q75, ymax=q90, fill = "high(75th - 90th percentile)")) +
  geom_line() +
  ylim(0, 200000)

plot_historical_2024 + ggtitle("2024 Discharge at L&D4 compared to historical percentiles (1934-2024)")

#fill = "low(10th - 25th percentile)"

### scatterplots ###

#seasons as series
scatter_q_season <- ggplot(q_stats_season, aes(x=year, y=mean, color=season, shape=season)) +
  geom_point() +
  geom_smooth(method=loess) +
  labs(x="Year", y="Discharge (cfs)")


scatter_q_season + ggtitle("Daily Discharge at L&D4  by Season")

### scatterplot daily discharge - facet wrap by season ###
scatter_q_season4 <- ggplot(q_stats_season, aes(x=year, y=mean)) +
  geom_point() +
  geom_smooth(method=loess) +
  facet_wrap(~season) +
  labs(x="Year", y="Discharge (cfs)")

scatter_q_season4 + ggtitle("Mean Daily Discharge at L&D4 by Season")

#early (1935-1989) and late (since 1990) time period as series and 
#facet wrap by season

scatter_q_season_period <- ggplot(q_stats_season_yr, aes(year, y=mean, color=tperiod, shape=tperiod)) +
  geom_point() +
  geom_smooth(method=lm) +
  facet_wrap(~season) +
  labs(x="Year", y="Discharge (cfs)")

scatter_q_season_period + ggtitle("Mean Daily Discharge at L&D4 by Season")

#______vi) by week----

### line plots q10, q25, q50, q75, q90 ###
line_plot_quantiles_week <- ggplot(q_stats_week_long, aes(x=week, y= value, color = quantile))  +
  geom_line() +
  labs(x="Week", y="Discharge (cfs)")

line_plot_quantiles_week + ggtitle("Weekly Discharge at L&D4 (1934-2025)")


# Z. USER INPUT DATA TESTS ----

Beg_Year <- readline("What is Starting Year? Enter a Year from 1934 to 2025:  ")

End_Year <- readline("What is Ending Year? Enter a Year from 1934 to 2025:  ")

Beg_Year <- as.numeric(Beg_Year)
End_Year <- as.numeric(End_Year)

dat_wse_ld3tail_yrs <- dat_wse_ld3tail %>% filter (year >= Beg_Year & year <= End_Year)

Metric1 <- readline(prompt=paste0("What daily WSE metric would you like to calculate for the Years ", Beg_Year, " to " , End_Year, " ? Enter mean, median, min, max: "))

calc_min <- min(dat_wse_ld3tail_yrs$Elev)

Variable1 <- eval(parse(text = paste('f <- function(', Metric1, ') {return(', calc_min, ')}', sep='')))
print(Variable1)