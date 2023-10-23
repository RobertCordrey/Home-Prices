

library(dplyr)
library(lubridate)
library(ggplot2)
library(fredr)
library(scales)

# Get the current month and year
month <- format(Sys.Date(), "%B")
year <- format(Sys.Date(), "%Y")

# Set FRED API key
fredr_set_key("*****")

# Retrieve median sale price of home sold, data from FRED
medianSales <- fredr(
    series_id = 'MSPUS',
    observation_start = as.Date('1971-01-01'),
    observation_end = as.Date(Sys.Date())
)

min_date <- format(min(medianSales$date), '%B %Y')

medianSales1 <- medianSales %>%
    na.omit() %>%
    select(1, 3) %>%
    rename(Date = 1, Price = 2)

class(medianSales1$Date)

yearlymedianSales1 <- medianSales1 %>%
    group_by(year = year(Date)) %>%
    summarize(medianPrice = round(median(Price), 2))

# shows the median sale price for each year
ggplot(yearlymedianSales1, aes(year, medianPrice)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = round(seq(min(yearlymedianSales1$year)
                                          , max(yearlymedianSales1$year), by = 4),2)) +
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    ggtitle(paste0("Median Price from ",
                  min(yearlymedianSales1$year),
                  " to ",
                  max(yearlymedianSales1$year))) +
    labs(caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab(" ") +
    ylab("Median Price")

class(yearlymedianSales1$year)

# shows monthly flucations for each year
ggplot(medianSales1,
       aes(x = year(Date),
           y = Price,
           group = year(Date))) +
    geom_boxplot() +
    scale_x_continuous(breaks = seq(min(year(medianSales1$Date)),
                                          max(year(medianSales1$Date)),
                                          by = 4)) +
    scale_y_continuous(limits = c(min(medianSales1$Price, na.rm = TRUE),
                                  max(medianSales1$Price, na.rm = TRUE)),
                       labels = label_dollar(prefix = "$")) +
    scale_color_brewer(palette="Paired")+
    theme_minimal() +
    ggtitle(paste0("Yearly Variation Rate from ",
                   min(lubridate::year(medianSales1$Date)),
                   " to ",
                   max(lubridate::year(medianSales1$Date)))) +
    labs(subtitle = "",
         caption = "Blue Hen Analytics - Data from Federal Reserve Economic Database") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(hjust = 0.5),
          plot.caption=element_text(hjust = 1)) +
    xlab("") +
    ylab("Monthly Price")








