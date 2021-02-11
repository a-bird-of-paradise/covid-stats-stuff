library(tidyverse)

latest_data <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissions&metric=newCasesBySpecimenDate&format=csv',
                        col_types = cols(  date = col_date(format = ""),
                                           areaType = col_character(),
                                           areaCode = col_character(),
                                           areaName = col_character(),
                                           newAdmissions = col_integer(),
                                           newCasesBySpecimenDate = col_integer()))

# have to ignore date prior to 01 Aug - old and stale!

fit_window <- latest_data %>% 
  filter(date >= lubridate::dmy('01-Aug-2020')) %>%
  select(date, newAdmissions, newCasesBySpecimenDate)

lag_plot <- 0:20 %>%
  map(~ fit_window %>% 
        mutate(newCasesBySpecimenDate = lead(newCasesBySpecimenDate, 
                                             .x)) %>%
        na.omit) %>%
  map_dbl(~ lm(newAdmissions ~ newCasesBySpecimenDate - 1, .x) %>%
            summary %>%
            `$`(r.squared)) %>%
  tibble( r_squared = ., lead = 0:20) %>%
  ggplot(aes(x = lead, y = r_squared)) + 
  geom_point() + 
  ggtitle('Lag between cases and admits')

fit_plot <- fit_window %>%
  mutate(newCasesBySpecimenDate = lead(newCasesBySpecimenDate,7)) %>%
  na.omit %>%
  gather(key,value,-date) %>%
  left_join(fit_window %>%
              na.omit %>%
              select(-date) %>%
              summarise_all(mean) %>%
              gather(value = divisor),
            by = 'key') %>%
  mutate(value = value / divisor) %>%
  select(-divisor) %>%
  ggplot(aes(x=date,y=value,colour=key)) + geom_point() + theme(legend.position = 'bottom')

ggsave(lag_plot,
       filename = 'lag_plot.png',
       width = 8, height = 6, dpi = 100)

ggsave(fit_plot,
       filename = 'fit_plot.png',
       width = 8, height = 6, dpi = 100)
