##### Visualizing the frequency and magnitude of NK provocations
#####===========================================================================

##### Load libary
#####===========================================================================
library(ggplot2)

### Create figure (that highlights nuclear provocations)
###=============================================================================
weekly_data %>%
  group_by(year) %>%
  summarize(events = sum(number_events),
            nuke  = sum(nuclear)) %>%
  mutate(nuclear = ifelse(nuke > 0, Inf, -Inf)) %>%
  ggplot(aes(year, events)) + 
  geom_line(linewidth=1) + 
  geom_col(aes(year, nuclear), alpha = 0.25, fill = "red", width = 0.3) + 
  xlab("Year") + 
  ylab("Number of Provocations") +
  ggtitle("North Korean Provocations by Year",
          subtitle = "Nuclear Provocations Highlighted") + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(1960, 2020, 10))
