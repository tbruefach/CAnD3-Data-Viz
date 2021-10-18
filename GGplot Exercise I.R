### GGplot Exercise I ###


# Loading Packages --------------------------------------------------------

library(broom)
library(tidyverse)
library(ggplot2)
library(haven)
library(skimr)

# Exercise 1 --------------------------------------------------------------

# Using the migration data set, create a graph displaying the migrant values
# by location in a boxplot.

# Loading Data
load('data.RData')

  # Tabulating Regions
mig %>% count(location)

  # Selecting only Canadian Provinces/Territories (not 'Canada')
ex1_data <- ggplot(data = mig,
                   mapping = aes(x = persons,
                                 y = location,
                                 fill = location))

  # Plotting Migration by Province/Territory
ex1_data + 
geom_boxplot() +
labs(x = 'Migrants',
     y = 'Location',
     title = 'Migration by Province/Territory',
     caption = 'Created for CAnD3 GGplot Exercises') +
guides(fill=guide_legend(title = 'Location')) +
theme(plot.caption.position = 'plot',
      panel.background = element_rect(fill = 'white'),
      legend.key   = element_rect(fill = 'white'))

  # Saving Plot to a PDF File
ggsave('Exercise_1.pdf', dpi = 1080)




# Exercise 2 --------------------------------------------------------------

# Using the HDI data set, create a scatterplot for income per capita against 
# life expectancy, where the y-axis reversed and the datapoints are colored 
# according to the variable Status using the viridis color palette.

  # Summarizing Life Expectancy and Income Per Capita
hdi %>% skim(GNIpCap, LifeExp)

  # Filtering Data for Scatterplot
ex2_data <- ggplot(data = hdi,
                   mapping = aes(x = LifeExp,
                                 y = GNIpCap,
                                 color = Status))

  # Plotting Life Expectancy by Income per Capita
ex2_data +
  geom_point() +
  scale_y_reverse(labels = scales::comma,
                  n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  scale_color_viridis_d(option = 'H') +
  labs(x = 'Life Expectancy (Years)',
       y = 'Income Per Capita',
       title = 'Life Expectancy as a Function of Income Per Capita',
       caption = 'Created for CAnD3 GGplot Exercises') +
  theme(panel.background = element_rect(fill = 'white'),
        legend.key   = element_rect(fill = 'white'),
        plot.caption.position = 'plot',
        plot.title = element_text(hjust = 0.67))

  # Saving Plot to a PDF File
ggsave('Exercise_2.pdf', dpi = 1080)





# Exercise 3 --------------------------------------------------------------

# Using the HDI data set, create a line graph for mean years of schooling 
# through all the years for all South Asian countries, coloring the lines by 
# country with the color brewer.

  # Selecting South Asian Countries and Grouping Data Accordingly
ex3_data <- hdi %>% 
  filter(Region == 'South Asia') %>% 
  ggplot(mapping = aes(x = Year,
                       y = ExpYrsSchool,
                       color = Country,
                       group = Country))

RColorBrewer::display.brewer.all()

  # Plotting Data
ex3_data +
  geom_line(size = 1) +
  scale_color_brewer(palette = 'Dark2') +
  scale_y_continuous(n.breaks = 20) +
  labs(x = 'Year',
       y = 'Expected Years of Schooling',
       title = 'Mean Schooling in South Asian Countries',
       caption = 'Created for CAnD3 GGplot Exercises') +
  theme(legend.key   = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = 'white'),
        plot.caption.position = 'plot',
        plot.title = element_text(hjust = 0.67))

  # Saving Plot to PDF File
ggsave('Exercise_3.pdf', dpi = 1080)



# Exercise 4 --------------------------------------------------------------

# Using the migration data set, create boxplots for each province/territory 
# migration distribution facetting the years 1960, 1970, 1980, . . . , 2010.


# Tabulating Regions
mig %>% count(location)

# Selecting Data from Each Decade
ex4_data <- ggplot(data = mig %>% filter(year == seq(1960, 2010, 10)),
                   mapping = aes(y = persons,
                                 x = location,
                                 fill = location))

ex4_data +
  geom_boxplot() +
  facet_wrap(. ~ year, ncol = 5) +
  scale_y_continuous(limits = c(-50000, 50000)) +
  labs(x = 'Province/Territory',
       y = 'Net Migration (Persons)',
       title = 'Net Migration by Province/Territory (1970-2010)',
       caption = 'Created for CAnD3 GGplot Exercises') +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.key   = element_rect(fill = 'white'),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption.position = 'plot',
        plot.title = element_text(hjust = 0.5))

  # Saving Plot to PDF File
ggsave('Exercise_4.pdf', dpi = 1080)


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


