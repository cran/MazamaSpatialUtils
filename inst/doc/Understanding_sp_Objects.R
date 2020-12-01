## ----histogram----------------------------------------------------------------
# Plot a histogram but save the return value
h <- hist(rnorm(100), plot = FALSE)

# Query this 'object'
typeof(h)
class(h)
attributes(h)
names(h)

## ----histogram_elements-------------------------------------------------------
# What types of elements do we have?

str(lapply(h, class))

# Let's look a "counts"
h$counts

# Or, alternatively
h[["counts"]]

## ----two_plots, fig.width = 7, fig.height = 10--------------------------------
# Lets set up some more "objects"
h1 <- 1:10        # Numbers 1-10

layout(matrix(seq(2)))
plot(h)
plot(h1)
layout(1)

## ----method_dispatch----------------------------------------------------------
class(h)
class(h1)

methods("plot")

## ----s4-----------------------------------------------------------------------
# Load the 'SimpleCountries' dataset
library(MazamaSpatialUtils)
data("SimpleCountries")

class(SimpleCountries)
typeof(SimpleCountries)

# It's big so we don't just print out the attributes
a <- attributes(SimpleCountries)
class(a)
names(a)

## ----slots--------------------------------------------------------------------
# S4 objects have 'slots'
slotNames(SimpleCountries)

# Access slots using 'slot()' or '@'
class(slot(SimpleCountries, "data"))
class(SimpleCountries@data)

## ----SPDF, fig.width = 7, fig.height = 7--------------------------------------
# Compare 'data' and 'polygons' slots
nrow(SimpleCountries@data)
length(SimpleCountries@polygons)

# Look at the first record
str(SimpleCountries@data[1,])

# We can also use normal dataframe syntax to access columns of data
SimpleCountries$countryName[1]

# Magic happens when we plot using normal dataframe syntax
plot(SimpleCountries[1,])

# Here is a more advanced example using pipes
SimpleCountries %>% 
  subset(UN_region == 150) %>% 
  subset(countryCode != "RU") %>% 
  plot()

SimpleCountries %>%
  subset(countryCode %in% c("DE", "AT", "CH")) %>%
  plot(col = 'red', add = TRUE)

title("German Speakers in Europe")


