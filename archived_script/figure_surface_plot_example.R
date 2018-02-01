library(devtools)
# devtools::install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)
packageVersion('plotly')

# Setting credentials
Sys.setenv("plotly_username"="robcrystalornelas")
Sys.setenv("plotly_api_key"="4aGjAjXCnUUMGAyekWJD")


# 3D Surface plot
class(volcano)
head(volcano)
# ?write.csv
# write.csv(volcano, file = "volcano.csv")
p <- plot_ly(z = ~volcano) %>% add_surface()
p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started

chart_link = api_create(p, filename="surface")
chart_link

### Second test
getwd()
test_csv<-read.csv("surface_plot_test2.csv")
test_matrix <- as.matrix(test_csv)
head(test_matrix)
p <- plot_ly(z = ~test_matrix) %>% add_surface()
p

chart_link = api_create(p, filename="surface2")
chart_link

