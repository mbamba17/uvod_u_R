library(plotly)
library(readxl)

# Read in data
df <- read_excel(path = "timeline.xlsx")

# today
#danas <- as.Date("2020-02-17")
danas <- Sys.Date()  
  
# Convert to dates
df$Start <- as.Date(df$Start, format = "%m/%d/%Y")

# Sample client name
client = "Sample Client"

# Choose colors based on number of resources
#cols <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set6")
#df$color <- factor(df$color, labels = cols)

# Initialize empty plot
p <- plot_ly()

# Each task is a separate trace
# Each trace is essentially a thick line plot
# x-axis ticks are dates and handled automatically

for(i in 1:(nrow(df) - 1)){
  p <- add_trace(p,
                 x = c(df$Start[i], df$Start[i] + df$Duration[i]),  # x0, x1
                 y = c(i, i),  # y0, y1
                 mode = "lines",
                 line = list(color = df$color[i], width = 20),
                 showlegend = F,
                 hoverinfo = "text",
                 
                 # Create custom hover text
                 
                 text = paste("Zadatak: ", df$Task[i], "<br>",
                              "Početak: ", df$Start[i], "<br>",
                              "Kraj: ", df$End[i], "<br>",
                              "Trajanje: ", df$Duration[i], "<br>",
                              "Grupa: ", df$Resource[i]),
                 
                 evaluate = T  # needed to avoid lazy loading
  ) 
}

# Add information to plot and make the chart more presentable

p <- layout(p,
            
            # Axis options:
            # 1. Remove gridlines
            # 2. Customize y-axis tick labels and show task names instead of numbers
            
            xaxis = list(showgrid = T, tickfont = list(color = "black"),tickvals=seq(min(df$Start),max(df$Start),1), tickformat = "%d/%m (%a)"),
            
            yaxis = list(showgrid = T, tickfont = list(color = "black"),
                         tickmode = "array", tickvals = 1:nrow(df), ticktext = df$Task,
                         domain = c(0, 0.9)),
            
            plot_bgcolor = "white",  # Chart area color
            paper_bgcolor = "white") # Axis area color
p <- add_segments(p, x = danas, xend = danas, y = 0, yend = nrow(df), color="red")

p
