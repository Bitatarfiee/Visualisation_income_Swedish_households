
library(rjson)
library(plotly)
library(dplyr)
library(ggplot2)
#library(MASS)
library(readr)
library(plotly)

json<-fromJSON(file="gadm41_SWE_1.json")
df<-read.csv("000006SW_20230919-130923.csv", fileEncoding="latin1")

#Read your data into R and process it in such a way
#that different age groups are shown in
#different columns. Let’s call these groups Young,
#Adult and Senior

data <- df %>%
  filter(age %in% c("18-29 years"))%>%
  dplyr::select(region,X2016)%>%
  dplyr::rename(Young = X2016)

data<-mutate(data,df %>%
               filter(age %in% c("30-49 years"))%>%dplyr::select( X2016)%>%
               dplyr::rename(Adult = X2016))%>%mutate(data,df %>%
                                                        filter(age %in% c("50-64 years"))%>%dplyr::select( X2016)%>% 
                                                        dplyr::rename(Senior = X2016))

head(data)
#Part2
#Create a plot in Plotly containing three violin plots 
#showing mean income

#plot_ly by default data 
p3<-plot_ly(df, x=~factor(age), y=~X2016, split=~factor(age),
            type="violin", box=list(visible=T))%>% layout(yaxis=list(title="Mean income distributions"),
                                                          xaxis=list(title="Age Group"))
p3


#plot_ly by Processed data 
p4<-plot_ly(data, type = "violin", box = list(visible = TRUE)) %>%
  add_trace(y = ~Young, name = "Young") %>%
  add_trace(y = ~Adult, name = "Adult") %>%
  add_trace(y = ~Senior, name = "Senior") %>%
  layout(title = "Mean Income Distribution by processed data",yaxis=list(title="Mean income distributions"),
         xaxis=list(title="Age Group"))

p4


#part3
library(dplyr)
library(ggplot2)
library(MASS)

library(plotly)

#data %>%plot_ly(x=~Young, y=~Adult, z=~Senior, type="scatter3d")
#data %>%plot_ly(x=~Young, y=~Adult, z=~Senior, type="contour")


library(akima)
attach(data)
s=interp(Young,Adult,Senior, duplicate = "mean")
detach(data)

plot_ly(x=~s$x, y=~s$y, z=~s$z, type="surface") %>%
  layout(scene = list(
    xaxis = list(title = "Young Income"),
    yaxis = list(title = "Adult Income"),
    zaxis = list(title = "Senior Income")
  ),
  title = "Dependence of Senior Incomes on Adult and Young Incomes by Region")


#part4
print(json$features[[2]]$properties)
#plot
g <- list(fitbounds = "location", visible = FALSE,scope = 'europe')
p1 <- plot_geo(data) %>% add_trace(
  type = "choropleth",
  geojson = json,
  locations = ~region,
  z = ~Young,
  featureidkey = "properties.NAME_1") %>%
  layout(geo = g, title = "Young Incomes by Region")

p1
p2 <- plot_geo(data) %>% add_trace(
  type = "choropleth",
  geojson = json,
  locations = ~region,
  z = ~Adult,
  featureidkey = "properties.NAME_1") %>%
  layout(geo = g, title = "Adult Incomes by Region")

p2


##ccoordinates of linkoping
#latitude,longitude,name,desc,color,source,precision
#58.41109,15.62565,Linkoping,"Linkoping, Linkoping, E, SE",,MapQuest,city/town
lat=58.41109
long=15.62565

p1 <- plot_geo(data) %>% add_trace(
  type = "choropleth",
  geojson = json,
  locations = ~region,
  z = ~Young,
  featureidkey = "properties.NAME_1") %>%
  layout(geo = g,title = "Adult Incomes by Region, Red dot is Linkoping ") %>%
  add_markers(
    x = ~long,
    y = ~lat,
    color = "red",
    size=2
  )


p1