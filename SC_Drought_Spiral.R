library(tidyverse)
library(showtext)
library(ggtext)
library(camcorder)
library(patchwork)
library(spiralize)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Fira Sans","fira")
showtext_auto()

# Plot size
# A4
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 29.7, 
  height = 16, 
  units = "cm", 
  dpi = 300 
)

# Dl data
# see here for data download :
# https://droughtmonitor.unl.edu/DmData/DataDownload.aspx
# (comprehensive statistics)
drought <- readr::read_csv('Data/Drought/data/drought_cali.csv')

# Started with Arkansas but chosed cali instead
# (but did not change name...)
ak<-drought%>%
  filter(StateAbbreviation=="CA")

wide_ak<-ak%>%
  select(ValidStart,None,D0,D1,D2,D3,D4)%>%
  arrange(ValidStart)%>%
  mutate(yx=lubridate::year(ValidStart))%>%
  filter(yx>2000&yx<2022)%>%
  mutate(nb=row_number())

x = wide_ak$nb
y = as.matrix(wide_ak[,2:7])/100

col_pal <- c("#FF006D","#EE6C7D","#E6A285","#DDD78D","#80C9DC", "#01BEFE")
rev_pal<-rev(col_pal)

par(bg=NA)

spiral_initialize(xlim = c(0, nrow(wide_ak)), start = 90, end = 360*21+90,flip = 'horizontal') # a circle of 52 weeks
spiral_track(height = 0.85)
spiral_bars(x, y, gp = gpar(fill = rev_pal, col = NA))

dev.copy(png,'my_spi.png',width=2970/3,height=1600/3,units='mm',res = 300)
dev.off()
