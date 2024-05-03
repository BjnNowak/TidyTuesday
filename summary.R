library(tidyverse)
library(camcorder)
library(ggtext)
library(showtext)
library(patchwork)
library(scico)
library(treemap)
library(treemapify)
library(RColorBrewer)

# Set fonts
font_add_google("JetBrains Mono","jet")
font_add_google("Open Sans","open")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 23.3, 
  height = 32, 
  units = "cm", 
  dpi = 600 
)

# Define botanic famillies
amaranthaceae<-c(
  'Sugar beet'
)

amaryllidaceae<-c(
  'Onions and shallots, dry (excluding dehydrated)',
  'Green garlic'
)

anacardiaceae<-c(
  'Cashew nuts, in shell','Mangoes, guavas and mangosteens'
)

araceae<-c(
  'Taro'
)

arecaceae<-c(
  'Oil palm fruit', 'Coconuts, in shell','Dates'
)

asteraceae<-c(
  'Sunflower seed','Lettuce and chicory'
)

brassicaceae<-c(
  'Rape or colza seed','Cabbages',
  'Cauliflowers and broccoli','Mustard seed'
)

convolvulaceae<-c(
  'Sweet potatoes'
)

cucurbitaceae<-c(
  'Pumpkins, squash and gourds','Watermelons','Melons',
  'Cucumbers and gherkins'
)

dioscoreaceae<-c(
  'Yams'
)

euphorbiaceae<-c(
  'Cassava, fresh','Natural rubber in primary forms'
)

fabaceae<-c(
  'Soya beans','Beans, dry','Groundnuts, excluding shelled',
  'Chick peas, dry','Cow peas, dry','Peas, dry','Pigeon peas, dry',
  'Other pulses n.e.c.','Lentils, dry','Broad beans and horse beans, dry',
  'Peas, green','Other beans, green','Lupins'
)

linaceae<-c(
  'Linseed'
)

malvaceae<-c(
  'Seed cotton, unginned','Cocoa beans','Okra',
  'Jute, raw or retted'
)

musaceae<-c(
  'Bananas'
)

oleaceae<-c(
  'Olives'
)

pedaliaceae<-c(
  'Sesame seed'
)

poaceae<-c(
  'Wheat','Barley','Rye','Triticale',
  'Maize (corn)','Rice','Sorghum','Millet',
  'Sugar cane','Oats','Maize'
)

polygonaceae<-c(
  'Buckwheat'
)

rosaceae<-c(
  'Apples','Plums and sloes','Peaches and nectarines','Pears',
  'Almonds, in shell'
)

rubiaceae<-c(
  'Coffee, green'
)

rutaceae<-c(
  'Oranges','Tangerines, mandarins, clementines',
  'Other citrus fruit, n.e.c.','Lemons and limes'
)

solanaceae<-c(
  'Potatoes','Tomatoes','Peppers',
  'Eggplants (aubergines)','Unmanufactured tobacco'
)

theaceae<-c(
  'Tea leaves'
)

vitaceae<-c(
  'Grapes'
)

# Load data
data <- read_delim('data/summary/summary.csv')%>%
  mutate(Item=case_when(
    Item%in%c('Plantains and cooking bananas','Bananas')~'Bananas',
    Item=="Pears, green"~"Pears",
    Item%in%c(
      'Chillies and peppers, green (Capsicum spp. and Pimenta spp.)',
      'Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw'
      )~'Peppers',
    Item%in%c('Maize (corn)','Green corn (maize)')~'Maize',
    Item%in%c('Cantaloupes and other melons','Melonseed')~'Melons',
    TRUE~Item
  ))%>%
  group_by(Item,Year)%>%
  summarize(Value=sum(na.omit(Value)))%>%
  ungroup()%>%
  group_by(Item)%>%
  summarize(av_area=mean(na.omit(Value)))%>%
  ungroup()%>%
  mutate(fam=case_when(
    Item%in%amaranthaceae~'Amaranthaceae',
    Item%in%amaryllidaceae~'Amaryllidaceae',
    Item%in%anacardiaceae~'Anacardiaceae',
    Item%in%araceae~'Araceae',
    Item%in%arecaceae~'Arecaceae',
    Item%in%asteraceae~'Asteraceae',
    Item%in%brassicaceae~'Brassicaceae',
    Item%in%convolvulaceae~'Convolvulaceae',
    Item%in%cucurbitaceae~'Cucurbitaceae',
    Item%in%dioscoreaceae~'Dioscoreaceae',
    Item%in%euphorbiaceae~'Euphorbiaceaee',
    Item%in%fabaceae~'Fabaceae',
    Item%in%linaceae~'Linaceae',
    Item%in%malvaceae~'Malvaceae',
    Item%in%musaceae~'Musaceae',
    Item%in%oleaceae~'Oleaceae',
    Item%in%pedaliaceae~'Pedaliaceae',
    Item%in%poaceae~'Poaceae',
    Item%in%polygonaceae~'Polygonaceae',
    Item%in%rosaceae~'Rosaceae',
    Item%in%rubiaceae~'Rubiaceae',
    Item%in%rutaceae~'Rutaceae',
    Item%in%solanaceae~'Solanaceae',
    Item%in%theaceae~'Theaceae',
    Item%in%vitaceae~'Vitaceae'
  ))%>%
  arrange(-av_area)


sm<-data%>%
  group_by(fam)%>%
  summarise(per_fam=sum(av_area))%>%
  ungroup()%>%
  mutate(sz=100*per_fam/max(per_fam))

# Make treemap
tm<-treemap(
  data,
  index=c("fam","Item"),
  vSize="av_area",
  type="index"
) 

tm_plot_data <- tm$tm %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>% 
  # mark primary groupings and set boundary thickness
  mutate(primary_group = case_when(
    is.na(fam)~3,
    TRUE~1
  ))%>%
  group_by(fam)%>%
  mutate(rel_vSize=vSize/sum(vSize))%>%
  arrange(vSize)%>%
  mutate(
    ct=1,
    nb=row_number()/sum(ct)
  )%>%
  ungroup

n=length(levels(as.factor(tm_plot_data$fam)))

pal <- c(
  '#40c9a2', # mint 
  '#FEE440', # maize
  '#F15BB5', # rose
  '#00bbf9', # blue
  '#FF8811'  # orange;
)

# Extend color palette
ext_pal<-colorRampPalette(
  pal, 
  bias = 1, 
  space = "rgb", #c("rgb", "Lab"),
  interpolate = "linear", #c("linear", "spline"), 
  alpha = FALSE)(n)

ggplot(
  tm_plot_data, 
  aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  geom_rect(
    data=tm_plot_data%>%filter(level==2),
    aes(fill = fam,alpha=nb),
    show.legend = FALSE, color = "white", 
    #alpha = .3, 
    size=0.5
  )+
  geom_rect(
    data=tm_plot_data%>%filter(level==1),
    aes(),
    fill=NA,
    show.legend = FALSE, color = "black", fill=NA,
    #alpha = .3, 
    lwd=1
  )+
  geom_richtext(
    data=tm_plot_data%>%
      filter(level==1),
    mapping=aes(
      x=x0,y=y1,
      label=fam),
    hjust=0,vjust=1,
    size=12,
    family="jet",color="black",alpha=0.95,
    lineheight=0.25,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    #inherit.aes=FALSE
  )+
  scale_fill_manual(values=ext_pal)+
  scale_alpha(range=c(0.2,1))+
  theme_void()
  #coord_polar()

p1<-ggplot(
  tm_plot_data, 
  aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  # add fill and borders for groups and subgroups
  geom_rect(
    data=tm_plot_data%>%filter(level==2),
    aes(fill = fam,alpha=nb),
    show.legend = FALSE, color = "white", 
    #alpha = .3, 
    size=0.5
  )+
  geom_rect(
    data=tm_plot_data%>%filter(level==1),
    aes(),
    fill=NA,
    show.legend = FALSE, color = "black", fill=NA,
    #alpha = .3, 
    lwd=1
  )+
  scale_fill_manual(values=ext_pal)+
  scale_alpha(range=c(0.2,1))+
  theme_void()

ggsave("tst1.svg",plot=p1,device="svg",width=23.3,height=32,unit='cm')


