library(tidyverse)
library(rnaturalearth)
library(sf)
library(patchwork)
library(showtext)
library(camcorder)

# Set fonts
font_add_google("Bebas Neue","beb")
font_add_google("Barlow Condensed","bar")
font_add_google("Staatliches","play")
font_add_google("Bitter","bit")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 10, 
  height = 10*0.5625, 
  units = "cm", 
  dpi = 300 
)


# 1. Load basemap
world_110 <- ne_countries(
  scale = 110,
  type = "countries",
  returnclass = "sf"
)

grat <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9))

remove_ant <- c(
  'Euler', 'Albers Equal Area', 'Bertin 1953',
  'Equidistant Conic', 'Lambert Conformal Conic Alternative',
  'Lambert Equal Area Conic','Mercator','Murdoch I','Murdoch III',
  'Tissot', 'Vitkovsky I'
)

remove_both <- c('Interrupted Goode Homolosine')

pro<-tibble(
  proj=c(
    '+proj=adams_ws1','+proj=adams_ws2',
    '+proj=aea +lat_1=29.5 +lat_2=42.5',
    '+proj=aeqd',
    '+proj=airy',
    '+proj=aitoff',
    '+proj=apian',
    '+proj=august',
    '+proj=bacon',
    '+proj=bertin1953',
    '+proj=boggs',
    '+proj=bonne +lat_1=10',
    '+proj=bonne +lat_1=90',
    '+proj=chamb +lat_1=10 +lon_1=30 +lon_2=40',
    '+proj=collg',
    '+proj=comill',
    '+proj=crast',
    '+proj=denoy',
    '+proj=eck1','+proj=eck2','+proj=eck3','+proj=eck4','+proj=eck5','+proj=eck6',
    '+proj=cea',
    '+proj=eqc',
    '+proj=eqdc +lat_1=55 +lat_2=60',
    '+proj=eqearth',
    '+proj=euler +lat_1=67 +lat_2=75',
    '+proj=fahey',
    '+proj=fouc',
    '+proj=fouc_s',
    '+proj=gall',
    '+proj=gins8',
    '+proj=gn_sinu +m=2 +n=3',
    '+proj=goode',
    '+proj=guyou',
    '+proj=hammer',
    '+proj=hatano',
    #'+proj=igh',
    '+proj=kav5',
    '+proj=kav7',
    '+proj=laea',
    '+proj=lagrng',
    '+proj=larr',
    '+proj=lask',
    '+proj=lcca +lat_0=35',
    '+proj=leac',
    '+proj=loxim',
    '+proj=mbt_s','+proj=mbt_fps',
    '+proj=mbtfpp','+proj=mbtfpq','+proj=mbtfps',
    #'+proj=merc',
    '+proj=mill',
    '+proj=moll',
    '+proj=murd1 +lat_1=30 +lat_2=50',
    #'+proj=murd3 +lat_1=30 +lat_2=50',
    '+proj=natearth','+proj=natearth2',
    '+proj=nell','+proj=nell_h',
    '+proj=nicol',
    '+proj=ob_tran +o_proj=moll +o_lon_p=40 +o_lat_p=50 +lon_0=60',
    '+proj=ocea',
    "+proj=ortel",
    '+proj=ortho +lon_0=4.75r',
    '+proj=patterson',
    '+proj=poly',
    '+proj=putp1','+proj=putp2','+proj=putp3','+proj=putp4p','+proj=putp5','+proj=putp6',
    '+proj=qua_aut',
    '+proj=robin',
    '+proj=rouss',
    '+proj=rpoly',
    '+proj=sinu',
    '+proj=tcea',
    '+proj=times',
    '+proj=tissot +lat_1=60 +lat_2=65',
    '+proj=tobmerc',
    '+proj=tpeqd +lat_1=60 +lat_2=65',
    '+proj=urm5 +n=0.9 +alpha=2 +q=4',
    '+proj=urmfps +n=0.5',
    '+proj=vandg','+proj=vandg2','+proj=vandg3','+proj=vandg4',
    '+proj=vitk1 +lat_1=45 +lat_2=55',
    '+proj=wag1','+proj=wag2','+proj=wag3', '+proj=wag4', '+proj=wag5', '+proj=wag6', '+proj=wag7',
    '+proj=weren',
    '+proj=wink1',
    '+proj=wink2'
  ),
  name=c(
    'Adams World in a Square I','Adams World in a Square II',
    'Albers Equal Area',
    'Azimuthal Equidistant',
    'Airy',
    'Aitoff',
    'Apian',
    'August Epicycloidal',
    'Bacon Globular',
    'Bertin 1953',
    'Boggs Eumorphic',
    'Bonne',
    'Bonne - Werner',
    'Chamberlin Trimetric',
    'Collignon',
    'Compact Miller',
    'Craster Parabolic',
    'Denoyer Semi-Elliptical',
    'Eckert I','Eckert II','Eckert III','Eckert IV','Eckert V','Eckert VI',
    'Equal Area Cylindrical',
    'Equidistant Cylindrical (Plate Carrée)',
    'Equidistant Conic',
    'Equal Earth',
    'Euler',
    'Fahey',
    'Foucaut',
    'Foucaut Sinusoidal',
    'Gall Stereographic',
    'Ginsburg VIII',
    'General Sinusoidal Series',
    'Goode Homolosine',
    'Guyou',
    'Hammer & Eckert-Greifendorff',
    'Hatano Asymmetrical Equal Area',
    #'Interrupted Goode Homolosine',
    'Kavrayskiy V',
    'Kavrayskiy VII',
    'Lambert Azimuthal Equal Area',
    'Lagrange',
    'Larrivee',
    'Laskowski',
    'Lambert Conformal Conic Alternative',
    'Lambert Equal Area Conic',
    'Loximuthal',
    'McBryde-Thomas Flat-Polar Sine (No. 1)','McBryde-Thomas Flat-Pole Sine (No. 2)',
    'McBride-Thomas Flat-Polar Parabolic','McBryde-Thomas Flat-Polar Quartic','McBryde-Thomas Flat-Polar Sinusoidal',
    #'Mercator',
    'Miller Cylindrical',
    'Mollweide',
    'Murdoch I',
    #'Murdoch III',
    'Natural Earth I','Natural Earth II',
    'Nell','Nell-Hammer',
    'Nicolosi Globular',
    'General Oblique Transformation',
    'Oblique Cylindrical Equal Area',
    'Ortelius Oval',
    'Orthographic',
    'Patterson',
    'Polyconic (American)',
    'Putnins P1', 'Putnins P2', 'Putnins P3', "Putnins P4'", 'Putnins P5', 'Putnins P6',
    'Quartic Authalic',
    'Robinson',
    'Roussilhe Stereographic',
    'Rectangular Polyconic',
    'Sinusoidal (Sanson-Flamsteed)',
    'Transverse Cylindrical Equal Area',
    'Times',
    'Tissot',
    'Tobler-Mercator',
    'Two Point Equidistant',
    'Urmaev V',
    'Urmaev Flat-Polar Sinusoidal',
    'van der Grinten I', 'van der Grinten II', 'van der Grinten III', 'van der Grinten IV',
    'Vitkovsky I',
    'Wagner I', 'Wagner II', 'Wagner III', 'Wagner IV', 'Wagner V', 'Wagner VI', 'Wagner VII',
    'Werenskiold I',
    'Winkel I',
    'Winkel II'
  )
)


col_back<-'#00171f'
#col_back<-'#534666'
col_map<-"#09BC8A"



col_map<-'#EF6F6C'

col_coun<- '#5D3239'
col_coun <- "#DC8665"


theme_cust<-theme_void()+
  theme(
    plot.margin=margin(1,1,1,1,'cm'),
    plot.background = element_rect(
      fill=col_back,
      #fill=NA,
      color=NA),
    plot.title = element_text(
      #face='bold',
      #color='white',hjust=0.5,family='bar',size=45
      color='white',hjust=0.5,family='play',size=30
      )
  )


i=24

fun_plot<-function(i){
  
  if (pro$name[i]%in%remove_ant) {
    world_110<-world_110%>%filter(name!='Antarctica')
  } else if (pro$name[i]%in%remove_both){
    world_110<-world_110%>%filter(name!='Antarctica')%>%filter(name!='Greenland')
  } else {
    world_110<-world_110
  }
  
  p<-ggplot(world_110)+
    geom_sf(
      #fill="#723D46",color="#723D46"
      # with darker color
      fill=col_coun,color=col_coun
      )+
    #geom_sf(
    #  world_110%>%filter(continent=="North America"),
    #  mapping=aes(geometry=geometry),
    #  fill=col_map,color=col_map
    #)+
    geom_sf(grat,mapping=aes(geometry=geometry),color=alpha("white",0.5),lwd=0.1)+
    coord_sf(crs=pro$proj[i])+
    labs(title=pro$name[i])+
    theme_cust
  
  return(p)
  
}


fun_plot(100)

for (i in 1:100){
  p=fun_plot(i)
  print(p)
}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "north_america.gif"),
  first_image_duration = 0.5,
  last_image_duration = 0.5,
  frame_duration = 1,
  image_resize = 1200
)
