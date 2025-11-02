# Chargement des pkg
####################
library(camcorder)
library(ggforce)
library(tidyverse)

# Taille du graphique
#####################
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

# Chargement des données
########################

# Données téléchargeables sur le site de l'ADEME :
# https://data.ademe.fr/datasets?topics=TQJGtxm2_
data<-read_delim('data/agribalyse.csv',delim=';')

# Défition des trois classes d'impacts
######################################

# Description des différents types d'impacts :
# https://doc.agribalyse.fr/documentation/les-donnees/methodologie-acv

# Environnement
enviro<- c(
  'a1_ecotox_eau' , 'a2_eutroph_P' , 'a3_eutroph_N' ,
  'a4_eutroph_terre' , 'a5_acidification' , 'a6_chgt_climatique'
)

# Santé
sante<-c(
  'a7_couche_ozone' , 'a8_ray_ion' , 'a9_form_oz' ,
  'a10_particules' , 'a11_toxico_non_cancer' , 'a12_toxico_cancer'
)

ressources<-c(
  'a13_utilisation_sol' , 'a14_epuise_eau' ,
  'a15_epuise_nrj' ,'a16_epuise_min' 
)

# Nettoyage de la table de données
##################################

clean<-data%>%
  rename(
    nom = 'Nom du Produit en Français',
    production = 'Type de production',
    score_unique = 'Score unique EF3.1 ',
    # Environnement
    a6_chgt_climatique = 'Changement climatique',
    a2_eutroph_P = "Eutrophisation eaux douces ",
    a3_eutroph_N = "Eutrophisation marine ",
    a4_eutroph_terre = "Eutrophisation terreste",
    a5_acidification = "Acidification terrestre et eaux douces",
    a1_ecotox_eau = "Écotoxicité pour écosystèmes aquatiques d'eau douce",
    # Santé
    a7_couche_ozone = "Appauvrissement de la couche d'ozone ",
    a8_ray_ion = "Rayonnements ionisants ",
    a9_form_oz = "Formation photochimique d'ozone ",
    a10_particules = "Particules ",
    a11_toxico_non_cancer = "Effets toxicologiques sur la santé humaine : substances non-cancérogènes*",
    a12_toxico_cancer = "Effets toxicologiques sur la santé humaine : substances cancérogènes*",
    # Ressources
    a13_utilisation_sol = "Utilisation du sol",
    a14_epuise_eau = "Épuisement des ressources eau",
    a15_epuise_nrj = "Épuisement des ressources énergétiques",
    a16_epuise_min = "Épuisement des ressources minéraux"
  )%>%
  select(
    nom,production,score_unique,
    # Environnement
    a1_ecotox_eau ,
    a2_eutroph_P ,
    a3_eutroph_N ,
    a4_eutroph_terre ,
    a5_acidification ,
    a6_chgt_climatique ,
    # Santé
    a7_couche_ozone ,
    a8_ray_ion ,
    a9_form_oz ,
    a10_particules ,
    a11_toxico_non_cancer ,
    a12_toxico_cancer ,
    # Ressources
    a13_utilisation_sol ,
    a14_epuise_eau ,
    a15_epuise_nrj ,
    a16_epuise_min 
  )%>%
  pivot_longer(
    !c(nom,production),names_to = "impact",values_to = "score" 
  )%>%
  drop_na()%>%
  # Normalisation des variables
  # (hors pêche et aquaculture)
  filter(production!="Pêche")%>%
  filter(production!="Aquaculture")%>%
  # Normalisation par rapport au max
  group_by(impact)%>%
  mutate(mx=max(score))%>%
  ungroup()%>%
  mutate(rel_score=score/mx)%>%
  filter(impact!="score_unique")%>%
  mutate(cl=case_when(
    impact%in%enviro~'enviro',
    impact%in%sante~'sante',
    impact%in%ressources~'ressources'
  ))

# Mise en forme du graphique
############################

# Paramètres pour la disposition des pétales
radius_base <- 0.33  # Distance du centre au centre de chaque ellipse
width_petal <- 0.25  # Largeur des pétales

# Calculer les positions et orientations des ellipses
petals_data <- clean %>%
  arrange(impact,nom)%>%
  group_by(nom) %>%
  mutate(
    angle = seq(0, 2*pi, length.out = n() + 1)[1:n()],
    # Centre de chaque ellipse
    x0 = radius_base * cos(angle),
    y0 = radius_base * sin(angle),
    # Demi-grand axe (selon la valeur)
    a = rel_score*0.2,
    # Demi-petit axe (largeur fixe)
    b = width_petal
  ) %>%
  ungroup()

# Palette de couleur
pal<-c(
  "enviro"="#FF595E",
  "sante"="#330C2F",
  "ressources"="#3891A6"
)

# Selection de certaines productions
sub<-	c(
  'Ananas, production mixte, à la ferme, île de la Réunion',
  'Blé tendre, scénario de base, à la ferme, France',
  'Boeuf, moyenne nationale, à la ferme, France', 
  #'Chou-fleur conventionnel, moyenne nationale, à la ferme, France',
  #'Colza, scénario de base, à la ferme, France',
  'Clémentine, pour exportation, au verger, Souss, Maroc',
  "Fraise conventionnelle, hors sol sous abri chauffé, à la ferme, France",
  "Grain de café (robusta), dépulpés, au Brésil, au départ de l'exploitation, Brésil",
  #"Homard, au débarquement, France",
  #"Lait de vache, système conventionnel, Lait de plaine, maïs ensilage plus de 30%, à la ferme, France",
  "Lait de vache, système conventionnel, montagne, nourri à l'herbe, à la ferme, France",
  'Pomme conventionnelle, non tolérante à la tavelure, au verger, France',
  'Poulet, conventionnel, à la ferme, France',
  'Porc, conventionnel, moyenne nationale, à la ferme, France',
  'Raisin, production intégrée, AOC Beaujolais, au vignoble, France',
  #'Sardine européenne,  Sardina pilchardus, atlantique centre est, senne, au débarquement, France',
  #"Saumon issu d'aquaculture, conventionnel, à la ferme, Norvège",
  #"Thon albacore, Thunnus alalunga, atlantique nord est, chalut pélagique, au débarquement, France",
  "Tomate conventionnelle, calibre intermédiaire, sous abri chauffé, à la ferme, France"
)

# Création du graphique
#######################

ggplot() +
  geom_ellipse(
    data=petals_data%>%filter(nom%in%sub),
    aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle + pi/2, fill=cl,color=cl),
    alpha=0.5
  )+
  facet_wrap(.~nom,ncol=3)+
  guides(fill='none',color='none')+
  scale_fill_manual(values=pal)+
  scale_color_manual(values=pal)+
  coord_fixed()+
  theme_void()+
  theme(
    plot.title=element_blank(),
    strip.text=element_blank()
  )
