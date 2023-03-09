library(tidyverse)
library(phyloseq)
library(camcorder)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)


# Set fonts
font_add_google("Open Sans","open")
font_add_google("Acme","acme")
font_add_google("Fira Sans","fira sans")
font_add_google("Fira Sans Condensed","fira")
font_add_google("Bitter","bit")
font_add_google("Staatliches","staat")
font_add_google("Raleway","ral")

showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

data(GlobalPatterns)

#otu_table(GlobalPatterns)
samp <- sample_data(GlobalPatterns)
#tax_table(GlobalPatterns)

all <- GlobalPatterns%>%
    tax_glom(taxrank = "Order")%>% 
    psmelt() # Melt to long format

# Subsetting
############

nature <- all%>%
  filter(SampleType%in%c("Soil","Freshwater","Freshwater (creek)","Ocean","Sediment (estuary)"))%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

human <- all%>%
  filter(SampleType%in%c("Skin","Tongue","Feces"))%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

soil <- all%>%
  filter(SampleType=="Soil")%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

freshwater <- all%>%
  filter(SampleType%in%c("Freshwater","Freshwater (creek)"))%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

ocean <- all%>%
  filter(SampleType%in%c("Ocean"))%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

sedi <- all%>%
  filter(SampleType%in%c("Sediment (estuary)"))%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

feces <- all%>%
  filter(SampleType=="Feces")%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

skin <- all%>%
  filter(SampleType=="Skin")%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

tong <- all%>%
  filter(SampleType=="Tongue")%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  group_by(Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))

soil_samples <- all%>%
  filter(X.SampleID%in%c('CL3','SV1','CC1'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(
    rel_ab=sum(tot_ab)
    #OTUn = unique(OTU)%>%length(),
    #rel_ab = sum(Abundance)/tot_ab
  )%>%
  ungroup()

sedi_samples <- all%>%
  filter(X.SampleID%in%c('TRRsed1','TRRsed2','TRRsed3'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(
    rel_ab=sum(tot_ab)
    #OTUn = unique(OTU)%>%length(),
    #rel_ab = sum(Abundance)/tot_ab
  )%>%
  ungroup()

freshwater_samples <- all%>%
  filter(X.SampleID%in%c('LMEpi24M','SLEpi20M','AQC1cm','AQC4cm','AQC7cm'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))%>%
  ungroup()

ocean_samples <- all%>%
  filter(X.SampleID%in%c('NP2','NP3','NP5'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))%>%
  ungroup()


feces_samples <- all%>%
  filter(X.SampleID%in%c('M31Fcsw','M11Fcsw','TS28','TS29'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))%>%
  ungroup()

skin_samples <- all%>%
  filter(X.SampleID%in%c('M31Plmr','M11Plmr','F21Plmr'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))%>%
  ungroup()

tong_samples <- all%>%
  filter(X.SampleID%in%c('M31Tong','M11Tong'))%>%
  group_by(X.SampleID)%>%
  mutate(tot_ab=Abundance/sum(Abundance))%>%
  ungroup()%>%
  group_by(X.SampleID,Phylum) %>% 
  summarize(rel_ab=sum(tot_ab))%>%
  ungroup()



# Cleaning subsets for plots
#############################

# First for outer circles

clean_nature <- nature%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )

clean_human <- human%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )

clean_freshwater <- freshwater%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )

clean_ocean <- ocean%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )

clean_sedi <- sedi%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==3 ~ 2,
    id==4 ~ 3,
    id==5 ~ 4,
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )

clean <- soil%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )


clean_feces <- feces%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5)

clean_skin <- skin%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number()
  )%>%
  mutate(id=case_when(
    id==6 ~ 5,
    phyl=="Others"~6,
    TRUE~id
  ))%>%
  arrange(id)%>% 
  mutate(
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )

clean_tong <- tong%>%
  arrange(-rel_ab)%>%
  mutate(id=row_number())%>%
  mutate(phyl=case_when(
    id<6~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  arrange(-ab)%>%
  mutate(
    id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  mutate(type="mean")%>%
  mutate(
    y=4,
    ymin=4-0.35,
    ymax=4+0.5
  )



# Main phyllum for soil & feces
sel<-clean$phyl[1:5]
mat<-tibble(
  phyl=clean$phyl,
  id=clean$id
)

sel_freshwater<-clean_freshwater$phyl[1:5]
mat_freshwater<-tibble(
  phyl=clean_freshwater$phyl,
  id=clean_freshwater$id
)

sel_ocean<-clean_ocean$phyl[1:5]
mat_ocean<-tibble(
  phyl=clean_ocean$phyl,
  id=clean_ocean$id
)

sel_sedi<-clean_sedi$phyl[1:5]
mat_sedi<-tibble(
  phyl=clean_sedi$phyl,
  id=clean_sedi$id
)

sel_feces<-clean_feces$phyl[1:5]
mat_feces<-tibble(
  phyl=clean_feces$phyl,
  id=clean_feces$id
)

sel_skin<-clean_skin$phyl[1:5]
mat_skin<-tibble(
  phyl=clean_skin$phyl,
  id=clean_skin$id
)

sel_tong<-clean_tong$phyl[1:5]
mat_tong<-tibble(
  phyl=clean_tong$phyl,
  id=clean_tong$id
)

# Cleaning individual samples

clean_samples <- soil_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='CL3'~1,
    X.SampleID=='SV1'~2,
    X.SampleID=='CC1'~3
  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

clean_freshwater_samples <- freshwater_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel_freshwater~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat_freshwater)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='LMEpi24M'~-1,
    X.SampleID=='SLEpi20M'~0,
    X.SampleID=='AQC1cm'~1,
    X.SampleID=='AQC4cm'~2,
    X.SampleID=='AQC7cm'~3
    
  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

clean_ocean_samples <- ocean_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel_ocean~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat_ocean)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='NP2'~1,
    X.SampleID=='NP3'~2,
    X.SampleID=='NP5'~3
  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

clean_sedi_samples <- sedi_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel_sedi~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat_sedi)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='TRRsed1'~1,
    X.SampleID=='TRRsed2'~2,
    X.SampleID=='TRRsed3'~3
  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

clean_feces_samples <- feces_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel_feces~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat_feces)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='M31Fcsw'~0,
    X.SampleID=='M11Fcsw'~1,
    X.SampleID=='TS28'~2,
    X.SampleID=='TS29'~3

  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

clean_skin_samples <- skin_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel_skin~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat_skin)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='M31Plmr'~1,
    X.SampleID=='M11Plmr'~2,
    X.SampleID=='F21Plmr'~3
  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

clean_tong_samples <- tong_samples%>%
  mutate(phyl=case_when(
    Phylum%in%sel_tong~Phylum,
    TRUE~"Others"
  ))%>%
  group_by(X.SampleID,phyl)%>%
  summarize(
    ab=sum(rel_ab)
  )%>%
  ungroup()%>%
  left_join(mat_tong)%>%
  group_by(X.SampleID)%>%
  arrange(id)%>%
  mutate(
    #id=row_number(),
    end=cumsum(ab),
    start=end-ab
  )%>%
  ungroup()%>%
  mutate(type="samples")%>%
  mutate(y=case_when(
    X.SampleID=='M31Tong'~2,
    X.SampleID=='M11Tong'~3
  ))%>%
  mutate(
    ymin=y-0.35,
    ymax=y+0.35
  )

tb <- clean%>%
  bind_rows(clean_samples)

tb_freshwater <- clean_freshwater%>%
  bind_rows(clean_freshwater_samples)

tb_ocean <- clean_ocean%>%
  bind_rows(clean_ocean_samples)

tb_sedi <- clean_sedi%>%
  bind_rows(clean_sedi_samples)

tb_feces <- clean_feces%>%
  bind_rows(clean_feces_samples)

tb_skin <- clean_skin%>%
  bind_rows(clean_skin_samples)

tb_tong <- clean_tong%>%
  bind_rows(clean_tong_samples)





######################################

# Making plots

pal <- c(
  "Proteobacteria"="#FF7D00",  
  "Verrucomicrobia"="#D6E681",
  "Acidobacteria"="#ffbe0b",
  "Bacteroidetes"="#01BEFE",  
  "Actinobacteria"="#26547C", 
  "Firmicutes"="#FF006D",
  "Cyanobacteria"="#4CB944",
  "Euryarchaeota"="#a663cc",
  "Crenarchaeota"="#218380",
  "Fusobacteria"="#621708",
  "Others"="grey80"
)



fun_pl_main<-function(tb){
  p1<-ggplot(tb)+
    geom_rect(aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax,fill=fct_reorder(phyl,id),alpha=type))+
    #annotate("segment",x=0,xend=1,y=3.5,yend=3.5,lwd=0.2)+
    coord_polar()+
    guides(alpha="none")+
    scale_y_continuous(limits=c(-3,5))+
    scale_alpha_manual(values=c(1,0.2))+
    scale_fill_manual(values=pal)+
    guides(fill = "none")+
    theme_void()+
    theme(
      plot.margin=margin(1,1,1,1,"cm"),
      legend.position="bottom",
      plot.title = element_text(size=80,family="ral",hjust=0.5,face="bold",margin=margin(1,0,0,0,"cm")),
      legend.title = element_text(size=60,family="bit",face="bold"),
      legend.text = element_text(size=40,family="fira sans"),
      legend.spacing.x = unit(0.5, 'cm')
    )
  return(p1)
}

fun_pl_main(clean_nature)
fun_pl_main(clean_human)


fun_pl<-function(tb){
  p1<-ggplot(tb)+
    geom_rect(aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax,fill=fct_reorder(phyl,id),alpha=type))+
    #annotate("segment",x=0,xend=1,y=3.5,yend=3.5,lwd=0.2)+
    coord_polar()+
    guides(alpha="none",fill="none")+
    scale_y_continuous(limits=c(-3,5))+
    scale_alpha_manual(values=c(1,0.2))+
    scale_fill_manual(values=pal)+
    theme_void()+
    theme(
      plot.margin=margin(1,1,1,1,"cm"),
      legend.position="bottom",
      plot.title = element_text(size=80,family="ral",hjust=0.5,face="bold",margin=margin(1,0,0,0,"cm")),
      legend.title = element_text(size=60,family="bit",face="bold"),
      legend.text = element_text(size=40,family="fira sans"),
      legend.spacing.x = unit(0.5, 'cm')
    )
  return(p1)
}

fun_pl(tb)
fun_pl(tb_freshwater)
fun_pl(tb_ocean)
fun_pl(tb_sedi)

fun_pl(tb_feces)
fun_pl(tb_skin)
fun_pl(tb_tong)

fun_pl(clean_human)

p1<-ggplot(tb)+
  geom_rect(aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax,fill=fct_reorder(phyl,id),alpha=type))+
  #annotate("segment",x=0,xend=1,y=3.5,yend=3.5,lwd=0.2)+
  coord_polar()+
  guides(alpha="none")+
  scale_y_continuous(limits=c(-3,5))+
  scale_alpha_manual(values=c(1,0.2))+
  scale_fill_manual(values=pal)+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5,ncol=2))+
  labs(
    title="Soil samples",
    fill="Phyllum"
  )+
  theme_void()+
  theme(
    plot.margin=margin(1,1,1,1,"cm"),
    legend.position="bottom",
    plot.title = element_text(size=80,family="ral",hjust=0.5,face="bold",margin=margin(1,0,0,0,"cm")),
    legend.title = element_text(size=60,family="bit",face="bold"),
    legend.text = element_text(size=40,family="fira sans"),
    legend.spacing.x = unit(0.5, 'cm')
  )

p2<-ggplot(tb_feces)+
  geom_rect(aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax,fill=fct_reorder(phyl,id),alpha=type))+
  coord_polar()+
  guides(alpha="none")+
  scale_y_continuous(limits=c(-3,5))+
  scale_alpha_manual(values=c(1,0.2))+
  scale_fill_manual(values=pal)+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5,ncol=2))+
  labs(
    title="Feces samples",
    fill="Phyllum"
  )+
  theme_void()+
  theme(
    plot.margin=margin(1,1,1,1,"cm"),
    legend.position="bottom",
    plot.title = element_text(size=80,family="ral",hjust=0.5,face="bold",margin=margin(1,0,0,0,"cm")),
    legend.title = element_text(size=60,family="bit",face="bold"),
    legend.text = element_text(size=40,family="fira sans"),
    legend.spacing.x = unit(0.5, 'cm')
  )


# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 40, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

p1+p2&
  theme(plot.background = element_rect(fill="white",color=NA))
