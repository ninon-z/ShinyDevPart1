# file path

setwd("~/Desktop/zongo gwladys analyse/project")

#upload agricultural data
library(readxl)
data <- read_excel("data/data.xlsx")
data <-as.data.table(data)
#see variables names
#names(data_agri)

#view six first rows of data
#head(data_agri)

data_agri<-data[annee >1999 & annee <2020,]
#names(data)


#redement du mil et du mais et du riz
data_agri<-data_agri %>% dplyr::group_by(Pays,Regions, annee)%>%
  dplyr::mutate(`Rendement du mil`= round((sum(Production_Mil)/superficie_Mil),2),
                   `Rendement du riz`= round((sum(Production_Riz)/superficie_Riz),2),
                `Rendement du maïs`= round((sum(production_Mais)/Superficie_Mais),2))

data_agri<-as.data.table(data_agri)

data_agri[Regions=="Sahel",station_names:="Dori"]
data_agri[Regions=="Nord"|Regions=="Centre-Nord",station_names:="Ouahigouya"]
data_agri[Regions=="Centre"|Regions=="Plateau Central",station_names:="Ouagadougou"]
data_agri[Regions=="Boucle Du Mouhoun",station_names:="Dedougou"]
data_agri[Regions=="Est",station_names:="Fada N'Gourma"]
data_agri[Regions=="Hauts-Bassins"|Regions=="Cascades",station_names:="Bobo-Dioulasso"]
data_agri[Regions=="Centre-Ouest",station_names:="Boromo"]
data_agri[Regions=="Centre-Sud"|Regions=="Centre-Est",station_names:="Po"]
data_agri[Regions=="Sud-Ouest",station_names:="Gaoua"]
data_agri[,table(Regions)]
data_agri[,table(station_names)]


data_agri[Regions=="Sahel",station_ID:=65501]
data_agri[Regions=="Nord"|Regions=="Centre-Nord",station_ID:=65502]
data_agri[Regions=="Centre"|Regions=="Plateau Central",station_ID:=65503]
data_agri[Regions=="Boucle Du Mouhoun",station_ID:=65505]
data_agri[Regions=="Est",station_ID:= 65507]
data_agri[Regions=="Hauts-Bassins"|Regions=="Cascades",station_ID:=65510]
data_agri[Regions=="Centre-Ouest",station_ID:=65516]
data_agri[Regions=="Centre-Sud"|Regions=="Centre-Est",station_ID:=65518]
data_agri[Regions=="Sud-Ouest",station_ID:=65522]


##


##############################################################################################################################


#geographique data

######################################
library(sf)
region <- st_read(dsn = "data/gadm36_BFA_shp/gadm36_BFA_1.shp", stringsAsFactors = F)

mergedData <- merge(x=region, y=data_agri, by.x="NAME_1",by.y="Regions")
#names(mergedData)


#str(region)

#library(data.table)
#region <-as.data.table(region)

#names(region)

#base_finale[,table(station_names)]

#ajout des nom des station synoptiques a la base region
region$station_names[region$NAME_1=="Sahel"]<- "Dori"
region$station_names[region$NAME_1=="Nord"|region$NAME_1=="Centre-Nord"]<- "Ouahigouya"
region$station_names[region$NAME_1=="Centre"|region$NAME_1=="Plateau-Central"]<- "Ouagadougou"
region$station_names[region$NAME_1=="Boucle du Mouhoun"]<- "Dedougou"
region$station_names[region$NAME_1=="Est"]<- "Fada N'Gourma"
region$station_names[region$NAME_1=="Haut-Bassins"|region$NAME_1=="Cascades"]<- "Bobo-Dioulasso"
region$station_names[region$NAME_1=="Centre-Ouest"]<- "Boromo"
region$station_names[region$NAME_1=="Centre-Sud"|region$NAME_1=="Centre-Est"]<- "Po"
region$station_names[region$NAME_1=="Sud-Ouest"]<- "Gaoua"

#ajout des code des stations synoptiques
region$station_ID[region$NAME_1=="Sahel"]<-65501
region$station_ID[region$NAME_1=="Nord"|region$NAME_1=="Centre-Nord"]<-65502
region$station_ID[region$NAME_1=="Centre"|region$NAME_1=="Plateau-Central"]<-65503
region$station_ID[region$NAME_1=="Boucle du Mouhoun"]<-65505
region$station_ID[region$NAME_1=="Est"]<-65507
region$station_ID[region$NAME_1=="Haut-Bassins"|region$NAME_1=="Cascades"]<-65510
region$station_ID[region$NAME_1=="Centre-Ouest"]<-65516
region$station_ID[region$NAME_1=="Centre-Sud"|region$NAME_1=="Centre-Est"]<-65518
region$station_ID[region$NAME_1=="Sud-Ouest"]<-65522











table_options <- function() {
  list(
    dom = 'Bfrtip',
    #Bfrtip
    pageLength = 5,
    buttons = list(
      c('copy', 'csv', 'excel', 'pdf', 'print'),
      list(
        extend = "collection",
        text = 'Show All',
        action = DT::JS(
          "function ( e, dt, node, config ) {
          dt.page.len(-1);
          dt.ajax.reload();}"
        )
      ),
      list(
        extend = "collection",
        text = 'Show Less',
        action = DT::JS(
          "function ( e, dt, node, config ) {
          dt.page.len(5);
          dt.ajax.reload();}"
        )
      )
    ),
    deferRender = TRUE,
    lengthMenu = list(c(10, 20,-1), c('10', '20', 'All')),
    searching = FALSE,
    editable = TRUE,
    scroller = TRUE,
    scrollX = TRUE,
    lengthChange = FALSE ,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#5A5E6B', 'color': '#fff'});",
      "}"
    )
  )
}
