

setwd("~/Desktop/zongo gwladys analyse/project")

source("appfiles/packageLoad.R")
source("appfiles/global.R")

#************************************************************************************************************************
#credentials----
credentials <- data.frame(
  user = c("me","shiny", "shinymanager", "zonglas@gmail.com"), # mandatory
  password = c("me","azerty", "12345", "passtest"),
  admin=c(TRUE,FALSE,FALSE,FALSE),# mandatory
 # start = c("2019-04-15"), # optinal (all others)
  #expire = c(NA, "2019-12-31"),
  #admin = c(FALSE, TRUE),
  #  comment = "Simple and secure authentification mechanism 
  # for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE)

#**********************************
create_db(
  credentials_data = credentials,
  sqlite_path="database.sqlite"
)


#***********************************************************************************************************************
#ui----



ui <- secure_app(enable_admin = TRUE,
                 theme = shinythemes::shinytheme("spacelab"),
                 #tag_top = tags$img(
                  # src = "image_v/medicine-5677000_1920.jpg", width = 100
                #),
                language = "fr",
                 choose_language = TRUE,
                 background = "
                    url('images/fond.jpeg') ;",
   
                 shinyUI(navbarPage(title = "DASHBOARD TEST",
                                      
                                      
                                     #### div(img(src='image_v/analysis.jpg',
                                                 ####   style=" margin-top: -35px;
                              #### padding-right:10px;
                               ###padding-bottom:10px",
                                               ####     height = 60),"GESTION DES STOCKS"),
                                    #theme = "journal",
                                   #### windowTitle="GESTION DES STOCKS",
                   
                   
                  # title = div(img(src='image_v/analysis.jpg',height = 60),"GESTION DES STOCKS"), 
                   id = "inTabset",# theme = shinytheme("united"),#spacelab
                                    #windowTitle = "Gestion des stocks",
                                  
                                    
                                 
                                    
                                    #tags$head(tags$style(HTML('.navbar-static-top {background-color: green;}',
                                                           #  '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
                                  
                                    header = tagList(useShinydashboard()
                                                     
                                                     ),
                                
                           #theme = "style/style2.css",
                           footer = includeHTML("www/footer.html"),
                           fluid = TRUE, 
                           collapsible = TRUE,
                           
                  
#ACCEUIL ----------------------------------
                           tabPanel("ACCUEIL",icon = icon("home"),
                                    includeHTML("www/home.html"),
                                    tags$script(src = "plugins/scripts.js"),
                                    tags$head(
                                        tags$link(rel = "stylesheet", 
                                                  type = "text/css", 
                                                  href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                                        tags$link(rel = "icon",
                                                  type = "image/png",
                                                  href = "images/Statistics-PNG-Free-Download.png") 
                                    )
                           ),
                           
# "DESCRIPTION DES DONNEES----------------------------------
                         tabPanel("GÉNÉRALITÉS", value = "generalite_id",
                                 
                                  br(),
                             
                                  fluidRow(
                                    summaryBox2("test test", "testetsts", width = 3, icon = "fas fa-calendar", style = "info"),
                                    summaryBox2("test test test", "1", width = 3,  style = "success"), #icon = "fas fa-dollar-sign",
                                    summaryBox2("testest test", "13", width = 3, icon = "fas fa-clipboard-list", style = "danger"),
                                    summaryBox2("test test test", " ", width = 3, icon = "fas fa-comments", style = "primary")
                                  ),
                                  
                                 
                                  
                                 includeHTML("www/General.html")
                                  #   neighborhoodDescription(),
                                  # includeHTML("scrollToTop.html")
                                 
                                 
                                 
                               #   fluidRow( withSpinner(valueBoxOutput(1, width = 2),type = 4,
                                         # color = "#d33724",size = 0.7), valueBoxOutput("35", width = 2))
                                  
                                
                                 
                                   ),
#PRODUIT----------------------------------------------------                        
                         #  tabPanel("PRODUIT",
                                 #   neighborhoodDescription(),
                         
                          # ),
                           
#INVENTAIRE--------------------------------------------------
                           tabPanel("STATISTIQUE DES DONNÉES AGRICOLE",
                                    column( width = 12,
                                    fluidRow(column(width = 4),
                                      column(width = 4, #box(width = 12,background = "black",
                                             #   h3(
                                             #   id = "tour_6",
                                             #     class = "well-title",
                                             #    ""
                                             # ),
                                             fluidRow(
                                               
                                               column(width = 4, id = "tour_7.1",
                                                      selectInput( inputId = "Pays_id",
                                                                   label = h3("PAYS:"),
                                                                   choices = data_agri[,unique(sort(Pays))],
                                                                   selected = '')
                                               ),
                                               
                                               
                                               
                                               column(width = 4,id = "tour_9.1",
                                                      selectInput( inputId = 'Annee_id',
                                                                   choices= data_agri[,unique(sort(annee))], 
                                                                   label =h3("ANNEE:"),
                                                                   selected = '2021', multiple = FALSE, selectize = TRUE)
                                               ),
                                               
                                               column(width =4 ,id = "tour_8",uiOutput("region"))
                                              # column(width = 4,id = "tour_8",
                                                     # selectInput( inputId = 'Region_id',
                                                                   #choices= data_agri[,unique(sort("Regions"))], 
                                                                   #label =h3("REGIONS:"),
                                                              #     selected = '', multiple = FALSE, selectize = TRUE)
                                              # ),
                                             )
                                        #)
                                      )
                                    )
                                    ),
                                    
                                    fluid_design("antimicrobials_panel", "box2", "box1", "box3", "box4")
                                    
                                   # propertyComparison()
                           ),
                           
#MOUVEMENT DE STOCK"-----    
                              tabPanel("STATISTIQUE DES DONNÉES CLIMATIQUE",
                                   column(width = 12,
                                    fluidRow(
                                      
                                      
                                    )), 
                                    fluid_design("outcome_panel", "box7", "box8", "box11", "box12")
                                    
                                    
                                    # propertyComparison()
                           ),

                         
#COMMANDE PRODUIT-----------------------------------------------------
                           
                           tabPanel("CARTE",
                                 # column( 
                                 column(width = 12,
                                   fluidRow(
                                     
                                     
                                   )
                                   
                                 ),
                                 fluid_design("diagnostics_panel", "box5", "box6", "box9", "box10")
                                 
                                            
                                    # propertyComparison()
                           ),
#SORTIE----------------------------------------------------
tabPanel("TABLE DES DONNEES",
         
 fluid_design("diagnostics_data2", "data1", "data2", "data3", "data4")  
         # propertyComparison()
),

                         
                         
#aide-------------------------
                         
                         #tabPanel("About",
                                  #   neighborhoodDescription(),
                                  #includeHTML("scrollToTop.html")
                        # ),
                           
# About----------------------------------  
                           tabPanel("Aide",
                                   includeHTML("www/help.html"),
                                    shinyjs::useShinyjs(),
                                    tags$head(
                                        tags$link(rel = "stylesheet", 
                                                  type = "text/css", 
                                                  href = "plugins/carousel.css"),
                                        tags$script(src = "plugins/holder.js")
                                    ),
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                    )
                           )
                           
))
)

#server----------------------------------------
server <- function(input, output,session) {
  
  #credential server----------------
  #******************************************************************************************************************
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite"))
    Sys.sleep(1) # si inaction pendant un moment deconnexion automatique à la page de connction
  
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  

  # reactive in select input 
  reac_pays_region<-reactive({ data_agri[Pays==input$Pays_id,][annee==input$Annee_id,]})
  # reac_tbdata<-reactive({ Commande_Produit[district==input$District,][date >= input$dateRange[1] & date <= input$dateRange[2]]})
  output$region = renderUI({
    selectInput('Region_id', h3('REGIONS:'), choices = unique(c("PAYS",as.character( reac_pays_region()[,sort(unique(Regions))]))),multiple = TRUE ,selected = "PAYS")
  })
  
  
  
 
  #graphique
  output$graph1<-renderHighchart({
    # if (input$period=="Mois"){
    if(input$type=="Production"){
      if(input$Region_id!="PAYS"){
        #cols <- viridis(3)
        cols <- c("#BBACAC", "#9fbdc1","#E9C9B1")
        reac_pays_region()[Regions%in%input$Region_id,] %>%
          dplyr::select(Pays,Regions,annee,Production_Mil,Production_Riz,production_Mais)%>%
        dplyr::group_by(Pays,Regions,annee)%>%
          dplyr:: summarise(Production_Riz= round(sum(Production_Riz),2),production_Mais=round(sum(production_Mais),2),
                            Production_Mil=round(sum(Production_Mil),2) ) %>%
          gather(key = "variable", value = "value", -c("Pays","Regions","annee"))%>%
        as.data.table() %>%
          
          hchart('column', hcaes(x =variable  , y = value ,group=Regions))%>%
          hc_xAxis(title = list(text=input$Regions_id))%>%
          hc_colors(cols)%>%
          
          hc_title(text = paste0("Production agricole du mil, maïs et du riz, année", " : ", input$Annee_id), align = "center")%>%
          hc_plotOptions(
            series = list(
              boderWidth = 2,
              dataLabels = list(enabled = TRUE)
            )
          )%>%hc_exporting(enabled = TRUE)
      }
      else {
        reac_pays_region()%>%
          dplyr:: select(Pays,annee,Production_Mil,Production_Riz,production_Mais)%>%
        dplyr::group_by(Pays,annee)%>%
          dplyr:: summarise(Production_Riz= round(sum(Production_Riz)/n(),2),production_Mais=round(sum(production_Mais)/n(),2),
                            Production_Mil=round(sum(Production_Mil)/n(),2) ) %>%
          gather(key = "variable", value = "value", -c("Pays","annee"))%>%
        as.data.table() %>%
          
          hchart('column', hcaes(x =variable  , y = value))%>%hc_xAxis(title = list(text=input$Pays_id))%>%
          hc_colors("#9fbdc1")%>%
          hc_title(text = paste0("Production moyenne du mil maïs et du riz, année"," : ", input$Annee_id," "))%>%
          hc_plotOptions(
            series = list(
              boderWidth = 2,
              dataLabels = list(enabled = TRUE)
            )
          )%>%hc_exporting(enabled = TRUE)
      }
    }
    else if(input$type=="Superficie"){ 
      
      if(input$Region_id!="PAYS"){
        #cols <- viridis(3)
        cols <- c("#BBACAC", "#9fbdc1","#E9C9B1")
        reac_pays_region()[Regions%in%input$Region_id,] %>%
          dplyr::select(Pays,Regions,annee,superficie_Mil,Superficie_Mais,superficie_Riz)%>%
        dplyr::group_by(Pays,Regions,annee)%>%
          dplyr:: summarise_if(is.numeric,sum) %>%
          gather(key = "variable", value = "value", -c("Pays","Regions","annee"))%>%
        as.data.table() %>%
          
          hchart('column', hcaes(x =variable  , y = value, group=Regions))%>%
          hc_xAxis(title = list(text=input$csps))%>%
          hc_colors(cols)%>%
          
          hc_title(text = paste0("Les superficies emblavées, année", " : ", input$Annee_id), align = "center")%>%
          hc_plotOptions(
            series = list(
              boderWidth = 2,
              dataLabels = list(enabled = TRUE)
            )
          )%>%hc_exporting(enabled = TRUE)
      }
      else {
        reac_pays_region()%>%
          dplyr::select(Pays,annee,superficie_Mil,Superficie_Mais,superficie_Riz)%>%
        dplyr::group_by(Pays,annee)%>%
          dplyr:: summarise(superficie_Mil= round(sum(superficie_Mil)/n(),2),Superficie_Mais=round(sum(Superficie_Mais)/n(),2),
                            superficie_Riz=round(sum(superficie_Riz)/n(),2) )%>%
          gather(key = "variable", value = "value", -c("Pays","annee"))%>%
        as.data.table() %>%
          hchart('column', hcaes(x =variable  , y = value))%>%hc_xAxis(title = list(text=input$Pays_id))%>%
          hc_colors("#9fbdc1")%>%
          hc_title(text = paste0("Les superficies moyenne emblavées, année"," : ", input$Annee_id," "))%>%
          hc_plotOptions(
            series = list(
              boderWidth = 2,
              dataLabels = list(enabled = TRUE)
            )
          )%>%hc_exporting(enabled = TRUE)
      }
    }
    
    else if(input$type=="Rendement"){ 
      
      
      if(input$Region_id!="PAYS"){
        #cols <- viridis(3)
        cols <- c("#BBACAC", "#9fbdc1","#E9C9B1")
        reac_pays_region()[Regions%in%input$Region_id,] %>%
          dplyr::select(Pays,Regions,annee,`Rendement du mil`,`Rendement du riz`,`Rendement du maïs`) %>% 
          dplyr::group_by(Pays,Regions,annee)%>%
          dplyr:: summarise(`Rendement du mil`= round(sum(`Rendement du mil`),2),`Rendement du riz`=round(sum(`Rendement du riz`),2),
                            `Rendement du maïs`=round(sum(`Rendement du maïs`),2) )%>%
          gather(key = "variable", value = "value", -c("Pays","Regions","annee"))%>%
        as.data.table()%>%
          
          hchart('column', hcaes(x =variable  , y = value,group=Regions))%>%
          hc_xAxis(title = list(text=input$Region_id))%>%
          hc_colors(cols)%>%
          
          hc_title(text = paste0("Les rendement des céréales, année", " : ", input$Annee_id), align = "center")%>%
          hc_plotOptions(
            series = list(
              boderWidth = 2,
              dataLabels = list(enabled = TRUE)
            )
          )%>%hc_exporting(enabled = TRUE)
      }
      else {
        cols <- c("#BBACAC", "#9fbdc1","#E9C9B1")
        reac_pays_region()%>%
          dplyr::select(Pays,annee,`Rendement du mil`,`Rendement du riz`,`Rendement du maïs`) %>% 
          dplyr::group_by(Pays,annee)%>%
          dplyr:: summarise(`Rendement du mil`= round(sum(`Rendement du mil`)/n(),2),`Rendement du riz`=round(sum(`Rendement du riz`)/n(),2),
                            `Rendement du maïs`=round(sum(`Rendement du maïs`)/n(),2) ) %>%
          gather(key = "variable", value = "value", -c("Pays","annee"))%>%
          as.data.table()%>%
          hchart('column', hcaes(x =variable  , y = value))%>%hc_xAxis(title = list(text=input$Pays_id))%>%
          hc_colors("#9fbdc1")%>%
          hc_title(text = paste0("Les rendement des céréales, année"," : ", input$Annee_id," "))%>%
          hc_plotOptions(
            series = list(
              boderWidth = 2,
              dataLabels = list(enabled = TRUE)
            )
          )%>%hc_exporting(enabled = TRUE)
      }
      
    }
    
    #}
    
  })
  
  
 
  reac_pays_region1<-reactive({ data_agri[Pays==input$Pays_id,]})
  ################################################################################
  #graphique2
  output$graph2<-renderHighchart({
 
        if(input$Region_id!="PAYS"){
          #cols <- viridis(3)
          cols <- c("#BBACAC", "#9fbdc1","#E9C9B1")
          reac_pays_region1()[Regions%in%input$Region_id,] %>%
            dplyr::select(Pays,Regions,annee,`Rendement du mil`,`Rendement du riz`,`Rendement du maïs`) %>% 
            dplyr::group_by(Pays,Regions,annee)%>%
            dplyr:: summarise(`Rendement du mil`= round(sum(`Rendement du mil`),2),`Rendement du riz`=round(sum(`Rendement du riz`),2),
                              `Rendement du maïs`=round(sum(`Rendement du maïs`),2) )%>%
            gather(key = "variable", value = "value", -c("Pays","Regions","annee"))%>%
            as.data.table() %>%
            
            hchart('line', hcaes(x =  annee, y = value,group=variable))%>%
            hc_xAxis(title = list(text=input$Regions_id))%>%
            hc_colors(cols)%>%
            
            hc_title(text = paste0("Evolution du rendement", " : ", input$Pays_id), align = "center")%>%
            hc_plotOptions(
              series = list(
                boderWidth = 2,
                dataLabels = list(enabled = TRUE)
              )
            )%>%hc_exporting(enabled = TRUE)
        }
        else {
          cols <- c("#BBACAC", "#9fbdc1","#E9C9B1")
          reac_pays_region1()%>%
            dplyr::select(Pays,annee,`Rendement du mil`,`Rendement du riz`,`Rendement du maïs`) %>% 
            dplyr::group_by(Pays,annee)%>%
            dplyr:: summarise(`Rendement du mil`= round(sum(`Rendement du mil`)/n(),2),`Rendement du riz`=round(sum(`Rendement du riz`)/n(),2),
                              `Rendement du maïs`=round(sum(`Rendement du maïs`)/n(),2) )%>%
            gather(key = "variable", value = "value", -c("Pays","annee"))%>%
            as.data.table()%>%
            hchart('line', hcaes(x =  annee, y = value,group=variable))%>%hc_xAxis(title = list(text=input$Pay_id))%>%
            hc_colors(cols)%>%
            hc_title(text = paste0("Evolution des rendements moyen"," : ", input$Pay_id," "))%>%
            hc_plotOptions(
              series = list(
                boderWidth = 2,
                dataLabels = list(enabled = TRUE)
              )
            )%>%hc_exporting(enabled = TRUE)
        }
      
    
  })
  
  
  
  ##############################################################
  #graph3
  output$graph3 <- renderPlot({
    plot(st_geometry(region),col="lightgray", border="black", lwd=0.5)
  })
  
 
  ###############################################################
  #graph4
  output$graph4<-renderPlot({
    fra_map <- ggplot(region) + 
      geom_sf(aes(fill = station_names), show.legend = TRUE, color = "white", size = 0.2) + 
      
      
      # enlever l'affichage des coordonnés et de la grille
      coord_sf(datum = NA, expand = FALSE) +
      # scale_fill_viridis(discrete = TRUE, option = "D") +
      ggtitle("Répartion des stations dans les 13 régions du Burkina Faso") +
      geom_sf_text(data = region, aes(label = NAME_1))+
      
      
      #geom_text(region,aes(label=NAME_1))+
      theme(panel.background = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16,))
      
    fra_map
  })
  
  
  ############################################################
  output$graph5<-renderPlot({
    p<-ggplot(mergedData) + geom_sf(aes(fill = `Rendement du maïs`))+
      
      theme(panel.background = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16))+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      scale_fill_distiller("Rendement",palette = "green", direction = 1) +
      ggtitle("Maïs")
    
    p
  
  })
  
  #######################################################
  output$data_1<-DT::renderDataTable({
    data_agri  %>%
      
      DT::datatable(
        rownames = FALSE,
        editable = TRUE,
        class = 'cell-border',
        escape = FALSE,
        # container = table_frame(),
        options = table_options(),
        extensions = 'Buttons'
      )
    #})
    
    
    # )
  })#mois
  
  
  
  
  #################################################################
  observeEvent(input$gogeneralite, {
    updateTabsetPanel(session, "inTabset",
                      selected = "generalite_id")
  })

 

 
 
 

 #######boby element------
 #**************************************************************************************************************
 # box 5----
 
 observeEvent(input$box5.1,{
   if(input$box5.1 == 'Fitre 1'){
     shinyjs::hide('PRO')}
   else {
     shinyjs::show('PRO')
   }})
 
 #**************
 observeEvent(input$box5.1.1,{
   if(input$box5.1.1 == 'Fitre 1'){
     shinyjs::hide('PRO1')}
   else {
     shinyjs::show('PRO1')
   }})
 
 #**************
 output$box5 <- renderUI({
 
   div( style = "position: relative",
     tabBox(id = "box5",
       width = NULL,
       height = 450,
       tabPanel(title = "test test test test test test test test",
         div(style = "position: absolute; left: 0.5em; bottom: 0.5em;",
           dropdown(
             radioGroupButtons(
               inputId = "box5.1",
               #label = "Change time", 
               choiceNames = c("Tous les second filtre", "second filtre"), 
               choiceValues = c("Fitre 1", "second filtre"), 
               selected = "Fitre 1", 
               status = "success",
               direction = "vertical"
             ),
             uiOutput("PRO"),
             
            # selectInput( inputId = 'Produit',
                      #    choices= Commande_Produit[,sort(nom_produit)], 
                        #  label =h5("PRODUIT:"),
                          
                         # selected = '', multiple = FALSE, selectize = TRUE),
        
             size = "xs",
             icon = icon("gear", class = "opt"), 
             up = TRUE
           )
         ),
       
         withSpinner(
           plotOutput("graph5", height = 350),
           #highchartOutput("graphique_tb1_test", height = 300),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       ),
       
       tabPanel(title = "test test test test test test test test",
              
         div(style = "position:absolute;left:0.5em;bottom: 0.5em;",
           dropdown(
             radioGroupButtons(
               inputId = "box5.1.1",
               label = "", 
               choiceNames = c("Tous les second filtre", "second filtre"), 
               choiceValues = c("Fitre 1", "second filtre"), 
               selected = "Fitre 1", 
               status = "success",
               direction = "vertical"
             ),
             uiOutput("PRO1"),
             #downloadButton(outputId = "down_box_6", label = "Télécharger Graphique"),
             size = "xs",
             icon = icon("gear", class = "opt"), 
             up = TRUE
           )
         ),
         withSpinner(
           plotlyOutput("", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       )
     )
   )
 })
 
 
 #******************************************************************************************************
 # box6----
 
 
 observeEvent(input$box6.1,{
   if(input$box6.1 == 'Fitre 1'){
     shinyjs::hide('TRA')
     shinyjs::hide('PRO2')
   }
  else if(input$box6.1 == 'second filtre'){
     shinyjs::hide('TRA')
     shinyjs::show('PRO2')
   }
   else {
     shinyjs::show('TRA')
     shinyjs::hide('PRO2')
   }})
 
 
 #*****************
 output$box6 <- renderUI({
   
   div(style = "position: relative",
     tabBox(id = "box6",
       width = NULL,
       height = 450,
       tabPanel(title = "test test test test test test test test",
         div( style = "position: absolute; left: 0.5em; bottom: 0.5em;",
           dropdown(
             label = "cliquez ici après avoir selectionné la région", 
             radioGroupButtons(
               inputId = "box6.1",
               #label = "cliquez ici après avoir selectionné le csps", 
               choiceNames = c( "second filtre", "Traceur"), 
               choiceValues = c("second filtre", "traceur"), 
               selected = "second filtre", 
               status = "success",
               direction = "vertical"
             ),
             uiOutput("PRO2"),
             uiOutput("TRA"),
             
             # selectInput( inputId = 'Produit',
             #    choices= Commande_Produit[,sort(nom_produit)], 
             #  label =h5("PRODUIT:"),
             
             # selected = '', multiple = FALSE, selectize = TRUE),
             size = "xs",
             icon = icon("gear", class = "opt"), 
             up = TRUE
           )
         ),
         withSpinner(
           plotOutput("graph4", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       ),
       
       tabPanel(title = "test test test test test test test test",
         div(style = "position:absolute;left:0.5em;bottom: 0.5em;",
           dropdown(
             radioGroupButtons(
               inputId = "box6.1.1",
               label = "", 
               choiceNames = c("Tous les second filtre", "second filtre"), 
               choiceValues = c("Fitre 1", "second filtre"), 
               selected = "Fitre 1", 
               status = "success",
               direction = "vertical"
             ),
            # uiOutput("PRO1"),
             #downloadButton(outputId = "down_box_6", label = "Télécharger Graphique"),
             size = "xs",
             icon = icon("gear", class = "opt"), 
             up = TRUE
           )
         ),
         withSpinner(
           plotlyOutput("", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       )
     )
   )
 })
 
 
 #******************************************************************************************************
 
 # box7----
 observeEvent(input$box7.1,{
   if(input$box7.1 == 'Fitre 11'){
     shinyjs::hide('PRO4') }
   else {
     shinyjs::show('PRO4')
   }})
 
 observeEvent(input$box7.2,{
   if(input$box7.2 == 'Fitre 12'){
     shinyjs::hide('PRO5') }
   else {
     shinyjs::show('PRO5')
   }})
 
 output$box7 <- renderUI({
   div(style = "position: relative",
     tabBox(id = "box7",
       width = NULL,
       height = 450,
       tabPanel(title = "test test test test", 
         div(style = "position: absolute; left: 0.5em; bottom: 0.5em;",
           dropdown(
             radioGroupButtons(
               inputId = "box7.1",
               #label = "Change time", 
               choiceNames = c("Tous les second filtre", "second filtre"), 
               choiceValues = c("Fitre 11","second filtre"), 
               selected = "Fitre 11", 
               status = "success",
               direction = "vertical"
             ),
             uiOutput(""),
            
             
             # selectInput( inputId = 'Produit',
             #    choices= Commande_Produit[,sort(nom_produit)], 
             #  label =h5("PRODUIT:"),
             
             # selected = '', multiple = FALSE, selectize = TRUE),
             
             size = "xs",
             icon = icon("gear", class = "opt"), 
             up = TRUE
           )
         ),
         withSpinner(
           plotOutput("", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       ),
       
       tabPanel( title = "test test test test test test test tets tets",
         div(style = "position: absolute; left: 0.5em; bottom: 0.5em;",
           dropdown(
             radioGroupButtons(
               inputId = "box7.2",
               #label = "Change time", 
               choiceNames = c("Tous les second filtre", "second filtre"), 
               choiceValues = c("Fitre 12","second filtre"), 
               selected = "Fitre 12", 
               status = "success",
               direction = "vertical"
             ),
             uiOutput(""),
             
           
             # selectInput( inputId = 'Produit',
             #    choices= Commande_Produit[,sort(nom_produit)], 
             #  label =h5("PRODUIT:"),
             
             # selected = '', multiple = FALSE, selectize = TRUE),
             size = "xs",
             icon = icon("gear", class = "opt"), 
             up = TRUE
             
         )),
         withSpinner(
           plotlyOutput("", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       )
     )
   )
 })
 

 
 #box8----
 observeEvent(input$box8.1,{
   if(input$box8.1 == 'Fitre 13'){
     shinyjs::hide('PRO6') }
   else {
     shinyjs::show('PRO6')
   }})
 
 output$box8 <- renderUI({
   div(style = "position: relative",
     tabBox(id = "box8",
       width = NULL,
       height = 450,
       tabPanel(title = "test test test test test test test test test", 
         div(style = "position: absolute; left: 0.5em; bottom: 0.5em;",
         
         dropdown(
           radioGroupButtons(
             inputId = "box8.1",
             #label = "Change time", 
             choiceNames = c("Tous les second filtre", "second filtre"), 
             choiceValues = c("Fitre 13","second filtre"), 
             selected = "Fitre 13", 
             status = "success",
             direction = "vertical"
           ),
           uiOutput(""),
           
           
           # selectInput( inputId = 'Produit',
           #    choices= Commande_Produit[,sort(nom_produit)], 
           #  label =h5("PRODUIT:"),
           
           # selected = '', multiple = FALSE, selectize = TRUE),
           size = "xs",
           icon = icon("gear", class = "opt"), 
           up = TRUE
           
         )),
         
         withSpinner(
           plotlyOutput("", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       ),
       
       tabPanel(title = "test test test test test test test test test test test test test ",
         div( style = "position: absolute; left: 4em; bottom: 0.5em;",
           #dropdown(
             #radioGroupButtons(
              # inputId = "box7.2",
              # label = "Select group", 
               #choiceNames = c("All", "Year", "Gender", "Specialty", "Subspecialty", "Origin"),
               #choiceValues = c("fullname", "year", "gender", "specialty", "sub_specialty", "adm_route"), 
               #selected = "fullname", 
               #direction = "vertical"
           #  ),
           #  size = "xs",
            # icon = icon("gear", class = "opt"), 
             #up = TRUE
           #)
         ),
         withSpinner(
           DT::dataTableOutput("micro_table", height = 350),
           type = 4,
           color = "#d33724",
           size = 0.7
         )
       )
     )
   )
 })
 
 
 
 
 
 #box1----
 output$box1 <- renderUI({
   div(style = "position: relative",
     tabBox(id = "box1",
       width = NULL,
       height = 450,
       tabPanel(title = "Indicateur 1",
        # div( style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          # dropdown(
              # radioGroupButtons(
             #  inputId = "box1.1",
              # label = "", 
             # # choiceNames = c("Graphique", "Tableau des données"),
              # choiceValues = c("graphique", "tableau_des_données"),
             #  selected = "graphique", 
              # direction = "vertical"
            # ),
            # size = "xs",
            # icon = icon("gear", class = "opt"), 
           #  up = TRUE
          # )
        # ),
       
        highchartOutput("graph1",height = 300) %>% withSpinner(),
        h6("selectionnez au plus 3 région pour plus de lissibilité",radioButtons("type",h6("selectionner la nature"),
                        choices = list("Production","Superficie","Rendement"),
                        selected = 'Rendement',inline=TRUE))
        
      
          
       ),
       
       
       tabPanel(title = "Indicateur 2",
                # div( style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                # dropdown(
                # radioGroupButtons(
                #  inputId = "box1.1",
                # label = "", 
                # # choiceNames = c("Graphique", "Tableau des données"),
                # choiceValues = c("graphique", "tableau_des_données"),
                #  selected = "graphique", 
                # direction = "vertical"
                # ),
                # size = "xs",
                # icon = icon("gear", class = "opt"), 
                #  up = TRUE
                # )
                # ),
                withSpinner(
                  DT::dataTableOutput("", height = 350),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
       )
     )
   )
 })
 

 
 
 # box2 ----
 
 output$box2 <- renderUI({
   div( style = "position: relative",
     tabBox(id = "box2",
       width = NULL,
       height = 450,
       tabPanel(title = "Indicateur 3",
        # div(style = "position: absolute; left: 0.5em; bottom: 0.5em;",
         #  dropdown(
          #  # radioGroupButtons(
              # inputId = "box2.2",
              # label = "Select time", 
              # choiceNames = c("Graphique", "Tableau de données"),
              # choiceValues = c("graphique", "tableau de données"), 
              # selected = "graphique", 
              # direction = "vertical"
            # ),
         
            # size = "xs",
            # icon = icon("gear", class = "opt"), 
            # up = TRUE
           #)
         #),
        highchartOutput("graph2",height = 350) %>% withSpinner(),
        h6("selectionnez une seul région pour plus de lissibilité"),
                      
        
       )
     )
   )
 })
 


 
 
 
 
 #data1
 # data1----
 output$data1 <- renderUI({
 div(style = "position: relative",
  tabBox(id = "data1_los3",
 width = NULL,
  height = 450,
 tabPanel(
  title = "Table",
# div(
 # style = "position: absolute; left: 0.5em; bottom: 0.5em;"#,
# radioGroupButtons(
  # inputId = "box_los3",
  # label = "Select group", 
  # choiceNames = c("Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Origin"),
 # choiceValues = c("gender", "year", "ab_group", "ab_type", "check", "specialty", "sub_specialty", "adm_route"), 
  # selected = "gender", 
  #  direction = "vertical"
 # ),
 # size = "xs",
  #icon = icon("gear", class = "opt"), 
 # up = TRUE
  # )
 # ),
 withSpinner(
  DT::dataTableOutput("data_1", height = 300),
  type = 4,
  color = "#d33724",
   size = 0.7
  )
   )
  )
  )
 })
 


 
 output$data2 <- renderUI({
   div(style = "position: relative",
       tabBox(id = "data2_los3",
              width = NULL,
              height = 450,
              tabPanel(
                title = "Table",
                
                
                withSpinner(
                  DT::dataTableOutput("", height = 300),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
       )
   )
 })
 
 
 

 
 
 output$data3 <- renderUI({
   div(style = "position: relative",
       tabBox(id = "data3_los3",
              width = NULL,
              height = 400,
              tabPanel(
                title = "Table",
               # div(
                #  style = "position: absolute; left: 0.5em; bottom: 0.5em;"#,
                  # radioGroupButtons(
                  # inputId = "box_los3",
                  # label = "Select group", 
                  # choiceNames = c("Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Origin"),
                  # choiceValues = c("gender", "year", "ab_group", "ab_type", "check", "specialty", "sub_specialty", "adm_route"), 
                  # selected = "gender", 
                  #  direction = "vertical"
                  # ),
                  # size = "xs",
                  #icon = icon("gear", class = "opt"), 
                  # up = TRUE
                  # )
                #),
                withSpinner(
                  DT::dataTableOutput("", height = 300),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
       )
   )
 })
 
 
 

 output$data4 <- renderUI({
   div(style = "position: relative",
       tabBox(id = "data4_los3",
              width = NULL,
              height = 400,
              tabPanel(
                title = "Table",
               # div(
                 # style = "position: absolute; left: 0.5em; bottom: 0.5em;"#,
                  # radioGroupButtons(
                  # inputId = "box_los3",
                  # label = "Select group", 
                  # choiceNames = c("Gender", "Year", "Antimicrobial - Groups", "Antimicrobials", "Diagnostics", "Specialty", "Subspecialty", "Origin"),
                  # choiceValues = c("gender", "year", "ab_group", "ab_type", "check", "specialty", "sub_specialty", "adm_route"), 
                  # selected = "gender", 
                  #  direction = "vertical"
                  # ),
                  # size = "xs",
                  #icon = icon("gear", class = "opt"), 
                  # up = TRUE
                  # )
               # ),
                withSpinner(
                  DT::dataTableOutput("", height = 300),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
                )
              )
       )
   )
 })
 
 
 #box11 -----------------------------------------------------------
 
 output$box11 <- renderUI({
    div(style = "position: relative",
        tabBox(id = "box11",
               width = NULL,
               height = 450,
               tabPanel( title = "Indicateur 4 ",
                         
                         withSpinner(
                            plotlyOutput("", height = 350),
                            type = 4,
                            color = "#d33724",
                            size = 0.7
                         )
               )
        )
    )
 })
 
 #box12 -----------------------------------------------------------
 
 output$box12 <- renderUI({
    div(style = "position: relative",
        tabBox(id = "box12",
               width = NULL,
               height = 450,
               tabPanel( title = "Indicateur 5 ",
                         
                         withSpinner(
                            plotlyOutput("", height = 350),
                            type = 4,
                            color = "#d33724",
                            size = 0.7
                         )
               )
        )
    )
 })
 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
