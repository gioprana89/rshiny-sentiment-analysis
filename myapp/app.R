

#library(webr)
#library(stringi)
#library(stringr)
#library(ggthemes)
#library(plyr)
#library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)
library(readxl)
library(leaflet)
library(sf)
library(shinycssloaders)

library(data.table)

library(stringi)
library(stringr)




library(shinyscreenshot)
library(scales)

library(ggplot2)
library(stringr)
library(shiny)
library(ggthemes)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)

library(tidytext)

library(wordcloud)

library(widyr)
library(ggthemes)

library(tidygraph)




#library(curl)

#library(shinyAce)
#source("chooser.R")
#library(shinyscreenshot)
library(scales)
#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)
#library(ggplot2)

########################################
########UI (User Interface)#############
########################################

modul_dashboard_ui <- function(id) {
  
  
  
  ns <- NS(id)
  

  fluidPage(
    
    
    
    
    dashboardPage(
      
      
      
      dashboardHeader(title = "",
                      titleWidth = "100%"),
      
      
      dashboardSidebar(disable = TRUE),
      
     
      
      dashboardBody(skin = "purple",
        
        
        
        
        
        
        
        
        fluidRow(
          # A static valueBox
          
          box(
            title = "", status = "warning", solidHeader = TRUE,
            collapsible = TRUE, width = "50px",
            withSpinner(valueBoxOutput(ns("number_of_articles_scopus_non_scopus"), width = 3)), 
            
             
                   
            withSpinner(valueBoxOutput(ns("number_of_journal"), width = 3)),
            withSpinner(valueBoxOutput(ns("number_of_scopus_journal"), width = 3)),
            withSpinner(valueBoxOutput(ns("number_of_sinta_journal"), width = 3)),
                   
                   br()
       
            
            
          ), #akhir box
          
          
          
          
      
          
     
          
          
          
          
          
        ), #Akhir fluidrow
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        fluidRow(
          
        
      
        
        box(
          title = "List of Articles", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = "100%",
          
      
          withSpinner(DT::DTOutput(ns("data_artikel_jurnal"))),
          
        ) #akhir box
        
        
        ), #Akhir fluid row
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        # Boxes need to be put in a row (or column)
        fluidRow(
          
          
          box(
            title = "List of Journal", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = "100%",
            
        
            
        
           # leafletOutput(ns("mymap"), width = "100%", height = "900px"  ),
          # withSpinner(leafletOutput(ns("mymap"), width = "100%" , height = "500px" )),
          
          withSpinner(DT::DTOutput(ns("data_list_jurnal_new"))),
          
          
          
          
          
          
          
            
              ) #akhir box
          
          
          
          
        ), #fluidrow
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        # Boxes need to be put in a row (or column)
        fluidRow(
          
          
          box(
            title = "Available Keywords", status = "warning", solidHeader = TRUE,
            collapsible = TRUE, width = "100%",
            
            
            # leafletOutput(ns("mymap"), width = "100%", height = "900px"  ),
            # withSpinner(leafletOutput(ns("mymap"), width = "100%" , height = "500px" )),
            withSpinner(DT::DTOutput(ns("katakunci_yang_tersedia"))),
            
            
            
            
            
          ) #akhir box
          
          
          
          
        ), #fluidrow
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        # Boxes need to be put in a row (or column)
        fluidRow(
          
          
          column(5,
                 
                 
                 box(
                   title = "Select Journal", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = "100%",
                   
                   
                   # leafletOutput(ns("mymap"), width = "100%", height = "900px"  ),
                   # withSpinner(leafletOutput(ns("mymap"), width = "100%" , height = "500px" )),
                   uiOutput(ns("pilih_jurnal")),
                   
                   
              
                 br()
                 
          ) #akhir box
          
          ),
          
          
          
          column(5,
                 
                 
                 box(
                   title = "Select Information", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = "100%",
                   
                   
                   # leafletOutput(ns("mymap"), width = "100%", height = "900px"  ),
                   # withSpinner(leafletOutput(ns("mymap"), width = "100%" , height = "500px" )),
                   uiOutput(ns("pilih_informasi")),
                   
                   
                   
                   br()
                   
                 )#akhir box
                 
          ),
          
          
          
          
          column(2,
                 
                 
                 box(
                   title = "Select Year", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE, width = "100%",
                   
                   
                   # leafletOutput(ns("mymap"), width = "100%", height = "900px"  ),
                   # withSpinner(leafletOutput(ns("mymap"), width = "100%" , height = "500px" )),
                   uiOutput(ns("pilih_tahun")),
                   
                   
                   
                   br()
                   
                 )#akhir box
                 
          ),
                 
          
          
          
          
         
            
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
        ), #fluidrow
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        # Boxes need to be put in a row (or column)
        fluidRow(
          
          
          box(
            title = "Mapping of Keywords", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = "100%",collapsed = TRUE,
            
            
            
            
            
            
            
            
            fluidRow(
              column(4,
                     
                     
                     
                     
                     sliderInput(ns("grafik_coocur_jumlah_coocur_keseluruhan_ya"), "number of co-occurrences:",
                                 min = 1, max = 1000,
                                 value = 100),
                     
                     
                     br()
                     
                     
              ),
              
              
              column(4,
                     
                     
                     sliderInput(ns("grafik_coocur_ukuran_teks_keseluruhan_ya"), "text size:",
                                 min = 1, max = 20,
                                 value = 5, step = 0.1),
                     
                     
                     br()
                     
                     
              ),
              
              
              column(4,
                     
                     radioButtons(ns("grafik_coocur_tipe_grafik_keseluruhan_ya"),
                                  
                                  "Type of Graph:", 
                                  c("1" = "1", "2"="2",
                                    "3"="3", "4"="4", "5" = "5"), inline=TRUE, selected = "1"   ),
                     
                     
                     
                     
                     br()
                     
                     
              )
              
              
              
            ),
            
            
            
            
            
            
            
            
            
            # leafletOutput(ns("mymap"), width = "100%", height = "900px"  ),
            # withSpinner(leafletOutput(ns("mymap"), width = "100%" , height = "500px" )),
            #withSpinner(DT::DTOutput(ns("data_list_jurnal"))),
            
            
            
            shinycssloaders::withSpinner(plotOutput(ns("grafik_pemetaan_kata_kunci_keseluruhan"), width = "100%", height = "900px" )),
            
            
            
          ) #akhir box
          
          
          
          
        ), #fluidrow
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        #######################
        
        
        
        
        
        
        
        
        # Boxes need to be put in a row (or column)
        fluidRow(
          
          
          box(
            title = "Wordcloud of Keywords", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = "100%",collapsed = TRUE,
            
            
            
            
            fluidRow(
              column(4,
                     
                     radioButtons(ns("warna_wordcloud_all"),
                                  
                                  "Theme of Words:", 
                                  c("Blues" = "Blues", "BuGn"="BuGn",
                                    "BuPu"="BuPu", "GnBu"="GnBu", "Greens"="Greens", "YlOrRd"="YlOrRd", "YlOrBr" = "YlOrBr", "YlGnBu" = "YlGnBu",
                                    "Spectral" = "Spectral", "RdYlGn" = "RdYlGn", "YlGn" = "YlGn",
                                    "RdBu" = "RdBu", "RdGy" = "RdGy", "RdYlBu" = "RdYlBu",
                                    "PiYG" = "PiYG", "PRGn" = "PRGn", "PuOr" = "PuOr",
                                    "Purples" = "Purples", "RdPu" = "RdPu", "BrBG" = "BrBG"), inline=TRUE, selected = "Spectral"   ),
                     
                     
                     
                     
                     br()
                     
                     
              ),
              
              
              column(4,
                     
                     
                     
                     
                     sliderInput(ns("max_words_all"), "max.words:",
                                 min = 1, max = 1000,
                                 value = 5),
                     
                     
                     
                     
                     sliderInput(ns("n.brewer.pal_all"), "n.brewer.pal:",
                                 min = 1, max = 100,
                                 value = 10),
                     
                     #n.brewer.pal
                     
                     
                     br()
                     
                     
              ),
              
              
              
              column(4,
                     
                     
                     
                     sliderInput(ns("min_freq_all"), "min.freq:",
                                 min = 1, max = 1000,
                                 value = 1),
                     
                     
                     textAreaInput(ns("rot.per_all"), 
                                   "rot.per", value = "0.35", height = 70, width = 100),
                     
                     
                     #rot.per=0.35
                     
                     
                     #min.freq = 4
                     
                     br()
                     
                     
              )
              
              
              
            ), #Akhir fluidrow
            
            
            
            
            
            shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_full"), width = "100%", height = "900px" ) ),
            
            
            
          ) #akhir box
          
          
          
          
        ), #fluidrow
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        # Boxes need to be put in a row (or column)
        fluidRow(
          
          
          box(
            title = "Find & Analysis Articles Using Keywords", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = "100%",collapsed = TRUE,
            
            
            
            
            
            
            
            
            
            textInput(ns("get_keyword"),
                      "Input One Keyword", 
                      "financial performance"),
            
            br(),
            
            uiOutput(ns("tampilkan_pilihan_variabel_yang_akan_ditampilkan")),
            
            br(),
            
            uiOutput(ns("tampilkan_select_articles")),
            
            
            
            
            br(),
            
            
            uiOutput(ns("tampilkan_data_artikel")),
            
            
            
            
            
            
            
            
            
            uiOutput(ns("tampilkan_analisis_keyword")),
            
            
            
            br(),           
            
            textInput(ns("get_number_of_keyword_display_in_line_chart"),
                      "Number of Keywords in Line Chart", 
                      "5"),
            
            
            
            tabsetPanel(
              
              
              tabPanel("300 x 300",
                       
                       actionButton(ns('cetak_gambar_300_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_300_type1"), width = "300px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 300",
                       
                       
                       actionButton(ns('cetak_gambar_500_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_300_type1"), width = "500px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 300",
                       
                       
                       actionButton(ns('cetak_gambar_700_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_300_type1"), width = "700px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 300",
                       
                       
                       actionButton(ns('cetak_gambar_900_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_300_type1"), width = "900px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 300",
                       
                       
                       actionButton(ns('cetak_gambar_1100_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_300_type1"), width = "1100px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 300",
                       
                       
                       actionButton(ns('cetak_gambar_1200_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_300_type1"), width = "1200px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 300",
                       
                       actionButton(ns('cetak_gambar_1300_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_300_type1"), width = "1300px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 300",
                       
                       actionButton(ns('cetak_gambar_1400_300_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_300_type1"), width = "1400px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              
              
              
              
              
              
              
              
              tabPanel("300 x 500",
                       
                       actionButton(ns('cetak_gambar_300_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_500_type1"), width = "300px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 500",
                       
                       
                       actionButton(ns('cetak_gambar_500_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_500_type1"), width = "500px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 500",
                       
                       
                       actionButton(ns('cetak_gambar_700_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_500_type1"), width = "700px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 500",
                       
                       
                       actionButton(ns('cetak_gambar_900_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_500_type1"), width = "900px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 500",
                       
                       
                       actionButton(ns('cetak_gambar_1100_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_500_type1"), width = "1100px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 500",
                       
                       
                       actionButton(ns('cetak_gambar_1200_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_500_type1"), width = "1200px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 500",
                       
                       actionButton(ns('cetak_gambar_1300_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_500_type1"), width = "1300px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 500",
                       
                       actionButton(ns('cetak_gambar_1400_500_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_500_type1"), width = "1400px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              
              
              
              
              
              
              
              
              tabPanel("300 x 700",
                       
                       actionButton(ns('cetak_gambar_300_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_700_type1"), width = "300px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 700",
                       
                       
                       actionButton(ns('cetak_gambar_500_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_700_type1"), width = "500px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 700",
                       
                       
                       actionButton(ns('cetak_gambar_700_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_700_type1"), width = "700px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 700",
                       
                       
                       actionButton(ns('cetak_gambar_900_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_700_type1"), width = "900px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 700",
                       
                       
                       actionButton(ns('cetak_gambar_1100_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_700_type1"), width = "1100px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 700",
                       
                       
                       actionButton(ns('cetak_gambar_1200_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_700_type1"), width = "1200px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 700",
                       
                       actionButton(ns('cetak_gambar_1300_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_700_type1"), width = "1300px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 700",
                       
                       actionButton(ns('cetak_gambar_1400_700_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_700_type1"), width = "1400px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("300 x 900",
                       
                       actionButton(ns('cetak_gambar_300_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_900_type1"), width = "300px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 900",
                       
                       
                       actionButton(ns('cetak_gambar_500_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_900_type1"), width = "500px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 900",
                       
                       
                       actionButton(ns('cetak_gambar_700_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_900_type1"), width = "700px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 900",
                       
                       
                       actionButton(ns('cetak_gambar_900_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_900_type1"), width = "900px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 900",
                       
                       
                       actionButton(ns('cetak_gambar_1100_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_900_type1"), width = "1100px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 900",
                       
                       
                       actionButton(ns('cetak_gambar_1200_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_900_type1"), width = "1200px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 900",
                       
                       actionButton(ns('cetak_gambar_1300_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_900_type1"), width = "1300px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 900",
                       
                       actionButton(ns('cetak_gambar_1400_900_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_900_type1"), width = "1400px", height = "900px" )),
                       
                       br()
                       
              )
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
            ), #akhir dari tabset panel
            
            
            
            
            
            
            
            
            
            
            ###############Distribusi Frekuensi Data Keyword######################
            
            
            br(),
            
            br(),
            
            
            
            
            uiOutput(ns("tampilkan_frekuensi_keyword")),
            
            
            br(),
            
            
            
            
            
            
            
            #######################Analisis Wordcloud
            
            
            
            
            fluidRow(
              column(4,
                     
                     radioButtons(ns("warna_wordcloud"),
                                  
                                  "Theme of Words:", 
                                  c("Blues" = "Blues", "BuGn"="BuGn",
                                    "BuPu"="BuPu", "GnBu"="GnBu", "Greens"="Greens", "YlOrRd"="YlOrRd", "YlOrBr" = "YlOrBr", "YlGnBu" = "YlGnBu",
                                    "Spectral" = "Spectral", "RdYlGn" = "RdYlGn", "YlGn" = "YlGn",
                                    "RdBu" = "RdBu", "RdGy" = "RdGy", "RdYlBu" = "RdYlBu",
                                    "PiYG" = "PiYG", "PRGn" = "PRGn", "PuOr" = "PuOr",
                                    "Purples" = "Purples", "RdPu" = "RdPu", "BrBG" = "BrBG"), inline=TRUE, selected = "Spectral"   ),
                     
                     
                     
                     
                     br()
                     
                     
              ),
              
              
              column(4,
                     
                     
                     
                     
                     sliderInput(ns("max_words"), "max.words:",
                                 min = 1, max = 1000,
                                 value = 5),
                     
                     
                     
                     
                     sliderInput(ns("n.brewer.pal"), "n.brewer.pal:",
                                 min = 1, max = 100,
                                 value = 10),
                     
                     #n.brewer.pal
                     
                     
                     br()
                     
                     
              ),
              
              
              
              column(4,
                     
                     
                     
                     sliderInput(ns("min_freq"), "min.freq:",
                                 min = 1, max = 1000,
                                 value = 1),
                     
                     
                     textAreaInput(ns("rot.per"), 
                                   "rot.per", value = "0.35", height = 70, width = 100),
                     
                     
                     #rot.per=0.35
                     
                     
                     #min.freq = 4
                     
                     br()
                     
                     
              )
              
              
              
            ), #Akhir fluidrow
            
            
            
            
            
            
            br(),
            
            
            
            
            shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud"), width = "100%", height = "900px")  ),
            
            
            #shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud"), width = "1400px", height = "900px" )),
            
            
            
            
            
            
            
            
            
            
            
            br(),
            
            br(),
            
            br(),
            
            
            
            fluidRow(
              column(4,
                     
                     
                     
                     
                     sliderInput(ns("grafik_coocur_jumlah_coocur"), "number of co-occurrences:",
                                 min = 1, max = 1000,
                                 value = 20),
                     
                     
                     br()
                     
                     
              ),
              
              
              column(4,
                     
                     
                     sliderInput(ns("grafik_coocur_ukuran_teks"), "text size:",
                                 min = 1, max = 20,
                                 value = 3, step = 0.1),
                     
                     
                     br()
                     
                     
              ),
              
              
              column(4,
                     
                     radioButtons(ns("grafik_coocur_tipe_grafik"),
                                  
                                  "Type of Graph:", 
                                  c("1" = "1", "2"="2",
                                    "3"="3", "4"="4", "5" = "5"), inline=TRUE, selected = "1"   ),
                     
                     
                     
                     
                     br()
                     
                     
              )
              
              
              
            ),
            
            ##########5 Oktober 2025############
            
            
            
            
            
            
            #shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci_1"), width = "100%" )),
            
            
            
            
            
            tabsetPanel(
              
              
              tabPanel("300 x 300",
                       
                       #actionButton(ns('cetak_gambar_300_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_300_type1"), width = "300px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 300",
                       
                       
                       #actionButton(ns('cetak_gambar_500_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_300_type1"), width = "500px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 300",
                       
                       
                       #   actionButton(ns('cetak_gambar_700_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_300_type1"), width = "700px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 300",
                       
                       
                       #   actionButton(ns('cetak_gambar_900_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_300_type1"), width = "900px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 300",
                       
                       
                       #  actionButton(ns('cetak_gambar_1100_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_300_type1"), width = "1100px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 300",
                       
                       
                       #  actionButton(ns('cetak_gambar_1200_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_300_type1"), width = "1200px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 300",
                       
                       #  actionButton(ns('cetak_gambar_1300_300_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_300_type1"), width = "1300px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 300",
                       
                       #  actionButton(ns('cetak_gambar_1400_300_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_300_type1"), width = "1400px", height = "300px" )),
                       
                       br()
                       
              ),
              
              
              
              
              
              
              
              
              
              
              
              tabPanel("300 x 500",
                       
                       #  actionButton(ns('cetak_gambar_300_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_500_type1"), width = "300px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 500",
                       
                       
                       #  actionButton(ns('cetak_gambar_500_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_500_type1"), width = "500px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 500",
                       
                       
                       #  actionButton(ns('cetak_gambar_700_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_500_type1"), width = "700px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 500",
                       
                       
                       #   actionButton(ns('cetak_gambar_900_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_500_type1"), width = "900px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 500",
                       
                       
                       #  actionButton(ns('cetak_gambar_1100_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_500_type1"), width = "1100px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 500",
                       
                       
                       #    actionButton(ns('cetak_gambar_1200_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_500_type1"), width = "1200px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 500",
                       
                       #     actionButton(ns('cetak_gambar_1300_500_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_500_type1"), width = "1300px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 500",
                       
                       #    actionButton(ns('cetak_gambar_1400_500_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_500_type1"), width = "1400px", height = "500px" )),
                       
                       br()
                       
              ),
              
              
              
              
              
              
              
              
              
              
              
              tabPanel("300 x 700",
                       
                       #   actionButton(ns('cetak_gambar_300_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_700_type1"), width = "300px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 700",
                       
                       
                       #    actionButton(ns('cetak_gambar_500_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_700_type1"), width = "500px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 700",
                       
                       
                       #     actionButton(ns('cetak_gambar_700_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_700_type1"), width = "700px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 700",
                       
                       
                       #      actionButton(ns('cetak_gambar_900_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_700_type1"), width = "900px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 700",
                       
                       
                       #    actionButton(ns('cetak_gambar_1100_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_700_type1"), width = "1100px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 700",
                       
                       
                       #    actionButton(ns('cetak_gambar_1200_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_700_type1"), width = "1200px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 700",
                       
                       #    actionButton(ns('cetak_gambar_1300_700_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_700_type1"), width = "1300px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 700",
                       
                       #    actionButton(ns('cetak_gambar_1400_700_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_700_type1"), width = "1400px", height = "700px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("300 x 900",
                       
                       #     actionButton(ns('cetak_gambar_300_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_900_type1"), width = "300px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("500 x 900",
                       
                       
                       #      actionButton(ns('cetak_gambar_500_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_900_type1"), width = "500px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("700 x 900",
                       
                       
                       #    actionButton(ns('cetak_gambar_700_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_900_type1"), width = "700px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              
              tabPanel("900 x 900",
                       
                       
                       #    actionButton(ns('cetak_gambar_900_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_900_type1"), width = "900px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1100 x 900",
                       
                       
                       #     actionButton(ns('cetak_gambar_1100_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_900_type1"), width = "1100px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1200 x 900",
                       
                       
                       #     actionButton(ns('cetak_gambar_1200_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_900_type1"), width = "1200px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              tabPanel("1300 x 900",
                       
                       #    actionButton(ns('cetak_gambar_1300_900_type1'),'Print'),
                       
                       br(),
                       
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_900_type1"), width = "1300px", height = "900px" )),
                       
                       br()
                       
              ),
              
              
              
              tabPanel("1400 x 900",
                       
                       #    actionButton(ns('cetak_gambar_1400_900_type1'),'Print'),
                       
                       br(),
                       
                       shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_900_type1"), width = "1400px", height = "900px" )),
                       
                       br()
                       
              )
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
            ), #akhir dari tabset panel
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
          )#Akhir box
          
          
          
        ), #Akhir fluid row
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        fluidRow(
          
          
          
          
          box(
            title = "List of Journal Publication Fees", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = "100%",collapsed = TRUE,
            
            withSpinner(DT::DTOutput(ns("daftar_biaya_publikasi_jurnal"))),
            
            
            
          ) #akhir box
          
          
        ), #Akhir fluid row
        
        
        
        
        
        
        
        
        
        
        
        
        
        br(),

        br(),
        
        br(),
        
        
        
        
        
       # shinycssloaders::withSpinner(verbatimTextOutput(ns("informasi_cek"))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
      ) #akhir dashboardBody
      
      
      
      
      
      
      
    ), #Akhir dashboardpage
    
    
    
    
    br()
    
  ) #Akhir dari fluidpage
    
    
    
    
    
    
    
    
    

  
  
} #Akhir dari modul_dashboard_ui

#Akhir dari modul_dashboard_ui
#Akhir dari modul_dashboard_ui
#Akhir dari modul_dashboard_ui
#Akhir dari modul_dashboard_ui











































































########################################
################Server##################
########################################



modul_dashboard_server <- function(input, output, session) {
  
  
  
  
  ###########Project R-Shiny IDX Dimulai##############
  
  
  
  output$data_artikel_jurnal <- DT::renderDT({
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama

       
    terpilih_nama_informasi <- input$terpilih_nama_informasi
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
     tampilkan_data <- dataset[c(terpilih_nama_informasi)]
        
        
        nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
        
        
        nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
        
        
        nama_lengkap <- tampilkan_data[,"Journal"]
        
        
        
        indeks <-  nama_lengkap %in% nama_jurnal_terpilih
        indeks <- which(indeks == TRUE)
        data_terpilih <- tampilkan_data[c(indeks),]
        
        
        ################Pilih Tahun
        
        terpilih_pilih_tahun <- input$terpilih_pilih_tahun
        terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
        
        tahun_lengkap <- data_terpilih[,"Year"]
        
        indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
        indeks <- which(indeks == TRUE)
        data_terpilih2 <- data_terpilih[c(indeks),]
        
        
  
    print(data_terpilih2)
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$number_of_articles_scopus_non_scopus <- renderValueBox({
    
    dataku <- read_xlsx("data_idx_gabung.xlsx")
    dataku <- as.data.frame(dataku)
    
    jumlah_artikel <- dataku[,1]
    jumlah_artikel <- length(jumlah_artikel)
    
    
    
    
    valueBox(
      
      
      
      jumlah_artikel, "Number of Articles", icon = icon("list"),
      color = "purple", width = "50px"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$number_of_journal <- renderValueBox({
    
    dataku <- read_xlsx("data_idx_gabung.xlsx")
    dataku <- as.data.frame(dataku)
    
    #jumlah_artikel <- dataku[,1]
    #jumlah_artikel <- length(jumlah_artikel)
    
    ambil_nama_jurnal_issn <- dataku[c("Journal", "ISSN", "Scopus")]
    x <- unique(ambil_nama_jurnal_issn)

    jumlah_artikel_jurnal_dalam_database <- length(x[,1])
    
    
    
    valueBox(
      
      
      
      jumlah_artikel_jurnal_dalam_database, "Number of Journal", icon = icon("list"),
      color = "green"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$number_of_scopus_journal <- renderValueBox({
    
    dataku <- read_xlsx("data_idx_gabung.xlsx")
    dataku <- as.data.frame(dataku)
    
    #jumlah_artikel <- dataku[,1]
    #jumlah_artikel <- length(jumlah_artikel)
    
    ambil_nama_jurnal_issn <- dataku[c("Journal", "ISSN", "Scopus")]

    x <- unique(ambil_nama_jurnal_issn)
    
     scopus_journal <- x[complete.cases(x), ]
    
    jumlah_jurnal_scopus <- length(scopus_journal[,1])
    
    
    
    
    
    
    valueBox(
      
      
      
      jumlah_jurnal_scopus, "Number of Scopus Journal", icon = icon("list"),
      color = "red"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$number_of_sinta_journal <- renderValueBox({
    
    dataku <- read_xlsx("data_idx_gabung.xlsx")
    dataku <- as.data.frame(dataku)
    
    #jumlah_artikel <- dataku[,1]
    #jumlah_artikel <- length(jumlah_artikel)
    
    ambil_nama_jurnal_issn <- dataku[c("Journal", "ISSN", "Indexed by Sinta", "Link Sinta")]
    
    x <- unique(ambil_nama_jurnal_issn)
    
    sinta_journal <- x[complete.cases(x), ]
    
    sinta_journal <- length(sinta_journal[,1])
    
    
    
    
    
    
    valueBox(
      
      
      
      sinta_journal, "Number of Sinta Journal", icon = icon("list"),
      color = "aqua"
      
      
      
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$approvalBox <- renderValueBox({
    
    dataset <- read.xlsx("dataset.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    jmlh_pr <- dataset[,7]
    jmlh_pr <- sum(jmlh_pr)
    
    
    valueBox(
      jmlh_pr, "Jumlah Perempuan 2024", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$data_list_jurnal_new <- DT::renderDT({
  
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    terpilih_nama_informasi <- input$terpilih_nama_informasi
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
   # print(data_terpilih2)
    
    
    
    
    
    
    
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    ambil_nama_jurnal_issn <- dataset[c("Journal", "ISSN", "Scopus", "Indexed by Sinta", "Link Sinta")]
    
    x <- unique(ambil_nama_jurnal_issn)
    
    
    print(ambil_nama_jurnal_issn)
    
    
    
    simpan_jumlah_artikel = 0
    jumlah_jurnal <- length(x[,"Journal"])
    nama_jurnal <- x[,"Journal"]
    
    
    for(i in 1 : jumlah_jurnal)
    {
      
      tes <- nama_jurnal[i]
      indeks <- dataset[,"Journal"] %in% tes 
      indeks_terpilih <- which(indeks == TRUE)
      
      simpan_jumlah_artikel[i] <- length(indeks_terpilih)
      
      
    }
    
    
    x <- data.frame(x, simpan_jumlah_artikel)
    colnames(x) <- c("Journal", "ISSN", "Scopus", "Indexed by Sinta", "Link Sinta","Number of Articles")
    
    
    
    
    
    
    
    
    print(x)
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ########################
  
  nama_jurnal <- function(){
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    nama_jurnal <- dataset[,"Journal"]
    
    nama_jurnal <- unique(nama_jurnal)
    
    
   # return(nama_jurnal)
    
    #########################
    simpan_nama = 0
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    x <- dataset[c("Journal", "ISSN", "Link Sinta" , "Scopus")]

    x <- unique(x)
    
    for(i in 1 :  length(nama_jurnal)  )
    {
      for(j in 1 : length(x[,1])   )
      {
        
        if(nama_jurnal[i] == x[j,1]  )
        {
          
          #simpan_nama[i] = paste0(x[j,1],x[j,2],x[j,3],x[j,4] )
          if(!is.na(x[j,4]) && !is.na(x[j,3]))
          {
            
            simpan_nama[i] = paste0(x[j,1],"; ISSN: ",x[j,2], "; Indexed by Scopus & Sinta" )
            next
            
          }
          
          
          
          if(!is.na(x[j,4]))
          {
            
            simpan_nama[i] = paste0(x[j,1],"; ISSN: ",x[j,2], "; Indexed by Scopus" )
            next
            
          }
          if(!is.na(x[j,3]))
          {
            
            simpan_nama[i] = paste0(x[j,1],"; ISSN: ",x[j,2], "; Indexed by Sinta" )
            next
            
          }
          
          
        }
        
        
      }
    }
    
    
    
    return(simpan_nama)
    
    
    
    
    
  }
  
  ###############
  
  
  
  
  output$pilih_jurnal <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_pilih_jurnal"), 
                       label="Journal Available:", choices = c(nama_jurnal()), 
                       selected=c( nama_jurnal()  ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############Pilih Tahun
  
  
  fungsi_nama_tahun <- function(){
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
nama_tahun <- dataset[,"Year"]
nama_tahun <- as.factor(nama_tahun)
nama_tahun <- levels(nama_tahun)


return(nama_tahun)
    
    
    
    
    
  }
  
  
  
  ############
  
  
  
  output$pilih_tahun <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_pilih_tahun"), 
                       label="Year:", choices = c(fungsi_nama_tahun()), 
                       selected=c( fungsi_nama_tahun()  ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################
  
  
  
  ##############Pilih Informasi
  
  
  fungsi_nama_informasi <- function(){
    
    nama <- c(
      
      "No",
     # "Title of Article",
     # "Author(s)",
     # "Year",
      "Volume",
      "Issue",
     # "Journal",
      "ISSN",
      "Source of Article",
     # "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    return(nama)
    
    
    
    
    
  }
  
  
  
  ############
  
  
  
  output$pilih_informasi <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_nama_informasi"), 
                       label="Year:", choices = c(fungsi_nama_informasi()), 
                       selected=c( c()  ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####################Pemetaan Kata Kunci
  
  
  
  
  
  
  output$grafik_pemetaan_kata_kunci_keseluruhan<- renderPlot({
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    terpilih_nama_informasi <- input$terpilih_nama_informasi
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    ########Data Terpilih 2 yang digunakan
    
    
    
    
    
    
    
    ambil_kata_kunci <- data_terpilih2[c("Keywords")]
    
    
    
    for(i in 1 : length(ambil_kata_kunci[,1]))
    {
      
      x <- ambil_kata_kunci[i,1]
      
      simpan_keyword_hapus_spasi <- gsub(" ", "", x)
      simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
      simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
      
      ambil_kata_kunci[i,1] = simpan_keyword_hapus_spasi
      ambil_kata_kunci[i,1] = tolower(ambil_kata_kunci[i,1])
      
      
      
    }
    
    
    data_keywords_terpilih_new <- ambil_kata_kunci
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    
    
    gambar = 0    
    
    # print(title_word_pairs)
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    
    
    gambar = 0    
    
    # print(title_word_pairs)
    
    cooccur <-  title_word_pairs
    
    
    wordnetwork <- head(cooccur, input$grafik_coocur_jumlah_coocur_keseluruhan_ya)
    gm1 <- graph_from_data_frame(wordnetwork)
    gambar1 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) +
      theme(legend.position="none")
    
    
    
    gambar2 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) + theme_classic() +
      theme(legend.position="none")
    
    
    
    
    
    
    gambar3 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(color = "orange", width=0.7) +
      geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                      colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    gambar4 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(color = "#57d3e6", width=0.7) +
      geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                      colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    
    wordnetwork2 <- head(cooccur, input$grafik_coocur_jumlah_coocur_keseluruhan_ya)
    wordnetwork2 <- graph_from_data_frame(wordnetwork2)
    gambar5 <- ggraph(wordnetwork2, layout = "fr") +
      geom_edge_link(aes(width = n, edge_alpha = n), edge_colour = "#ed9de9") +
      geom_node_point(aes(size = igraph::degree(wordnetwork2)), shape = 1, color = "black") +
      geom_node_text(aes(label = name), col = "darkblue", size = input$grafik_coocur_ukuran_teks_keseluruhan_ya) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    # gm1 <- igraph::as_data_frame(wordnetwork)
    #gm1 <- graph_from_data_frame(wordnetwork)
    
    #print("ugiiiiiiiiii")
    #print(wordnetwork)
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "1")
    {
      gambar = gambar1
    }
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "2")
    {
      gambar = gambar2
    }
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "3")
    {
      gambar = gambar3
    }
    
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "4")
    {
      gambar = gambar4
    }
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "5")
    {
      gambar = gambar5
    }
    
    
    
    
    
    
    print(gambar)
    
    
  })
  
  
  
  
  
  
  
  ###########Grafik Wordcloud
  
  
  output$grafik_wordcloud_full <- renderPlot({
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    terpilih_nama_informasi <- input$terpilih_nama_informasi
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    ########Data Terpilih 2 yang digunakan
    
    
    
    
    
    
    
    
    
    
    
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- data_terpilih2[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
     # cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      
      simpan_kata <- c(simpan_kata, X)
      
      
      
    }
    
    
    
    
    simpan_keyword = simpan_kata
    
    
    simpan_keyword_hapus_spasi <- gsub(" ", "", simpan_keyword)
    simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
    simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
    
    
    
    
    
    
    
    text <- simpan_keyword_hapus_spasi
    
    jumlah_teks <- length(text)
    
    
    
    text_df <- data_frame(line = 1:jumlah_teks , text = text)
    
    simpan_kata <- text_df %>%
      unnest_tokens(word, text)
    
    rot.per <- read.csv(text=input$rot.per_all, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <-    simpan_kata %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = input$max_words_all,
                     min.freq = input$min_freq_all,           
                     random.order=FALSE, rot.per = angka_rot.per,            
                     colors=brewer.pal(input$n.brewer.pal_all,    input$warna_wordcloud_all  )))
    
    
    
    
    
    print(p)
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################Kata Kunci yang tersedia
  
  
  
  
  output$katakunci_yang_tersedia <- DT::renderDT({
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    terpilih_nama_informasi <- input$terpilih_nama_informasi
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    ########Data Terpilih 2 yang digunakan
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ambil_keyword <- data_terpilih2[,"Keywords"]
    ambil_keyword <- as.data.frame(ambil_keyword)
    
    simpan_keyword <- vector(mode = "character")
    
    
    for(i in 1 :  length(ambil_keyword[,1])  )
    {
      
      X <- ambil_keyword[i,1]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      simpan_keyword = c(simpan_keyword, X)
      
      
      
      
    }
    
    
    
    
    
    tabel <- table(simpan_keyword)
    nama <- names(tabel)
    
    frekuensi <- unlist(tabel)
    names(frekuensi) <- NULL
    
    frekuensi <- unlist(frekuensi)
    frekuensi <- as.numeric(frekuensi)
    
    
    
    
    
    persentase <- frekuensi / sum(frekuensi) * 100
    
    persentase <- round(persentase, digits = 2)
    
    nama <- unlist(nama)
    
    
    data_tabel <- data.frame(nama, frekuensi, persentase)
    
    colnames(data_tabel) <- c("Keywords", "Frequency", "Percentage (%)")
    
    print(data_tabel)
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #############Analisis Artikel berdasarkan kata Kunci
  
  
  
  
  
  fungsi_kirim_nama_analisis_kata_kunci <- function()
  {
    
      
      nama <- c(
        
        "No",
        #"Title of Article",
        #"Author(s)",
        #"Year",
        "Volume",
        "Issue",
        #"Journal",
        "ISSN",
        "Source of Article",
        #"Keywords",
        "Indexed by Sinta",
        "Link Sinta",
        "Software",
        "Data Analysis Method",
        "Data Analysis Method: Keywords",
        "About Data",
        "Scimago",
        "Scopus",
        "Publisher",
        "Variables",
        "Journal Information"
        
      )
      
      return(nama)
      
  
 
  }
  
  
  
  #############
  
  output$tampilkan_pilihan_variabel_yang_akan_ditampilkan <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_variabel_yang_akan_ditampilkan"), 
                       label="Select Information:", choices = c( fungsi_kirim_nama_analisis_kata_kunci()), 
                       selected=c(""), inline = TRUE )
    
    
    
    
  })
  
  
  ###############
  
  
  
  output$tampilkan_analisis_keyword <- renderUI({
    
    h1("Analysis of Keywords",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         )
    
  })
  
  
  
  
  ############
  
  
  
  
  
  output$tampilkan_select_articles <- renderUI({
    
    h1("Selected Articles",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########################
  
  
  
  
  
  
  output$tampilkan_data_artikel <- renderUI({
    
    shinycssloaders::withSpinner(DT::DTOutput(session$ns("open_data_article")))
    
  })
  
  
  ############
  ############
  
  
  
  output$open_data_article <- DT::renderDT({
    
    data_artikel_terpilih <- fungsi_hitung_artikel_terpilih()
    
    
    ###########
    
    
  
    
    
    
    print(data_artikel_terpilih)
    
    
    
  }) #Akhir renderDT 
  
  
  
  
  ###########################
  
  
  
  
  
  
  
  fungsi_hitung_artikel_terpilih <- function()
  {
    
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    

    
    
    
    terpilih_nama_informasi <- input$terpilih_variabel_yang_akan_ditampilkan
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    
    
    
    
    dat <- data_terpilih2
    
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    
    
    data_terpilih <- dat[c(simpan_indeks),]
    
    
    
    
    
    
    
    
    
    
    
    
    
    return(data_terpilih)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  fungsi_kirim_grafik <- function()
  {
    
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    
    
    
    terpilih_nama_informasi <- input$terpilih_variabel_yang_akan_ditampilkan
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- data_terpilih2
    
    
    
    
    
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    tabel <- table(simpan_kata)
    
    
    nama <- names(tabel)
    
    frekuensi <- unlist(tabel)
    names(frekuensi) <- NULL
    
    frekuensi <- unlist(frekuensi)
    frekuensi <- as.numeric(frekuensi)
    
    
    
    
    
    persentase <- frekuensi / sum(frekuensi) * 100
    
    persentase <- round(persentase, digits = 2)
    
    nama <- unlist(nama)
    
    
    data_tabel <- data.frame(nama, frekuensi, persentase)
    
    data_tabel <- arrange(data_tabel, desc(frekuensi) )
    
    
    
    urutan <- data_tabel[, "nama"]
    
    data_tabel[,1] <- factor(data_tabel[,1], levels = c(urutan) )
    
    
    
    jumlah <- input$get_number_of_keyword_display_in_line_chart
    
    jumlah <- unlist(jumlah)
    jumlah <- as.numeric(jumlah)
    
    data_tabel2 <- data_tabel[c(1:jumlah),]
    
    
    
    library(ggplot2)
    # Basic line plot with points
    p <- ggplot(data = data_tabel2, aes(x = nama, y = frekuensi, group = 1)) +
      geom_line( ) +  geom_point() + coord_flip() + xlab("Keywords") + ylab("Frequency") + theme_base()
    
    return(p)
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  #################
  
  
  
  
  
  
  ##########300 x 300
  
  output$grafik_garis_300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 300
  
  output$grafik_garis_500_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 300
  
  output$grafik_garis_700_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 300
  
  output$grafik_garis_900_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 300
  
  output$grafik_garis_1100_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 300
  
  output$grafik_garis_1200_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 300
  
  output$grafik_garis_1300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 300
  
  output$grafik_garis_1400_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 500
  
  output$grafik_garis_300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 500
  
  output$grafik_garis_500_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 500
  
  output$grafik_garis_700_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 500
  
  output$grafik_garis_900_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 500
  
  output$grafik_garis_1100_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 500
  
  output$grafik_garis_1200_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 500
  
  output$grafik_garis_1300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 500
  
  output$grafik_garis_1400_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 700
  
  output$grafik_garis_300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 700
  
  output$grafik_garis_500_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 700
  
  output$grafik_garis_700_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 700
  
  output$grafik_garis_900_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 700
  
  output$grafik_garis_1100_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 700
  
  output$grafik_garis_1200_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 700
  
  output$grafik_garis_1300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 700
  
  output$grafik_garis_1400_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 900
  
  output$grafik_garis_300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 900
  
  output$grafik_garis_500_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 900
  
  output$grafik_garis_700_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 900
  
  output$grafik_garis_900_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 900
  
  output$grafik_garis_1100_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 900
  
  output$grafik_garis_1200_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 900
  
  output$grafik_garis_1300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 900
  
  output$grafik_garis_1400_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$tampilkan_frekuensi_keyword <- renderUI({
    
    shinycssloaders::withSpinner(DT::DTOutput(session$ns("distribusi_frekuensi_data_keywords")))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$distribusi_frekuensi_data_keywords <- DT::renderDT({
    
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    
    
    
    terpilih_nama_informasi <- input$terpilih_variabel_yang_akan_ditampilkan
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- data_terpilih2
    
    
    
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    tabel <- table(simpan_kata)
    
    
    nama <- names(tabel)
    
    frekuensi <- unlist(tabel)
    names(frekuensi) <- NULL
    
    frekuensi <- unlist(frekuensi)
    frekuensi <- as.numeric(frekuensi)
    
    
    
    
    
    persentase <- frekuensi / sum(frekuensi) * 100
    
    persentase <- round(persentase, digits = 2)
    
    nama <- unlist(nama)
    
    
    data_tabel <- data.frame(nama, frekuensi, persentase)
    
    data_tabel <- arrange(data_tabel, desc(frekuensi) )
    
    
    
    urutan <- data_tabel[, "nama"]
    
    data_tabel[,1] <- factor(data_tabel[,1], levels = c(urutan) )
    
    
    colnames(data_tabel) = c("Keywords", "Frequency", "Percentage (%)")
    
    
    print(data_tabel)
    
    
    
  }) #Akhir renderDT 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ########################Grafik
  
  
  fungsi_grafik_wordcloud <- function()
  {
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    
    
    
    terpilih_nama_informasi <- input$terpilih_variabel_yang_akan_ditampilkan
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- data_terpilih2
    
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    
    
    
    
    simpan_keyword = simpan_kata
    
    
    simpan_keyword_hapus_spasi <- gsub(" ", "", simpan_keyword)
    simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
    simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
    
    
    
    
    
    
    
    text <- simpan_keyword_hapus_spasi
    
    jumlah_teks <- length(text)
    
    
    
    text_df <- data_frame(line = 1:jumlah_teks , text = text)
    
    simpan_kata <- text_df %>%
      unnest_tokens(word, text)
    
    rot.per <- read.csv(text=input$rot.per, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <-    simpan_kata %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = input$max_words,
                     min.freq = input$min_freq,           
                     random.order=FALSE, rot.per = angka_rot.per,            
                     colors=brewer.pal(input$n.brewer.pal,    input$warna_wordcloud  )))
    
    
    
    
    
    return(p)
    
    
    
    
  } #Akhir fungsi grafik
  
  
  
  
  
  
  
  
  
  output$grafik_wordcloud <- renderPlot({
    
    p <- fungsi_grafik_wordcloud()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################
  
  
  
  fungsi_kirim_grafik_pemetaan_kata_kunci1 <- function()
  {
    
    
    
    
    nama <- c(
      
      "No",
      "Title of Article",
      "Author(s)",
      "Year",
      "Volume",
      "Issue",
      "Journal",
      "ISSN",
      "Source of Article",
      "Keywords",
      "Indexed by Sinta",
      "Link Sinta",
      "Software",
      "Data Analysis Method",
      "Data Analysis Method: Keywords",
      "About Data",
      "Scimago",
      "Scopus",
      "Publisher",
      "Variables",
      "Journal Information"
      
    )
    
    dataset <- read.xlsx("data_idx_gabung.xlsx")
    
    
    dataset <- as.data.frame(dataset)
    
    colnames(dataset) <- nama
    
    
    
    
    
    terpilih_nama_informasi <- input$terpilih_variabel_yang_akan_ditampilkan
    terpilih_nama_informasi <- c("Title of Article", "Author(s)", "Year", "Journal", "Keywords", terpilih_nama_informasi)
    
    tampilkan_data <- dataset[c(terpilih_nama_informasi)]
    
    
    nama_jurnal_terpilih <- input$terpilih_pilih_jurnal
    
    
    nama_jurnal_terpilih <- sub(";.*", "", nama_jurnal_terpilih)
    
    
    nama_lengkap <- tampilkan_data[,"Journal"]
    
    
    
    indeks <-  nama_lengkap %in% nama_jurnal_terpilih
    indeks <- which(indeks == TRUE)
    data_terpilih <- tampilkan_data[c(indeks),]
    
    
    ################Pilih Tahun
    
    terpilih_pilih_tahun <- input$terpilih_pilih_tahun
    terpilih_pilih_tahun <- as.numeric(terpilih_pilih_tahun)
    
    tahun_lengkap <- data_terpilih[,"Year"]
    
    indeks <-  tahun_lengkap %in% terpilih_pilih_tahun
    indeks <- which(indeks == TRUE)
    data_terpilih2 <- data_terpilih[c(indeks),]
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- data_terpilih2
    
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      
      X <- str_replace_all(X, "  ;  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, " ; ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";   ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, ";  ", ";") #tambahan kode di 19 oktober 2025
      X <- str_replace_all(X, "; ", ";") #tambahan kode di 19 oktober 2025
      
      
      
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    
    
    
    
    
    data_keywords_terpilih <- dat[c(simpan_indeks),]
    
    
    data_keywords_terpilih_new <- data_keywords_terpilih[c("Keywords")]
    
    
    
    
    
    for(i in 1 : length(data_keywords_terpilih_new[,1]))
    {
      
      x <- data_keywords_terpilih_new[i,1]
      
      simpan_keyword_hapus_spasi <- gsub(" ", "", x)
      simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
      simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
      
      data_keywords_terpilih_new[i,1] = simpan_keyword_hapus_spasi
      
      
      
      
    }
    
    
    
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    
    
    gambar = 0    
    
    # print(title_word_pairs)
    
    cooccur <-  title_word_pairs
    
    
    wordnetwork <- head(cooccur, input$grafik_coocur_jumlah_coocur)
    gm1 <- graph_from_data_frame(wordnetwork)
    gambar1 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) +
      theme(legend.position="none")
    
    
    
    gambar2 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) + theme_classic() +
      theme(legend.position="none")
    
    
    
    
    
    
    gambar3 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(color = "orange", width=0.7) +
      geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                      colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    gambar4 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(color = "#57d3e6", width=0.7) +
      geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                      colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    
    wordnetwork2 <- head(cooccur, input$grafik_coocur_jumlah_coocur)
    wordnetwork2 <- graph_from_data_frame(wordnetwork2)
    gambar5 <- ggraph(wordnetwork2, layout = "fr") +
      geom_edge_link(aes(width = n, edge_alpha = n), edge_colour = "#ed9de9") +
      geom_node_point(aes(size = igraph::degree(wordnetwork2)), shape = 1, color = "black") +
      geom_node_text(aes(label = name), col = "darkblue", size = input$grafik_coocur_ukuran_teks) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    # gm1 <- igraph::as_data_frame(wordnetwork)
    #gm1 <- graph_from_data_frame(wordnetwork)
    
    #print("ugiiiiiiiiii")
    #print(wordnetwork)
    
    
    if(input$grafik_coocur_tipe_grafik == "1")
    {
      gambar = gambar1
    }
    
    
    if(input$grafik_coocur_tipe_grafik == "2")
    {
      gambar = gambar2
    }
    
    
    if(input$grafik_coocur_tipe_grafik == "3")
    {
      gambar = gambar3
    }
    
    
    
    if(input$grafik_coocur_tipe_grafik == "4")
    {
      gambar = gambar4
    }
    
    
    if(input$grafik_coocur_tipe_grafik == "5")
    {
      gambar = gambar5
    }
    
    
    return(gambar)
    
  }
  
  
  
  
  #################
  
  
  
  
  
  
  ##########300 x 300
  
  output$pemetaan_kata_kunci1_300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########500 x 300
  
  output$pemetaan_kata_kunci1_500_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########700 x 300
  
  output$pemetaan_kata_kunci1_700_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 300
  
  output$pemetaan_kata_kunci1_900_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 300
  
  output$pemetaan_kata_kunci1_1100_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 300
  
  output$pemetaan_kata_kunci1_1200_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 300
  
  output$pemetaan_kata_kunci1_1300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########1400 x 300
  
  output$pemetaan_kata_kunci1_1400_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 500
  
  output$pemetaan_kata_kunci1_300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########500 x 500
  
  output$pemetaan_kata_kunci1_500_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 500
  
  output$pemetaan_kata_kunci1_700_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########900 x 500
  
  output$pemetaan_kata_kunci1_900_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 500
  
  output$pemetaan_kata_kunci1_1100_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 500
  
  output$pemetaan_kata_kunci1_1200_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 500
  
  output$pemetaan_kata_kunci1_1300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########1400 x 500
  
  output$pemetaan_kata_kunci1_1400_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 700
  
  output$pemetaan_kata_kunci1_300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########500 x 700
  
  output$pemetaan_kata_kunci1_500_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 700
  
  output$pemetaan_kata_kunci1_700_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########900 x 700
  
  output$pemetaan_kata_kunci1_900_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1100 x 700
  
  output$pemetaan_kata_kunci1_1100_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1200 x 700
  
  output$pemetaan_kata_kunci1_1200_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 700
  
  output$pemetaan_kata_kunci1_1300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########1400 x 700
  
  output$pemetaan_kata_kunci1_1400_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########300 x 900
  
  output$pemetaan_kata_kunci1_300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########500 x 900
  
  output$pemetaan_kata_kunci1_500_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 900
  
  output$pemetaan_kata_kunci1_700_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########900 x 900
  
  output$pemetaan_kata_kunci1_900_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########1100 x 900
  
  output$pemetaan_kata_kunci1_1100_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1200 x 900
  
  output$pemetaan_kata_kunci1_1200_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 900
  
  output$pemetaan_kata_kunci1_1300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########1400 x 900
  
  output$pemetaan_kata_kunci1_1400_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #############
  
  
  
  
  output$informasi_cek <- renderPrint({
    
    nama_jurnal <- input$terpilih_pilih_jurnal
    
    
   
    
    result <- sub(";.*", "", nama_jurnal)
    
    
    print(result)
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  output$daftar_biaya_publikasi_jurnal <- DT::renderDT({
    
    
    
    dataset <- read.xlsx("daftar_biaya_publikasi.xlsx")
    
    
    colnames(dataset) <- c(
      
      
      
      "Journal",
      "ISSN",
      "Indexed by Sinta",
      "Link Sinta",
      "Scimago",
      "Scopus",
      "Article Processing Charge (APC)",
      "Journal Information"
      
      
      
      
      
    )
    
    
    
    print(dataset)
    
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_dashboard_server

#akhir dari modul_dashboard_server
#akhir dari modul_dashboard_server
#akhir dari modul_dashboard_server

















































































ui <- fluidPage(
  
  
 includeHTML("intro_home.html"),
  
 
 
 
  
  uiOutput("modul_dashboard"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_dashboard <- renderUI({
    
    
    
    #source("module//modul_dashboard.R")
    callModule(module = modul_dashboard_server, id = "modul_dashboard")
    modul_dashboard_ui(id = "modul_dashboard")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














