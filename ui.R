########################################################
################### Création de l'ui ###################
########################################################

ui <- dashboardPage(
  dashboardHeader(title = "Challenge Sécurité",
                  tags$li(
                    a(
                      strong("Université Lyon 2 - M2 SISE et OPSIE"),
                      height = 40,
                      href = "https://www.univ-lyon2.fr/"
                    ),
                    class = "dropdown"
                  )
  ),
  # Création de la barre des onglets de gauche
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("door-open")),
      menuItem("Description des données", tabName = "desc_data", icon=icon("line-chart"),
               menuSubItem("Classement des règles",tabName='class_reg', icon=icon("trophy")),
               menuSubItem("Classement des ports",tabName='class_ports', icon=icon("trophy")),
               menuSubItem("Rapprochement des règles",tabName='rapp_reg',icon = icon("link")),
               menuSubItem("Histogramme des protocoles",tabName='hist_pro',icon = icon("chart-bar")),
               menuSubItem("Adresses IP",tabName='adr_ip',icon = icon("project-diagram")),
               menuSubItem("Adresses hors plan",tabName='adr_hp',icon = icon("glasses")),
               menuSubItem("Analyse temporelle",tabName='anal_temp',icon = icon("algolia")),
               menuSubItem("Visualisation des données brutes",tabName='raw_data',icon = icon("filter"))),
      menuItem("Aide à la décison", tabName = "aide_de", icon=icon("dashboard"),
               menuSubItem("Clustering",tabName='clust', icon=icon("sitemap")),
               menuSubItem("Détection des anomalies",tabName='detec_ano', icon=icon("user-secret")),
               menuSubItem("Heatmap",tabName='heatmap', icon=icon("th")))

    )
  ),
  # Création des onglets
  dashboardBody(
    tabItems(
      tabItem(tabName = "accueil",
              sidebarLayout(
                sidebarPanel(
                  h2("Bienvenue !"),
                  p(
                      "Cette application a été créée dans le cadre du challenge de sécurité enseigné par
              M. Ricco RAKOTOMALALA et M. David PIERROT. L'objectif du projet est de faire travailler en 
              collaboration des étudiants du M2 OPSIE (Sécurité Informatique) et des étudiants du M2 SISE (Science des Données).
              Le projet porte sur la création d'une application permettant de visualiser des données provenants des logs d'un
              firewall et d'implémenter des méthodes d'aide à la décision. Ce challenge a été relevé en un jour et demi."
                  ),
                  img(
                    src = "logo_univ.jpg",
                    height = 165,
                    width = 165
                  ),
                  br(),
                  "Auteurs :",
                  span(
                    "Gatien CONTINSOUZAS, Mounir GUIDOUN, Oumaïma KHATTABI, Clément LE PADELLEC, Hugo MOHAMED, Romain PIC ",
                    style = "color:blue"
                  )
                ),
                mainPanel(
                  p(
                    "La base de données est générée par les étudiants d'OPSIE qui ont dû créer, à l'aide 
                    de plusieurs machines virtuelles, des simulations de flux sur un réseau passant par un firewall.
                    Ces étudiants ont créé des règles de firewall et ont rendu les logs exploitables en les exportant au format csv."
                  ),
                  br(),
                  p("Les étudiants de SISE quant à eux se sont occupés de l'application dynamique. Cette application permet d'effectuer
                    les traitements et visualisations suivantes :"
                  ),
                  p(
                    strong("- Analyse et visualisation des ports et des IPs :"),
                    " visualisation des adresses IP les plus utilisées, des ports les plus utilisés avec les protocoles et les actions du pare-feu qui y sont associés. "
                  ),
                  br(),
                  p(
                    strong("- Analyse temporelle :"),
                    " visualisation des données au cours du temps permettant d'observer en détail l'activité d'un jour ou d'une heure."
                  ),
                  br(),
                  p(
                    strong("- Aide à la décision :"),
                    "clustering afin de trouver des IP correspondant à un comportement anormal, détection par seuil sur le comportement d'individus et heatmap pour visualiser les flux par port selon l'heure.."
                  ),
                  br()
                )
              )
      ),
      tabItem(tabName = "class_reg",
              sidebarLayout(sidebarPanel(
                sliderInput(
                  inputId = "top_reg",
                  label = "Nombre de règles",
                  min = 1,
                  max = 5000,
                  value = 300,
                  step = 50
                ),
                checkboxGroupInput(
                  inputId="proto_choice",
                  label="Sélection du protocole",
                  choices = c("TCP","UDP"),
                  selected = c("TCP","UDP")
                  
                ),
                checkboxGroupInput(
                  inputId="action_choice",
                  label="Selection de l'action",
                  choices = c("Permit","Deny"),
                  selected = c("Permit","Deny")
                  
                ),
                radioButtons(
                  inputId="fill_reg",
                  label="Colorer par",
                  choices = c("action","proto"),
                  selected = "proto"
                )
                
                
              ),
              mainPanel(tabsetPanel(
                tabPanel("Plot",plotlyOutput("reg_desc")),
                tabPanel("Table",dataTableOutput('table_reg_desc'))
              )

              ))
      ),
      tabItem(tabName = "class_ports",
              sidebarLayout(sidebarPanel(
                sliderInput(
                  inputId = "top_ports",
                  label = "Nombre de ports",
                  min = 1,
                  max = 5000,
                  value = 300,
                  step = 5
                ),
                checkboxGroupInput(
                  inputId="action_choice_p",
                  label="Selection de l'action",
                  choices = c("permit","deny"),
                  selected = c("permit")
                  
                ),
                sliderInput("range_ports", label = "Plage de ports", min = 0, 
                            max = 70000, value = c(0, 1024)
                )
                
                
              ),
              mainPanel(tabsetPanel(
                tabPanel("Plot",plotlyOutput("port_desc")),
                tabPanel("Table",dataTableOutput('table_port_desc'))
              )
              ))
      ),
      tabItem(tabName = "rapp_reg",
              sidebarLayout(sidebarPanel(
                sliderInput(
                  inputId = "top_rules_ports",
                  label = "Nombre de regles par ports",
                  min = 1,
                  max = 10000,
                  value = 500,
                  step = 50
                ),
                checkboxGroupInput(
                  inputId="action_choice_p2",
                  label="Selection de l'action",
                  choices = c("permit","deny"),
                  selected = c("permit")
                  
                ),
                pickerInput(
                  inputId = "regles_selec",
                  label = "Regles a selectionner",
                  choices = unique(data$policyid),
                  selected = c('999', '1', '3', '6','17'),
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    size = 8)
                  
                )
                
              ),
              mainPanel(
                plotlyOutput("rapp_regles")
              ))
      ),
      tabItem(tabName = "hist_pro",
              fluidPage(
                # h2("Etude des individus : "),
                box(title = "Actions par protocole",
                    status = "primary", solidHeader = TRUE,
                    width = 12,
                    fluidRow(
                      column(width=6, h4("TCP", align = 'center'), plotlyOutput("act_tcp")),
                      column(width=6, h4("UDP", align = 'center'), plotlyOutput("act_udp"))
                    )
                    # ,
                    # fluidRow(
                    #   column(width=6, h4("Permitted", align = 'center'), plotlyOutput("pie_permit")),
                    #   column(width=6, h4("Denied", align = 'center'), plotlyOutput("pie_deny"))
                    # )
                    # ,
                    # fluidRow(
                    #   column(width=12, h4("TCP & UDP", align = 'center'), plotlyOutput("protocols"))
                    #   )
                )
                # box(title = "Protocoles",
                #     status = "primary", solidHeader = TRUE,
                #     width = 12,
                #     plotlyOutput("protocols")
                # )
              )),
      tabItem(tabName = "adr_ip",
              h1("Adresses IP"),
              sidebarLayout(sidebarPanel(
                                          sliderInput(
                                            'top_ip',
                                            "Nombre d'IP",
                                            min = 1,
                                            max = 20,
                                            value = 5
                                          )
                                          
                                        ),
                            mainPanel(tabsetPanel(
                              tabPanel("Top IP",plotlyOutput("top_src")),
                              tabPanel("Table",dataTableOutput("table_ip")),
                              tabPanel("Graphe",plotOutput("ip_graph",height = 720))
                              )
                              
                            ))
      ),
      
      tabItem(tabName = "adr_hp",
              h1("Adresses IP"),
              sidebarLayout(sidebarPanel(
                textInput("id_type","IP type :",value="^192.168.1.")
                ),
              mainPanel(
                dataTableOutput("table_hp")
              )
      )),
      tabItem(tabName = "anal_temp",
              fluidRow(
                box(title = "Analyse temporelle",
                    status = "primary", solidHeader = TRUE,
                    width = 12,
                    # selection du jour
                    dateInput("date",
                              label = "Date",
                              value = "2021-03-09"),
                    hr(),
                    # fluidRow(column(3, verbatimTextOutput("value"))),
                    fluidRow(
                      column(width=12, h4("Par heure", align = 'center'), plotlyOutput("temp_data_heure"))
                    ),
                    
                    selectInput('hour_selec', 'Heure', c(0:23)),
                    
                    fluidRow(
                      column(width=12, h4("Par minute", align = 'center'), plotlyOutput("temp_data_min"))
                    )
                )
              )
            
      ),
      tabItem(tabName = "raw_data",
              dataTableOutput("data_raw")
      ),
      tabItem(tabName = "clust",
              sidebarLayout(sidebarPanel(
                sliderInput(
                  inputId = "nb_clust",
                  label = "Nombre de clusters",
                  min = 2,
                  max = 10,
                  value = 5,
                  step = 1
                ),
                textInput(
                  inputId = "choose_clust",
                  label = "Analyse d'un cluster",
                  value = "1")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Dendrograme",plotOutput("dendogram",height=720))
                  ,tabPanel("Analyse d'un cluster",dataTableOutput('clusters_ana')),
                  tabPanel("Importance des variables par clusters",plotlyOutput("imp_var_clust",height=720))
                )
                
              )
              )
      ),
      tabItem(tabName = "detec_ano",
              tabsetPanel(
                tabPanel("Table",
                    textInput("id_occur","Nombre de requêtes par heure :",value="0"),
                    hr(),
                    dataTableOutput("ip_anomalies")),
              tabPanel("Visualisation",
                    selectInput('adr_ipsrc', 'Adresse IP source', unique(data$ipsrc)),
                    fluidRow(
                      column(width=6,dateInput("date_ipsrc",
                                               label = "Date",
                                               value = "2021-03-09")),
                      column(width=6,selectInput('hour_selec_ipsrc', 'Heure', c(0:23)))
                    ),
                    
                    hr(),
                    fluidRow(
                      column(width=12, h4("Requêtes par adresse IP", align = 'center'), plotlyOutput("seuil_ip_hr"))
                    ),
                    fluidRow(
                      column(width=12, h4("Requêtes par adresse IP", align = 'center'), plotlyOutput("seuil_ip_min"))
                    )
                )
              )
      ),
      tabItem(tabName = "heatmap",
              h1("Heatmap"),
              sidebarLayout(sidebarPanel(
                pickerInput(
                  inputId = "hm_ports",
                  label = "Ports",
                  choices = ports,
                  selected = port_list,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    size = 10
                  )
                ),
                dateInput("date_hm",
                          label = "Date",
                          value = "2021-03-09"),
                textInput("hm_ip","Filtre IP :",value="")
                
              ),
              mainPanel(
                plotlyOutput("heatmap",height=720)
              ))  
              
      )
    )
  )
)