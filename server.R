##########################################################
################### server ###################
##########################################################

server <- function(input, output,session) {
  
  
  #Affichage des données
  output$data_raw <- renderDataTable({
    data
  }
  ,options = list(scrollX = TRUE)
  )
  
  # Action TCP
  output$act_tcp <- renderPlotly({
    colors <- c('red', 'blue') 
    fig <- plot_ly(
      x = unique(data[data$proto=="TCP",]$action),
      y = table(data[data$proto=="TCP",]$action)[],
      marker = list(color = colors),
      name = "TCP",
      type = "bar"
    )
    fig <- fig %>% layout(title = "Protocol TCP")
    fig
  })
  
  # Actions UDP
  output$act_udp <- renderPlotly({
    colors <- c('red', 'blue') 
    fig <- plot_ly(
      x = unique(data[data$proto=="UDP",]$action),
      y = table(data[data$proto=="UDP",]$action)[],
      marker = list(color = colors),
      name = "UDP",
      type = "bar"
    )
    fig <- fig %>% layout(title = "Protocol UDP")
    fig
  })
  
  #######################################
  ##    ONGLET Analyse temporelle   ##
  #######################################
  
  # Donnees selon heures
  output$temp_data_heure <- renderPlotly({
    v<-input$date
    v<-as.Date(v, "%YYYY-%mm-%dd")
    
    d<-as.integer(format(v, format = "%d"))
    m<-as.integer(format(v, format = "%m"))
    y<-as.integer(format(v, format = "%Y"))
    
    heure <- as.integer(rownames(table(data$datetime_hour)))
    y_deny<- table(data[data$action=="Deny" & 
                          data$datetime_day==d & 
                          data$datetime_month==m &
                          data$datetime_year==y
                        ,]$datetime_hour)[]
    count<- table(data[data$action=="Permit" & 
                         data$datetime_day==d & 
                         data$datetime_month==m &
                         data$datetime_year==y
                       ,]$datetime_hour)[]
    
    all_hrs<- 0:23
    
    added_hrs <- all_hrs[!all_hrs %in% rownames(count)]
    count[as.character(added_hrs)]<-0
    names(count)[is.na(count)] <- added_hrs
    count[is.na(count)] <- 0
    
    
    added_hrs <- all_hrs[!all_hrs %in% rownames(y_deny)]
    y_deny[as.character(added_hrs)]<-0
    names(y_deny)[is.na(y_deny)] <- added_hrs
    y_deny[is.na(y_deny)] <- 0
    
    hrs <- as.integer(rownames(table(data$datetime_hour)))
    
    count<-count[as.character(0:23)]
    y_deny<-y_deny[as.character(0:23)]
    
    
    dt <- data.frame(all_hrs, as.array(count))
    fig <- plot_ly(dt, x = ~all_hrs, y = ~count, name = "Permitted", type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~ y_deny, name = "Denied", connectgaps = TRUE)
    
    fig
  })
  
  # Donnees selon minutes
  output$temp_data_min <- renderPlotly({
    v<-input$date
    v<-as.Date(v, "%YYYY-%mm-%dd")
    d<-as.integer(format(v, format = "%d"))
    m<-as.integer(format(v, format = "%m"))
    y<-as.integer(format(v, format = "%Y"))
    hour <- input$hour_selec
    y_deny<- table(data[data$action=="Deny" & 
                          data$datetime_day==d & 
                          data$datetime_month==m &
                          data$datetime_year==y &
                          data$datetime_hour==hour
                        ,]$datetime_min)[]
    
    count<- table(data[data$action=="Permit" & 
                         data$datetime_day==d & 
                         data$datetime_month==m &
                         data$datetime_year==y &
                         data$datetime_hour==hour
                       ,]$datetime_min)[]
    
    all_min<- 0:59
    
    added_min <- all_min[!all_min %in% rownames(count)]
    count[as.character(added_min)]<-0
    names(count)[is.na(count)] <- added_min
    count[is.na(count)] <- 0
    
    added_min <- all_min[!all_min %in% rownames(y_deny)]
    y_deny[as.character(added_min)]<-0
    names(y_deny)[is.na(y_deny)] <- added_min
    y_deny[is.na(y_deny)] <- 0
    
    min <- as.integer(rownames(table(data$datetime_min)))
    
    count<-count[as.character(0:59)]
    y_deny<-y_deny[as.character(0:59)]
    
    dt <- data.frame(all_min, as.array(count))
    fig <- plot_ly(dt, x = ~all_min, y = ~count, name = "Permitted", type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~ y_deny, name = "Denied", connectgaps = TRUE)
    
    fig
  })
  
  #######################################
  ##    ONGLET CLASSEMENT DES REGLES   ##
  #######################################
  
  
  # GRAPHIQUE
  output$reg_desc <- renderPlotly({
    rule_by_proto <- data[,c("policyid","proto","action")]
    rules_by_port <- rule_by_proto %>% group_by(policyid) %>% count
    rules_by_port <- rules_by_port %>% filter(n>input$top_reg)
    if(input$fill_reg=="proto"){
      graph=rule_by_proto %>%
        group_by(policyid,proto) %>%
        count %>%
        filter(proto %in% input$proto_choice) %>%
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n))%>%
        ggplot(aes(n, reorder(policyid, n), fill=proto)) + geom_col() + labs(y = NULL)
      
      graph
    }else{
      graph=rule_by_proto %>%
        group_by(policyid,action) %>%
        count %>%
        filter(action %in% input$action_choice) %>%
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n)) %>%
        ggplot(aes(n, reorder(policyid, n), fill=action)) + geom_col() + labs(y = NULL)
      
      graph
      
    }
    
    
  })
  
  # DATATABLE
  output$table_reg_desc <- renderDataTable({
    rule_by_proto <- data[,c("policyid","proto","action")]
    rules_by_port <- rule_by_proto %>% group_by(policyid) %>% count
    rules_by_port <- rules_by_port %>% filter(n>input$top_reg)
    if(input$fill_reg=="proto"){
      graph=rule_by_proto %>%
        group_by(policyid,proto) %>%
        count %>%
        filter(proto %in% input$proto_choice) %>%
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n))
      
      graph
    }else{
      graph=rule_by_proto %>%
        group_by(policyid,action) %>%
        count %>%
        filter(action %in% input$action_choice) %>%
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n))
      
      graph
      
    }
    
    
  })   
  
  
  #######################################
  ##    ONGLET CLASSEMENT DES PORTS    ##
  #######################################  
  
  # GRAPHIQUE
  output$port_desc <- renderPlotly({
    
    table_port <- data %>% group_by(dstport) %>%
      summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
      arrange(desc(n)) 
    table_port$dstport <- as.numeric(table_port$dstport)
    t <- table_port %>% filter(n>input$top_ports) %>% filter(dstport>input$range_ports[1]) %>% filter(dstport<input$range_ports[2]) %>%select(dstport, deny, permit)
    
    df2 <- melt(t, id.vars='dstport')
    
    df2$dstport <- as.character(df2$dstport)
    df2 %>% filter(variable %in% input$action_choice_p) %>% filter(value >0) %>%  ggplot(aes(value, dstport,fill=variable)) +
      geom_col() +
      labs(y = NULL)+
      theme_minimal()
    
    
    
  })
  
  # DATATABLE
  output$table_port_desc <- renderDataTable({
    table_port <- data %>% group_by(dstport) %>%
      summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
      arrange(desc(n)) 
    table_port$dstport <- as.numeric(table_port$dstport)
    t <- table_port %>% filter(n>input$top_ports) %>% filter(dstport>input$range_ports[1]) %>% filter(dstport<input$range_ports[2]) %>%select(dstport, deny, permit)
    
    df2 <- melt(t, id.vars='dstport')
    
    df2$dstport <- as.character(df2$dstport)
    tab=df2 %>% filter(variable %in% input$action_choice_p) %>% filter(value >0)
    tab
  })
  
  #######################################
  ## ONGLET RAPPROCHEMENT DES REGLES   ##
  #######################################
  output$rapp_regles <- renderPlotly({
    
    table_port <- data %>% group_by(dstport, policyid) %>%
      summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
      arrange(desc(n)) 
    
    t <- table_port %>% filter(n>input$top_rules_ports) %>% filter(policyid %in% input$regles_selec)  %>% select(dstport, deny, permit)
    
    df2 <- melt(t, id.vars='dstport')
    
    df2$dstport <- as.character(df2$dstport)
    df2 %>% filter(variable %in% input$action_choice_p2) %>% filter(value >0) %>%  ggplot(aes(value, dstport,fill=variable)) +
      geom_col() +
      labs(y = NULL)+
      theme_minimal()
    
    
  })
  
  
  #######################################
  ##         ONGLET CLUSTERING         ##
  #######################################
  
  output$dendogram <- renderPlot({
    
    d = dist(analyse)
    cah = hclust(d, method = "ward.D2")
    
    colors = c(1:input$nb_clust)
    clus = cutree(cah, input$nb_clust)
    par(mar = c(0, 0, 0, 0))
    plot(as.phylo(cah), type = "fan", tip.color = colors[clus],label.offset = 1, cex = 1, y.lim = c(-50,50))
    legend(0,20,legend= c(1:input$nb_clust), col=c(1:input$nb_clust),lty=1, cex=0.9, title= "Groupes")
  })
  
  output$clusters_ana <- renderDataTable({
    d = dist(analyse)
    cah = hclust(d, method = "ward.D2")
    
    colors = colors = c(1:input$nb_clust)
    clus = cutree(cah, input$nb_clust)
    grp_cah <- as.data.frame(clus)
    grp_cah$ip <-rownames(grp_cah)
    analyse2 <- analyse
    analyse2$ip <- rownames(analyse2)
    analyse2 <- dplyr::left_join(analyse2,grp_cah,by="ip")
    
    analyse2 %>% filter(clus== input$choose_clust)
    
    
  },options = list(scrollX = TRUE))
  
  
  output$imp_var_clust <- renderPlotly({
    d = dist(analyse)
    cah = hclust(d, method = "ward.D2")
    colors = colors = c(1:input$nb_clust)
    clus = cutree(cah, input$nb_clust)
    grp_cah <- as.data.frame(clus)
    grp_cah$ip <-rownames(grp_cah)
    analyse2 <- analyse
    analyse2$ip <- rownames(analyse2)
    analyse2 <- dplyr::left_join(analyse2,grp_cah,by="ip")
    
    analyse2 %>% filter(clus== input$choose_clust)
    analyse2$clus <- as.character(analyse2$clus)
    vars <- colnames(analyse2[,1:10])
    analyse2_vert <- melt(analyse2,
                          id.vars = "clus",
                          measure.vars = vars)
    
    analyse2_vert <- sqldf("select clus,variable,sum(value) as value from analyse2_vert group by clus,variable")
    
    analyse2_vert <- analyse2_vert %>% filter( !is.na(clus)) %>% arrange(desc(value))
    
    graph=analyse2_vert %>%
      ggplot(aes(value, reorder(variable, value), fill=clus)) +
      geom_col() +
      labs(y = NULL)
    
    graph
  })
  
  # Onglet IP
  
  table_ip <- data %>% group_by(ipsrc) %>%
    summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>% 
    arrange(desc(n))
  
  table_port <- data %>% group_by(dstport) %>%
    summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) 
  
  output$top_src <- renderPlotly({
    t <- table_ip %>% top_n(input$top_ip,n) %>% select(ipsrc, deny, permit)
    
    df2 <- melt(t, id.vars='ipsrc')
    
    df2 %>% ggplot(aes(value, ipsrc,fill=variable)) +
      geom_col() +
      labs(y = NULL)+
      theme_minimal()
    
    
    })
    
    output$table_ip <- renderDataTable({
      table_ip %>% rename(total=n)
    })
    output$ip_graph <- renderPlot({
      ip_rel <- data %>% group_by(ipsrc,ipdst) %>% count %>% arrange(desc(n))
      
      ip_graph <- ip_rel[1:input$top_ip,] %>%
        graph_from_data_frame()
      
      ggraph(ip_graph, layout = 'linear') + 
        geom_edge_arc(aes(width=n),color="orange", alpha=.7) +
        geom_node_point(size=5, color="gray50")+
        geom_node_text(aes(label = name), size=4, color="gray50",hjust=-0.4,angle=-90)+
        expand_limits(y=c(-1, NA))
    
    })
    #######################################
    ##        Onglet IP hors plan        ##
    #######################################
  
    
  output$table_hp <- renderDataTable({
    data[-grep(input$id_type,data$ipsrc),] 
  })
  
  # Onglet Heatmap
  output$heatmap <- renderPlotly({
    
    v<-input$date_hm
    v<-as.Date(v, "%YYYY-%mm-%dd")
    
    d<-as.integer(format(v, format = "%d"))
    m<-as.integer(format(v, format = "%m"))
    y<-as.integer(format(v, format = "%Y"))
    
    df <- data[grep(input$hm_ip,data$ipsrc),]
    
    pre_mat <- df %>% filter(dstport %in% input$hm_ports, datetime_month == m, datetime_day == d, datetime_year == y) %>% 
      group_by(dstport,datetime_hour) %>% count
    
    ggplot(pre_mat,aes(datetime_hour,dstport,fill=n))+
      geom_tile()+
      scale_fill_gradient(low="yellow", high="red")+
      geom_text(aes(label=n),cex=3.5)+
      xlim(-0.5,23.5)
      })
  
  #######################################
  ##     Onglet Detection anomalies   ##
  #######################################  
  
  # Table de adress ip
  dt_ip_src_filtered=reactive({
    occr<- as.integer(input$id_occur)
    data %>%
      dplyr::group_by(ipsrc,datetime_year,datetime_month,datetime_day,datetime_hour)%>%
      count%>%
      arrange(
        desc(n)
      )%>%
      filter(n >= occr)
    
  })
  
  # 
  output$ip_anomalies <- renderDataTable({
    occr<- as.integer(input$id_occur)
    dt <- data %>%
      dplyr::group_by(ipsrc,datetime_year,datetime_month,datetime_day,datetime_hour)%>%
      dplyr::summarise(n = n())%>%
      arrange(
        desc(n)
      )%>%
      filter(n >= occr)
    dt
  })
  
  observeEvent(input$id_occur,{
    updateSelectInput(session = session,
                      inputId = 'adr_ipsrc',
                      choices = unique(dt_ip_src_filtered()$ipsrc))
  },ignoreInit = TRUE)
  
  # Graph adresse ip pour seuils
  output$seuil_ip_min <- renderPlotly({
    v<-input$date_ipsrc
    v<-as.Date(v, "%YYYY-%mm-%dd")
    d<-as.integer(format(v, format = "%d"))
    m<-as.integer(format(v, format = "%m"))
    y<-as.integer(format(v, format = "%Y"))
    hour <- input$hour_selec_ipsrc
    ip<- toString(input$adr_ipsrc)
    print("ip")
    print(ip)
    y_deny<- table(data[data$ipsrc== ip &
                          data$action=="Deny" &
                          data$datetime_day==d &
                          data$datetime_month==m &
                          data$datetime_year==y &
                          data$datetime_hour==hour
                        ,]$datetime_min)[]
    
    count<- table(data[data$ipsrc== ip &
                         data$action=="Permit" &
                         data$datetime_day==d &
                         data$datetime_month==m &
                         data$datetime_year==y &
                         data$datetime_hour==hour
                       ,]$datetime_min)[]
    
    all_min<- 0:59
    
    added_min <- all_min[!all_min %in% rownames(count)]
    count[as.character(added_min)]<-0
    names(count)[is.na(count)] <- added_min
    count[is.na(count)] <- 0
    
    added_min <- all_min[!all_min %in% rownames(y_deny)]
    y_deny[as.character(added_min)]<-0
    names(y_deny)[is.na(y_deny)] <- added_min
    y_deny[is.na(y_deny)] <- 0
    
    min <- as.integer(rownames(table(data$datetime_min)))
    
    count<-count[as.character(0:59)]
    y_deny<-y_deny[as.character(0:59)]
    
    dt <- data.frame(all_min, as.array(count))
    fig <- plot_ly(dt, x = ~all_min, y = ~count, name = "Permitted", type = 'scatter', mode = 'lines')
    fig <- fig %>% add_trace(y = ~ y_deny, name = "Denied", connectgaps = TRUE)
     
    
    fig
  })
  
  # Graph adresse ip pour seuils
  output$seuil_ip_hr <- renderPlotly({
    occr <- as.integer(input$id_occur)
    v<-input$date_ipsrc
    v<-as.Date(v, "%YYYY-%mm-%dd")
    d<-as.integer(format(v, format = "%d"))
    m<-as.integer(format(v, format = "%m"))
    y<-as.integer(format(v, format = "%Y"))
    
    ip<- toString(input$adr_ipsrc)
    heure <- as.integer(rownames(table(data$datetime_hour)))
    y_deny<- table(data[data$ipsrc== ip &
                          data$action=="Deny" & 
                          data$datetime_day==d & 
                          data$datetime_month==m &
                          data$datetime_year==y
                        ,]$datetime_hour)[]
    count<- table(data[data$ipsrc== ip &
                         data$action=="Permit" & 
                         data$datetime_day==d & 
                         data$datetime_month==m &
                         data$datetime_year==y
                       ,]$datetime_hour)[]
    
    all_hrs<- 0:23
    
    added_hrs <- all_hrs[!all_hrs %in% rownames(count)]
    count[as.character(added_hrs)]<-0
    names(count)[is.na(count)] <- added_hrs
    count[is.na(count)] <- 0
    
    
    added_hrs <- all_hrs[!all_hrs %in% rownames(y_deny)]
    y_deny[as.character(added_hrs)]<-0
    names(y_deny)[is.na(y_deny)] <- added_hrs
    y_deny[is.na(y_deny)] <- 0
    
    hrs <- as.integer(rownames(table(data$datetime_hour)))
    
    count<-count[as.character(0:23)]
    y_deny<-y_deny[as.character(0:23)]
    
    
    dt <- data.frame(all_hrs, as.array(count))
    fig <- plot_ly(dt, x = ~all_hrs, y = ~count, name = "Permitted", type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~ y_deny, name = "Denied", connectgaps = TRUE)
    fig<- fig %>% add_segments(x = 0, xend = 23, y = occr, yend = ~occr, line = list(dash = "dash"), showlegend = FALSE)
    
    fig
  })
  
  
  
}
