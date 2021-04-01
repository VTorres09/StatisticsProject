

server <- function(input, output) {
    ################### INPUT ####################
    select_cripto <- eventReactive(input$go, {
        
        cripto_name <- input$stock
        twin <- input$true_date
        
        df_cripto1 <- geral %>% 
            filter(criptocurrency == cripto_name)
            
        df_cripto <- df_cripto1 %>%
           filter(Date>as.Date(twin[1]) & Date<as.Date(twin[2]))

        
        return(df_cripto)
    })
    
    select_criptos <- eventReactive(input$go2, {
      
      cripto_name1 <- input$stock1
      cripto_name2 <- input$stock2
      
      twin1 <- input$true_date1
      
      df_cripto1 <- geral %>% 
        filter(criptocurrency == cripto_name1)
      
      df_cripto2 <- geral %>% 
        filter(criptocurrency == cripto_name2)
      
      df_criptoa <- df_cripto1 %>%
        filter(Date>as.Date(twin1[1]) & Date<as.Date(twin1[2]))
      
      df_criptob <- df_cripto2 %>%
        filter(Date>as.Date(twin1[1]) & Date<as.Date(twin1[2]))
      
      Mylist <- list("df1" =df_criptoa, "df2" = df_criptob)
      
      return(Mylist)
      
    })
    
    output$timedate <- renderUI({
        ##DEFININDO LIMITE DE INPUT DE DATAS PAGINA 1
        cripto_name <- input$stock
        
        df <- geral %>% 
            filter(criptocurrency == cripto_name)
        
        min_time <- min(df$Date)
        max_time <- max(df$Date)
        dateRangeInput("true_date", "Periodo de analise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate1 <- renderUI({
      ##DEFININDO LIMITE DE INPUT DE DATAS PAGINA 2
      cripto_name1 <- input$stock1
      cripto_name2 <- input$stock2
      
      df1 <- geral %>% 
        filter(criptocurrency == cripto_name1)
      
      df2<- geral %>% 
        filter(criptocurrency == cripto_name2)

      
      min_time <- max(min(df1$Date), min(df2$Date))
      
      max_time <- min(max(df1$Date), max(df2$Date))
      
      dateRangeInput("true_date1", "Periodo de analise",
                     end = max_time,
                     start = min_time,
                     min  = min_time,
                     max  = max_time,
                     format = "dd/mm/yy",
                     separator = " - ",
                     language='pt-BR')
    })
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_cripto()
        mean <- df %>% select(Close) %>% colMeans()
        ##CALCULANDO MEDIDAS PAGINA 1
        Media <- mean[[1]]
        Mediana <- median(df$Close)
        
        getmode <- function(v) {
            uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        
        Moda <- getmode(df$Close)
        Desvio <- sd(df$Close)
        Maxima <- max(df$Close, na.rm = TRUE)
        Minima <- min(df$Close, na.rm = TRUE)
        Criptomoeda <- input$stock
        df_tb <-  data.frame(Criptomoeda, Media, Mediana, Moda, Desvio, Maxima, Minima)
        
        df_tb <- as.data.frame(t(df_tb))
        names(df_tb)[1] <- "Metricas"
      
        return(df_tb)
    })
    
    Info_DataTable1 <- eventReactive(input$go2,{
      df <- select_criptos()
      dataframe1 <- df$df1
      dataframe2 <- df$df2
      ##CALCULANDO MEDIDAS PAGINA 2
      mean1 <- dataframe1 %>% select(Close) %>% colMeans()
      mean2 <- dataframe2 %>% select(Close) %>% colMeans()
      
      Media <- mean1[[1]]
      Mediana <- median(dataframe1$Close)
      
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      
      Moda <- getmode(dataframe1$Close)
      Desvio <- sd(dataframe1$Close)
      Maxima <- max(dataframe1$Close, na.rm = TRUE)
      Minima <- min(dataframe1$Close, na.rm = TRUE)
      Criptomoeda <- input$stock1
      
      Media2 <- mean2[[1]]
      Mediana2 <- median(dataframe2$Close)
      
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      
      Moda2 <- getmode(dataframe2$Close)
      Desvio2 <- sd(dataframe2$Close)
      Maxima2 <- max(dataframe2$Close, na.rm = TRUE)
      Minima2 <- min(dataframe2$Close, na.rm = TRUE)
      Criptomoeda2 <- input$stock2
      
      ##COLOCAR LEGENDA AO LADO
      ##leg <- c('Criptomoeda','Media','Mediana', 'Moda', 'Desvio', 'Maxima', 'Minima')
      Criptomoeda_1 <-  data.frame(Criptomoeda, Media, Mediana, Moda, Desvio, Maxima, Minima)
      Criptomoeda_2  <-  data.frame(Criptomoeda2, Media2, Mediana2, Moda2, Desvio2, Maxima2, Minima2)
      df_tb <- data.frame(t(Criptomoeda_1), t(Criptomoeda_2))
      names(df_tb)[1] <- "Metricas 1"
      names(df_tb)[2] <- "Metricas 2"
      
      return(df_tb)
    })
    
    ##IMPRIMINDO TABELA PAGINA 1
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
    ##IMPRIMINDO TABELA PAGINA 2
    output$info1 <- renderDT({
      Info_DataTable1() %>%
        as.data.frame() %>% 
        DT::datatable(options=list(
          language=list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
          )
        ))
    })
    
    output$sh <- renderPlot({
        #PLOTANDO O GRAFICO DE LINHA
        df <- select_cripto()
        
        aux <- df$Close %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$Date <- ymd(df$Date)
        a <- df %>% 
            ggplot(aes(Date, Close, group=1)) +
            geom_line() +
            geom_point() +
            geom_line(colour = "#000080") +
            ylab('Preço da Criptomoeda em $') +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
    })
    
    output$sh2 <- renderPlot({
        
        #PLOTANDO O HISTOGRAMA
        df <- select_cripto()
        aux <- df$Close %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$Date <- ymd(df$Date)
       a <-  ggplot(df, aes(Date, Volume, group=1)) +
            geom_histogram(stat='identity', aes(fill=Volume)) +
            labs(x="Data", y='Volume de Negociacoes') +
            theme_bw() +
            xlim(c(aux1,aux2)) +
            scale_x_date(date_labels = "%Y-%m-%d")
        a
        
    })
    output$sh3 <- renderPlot({
        
        #PLOTANDO O GRAFICO BOXPLOT
        df <- select_cripto()
        
        aux <- df$Close %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)

        
        df$Date <- ymd(df$Date)
        a <- df %>% 
            ggplot(aes(x = Date, y = Close)) +
            geom_candlestick(aes(open = Open, high = High, low = Low, close = Close), color_up = "green", color_down = "red", 
                             fill_up  = "green", fill_down  = "red") +
            geom_ma(color = "darkgreen") +
          theme_void() +
            xlim(c(aux1, aux2)) +
            scale_x_date(date_labels = "%Y-%m-%d")
        a
    })
    
    output$sh4 <- renderPlot({
      
      #PLOTANDO O GRAFICO DE COMPARACAO LINHA
      df <- select_criptos()
      
      dataframe1 <- df$df1
      dataframe2 <- df$df2
      
      aux <- dataframe1$Close %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      dataframe1$Date <- ymd(dataframe1$Date)
      dataframe2$Date <- ymd(dataframe2$Date)
      
      a <-
        ggplot() +
        geom_line(data = dataframe1, aes(x = Date, y = Close), color = "deepskyblue") +
        geom_line(data = dataframe2, aes(x = Date, y = Close), color = "darkblue") +
        geom_point() +
        geom_line(colour = "#000080") +
        ylab('Preço da Criptomoeda em $') +
        coord_cartesian(ylim = c(aux1, aux2)) +
        theme_bw() +
        scale_x_date(date_labels = "%Y-%m-%d")
      
      a
      
    })
    
    output$sh5 <- renderPlot({
      
      #PLOTANDO O GRAFICO DE COMPARACAO BARRA
      df1 <- select_criptos()
      
      dataframe1 <- df1$df1
      dataframe2 <- df1$df2
      
      df <- union_all(dataframe1, dataframe2)
      
      aux <- df$Close %>% na.omit() %>% as.numeric()
      aux1 <- min(aux)
      aux2 <- max(aux)
      
      df$Date <- ymd(df$Date)

      
      a <- ggplot(df, aes(Date, Volume, group=1)) +
        geom_histogram(stat='identity', aes(fill=criptocurrency)) +
        labs(x="Data", y='Volume de Negociacoes') +
        theme_light() +
        xlim(c(aux1,aux2)) +
        scale_x_date(date_labels = "%Y-%m-%d")
      a
    })
}
