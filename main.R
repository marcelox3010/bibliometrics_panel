 #BIBLIOTECAS

library(shiny)
library(bibliometrix)
#install.packages("tm")
library("tm")
#install.packages("SnowballC")
library("SnowballC")
#install.packages("wordcloud")
library("wordcloud")

#VARIÁVEIS GLOBAIS

dados = data.frame()
analise_bibliometrica = ""


topindiceh = function(qtd=10){
  
  authors=gsub(","," ",names(analise_bibliometrica$Authors)[1:qtd])
  
  indices <- Hindex(dados, authors, sep = ";")
  
  ind = capture.output(indices$H)
  
  j=2
  i=1
  saida = ""
  txt = ind
  
  while(j <= length(txt)){
    
    tmp = strsplit(txt[[j]],"  ")
    k=1
    for(k in length(tmp))
    {
      tmp2=c(tmp[[k]])
      tmp2 = tmp2 [! tmp2 %in% c("")]
      
      
      
      
      saida = paste(saida, "<tr><td>" ,tmp2[1], "</td><td align='right'>" ,tmp2[2], "</td>
                    <td align='right'>" ,tmp2[3], "</td>
                    
                  
                      <td align='right'>" , tmp2[4],  "</td>
                    </tr>")
      
      
      i=i+1
      k=k+1
      
    }
    
    
    
    j=j+1
    
    
  }
  
  saida
  
}

IndiceCrescimentoAnual = function(){
  
  Y=data.frame(table(analise_bibliometrica$Years))
  names(Y)=c("Year   ", "Articles")
  print(Y,row.names=FALSE);cat("\n")
  ny=dim(Y)[1]
  GR=((Y[ny,2]/Y[1,2])^(1/(ny-1))-1)*100
  GR
  
}

localcitation = function(qtd=10){
  
  
  CR <- localCitations(dados, analise_bibliometrica, sep = ".  ")
  lc = capture.output(CR[1:qtd])
  
  j=2
  i=1
  saida = ""
  txt = lc
  
  while(j <= length(txt)){
    
    tmp = strsplit(txt[[j]],"   ")
    k=1
    for(k in length(tmp))
    {
      tmp2=c(tmp[[k]])
      tmp2 = tmp2 [! tmp2 %in% c("")]
      
      
      saida = paste(saida, "<tr><td>",i,"</td><td>" ,tmp2[1], "</td><td align='right'>" ,tmp2[2], "</td>
                    <td align='right'>" ,tmp2[3], "</td>
                    <td align='right'>" ,tmp2[4], "</td>
                    <td align='right'>" ,tmp2[5], "</td>
                    </tr>")
      
      
      i=i+1
      k=k+1
      
    }
    
    
    
    j=j+1
    
    
  }
  
  saida
  
}

#
# FUNÇÕES BÁSICAS DE FORMATAÇÃO DE NÚMEROS
#

fns = function(n){
  
  prettyNum(n,big.mark = ".", big.interval =  3,decimal.mark=",")
  
  
}

fnd = function(n){
  
  prettyNum(round(n,2), big.mark = ".", big.interval = 3, decimal.mark = ",", nsmall = 2)
  
}

QuantidadeLinhas = function(texto){
  
  linhas = strsplit(texto,"\n")
  length(linhas)
  
}

TransformaEmDataFrame = function(texto){
  
  dados <<- convert2df(texto, dbsource="isi", format="plaintext")
  analise_bibliometrica <<- biblioAnalysis(dados, sep = ";")
  
}

QuantidadePublicacoes = function(){
  
  length(dados[[1]])
  
}

AutorDominance = function(qtd_autores=10){
  #qtd_autores=10

  DF <- dominance(analise_bibliometrica,qtd_autores )
  txt = capture.output(DF)
  
  
  saida = ""
  
  j=2
  i=1
  
  while(j <= length(txt)){
    
    tmp = strsplit(txt[[j]],"   ")
    k=2
    for(k in length(tmp))
    {
      tmp2=c(tmp[[k]])
      tmp2 = tmp2 [! tmp2 %in% c("")]
      
      saida = paste(saida, "<tr><td>",i,"</td><td>" ,tmp2[1], "</td><td align='right'>" ,tmp2[2], "</td>
                    <td align='right'>" ,tmp2[3], "</td>
                    <td align='right'>" ,tmp2[4], "</td>
                    <td align='right'>" ,tmp2[5], "</td>
                    <td align='right'>" ,tmp2[6], "</td></tr>")
      
      
      i=i+1
      k=k+1
      
    }
    
    
    
    j=j+1
    
    
  }
  
  saida
  
  
}

PublicacoesMaisReferenciadas = function (qtd=10){
  
  
  
  CR <- citations(dados, field = "article", sep = ".  ")
  
  publicacoes = capture.output(CR$Cited[2:qtd])
  
 i=1
  saida = ""
  j=2
  while(j <= length(publicacoes)){
    
    saida = paste(saida, "<tr><td>",i,"</td><td>" ,publicacoes[j], "</td><td>" ,publicacoes[j+1], "</td></tr>")
    j=j+2
    i=i+1
    
    
  }
  
  saida
  
}


#
# FUNÇÕES DE PALAVRAS CHAVES
#


QuantidadePalavrasChavesSemRepeticao = function(){
  
  pc = c()
  
  
  for(i in dados$ID){
    
    for(j in strsplit(i, ";")){
      
      pc = c(pc, trimws(j))
      
    }
    
  }
  
  length(unique(pc[!is.na(pc) & pc !=""]))
  
  
}

QuantidadePalavrasChavesSemRepeticaoDE = function(){
  
  pc = c()
  
  
  for(i in dados$DE){
    
    for(j in strsplit(i, ";")){
      
      pc = c(pc, trimws(j))
      
    }
    
  }
  
  
  length(unique(pc[!is.na(pc) & pc !=""]))
  
}


PalavrasChavesMaisUtilizadas = function(n=10, ano = "Todos"){
  
  pc = c()
  
  analise = dados$DE
    
  if(ano != "Todos"){
    
    analise = dados$DE[dados$PY == ano]
    
  }
  
  
  if(n == "Todos"){
    
    n = length(dados$PY)
    
  }
  
  
  
  for(i in analise){
    
    for(j in strsplit(i, ";")){
      
      
        pc = c(pc, trimws(j))
      
    }
    
  }
  
  pc = pc[pc != ""]
  
  x= sort(table(pc), decreasing = TRUE)
  
  pc = names(x)
  valores = c(as.vector(x))
  
  saida = ""
  
  for(i in 1:n){
    
    if(!is.na(pc[i])){
    
        saida =   paste(saida,"<tr>
                    <td>",i,"</td>
                    <td>",pc[i],"</td>
                    <td align='right'>",fns(valores[i]),"</td>
                    </tr>
                    ")
    }
    
    
  }
  saida
  
}

palavrasnoano = function(ano){
  
  pc = c()
  
  
  for(i in dados$DE[dados$PY==ano]){
    
    for(j in strsplit(i, ";")){
      
      pc = c(pc, trimws(j))
      
    }
    
  }
  
  tpc = sort(table(pc), decreasing = TRUE)
  
  pcs = names(tpc)
  
  qtds = c(as.vector(tpc))
  
  pcs
  qtds
  
  saida = ""
  j=1
  
  for(i in pcs){
    
    if(i>0)  saida = paste(saida, "<tr><td>",i,"</td><td>",qtds[j],"</td></tr>")
    
    j=j+1
    
  }
  
  saida
  
}

#
# FUNÇÕES DE TEMPO DE PUBLICAÇÕES
#

anomin = function(){
  
   min(dados$PY)
  
}

anomax = function(){
  
  max(dados$PY)
  
}

VetorAnos = function(){
  
  x= sort(table (dados$PY), decreasing = TRUE)
  anos = c(names(x))
  as.list(sort(anos, decreasing = TRUE))
}

VetorAnos2 = function(){
  
  x= sort(table (dados$PY), decreasing = TRUE)
  anos = c(names(x))
  
  anos = c("Todos", anos)
  
  as.list(sort(anos, decreasing = TRUE))
  
  
  
}

AnosPublicacao = function(){
  
  x= table (dados$PY)
  anos = c(names(x))
  valores = c(as.vector(x))
  saida = ""
  
  for(i in 1:length(anos)){
    
    saida =   paste(saida,"<tr>
                    <td>",anos[i],"</td>
                    <td align='right'>",fns(valores[i]),"</td>
                    </tr>
                    ")
    
    
  }
  saida
  
}


#
# FUNÇÕES DAS LÍNGUAS DE PUBLICAÇÃO
#


LinguasPublicacao = function(){
    
  x= sort(table (dados$LA), decreasing = TRUE)
  
  linguas = c(names(x))
  valores = c(as.vector(x))
  saida = ""
  
  for(i in 1:length(linguas)){
    
    saida =   paste(saida,"<tr>
                    <td>",linguas[i],"</td>
                    <td align='right'>",fns(valores[i]),"</td>
                    </tr>
                    ")
    
    
  }
  saida
  
}

#
# FUNÇÕES DOS TIPOS DE PUBLICAÇÕES
#


TiposPublicacao = function(){
  
  x= sort(table (dados$DT), decreasing = TRUE)
  
  linguas = c(names(x))
  valores = c(as.vector(x))
  saida = ""
  
  for(i in 1:length(linguas)){
    
    saida =   paste(saida,"<tr>
                    <td>",linguas[i],"</td>
                    <td align='right'>",fns(valores[i]),"</td>
                    </tr>
                    ")
    
    
  }
  saida
  
}

#
# FUNÇÕES DAS CIDADES, PAÍSES DA PUBLICAÇÃO
#


listapaises2 = function(){
  
  dt=c()
  
  for( i in dados$RP){
    
    dt = c (dt , ( gsub("[.]", "", tail(strsplit(i,split=" ")[[1]],1)   ) ) )
    
  }
  
  
  ff= sort(table(dt), decreasing = TRUE)
  
  paises = names(ff)
  qtds = as.vector(ff)
  
  saida2 = ""
  
  for(i in 1:length(paises)){
    
    saida2 =   paste(saida2, "<tr><td>",paises[i],"</td><td align='right'>",fns(qtds[i]),"</td></tr>")
    
    
  }
  
  saida2
  
  
}

periodicostop = function(n,ano="Todos"){
  
  if(ano != "Todos") {periodicos = dados$PU[dados$PY==ano]}
  else {periodicos = dados$PU}
  
  periodicos
  
  t=sort(table(periodicos), decreasing = TRUE)
  nomes = names(t)
  qtds = as.vector(t)
  
  saida = ""
  
  for(i in 1:n){
    
    saida =   paste(saida,"<tr>
                    <td>",nomes[i],"</td>
                    <td align='right'>",fns(qtds[i]),"</td>
                    </tr>
                    ")
    
    
  }
  
  saida
  
}


CidadesPublicacao = function(){
  
  linguas = c(names(table (dados$PI)))
  valores = c(as.vector(table(dados$PI)))
  saida = ""
  
  for(i in 1:length(linguas)){
    
    saida =   paste(saida,"<tr>
                    <td>",linguas[i],"</td>
                    <td align='right'>",fns(valores[i]),"</td>
                    </tr>
                    ")
    
    
  }
  saida
  
}

#
# FUNÇÕES SOBRE OS AUTORES
#


autorestop = function(n,ano="Todos"){
  
  if(ano != "Todos") {autores = dados$AU[dados$PY==ano]}
  else {autores = dados$AU}
  
  
  vautores = c()
  
  for(i in autores){
    
   
    for(j in strsplit(i, ";"))   vautores = c(vautores, trimws(j))
    
  }
  
  t=sort(table(vautores), decreasing = TRUE)
  autores = names(t)
  qtds = as.vector(t)
  
  saida = ""
  
  for(i in 1:n){
    
    saida =   paste(saida,"<tr>
                    <td>",autores[i],"</td>
                    <td align='right'>",fns(qtds[i]),"</td>
                    </tr>
                    ")
    
    
  }
  
  saida
  
  
  
}

QtdAutores = function(){
  
  autores = dados$AU
  vautores = c()
  
  for(i in autores){
    
    
    for(j in strsplit(i, ";"))   vautores = c(vautores, trimws(j))
    
  }
  length(unique(vautores[!is.na(vautores) & vautores != ""]))
  
}

QtdPublicacoesUmAutor = function(){
  
  
  autores = as.vector(dados$AU)
  autores  = autores[autores!= "[ANONYMOUS]"& !is.na(autores) & autores != ""]
  
  
  qtd = 0
  v =c()
  
  for(i in autores){
     
    
    #print (i)
    #print (length(strsplit(i, ";")[[1]]))
    
    v = c(v , (length(strsplit(i, ";")[[1]])))
       
    
  }  
  
  #v[v==1]
  length(v[v==1])
  
}



QtdPublicacoesComMaisDeUmAutor = function(){
  autores = dados$AU
  qtd = 0
  
  for(i in autores){
    
    
    if(lengths(strsplit(i, ";")) >= 2)   qtd = qtd + 1
    
  }
  
  qtd
  
}

QtdPublicacoesSemAutor = function(){
  
  
  autores = as.vector(dados$AU)
  autores  = autores[autores== "[ANONYMOUS]" | is.na(autores) | autores == ""]
  
  
  qtd = 0
  v =c()
  
  for(i in autores){
    
    
    
    v = c(v , (length(strsplit(i, ";")[[1]])))
    
    
  }  
  
  #v[v==1]
  length(v)
  
}


MediadeAutoresPorPublicacao = function(){
  
  
  QtdAutores()/QuantidadePublicacoes()
  
}


leidebradford = function(){
  
  
  categorias = dados$SC
  
  
  vcategorias = c()
  
  for(i in categorias){
    
    for(j in strsplit(i, ";"))   vcategorias = c(vcategorias, trimws(j))
    
  }
  
  t=sort(table(vcategorias[vcategorias!=""]), decreasing = TRUE)
  categorias = sort(names(t))
  
  categorias
  
  saida = "<tr><th >PU/SC</th><th>Total</th>"
  
  for(categoria in categorias){
    
    saida = paste(saida, "<th >",categoria,"</th>")
    
  }
  
  saida = paste(saida, "</tr>")
  
  editores = sort(unique(dados$PU), decreasing = FALSE )
  
  for(editor in editores){
    
    saida = paste(saida, "<tr><td>",editor,"</td>")
    
    z = 0
    tmp=""
    for(categoria in categorias){
      
      t = length (   dados$PU[dados$PU == editor & grepl(categoria, dados$SC) ]   )
      z=z+t
      
      tmp = paste(tmp, "<td><center>",t,"</center></td>")
      
    }
    
    saida = paste( saida, "<td><center>",z,"</center></td>",tmp,"</tr>")
    
  }
  
  saida
  
  
}


#
# CITAÇÕES POR ARTIGO
#


MediadeCitacoesporArtigo = function(){
  
   #citacoes = dados$CR
   #vcitacoes = c()
  
  #for(i in citacoes){
   # for(j in strsplit(i, ";"))   vcitacoes = c(vcitacoes, trimws(j))
  #}
   #length(vcitacoes[!is.na(vcitacoes) & vcitacoes !=""])/QuantidadePublicacoes()
   
   citacoes = dados$NR
   sum(as.numeric(citacoes))/QuantidadePublicacoes()
  
}

acoplamento = function(){
  
  x = dados$UT[dados$AU !="[ANONYMOUS]"]
  x = x[!is.na(x)]
  
  y=x
  
  saida = ""
  
  saida = "<tr><th>artigoxartigo</th>"
  
  for(i in x){
    
    saida = paste(saida, "<th>",i,"</th>")
    
  }
  
 
  ppp= 1
  for(i in x){
    
    saida = paste(saida, "<tr><td>",i,"</td>")
    
     
    
     
     ppp = ppp+1
     
     ttt=1
      for(j in y){
        print (paste(">>",ttt, ":",ppp))
        
        ttt = ttt+1
        
        
        saida = paste(saida, "<td>1</td>")
       
        
      }
    
    saida = paste(saida, "</tr>")
      
    
  }
  
   saida
  
  
}

#
# PROGRAMA PRINCIPAL
#


ui = fluidPage(
  
  titlePanel("Análise Bibliométrica"),
  
  
  #ABA INICIAL
  tabsetPanel(
    tabPanel(
      "Dados",
      mainPanel(
          
          fileInput(
                    'file1', 
                    'Escolha o arquivo:',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')
          ),
          mainPanel(
            textOutput('paginainicial')
          )
      )
    ),
    
    #ABA DE RESUMO
    tabPanel(
      "Resumo",
      htmlOutput("resumo")
    ),
    tabPanel(
      "Publicações",
      tabsetPanel( 
        
        tabPanel("Mais Referenciados",  mainPanel(
          
          selectInput("qtd_top_pu", 
                      "Quantidade de Registros: ",
                      c("10" = 10,"20" = 20, "30" = 30,"40" = 40)),
          
          htmlOutput("publicacoes")
          
        )
        )
        
      )
    ),
    
    #ABA DE PALAVRAS CHAVES
    tabPanel(
      "Palavras Chaves",
      tabsetPanel( 
        
        tabPanel("Mais Utilizadas",  mainPanel(
                                  
                                      selectInput("qtd_top_pc", 
                                                  "Quantidade de Registros: ",
                                                  c("10" = 10,"20" = 20, "30" = 30,"40" = 40, "Todos")),
                                      selectInput("select_ano", "Escolha o ano: ", c()),
              
                                      htmlOutput("top10palavraschaves")
                                  
                                  )
                 ),
       
        
        tabPanel("Nuvem de Palavras (Lei de Zipf)",  mainPanel(
          
          plotOutput("mapapalavraschaves", width = "100%")
          
        )
        )
        
      )
    ),
    
    tabPanel(
      "Autores",
      tabsetPanel(
        
        tabPanel("Autores que mais publicaram (Lei de Lotka)", mainPanel(
          
          selectInput("qtd_top_autores", 
                      "Qtd. de Autores: ",
                      c("10 Mais" = 10,"20 Mais" = 20, "30 Mais" = 30,"40 Mais" = 40)),
          selectInput("select_ano_autor", "Escolha o ano: ", c()),
          
          htmlOutput("top10autores")
          
        )),
        
        tabPanel("Índice-H", mainPanel(
          
          selectInput("qtd_top_h", 
                      "Qtd. de Autores: ",
                      c(10,20,  30, 40)),
          
          htmlOutput("indiceh")
          
        )),
        
        tabPanel("Dominance", mainPanel(
          
          selectInput("qtd_top_dominance", 
                      "Qtd. de Autores: ",
                      c(10,20,  30, 40)),
          
          htmlOutput("topdominance")
          
        ))
        
        
      )
    ),
    
    tabPanel(
      "Periódicos",
      tabsetPanel(
        
        tabPanel("Periódicos que mais publicaram", mainPanel(
          
          selectInput("qtd_top_periodicos", 
                      "Qtd. de periodicos: ",
                      c("10 Mais" = 10,"20 Mais" = 20, "30 Mais" = 30,"40 Mais" = 40)),
          selectInput("select_ano_periodico", "Escolha o ano: ", c()),
          
          htmlOutput("top10periodicos")
          
        )),
        
        
        tabPanel("Lei de Bradford", mainPanel(
        
          htmlOutput("leidebradford")
          
        ))
        
      )
    ),
    
    tabPanel(
      
      "Citações",
      tabsetPanel(
        
        tabPanel(
         
          "Local Citations",
          
          selectInput("qtd_top_local_citation", 
                      "Qtd. de Autores: ",
                      c("10 Mais" = 10,"20 Mais" = 20, "30 Mais" = 30,"40 Mais" = 40)),
          htmlOutput("localcitations")
        ),
        
        tabPanel(
          
          "Acoplamento",
          htmlOutput("acoplamento")
        )
        
      )
      
    )

  )
)

server = function(input, output, session){
  
  
  output$indiceh = renderText({
    
    HTML(paste(
      
      "<table width=\"90%\" border='1'> 
      <caption>Autor - Local Citation:</caption>
      <tr>
      <th>n.</th>
      <th>Autor</th>
      <th>Índice H</th>
      <th>Índice G-M</th>
      
      </tr>
      
      ",topindiceh(as.numeric(input$qtd_top_h)),"</table>"
    )) 
    
    
    
  })
  
  output$localcitations = renderText({
    
    HTML(paste(
      
      "<table width=\"90%\" border='1'> 
      <caption>Autor - Local Citation:</caption>
      
      ",localcitation(as.numeric(input$qtd_top_local_citation)),"</table>"
    )) 
    
    
  })
  output$topdominance = renderText({
    
    HTML(paste(
      
      "<table width=\"90%\" border='1'> 
      <caption>Autor - Dominance:</caption>
      
      <tr><th><center>n.</center></th>
        <th>Nome</th>
        <th>Fator Dominance</th>
        <th>Mult Autoral</th>
        <th>Primeiro Autor</th>
        <th>Rank por Artigo</th>
<th>Rank por DF</th>


      </tr>
      
      ",AutorDominance(as.numeric(input$qtd_top_dominance)),"</table>"
    )) 
    
  })
  
  output$publicacoes=renderText({
    HTML(paste(
      
      "<table width=\"90%\" border='1'> <th><center>n.</center></th><th><center>Publicação</center></th><th><center>Qtd. de Referências</center></th>
     <caption>Publicacões mais Referênciadas:</caption>",PublicacoesMaisReferenciadas(input$qtd_top_pu),"</table>"))
    
    
  })
  
  output$acoplamento = renderText({
    
    HTML(paste(
      
      "<table width=\"90%\" border='1'>
      <caption>Acoplamento Bibliográfico<caption><tr>
     
      </tr>",acoplamento(),"</table>"
      
    ))
    
    
  })
  
  
  
  output$leidebradford = renderText({
    
    HTML(paste(
      
      "<table width=\"90%\" border='1'>
      <caption>Lei de Bradford<caption>",leidebradford(),"</table>"
      
    ))
    
    
  })
  
  output$top10periodicos= renderText({
    
    HTML(paste("<table width=\"90%\" border='1'><caption>Periódicos que mais publicam - TOP 
              (",input$qtd_top_periodicos,")<caption>
               <tr><th><center>Palavra</center></th><th><center>Qtd.</center></th></tr>
               
               ",
               
               periodicostop(input$qtd_top_periodicos, input$select_ano_periodico)
               
               ,"
               
               </table>"))
    
  })
  
  output$top10autores = renderText({
    
    HTML(paste("<table width=\"90%\" border='1'><caption>Autores que mais publicam - TOP 
              (",input$qtd_top_autores,")<caption>
               <tr><th><center>Palavra</center></th><th><center>Qtd.</center></th></tr>
               
               ",
               
               autorestop(input$qtd_top_autores, input$select_ano_autor)
               
               ,"
               
               </table>"))
    
    
  })
  
  
  output$mapapalavraschaves = renderPlot({
    
    
    d = dados$DE[!is.na(dados$DE)]
    
    t = Corpus(VectorSource(d))
    t <- tm_map(t, content_transformer(tolower))
    t <- tm_map(t, removePunctuation)
    t <- tm_map(t, PlainTextDocument)
    t <- tm_map(t, removeWords, stopwords('english'))
    
    t <- tm_map(t, stemDocument)
    wordcloud(t, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
   
    })
  
  
  
  output$paginainicial = renderText({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)

    texto = readLines(inFile$datapath)
    
    TransformaEmDataFrame(texto)
    
    updateSelectInput(session, "select_ano", choices = VetorAnos2())
    updateSelectInput(session, "select_ano_autor", choices = VetorAnos2())
    updateSelectInput(session, "select_ano_periodico", choices = VetorAnos2())
    
    updateSelectInput(session, "select_ano_pu", choices = VetorAnos2())
    
    
    print(paste("Quantidade de Publicações no arquivo: ", QuantidadePublicacoes()))
    
    
    
  })
  
  
  output$resumo = renderText({
    
    HTML(paste("
          <center>
         <p/>
         <table width=\"90%\" border='1'><caption>Análise Bibliométrica Básica<caption>
                            <tr>
                <td width='70%'>Quantidade de Artigos:</td>
                <td align='right'>",fns(nrow(dados)),"</td>
              </tr>
              <tr>
                <td>Fontes de Informações (Revistas/Períodicos):</td>
                <td align='right'>",fns(length(unique(dados$SO))),"</td>
               </tr>
          </table>
          <table width=\"90%\" border='1'> 
          <caption>Palavras Chaves</caption>
               <tr>
                <td width='70%'>Palavras Chaves sem Repetição (Thomson Reuters:ID):</td>
                <td align='right'>",fns(QuantidadePalavrasChavesSemRepeticao()),"</td>
               </tr>
                <tr>
                <td>Média (Thomson Reuters:ID):</td>
               <td align='right'>",fnd(QuantidadePalavrasChavesSemRepeticao()/nrow(dados)),"</td>
               </tr>
              <tr>
                <td>Palavras Chaves sem Repetição  (Autores:DE) :</td>
               <td align='right'>",fns(QuantidadePalavrasChavesSemRepeticaoDE()),"</td>
               </tr>
            </tr>
              <tr>
               <td>Média (Autores:DE) :</td>
               <td align='right'>",fnd(QuantidadePalavrasChavesSemRepeticaoDE()/nrow(dados)),"</td>
               </tr>
         </table>

            <table width=\"90%\" border='1'> <caption>Autores</caption>
               <tr>   
               <td width='70%'>Quantidade de Autores:</td><td align='right'>",fns(QtdAutores()),"</td></tr>
                <tr>  <td >Publicações com um único autor:</td><td align='right'>",fns(QtdPublicacoesUmAutor()),"</td></tr>
               
                  <tr>   <td >Publicações com múltiplos autores:</td><td align='right'>",QtdPublicacoesComMaisDeUmAutor(), "</td></tr>

               <tr>   <td >Publicações Anônimas, NA, ou \"\":</td><td align='right'>",QtdPublicacoesSemAutor(),"</td>
               </tr>
                 <tr>   <td >Média de Qtd. de Autor por Publicação:</td><td align='right'>",fnd(MediadeAutoresPorPublicacao()),"</td>
                <tr>   <td >Média de Qtd. de Publicações por Autor:</td><td align='right'>",fnd(1/MediadeAutoresPorPublicacao()),"</td>

               </tr>

 <tr>   <td >Autores (Apparences):</td><td align='right'>",fns(analise_bibliometrica$Apparences),"</td>

             

 <tr>   <td >Média de CoAutores por Artigo:</td><td align='right'>",fnd(mean(analise_bibliometrica$nAUperPaper)),"</td>




 <tr>   <td >Índice de Colaboração:</td><td align='right'>",fnd(analise_bibliometrica$AuMultiAuthoredArt/sum(analise_bibliometrica$nAUperPaper>1)),"</td>
               </table>




          <table width=\"90%\" border='1'> <caption>Anos de Publicação</caption>
                   <tr>
                      <th width='70%'>
                          <center>
                            Ano
                          </center>
                      </th>
                      <th>
                        <center>
                          Qtd. Publicações
                        </center>
                      </th>
                    </tr>
                    ",AnosPublicacao(),"
          
          </table>


          <table width=\"90%\" border='1'> <caption>Línguas</caption>
               <tr>
               <th width='70%'>
               <center>
               Língua
               </center>
               </th>
               <th>
               <center>
               Qtd. Publicações
               </center>
               </th>
               </tr>
               ",fns(LinguasPublicacao()),"
               
               </table>
 <table width=\"90%\" border='1'> 
<caption>Outros</caption>
               <tr>
                <td width='70%'>Média de Citações por Artigo</td>
                <td align='right'>",fnd(MediadeCitacoesporArtigo()),"</td>
               </tr>
 <tr>
                <td width='70%'>Índice de Crescimento Anual</td>
               <td align='right'>",fnd(IndiceCrescimentoAnual()),"</td>
               </tr>
         </table>


 <table width=\"90%\" border='1'> <caption>Tipos </caption>
               <tr>
               <th width='70%'>
               <center>
               Tipo de Publicação
               </center>
               </th>
               <th>
               <center>
               Qtd. Publicações
               </center>
               </th>
               </tr>
               ",fns(TiposPublicacao()),"</table>
               
              <table width=\"90%\" border='1'> <caption>Países</caption>
               <tr>
               <th width='70%'>
               <center>
               País
               </center>
               </th>
               <th>
               <center>
               Qtd. de Publicações
               </center>
               </th>
               </tr>
               ",listapaises2(),"</table>

</center>
         "))
    
  })
 
  output$top10palavraschaves = renderText({
    
   
    HTML(paste("
               
               <center><table  width=\"90%\" border='1'>
                <caption>Tipos </caption>
                 <tr>
                <th width='10%'>
                 <center>
               n.
               </center>
               </th> 

                <th width='70%'>
                 <center>
                 Palavra
                 </center>
                 </th>
                 <th>
                 <center>
                 Qtd. Citações
                 </center>
                 </th>
                 </tr>",
                  
               PalavrasChavesMaisUtilizadas(input$qtd_top_pc, input$select_ano)

                ,"</table></center>
               
               
               "))
    
    
  })
  
  
  
}

shinyApp(ui=ui, server=server)

