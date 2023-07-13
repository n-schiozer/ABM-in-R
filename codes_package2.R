#Pacotes:
  
install.packages("Rlab ")

install.packages("Gmisc")

install.packages("Rfast")

install.packages("Matrix")

install.packages("data.table")

install.packages("DescTools")

install.packages("pryr")

install.packages("Rfast")

install.packages("matrixStats")

install.packages("Rlab")

install.packages("microbenchmark")

install.packages("mFilter")

install.packages("doParallel")

install.packages("Rlab")

install.packages("purrr") # Par aa função possibly

install.packages("randomcoloR") # Para os gráficos de rede

install.packages("trycatch")


library(Rlab)

library(parallel)

library(doParallel)

library(mFilter)

library(DescTools)

library(data.table)

library(Matrix)

library(Rfast)

library(stringr) # Operações com strings

library(sjmisc)

library(ggplot2)

library(network)

library(igraph) # Para gráficos de redes

library(dplyr)

library(pryr) # Where function, para encontrar em que ambiente etá a variável

library(mosaic)

library(mosaicCalc)

library(Rlab)

library(matrixStats) # Para o load do sum2

library(purrr) # Par aa função possibly
 
library(Gmisc)

library(microbenchmark) # Para o microbenchmark

library(randomcoloR) # Para os gráficos de rede

#library(trycatch) # Para o DEBBUG


# Função para criar funções

dar_nomes_lista_funcoes <- function(list_funcoes){
  
  for(numlista in 1:length(list_funcoes)){
    
    
    Frase<-toString(body(list_funcoes[[numlista]]))
    
    Inicio <- str_locate( Frase , 'return')[2] + 2
    
    Opcoes <- str_locate_all( Frase , "\\)")
    
    Final <- 0
    for(i in 1:length(Opcoes[[1]][,2])){
      
      if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
      
    }
    
    Variavel <- str_sub(Frase,Inicio,Final)
    
    if(numlista == 1){ nomes <- c(Variavel)  }else{
      nomes <- c(nomes,Variavel)
    }
    
    
  }
  names(list_funcoes) <- nomes
  
  return(list_funcoes)
}

check_estrutura_modelo <- function(list_funcoes){
  
  list_funcoes <- dar_nomes_lista_funcoes(list_funcoes)
  
  if( any( table(names(list_funcoes)) > 1 ) ){
    
    print(paste("Variável com problema:",names(which( table(names(list_funcoes)) > 1 ))))
    stop("Há um erro na estruturação do modelo! Uma variável está sendo determinada por mais de uma função ao mesmo tempo. Verificar se há funções repetidas em 'list_function' e o 'return' das funções.")
    
    ;break}
  
  #if( exists( "matrixcheck" ) ){ rm(matrixcheck) }
  
  Flag <- matrix(rep(0, length(list_funcoes)),1,length(list_funcoes))
  colnames(Flag) <- names(list_funcoes)
  
  contador <- 0
  contador1 <- 0
  
  while( length(  Flag[ Flag != 0] ) < length(Flag) ){
    
    contador1 <- contador1 + 1
    
    if( contador1 >  length(Flag)) print("Ordenamento das variáveis demorando mais do que o esperado.")
    if( contador1 >  length(Flag)) print("Não estamos conseguindo ordenar as variáveis. Provável circularidade de causalidade indiretamente.")
    
    if(contador1 > length(Flag)) print(paste("Variáveis que faltam ordenar:", colnames(Flag)[Flag==0] ))
    
    if(contador1 == length(Flag) + 1){
      
      stop("Não estamos conseguindo ordenar as variáveis. Provável circularidade de causalidade indiretamente.")
      break
    }
    
    
    Break <- 0
    
    for(numlista in 1:length(list_funcoes)){
      
      print(numlista)
      
      if( Flag[1,numlista] == 0 ){ # Se não determinou posição ainda
        
        body_funcao <- toString(body(list_funcoes[[numlista]])) # Corpo
        
        variables <-  str_extract_all(body_funcao,"VAR\\([^()]+\\)")[[1]] %>% # seleção variáveis que causam
          str_replace("VAR\\(","") %>%
          str_extract_all("[^\\)]")
        
        
        if( str_detect(body_funcao,"apply") ){ # Looping para detectar a existÊncia de função auxiliar e coletar o VAR dela
          
          
          internal_func <- paste((str_extract_all(body_funcao,"apply\\([^()]+\\)") %>%
                                    str_extract_all(",[^()]+\\)") %>%
                                    str_extract_all("[^,\\s\\)]"))[[1]], collapse='')
          
          internal_func <- (str_extract_all(body_funcao,"apply\\([^()]+\\)") %>%
                              str_extract_all(",[^()]+\\)") %>%
                              str_split(","))[[1]]
          
          internal_func <- internal_func[ length(internal_func)]
          
          internal_func <- (str_extract_all(internal_func,"[^\\s\\FUN=)]"))[[1]]
          
          internal_func <- paste(internal_func,collapse = '')
          
          body_funcao <- toString(body(internal_func)) # Coleta do body da função auxiliar
          
          variables1 <-  str_extract_all(body_funcao,"VAR\\([^()]+\\)")[[1]] %>%
            str_replace("VAR\\(","") %>%
            str_extract_all("[^\\)]")
          
          if( identical(variables1,list()) == FALSE ){
            
            variables <- c(variables,variables1) # Adiciona componentes da auxiliar no existente
            
          }
        }
        
        
        
        if( length(variables) == 0 ){ Flag[1,numlista]  <- length(  Flag[ Flag != 0] ) + 1 }else{ # Se não tiver variáveis. Então, posição já é a seguinte disponível
          
          
          for(i in 1:length(variables)){ # Juntando os caracteres em palavra
            
            variables[[i]] <- paste(variables[[i]],collapse = '')
            
          }
          
          if( any( variables == names(list_funcoes[numlista]) ) == TRUE ) { # Retirando vari?vel resultado dos inputs
            
            variables <- variables[ variables != names(list_funcoes[numlista]) ]
            
          }
          
          if( contador == 0){ # Se contador for nulo, preciso lista de checagem
            
            if( exists( "matrixcheck" ) == FALSE ){ matrixcheck <- list(list( names(list_funcoes)[numlista] , unlist(variables) )) }else{
              
              matrixcheck <- c( list(list( names(list_funcoes)[numlista] , unlist(variables) )) , matrixcheck)
              
            } # Criando lista de checagem. Se já existe, adicionando a lista de checagem
          }
          
          Flag2 <- length(  Flag[ Flag != 0] ) + 1 # Qual é a ordem que irá entrar a função na lista
          
          for(y in 1:length(variables)){ # Para cada uma das variáveis que causam a variável selecionada
            if( any( colnames(Flag) == variables[y] ) ){ # SE houver algum elemento da Flag que for igual à input de variável.
              
              
              position <- which(colnames(Flag) == variables[y]) # Coleta posição da variável que causa variável selecionada
              
              
              if( Flag[1,position] == 0 ){ Flag2 <- 0 } # Se valor for nulo, quer dizer que a função ainda não rodou do input.
              # Quer dizer que estou colocando a variável selecionada ANTES de uma variável que é INPUT CONTEMPORANEAMENTE.
              # Preciso resolver a outra variável antes. Logo, volta o valor para 0
              
            }
            
            Flag[1,numlista] <- Flag2 # Substitui valor de Flag2 em Flag1. Seja 0 ou não.
            
          }
        }
        
        if(  exists( "matrixcheck" ) ){ # Looping para detectar causalidade simultânea contemporânea
          # Algo que ABM não resolve. Preciso ser corrigido pelo autor.
          
          for(numlista in 1:length(matrixcheck)){ # P/ cada uma das variáveis
            for(z in 1:length(matrixcheck)){ # Para todas as variáveis
              if(numlista < z){ # Se posição menor (Feito para evitar mensagens repetidas)
                
                if( any( unlist(matrixcheck[[numlista]][1]) == unlist(matrixcheck[[z]][2]) ) == TRUE &&  # Checa se nome da função X igual à input de função Y
                    any( unlist(matrixcheck[[z]][1]) == unlist(matrixcheck[[numlista]][2]) ) == TRUE ){ # Checa se nome da função Y igual à input de função X
                  
                  # Se ambas positivas, existe causalidade simultânea comtenporânea:
                  print(paste("Existe causalidade mutua e simultânea entre",matrixcheck[[numlista]][1],"e",
                              matrixcheck[[z]][1],". Sugestão: colocar alguma variável causada por lag da outra."))
                  
                  Break <- 1 # Break. Para a função. Problema deve ser resolvido antes de prosseguir.
                  if(Break == 1){ break }
                  
                }
              }
            }
          }
        }
      }
      
      #if(Break == 1){print(matrixcheck)}
      if(Break == 1){ break }
      
    }
    
    contador <- 1
    
  }
  
  # Criação da nova lista de funções com base em FUN  ----------------------------------------------------------------------------
  
  list_alternativa <- list_funcoes
  
  nomes <- names(list_funcoes)
  
  nomes_alternativos <- nomes
  
  for(i in 1:length(list_funcoes)){
    
    list_alternativa[[ Flag[1,i]   ]] <- list_funcoes[[i]]
    
    nomes_alternativos[ Flag[1,i] ] <- c(nomes[i])
    
  }
  
  names(list_alternativa) <- nomes_alternativos
  
  base::assign("list_funcoes", list_alternativa , envir= .GlobalEnv)
  
  # Estrutura do modelo ----------------------------------------------------------------------------------------------
  
  # Criação da matriz
  matrix_network <- matrix( rep(0,length( names(list_funcoes))^2) , length( names(list_funcoes)) , length( names(list_funcoes)))
  colnames(matrix_network) <- names(list_funcoes)
  row.names(matrix_network) <- names(list_funcoes)
  
  for(numlista in 1:length(list_funcoes)){
    
    body_funcao <- base::toString(body(list_funcoes[[numlista]])) # Corpo
    
    variables <-  stringr::str_extract_all(body_funcao,"LAG\\([^()]+\\)")[[1]] %>% # seleção variáveis que causam
      stringr::str_replace("LAG\\(","") %>%
      stringr::str_extract_all("[^\\)]")
    
    
    for(k in 1:length(matrixcheck)){
      
      # if( matrixcheck[[k]][[1]] == names(list_funcoes)[numlista]) print("Ok") Futura extrutura para adicionar LAGs na rede
      
      
    }
    
  }
  
  
  # Fazendo ligações
  
  for(i in 1:length(matrixcheck)){
    if( is.null(matrixcheck[[i]][2]) == FALSE){
      for(y in 1:length( unlist( matrixcheck[[i]][2] ))){
        
        colmatrix <- which( colnames(matrix_network) == matrixcheck[[i]][1] )
        rowmatrix <- which( row.names(matrix_network) == unlist(matrixcheck[[i]][2])[y]   )
        
        
        matrix_network[rowmatrix , colmatrix] <-  1 #matrix_network[rowmatrix , colmatrix]
        
      }
    }
  }
  
  #net2 <- graph_from_incidence_matrix(matrix_network)
  #net2.bp <- bipartite.projection(net2)
  #plot(net2.bp$proj2)
  
  # Diferenciação de cores por tipo de agente
  agent_class <- c(1:length(list_funcoes))
  
  for(i in 1: length(list_funcoes)){
    
    string <- toString(names(list_funcoes)[[i]])
    
    pos <- str_locate(string, "_")[1,1]
    
    agent_class[i] <- substr(string,1,pos-1)
    
  }
  
  agent_class_color <- agent_class
  
  palette <- distinctColorPalette( length( table(agent_class)) )
  
  for(z in 1:length( table(agent_class))){
    
    agent_class_color[ agent_class_color == names(table(agent_class_color)[z]) ] <- palette[z]
    
  }
  
  ## Criação do gráfico
  
  #par(mar = c(1, 1, 1,1))
  
  net3 <- graph_from_adjacency_matrix(matrix_network)
  
  
  V(net3)$color <- agent_class_color
  #V(net3)$label <- NA
  V(net3)$size <- 13
  
  #dev.new(width=21*9/10, height=29.7*9/10,unit="cm")
  
  # Gráfico
  
  Vec.net3 <- evcent(net3)$vector
  
  #png(file="mygraphic1.png",res=72*2)#,width=21*9/10,height=29.7*9/10)

  plot(net3 , vertex.shape="circle",
       ylim=c(-1,1),xlim=c(-1,1),
       rescale=TRUE,
       vertex.size = 10,
       #vertex.size = Vec.net3/max(Vec.net3) * 20,
       vertex.label.cex=0.65, vertex.label.font=20,
       edge.width=2,edge.curved = 0.25 , edge.arrow.size= 0.4,
       layout= layout_with_graphopt)

  legend(x=-1.4, y=-1, names(table(agent_class)), pch=21,
         col="#777777", pt.bg=palette, pt.cex=2, cex=.8, bty="n", ncol=2)
  
  #dev.off()
  
  #centralityPlot()
  
  degree(net3, mode="all")
  sort(degree(net3, mode="all"))
  centr_degree(net3, mode = "out")
  closeness(net3,mode="out") # Out degree = quanto AFETA a rede
  closeness(net3,mode="in") # In degree = quanto é AFETADO pela rede
  closeness(net3,mode="all")  # All - soma. O quanto sobre de circularidade da rede.
  
  
  sort(evcent(net3)$vector)
  sort(betweenness(net3))
  
  sort( closeness(net3,mode="all") ,decreasing = TRUE) # All - soma. O quanto sobre de circularidade da rede.
  sort( closeness(net3,mode="out") ,decreasing = TRUE)# Out degree - quanto AFETA a rede
  sort( closeness(net3,mode="in") ,decreasing = TRUE) # In degree = quanto é AFETADO pela rede
  
  sort( degree(net3, mode="all") ,decreasing = TRUE)
  
  
  #measures how frequently a vertex lies on the shortest path between any two vertices in the network
  # High betweenness - key bridges between diff parts of the network
  
  #sort( betweenness(net3, directed = TRUE),decreasing = TRUE)
  
  #matrix_network <- as.network.matrix(matrix_network)
  #network <- network(matrix_network)
  #plot(network, vertex.cex = 2, displaylabels = TRUE, boxed.labels = TRUE)
  #paste( matrixcheck[[i]][2] , sep=" ")
  
  sort( eigen_centrality(net3)$vector,decreasing = TRUE)
  
  
  if(Break == 0){print("Nova lista obtida")}
  
}

addfunABM <- function(name){
  
  # Checagem se a variável é repetida
  
  # Coleta do nome
  Frase<-toString(body(name))
  
  Inicio <- str_locate( Frase , 'return')[2] + 2
  
  Opcoes <- str_locate_all( Frase , "\\)")
  
  Final <- 0
  for(i in 1:length(Opcoes[[1]][,2])){
    
    if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
    
  }
  
  Variavel <- str_sub(Frase,Inicio,Final)
  
  # Check após coleta do nome:
  if(exists("list_funcoes")){
    if( identical(list_funcoes,list()) == FALSE){
      if( any( names(list_funcoes) == Variavel ) ){ stop("Já existe uma função com o nome da variável em 'return'. Função NÃO adicionada!") }
    }
  }
  
  #if(numlista == 1){ nomes <- c(Variavel)  }else{
  #  nomes <- c(nomes,Variavel)
  #}
  
  
  # ----------------------------
  
  # Checagem se foram usados "LAG" ou "VAR" p/ as variáveis que são inputs
  size <- length( names(formals(name)) )
  
  if( size > 0 ){
    
    for(i in 1:size){
      if( str_detect( toString(body(name)) , names(formals(name))[1]  ) ){
        
        pos <- str_locate_all(pattern = names(formals(name))[1] , toString(body(name)) )[[1]][1,1]
        
        for(y in 1:length(pos)){
          
          substr( toString(body(name)) , pos[y] - 10, pos[y] - 1)
          
          if( any(str_detect( toString(body(name)) , c("VAR","LAG"))) == FALSE ){
            
            print("Não foi definido VAR ou LAG para variável definida como INPUT da função!")
            
          }
          
          if( any(str_detect( toString(body(name)) , c("LAG"))) ){ # Detecta se est? presente o identificador de lag
            
            lista <- str_extract_all(toString(body(name)),"LAG\\([^()]+\\)")[[1]] %>%
              str_extract_all(",[^()]+\\)") %>% # Problema. Não pega quando esquece da vírgula!
              str_extract_all("[^,\\s\\)]")
            
            lista <- as.numeric(lista)
            
            if( any( is.numeric(lista[[1]]) ) == FALSE) stop("Não foi definido o valor do LAG")
          }
        }
      }
    }
    
  }
  
  # Adição da função na lista de funções do ABM
  
  if( exists("list_funcoes")){ list_funcoes <- c(list_funcoes,name) ; base::assign("list_funcoes",list_funcoes,envir = .GlobalEnv )}else{
    
    base::assign("list_funcoes",list(name),envir = .GlobalEnv )
  }
  
  # Criação de lista já com nomes
  list_funcoes <- dar_nomes_lista_funcoes(list_funcoes)
  base::assign("list_funcoes",list_funcoes,envir = .GlobalEnv )
  # Retorna pro ambiente global!
  
  print(paste("Função adicionada com sucesso!"))
  
}

check_functions <- function(list_funcoes){
  
  flag <- 0
  
  for(y in 1:length(list_funcoes)){
    
    for(i in 1:length( formals(list_funcoes[[y]]) )){
      
      if( exists( names( formals(list_funcoes[[y]])[i]) ) == FALSE ){
        
        print(paste("N?o existe a vari?vel", names( formals(list_funcoes[[y]])[i] )))
        flag <- 1
        
      }
    }
    
    if( str_detect( toString(body(list_funcoes[[y]])) ,"return") == FALSE){
      
      print(paste("N?o foi definido return da fun??o",y))
      
    }else{
      
      Frase<-toString(body(list_funcoes[[y]]))
      
      Inicio <- str_locate( Frase , 'return')[2] + 2
      
      Opcoes <- str_locate_all( Frase , "\\)")
      
      Final <- 0
      for(i in 1:length(Opcoes[[1]][,2])){
        
        if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
        
      }
      
      Variavel <- str_sub(Frase,Inicio,Final)
      
      if( exists( paste(Variavel) ) == FALSE ){
        
        print(paste("N?o existe a vari?vel", paste( Variavel )))
        
        flag <- 1
        
      }
    }
  }
  
  if(flag == 0){ print("Existem todas as vari?veis que s?o inputs das fun??es")}
  
}

LAG <- function(matrix , lag) return(matrix[,lag+1])

VAR <- function(matrix) return(matrix[,1])

# FUnção para criar agentes

create_agent <- function(name, statevariables, initialvariables, quantidade_agentes){
  
  # Ideia que preciso dar valor de condicao inicial. Se quiser modificar condicao inicial, preciso alterar depois
  
  # Funções de suporte
  
  dar_nomes_lista_funcoes <- function(list_funcoes){
    
    for(numlista in 1:length(list_funcoes)){
      
      Frase<-toString(body(list_funcoes[[numlista]]))
      
      Inicio <- str_locate( Frase , 'return')[2] + 2
      
      Opcoes <- str_locate_all( Frase , "\\)")
      
      Final <- 0
      for(i in 1:length(Opcoes[[1]][,2])){
        
        if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
        
      }
      
      Variavel <- str_sub(Frase,Inicio,Final)
      
      if(numlista == 1){ nomes <- c(Variavel)  }else{
        nomes <- c(nomes,Variavel)
      }
      
      
    }
    names(list_funcoes) <- nomes
    
    return(list_funcoes)
  }
  
  #
  
  # Dando nome para funcoes
  if( exists("list_funcoes" , envir = .GlobalEnv)){ list_funcoes <- dar_nomes_lista_funcoes(list_funcoes) }
  #
  
  # Cria lista de nomes de agentes j? criados
  
  if( exists("nomes_agentes" , envir = .GlobalEnv) == FALSE ){ base::assign("nomes_agentes",c(name),envir = .GlobalEnv) }else{ # Checa se j? existe lista
    
    if(any(str_detect(name,nomes_agentes) == FALSE)){  # Verifica se nome do agente j? est? presente
      
      base::assign("nomes_agentes",c(name,nomes_agentes),envir = .GlobalEnv) # Se não estiver presente, é adicionado
      
    }
  }
  
  if( exists("list_agents_model" , envir = .GlobalEnv) == FALSE ){ base::assign("list_agents_model", list(name,statevariables,initialvariables,quantidade_agentes),envir = .GlobalEnv) }else{ # Checa se j? existe lista
    
    if( any( unlist(list_agents_model) == name) == FALSE){  # Verifica se nome do agente já está presente
      
      base::assign("list_agents_model",c( list(name,statevariables,initialvariables,quantidade_agentes),list_agents_model),envir = .GlobalEnv) # Se n?o estiver presente, ? adicionado
      
    }else{
      
      pos <- which(list_agents_model == name)[1]
      
      list_agents_model[[pos + 1]] <- statevariables
      list_agents_model[[pos + 2]] <- initialvariables
      list_agents_model[[pos + 3]] <- quantidade_agentes
      
      base::assign("list_agents_model",list_agents_model,envir = .GlobalEnv)
      
    }
  }
  
  
  if( length(statevariables) == length(initialvariables) ){ # Verifica se condi??es iniciais igual ao n?mero de vari?veis de estado das firmas
    
    if( exists("lista_variaveis" , envir = .GlobalEnv) == FALSE ){  # Se não existir a lista de vari?veis do modelo ainda
      
      lista_variaveis <- list(statevariables)  # Vari?veis de estado
      names(lista_variaveis) <- name # Nome do agente h? que pertence as vari?veis de estado
      
      base::assign("lista_variaveis",lista_variaveis,envir = .GlobalEnv) # cria lista com nome do agente sendo criado e com sua lista de vari?veis
      
      
    }else{ # Se j? existir o nome da vari?vel na lista:
      
      if( any( str_detect( names(lista_variaveis),gsub(" ","",paste("\\b",name,"\\b")))) == FALSE ){ #  Se não existir o nome do agente ainda na lista:
        
        names_originais <- names(lista_variaveis) # Nomes j? presentes
        
        lista_variaveis1 <- list(statevariables) # Item novo da lista
        lista_variaveis <- c(lista_variaveis1,lista_variaveis) # Junta listas
        
        names(lista_variaveis) <- c(name,names_originais) # Dou nome para tudo novamente
        
        base::assign("lista_variaveis",lista_variaveis,envir = .GlobalEnv) # Assing para o ambiente global
        
      }else{ # Se for detectado que o nome já existe, e, portanto, estou mudando apenas as variáveis de estado
        
        identificador <- which(names(lista_variaveis) == name) # Qual a posição do agente na lista de variáveis
        
        for(variavel in 1:length( lista_variaveis[[identificador]] ) ){ # Excluo vari?vei existentes no ambiente global
          
          rm( list = gsub(" ","", paste(name,"_",lista_variaveis[[identificador]][variavel] ) ) , envir = .GlobalEnv)
          
        }
        
        lista_variaveis[[identificador]] <- statevariables # Substitui??o das vari?veis de estado
        
        base::assign("lista_variaveis",lista_variaveis,envir = .GlobalEnv) #  Assing para o ambiente global
        
      }
    }
    
    
    # Condições iniciais  -------------------------------------------------------------------------
    
    tinit <- rep(1,length(statevariables)) # Condição inicial base para as funções
    
    for(i in 1:length(statevariables)){ # Para cad uma das funções
      
      name_var_tested <- gsub(" ","",paste(name,"_",statevariables[i])) # Nome das variáveis state
      
      if( str_detect(name_var_tested , "sim") ){ # Criação das state_variables
        
        args <- strsplit(name_var_tested, "_")[[1]]
        
        if( is.na(as.numeric( strsplit(args[1],"")[[1]][ length(strsplit(args[1],"")[[1]]) ])) == FALSE ){
          
          name_var_tested <- gsub(", ","_",toString(args[2:length(args)]))
          
        }
      }
      
      for( y in 1:length(list_funcoes)){ # Ciclo para detectar o maior LAG das variáveis. De cada variável de state,
        # Eu entro em cada uma das funções para procurar por elas
        
        expression <- toString( body( list_funcoes[[y]] ))
        
        if( str_detect( expression , name_var_tested ) ){ # Se a variável testada está dentro da função:
          if(  str_detect(expression , "LAG") ){ # Se há LAG
            
            expressions1 <- str_extract_all( expression ,"LAG\\([^()]+\\)") # Extração dos lags
            
            cycle <- length(expressions1[[1]])
            
            for(z in 1:cycle){ # Ciclo sobre todos os LAGS para verificar se o nome da variável "state" está dentro
              
              if( str_detect( expressions1[[1]][z] , name_var_tested ) ){ # Se está dentro:
                
                lag <- max(as.numeric(str_extract_all( expressions1[[1]][z] ,"LAG\\([^()]+\\)")[[1]] %>%
                                        str_extract_all(",[^()]+\\)") %>%
                                        str_extract_all("[^,\\s\\)]")))
                
                
                if(lag + 1 > tinit[i]){ tinit[i] <- lag + 1} # Condição inicial da matriz da variável state é pode ser maior, ajustando para a função LAG
                
              }
            }
          }
        }
        
        
        if( str_detect(expression,"apply") ){ # Looping para detectar a existÊncia de função auxiliar dentro da função
          
          internal_func <- paste((str_extract_all(expression,"apply\\([^()]+\\)") %>%
                                    str_extract_all(",[^()]+\\)") %>%
                                    str_extract_all("[^,\\s\\)]"))[[1]], collapse='')
          
          internal_func <- (str_extract_all(expression,"apply\\([^()]+\\)") %>%
                              str_extract_all(",[^()]+\\)") %>%
                              str_split(","))[[1]]
          
          internal_func <- internal_func[ length(internal_func)]
          
          internal_func <- (str_extract_all(internal_func,"[^\\s\\FUN=)]"))[[1]]
          
          internal_func <- paste(internal_func,collapse = '')
          
          body_funcao <- toString(body(internal_func)) # Coleta do body da função auxiliar
          
          lista2 <- str_extract_all(body_funcao,"LAG\\([^()]+\\)")[[1]] #%>% # COleta de todos os parâmetros dentro do LAG
          #str_extract_all(",[^()]+\\)") %>%
          #str_extract_all("[^,\\s\\)]")
          
          if( identical(lista2,character(0)) == FALSE ){ # Se não for nulo
            for(j in 1:length(lista2)){ # Para cada um dos lags
              
              if( str_detect( lista2[j] , name_var_tested ) ){ # Se está dentro:
                
                lag <- max(as.numeric(str_extract_all( lista2[j] ,"LAG\\([^()]+\\)")[[1]] %>%
                                        str_extract_all(",[^()]+\\)") %>%
                                        str_extract_all("[^,\\s\\)]")))
                
                
                if(lag + 1 > tinit[i]){ tinit[i] <- lag + 1} # Condição inicial da matriz da variável state é pode ser maior, ajustando para a função LAG
                
              }
            }
          }
        }
      }
      
      # Se houver nome de variávei que é nome de função. Então, entrar dentro dela. Encontrar todos os LAGS.
      # Defini LAG da variável criada como igual ao dos inputs.
      
      if( any(name_var_tested == names(list_funcoes)) ){
        
        position <- which( names(list_funcoes) == name_var_tested)
        
        if( str_detect( toString( body( list_funcoes[[position]] )) , "LAG") ){
          
          lags <- as.numeric( unlist(str_extract_all( toString( body(list_funcoes[[position]] )) ,"LAG\\([^()]+\\)")[[1]] %>%
                                       str_extract_all(",[^()]+\\)") %>%
                                       str_extract_all("[^,\\s\\)]")) )
          
          if( any(is.na(lags))) print(paste("Erro na função",body( list_funcoes[[position]] )))
          
          lags[ is.na(lags)] <- 0
          
          lags <- max(lags)
          
          if( lags + 1 > tinit[i] ){ tinit[i] <- lags + 1}
          
          
        }
      }else{
        
        print(paste("Não há função definida para a variável",statevariables[i],"! Se for criada, agente deverá ser criado novamente!"))
        
      }
    }
    
    
    # Criação das matrizes das variáveis -------------------------------------------------------------------------
    
    for(i in 1:length(statevariables)){
      
      base::assign( gsub(" ","",paste(name,"_",statevariables[i])) , matrix(initialvariables[i], quantidade_agentes, tinit[i]), envir = .GlobalEnv)
      
    }
    
    
  }else{
    stop("ERRO! Nome de variaveis de estado diferente do numero de estados iniciais!!")
  }
}

create_list_objects <- function(name, quantidade_itens, inicialmatrix){
  
  base::assign(name,rep(list(inicialmatrix),quantidade_itens), envir = .GlobalEnv)
  
  if( exists( "list_listas_model") == FALSE ){
    
    base::assign("list_listas_model",list(name,quantidade_itens,inicialmatrix), envir = .GlobalEnv )
    
  }else{
    
    if( any( unlist(list_listas_model) == name) == FALSE){
      
      list_listas_model <- c(list_listas_model,list(name,quantidade_itens,inicialmatrix))
      
      base::assign("list_listas_model",list_listas_model, envir = .GlobalEnv )
      
    }else{
      
      pos <- which(list_listas_model == name)[1]
      
      list_listas_model[[pos+1]] <- quantidade_itens
      list_listas_model[[pos+2]] <- inicialmatrix
      
      base::assign("list_listas_model",list_listas_model, envir = .GlobalEnv )
      
    }
  }
  
}

check_variaveis_salvar <- function(lista_variaveis_salvar){
  
  Flag<-0
  
  for(i in 1:length(lista_variaveis_salvar[[1]])){
    
    if( exists( lista_variaveis_salvar[[1]][i] , envir  = .GlobalEnv ) == FALSE){ print(paste("N?o existe a variavel para salvar",lista_variaveis_salvar[[1]][i]))}
    
  }
  
  if(Flag==0){print("Existem todas as vari?veis em na lista de vari?veis para salvar no ambiente global")}
}

funcao_detectar_maior_lag <- function(list_funcoes){
  
  valorinicial <- 0 # Valor iniciar de lags = 0
  
  for(numlista in 1:length(list_funcoes)){ # teste em todas as fun??es existentes
    
    print(numlista)
    
    funcao <- toString(body(list_funcoes[[numlista]]))# Transforma texto da fun??o em string

    if( str_detect( funcao ,"LAG") == TRUE){ # Detecta se está presente o identificador de lag
      
      lista <- str_extract_all(funcao,"LAG\\([^()]+\\)")[[1]] %>%
        str_extract_all(",[^()]+\\)") %>%
        str_extract_all("[^,\\s\\)]")
      
      for(i in 1:length(lista)){
        
        if( as.numeric(lista[[i]]) > valorinicial) valorinicial <- as.numeric(lista[[i]])
        
      }
      
    }
    
    if( str_detect(funcao,"apply") ){ # Looping para detectar a existÊncia de função auxiliar e coletar o VAR dela
      
      internal_func <- paste((str_extract_all(funcao,"apply\\([^()]+\\)") %>%
                                str_extract_all(",[^()]+\\)") %>%
                                str_extract_all("[^,\\s\\)]"))[[1]], collapse='')
      
      internal_func <- (str_extract_all(funcao,"apply\\([^()]+\\)") %>%
                          str_extract_all(",[^()]+\\)") %>%
                          str_split(","))[[1]]
      
      internal_func <- internal_func[ length(internal_func)]
      
      internal_func <- (str_extract_all(internal_func,"[^\\s\\FUN=)]"))[[1]]
      
      internal_func <- paste(internal_func,collapse = '')
      
      body_funcao <- toString(body(internal_func)) # Coleta do body da função auxiliar
      
      lista2 <- str_extract_all(body_funcao,"LAG\\([^()]+\\)")[[1]] %>%
        str_extract_all(",[^()]+\\)") %>%
        str_extract_all("[^,\\s\\)]")
      
      if( identical(lista2,list()) == FALSE ){
        for(i in 1:length(lista2)){
          
          if( as.numeric(lista2[[i]]) > valorinicial) valorinicial <- as.numeric(lista[[i]])
          
        }
      }
    }
  }
  
  return(valorinicial)
}

# Funções de execução do modelo

funcao_executar_listas <- function(list_funcoes,lista_variaveis){ # Fun??o para executar a lista de fun??es do ABM
  
  for(listas in 1:length(lista_variaveis)){
    for(y in 1:length(lista_variaveis[[listas]])){
      
      name <-  gsub(" ","",paste( names( lista_variaveis )[listas] , "_", lista_variaveis[[listas]][y]  ))
      
      matrix <- eval(parse(text = name))
      
      size <- length( eval(parse(text = name))[1,] )
      
      for(i in 1:(size-1)){
        
        matrix[,size+1-i] <- matrix[,size-i]
        
      }
      
      base::assign( name, matrix, envir = .GlobalEnv)
      
    }
  }
  
  for(numlista in 1:length(list_funcoes)){ # Para cada elemento dentro da lista
    
    if( str_detect( toString(body(list_funcoes[[numlista]])) ,"return") == FALSE){ # Primeiro verifico se foi definido o return da fun??o
      
      print(paste("N?o foi definido return da fun??o",numlista))
      
    }else{
      
      # Identificador de list.args nos argumentos
      
      for(i in 1:length(names(formals(list_funcoes[[numlista]])))){
        
        if(i == 1){ list.args <- list( names(formals(list_funcoes[[numlista]]))[ length(names(formals(list_funcoes[[numlista]]))) - i +1] ) }
        else{  list.args <- c( names(formals(list_funcoes[[numlista]]))[ length(names(formals(list_funcoes[[numlista]]))) - i +1 ],list.args)  }
        
      }
      
      # Usa o nome da lista de argumentos para encontrar os vetores correspondentes
      
      for(i in 1:length(list.args)){
        
        list.args[[i]] <- eval(parse ( text=list.args[[i]] ))
        
      }
      
      # Identifica vari?vel em return #
      
      Frase<-toString(body(list_funcoes[[numlista]]))
      
      Inicio <- str_locate( Frase , 'return')[2] + 2
      
      Opcoes <- str_locate_all( Frase , "\\)")
      
      Final <- 0
      for(i in 1:length(Opcoes[[1]][,2])){
        
        if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
        
      }
      
      Variavel <- str_sub(Frase,Inicio,Final)
      
      TEXTO <- paste( gsub(" ","",paste(Variavel,"[,1]")),"<<- do.call(list_funcoes[[",numlista,"]] , args = list.args)")
      
      print(paste("Executando fun??o",numlista))
      #start <- Sys.time()
      
      eval(parse(text=TEXTO))
      
      #end <- Sys.time()
      #print(end - start)
      
    }
  }
}

# Função mãe


simulationABMFAST2 <- function(periodos_simulacao,list_funcoes,lista_variaveis,lista_variaveis_salvar,DEBBUG){
  
  print("Preparando tudo para começar")
  
  if( class(lista_variaveis_salvar) != "list" ){  stop(print(" 'lista_variaveis_salvar' must be a list!"))  }
  
  gc() # limpeza inicial
  
  ENVIROMENT_FUNCAO_MAE <- where("simulationABMFAST2")
  
  names_var_salvar <- lista_variaveis_salvar
  
  # Todas as funções necessárias para simular
  funcao_detectar_maior_lag <- function(list_funcoes){
    
    valorinicial <- 0 # Valor iniciar de lags = 0
    
    for(numlista in 1:length(list_funcoes)){ # teste em todas as funções existentes
      
      funcao <- toString(body(list_funcoes[[numlista]]))# Transforma texto da função em string
      
      if( str_detect( funcao ,"LAG") == TRUE){ # Detecta se est? presente o identificador de lag
        
        lista <- str_extract_all(funcao,"LAG\\([^()]+\\)")[[1]] %>%
          str_extract_all(",[^()]+\\)") %>%
          str_extract_all("[^,\\s\\)]")
        
        for(i in 1:length(lista)){
          
          if( as.numeric(lista[[i]]) > valorinicial) valorinicial <- as.numeric(lista[[i]])
          
        }
        
      }
    }
    
    return(valorinicial)
  }
  
  func_mod_list_variaveis <- function(lista_variaveis){
    
    # Pega as variáveis que estão separadas em grupos e transforma numa única lista
    
    for(listas in 1:length(lista_variaveis)){
      for(y in 1:length(lista_variaveis[[listas]])){
        
        name <-  gsub(" ","",paste( names( lista_variaveis )[listas] , "_", lista_variaveis[[listas]][y]  ))
        
        if(listas == 1 && y == 1){ lista_variaveis_mod <- list(name)  }else{
          
          lista_variaveis_mod <- c(lista_variaveis_mod,list(name))
          
        }
      }
    }
    
    return(lista_variaveis_mod)
  } # Função para me dar uma lista com todas as variáveis criadas para rodar o modelo
  
  aux_exec_list <- function(list_variaveis_mod){
    
    #PARTE_TEXT <- parse(text = list_variaveis_mod)
    
    matrix <- eval(list_variaveis_mod[[2]][1]) # Executa a variáveis da lista mod. Se existe variável, existe um matriz para ela.
    
    size <- length( matrix[1,] ) # Determina o número de lags pelo tamanho da matriz
    
    #if(size > 1){ # Se o tamanho for maior do que 1, precisa ocorrer a atualização dos lags. Caso contrário, a própria função do modelo fará isso.
    #
    #  matrix[,2:size] <- matrix[,1:(size-1)] # Substitui
    #
    #}else{
    #
    #  matrix <- as.matrix(matrix)
    #
    #}
    
    ifelse(size > 1 , matrix[,2:size] <- matrix[,1:(size-1)] , matrix <- as.matrix.default(matrix))
    
    base::assign(list_variaveis_mod[[1]][1], matrix, envir= ENVIROMENT_FUNCAO_MAE) # base::assign, muda o valor da matriz no ambiente onde está rodando o modelo.
    
  }  # atualizar lag's das variáveis ao longo da simulação. Oq é t passa a ser t-1 e assim por diante
  
  funcao_executar_SEMDEBBUG <- function(X){
    
    # A variável que entra na função executar, X, é
    
    lista_args_internal <- list() # Lista argumentos
    
    for(i in 1:base::length(X[[2]])){ # Puxa os valores dos argumentos que entrarão na função, são as variáveis que causam a função
      
      #lista_args[[i]] <- eval(parse ( text=X[[2]][i] ))
      
      lista_args_internal[[i]] <- base::eval(X[[2]][i])
      
    } # Criado as matrizes de argumentos
    
    
    ############# EXECUTA A FUNÇÃO
    
    base::eval(X[[1]])
    
  }
  
  readUrl <- function(X,lista_args_internal) {
    
    lista_args_internal
    
    out <- tryCatch(
      
      ########################################################
      # Try part: define the expression(s) you want to "try" #
      ########################################################
      
      {
        # Just to highlight:
        # If you want to use more than one R expression in the "try part"
        # then you'll have to use curly brackets.
        # Otherwise, just write the single expression you want to try and
        
        #message("New function now runned")
        eval(X)
      },
      
      ########################################################################
      # Condition handler part: define how you want conditions to be handled #
      ########################################################################
      
      # Handler when a warning occurs:
      warning = function(cond) {
        message(paste("Reading the function caused a warning:", X))
        message("Here's the original warning message:")
        message(cond)
        
        # Choose a return value when such a type of condition occurs
        return(NULL)
      },
      
      # Handler when an error occurs:
      error = function(cond) {
        message(paste("This seems to be an invalid function:",  X))
        message("Here's the original error message:")
        message(cond)
        
        # Choose a return value when such a type of condition occurs
        return(NA)
      },
      
      ###############################################
      # Final part: define what should happen AFTER #
      # everything has been tried and/or handled    #
      ###############################################
      
      finally = {
        # message(paste("Processed function:", X))
        #message("Some message at the end\n")
        return(NA)
      }
    )
    return(out)
  } # Try to catch do DEBBUG
  
  funcao_executar_DEBBUG <- function(X){
    
    print((X[[1]]))

    # A variável que entra na função executar, X, é
    
    lista_args_internal <- list() # Lista argumentos
    
    for(i in 1:length(X[[2]])){ # Puxa os valores dos argumentos que entrarão na função, são as variáveis que causam a função
      
      #lista_args[[i]] <- eval(parse ( text=X[[2]][i] ))
      
      lista_args_internal[[i]] <- eval(X[[2]][i])
      
    } # Criado as matrizes de argumentos
    
    
    ############# EXECUTA A FUNÇÃO
    
    # Debugger
    
    readUrl( X[[1]] , lista_args_internal ) # Primeiro eu rodo a função para testar se há algum erro. Se houver, vai interromper a simulação.
    
    eval( X[[1]] ) # Depois, se o Try to Catch houver funcionado, eu rodo a função normalmente para que os valores possam ir para o ambiente.
    
    # De var até obs, para puxar valor que foi calculado:
    
    var <- sub(" <<-.*","", X[[1]])
    
    var2 <- sub("\\[, 1].*", "", var)
    
    DEBBUG <- parse(text=var2)
    
    obs <- eval(DEBBUG)
    
    # Obs = valor calculado
    
    # Testa no debbuger abaixo:
    if( any( is.nan(obs) | is.na(obs) | is.infinite(obs) )){
      
      if(  any( is.nan(obs)) ){
        
        pos <- which( is.nan(obs) )
        
        stop(print(paste("Problema na função",var2,". Não é número. Posição:",pos)))
        
      }
      
      if(  any( is.na(obs)) ){
        
        pos <- which( is.na(obs) )
        
        stop(print(paste("Problema na função",var2,". Valor ausente. Posição: ",pos)))
        
      }
      
      if(  any( is.infinite(obs) ) ){
        
        pos <- which( is.infinite(obs) )
        
        stop(print(paste("Problema na função",var2,". Valor infinito. Posição: ",pos)))
        
      }
      
      # if( object.size(obs) > 80000 ){
      #
      #   stop(print(paste("Problema na função",var2,". Valor explosivo. Posição: ",pos)))
      #
      # }
    }

    if(ncol(obs) > 1 & exists("list.names.check",where= .GlobalEnv) ){
      
      if( sum2(obs[,2]) > 1 & any(list.names.check == var2) & (t > 0.25*periodos_simulacao ) ){

        if( sqrt((sum2(obs[,1])/sum2(obs[,2]) -1)^2) > 3 ){ stop(print(paste("Problema na função",var2,". Comportamento explosivo."))) }

      }
      
      if( sum2(obs[,2]) < 1 & any(list.names.check == var2) & (t > 0.25*periodos_simulacao ) ){
        
        if( sqrt((sum2(obs[,1]) - sum2(obs[,2]))^2) > 0.3 ){ stop(print(paste("Problema na função",var2,". Comportamento explosivo."))) }
        
      }
    }

  }
  
  # Executar listas

  funcao_executar_listas_DEBBUG <- function(lista_execucao,lista_variaveis,aux_exec_list,funcao_executar_DEBBUG){ # Fun??o para executar a lista de fun??es do ABM
    
    base::lapply(list_variaveis_mod, aux_exec_list)
    
    base::lapply(lista_final_exec, funcao_executar_DEBBUG)
    
  }
  
  funcao_executar_listas_SEMDEBBUG <- function(lista_execucao,lista_variaveis,aux_exec_list,funcao_executar_SEMDEBBUG){ # Fun??o para executar a lista de fun??es do ABM
    
    base::lapply(list_variaveis_mod, aux_exec_list)
    
    base::lapply(lista_final_exec, funcao_executar_SEMDEBBUG)
    
  }
  
  dar_nomes_lista_funcoes <- function(list_funcoes){ # Criar lista de funções, dando nome para elas, simplesmente procurando pelo return.
    
    for(numlista in 1:length(list_funcoes)){
      
      Frase <- base::toString(body(list_funcoes[[numlista]]))
      
      Inicio <- stringr::str_locate( Frase , 'return')[2] + 2
      
      Opcoes <- stringr::str_locate_all( Frase , "\\)")
      
      Final <- 0
      for(i in 1:length(Opcoes[[1]][,2])){
        
        if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
        
      }
      
      Variavel <- str_sub(Frase,Inicio,Final)
      
      if(numlista == 1){ nomes <- c(Variavel)  }else{
        nomes <- c(nomes,Variavel)
      }
      
      
    }
    names(list_funcoes) <- nomes
    
    return(list_funcoes)
  }
  
  # Nome e lags
  
  list_funcoes <- dar_nomes_lista_funcoes(list_funcoes)
  
  tinit <- funcao_detectar_maior_lag(list_funcoes) + 1
  
  print(paste("Simulação iniciada no período",tinit,"pela existência de lags"))
  
  ### REINICIO DAS CONDIÇÕES INICIAIS DAS VARIÁVEIS
  
  resettinglistsagents()
  
  # Criaçao de lista de variáveis para salvar
  
  for(i in 1:length(lista_variaveis_salvar)){
    
    nome <- lista_variaveis_salvar[[i]]
    
    print(nome)
    
    lista_variaveis_salvar[[i]] <- matrix( rep( eval(parse(text=nome))[,1] ,periodos_simulacao) ,
                                           length( eval(parse(text=nome))[,1] ) , periodos_simulacao )
    
    names(lista_variaveis_salvar)[i] <- nome
    
  }
  
  # Preparação da lista de execução #
  for(numlista in 1:length(list_funcoes)){ # Para cada elemento dentro da lista
    
    if( str_detect( toString(body(list_funcoes[[numlista]])) ,"return") == FALSE){ # Primeiro verifico se foi definido o return da função
      
      print(paste("Não foi definido return da função",numlista)) # Se não houver o return, printar aviso
      
    }else{
      
      # Identificador de list.args nos argumentos das função
      
      list.args <- names(formals(list_funcoes[[numlista]])) # Nome das variáveis independentes da função.
      
      for(i in 1:length(names(formals(list_funcoes[[numlista]])))){ # Transforma list.args em uma lista
        
        if(i == 1){ list.args <- list( names(formals(list_funcoes[[numlista]]))[ length(names(formals(list_funcoes[[numlista]]))) - i +1] ) }
        else{  list.args <- c( names(formals(list_funcoes[[numlista]]))[ length(names(formals(list_funcoes[[numlista]]))) - i +1 ],list.args)  }
        
      }
      
      
      if( numlista == 1){ # Coloca a lista de variáveis independentes dentro de outra lista geral #
        
        lista_args <- list(list.args)
        
      }else{
        
        lista_args <- c(lista_args,list(list.args))
        
      }
      
      for(i in 1:length( lista_args[[1]] )){ # Para no caso de um erro, em que não foi criada a variáveis lista args.#
        
        if( exists(lista_args[[1]][[i]]) == FALSE) stop(print(paste("O argumento",lista_args[[1]][[i]],"não existe.")))
        
      }
      
      # Identifica variável em return #
      
      Frase<-toString(body(list_funcoes[[numlista]]))
      
      Inicio <- stringr::str_locate( Frase , 'return')[2] + 2
      
      Opcoes <- stringr::str_locate_all( Frase , "\\)")
      
      Final <- 0
      for(i in 1:length(Opcoes[[1]][,2])){
        
        if( Opcoes[[1]][i,2] > Inicio ){ Final <- Opcoes[[1]][i,2] - 1 ; break }
        
      }
      
      Variavel <- str_sub(Frase,Inicio,Final)
      # Identificada a variável dentro de return
      
      # Criação do texto para executar a função
      TEXTO <- base::paste( gsub(" ","",paste(Variavel,"[,1]")),"<<- base::do.call(list_funcoes[[",numlista,"]] , args = lista_args_internal )")
      
      TEXTO <- base::parse(text = TEXTO)
      
      #TEXTO <- paste( gsub(" ","",paste(Variavel,"[,1]")),  "<<- fastDoCall(list_funcoes[[",numlista,"]],args = lista_args,envir = ENVIROMENT_FUNCAO_MAE)")
      
      if(numlista == 1){ lista_execucao <- list(TEXTO)}else{
        
        lista_execucao <- c(lista_execucao,list(TEXTO))
        
      }
    }
  }
  
  # Lista que adiciona dentro de seus elementos o texto da função + o conjunto dos argumentos que devem entrar como variáveis independentes.
  for(i in 1:length(lista_execucao)){
    
    if(i == 1){
      
      lista_final_exec <- list(list( unlist( lista_execucao[[i]] ) , unlist(list(lista_args[[i]]) )))
      
    }else{
      
      lista_final_exec <- c(lista_final_exec, list(list( unlist( lista_execucao[[i]] ) , unlist(list(lista_args[[i]]) ))) )
      
    }
  }
  
  
  list_variaveis_mod <- func_mod_list_variaveis(lista_variaveis)
  
  # Deixar tudo na forma de expression dentro de lista_final_exec
  
  # Inputs das funções
  
  for(tam in 1:length(lista_final_exec)){
    for(tam2 in 1:length(lista_final_exec[[tam]][[2]])){
      
      lista_final_exec[[tam]][[2]][tam2] <- base::parse( text = lista_final_exec[[tam]][[2]][tam2] )
      
    }
  }
  
  # Lista de variáveis
  
  for(tam3 in 1:length(list_variaveis_mod)){
    
    list_variaveis_mod[[tam3]][[2]] <- list_variaveis_mod[[tam3]][1]
    
    list_variaveis_mod[[tam3]][2] <- list( parse(text = list_variaveis_mod[[tam3]][[1]]) )
    
  }
  
  
  ##### Simulação ####
  
  if(DEBBUG == 0){
    
    # Looping períodos do tempo
    for(t in tinit:periodos_simulacao){
      
      print(paste("Executando simulação | ",round(100*t/periodos_simulacao,2),"%"))
      
      funcao_executar_listas_SEMDEBBUG(lista_execucao,lista_variaveis,aux_exec_list,funcao_executar_SEMDEBBUG)
      
      # Salvando variáveis desejadas =
      for(i in 1:length(lista_variaveis_salvar)){
        
        lista_variaveis_salvar[[i]][,t] <- base::eval(base::parse(text = names(lista_variaveis_salvar)[i] ) , envir = environment() )[,1]
        
      }
    }
    
  }else{
    
    # Looping períodos do tempo
    
    
    for(t in tinit:periodos_simulacao){
      
      
      
      print(paste("Executando simulação | ",base::round(100*t/periodos_simulacao,2),"%"))
      
      if( as.numeric(mem_used()) > 16267 * 1048576 * 1/2 ){ stop(print("Risco de falta de memória. Provável instalibilidade do modelo. Simulação encerrada."))}
      
      TIME_INIT <- Sys.time()
      
        funcao_executar_listas_DEBBUG(lista_execucao,lista_variaveis,aux_exec_list,funcao_executar_DEBBUG)
      
      TIME_END <- Sys.time()
      
      if( TIME_END - TIME_INIT > 2) stop(print("Error: time expired"))
      
      if(t == periodos_simulacao & DEBBUG == 2){ # No final do ciclo, se chegar até lá, calculo do tempo de execucação das funções na função normal, sem DEBBUG
        
        data.time <- data.frame()
        
        for(i in 1:length(lista_final_exec)){
          
          print(paste("######################################### |"))
          print(paste("Computing time execution of each function |", base::round(i/length(lista_final_exec)*100,2) ,"%"))
          
          if(i == 1){
            data.time <- summary(microbenchmark(times = 10, unit = "ms", # milliseconds
                                                lista_final_exec[[i]][[1]]))
          }else{
            data.time[i,] <- summary(microbenchmark(times = 10, unit = "ms", # milliseconds
                                                    funcao_executar_SEMDEBBUG(lista_final_exec[[i]])))
          }
          
        }
        
        levels(data.time$expr) <- "Function"
        
        row.names(data.time) <- (names(list_funcoes))
        
        print(data.time)
        
        base::assign("data.time.function",data.time,envir=where("simulationABMFAST2"))
        
        print("Find the time of execution of firms in the object 'data.time.function'.")
        
      }
      
      # Salvando variáveis desejadas =
      for(i in 1:length(lista_variaveis_salvar)){
        
        lista_variaveis_salvar[[i]][,t] <- base::eval(base::parse(text = names(lista_variaveis_salvar)[i] ) , envir = environment() )[,1]
        
      }
      
      # if( t > tinit*3 + 30){
      #   
      #   for( i in 1:round(length(list.names.check))){
      # 
      #     Position <- which( list.names.check[[i]] ==  names_var_salvar )
      #     
      #     Var <- lista_variaveis_salvar[[Position]]
      #     
      #     Var <- Var[,(t-30):(t)]
      # 
      #     if( is.null(nrow(Var)) == FALSE ){
      #       
      #       Var <- apply(Var, MARGIN=2, mean)
      #       
      #     }
      #     
      #     if( sd( Var[3:30] ) > 2 * sd( Var[1:28]) ){
      #       
      #       print(paste("Variável se tornou explosiva :",names_var_salvar[i] ))
      #       stop()
      #       
      #     }
      #   
      #   }
      # }
      
      
      # if( t > round(periodos_simulacao/3)){
      #   
      #   if(t == 1 + round(periodos_simulacao/3)){
      #     
      #     print("Baseline stability")
      #     
      #     grau.integer.variables <- rep(3,length(lista_variaveis_salvar))
      #     
      #     for( i in 1:round(length(lista_variaveis_salvar))){
      #       
      #       Var <- lista_variaveis_salvar[[i]]
      #       
      #       Var <- Var[,(t-15):(t)]
      #       
      #       if( is.null(nrow(Var)) == FALSE ){
      #         
      #         Var <- apply(Var, MARGIN=2, mean)
      #         
      #       }
      #       
      #       if( any( Var[2:length(Var)] == Var[1]) == FALSE ){
      #         
      #         grau.integer.variables[i] <- 0
      #         
      #         test <- summary(urca::ur.df(Var))
      #         
      #         if( is.nan(test@teststat[1]) == FALSE ){
      #           
      #           if( sqrt((test@cval[3])^2) > sqrt( test@teststat[3]^2 ) ){
      #             
      #             grau.integer.variables[i] <- 1
      #             
      #           }
      #         }
      #       }
      #     }
      #   }else{
      #     
      #     for( i in 1:round(length(lista_variaveis_salvar))){
      #       
      #       print("Stability analylis")
      #       print(i)
      #       
      #       Var <- lista_variaveis_salvar[[i]]
      #       
      #       Var <- Var[,(t-15):(t)]
      #       
      #       if( is.null(nrow(Var)) == FALSE ){
      #         
      #         Var <- apply(Var, MARGIN=2, mean)
      #         
      #       }
      #       
      #       if(grau.integer.variables[i] == 1){
      #         
      #         Var <- diff(Var)
      #         
      #       }
      #       
      #       
      #       if( any( Var[2:length(Var)] == Var[1]) == FALSE & grau.integer.variables[i] != 3 ){
      #         
      #         test <- summary(urca::ur.df(Var))
      #         
      #         if( is.nan(test@teststat[1]) == FALSE ){
      #           
      #           if( sqrt((test@cval[3])^2) > sqrt( test@teststat[1]^2 ) ){
      #             
      #             print(paste("Variável se tornou explosiva :",names_var_salvar[i] ))
      #             stop()
      #             
      #           }
      #           
      #         }
      #       }
      #     }
      #     
      #   }
      # }
      
      
    }
  }
  
  gc() # limpeza final
  
  print("Simulação finalizada com sucesso!!! =) ")
  
  return(lista_variaveis_salvar)
}

# Função para executar a função mão de forma segura
# Para compreender o possibly
# https://blog.curso-r.com/posts/2017-04-09-try
simulationABMSAFE <- possibly(simulationABMFAST2, otherwise = -(10)^10)

onesimulation <- function(i){
  
  print(par.pas.dem)
  
  #DEBBUG <- 0
  
  ambiente <- gsub(" ","",paste("sim",i))
  
  base::assign( ambiente , new.env())
  
  #list_funcoes <- list_funcoes
  #lista_variaveis <- lista_variaveis
  #lista_variaveis_salvar <- lista_variaveis_salvar
  resettinglistsagents()
  
  base::assign("list_funcoes",list_funcoes , envir= eval(parse(text=ambiente)) )
  base::assign("lista_variaveis",lista_variaveis , envir = eval(parse(text=ambiente)))
  base::assign("lista_variaveis_salvar",lista_variaveis_salvar , envir = eval(parse(text=ambiente)))
  base::assign("periodos_simulacao",periodos_simulacao , envir = eval(parse(text=ambiente)))
  base::assign("simulationABMFAST2",simulationABMFAST2, envir = eval(parse(text=ambiente)))
  
  set.seed(i)
  
  TEXT <- gsub(" ","",paste(ambiente,"$simulationABMFAST2(periodos_simulacao,list_funcoes,lista_variaveis,lista_variaveis_salvar,DEBBUG)"))
  
  try.to.catch.error <- function(TEXT) {
    out <- tryCatch(
      {
        # Just to highlight: if you want to use more than one
        # R expression in the "try" part then you'll have to
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression
        # in case the "try" part was completed successfully
        
        #message("This is the 'try' part")
        
        results <- eval( parse( text = toString(TEXT) ))
        # The return value of `readLines()` is the actual value
        # that will be returned in case there is no condition
        # (e.g. warning or error).
        # You don't need to state the return value via `return()` as code
        # in the "try" part is not wrapped inside a function (unlike that
        # for the condition handlers for warnings and error below)
      },
      error=function(cond) {
        message(paste("Error in simulation:", i))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(1)
      },
      warning=function(cond) {
        message(paste("Warning in simulation:", i))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(1)
      },
      finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
        
        
        #message(paste("Processed URL:", TEXT))
        #message("Some other message at the end")
      }
    )
    return(out)
  }
  
  results <- try.to.catch.error(TEXT)
  
  #results <- eval( parse( text = toString(TEXT) ))
  
  gc() # limpa o lixo para liberar espaço
  
  rm(  list_funcoes , envir = eval(parse(text=ambiente)) )
  rm(  lista_variaveis , envir = eval(parse(text=ambiente)) )
  rm(  lista_variaveis_salvar , envir = eval(parse(text=ambiente)) )
  rm(  periodos_simulacao , envir = eval(parse(text=ambiente)) )
  rm(  simulationABMFAST2 , envir = eval(parse(text=ambiente)) )
  
  return(results)
}

simulationMULTABM <- function(m){
  
  gc() # limpa o lixo para liberar espaço
  
  onesimulation <- function(i){
    
    DEBBUG <- 0
    
    ambiente <- gsub(" ","",paste("sim",i))
    
    base::assign( ambiente , new.env())
    
    #list_funcoes <- list_funcoes
    #lista_variaveis <- lista_variaveis
    #lista_variaveis_salvar <- lista_variaveis_salvar
    resettinglistsagents()
    
    base::assign("list_funcoes",list_funcoes , envir= eval(parse(text=ambiente)) )
    base::assign("lista_variaveis",lista_variaveis , envir = eval(parse(text=ambiente)))
    base::assign("lista_variaveis_salvar",lista_variaveis_salvar , envir = eval(parse(text=ambiente)))
    base::assign("periodos_simulacao",periodos_simulacao , envir = eval(parse(text=ambiente)))
    base::assign("simulationABMFAST2",simulationABMFAST2, envir = eval(parse(text=ambiente)))
    
    set.seed(i)
    
    TEXT <- gsub(" ","",paste(ambiente,"$simulationABMFAST2(periodos_simulacao,list_funcoes,lista_variaveis,lista_variaveis_salvar,DEBBUG)"))
    
    try.to.catch.error <- function(TEXT) {
      out <- tryCatch(
        {
          # Just to highlight: if you want to use more than one
          # R expression in the "try" part then you'll have to
          # use curly brackets.
          # 'tryCatch()' will return the last evaluated expression
          # in case the "try" part was completed successfully
          
          #message("This is the 'try' part")
          
          results <- eval( parse( text = toString(TEXT) ))
          # The return value of `readLines()` is the actual value
          # that will be returned in case there is no condition
          # (e.g. warning or error).
          # You don't need to state the return value via `return()` as code
          # in the "try" part is not wrapped inside a function (unlike that
          # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
          message(paste("Error in simulation:", i))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(1)
        },
        warning=function(cond) {
          message(paste("Warning in simulation:", i))
          message("Here's the original warning message:")
          message(cond)
          # Choose a return value in case of warning
          return(1)
        },
        finally={
          # NOTE:
          # Here goes everything that should be executed at the end,
          # regardless of success or error.
          # If you want more than one expression to be executed, then you
          # need to wrap them in curly brackets ({...}); otherwise you could
          # just have written 'finally=<expression>'
          
          
          #message(paste("Processed URL:", TEXT))
          #message("Some other message at the end")
        }
      )
      return(out)
    }
    
    results <- try.to.catch.error(TEXT)
    
    #results <- eval( parse( text = toString(TEXT) ))
    
    gc() # limpa o lixo para liberar espaço
    
    rm(  list_funcoes , envir = eval(parse(text=ambiente)) )
    rm(  lista_variaveis , envir = eval(parse(text=ambiente)) )
    rm(  lista_variaveis_salvar , envir = eval(parse(text=ambiente)) )
    rm(  periodos_simulacao , envir = eval(parse(text=ambiente)) )
    rm(  simulationABMFAST2 , envir = eval(parse(text=ambiente)) )
    
    return(results)
  }
  
  print("Starting the simmulation. Wait")
  
  print("All the objects from the global enviroment will be loaded to the CPUs.
         It is advisable that the global environment contains only the objects necessary for the simulation.")
  
  print("Computing estimative for total time.")
  
  Start <- Sys.time()
    invisible(capture.output(onesimulation(1)))
  End <- Sys.time()
  
  n.cores <- parallel::detectCores() - 1
  
  print("Simulation time:")
  print(End-Start)
  print("Simulation total time estimative:")
  print(round( (floor(m/(n.cores)) + 1 + n.cores)*(End-Start),0))
  if( round( (floor(m/(n.cores)) + 1 + n.cores)*(End-Start),0) > 10)  print("Coffee time!")
  
  invisible(capture.output(resettinglistsagents()))
  
  print("Conecting CPUs")
  n.cores <- parallel::detectCores() - 1
  if(m < n.cores) n.cores <- m
  clust <- makeCluster(n.cores)
  
  print("Loading packages to the CPUs")
  clusterEvalQ(clust, library(stringr))
  clusterEvalQ(clust, library(pryr))
  clusterEvalQ(clust, library(matrixStats))
  clusterEvalQ(clust, library(Rlab))
  clusterEvalQ(clust, library(DescTools))
  
  print("Loading objects from the global enviroment to the CPUs:")
  
  list <- c(ls(globalenv()))
  
  print(paste("Last object to be loaded:",list[[length(list)]]))
  
  for(i in 1:length(list)){
    
    #print(i)
    print(list[[i]])
    clusterExport(clust, list[[i]] )
    
  }
  
  #clusterExport(clust, c(ls(globalenv())) )
  
  #object.size(c(ls(globalenv())))
  
  #export_variables_list <- list()
  
  #for(i in 1:length(lista_variaveis)){
  #  for(y in 1:length(lista_variaveis[[i]])){
  #
  #    export_variables_list <- c(gsub(" ","",paste(names(lista_variaveis[i]),"_",lista_variaveis[[i]][[y]])),export_variables_list)
  #
  #
  #  }
  #}
  
  #export <- c("list_funcoes","lista_variaveis","lista_variaveis_salvar","periodos_simulacao","DEBBUG",lsf.str(),export_variables_list)
  
  Start1 <- Sys.time()
  
  print(paste("Start of the simulation batch in:",Start1))
  print(paste("Closing forecast:",Start1 + round( (floor(m/(n.cores)) + 1 + n.cores)*(End-Start),0)))
  
  # list_results_simulations <- parLapply(clust, 1:m, onesimulation)
  
  try.to.catch.error <- function() {
    out <- tryCatch(
      {
        # Just to highlight: if you want to use more than one
        # R expression in the "try" part then you'll have to
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression
        # in case the "try" part was completed successfully
        
        #message("This is the 'try' part")
        
        list_results_simulations <- parLapply(clust, 1:m, onesimulation)
        
        # The return value of `readLines()` is the actual value
        # that will be returned in case there is no condition
        # (e.g. warning or error).
        # You don't need to state the return value via `return()` as code
        # in the "try" part is not wrapped inside a function (unlike that
        # for the condition handlers for warnings and error below)
      },
      error=function(cond) {
        message(paste("There was an error in one of the simulations! Model has problems."))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(1)
      },
      warning=function(cond) {
        message(paste("There was an warning in one of the simulations! Model has problems."))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(1)
      },
      finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
        
        
        #message(paste("Processed URL:", TEXT))
        #message("Some other message at the end")
      }
    )
    return(out)
  }
  
  results <- try.to.catch.error()
  
  stopCluster(clust)
  
  onesimulationSAFE <- possibly(onesimulation, otherwise = -1000000000000000000000000000000000000000000)
  
  if(class(results) == "numeric"){
    
    print("Error search:")
    
    error.position <- 0
    i <- 1
    
    DEBBUG <- 0
    
    while( error.position == 0 ){
      
      if( i > m) break
       
      print(paste("SEED BEING TESTED:",i))
      
      invisible(capture.output( X <-  onesimulationSAFE(i) ))
      
      if( class(X) == "numeric"  ){ error.position <- i }
      
      i <- i + 1
      
    }
    
    
    print(paste("ERROR WITH SEED",error.position))
    
    results <- paste("Seed to generate error:",error.position)
    
  }
  
  
  End <- Sys.time()
  
  print(End - Start)
  print("End of the simulation!")
  
  return(results)
  
}

### Gráficos

graph.function <- function(results,Nomes,Titulo,start,LOG){
  
  m <- length(results)
  TIME <- length(results[[1]][[1]][1,])
  
  if(start >= TIME){stop(print("Start value higher than end period"))}
  
  
  for(i in 1:length(Nomes)){
    
    y <- length(eval(parse(text=gsub(" ","",paste("results[[1]]$",Nomes[i]))))[,1])
    
    if(y > 1){
      
      Matrix.Mean <- matrix(0,m,TIME)
      Matrix.Sd <- matrix(0,m,TIME)
      
      for(Simul in 1:m){
        for(time in 1:TIME){
          
          if(LOG[i] == 0){
            
            Matrix.Mean[Simul,time] <- mean((eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            Matrix.Sd[Simul,time] <- sd((eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            
            
          }else{
            
            Matrix.Mean[Simul,time] <- mean(log(eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            Matrix.Sd[Simul,time] <- sd(log(eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            
          }
        }
      }
      
      one <- Matrix.Mean[1,start:TIME]
      mean <- apply(Matrix.Mean,MARGIN=2,mean)[start:TIME]
      sd <- apply(Matrix.Mean,MARGIN=2,sd)[start:TIME]
      df_graph <- data.frame(mean,sd,start:TIME,one)
      names(df_graph) <- c("mean","sd","time","one")
      
      g <- ggplot(data=df_graph)+
        geom_line(aes(time,one),col='blue',size=0.3)+
        scale_linetype_manual(values=c("twodash")) +
        geom_line(aes(time,mean+sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean - sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean),size=1)+
        theme_bw() +
        labs( y = Titulo[i] , x = "Time")
      theme(text = element_text(size=14))
      
      print(g)
      
      # Salvando o gráfico
      
      filename <- gsub(" ","",paste(Nomes[i],".png"))
      
      ggsave(g, filename = filename, dpi = 300, type = 'cairo',
             width = 16, height = 16*0.6, units = 'cm')
      
      
    }else{
      
      Matrix.Mean <- matrix(0,m,TIME)
      
      for(Simul in 1:m){
        for(time in 1:TIME){
          
          Matrix.Mean[Simul,time] <- eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]
          
        }
      }
      
      
      
      if(LOG[i] == 0){
        
        one <- Matrix.Mean[1,start:TIME]
        mean <- apply(Matrix.Mean,MARGIN=2,mean)[start:TIME]
        sd <- apply(Matrix.Mean,MARGIN=2,sd)[start:TIME]
        
      }else{
        
        one <- log(Matrix.Mean[1,start:TIME])
        mean <- apply(log(Matrix.Mean),MARGIN=2,mean)[start:TIME]
        sd <- apply(log(Matrix.Mean),MARGIN=2,sd)[start:TIME]
        
      }
      
      df_graph <- data.frame(mean,sd,start:TIME,one)
      names(df_graph) <- c("mean","sd","time","one")
      
      g <- ggplot(data=df_graph)+
        geom_line(aes(time,one),col='blue',size=0.3)+
        geom_line(aes(time,mean+sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean - sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean),size=1)+
        theme_bw() +
        labs( y = Titulo[i] , x = "Time")
      theme(text = element_text(size=14))
      
      print(g)
      
      # Salvando o gráfico
      
      filename <- gsub(" ","",paste(Nomes[i],".png"))
      
      ggsave(g, filename = filename, dpi = 300, type = 'cairo',
             width = 16, height = 16*0.6, units = 'cm')
      
      
    }
  }
}


corr.function3.bk <- function(results,GDP,Nomes,
                              Titulo,start,periodos,LAGS,frequency){
  
  lowP      <- 6      # bandpass filter minimum period
  highP     <- 32     # bandpass filter maximum period
  bpfK      <- 12     # bandpass filter order
  lags      <- 12      # lags to analyze
  bPlotCoef <- 1.5    # boxplot whiskers extension from the box (0=extremes)
  bPlotNotc <- FALSE  # use boxplot notches
  smoothing <- 1e5    # HP filter smoothing factor (lambda)
  
  ccf1_INTERN <- function(x,y,type,lags.max){
    
    X <- cbind(x,y)
    
    colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    
    acf.out <- invisible(acf(X,lag.max=lags.max,type="correlation"))
    
    lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
    y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
    acf.out$acf <- array(y, dim = c(length(y), 1L, 1L))
    acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    
    return(acf.out)
  }
  
  if( any( class(results[[1]]) == "matrix" ) ){
    
    m <- 1
    TIME <- length(results[[1]][1,])
    
  }else{
    
    
    m <- length(results)
    TIME <- length(results[[1]][[1]][1,])
    
  }
  
  
  Table <- matrix(0,length(Nomes),2*periodos + 1)
  Table.Corr.DP <- matrix(0,length(Nomes),2*periodos + 1)
  Table.DP <- matrix(0,length(Nomes), 1)
  
  col.names <- rep(0,2*periodos + 1)
  
  for(i in 1:(2*periodos + 1)){
    
    if(periodos + 1 - i > -1){
      
      col.names[2*periodos + 2 - i] <- paste("+",periodos + 1 - i)
      
    }else{
      
      col.names[2*periodos + 2 - i] <- paste(periodos + 1 - i)
      
    }
    
  }
  
  colnames(Table) <- col.names
  
  rownames(Table) <- rep(Titulo)
  
  Matrices <- list()
  Matrices.DP <- list()
  Matrices.DP.Cor <- list()
  
  for(i in 1:length(Nomes)){
    
    Matrices[[i]]  <- matrix(0,m,periodos*2 + 1)
    
  }
  
  for(i in 1:length(Nomes)){
    
    Matrices.DP[[i]]  <- matrix(0,m,1)
    
  }
  
  
  for(i in 1:m ){ # Para cada resultado
    
    for(name in 1:length(Nomes)){ # Para cada variável
      
      print(i)
      print(Nomes[name])
      
      if(m  == 1){
        
        x <- eval(parse( text = gsub(" ","",paste( "results$",GDP) )))[start:TIME]
        
        y <- eval(parse( text = gsub(" ","",paste( "results$",Nomes[name]) )))[start:TIME]
        
      }else{
        
        x <- eval(parse( text = gsub(" ","",paste( "results[[",i,"]]$",GDP) )))[start:TIME]
        
        y <- eval(parse( text = gsub(" ","",paste( "results[[",i,"]]$",Nomes[name]) )))[start:TIME]
        
      }
      
      if(LAGS[1] == 1){
        
        if( any(x <0) ){
          
          print("Valores negativos para tirar o log de variável principal")
          
        }else{
          
          x <- log(x)
          
        }
      }
      
      if(LAGS[name] == 1){
        
        if(  any(y <0) ){
          
          print(paste("Valores negativos para tirar o log em",Nomes[name])) ; break }else{  y <- log(y) }
        
      }
      
      x <- bkfilter( ts(((x))) , pl = lowP, pu = highP , nfix= lags  )
      y <- bkfilter( ts(((y))) , pl = lowP, pu = highP , nfix= lags )
      
      x <- x$cycle[ is.na(x$cycle) == FALSE]
      y <- y$cycle[ is.na(y$cycle) == FALSE]
      
      result.ccf <- ccf1_INTERN(x,y,lags.max = periodos)
      
      Matrices[[name]][i,] <- t( as.matrix(result.ccf$acf))
      
      Matrices.DP[[name]][i,1] <- sd(y)
      
    }
  }
  
  for(i in 1:length(Nomes)){
    
    Table[i,] <-  colSums( Matrices[[i]] )/m
    Table.Corr.DP[i,] <- apply(Matrices[[i]],MARGIN=2,sd)
    Table.DP[i,1] <- colSums( Matrices.DP[[i]])/m
    
  }
  
  return <- list(round(Table,5),round(Table.DP,5),round(Table.Corr.DP,5))
  
  names(return) <- c("Table.Corr","Table.Dp","Table.SD.Corr")
  
  return(return)
  
}

# Para calibragem

simulationMULTABM_SIMPLIFICADO <- function(m){
  
  gc() # limpa o lixo para liberar espaço
  
  print("Starting the simmulation. Wait")
  
  print("Conecting CPUs")
  n.cores <- parallel::detectCores() - 1
  if(m < n.cores) n.cores <- m
  clust <- makeCluster(n.cores)
  
  print("Loading packages to the CPUs")
  clusterEvalQ(clust, library(stringr))
  clusterEvalQ(clust, library(pryr))
  clusterEvalQ(clust, library(matrixStats))
  clusterEvalQ(clust, library(Rlab))
  clusterEvalQ(clust, library(DescTools))
  
  print("Loading objects from the global enviroment to the CPUs:")
  
  list <- c(ls(globalenv()))
  
  #print(paste("Last object to be loaded:",list[[length(list)]]))
  
  for(i in 1:length(list)){
    
    #print(i)
    #print(list[[i]])
    clusterExport(clust, list[[i]] )
    
  }
  
  #clusterExport(clust, c(ls(globalenv())) )
  
  #object.size(c(ls(globalenv())))
  
  #export_variables_list <- list()
  
  #for(i in 1:length(lista_variaveis)){
  #  for(y in 1:length(lista_variaveis[[i]])){
  #
  #    export_variables_list <- c(gsub(" ","",paste(names(lista_variaveis[i]),"_",lista_variaveis[[i]][[y]])),export_variables_list)
  #
  #
  #  }
  #}
  
  #export <- c("list_funcoes","lista_variaveis","lista_variaveis_salvar","periodos_simulacao","DEBBUG",lsf.str(),export_variables_list)
  
  # list_results_simulations <- parLapply(clust, 1:m, onesimulation)
  
  print("Start of the simulation!")
  
  results <- parLapply(clust, 1:m, onesimulation)
  
  stopCluster(clust)
  
  print("End of the simulation!")
  
  gc() # limpa o lixo para liberar espaço
  
  return(results)
  
}

onesimulation <- function(i){
  
  set.seed(i)
  
  resettinglistsagents()
  
  DEBBUG <- 1
  
  results <- simulationABMFAST2(periodos_simulacao,list_funcoes,lista_variaveis,lista_variaveis_salvar,DEBBUG)
  
  gc() # limpa o lixo para liberar espaço
  
  return(results)
}

simulABMGA <- function(X){
  
  gc() # Limpeza
  
  Original_values <- rep(0,length(X))
  
  for(i in 1:length(X)){
    
    Original_values[i] <- eval(parse(text = names_parameters[i]))
    
    #print(Original_values[i])
    
    assign(names_parameters[i],round(X[i],4),envir=globalenv())
    
    print(paste(names_parameters[i],round(X[i],4)))
    
  }
  
  
  ##### Funcções e parâmetros
  
  
  # lowP      <- 6      # bandpass filter minimum period
  # highP     <- 32     # bandpass filter maximum period
  # bpfK      <- 12     # bandpass filter order
  # lags      <- 4      # lags to analyze
  # bPlotCoef <- 1.5    # boxplot whiskers extension from the box (0=extremes)
  # bPlotNotc <- FALSE  # use boxplot notches
  # smoothing <- 1e5    # HP filter smoothing factor (lambda)
  
  
  # onesimulation <- function(i){
  #   
  #   DEBBUG <- 0
  #   
  #   ambiente <- gsub(" ","",paste("sim",i))
  #   
  #   assign( ambiente , new.env())
  #   
  #   #list_funcoes <- list_funcoes
  #   #lista_variaveis <- lista_variaveis
  #   #lista_variaveis_salvar <- lista_variaveis_salvar
  #   resettinglistsagents()
  #   
  #   for(n in ls(.GlobalEnv, all.names=TRUE)) assign(n, get(n, .GlobalEnv), envir=eval(parse(text=ambiente)))
  #   
  #   TEXT <- gsub(" ","",paste(ambiente,"$simulationABMFAST2(periodos_simulacao,list_funcoes,lista_variaveis,lista_variaveis_salvar,DEBBUG)"))
  # 
  #   results <- eval( parse( text = toString(TEXT) ))
  # 
  #   #results <- eval( parse( text = toString(TEXT) ))
  #   
  #   gc() # limpa o lixo para liberar espaço
  # 
  #   list = ambiente[grep("^sim", ambiente)]
  #   
  #   remove( list = list  )
  #   
  #   return(results)
  # }
  
  onesimulation <- function(i){
    
    set.seed(i)
    
    resettinglistsagents()
    
    DEBBUG <- 0
    
    results <- simulationABMFAST2(periodos_simulacao,list_funcoes,lista_variaveis,lista_variaveis_salvar,DEBBUG)
    
    gc() # limpa o lixo para liberar espaço
    
    return(results)
  }
  
  multsimulation_SAFE <- possibly(simulationMULTABM_SIMPLIFICADO, otherwise = -(10^10^2))
  
  results <- multsimulation_SAFE(5)
  
  Check <- 0
  
  for(i in 1:length(results)){
    
    if( length(results[[i]]) == 1 ){
      
      FirstObs <- results[[i]][[1]][1]
      
      if( FirstObs == -(10^10^2) ){ Check <- 1 }
      
    }
  }
  
  
  if( Check == 0 ){
    
    corr.bk <- corr.function3.bk(results,GDP = c("agg_gdp"),Nomes = c("agg_gdp","agg_consumption","agg_investment","gov_consumption_real","agg_unemployment_rate",
                                                                      "agg_capacity_util","agg_labor_prod","agg_inflation","agg_var_real_wage","cb_ir"),
                                 Titulo=c("gdp","con","invest","gov_cons","unem","util","prod","inflation","real_Wage","interest"),start=ceiling(periodos_simulacao*3/4),periodos=6,
                                 LAGS=c(1,1,1,1,0,0,1,0,0,0),frequency = 4) # Lags para definir o log de quem deve ser tirado
    
    
    # Error <- -( sum2( (Matrix.Empiric[,2:14] - corr.bk$Table.Corr)^2 ) + 100 * sum2( (Matrix.Empiric[,1]/100 - corr.bk$Table.Dp)^2 )  )
    # 
    # sum1 <- (sqrt((Matrix.Empiric[,2:14] - corr.bk$Table.Corr)^2)/Matrix.Empiric[,2:14])[ is.infinite(sqrt((Matrix.Empiric[,2:14] - corr.bk$Table.Corr)^2)/Matrix.Empiric[,2:14]) == FALSE]
    # 
    #  sum2 <- sqrt((Matrix.Empiric[,1]/100 - corr.bk$Table.Dp)^2)/Matrix.Empiric[,1]/100
    #  
    #  sum(sum2)/length(sum2)
    #  sum(sum1)/length(sum1)
    #  
    #  sumtotal <- sum(sum1 , sum2)
    #  
    #  sumtotal/(length(sum2) + length(sum1))
    
    vec <- c(c(Matrix.Empiric[,2:14] - corr.bk$Table.Corr),c( Matrix.Empiric[,1]/100 - corr.bk$Table.Dp ))
    
    vec_matrix <- matrix(vec)
    
    weight <- c(rep(length(c(Matrix.Empiric[,2:14] - corr.bk$Table.Corr)),length(c(Matrix.Empiric[,2:14] - corr.bk$Table.Corr))),
                rep(length(c( Matrix.Empiric[,1]/100 - corr.bk$Table.Dp )),length(c( Matrix.Empiric[,1]/100 - corr.bk$Table.Dp ))))
    
    Error <- - sqrt( t(vec_matrix) %*% solve(diag(weight,length(vec_matrix))) %*% vec_matrix )/length(vec_matrix)
    
    
    plot( Matrix.Empiric[1,2:14] ,type="l",col="blue")
    lines( corr.bk$Table.Corr[1,] , col="red")
    
    
  }else{ Error <- -1e+100 }
  
  for(i in 1:length(X)){
    
    assign(names_parameters[i],Original_values[i],envir=globalenv())
    
  }
  
  gc() # Limpeza
  rm(results)
  
  
  return(Error)
  
}

resettinglistsagents <- function(){
  
  
  if(exists("list_agents_model") == TRUE){
    
    for(i in 1:(length( list_agents_model )/4)){
      
      z <- (i-1)*4 + 1
      
      list.args <- list()
      
      list.args[[1]] <- list_agents_model[[z]]
      list.args[[2]] <- list_agents_model[[z+1]]
      list.args[[3]] <- list_agents_model[[z+2]]
      list.args[[4]] <- list_agents_model[[z+3]]
      
      do.call(create_agent,list.args, envir = parent.frame())
      
      
    }
  }
  
  if(exists("list_listas_model") == TRUE){ 
    for(i in 1:(length( list_listas_model )/3)){
      
      z <- (i-1)*3 + 1
      
      list.args <- list()
      
      list.args[[1]] <- list_listas_model[[z]]
      list.args[[2]] <- list_listas_model[[z+1]]
      list.args[[3]] <- list_listas_model[[z+2]]
      
      do.call(create_list_objects,list.args, envir = parent.frame())
      
      
    }
  }
}

# Gráfico para simulações múltiplas 

graph.function <- function(results,Nomes,Titulo,start,LOG){
  
  m <- length(results)
  TIME <- length(results[[1]][[1]][1,])
  
  if(start >= TIME){stop(print("Start value higher than end period"))}
  
  
  for(i in 1:length(Nomes)){
    
    y <- length( eval(parse(text=gsub(" ","",paste("results[[1]]$",Nomes[i]))))[,1]  )
    
    if(y > 1){
      
      Matrix.Mean <- matrix(0,m,TIME)
      Matrix.Sd <- matrix(0,m,TIME)
      
      for(Simul in 1:m){
        for(time in 1:TIME){
          
          if(LOG[i] == 0){
            
            Matrix.Mean[Simul,time] <- mean((eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            Matrix.Sd[Simul,time] <- sd((eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            
            
          }else{
            
            Matrix.Mean[Simul,time] <- mean(log(eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            Matrix.Sd[Simul,time] <- sd(log(eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]))
            
          }
        }
      }
      
      one <- Matrix.Mean[1,start:TIME]
      mean <- apply(Matrix.Mean,MARGIN=2,mean)[start:TIME]
      sd <- apply(Matrix.Mean,MARGIN=2,sd)[start:TIME]
      df_graph <- data.frame(mean,sd,start:TIME,one)
      names(df_graph) <- c("mean","sd","time","one")
      
      g <- ggplot(data=df_graph)+
        geom_line(aes(time,one),col='blue',size=0.3)+
        scale_linetype_manual(values=c("twodash")) +
        geom_line(aes(time,mean+sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean - sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean),size=1)+
        theme_bw() +
        labs( y = Titulo[i] , x = "Time")
      theme(text = element_text(size=14))
      
      print(g)
      
      # Salvando o gráfico
      
      filename <- gsub(" ","",paste(Nomes[i],".png"))
      
      ggsave(g, filename = filename, dpi = 300, type = 'cairo',
             width = 16, height = 16*0.6, units = 'cm')
      
      
    }else{
      
      Matrix.Mean <- matrix(0,m,TIME)
      
      for(Simul in 1:m){
        for(time in 1:TIME){
          
          Matrix.Mean[Simul,time] <- eval(parse(text=gsub(" ","",paste("results[[",Simul,"]]$",Nomes[i]))))[,time]
          
        }
      }
      
      
      
      if(LOG[i] == 0){
        
        one <- Matrix.Mean[1,start:TIME]
        mean <- apply(Matrix.Mean,MARGIN=2,mean)[start:TIME]
        sd <- apply(Matrix.Mean,MARGIN=2,sd)[start:TIME]
        
      }else{
        
        one <- log(Matrix.Mean[1,start:TIME])
        mean <- apply(log(Matrix.Mean),MARGIN=2,mean)[start:TIME]
        sd <- apply(log(Matrix.Mean),MARGIN=2,sd)[start:TIME]
        
      }
      
      df_graph <- data.frame(mean,sd,start:TIME,one)
      names(df_graph) <- c("mean","sd","time","one")
      
      g <- ggplot(data=df_graph)+
        geom_line(aes(time,one),col='blue',size=0.3)+
        geom_line(aes(time,mean+sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean - sd),linetype="dashed",col="red",size=0.3) +
        geom_line(aes(time,mean),size=1)+
        theme_bw() +
        labs( y = Titulo[i] , x = "Time")
      theme(text = element_text(size=14))
      
      print(g)
      
      # Salvando o gráfico
      
      filename <- gsub(" ","",paste(Nomes[i],".png"))
      
      ggsave(g, filename = filename, dpi = 300, type = 'cairo',
             width = 16, height = 16*0.6, units = 'cm')
      
      
    }
  }
}

