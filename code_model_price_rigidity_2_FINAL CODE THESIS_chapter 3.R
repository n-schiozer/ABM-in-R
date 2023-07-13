############ Funções modelo  ##############

# Funções firma production, capacity of production

name <- atualizar_expdem <- function(firm_expdem,firm_estoque,firm_vendas,firm_vendas_nr){
  
  # Caso com produto indivisivel/divisível.
  
  #firm_expdem <- ceiling( LAG(firm_vendas,1) + par.pas.dem *(LAG(firm_vendas,1) - LAG(firm_vendas,2)) + 0.2 * LAG(firm_vendas_nr,1) )
  
  firm_expdem <- ceiling((LAG(firm_vendas,1) + LAG(firm_vendas,2) + LAG(firm_vendas,3) + LAG(firm_vendas,4))/4 + 0.2 * LAG(firm_vendas_nr,1))
  
  firm_expdem <- ifelse( firm_expdem < 0.1 , 0 , firm_expdem)
  
  return(firm_expdem)
}
addfunABM(name)

name <- atualizar_producao <- function(firm_expdem,firm_estoque,firm_capacidade_producao){
  
  # Variáveis 
  firm_expdem <- VAR(firm_expdem)
  firm_capacidade_producao <- VAR(firm_capacidade_producao)
  firm_stock <- LAG(firm_estoque,1)
  
  # Equações
  
  firm_oferta_desejada <- ceiling(firm_expdem + firm_estoque_desejado_perc * firm_expdem - firm_stock)
  
  firm_producao <- ifelse( firm_oferta_desejada > firm_capacidade_producao , firm_capacidade_producao , firm_oferta_desejada )
  firm_producao[ firm_producao < 0 ] <- 0
  
  return(firm_producao)
}
addfunABM(name)

aux_vendas <- function(x){
  
  agg_demand_disp3 <- get("agg_demand_disp", envir = parent.frame(2), inherits = TRUE)
  
  prod <- VAR(firm_producao)[x]
  
  price <- VAR(firm_price)[x]
  
  
  maxvendas <- prod + LAG(firm_estoque,1)[x]
  
  if( maxvendas * price < agg_demand_disp3 ){
    
    qtidade <- floor( runif(1,0.6,0.95) * maxvendas )
    
    expense <- price * qtidade
    
  }else{
    
    qtidade1 <- floor( runif(1,0.6,0.95) * agg_demand_disp3/price )
    
    if( qtidade1 * price < agg_demand_disp3 ){
      
      qtidade <- qtidade1
      
      expense <- price * qtidade
      
    }else{
      
      qtidade <- 0
      
      expense <- 0
      
    }
  }
  
  qtidade <- ifelse( qtidade > maxvendas , maxvendas , qtidade)
  
  agg_demand_disp2 <-  agg_demand_disp3 - expense
  
  assign("agg_demand_disp",agg_demand_disp2,envir=parent.frame(2))
  
  return(qtidade)
}

name <- atualizar_vendas <- function(agg_nom_demand,firm_quality,firm_price,firm_producao,firm_estoque){
  
  # Load de funçóes
  
  maxvendas <- VAR(firm_producao) + LAG(firm_estoque,1)
  
  agg_demand_disp <- VAR(agg_nom_demand)
  
  price <- VAR(firm_price)
  
  quality <- VAR(firm_quality)
  
  # Função
  
  supply_nom <- price * maxvendas
  
  agg_supply_nom <- sum2(price* maxvendas)
  
  
  X <- Sample(c(1:quantidade_firmas), quantidade_firmas, replace = FALSE, prob = ( VAR(firm_quality) / sum2(VAR(firm_quality))) )
  
  lag_firm_ms <- LAG(firm_ms,1)
  
  agg_demand_disp_firm <- ((1 - market.inertia) * (( quality^1 / sum2( quality^1 ) ) + ( price^(-1)/ sum2( price^(-1))))/2  + market.inertia * lag_firm_ms ) * agg_demand_disp
  
  vendas <- ceiling(agg_demand_disp_firm/price)
  
  
  resultados <- ifelse( vendas > maxvendas, maxvendas, vendas)
  
  #prices <- c(1:quantidade_firmas)
  
  #for(i in 1:quantidade_firmas){
  #  
  #  index <- X[i]
  #  
  #  prices[i] <- VAR(firm_price)[index]
  #  
  #}
  
  #firm_vendas1 <- apply(X=X,MARGIN=2,FUN=aux_vendas)  #/prices
  
  #print(firm_vendas1)
  
  #result <- c(1:quantidade_firmas)
  
  #for(i in 1:quantidade_firmas){
  #  
  #  index <- X[i] # Qual foi a primeira empresa sorteada
  
  #  result[index] <- firm_vendas1[i] # Qual foi o valor vendido por esta empresa. COlocar na posição correta em vetor reusltado
  
  #}
  
  firm_vendas <- resultados
  
  
  return(firm_vendas)
}
#addfunABM(name)

name <- atualizar_agg_demand_disp_firm <- function(agg_nom_demand,firm_quality,firm_price,firm_producao,firm_estoque){
  
  # Load de funçóes
  
  agg_demand_disp <- sum2(VAR(workers_consumo)) + VAR(capitalist_consumo) + VAR(gov_consumption)
  
  firm_demand_disp <- VAR(firm_ms) * agg_demand_disp
  
  return(firm_demand_disp)
}
addfunABM(name)

name <- atualizar_vendas <- function(firm_demand_disp,firm_estoque,firm_producao,firm_price){
  
  # Load de funçóes
  
  maxvendas <- VAR(firm_producao) + LAG(firm_estoque,1)
  
  price <- VAR(firm_price)
  
  firm_demand_disp <- VAR(firm_demand_disp)
  
  ms <- VAR(firm_ms)
  
  id_firmas <- 1:quantidade_firmas
  
  # Funções
  
  # Ciclo abaixo em que o produto não pode ser quebrado.
  
  vendas <- floor(firm_demand_disp/price)
  
  rest <- sum2( firm_demand_disp - vendas * price )
  
  firms_with_price_abore_rest <- rest > price
  
  division <- 2
  
  while( any( rest > price  ) & division < 11  ){ # While para a distribuição da renda restante
    
    firms_with_price_abore_rest <- rest > price
    
    id_firms_in_market <- id_firmas[firms_with_price_abore_rest ]
    
    qt_firmas <- ceiling( length(id_firms_in_market)/division )
    
    if( qt_firmas > 1 ){
      
      selected_firmas <- Sample(id_firms_in_market,qt_firmas,replace=F,prob = ms[id_firms_in_market] )
      
      ms_selected <- ms[ selected_firmas]/sum2(ms[ selected_firmas])

      vendas[selected_firmas] <-  floor( ms_selected*rest/price[selected_firmas] ) + vendas[selected_firmas]
      
    }
    
    rest <- sum2( firm_demand_disp - vendas * price )
    
    division <- division + 1
    
  }
  
  # Se produto puder ser dividido em partes infenitesimais (grande vantagem é que cálculo é mais rápido, mas gera problemas. Ciclos estranhos).
  
  #vendas <- (firm_demand_disp/price)
  
  resultados <- ifelse( vendas > maxvendas, maxvendas, vendas)
  
  firm_vendas <- resultados
  
  return(firm_vendas)
}
addfunABM(name)

name <- atualizar_vendas_nrealizadas <- function(firm_vendas,firm_producao,firm_estoque,firm_price,firm_demand_disp){
  
  maxvendas <- VAR(firm_producao) + LAG(firm_estoque,1)
  
  vendas_possiveis <- floor(VAR(firm_demand_disp)/VAR(firm_price))
  
  firm_vendas_nr <- 1:quantidade_firmas
  
  firm_vendas_nr <- vendas_possiveis - maxvendas
  
  firm_vendas_nr[ firm_vendas_nr <0 ] <- 0
  
  return(firm_vendas_nr)
}
addfunABM(name)

name <- atualizar_estoques <- function(firm_vendas, firm_producao,firm_estoque){
  
  firm_estoque <- VAR(firm_producao) + LAG(firm_estoque,1) - VAR(firm_vendas)
  
  return(firm_estoque)
}
addfunABM(name)

name <- atualizar_grau_utilizacao <- function(firm_expdem, firm_capacidade_producao,firm_ms){
  
  firm_grau_utilizacao <- (VAR(firm_vendas))/( VAR(firm_machines) * prod_machine )
  firm_grau_utilizacao[ firm_grau_utilizacao > 1] <- 1
  
  return(firm_grau_utilizacao)
}
addfunABM(name)

# Capacidade de produção das firmas

aux_cap_prod <- function(i){
  
  klimite <- floor( sium2( lista_maquinas[[i]][2,] ) )
  
  num_maq <- length( lista_maquinas[[i]][2,] )
  
  num_trabalhadores <- sum2( lista_funcionarios[[i]][2,] )
  
  capacidade_producao_desejada <- VAR(firm_expdem)[i]/grau_utilizao_desejado
  
  indirect_labour_neaded <-  ceiling(capacidade_producao_desejada /( mean.default(lista_maquinas[[i]][2,])) * parameter_trab_indireto)
  
  num_trabalhadores_diretos <- num_trabalhadores - indirect_labour_neaded
  
  Med <- sum2( lista_maquinas[[i]][3,] )/length(lista_maquinas[[i]][3,])
  llimite <- num_trabalhadores_diretos * Med * sum2(lista_maquinas[[i]][2,])/length(lista_maquinas[[i]][2,])
  
  #if( num_trabalhadores_diretos < 0){ num_trabalhadores_diretos <- 0 ; num_maquinas_usadas <- 0 ; llimite <- 0 }else{
  #  
  #  Med <- sum2( lista_maquinas[[i]][3,] )/length(lista_maquinas[[i]][3,])
  #  
  #  num_maquinas_usadas <- floor( num_trabalhadores_diretos * Med )
  #  
  # K = K/L * L
  #  
  #  if( num_maquinas_usadas <= length(lista_maquinas[[i]][2,])  ){
  #    
  #    llimite <- sum2( sort( lista_maquinas[[i]][2,], decreasing = TRUE)[1:num_maquinas_usadas] )
  #    
  #  }else{ llimite <- num_trabalhadores_diretos * Med * sum2(lista_maquinas[[i]][2,])/length(lista_maquinas[[i]][2,]) }
  #  
  #  # Y/K * K/L * L = Y
  #  
  #}
  
  resultado <- floor(ifelse( klimite >= llimite , llimite , klimite ))
  resultado <- ifelse(resultado <= 0 , 1 , resultado)
  
  return(resultado)
  
}

name <- atualizar_capacidade_producao <- function(firm_capacidade_producao, lista_maquinas, lista_funcionarios){
  
  VAR(workers_empregador) # Para rodar o mercado de trabalho antes
  VAR(firmk_production) # Para rodar antes a produção do maquina produtoras de bens de capital
  # Apenas para marcar que é para rodar o mercado de trabalho antes!!!!!!!!!!
  
  firm_capacidade_producao <- VAR(firm_capacidade_producao)
  
  firm_exp_dem <- VAR(firm_expdem)
  
  # Variáveis 
  firm_expdem <- VAR(firm_expdem)
  
  firm_stock <- LAG(firm_estoque,1)
  
  firm_exp_dem <- ceiling( firm_expdem * ( 1 + firm_estoque_desejado_perc ) - firm_stock )
  
  # Importantíssimo! É oq faz identificar que precisa rodar antes o mercado de trabalho
  
  matriz <- matrix(c(1:quantidade_firmas),1,quantidade_firmas)
  # firm_capacidade_producao <- apply(matriz, MARGIN=2,FUN=aux_cap_prod)
  
  lista_maq <- lista_maquinas
  
  coef_tec <- VAR(firm_coef_tecnico)
  
  llimite_firmas <- rep(0,quantidade_firmas)
  klimite_firmas <- rep(0,quantidade_firmas)
  num_trabalhadores_firmas <- rep(0,quantidade_firmas)
  
  for(i in 1:quantidade_firmas){
    
    klimite <- floor( sum2( lista_maquinas[[i]][2,] ) )
    
    num_maq <- length( lista_maquinas[[i]][2,] )
    
    num_trabalhadores <- sum2( lista_funcionarios[[i]][2,] )
    num_trabalhadores_firmas[i] <- num_trabalhadores
    
    capacidade_producao_desejada <- firm_exp_dem[i]/grau_utilizao_desejado
    
    #indirect_labour_neaded <-  ceiling(capacidade_producao_desejada /( mean.default(lista_maq[[i]][2,])) * parameter_trab_indireto)
    
    #num_trabalhadores_diretos <- num_trabalhadores - indirect_labour_neaded
    
    #Med <- sum2( lista_maq[[i]][3,] )/length(lista_maq[[i]][3,])
    #llimite <- num_trabalhadores_diretos * Med * sum2(lista_maq[[i]][2,])/length(lista_maq[[i]][2,])
    
    #if( num_trabalhadores_diretos < 0){ num_trabalhadores_diretos <- 0 ; num_maquinas_usadas <- 0 ; llimite <- 0 }else{
    #  
    #  Med <- sum2( lista_maquinas[[i]][3,] )/length(lista_maquinas[[i]][3,])
    #  
    #  num_maquinas_usadas <- floor( num_trabalhadores_diretos * Med )
    #  
    # K = K/L * L
    #  
    #  if( num_maquinas_usadas <= length(lista_maquinas[[i]][2,])  ){
    #    
    #    llimite <- sum2( sort( lista_maquinas[[i]][2,], decreasing = TRUE)[1:num_maquinas_usadas] )
    #    
    #  }else{ llimite <- num_trabalhadores_diretos * Med * sum2(lista_maquinas[[i]][2,])/length(lista_maquinas[[i]][2,]) }
    #  
    #  # Y/K * K/L * L = Y
    #  
    #}
    
    llimite <- floor(num_trabalhadores/coef_tec[i]) # L = coef * Y # Coef = Yd/a + kappa K = Y/a + kappa * Y/v = (1/a + kappa/v) Y
    
    
    resultado <- floor(ifelse( klimite >= llimite , llimite , klimite ))
    resultado <- ifelse(resultado <= 0 , 1 , resultado)
    
    llimite_firmas[i] <- llimite
    klimite_firmas[i] <- klimite
    
    firm_capacidade_producao[i] <- resultado
    
  }
  
  return(firm_capacidade_producao)
  
}
addfunABM(name)

name <- atualizar_labor_productivity <- function(firm_producao, lista_funcionarios){
  
  firm_labor_productivity <- c(1:quantidade_firmas)
  
  firm_producao <- VAR(firm_producao)
  
  firm_tot_funcionarios <- VAR(firm_tot_funcionarios)
  
  firm_labor_productivity[ firm_tot_funcionarios == 0 ] <- 0.01
  firm_labor_productivity[ firm_tot_funcionarios != 0] <- firm_producao[ firm_tot_funcionarios != 0]/firm_tot_funcionarios[ firm_tot_funcionarios != 0]
  
  return(firm_labor_productivity)
  
}
addfunABM(name)

name <- atualizar_trabalhadores_necessarios <- function(firm_expdem,lista_maquinas){
  
  resultado <- rep(0,quantidade_firmas)
  
  capacidade_producao_desejada <- VAR(firm_expdem)/(grau_utilizao_desejado) # Aumento um pouco pelo risco de roubo de funcionários
  
  capacidade_producao_desejada <- VAR(firm_expdem)*(1 + 0.05) # Aumento um pouco pelo risco de roubo de funcionários
  
  coef_tec <- VAR(firm_coef_tecnico)
  
  machines_por_firm <- VAR(firm_machines) # Marcador para rodar antes a produção da firma produttora de bens de K
  
  coef_tecn <- VAR(firm_coef_tecnico)
  
  for(i in 1:quantidade_firmas){
    
    #capacidade_producao_desejada <- VAR(firm_expdem)[i] * ( 1 + firm_estoque_desejado_perc[i])/grau_utilizao_desejado
    
    #  L = Y'/a = Y/u * 1/a Y/L = Y/K * K/L 
    
    y_k <- mean.default(lista_maquinas[[i]][2,])
    k_l <- sum2(lista_maquinas[[i]][3,]) /length(lista_maquinas[[i]][3,])
    
    a <- y_k * k_l # Relação Y/L pelos coeficientes das máquinas
    
    direct_labor_prod <-  capacidade_producao_desejada[i] /a  # Yd = ( Y/K * K/L) * L  - > Yd * K/Y * L/K = L      
    
    direct_labor_prod <- ifelse(direct_labor_prod < 0 , 0, direct_labor_prod)
    
    #indirect_labour_neaded <- ceiling( VAR(firm_expdem)[i] /( mean.default(lista_maquinas[[i]][2,]))) * parameter_trab_indireto # Yd = Yd/K * K  -> Yd * K/Yd = K
    
    #indirect_labour_neaded <- length(lista_maquinas[[i]][2,]) * parameter_trab_indireto  # Exp.D/Prod.Máq = número máq usadas. * Parâmetros imob
    
    indirect_labour_neaded <-  capacidade_producao_desejada[i] /y_k * parameter_trab_indireto
    
    limite_per_machines <- ceiling( machines_por_firm[i] * 1/coef_tecn[i] )
    
    resultado[i] <- min( direct_labor_prod + indirect_labour_neaded , limite_per_machines)
    
    # Considerando rstrição do estoque de máquinas. COmplicação demais.
    
    #max.given.capital <- length( lista_maquinas[[i]][1,] ) * ( parameter_trab_indireto +  sum2(lista_maquinas[[i]][3,])/length(lista_maquinas[[i]][3,]) )
    
    #resultado[i] <- ifelse(resultado[i] > max.given.capital , max.given.capital , resultado[i])
    
    
  }
  
  
  firm_labor_demand <- ceiling(resultado)
  
  return(firm_labor_demand)
}
#addfunABM(name)

name <- atualizar_trabalhadores_necessarios <- function(firm_expdem,lista_maquinas){
  
  machines_por_firm <- LAG(firm_machines,1) # Marcador para rodar antes a produção da firma produttora de bens de K
  
  limite_per_exp <- rep(0,quantidade_firmas)
  
  capacidade_producao_desejada <- VAR(firm_expdem)/(grau_utilizao_desejado) # Aumento um pouco pelo risco de roubo de funcionários
  
  capacidade_producao_desejada <- (VAR(firm_expdem)*(1 + estoque_desejado_perc ) - LAG(firm_estoque,1))*1.05   # Aumento um pouco pelo risco de roubo de funcionários
  
  coef_tec <- VAR(firm_coef_tecnico)
  
  limite_per_exp <- capacidade_producao_desejada*coef_tec # L = coef Y
  
  limite_per_machines <- ceiling( machines_por_firm*( 1/rel.k.l + parameter_trab_indireto ) ) # Y - C L  
  
  escolhas_possiveis <- cbind(limite_per_exp,limite_per_machines)
  
  resultados <- apply( escolhas_possiveis ,MARGIN=1,min)
  
  firm_labor_demand <- ceiling(resultados)
  
  firm_labor_demand[ firm_labor_demand == 0 ] <- 1
  
  return(firm_labor_demand)
}
addfunABM(name)


# descrição coeiciente técnioco
# Parâmetro para transformar o salário médio em custo unitário
# Muito usado em vários momentos

# Sendo: CU= WL/Y =W (L/Y)  # coeficiente técnico é a inversa da produtividade do trabalho

# Chegamos nisso no modelo:

# L = Ld + Li = Y/a + kappa K = Y/a + kappa * Y/v

# Dado que precisamos de trabalhadores suficientes para a capacidade ma´xima de produção, Y é a capacidade máxima e Y/0.8 desejada

name <- atualizar_coef_tecnico_firmas <- function(firm_coef_tecnico){
  
  firm_coef_tecn <- 1:quantidade_firmas
  
  # L = Y/a + kappa * K = Y/a + kappa * Y/v = (1/a + kappa/v) * Y
  # Coef = (1/a + kappa/v)
  
  
  # for(i in 1:quantidade_firmas){ # DESATIVADO O CICLO SOBRE AS FIRMAS JÁ QUE É TUDO IGUAL E TOMA MTO TEMPO
  #   
  #   lista <- lista_maquinas[[i]]
  #   
  #   v <- (sum2(lista[2,]))/length(lista[2,]) # v = Y/K
  #   a <- v * (sum2(lista[3,])/length(lista[3,])) # Y/L = 1/v * rela.k.l
  #   
  #   firm_coef_tecn[i] <- (parameter_trab_indireto/v  + 1/a) # L = coef Y
  #   
  #   # L = (Y/a + kappa * K) = (Y/a + kappa * Y/v) = (1/a + kappa/v) Y
  #   
  # }
  # 
  
  i <- 1 
  
  lista <- lista_maquinas[[i]]
  
  v <- (sum2(lista[2,]))/length(lista[2,]) # v = Y/K
  a <- v * (sum2(lista[3,])/length(lista[3,])) # Y/L = v * rela.k.l
  
  firm_coef_tecnico <- rep((parameter_trab_indireto/v  + 1/a),quantidade_firmas) # L = coef Y
  
  
  return(firm_coef_tecnico)
}
addfunABM(name)

# Mercado de trabalho

name <- wage_ref_firm <- function(workers_salario,workers_empregador,firm_capacidade_producao,firm_vendas_nr){
  
  lag_custo_unit <- LAG(firm_wage_ref,1) # Wage ref
  lag_firm_nr <- LAG(firm_vendas_nr,1) 
  lag_cap_prod <- LAG(firm_capacidade_producao,1)
  lag_firm_custo_unit <- LAG(firm_custo_unit,1) # Custo efetivo por unidade
  
  tempo <- VAR(time_t)
  VAR(time_s)
  
  tamanho_lista_selecionada_firmas <- ncol(list_firms_selected)
  
  list_firms_selected <- matrix(list_firms_selected,quantidade_firmas,tamanho_lista_selecionada_firmas)
  
  lag_custo_unit_consult <- rowSums2( matrix(lag_custo_unit[ list_firms_selected[,1:tamanho_lista_selecionada_firmas] ],quantidade_firmas,tamanho_lista_selecionada_firmas) )/tamanho_lista_selecionada_firmas
  
  firm_wage_ref <- LAG(firm_wage_ref,1)
  
  firmk_wage_ref <- LAG(firmk_wage_ref,1)
  
  capacidade_producao_desejada <- VAR(firm_expdem)
  
  coef_tecn <- c(VAR(firm_coef_tecnico)) ## Y = coef L 
  
  #llimite <- LAG(firm_tot_funcionarios,1)
  
  #llimite <- floor(llimite/coef_tecn)
  
  firm_wage_ref_init <- firm_wage_ref
  
  var <- ( 1 + par_var_wages * ( LAG(firm_labor_demand,1)/LAG(firm_tot_funcionarios,1) - 1.02) )
  

  if( tempo > 2){
    
    #mean <- 0.5 * lag_custo_unit + 0.25 * lag_custo_unit_consult + 0.25 * lag_firm_custo_unit  #+ 0.1 * firmk_wage_ref   # Custo médio por trabalhador das firmas
    # LAG DO WAGE REF, # LAG DOS VALORES PASSADOS , # LAG DO CUSTO EFETIVO
    
    mean <- (1 - par_wage_ref) * lag_custo_unit + par_wage_ref/2 * ( lag_custo_unit_consult +  lag_firm_custo_unit)
    
    var <- var
    var[ var > (max_readjustment_wage_ref + 1)] <- max_readjustment_wage_ref + 1
    
    #var <- 1
    
    firm_wage_ref <- mean*var
    
  }else{
    
    firm_wage_ref <- rep( 1.01*mean(LAG(workers_salario_desej,1)) , quantidade_firmas )
    
  }
  
  firm_wage_ref[ firm_wage_ref < 2 ] <-  mean(LAG(workers_salario_desej,1))
  
  firm_wage_ref[ LAG(firm_leverage,1) > 1] <- Sample( firm_wage_ref, sum( LAG(firm_leverage,1) > 1) , prob=LAG(firm_ms,1)  )
  
  # SE o salário ref hoje menor 
  
  #firm_wage_ref[ firm_wage_ref < lag_custo_unit ] <- lag_custo_unit[ firm_wage_ref < lag_custo_unit ]
  
  # Reajuste pela inflação
  
  # if(VAR(time_t) > 120){
  #   if( VAR(time_t)/4 - round( VAR(time_t)/4) == 0 ){
  # 
  #     firm_wage_ref <- firm_wage_ref * (1 + infla_indexation * (LAG(agg_index_prices,1)/LAG(agg_index_prices,5)-1))
  # 
  #   }
  # }
  
  # if(VAR(time_t) > 120){
  # 
  #     firm_wage_ref <- firm_wage_ref * (1 + infla_indexation * (LAG(agg_inflation,1)))
  # }
  
  
  return(firm_wage_ref)
}
addfunABM(name)


aux_contrat_demit <- function(X){
  
  return( any(X==index_dem) )
  
}

name <- contratar_demitir_funcionarios <- function(lista_funcionarios,firm_labor_demand,workers_empregador,firmk_labor_demand){
  
  # Carregar variáveis 
  resultados <- VAR(workers_empregador)
  
  ordemfirms <- Sample(1:(quantidade_firmas+1))
  
  lag_salario <- LAG(workers_salario_desej,1)
  mean.default_nom_wage <- sum2(lag_salario[ lag_salario != 0 ])/length(lag_salario[ lag_salario != 0 ])
  
  firm_labor_demand <- VAR(firm_labor_demand)
  
  firm_labor_demand <- c(firm_labor_demand,firmk_labor_demand)
  
  # Função
  
  for(index in 1:(quantidade_firmas+1)){
    
    i <- ordemfirms[index]
    
    # Demanda firma produtora de bens de capital
    
    if( i > quantidade_firmas ){  demanda_trab <- VAR(firmk_labor_demand)  }else{
      
      demanda_trab <- firm_labor_demand[i]
      
    }
    
    # Demissão por salários muito altos 
    
    if(length(lista_funcionarios[[i]][1,]) > 1){
      
      index_trab <- 2:length(lista_funcionarios[[i]][1,])
      
      salarios <- lag_salario[ lista_funcionarios[[i]][1, index_trab ] ]
      index_dem <- lista_funcionarios[[i]][1,index_trab][ salarios/mean.default_nom_wage > ( 1 + dif_salarial_emp_des) ]
      
      if( identical(index_dem,numeric(0)) == FALSE){
        
        resultados[index_dem] <- 0 # Alterando o vetor workers empregador
        
        lista <- lista_funcionarios[[i]]
        
        select <- rep(0,(length(index_trab)+1))
        
        for(v in 1:length(index_dem)){ # Alteração lista c/ apenas funcionários ativos
          
          select[ which(lista[1,] ==  index_dem[v])  ] <- 1
          
        }
        
        matrix_len <- length( select[ select == 0])
        
        lista_funcionarios[[i]] <- matrix( lista[1:2,][1:2, select==0 ] , 2 , matrix_len)
      }
      
    }
    # Final demissão por salários altos
    
    # Soma funcionários após primeiro ciclo de demissões
    
    soma_funcionarios <- sum2( lista_funcionarios[[i]][2,] )
    
    #if( is.na(soma_funcionarios) || is.nan(soma_funcionarios) || is.na(demanda_trab) || is.nan(demanda_trab) ){
    #  
    #  print(i)
    #  print(demanda_trab)
    #  print(soma_funcionarios)
    #  print(lista_funcionarios[[i]])
    #  
    #}
    
    deveempregar <- demanda_trab  > soma_funcionarios
    
    devedemitir <- demanda_trab  < soma_funcionarios
    
    if( deveempregar ){
      
      num_contratacoes <- demanda_trab - soma_funcionarios
      
      tamanho_Sample <- ifelse(quantidade_workers < num_contratacoes * 8, quantidade_workers, num_contratacoes * 5 )
      
      #selecionados <- Sample( 1:quantidade_workers , tamanho_Sample )
      
      if( i == quantidade_firmas + 1){
        
        selecionados <- round(runif(quantidade_workers/2,1,quantidade_workers),0) # Empresa produtora de bens de capital procura por metade dos trabalhadores
        
        
      }else{
        
        selecionados <- round(runif(tamanho_Sample,1,quantidade_workers),0)
        
      }
      
      candidatos <- rbind(matrix(c(selecionados),1),matrix(c(resultados[selecionados]),1))
      
      rownames(candidatos) <- c("selecionados","empregador")
      
      if( any(candidatos[2,] == 0) ){
        
        selecionados <- matrix( candidatos[1:2, candidatos[2,] == 0 ] , 2, length(candidatos[1:2, candidatos[2,] == 0 ])/2  )
        
        limite_matrix <- ifelse(num_contratacoes > length(selecionados[1,]), length(selecionados[1,]), num_contratacoes )
        
        if( limite_matrix > 0){
          
          posicao <- selecionados[1,1:limite_matrix]
          
          resultados[posicao] <- i
          
          selecionados[1:2,1:limite_matrix][ selecionados[1:2,1:limite_matrix] == 0 ] <- 1
          
          lista_funcionarios[[i]] <- cbind( lista_funcionarios[[i]], matrix( selecionados[1:2,1:limite_matrix] , 2 , limite_matrix)  )
          
        }
      }
      
    }else{
      if( devedemitir && (soma_funcionarios > 0) ){
        
        num_dem <- soma_funcionarios - demanda_trab
        
        Sample <- lista_funcionarios[[i]][1,2:length(lista_funcionarios[[i]][1,])]
        
        index_dem <- c( Sample( Sample, num_dem ) )
        
        resultados[index_dem] <- 0 # Alterando o vetor workers empregador
        
        lista <- lista_funcionarios[[i]]
        
        select <- rep(0,length(Sample))
        
        for(v in 1:length(index_dem)){ # Alteração lista c/ apenas funcionários ativos
          
          select[ which(lista[1,] ==  index_dem[v])  ] <- 1
          
        }
        
        lista_funcionarios[[i]] <- lista[1:2,][1:2, select==0 ]
        
      }
    }
    
    
    if( length(lista_funcionarios[[i]]) == 2   ){  lista_funcionarios[[i]] <- matrix(0,2,1)}
  }
  #}
  
  
  assign("lista_funcionarios", lista_funcionarios, envir= where("simulationABMFAST2"))
  
  workers_empregador <- resultados
  
  return(workers_empregador)
  
}
#addfunABM(name)

name <- contratar_demitir_funcionarios <- function(lista_funcionarios,firm_labor_demand,workers_empregador,firmk_labor_demand){
  
  # Carregar variáveis 
  status_emprego <- VAR(workers_empregador)
  
  ordemfirms <- Sample(1:(quantidade_firmas+1))
  
  lag_salario <- LAG(workers_salario_desej,1)
  mean.default_nom_wage <- sum2(lag_salario[ lag_salario != 0 ])/length(lag_salario[ lag_salario != 0 ])
  
  firm_labor_demand <- VAR(firm_labor_demand)
  
  # Função
  
  for(index in 1:(quantidade_firmas+1)){
    
    i <- ordemfirms[index]
    
    
    quantidade_funcionarios <- length( status_emprego[ status_emprego == i ] )
    index_funcionarios <- which( status_emprego == i  )
    
    # Demanda firma produtora de bens de capital
    
    if( i > quantidade_firmas ){  demanda_trab <- VAR(firmk_labor_demand)  }else{
      
      demanda_trab <- firm_labor_demand[i]
      
    }
    
    # Demissão por salários muito altos 
    
    if(quantidade_funcionarios > 1){
      
      salarios <- lag_salario[ index_funcionarios ] # Qual foi o salário no período anterior
      
      index_dem <- index_funcionarios[ salarios/mean.default_nom_wage > ( 1 + dif_salarial_emp_des) ]
      
      
      if( identical(index_dem,numeric(0)) == FALSE){
        
        status_emprego[index_dem] <- 0 # Alterando o vetor workers empregador
        
        
      }
    }
    # Final demissão por salários altos
    
    # Soma funcionários após primeiro ciclo de demissões
    
    quantidade_funcionarios <- length( status_emprego[ status_emprego == i ] )
    index_funcionarios <- which( status_emprego == i  )
    
    #if( is.na(soma_funcionarios) || is.nan(soma_funcionarios) || is.na(demanda_trab) || is.nan(demanda_trab) ){
    #  
    #  print(i)
    #  print(demanda_trab)
    #  print(soma_funcionarios)
    #  print(lista_funcionarios[[i]])
    #  
    #}
    
    deveempregar <- demanda_trab  > quantidade_funcionarios
    
    devedemitir <- demanda_trab  < quantidade_funcionarios
    
    if( deveempregar ){
      
      num_contratacoes <- demanda_trab - quantidade_funcionarios
      
      tamanho_Sample <- ifelse(quantidade_workers < num_contratacoes * 8, quantidade_workers, num_contratacoes * 5 )
      
      #selecionados <- Sample( 1:quantidade_workers , tamanho_Sample )
      
      if( i == quantidade_firmas + 1){
        
        selecionados <- round(runif(quantidade_workers/2,1,quantidade_workers),0) # Empresa produtora de bens de capital procura por metade dos trabalhadores
        
        
      }else{
        
        selecionados <- round(runif(tamanho_Sample,1,quantidade_workers),0)
        
      }
      
      status_selecionados <- status_emprego[selecionados]
      
      if( any(status_selecionados == 0) ){
        
        if( sum2( status_selecionados == 0 ) > num_contratacoes ){
          
          coordenada.contratados <- selecionados[status_emprego[selecionados] == 0][1:num_contratacoes]   # Selecionados que possuem status de emprego igual a 0 
          
          # (estão desempregados). SEleciono até os trabalhadores necessários.
          
          status_emprego[ coordenada.contratados ] <- i
          
        }else{ # Se o número de selecionados que estão desempregados não é maior do que o número de vagas:
          
          coordenada.contratados <- selecionados[status_emprego[selecionados] == 0] # Coleto a posição de todos os trabalhadores selecionados que possuem status = 0
          
          status_emprego[ coordenada.contratados ] <- i
          
        }
      }
    }else{ # Se eu não devo contratar:
      if( devedemitir && (quantidade_funcionarios > 0) ){
        
        num_dem <- quantidade_funcionarios - demanda_trab
        
        Sample <- index_funcionarios
        
        index_dem <- c( Sample( Sample, num_dem ) )
        
        status_emprego[index_dem] <- 0 # Alterando o vetor workers empregador
        
      }
    }
    
    row1 <-  matrix(c(0,which(status_emprego == i)),1,length(which(status_emprego == i))+1)
    row2 <- matrix( c(0, rep(1,(length(row1)-1)))    ,1,length(row1))
    
    lista_funcionarios[[i]] <- matrix(rbind(row1,row2),2)
    
  }
  
  assign("lista_funcionarios", lista_funcionarios, envir= where("simulationABMFAST2"))
  
  workers_empregador <- status_emprego
  
  return(workers_empregador)
  
}
#addfunABM(name)

# Função emprego com dinâ mica mais interessante e realista
name <- contratar_demitir_funcionarios <- function(lista_funcionarios,firm_labor_demand,workers_empregador,firmk_labor_demand){
  
  # Carregar variáveis 
  status_emprego <- VAR(workers_empregador)
  
  prob_encontro <- rep(1,quantidade_workers)
  prob_encontro[ status_emprego == 0 ] <- 10
  
  ordemfirms <- c(201,Sample(1:(quantidade_firmas)))
  
  lag_salario <- LAG(workers_salario,1)
  
  mean.default_nom_wage <- sum2(lag_salario[ lag_salario != 0 ])/length(lag_salario[ lag_salario != 0 ])
  
  firm_labor_demand_1 <- c(VAR(firm_labor_demand),VAR(firmk_labor_demand))
  
  reserve_wage <- VAR(workers_salario_desej)
  
  wage_firms <- VAR(firm_wage_ref)
  
  orders_machines <- sum(LAG(firm_machine_order,1)) + 1
  production_k <- LAG(firmk_production,1) + 1
  
  wage_firms <- c(wage_firms,VAR(firmk_wage_ref)) # Meand para considerar o salário do setor produtor de bens de capital
  
  id_workers <- 1:quantidade_workers
  
  id_workers_disponivei <- 1:quantidade_workers
  
  contatos <- rep(0,quantidade_workers)
  
  matrix_disp <- cbind(contatos,id_workers_disponivei)
  
  # Ciclo contratação
  
  for(index in 1:(quantidade_firmas+1)){
    
    #empregar <- 0
    
    i <- ordemfirms[index]
    
    #print(index)
    
    id_workers_empresa_i <- id_workers[status_emprego == i]
    
    num <- length(id_workers_empresa_i)
    
    #empregar[ num - firm_labor_demand[i] < 0 ] <- 1
    
    contratacoes <- firm_labor_demand_1[i] - num
    
    if(contratacoes > 0){
      
      #contratacoes <- firm_labor_demand[i] - num # Número de contratações 
      
      #vag <- matrix(matrix_disp,,2)[,2]
      
      #if( 3*(firm_labor_demand[i] - num) >= quantidade_workers ){ id_Sample <- id_workers }else{ id_Sample <- Sample(id_workers,3*contratacoes, replace=FALSE)}
      
      # if( length(id_workers[contatos < 4]) < 3*contratacoes ){ id_Sample <- id_workers[contatos < 4] }else{
      #    
      #  id_Sample <- Sample(id_workers[contatos < 4], 3*contratacoes, FALSE) # Id dos trabalhadores que entrou em contato
      #  
      #}
      
      # Fórmula do Ifelse #if( 3*(firm_labor_demand[i] - num) >= quantidade_workers ){ id_Sample <- id_workers }else{ id_Sample <- Sample(id_workers,3*contratacoes, replace=FALSE)}
      
      if( 2*contratacoes >= length(id_workers) ){ id_Sample <- id_workers }else{ id_Sample <- Sample(id_workers,2*contratacoes, replace=FALSE,prob = prob_encontro)}
      
      
      # IFELSE NÂO FUNCIONA. id_Sample <- ifelse( 3*contratacoes >= quantidade_workers , id_workers, Sample(id_workers,3*contratacoes, replace=FALSE) )
      
      
      # CONDIÇÇAO PARA VER MÚLTIPLAS CONDIÇOES : %in% . Exemplo: id_workers %in% in_Sample . Quais os id dos trabalhadores que estão no id_Sample
      
      # id_Sample <- id_Sample[ contatos[id_Sample] < 3 ]
      
      pos_selected <- which( id_workers %in% id_Sample )
      
      contatos[pos_selected] <- contatos[pos_selected] + 1
      
      
      #matrix_disp[ , 1][cond]  <- matrix_disp[ , 1][cond] + 1 # Marcador de que entrou em contato com o trabalahdores
      
      #matrix_disp <- matrix_disp[ matrix_disp[,1] < 4 , ]
      
      id_Sample_aceite <- id_Sample[ reserve_wage[ id_Sample ] < wage_firms[i] & contatos[id_Sample] < 2 ] # Dos trabalhadores em contato, quantos aceitam trabalhar para a empresa
      
      
      if( length(id_Sample_aceite) != 0 ){ # Se pelo menos 1 aceitou trabalhar para a empresa
        
        id_Sample_aceitos <- id_Sample_aceite[1:contratacoes] # Seleciona um número de trabalhadores iguai ao das contratações (os NAs ficam por último)
        #id_Sample_aceitos <- id_Sample_aceitos[is.na(id_Sample_aceitos) == FALSE] # Retira os nNAS
        
        reserve_wage[ id_Sample_aceitos ] <- wage_firms[i] # Atualiza o salário de referência dos contratados
        status_emprego[ id_Sample_aceitos ] <- i # Atualiza o status do emprego para o ID do novo empregador
        
        contatos[ as.vector(which( status_emprego == i )) ]  <- 10 # Refaz o contrato com os trabalhadores já empregados
        
        
      }
      
      #id_workers <- id_workers[ contatos < 2 ] # Tira da do vetor de ID dos trabalhadores aqueles que tiveram mais de 2 contatos ou tiveram contratos reassinados
      #contatos <- contatos[ contatos < 2 ]
      
      
    }
  }
  
  
  # Ciclo demissão
  for(i in 1:(quantidade_firmas+1)){
    
    demitir <- 0
    
    id_workers_empresa_i <- as.vector(which( status_emprego == i ))
    
    num <- length(id_workers_empresa_i)
    
    num_dem <- num - firm_labor_demand_1[i]
    
    if(num_dem > 0){
      
      num_dem <- num - firm_labor_demand_1[i]
      
      id_demitidos <- id_workers_empresa_i[ (length(id_workers_empresa_i) - num_dem):length(id_workers_empresa_i) ]
      
      status_emprego[ id_demitidos ] <- 0
      
      #id_workers_empresa_i[ id_demitidos ] <- 0
      
    }
    
    # Aproveito o ciclo de demissão para fazer a lista de funcionários. Evito precisar procurar novamente qual a empresa de cada trabalhador (toma mto tempo)
    
    
    id_workers_empresa_i <- id_workers_empresa_i[id_workers_empresa_i > 0]
    
    #}
    
    #for(i in 1:(quantidade_firmas+1)){
    
    #id_workers_empresa_i <- id_workers[ status_emprego == i ]
    
    row1 <-  matrix(c(0,id_workers_empresa_i),1,length(id_workers_empresa_i)+1)
    row2 <- matrix( c(0, rep(1,(length(row1)-1)))    ,1,length(row1))
    
    rbind <- rbind(row1,row2)
    lista_funcionarios[[i]] <- matrix(rbind,2)
    
  }
  
  assign("lista_funcionarios",lista_funcionarios, envir= where("simulationABMFAST2"))
  
  workers_empregador <- status_emprego
  
  return(workers_empregador)
  
}
#addfunABM(name)

num_workers <- function(i) return(sum( id_empregador == i))

name <- total_funcionarios <- function(lista_funcionarios){
  
  VAR(workers_empregador)
  
  firm_tot_funcionarios <- 1:quantidade_firmas
  
  for(i in 1:quantidade_firmas){
    
    firm_tot_funcionarios[i] <- sum2(lista_funcionarios[[i]][2,])
    
  }
  
  return(firm_tot_funcionarios)
}
addfunABM(name)

name <- function_faster_labor_market <- function(lista_funcionarios,firm_labor_demand,workers_empregador,firmk_labor_demand,workers_salario,firm_wage_ref){
  
  wages_workers <- LAG(workers_salario,1)
  id_empregador <- VAR(workers_empregador)
  
  firms_labor_demand <- c(VAR(firm_labor_demand),VAR(firmk_labor_demand))
  
  firm_wage_ref <- VAR(firm_wage_ref)
  firmk_wage_ref <- VAR(firmk_wage_ref)
  
  #M <- matrix(1:(quantidade_firmas+1),1)
  #tot_workers <- apply(M,MARGIN = 2,num_workers) # Total de trabalhadores empregados
  
  tot_workers <- rep(0,(quantidade_firmas+1))
  
  for(i in 1:(quantidade_firmas+1)){
    
    tot_workers[i] <- sum2(id_empregador == i)
    
  }
  
  empregados <- sum2(tot_workers)
  
  liq_demand <- firms_labor_demand - tot_workers # Demanda líquda de trabalho
  
  ### Identificação das firmas que vão contratar e demditir 
  
  id_firms_demissao <- which( liq_demand < 0)
  id_firms_admissao <- which( liq_demand > 0)
  
  # Demissões
  
  if(length(id_firms_demissao) > 0){
    
    for(i in 1:length(id_firms_demissao)){
      
      index <- id_firms_demissao[i]
      
      id_demitidos <- rev(lista_funcionarios[[index]][1,])[1:(-liq_demand[index])]
      wages_workers[id_demitidos] <- 0
      id_empregador[id_demitidos] <- 0
      
    }
  }
  
  id_workers <- 1:quantidade_workers # Vetor id dos workers 
  
  if(length(id_firms_admissao) > 0){ # Ciclo admissão
    
    run <- 1
    
    while(run < 2){
      
      run <- run + 1
      
      ms_applicants <- rep(0,(quantidade_firmas+1))  # MS das aplicações dentre as que procuram trabalho
      
      final_tot_demand <- sum2(liq_demand[liq_demand > 0]) # Demanda total líq das firmas que desejam contratar 
      
      ms_applicants[liq_demand > 0] <- liq_demand[liq_demand > 0]/final_tot_demand # MS das aplicações dentre as que procuram trabalho
      
      aplicam <- (wages_workers[ id_empregador != 0])/mean(wages_workers[ id_empregador != 0]) < runif(sum2(id_empregador != 0),0,1) # Cálculo se empregado aplicada para novo emprego
      
      id_workers_aplicam_cemprego <- id_workers[ (wages_workers != 0) ][ aplicam ] # Aplicantes empregados
      id_workers_desempregados <- id_workers[ (id_empregador == 0) ] # Aplicantes desempregados 
      
      applicants <- c(id_workers_aplicam_cemprego,id_workers_desempregados) # Vetor de aplicações
      
      lenght_applicants <- length(applicants) # Número de aplicantes
      
      selected_firms <- Sample(1:(quantidade_firmas+1),lenght_applicants,replace=TRUE,ms_applicants) # Seleção de firmas para aplicar 
      
      init <- 0 # Ciclo para que a distribuição de funcionários seja de forma que o número de aplicações recebidas == demanda
      
      sum_liq <- sum2(liq_demand > 0)
      
      for( i in 1:sum_liq ){
        
        index <- id_firms_admissao[i]
        
        selected_firms[(init+1):(init+liq_demand[index])] <- index # Coloco a ordem dos aplicantes de acordo a ordem das empresas demandando
        
        init <- init + liq_demand[index]
        
      }
      
      firms_wages_reg <- c(firm_wage_ref,firmk_wage_ref) # Salários das firmas 
      
      
      #workers_salario_desejado <- VAR(workers_salario_desej) # Salários reserva workers 
      
      # Contratações
      
      num_firmas_contrat <- length(id_firms_admissao)
      
      for(i in 1:num_firmas_contrat){
        
        index <- id_firms_admissao[i]
        
        Sample <- which( selected_firms == index  )
        
        #if( any(workers_salario_desej[Sample,1] <=[index]) ){ # Tirei a importância do salário de reserva.
        
        # selecionados <- Sample[ workers_salario_desejado[Sample] <= firms_wages_reg[index] ][1:liq_demand[index]] # Aqui há importância do salário relativo
        
        selecionados <- Sample[1:liq_demand[index]] # Aqui trabalhadores aceitam qualquer salário
        
        selecionados <- selecionados[is.na(selecionados) == FALSE]
        
        id_aplicantes_selecionados <- applicants[selecionados] # Aplicações que foram selecionadas 
        
        
        id_empregador[id_aplicantes_selecionados] <- index
        wages_workers[id_aplicantes_selecionados] <- firms_wages_reg[index]
        
        #}
        
      }
      
      for(i in 1:(quantidade_firmas+1)){
        
        tot_workers[i] <- sum2(id_empregador == i)
        
      }
      
      empregados <- sum2(tot_workers)
      
      liq_demand <- firms_labor_demand - tot_workers # Demanda líquda de trabalho
      
      id_firms_admissao <- which( liq_demand > 0) # Redefinindo firmas participantes do mercado
      
      
    }
  }
  
  # tot_workers_after <- rep(0,(quantidade_firmas+1))
  # 
  # for(i in 1:(quantidade_firmas+1)){
  #   
  #   tot_workers_after[i] <- sum2(id_empregador == i)
  #   
  # }
  # 
  for(i in 1:(quantidade_firmas+1)){
    
    id_workers_empresa_i <- id_workers[id_empregador == i]
    
    row1 <-  matrix(c(0,id_workers_empresa_i),1,length(id_workers_empresa_i)+1)
    row2 <- matrix( c(0, rep(1,(length(row1)-1)))    ,1,length(row1))
    
    rbind <- rbind(row1,row2)
    lista_funcionarios[[i]] <- matrix(rbind,2)
    
  }
  
  assign("lista_funcionarios",lista_funcionarios, envir= where("simulationABMFAST2"))
  
  
  workers_empregador <- id_empregador
  
  
  return(workers_empregador)
}
#addfunABM(name)

name <- function_faster_labor_market_2 <- function(lista_funcionarios,firm_labor_demand,workers_empregador,firmk_labor_demand,workers_salario,firm_wage_ref){
  
  wages_workers <- LAG(workers_salario,1)
  id_empregador <- VAR(workers_empregador)
  
  firm_c_demand <- VAR(firm_labor_demand)
  firm_k_demand <- VAR(firmk_labor_demand)
  
  firms_labor_demand <- c(firm_c_demand,firm_k_demand)
  
  firm_wage_ref <- VAR(firm_wage_ref)
  firmk_wage_ref <- VAR(firmk_wage_ref)
  
  firms_wages_ref <- c(firm_wage_ref,firmk_wage_ref)
  
  #M <- matrix(1:(quantidade_firmas+1),1)
  #tot_workers <- apply(M,MARGIN = 2,num_workers) # Total de trabalhadores empregados
  
  tot_workers <- rep(0,(quantidade_firmas+1))
  
  for(i in 1:(quantidade_firmas+1)){
    
    tot_workers[i] <- sum2(lista_funcionarios[[i]][2,])
    
  }
  
  liq_demand <- firms_labor_demand - tot_workers # Demanda líquda de trabalho
  
  ### Identificação das firmas que vão contratar e demditir 
  
  id_firms_demissao <- which( liq_demand < 0) # Id firmas demitindo
  id_firms_admissao <- which( liq_demand > 0) # Id firmas contratando
  
  
  if( length(id_firms_demissao) != 0 ) list_ids_firms_demis <- rep(list(matrix(0,1,1)),quantidade_firmas+1)
  if( length(id_firms_admissao) != 0 ) list_ids_firms_adms <- rep(list(matrix(0,1,1)),quantidade_firmas+1)
  
  lista_trabalhadoresquesedemitiram <-rep(list(matrix(0,1,1)),quantidade_firmas+1)
  
  # Demissões
  
  if(length(id_firms_demissao) > 0){
    
    for(i in 1:length(id_firms_demissao)){
      
      index <- id_firms_demissao[i]
      
      id_demitidos <- rev(lista_funcionarios[[index]][1,])[1:(-liq_demand[index])]
      wages_workers[id_demitidos] <- 0
      id_empregador[id_demitidos] <- 0
      
      list_ids_firms_demis[[index]] <- id_demitidos
      
    }
  }
  
  id_workers <- 1:quantidade_workers # Vetor id dos workers 
  workers_empregador <- rep(0,quantidade_workers) # vetor id do empregador dos trabalhadores
  
  qtidade_firmas_contratando <- length(id_firms_admissao)
  
  if(qtidade_firmas_contratando > 0){ # Ciclo admissão
    
    ms_applicants <- rep(0,qtidade_firmas_contratando)  # MasketShare das aplicações dentre as que procuram trabalho
    
    final_tot_demand <- sum2(liq_demand[id_firms_admissao]) # Demanda total líq das firmas que desejam contratar 
    
    ms_applicants <- rnorm(qtidade_firmas_contratando,1,sd_labor) *liq_demand[id_firms_admissao]/final_tot_demand # MasketShare das aplicações dentre as que procuram trabalho
    
    ms_applicants <- ms_applicants/sum2(ms_applicants)
    
    aplicam <- (wages_workers[ id_empregador != 0])/mean(wages_workers[ id_empregador != 0]) < runif(sum2(id_empregador != 0),0,1) # Cálculo se empregado aplicada para novo emprego
    
    id_workers_aplicam_cemprego <- id_workers[ (wages_workers != 0) ][ aplicam ] # Aplicantes empregados
    id_workers_desempregados <- id_workers[ (id_empregador == 0) ] # Aplicantes desempregados 
    
    applicants <- c(id_workers_aplicam_cemprego,id_workers_desempregados) # Vetor de aplicações
    applicants <- Sample(applicants,length(applicants),replace=FALSE)
    
    lenght_applicants <- length(applicants) # Número de aplicantes
    
    qtidade_aplicações <- round( lenght_applicants * ms_applicants ) # Quantidade de aplicações que recebem
    
    pos <- c(1,cumsum(qtidade_aplicações))
    
    ####
    
    workers_salario_target <- VAR(workers_salario_desej)
    
    for(i in 1:qtidade_firmas_contratando){
      
      applicantes_firm <- applicants[ pos[i]:pos[i+1]]
      
      applicantes_firm <- applicantes_firm[ workers_salario_target[ applicantes_firm ] < firms_wages_ref[ id_firms_admissao[i] ] ] 
      
      selecionados <- applicantes_firm[1:liq_demand[id_firms_admissao[i]]] # Aplicações que foram selecionadas 
      
      selecionados <- selecionados[ is.na(selecionados) == FALSE]
      
      if(any(selecionados %in% id_workers_aplicam_cemprego) == TRUE){
        
        for(z in 1:length(selecionados)){
          if( id_empregador[ selecionados[z] ] != 0 ){
            
            lista_trabalhadoresquesedemitiram[[ id_empregador[ selecionados[z] ] ]] <- append(lista_trabalhadoresquesedemitiram[[ id_empregador[ selecionados[z] ] ]],selecionados[z] )
            
          }
        }
      }
      
      id_empregador[selecionados] <- id_firms_admissao[i]
      wages_workers[selecionados] <- firms_wages_ref[id_firms_admissao[i]]
      list_ids_firms_adms[[ id_firms_admissao[i] ]] <- selecionados
      
    }
    
    
  }
  
  for(i in 1:(quantidade_firmas+1)){
    
    id_workers_empresa_i <- c(lista_funcionarios[[i]][1,])
    id_workers_empresa_i <- id_workers_empresa_i[ (id_workers_empresa_i %in% c( lista_trabalhadoresquesedemitiram[[i]]) ) == FALSE ]
    
    if( any( i == id_firms_admissao ) ){
      
      id_workers_empresa_i <- c( id_workers_empresa_i , list_ids_firms_adms[[i]] )
      
    }
    
    if(  any( i == id_firms_demissao ) ){
      
      id_workers_empresa_i <- id_workers_empresa_i[ ( id_workers_empresa_i  %in% list_ids_firms_demis[[i]] ) == FALSE ] 
      
    }
    
    workers_empregador[id_workers_empresa_i] <- i
    
    row1 <-  matrix(c(0,id_workers_empresa_i),1,length(id_workers_empresa_i)+1)
    row2 <- matrix( c(0, rep(1,(length(row1)-1)))    ,1,length(row1))
    
    rbind <- rbind(row1,row2)
    lista_funcionarios[[i]] <- matrix(rbind,2)
    
  }
  
  assign("lista_funcionarios",lista_funcionarios, envir= where("simulationABMFAST2"))
  
  
  return(workers_empregador)
}
#addfunABM(name)

name <- function_faster_labor_market_2 <- function(lista_funcionarios,firm_labor_demand,workers_empregador,firmk_labor_demand,workers_salario,firm_wage_ref){
  
  wages_workers <- LAG(workers_salario,1)
  id_empregador <- VAR(workers_empregador)
  
  firm_c_demand <- VAR(firm_labor_demand)
  firm_k_demand <- VAR(firmk_labor_demand)
  
  firms_labor_demand <- c(firm_c_demand,firm_k_demand)
  
  firm_wage_ref <- VAR(firm_wage_ref)
  firmk_wage_ref <- VAR(firmk_wage_ref)
  
  firms_wages_ref <- c(firm_wage_ref,firmk_wage_ref)
  
  
  #M <- matrix(1:(quantidade_firmas+1),1)
  #tot_workers <- apply(M,MARGIN = 2,num_workers) # Total de trabalhadores empregados
  
  tot_workers <- rep(0,(quantidade_firmas+1))
  
  for(i in 1:(quantidade_firmas+1)){
    
    tot_workers[i] <- sum2(lista_funcionarios[[i]][2,])
    
  }
  
  liq_demand <- firms_labor_demand - tot_workers # Demanda líquda de trabalho
  
  ### Identificação das firmas que vão contratar e demditir 
  
  id_firms_demissao <- which( liq_demand < 0) # Id firmas demitindo
  id_firms_admissao <- which( liq_demand > 0) # Id firmas contratando
  
  
  if( length(id_firms_demissao) != 0 ) list_ids_firms_demis <- rep(list(matrix(0,1,1)),quantidade_firmas+1)
  if( length(id_firms_admissao) != 0 ) list_ids_firms_adms <- rep(list(matrix(0,1,1)),quantidade_firmas+1)
  
  lista_trabalhadoresquesedemitiram <-rep(list(matrix(0,1,1)),quantidade_firmas+1)
  
  # Demissões
  
  if(length(id_firms_demissao) > 0){ # Ciclo de demissões
    
    for(i in 1:length(id_firms_demissao)){ # Para cada FIRMA
      
      index <- id_firms_demissao[i] # Index da firma que está demitindo
      
      id_demitidos <- rev(lista_funcionarios[[index]][1,])[1:(-liq_demand[index])] # Demite os funcionários por onde de chegada
      wages_workers[id_demitidos] <- 0 # Muda status
      id_empregador[id_demitidos] <- 0 # Muda status
      
      list_ids_firms_demis[[index]] <- id_demitidos # Salva as informações dos funcionários que devem sair da lista 
      
    }
  }
  
  id_workers <- 1:quantidade_workers # Vetor id dos workers 
  workers_empregador <- rep(0,quantidade_workers) # vetor id do empregador dos trabalhadores
  
  qtidade_firmas_contratando <- length(id_firms_admissao)
  
  if(qtidade_firmas_contratando > 0){ # Ciclo admissão
    
    ms_applicants <- rep(0,qtidade_firmas_contratando)  # MasketShare das aplicações dentre as que procuram trabalho
    
    final_tot_demand <- sum2(liq_demand[id_firms_admissao]) # Demanda total líq das firmas que desejam contratar 
    
    
    #ms_applicants <- rnorm(qtidade_firmas_contratando,1,sd_labor) * liq_demand[id_firms_admissao]/final_tot_demand # MasketShare das aplicações dentre as que procuram trabalho
    
    #ms_applicants <- ms_applicants/sum2(ms_applicants) # Normalizo
    
    ms_applicants <- exp(3 * firms_wages_ref[id_firms_admissao]/mean(firms_wages_ref[id_firms_admissao]))/
      sum(exp(3 * firms_wages_ref[id_firms_admissao]/mean(firms_wages_ref[id_firms_admissao])))
    
    #
    
    workers_salario_target <- VAR(workers_salario_desej) # Salário desejado. 
    
    aplicam <- (wages_workers[ id_empregador != 0])/mean(wages_workers[ id_empregador != 0]) < 1 * runif(sum2(id_empregador != 0),0,0.6) # Cálculo se empregado aplicada para novo emprego
    
    aplicam <- (wages_workers[ id_empregador != 0])/mean(firms_wages_ref) < 1 * runif(sum2(id_empregador != 0),0,0.9)
    
    
    id_workers_aplicam_cemprego <- id_workers[ (wages_workers != 0) ][ aplicam ] # Aplicantes empregados
    id_workers_desempregados <- id_workers[ (id_empregador == 0) ] # Aplicantes desempregados 
    
    applicants <- c(id_workers_aplicam_cemprego,id_workers_desempregados) # Vetor de aplicações
    applicants <- Sample(applicants,length(applicants),replace=FALSE)
    
    lenght_applicants <- length(applicants) # Número de aplicantes
    
    #qtidade_aplicações <- round( lenght_applicants * ms_applicants ) # Quantidade de aplicações que recebem
    
    # Ciclo para distribuir as aplicações entre as firmas:
    
    qtidade_aplicações <- rmultinom(1,lenght_applicants,prob=ms_applicants) # Distribuição de número de trabalhadores 
    
    pos <- cumsum(qtidade_aplicações) # Acumulado
    
    aplications_list <- rep(list(0),qtidade_firmas_contratando) # Lista de aplicações
    
    if(qtidade_aplicações[1] >0 ) aplications_list[[1]] <- applicants[ 1:pos[1] ] # Para a primeira firma 
    
    if(qtidade_firmas_contratando > 1 ){
      for(i in 2:qtidade_firmas_contratando){ # Para as demais
        
        if(qtidade_aplicações[i] > 1){
          
          aplications_list[[i]] <- applicants[ (pos[i-1]+1):pos[i] ]
          
        }
      }
    }
    
    ####
    
    for(i in 1:qtidade_firmas_contratando){ # Ciclo de contratação
      
      if(any(aplications_list[[i]] != 0)){ # Se houver aplicações recebidas 
        
        applicantes_firm <- aplications_list[[i]] # Aplications 
        
        applicantes_firm <- applicantes_firm[ workers_salario_target[ applicantes_firm ] < firms_wages_ref[ id_firms_admissao[i] ] ] # Se o salário dos aplicantes for menor do que o salário de referência da firma
        
        selecionados <- applicantes_firm[1:liq_demand[id_firms_admissao[i]]] # Aplicações que foram selecionadas 
        
        selecionados <- selecionados[ is.na(selecionados) == FALSE] # Tira eventuais NAs
        
        if(any(selecionados %in% id_workers_aplicam_cemprego) == TRUE){ # Se o trabalhador selecioado estava empregado, preciso adicioná-lo a lista de demitidos
          
          for(z in 1:length(selecionados)){
            if( id_empregador[ selecionados[z] ] != 0 ){
              
              lista_trabalhadoresquesedemitiram[[ id_empregador[ selecionados[z] ] ]] <- append(lista_trabalhadoresquesedemitiram[[ id_empregador[ selecionados[z] ] ]],selecionados[z] )
              
            }
          }
        }
        
        id_empregador[selecionados] <- id_firms_admissao[i]
        wages_workers[selecionados] <- firms_wages_ref[id_firms_admissao[i]]
        list_ids_firms_adms[[ id_firms_admissao[i] ]] <- selecionados
        
      }
    }
  }
  
  # Para criar a lista de funcionários: 
  for(i in 1:(quantidade_firmas+1)){
    
    id_workers_empresa_i <- c(lista_funcionarios[[i]][1,]) # Pego a lista como está 
    id_workers_empresa_i <- id_workers_empresa_i[ (id_workers_empresa_i %in% c( lista_trabalhadoresquesedemitiram[[i]]) ) == FALSE ] # Tiro eventuais demitidos
    
    if( any( i == id_firms_admissao ) ){ # 
      
      id_workers_empresa_i <- c( id_workers_empresa_i , list_ids_firms_adms[[i]] )
      
    }
    
    if(  any( i == id_firms_demissao ) ){
      
      id_workers_empresa_i <- id_workers_empresa_i[ ( id_workers_empresa_i  %in% list_ids_firms_demis[[i]] ) == FALSE ] 
      
    }
    
    workers_empregador[id_workers_empresa_i] <- i
    
    row1 <-  matrix(c(0,id_workers_empresa_i),1,length(id_workers_empresa_i)+1)
    row2 <- matrix( c(0, rep(1,(length(row1)-1)))    ,1,length(row1))
    
    rbind <- rbind(row1,row2)
    lista_funcionarios[[i]] <- matrix(rbind,2)
    
  }
  
  assign("lista_funcionarios",lista_funcionarios, envir= where("simulationABMFAST2"))
  
  
  return(workers_empregador)
}
addfunABM(name)

# Atualizar market-share

name <- atualizar_competiviness <- function(firm_quality,firm_price,firm_vendas_nr){
  
  # Load de funçóes
  
  delay <- LAG(firm_vendas_nr,1)
  vendas <- LAG(firm_vendas,1)
  
  lag_firm_E <- LAG(firm_E,1)
  
  vendas[ vendas < 2] <- 1
  
  price <- LAG(firm_price,1)
  price_2 <- LAG(firm_price,2)
  agg_price <- sum2( LAG(firm_price,1) * LAG(firm_ms,1) )
  agg_price <- mean(LAG(firm_price,1) )
  
  quality <- LAG(firm_quality,1)
  quality_2 <- LAG(firm_quality,2)
  agg_quality <- sum2( LAG(firm_quality,1) * LAG(firm_ms,1))
  agg_quality <- mean(LAG(firm_quality,1))
  
  
  ms <- LAG(firm_ms,1)
  agg_ms <- mean(ms)
  
  # Função
  
  fix.effect <- ifelse( round( price/price_2 , 6 ) != 1 , effect.pricefix.mk , 0 )
  
  firm_E <- ( (quality/agg_quality)^effect.qual.mk )/( (price/agg_price)^effect.price.mk * (delay/vendas + 1)^effect.delay.mk *
                                                         ( 1 + sqrt( ( price/price_2 - 1 )^2 ) )^effect.pricevar.mk * ( 1 + fix.effect ) )

  firm_E <- firm_E/max(firm_E)

  
  return(firm_E)
  
}
addfunABM(name)

name <- atualizar_market_share <- function(firm_E,firm_ms){
  
  # Load de funçóes
  
  firm_E <- LAG(firm_E,1)
  
  #firm_E <- firm_E/sum2(firm_E)
  
  ms <- LAG(firm_ms,1)
  average_firm_E <- sum2(firm_E * ms)
  
  # Função
  
  #firm_ms <- ( 1 + var.change.ms * ( firm_E / average_firm_E - 1 ) ) * ms
  
  #firm_ms <- ms * ( 1 - var.change.ms) + var.change.ms * firm_E/average_firm_E
  
  # firm_ms <- ms * ( 1 - var.change.ms) + var.change.ms * firm_E/sum2(firm_E)  # 
  
  #firm_ms <- firm_ms/sum2(firm_ms) # Normalização para dar 1 o ms
  
  firm_ms <- ms * ( 1 - var.change.ms) + var.change.ms * exp(2* firm_E)/sum2( exp(2 * firm_E) )
  
  firm_ms[ firm_ms < 0] <- 0
  
  firm_ms <- firm_ms/sum(firm_ms)
  
  return(firm_ms)
  
}
addfunABM(name)

name <- atualizar_market_share <- function(firm_vendas){
  
  # Load de funçóes
  
  maxvendas <- VAR(firm_producao) + LAG(firm_estoque,1)
  
  agg_demand_disp <- VAR(agg_nom_demand)
  
  price <- VAR(firm_price)
  
  quality <- VAR(firm_quality)
  
  # Função
  
  supply_nom <- VAR(firm_price) * maxvendas
  
  agg_supply_nom <- sum2(VAR(firm_price) * maxvendas)
  
  #X <- Sample(c(1:quantidade_firmas), quantidade_firmas, replace = FALSE, prob = ( VAR(firm_quality) / sum2(VAR(firm_quality))) )
  
  firms_current_ms <- effect.qual.mk * (quality - mean(quality) )/mean(quality)  -   effect.price.mk *(price - mean(price))/mean(price)+ rep(1/quantidade_firmas ,quantidade_firmas)
  
  penalty_nr <- ifelse(is.nan(LAG(firm_vendas_nr,1)/LAG(firm_vendas,1)) | is.infinite(LAG(firm_vendas_nr,1)/LAG(firm_vendas,1)),0,LAG(firm_vendas_nr,1)/LAG(firm_vendas,1))
  
  firm_market_share_pond <- ( (1 - market.inertia) * ( firms_current_ms )  + market.inertia * LAG(firm_ms,1) ) * ( 1 - 0.1 *penalty_nr*LAG(firm_ms,1) )
  
  firm_market_share_pond[firm_market_share_pond <0] <- 0
  
  firm_ms <- firm_market_share_pond/sum2(firm_market_share_pond)
  
  return(firm_ms)
  
}
#addfunABM(name)

# Investimentos em pesquisa e máquinas
# Investimento em qualidade

name <- atualizar_RD_invest <- function(firm_profit,firm_ms,firm_quality){
  
  firm_ms <- LAG(firm_ms,1)
  l2_quality <- LAG(firm_quality,2)
  l1_quality <- LAG(firm_quality,1)
  
  lag_firmRD <- LAG(firm_RD_invest,1)
  
  lag_receita <- LAG(firm_receita,1)
  firm_leverage <- LAG(firm_leverage,1)
  
  total_investido <- ifelse( lag_receita  < 0  , 0 , porc_gasto_PD*lag_receita )
  
  total_investido <- ifelse( firm_ms == 0 , LAG(firm_cash,1)/20 , total_investido)
  
  #percent_invest <- (1 - (firm_ms - 1/quantidade_firmas))^2
  
  percent_invest <- (1/(exp(firm_ms)^10))
  
  #percent_invest <- 1
  
  firm_RD_invest <-  percent_invest * total_investido #* (1 - firm_leverage)
  firm_RD_invest[ firm_RD_invest <0 ] <- 0
  
  return(firm_RD_invest)
}
addfunABM(name)

name <- atualizar_RD_invest_innov <- function(firm_profit,firm_ms,firm_quality){
  
  # Função para definir quanto é investido em inovação e quanto em imitação
  
  min.qual <- rep(min(LAG(firm_quality,1)),quantidade_firmas)
  max.qual <- rep(max(LAG(firm_quality,1)),quantidade_firmas)
  
  if(min.qual[1] == max.qual[1]){
    
    firm_RD_innov <- rep(1,quantidade_firmas) * VAR(firm_RD_invest)
    
  }else{
    
    firm_RD_innov <- VAR(firm_RD_invest) * (LAG(firm_quality,1) - min.qual)/(max.qual - min.qual)
    
    firm_RD_innov <- VAR(firm_RD_invest) * 0.5
    
  }
  
  return(firm_RD_innov)
}
addfunABM(name)

name <- atualizar_RD_invest_immitate <- function(firm_profit,firm_ms,firm_quality){
  
  firm_RD_immitate <- VAR(firm_RD_invest) - VAR(firm_RD_innov)
  
  return(firm_RD_immitate)
}
addfunABM(name)

name <- atualizar_RD_invest_acum <- function(firm_RD_invest,firm_RD_invest_acum,firm_quality){
  
  preco_index <- LAG(agg_index_prices,1)
  
  firm_RD_invest_acum <- LAG(firm_RD_invest,1)/preco_index + LAG(firm_RD_invest_acum,1)
  
  firm_RD_invest_acum <- ifelse( LAG(firm_quality,1) > LAG(firm_quality,2) | LAG(firm_leverage,1) > 1 , 0, firm_RD_invest_acum)
  
  return(firm_RD_invest_acum)
}
#addfunABM(name)

name <- atualizar_RD_invest_acum_immitate <- function(firm_RD_invest,firm_RD_invest_acum,firm_quality){
  
  # Ivnestimento em imitação acumulado em valores reais
  
  preco_index <- LAG(agg_index_prices,1)
  
  firm_RD_immitate_acum <- LAG(firm_RD_immitate,1)/preco_index + LAG(firm_RD_immitate_acum,1)
  
  firm_RD_immitate_acum <- ifelse( LAG(firm_quality,1) > LAG(firm_quality,2) | LAG(firm_leverage,1) > 1 | LAG(firm_ms,1) == 0 , 0, firm_RD_immitate_acum)
  
  return(firm_RD_immitate_acum)
}
addfunABM(name)

name <- atualizar_RD_invest_acum_innov <- function(firm_RD_invest,firm_RD_invest_acum,firm_quality){
  
  # Ivnestimento em imitação acumulado em valores reais
  
  preco_index <- LAG(agg_index_prices,1)
  
  firm_RD_innov_acum <- LAG(firm_RD_innov,1)/preco_index + LAG(firm_RD_innov_acum,1)
  
  firm_RD_innov_acum <- ifelse( LAG(firm_quality,1) > LAG(firm_quality,2) | LAG(firm_leverage,1) > 1 | LAG(firm_ms,1) == 0 , 0, firm_RD_innov_acum)
  
  return(firm_RD_innov_acum)
}
addfunABM(name)

# Investimento em máquinas 

# Processo para determinar estoque de capital com depreciação dentro

name <- contagem_maquinas_novas <- function(lista_maquinas,firmk_production,firm_machines_dp){
  
  VAR(firmk_production) # Marcador de que deve rodar primeiro a produção dass máquinas novas
  #VAR(firm_machines_dp) # Marcador para rodar a deprecição das máquinas 
  
  firm_machines <- LAG(firm_machines,1) # Maq ANTES
  firm_machines_pos <- firm_machines # Maq DEPOIS
  
  for(i in 1:quantidade_firmas){
    
    lista_maquinas[[i ]][1,] <- lista_maquinas[[i]][1,] - 1
    
    m <- matrix(lista_maquinas[[i]],3)
    
    lista_maquinas[[i ]] <- matrix( m[ , m[1,] > 0] , 3) # Selecção de todas as colunas em que, na linha 1, o valor é maior do que 0. É a depreciação
    
    if( length( lista_maquinas[[i ]][1,] ) < 2 ){ # Se a esmpresa tiver um estoque menor do que uma máquina, mantem ela.
      
      lista_maquinas[[i]] <- matrix( c(temp_depreci,prod_machine,rel.k.l) , 3, 1)
      
    }
    
    firm_machines_pos[i] <- length( lista_maquinas[[i]][1,] )
    
  }
  
  firm_new_machines <- VAR(firm_new_machines)
  
  firm_machines <- firm_new_machines + firm_machines_pos
  
  for(i in 1:quantidade_firmas){
    if( firm_new_machines[i] > 0   ){
      
      deprec_time <- round(runif(firm_new_machines[i],temp_depreci - min.deprec,temp_depreci+4))
      
      new_machines <- matrix(  rep( c(prod_machine,rel.k.l) , firm_new_machines[i] )  , 2 , firm_new_machines[i] ) 
      
      new_machines <- matrix(rbind(deprec_time,new_machines),3,)
      
      #new_machines <- matrix( rep( c(temp_depreci,prod_machine,rel.k.l) , firm_new_machines[i] ) , 3 , firm_new_machines[i] ) # Número de máquinas novas igual do pedido
      
      lista_maquinas[[ i ]] <- cbind( lista_maquinas[[ i ]] , new_machines )  # Adiciona na lista de máquinas da firma
      
    } 
  }
  
  # manda lista de máquinas para o ambiente global
  assign("lista_maquinas",lista_maquinas,envir = where("simulationABMFAST2"))
  
  return(firm_machines)
}
addfunABM(name)

name <- contagem_maquinas_novas <- function(lista_maquinas,firmk_production,firm_machines_dp){
  
  VAR(firmk_production) # Marcador de que deve rodar primeiro a produção dass máquinas novas
  VAR(firm_machines_dp) # Marcador para rodar a deprecição das máquinas 
  
  firm_machines <- LAG(firm_machines,1)
  
  for(i in 1:quantidade_firmas){
    
    firm_machines[i]  <- length(lista_maquinas[[i]][1,])
    
  }
  
  return(firm_machines)
}
#addfunABM(name)

name <- investment_machines <- function(firm_machines,firm_machines_dp){
  
  # dif_machines <- VAR(firm_machines) - LAG(firm_machines,1) + VAR(firm_machines_dp) # Diferença de máquinas + depreciadas 
  # 
  # firm_investimento <- rep(0,quantidade_firmas)
  # 
  # firm_investimento[ dif_machines > 0] <- dif_machines[ dif_machines > 0] * VAR(firmk_price)
  
  firm_investimento <- VAR(firm_new_machines) * VAR(firmk_price)
  
  return(firm_investimento)
}
addfunABM(name)

name <- atualizar_machine_invest_ANTIGO <- function(firm_expdem,firm_machine_order,firm_machines, firm_receita_op , firm_fb){
  
  # Variáveis 
  firm_machines <- VAR(firm_machines) # Marcador para ele contar antes todas as máquinas e as já depreciadas
  
  machine_demand <- ceiling( ( VAR(firm_expdem)*(1+firm_estoque_desejado_perc)/(grau_utilizao_desejado*prod_machine ))) # demanda por máquinas
  
  ordemfirms <- Sample(1:quantidade_firmas)
  
  list <- lista_orders_machines[[1]]
  
  firm_machine_order <- VAR(firm_machine_order)
  
  firm_leverage <- LAG(firm_leverage,1)
  
  # Função
  
  for(index in 1:quantidade_firmas){
    
    i <- ordemfirms[index]
    
    demand <- machine_demand[i] - firm_machines[i] - firm_machine_order[i]
    
    
    if(demand > 0){ # Se manda >0, entra um pedido novo na lista
      if( any( list[1,] == i) == FALSE ){ # Se não houver pedidos da empresa ja na lista
        if( firm_leverage[i] < 1 ){
          
          demand <- ceiling( ( 1 - firm_leverage[i]) * demand ) # Pondera a demanda pelo endividamento. Ceiling para ter valor sem vírgula
          
          list <- cbind(list , matrix(c(i,demand),2,1))
          
          firm_machine_order[i] <- demand
          
        }
      }
    }
  }
  
  list <- list(list)
  
  assign("lista_orders_machines", list, envir= where("simulationABMFAST2"))
  
  #firm_machine_order <- firm_machine_order
  
  return(firm_machine_order)
}
#addfunABM(name)

name <- atualizar_machine_invest <- function(firm_expdem,firm_machine_order,firm_machines, firm_receita_op , firm_fb, firm_machines_dp){
  
  # marcador para rodar antes as depreciações
  VAR(firm_machines_dp) # Diferença de máquinas + depreciadas 
  
  # Variáveis 
  firm_machines <- LAG(firm_machines,1) - VAR(firm_machines_dp) # Marcador para ele contar antes todas as máquinas e as já depreciadas
  
  machine_demand <- ceiling( ( VAR(firm_expdem)/(grau_utilizao_desejado*prod_machine )) ) # demanda por máquinas
  
  ordemfirms <- Sample(1:quantidade_firmas)
  
  list <- matrix(c(0,0),2,1)
  
  firm_machine_order <- rep(0,quantidade_firmas) # quantidade de máquinas encomendadas é zerada
  
  firm_leverage <- LAG(firm_leverage,1)
  
  # Máquinas que vão depreciar
  
  num_firm_deprec <- 1:quantidade_firmas
  
  for(i in 1:quantidade_firmas){
    
    num_firm_deprec[i] <- length(  lista_maquinas[[i]][1,][ lista_maquinas[[i]][1,] == 2 ] )  # Depois vou precisar avaliar também a produtividade das máquinas
    
  }
  
  
  # Função
  
  for(index in 1:quantidade_firmas){
    
    i <- ordemfirms[index]
    
    demand <- machine_demand[i] - (firm_machines[i] - num_firm_deprec[i]) 
    
    if(demand > 0){ # Se manda >0, entra um pedido novo na lista
      #if( any( list[1,] == i) == FALSE ){ # Se não houver pedidos da empresa ja na lista
      #if( firm_leverage[i] < 1 ){
      
      demand <- ceiling( ( 1 - 0 * firm_leverage[i]) * demand ) # Pondera a demanda pelo endividamento. Ceiling para ter valor sem vírgula
      
      list <- cbind(list , matrix(c(i,demand),2,1))
      
      firm_machine_order[i] <- demand
      
      #}
      #}
    }
  }
  
  list <- list(list)
  
  assign("lista_orders_machines", list, envir= where("simulationABMFAST2"))
  
  #firm_machine_order <- firm_machine_order
  
  return(firm_machine_order)
}
#addfunABM(name)

name <- atualizar_machine_invest_2 <- function(firm_expdem,firm_machine_order,firm_machines, firm_receita_op , firm_fb, firm_machines_dp){
  
  # marcador para rodar antes as depreciações
  VAR(firm_machines) # Diferença de máquinas + depreciadas 
  
  # Variáveis 
  firm_machines <- VAR(firm_machines) # Marcador para ele contar antes todas as máquinas e as já depreciadas
  
  machine_demand <- ceiling( ( VAR(firm_expdem)/(grau_utilizao_desejado*prod_machine )) ) # demanda por máquinas
  
  ordemfirms <- Sample(1:quantidade_firmas)
  
  list <- matrix(c(0,0),2,1)
  
  firm_machine_order <- rep(0,quantidade_firmas) # quantidade de máquinas encomendadas é zerada
  
  firm_leverage <- VAR(firm_leverage)
  firm_leverage_meta <- VAR(firm_leverage_meta)
  
  # Máquinas que vão depreciar
  
  num_firm_deprec <- 1:quantidade_firmas
  
  for(i in 1:quantidade_firmas){ # Cálculo das firmas que irão depreciar
    
    num_firm_deprec[i] <- length(  lista_maquinas[[i]][1,][ lista_maquinas[[i]][1,] == 1 ] )  # Depois vou precisar avaliar também a produtividade das máquinas
    
  }
  
  # Função
  
  
  # finalcial space to buy
  
  machine_possible_buy <- floor((VAR(firm_assets)*( firm_leverage_meta - firm_leverage    ))/VAR(firmk_price))
  
  
  # Limite alavancagem: Receita/FB = 3  -> Receita/3i = Debt 
  # Debt = I + Debt_(t-1)
  # I = Receita/3i - Debt_(t-1)
  
  
  machine_possible_buy <- floor(
    (
      
      (VAR(firm_receita_op) - VAR(firm_RD_invest))/( VAR(firm_interest) * 3) - VAR(firm_debt_stock)
     
     )/VAR(firmk_price)
    )
  
  
  machine_possible_buy_1 <- floor(
    
  (((VAR(firm_price) * VAR(firm_expdem) * (1 - porc_gasto_PD) - VAR(firm_custo_unit) * VAR(firm_expdem) /(1/VAR(firm_coef_tecnico)) )/( VAR(firm_interest) * 3) -
      VAR(firm_debt_stock))/VAR(firmk_price))
  )
  
  
  machine_possible_buy_2 <- floor(( (0.2 + firm_leverage_meta) * VAR(firm_assets) - VAR(firm_debt_stock))/VAR(firmk_price))
  
  machine_possible_buy <- rowMins(matrix(c(machine_possible_buy_1,machine_possible_buy_2),quantidade_firmas,2))

  
  machine_possible_buy[ machine_possible_buy <0] <- 0
  
  demand_given_exp_demand <- machine_demand  - firm_machines + num_firm_deprec
  
  final_demand <- ifelse( machine_possible_buy < demand_given_exp_demand , machine_possible_buy , demand_given_exp_demand   )
  
  for(i in 1:quantidade_firmas){
    
    if(final_demand[i] > 0){ # Se manda >0, entra um pedido novo na lista
      
      list <- cbind(list , matrix(c(i,final_demand[i]),2,1))
      
      firm_machine_order[i] <- final_demand[i]

    }
  }
  
  list <- list(list)
  
  assign("lista_orders_machines", list, envir= where("simulationABMFAST2"))
  
  #firm_machine_order <- firm_machine_order
  
  return(firm_machine_order)
}
addfunABM(name)

# Firms research x quality

name <- atualizar_quality <- function(firm_quality,firm_RD_invest_acum,agg_index_prices){
  
  # Variáveis
  
  lag_quality <- LAG(firm_quality,1)
  
  lag_vendas <- LAG(firm_vendas,1)
  preco_index <- LAG(agg_index_prices,1)
  
  firm_RD_invest <- VAR(firm_RD_invest_acum) # Gasto real com RD
  
  # Funções :
  
  lag <- lag_quality
  
  #mean.default <- (innov_dificculty + rbeta( length(lag_quality),2,2)) * sum2(lag_quality)/length(lag_quality)
  
  #mean.default <- (innov_dificculty + rbeta( length(lag_quality),2,2)) * lag_quality
  
  mean.default <- (
    ( innov_dificculty + rbeta( length(lag_quality),2,2)) + 
      ( innov_imitation * (1 - (lag_quality - min(lag_quality))/( max(lag_quality) - min(lag_quality) + 0.000000000001))) 
  ) * lag_quality
  
  
  firm_qual_nova <- ifelse( mean.default > lag , mean.default , lag)
  
  parameter_bernoulli <- 1 - exp(- par_exp_firm_qual * firm_RD_invest )
  parameter_bernoulli[ parameter_bernoulli <0 ] <-0
  parameter_bernoulli[ parameter_bernoulli >1 ] <-1
  draw_bernoulli <- rbern(quantidade_firmas,parameter_bernoulli)
  
  var_quality <- ifelse( draw_bernoulli > 0 , 1,0) * firm_qual_nova
  
  firm_quality <- ifelse( var_quality >  lag , var_quality , lag  )
  
  qualsequebrar <- Sample(lag_quality,quantidade_firmas,prob=LAG(firm_ms,1))
  
  firm_quality <- ifelse(LAG(firm_leverage,1) > 1 , qualsequebrar , firm_quality) # Se a empresa tiver quebrado no período anterior, a qualidade é a média 
  
  return(firm_quality)
}
#addfunABM(name)

name <- atualizar_quality_2invest <- function(firm_quality,firm_RD_innov_acum,firm_RD_immitate_acum,agg_index_prices){
  
  # Variáveis
  
  lag_quality <- LAG(firm_quality,1)
  
  lag_vendas <- LAG(firm_vendas,1)
  preco_index <- LAG(agg_index_prices,1)
  
  firm_RD_invest_innov_ac <- VAR(firm_RD_innov_acum) # Gasto real com RD innovation
  firm_RD_immitate_acum <- VAR(firm_RD_immitate_acum) # Gasto real com RD immitate
  
  # Funções :
  
  lag <- lag_quality
  
  #mean.default <- (innov_dificculty + rbeta( length(lag_quality),2,2)) * sum2(lag_quality)/length(lag_quality)
  
  #mean.default <- (innov_dificculty + rbeta( length(lag_quality),2,2)) * lag_quality
  
  mean.default_innov <- ( 1 + ( rbeta( length(lag_quality),3,3)*0.6 - 0.3 ) ) * lag_quality # Parâmetros do rbeta vem do K+S
  
  mean.default_immitate <- Sample(lag_quality,quantidade_firmas,replace=T,prob=lag_quality/sum2(lag_quality))
  
  
  for(i in 1:quantidade_firmas){
    
    dist <-  1 - abs(lag_quality[i] / lag_quality - 1)
    
    dist[ dist < 0] <- 0
    
    mean.default_immitate[i] <- Sample(lag_quality,1,replace=T,prob=dist)
    
  }
  
  
  firm_qual_nova_innov <- ifelse( mean.default_innov > lag , mean.default_innov , lag)
  firm_qual_nova_immitate <- ifelse( mean.default_immitate > lag , mean.default_immitate , lag)
  
  # Se investiu 0.1% do PIB, com certeza inova
  
  par_exp_firm_qual_innov <- 1/(0.001*VAR(agg_gdp))
  par_exp_firm_qual_immitate <- 1/(0.001*VAR(agg_gdp))
  
  parameter_bernoulli_innov <- 1 - exp(- par_exp_firm_qual_innov * firm_RD_invest_innov_ac )
  parameter_bernoulli_immitate <- 1 - exp(- par_exp_firm_qual_immitate * firm_RD_immitate_acum )
  
  
  #par_exp_firm_qual_immitate > par_exp_firm_qual_innov
  
  parameter_bernoulli_innov[ parameter_bernoulli_innov <0 ] <-0
  parameter_bernoulli_innov[ parameter_bernoulli_innov >1 ] <-1
  
  parameter_bernoulli_immitate[ parameter_bernoulli_immitate <0 ] <-0
  parameter_bernoulli_immitate[ parameter_bernoulli_immitate >1 ] <-1
  
  # Sorteio de bernoulli
  draw_bernoulli_innov <- rbern(quantidade_firmas,parameter_bernoulli_innov)
  draw_bernoulli_immitate <- rbern(quantidade_firmas,parameter_bernoulli_immitate)
  
  # 
  var_quality_innov <- ifelse( draw_bernoulli_innov == 1 , mean.default_innov,lag) 
  
  var_quality_immitate <- ifelse( draw_bernoulli_immitate == 1 , mean.default_immitate,lag)
  
  #### Seleção da tecnologia mais vantajosa
  
  firm_quality <- matrix(lag_quality,quantidade_firmas,1)
  matrix.tecs <- cbind(var_quality_immitate,var_quality_innov,lag)
  
  firm_quality <- matrix( rowMaxs(matrix.tecs) ,quantidade_firmas,1)
  
  # for(i in 1:length(matrix.tecs[,1])){
  #   
  #   firm_quality[i,] <- max(matrix.tecs[i,])
  #   
  # }
  # 
  
  # Rotina para o caso de empresa quebrada:
  
  firm_quality <- ifelse(LAG(firm_leverage,1) > 1 | LAG(firm_ms,1) == 0 , mean(lag_quality)*0.5 + 0.5*lag_quality , firm_quality) # Se a empresa tiver quebrado no período anterior, a qualidade é a média 
  
  
  return(firm_quality)
}
addfunABM(name)

# Firms wage x price policy 

name <- atualizar_data_reajuste <- function(workers_data_base){
  
  workers_data_base <- matrix(VAR(workers_data_base))
  stats_empregador <- VAR(workers_empregador)
  
  workers_data_base[ stats_empregador == 0 ] <- 0 # Se não estiver empregado
  
  workers_data_base[ stats_empregador != 0 ] <- workers_data_base[ stats_empregador != 0 ] + 1 # Se estiver empregado e período menor do que 4
  
  workers_data_base[ workers_data_base > 4 ] <- 0 # Se período maior do que 4, reinicia
  
  return(workers_data_base)
  
}
#addfunABM(name)

name <- atualizar_data_reajuste <- function(workers_data_base,time_t){
  
  workers_data_base <- matrix(VAR(workers_data_base))
  stats_empregador <- VAR(workers_empregador)
  
  workers_data_base <- workers_data_base + 1
  
  if(VAR(time_t) == 50)  workers_data_base <- round(runif(quantidade_workers,1,5),0)
  
  #if( VAR(time_t)/4 - round(VAR(time_t)/4) == 0 ) reajuste <- 4
  
  workers_data_base[ stats_empregador == 0 ] <- 0 # Se não estiver empregado
  
  workers_data_base[ stats_empregador != 0 & workers_data_base > 4 ] <- 0 # Se estiver empregado e período menor do que 4
  
  
  return(workers_data_base)
  
}
addfunABM(name)

aux_atualizar_salario_desej <- function(i){
  
  workers_salario_desej_change <- LAG(workers_salario_desej,1)[i]
  
  if( VAR(workers_data_base)[i] == 4 ){
    
    workers_salario_desej_change <- (1 + ifelse( VAR(workers_empregador)[i]  == 0 , 0 , runif(1,0,par_var_salario_max))) * LAG(workers_salario_desej,1)[i] 
    
  }
  return(workers_salario_desej_change)
  
}

name <- atualizar_salario_desejado <- function(workers_salario,workers_data_base,agg_inflation){
  
  # Load das variáveis usadas 
  
  infla_passada <- ifelse( LAG(agg_inflation,1) > 0 ,  LAG(agg_inflation,1) , 0)
  
  #data_base <- VAR(workers_data_base)
  
  desemprego <- LAG(agg_unemployment_rate,1)
  
  #stats_emprego_presente <- VAR(workers_empregador)
  
  workers_salario_desej <- LAG(workers_salario_desej,1)
  
  # Funções
  
  
  # Com 6% de desemprego, trabalhadores tem, em média, ganho salarial nulo.
  #runif(tamanho,-(desemprego - 0.06) - par_var_salario_max , -(desemprego - 0.06) + par_var_salario_max)
  
  #tamanho <- length( stats_emprego_presente[ stats_emprego_presente != 0 ] )
  #tamanho_desem <- quantidade_workers - tamanho
  
  #diff.emprego <- LAG(agg_unemployment_rate,1) - LAG(agg_unemployment_rate,5)
  
  #workers_salario_desej[ stats_emprego_presente != 0 ] <- (1 + infla_indexation * infla_passada) * (1 + (1-(desemprego - 0.1))*runif(tamanho,0,par_var_salario_max)) * workers_salario_desej[ stats_emprego_presente != 0 ]
  
  #workers_salario_desej[ stats_emprego_presente != 0 ] <- (1 + infla_indexation * (1 - desemprego) * infla_passada) * (1 + runif(tamanho,-desemprego - par.empreg.var,(1 - desemprego - par.empreg.var))*par_var_salario_max) * workers_salario_desej[ stats_emprego_presente != 0 ]
  
  #workers_salario_desej[ stats_emprego_presente != 0 ] <- (1 + infla_indexation * (1 - desemprego) * infla_passada) * (1 + runif(tamanho,-(desemprego - 0.06) - par_var_salario_max , -(desemprego - 0.06) + par_var_salario_max)) * workers_salario_desej[ stats_emprego_presente != 0 ]
  
  #workers_salario_desej[ stats_emprego_presente == 0 ] <- (1 + infla_indexation * (1 - desemprego) * infla_passada) * (1 - desemprego * runif(tamanho_desem,0,par_var_salario_max)) * workers_salario_desej[ stats_emprego_presente == 0 ]
  
  
  #workers_salario_desej <- (1 + infla_indexation * (1 - desemprego) * infla_passada ) * (1 + runif(quantidade_workers,-(desemprego - 0.06) - par_var_salario_max , -(desemprego - 0.06) + par_var_salario_max) ) * workers_salario_desej
  
  #workers_salario_desej <- (1 + infla_indexation * (1 - desemprego) * infla_passada ) * ( ( 1 - runif(quantidade_workers,desemprego,3*desemprego)) * par_var_salario_max *  + 1) * workers_salario_desej
  
  #workers_salario_desej[ stats_emprego_presente != 0 ] <- (1 + infla_indexation * infla_passada) * (1 + runif(tamanho,-diff.emprego - par.empreg.var,(1 - diff.emprego - par.empreg.var))*par_var_salario_max) * workers_salario_desej[ stats_emprego_presente != 0 ]
  
  #workers_salario_desej[ stats_emprego_presente == 0 ] <- (1 + infla_indexation * infla_passada) * (1 - diff.emprego * runif(tamanho_desem,0,par_var_salario_max)) * workers_salario_desej[ stats_emprego_presente == 0 ]
  
  #workers_salario_desej <- apply(seq,MARGIN = 2,aux_atualizar_salario_desej)
  
  # Atualização de salário desejado após contratação
  
  stats_emprego_presente <- LAG(workers_empregador,1)
  stats_emprego_passado <- LAG(workers_empregador,2)
  lag_workers_salario <- LAG(workers_salario,1)
  
  tamanho <- length(workers_salario_desej[ stats_emprego_presente != 0 ])
  
  # diff_wage_dej_market <- mean(LAG(firm_wage_ref,1))/workers_salario_desej[ stats_emprego_presente != 0 ] - 1
  # 
  # diff_wage_dej_market[ diff_wage_dej_market < 0] <- 0
  
  workers_salario_desej[ stats_emprego_presente != 0 ] <- (1 + infla_indexation * infla_passada ) * workers_salario_desej[ stats_emprego_presente != 0 ]
  
  # Behavioral adjustment parameter - 
  # FOnte: https://www.sciencedirect.com/science/article/pii/S0167268118301070?casa_token=NdEaDNBYUwIAAAAA:l-aoxs-JgRVkGM1_0gEAfylzz7F-_Feh46j7WVVHO2q-pkch6sky8V_IiYjOiEFGb62N-LW82Q
  
  # Funções
  
  # Se houve uma mudança do estado de emprego passado, e não foi de uma empresa para 0, trabalhador aceitou um novo emprego. Salário de referência é atualizado.
  
  workers_salario_desej[ stats_emprego_presente != stats_emprego_passado & stats_emprego_presente != 0 ] <- (1 + infla_indexation * infla_passada )*lag_workers_salario[  stats_emprego_presente != stats_emprego_passado & stats_emprego_presente != 0]
  
  # Se o salário desejado menor do que benefício desemprego, ajustar
  
  workers_salario_desej[ workers_salario_desej < VAR(gov_unem_ben)] <- VAR(gov_unem_ben)
  
  # Retorno
  
  return(workers_salario_desej)
  
}
addfunABM(name)

# Função em que os salários são específicos por trabalhador, data de reajuste depende da data de contratação do trabalhador
name <- atualizar_salarios <- function(workers_empregador,workers_salario,workers_salario_desej){
  
  # Load das variáveis usadas 
  
  data_base <- VAR(workers_data_base)
  
  stats_emprego_presente <- VAR(workers_empregador)
  stats_emprego_passado <- LAG(workers_empregador,1)
  
  stats_emprego_presente[ stats_emprego_presente == 0] <- 202
  stats_emprego_passado[ stats_emprego_passado == 0] <- 202
  
  workers_salario_lag <- LAG(workers_salario ,1)
  
  workers_salario <- LAG(workers_salario ,1)
  
  #workers_salario_desej <- VAR(workers_salario_desej)
  
  firm_wages_ofered <- c(VAR(firm_wage_ref),VAR(firmk_wage_ref),0) # Aumento o vetor de wage offered para considerar empresa produtora de bens de K e trabalhadores que não recebemra propostas
  
  # Funções
  
  workers_salario[ stats_emprego_presente != 202 &  stats_emprego_passado == 202] <- firm_wages_ofered[ stats_emprego_presente ][ stats_emprego_presente != 202 &  stats_emprego_passado == 202 ]
  
  #workers_salario[ data_base == 4] <- firm_wages_ofered[ stats_emprego_presente ][ data_base == 4]
  
  workers_salario[ data_base == 4] <- LAG(workers_salario_desej,1)[ stats_emprego_presente ][ data_base == 4]
  
  workers_salario[ stats_emprego_presente == 202] <- 0
  
  # Funções
  
  id_empregador <- VAR(workers_empregador) # Id do emprgador
  
  id_empregador[ id_empregador == 0] <- 202 # Se o ID do empregador é zero, se torna 202
  
  wage_off <- firm_wages_ofered[id_empregador] # A oferta do valor de salário que os trabalhadores receberam
  
  # Se o trabalhador mudou o status do empregador, e não é zero (está empregado), significa que ele mudou de empresa.
  workers_salario[ stats_emprego_presente != stats_emprego_passado & stats_emprego_presente != 0 ] <-  firm_wages_ofered[id_empregador][ stats_emprego_presente != stats_emprego_passado & stats_emprego_presente != 0 ]
  
  
  return(workers_salario)
  
}
#addfunABM(name)

# Função em que os salários são específicos por trabalhador, data de reajuste depende da data de contratação do trabalhador
name <- atualizar_salarios <- function(workers_empregador,workers_salario,workers_salario_desej){
  
  # Load das variáveis usadas 
  
  data_base <- VAR(workers_data_base)
  
  stats_emprego_presente <- VAR(workers_empregador)
  stats_emprego_passado <- LAG(workers_empregador,1)
  
  workers_salario_lag <- LAG(workers_salario ,1)
  
  workers_salario <- LAG(workers_salario ,1)
  
  firm_wages_ofered <- c(VAR(firm_wage_ref),VAR(firmk_wage_ref))
  
  for(i in 1:(quantidade_firmas+1)){
    
    IDS <- lista_funcionarios[[i]][1,1:length(lista_funcionarios[[i]][1,])]
    
    workers_salario[IDS][ stats_emprego_passado[IDS] == 0 ] <- firm_wages_ofered[i]
    
    workers_salario[IDS][ stats_emprego_passado[IDS] != i ] <- firm_wages_ofered[i]
    
    workers_salario[IDS][ data_base[IDS] == 4] <- ifelse(LAG(workers_salario_desej,1)[IDS][ data_base[IDS] == 4] < firm_wages_ofered[i] , LAG(workers_salario_desej,1)[IDS][ data_base[IDS] == 4],  firm_wages_ofered[i] )
    
    workers_salario[IDS][ data_base[IDS] == 4] <- ifelse( workers_salario[IDS][ data_base[IDS] == 4] < workers_salario_lag[IDS][ data_base[IDS] == 4] ,
                                                          workers_salario_lag[IDS][ data_base[IDS] == 4],  workers_salario[IDS][ data_base[IDS] == 4] )
    
    #workers_salario[IDS][ data_base[IDS] == 4] <- LAG(workers_salario_desej,1)[IDS][ data_base[IDS] == 4]
    
  }
  
  workers_salario[ stats_emprego_presente == 0] <- 0
  
  return(workers_salario)
}
addfunABM(name)

# Função em que o salário é igual para todos dentro de uma firma, data de reajuste é igual para todas as firmas.
name <- atualizar_salarios <- function(workers_empregador,workers_salario,workers_salario_desej){
  
  # Load das variáveis usadas 
  
  # Funções
  
  firm_wages_ofered <- c(VAR(firm_wage_ref),VAR(firmk_wage_ref),0) # Aumento o vetor de wage offered para considerar empresa produtora de bens de K e trabalhadores que não recebemra propostas
  
  id_empregador <- VAR(workers_empregador) # Id do emprgador
  
  id_empregador[ id_empregador == 0] <- 202 # Se o ID do empregador é zero, se torna 202
  
  wage_off <- firm_wages_ofered[id_empregador] # A oferta do valor de salário que os trabalhadores receberam
  
  workers_salario <- wage_off
  
  return(workers_salario)
}
#addfunABM(name)

name <- atualizar_salarios <- function(workers_empregador,workers_salario,workers_salario_desej){
  
  # Load das variáveis usadas 
  
  data_base <- VAR(workers_data_base)
  
  stats_emprego_presente <- VAR(workers_empregador)
  stats_emprego_passado <- LAG(workers_empregador,1)
  
  workers_salario_lag <- LAG(workers_salario ,1)
  
  workers_salario <- LAG(workers_salario ,1)
  
  workers_salario_desej <- VAR(workers_salario_desej)
  
  #inicial_wage <- VAR(workers_empregador)
  
  # Funções
  
  
  workers_salario[ stats_emprego_presente != 0 &  stats_emprego_passado == 0] <- workers_salario_desej[ stats_emprego_presente != 0 &  stats_emprego_passado == 0]
  
  workers_salario[ data_base == 4] <- workers_salario_desej[ data_base == 4]
  
  workers_salario[ stats_emprego_presente == 0] <- 0
  
  return(workers_salario)
  
}
#addfunABM(name)

aux_custo_firm <- function(lista_funcionarios){
  
  resultado <- sum2( VAR(workers_salario)[ lista_funcionarios[1,] ] ) 
  
  return(resultado)
}

name <- atualizar_custo <- function(firm_custo,workers_salario, lista_funcionarios){
  
  # Load de variáveis
  
  workers_salario <- VAR(workers_salario)
  workers_empregador <- VAR(workers_empregador)
  firm_custo <- 1:quantidade_firmas
  
  # Função
  
  for(i in 1:quantidade_firmas){
    
    id <- lista_funcionarios[[i]][1,]
    
    firm_custo[i] <- sum2( workers_salario[ id] )
    
  }
  
  firm_custo[ firm_custo == 0] <- 1
  
  return(firm_custo)
  
}
addfunABM(name)

name <- atualizar_custo_unitario <- function(firm_custo,workers_salario, lista_funcionarios){
  
  # Load de variáveis
  
  VAR(workers_empregador) # Para rodar primeiro o mercado de trabalho
  
  #workers_empregador <- VAR(workers_empregador)
  #firm_custo_unit <- 1:quantidade_firmas
  
  # Função
  # 
  # for(i in 1:quantidade_firmas){
  #   
  #   firm_custo_unit[i] <- ifelse(firm_custo[i] <= 1 , firm_custo[i], firm_custo[i]/sum2(lista_funcionarios[[i]][2,]) )
  #   # Custo por funcionário!
  #   
  # }
  
  firm_custo_unit <- VAR(firm_custo)/VAR(firm_tot_funcionarios)
  
  firm_custo_unit[ VAR(firm_custo) < 1.1] <- mean(firm_custo_unit[ VAR(firm_custo) > 1] )
  
  firm_custo_unit[is.nan(firm_custo_unit)] <- 1
  
  return(firm_custo_unit)
  
}
addfunABM(name)


name <- atualizar_mk_desej_util <- function(firm_grau_utilizacao,firm_par.adj.mk_util){ # Mudança no mark-up móvel
  
  # Load de variáveis
  
  firm_mkup.movel <- LAG(firm_mkup.movel,1)
  firm_grau_utilizacao <- LAG(firm_grau_utilizacao,1)
  firm_par.adj.mk_util <- VAR(firm_par.adj.mk_util)
  
  firm_capacidade_producao <- VAR(firm_capacidade_producao)
  
  # Função
  
  if(any(firm_grau_utilizacao > 0.85)){
    firm_mkup.movel[firm_grau_utilizacao > 0.85 & firm_capacidade_producao > 5] <- ( 1 + firm_par.adj.mk_util[firm_grau_utilizacao > 0.85 & firm_capacidade_producao > 5] ) * firm_mkup.movel[firm_grau_utilizacao > 0.85 & firm_capacidade_producao > 5]
  }
  if(any(firm_grau_utilizacao < 0.75)){
    firm_mkup.movel[firm_grau_utilizacao < 0.75 & firm_capacidade_producao > 5] <- ( 1 - firm_par.adj.mk_util[firm_grau_utilizacao < 0.75 & firm_capacidade_producao > 5] ) * firm_mkup.movel[firm_grau_utilizacao < 0.75 & firm_capacidade_producao > 5]
  }
  
  return(firm_mkup.movel)
  
}
addfunABM(name)

name <- atualizar_mk_desej_mkshare <- function(firm_grau_utilizacao,firm_par.adj.mk_util){ # Mudança no mark-up móvel
  
  # Load de variáveis
  
  
  lag.2.ms <- ifelse( LAG(firm_ms,2) < 0.00001 , 0.00001, LAG(firm_ms,2))
  
  lag.1.ms <- LAG(firm_ms,1)
  lag.1.ms <- ifelse( lag.1.ms < 0.00001 ,0.0000075, lag.1.ms)
  
  
  relative_ms <- lag.1.ms/lag.2.ms - 1 
  
  # Função
  
  
  firm_mkup.movel.ms <- (1 + VAR(firm_par.adj.markup)* relative_ms) * LAG(firm_mkup.movel.ms,1)
  
  firm_mkup.movel.ms <- ifelse( firm_mkup.movel.ms < 0.05 , 0.05, firm_mkup.movel.ms)
  
  
  return(firm_mkup.movel.ms)
  
}
addfunABM(name)


name <- atualizar_price_dej <- function(firm_price_dej,firm_custo_unit, firm_labor_demand,firm_price,firm_price_rule,firm_capacidade_producao, lista_funcionarios , agg_inflation){
  
  # Load de variáveis
  
  #firm_custo_unit <- VAR(firm_wage_ref)*VAR(firm_coef_tecn)/0.8
  
  # L = Coef Y'2
  # wL = w Coef Y/u
  
  #firm_custo_unit[ is.infinite(firm_custo_unit) | is.nan(firm_custo_unit) ] <- mean( firm_custo_unit[ ( is.infinite(firm_custo_unit) | is.nan(firm_custo_unit))== FALSE] )
  
  # Preços desejado dado o mark-up e o custo unitário
  
  price_desejado <- ( 1 + VAR(firm_fixed.markup)) * VAR(firm_wage_ref) * VAR(firm_coef_tecnico) * 0.8
  
  price_desejado <- ( 1 + VAR(firm_fixed.markup)) * VAR(firm_custo_unit) * VAR(firm_coef_tecnico) * 0.8
  
  # L = C Y
  # 
  
  #firm_price_dej <- sum2( LAG(firm_ms,1) * LAG(firm_price,1) ) * ( 1 - LAG(firm_ms,1) ) + LAG(firm_ms,1) * price_desejado
  
  firm_price_dej <- price_desejado
  
  return(firm_price_dej)
  
}
addfunABM(name)

name <- atualizar_profit_exp <- function(firm_ms,firm_quality,firm_fixed.markup){
  
  lag_profit_exp <- LAG(firm_exp_profit,1)
  
  if(VAR(time_t) < 5) lag_profit_exp <- LAG(firm_profit,1)
  
  #firm_exp_profit <- (1 + 0.1*( LAG(firm_profit,1)/LAG(firm_exp_profit,1) - 1 ))*LAG(firm_exp_profit,1)*(1+LAG(agg_inflation,1))
  
  firm_exp_profit <- (LAG(firm_profit,1)*0.1 + 0.9*LAG(firm_exp_profit,1))
  
  
  firm_exp_profit[ LAG(firm_leverage,1) > 1] <- mean(firm_exp_profit)
  
  return(firm_exp_profit)
}
addfunABM(name)

name <- firm_profit_rate_function <- function(firm_profit_rate){
  
  # VAR(time_s)
  # 
  # firm_profit_rate_lag <- matrix(LAG(firm_profit_rate,1),quantidade_firmas,1)
  # firm_profit_rate <- LAG(firm_profit_rate,1)
  # 
  # ms <- LAG(firm_ms,1)
  # lev <- LAG(firm_leverage,1)
  # 
  # if(VAR(time_t) > 10){
  #   
  #   taxa_lucro <- LAG(firm_profit,1)/LAG(firm_assets,1)
  #   
  #   taxa_lucro <-   LAG(firm_profit,1)/(LAG(firm_assets,1)-LAG(firm_debt_stock,1) + 0.000001)
  #   
  #   taxa_lucro_prob <- (1 + (taxa_lucro - min(taxa_lucro))/(max(taxa_lucro) - min(taxa_lucro)))^3 - 1
  #   
  #   taxa_lucro_prob[ LAG(firm_vendas,1) < 5] <- min(taxa_lucro_prob)
  #   
  #   taxa_lucro_prob <- taxa_lucro_prob/sum2(taxa_lucro_prob)
  #   
  #   taxa_lucro_prob[ is.nan(taxa_lucro_prob) | is.na(taxa_lucro_prob)] <- 1/quantidade_firmas
  #   
  #   selected <- matrix(Sample(1:quantidade_firmas,5*quantidade_firmas,replace=TRUE,prob=taxa_lucro_prob),quantidade_firmas,5)
  #   
  #   # selected <- matrix(list_firms_selected,quantidade_firmas,5)
  #   
  #   #### Seleção empresas para ponderar qualidade
  #   #list_firms_selected2 <- matrix(list_firms_selected,quantidade_firmas,length(list_firms_selected[1,]))
  #   
  #   #qual <- LAG(firm_quality,1)
  #   
  #   #qual3 <- qual/(rowSums2( matrix(qual[list_firms_selected2[1:100,]],quantidade_firmas,length(list_firms_selected[1,])))/length(list_firms_selected[1,]))
  #   
  #   firm_profit_rate <- matrix( rnorm(quantidade_firmas,mean=1,sd=0.05)*firm_profit_rate_lag ,quantidade_firmas,1)
  #   
  #   #firm_profit_rate <- firm_profit_rate
  #   
  #   firm_profit_rate[ ms == 0 | lev > 1 ] <- (rowSums2( matrix(firm_profit_rate_lag[selected,],quantidade_firmas,length(list_firms_selected[1,])))/length(list_firms_selected[1,]))[ ms == 0 | lev > 1 ]
  #   
  #   #firm_profit_rate[ firm_profit_rate < LAG(firm_interest,1)] <- LAG(firm_interest,1)[ firm_profit_rate < LAG(firm_interest,1) ]  
  #   
  # }
  # 
  # firm_profit_rate[ LAG(firm_price_rule,1) == 2] <- firm_profit_rate_lag[  LAG(firm_price_rule,1) == 2]
  # 
  # 
  # #if(VAR(time_t) == 1) firm_profit_rate <- seq(0.05,0.5,(0.5 - 0.05)/(quantidade_firmas-1))
  #if(VAR(time_t) == 1) firm_profit_rate <- rep(0.25,quantidade_firmas)
  
  if(VAR(time_t) == 1 ){
    
    
    firm_profit_rate <- 1/(1-LAG(firm_leverage_meta,1)) * ( (1+ LAG(firm_fixed.markup,1)) * LAG(firm_wage_ref,1) * LAG(firm_coef_tecnico,1) * grau_utilizao_desejado/prod_machine * 1/LAG(firm_price,1) -
                                                              LAG(firm_leverage_meta,1) * LAG(firm_interest,1))
    
    firm_profit_rate <- (1+ firm_profit_rate)^(1/4) -1
    
    
    
  }else{
    
    firm_profit_rate <- LAG(firm_profit_rate,1) * ( 1 + (LAG(firm_ms,1)/LAG(firm_ms,2) - 1)  )
    
    firm_profit_rate[ firm_profit_rate < LAG(cb_ir,1)] <- LAG(cb_ir,1)
    
  }
  
  return(firm_profit_rate)
}
addfunABM(name)


#  PY = wL + rK = wL + rKmaq/2  = r Y*/(2v) = r Y/(2vu)
# PY = wL + rK 
# P = wL/(uY*) + r2Kmaq/(uY*)
# P = w*u*(L/Y*) + r (2/(uv))
# 1 + m = 1 + r 2/(uv) * 1/(w*u*(L/Y*))
# m = r 2/(uv) * 1/(w x u x (L/Y*))

# L/Y = L/Y' * Y'/Y = coef *1/u

name <- atualizar_fix_markup_desejado <- function(firm_ms,firm_quality,firm_fixed.markup){
  
  #firm_fixed.markup <- 0.8 * lag_firm_fixed_markup + 0.2 * new_fix_mark
  
  prices <- LAG(firm_price,1)
  
  av_price <- sum2((LAG(firm_ms,1) * prices))
  
  prices_sentido <- prices > av_price
  
  stockes_sentido <- LAG(firm_estoque,1) > firm_estoque_desejado_perc * LAG(firm_expdem,1)
  
  #####
  
  price_rule <- VAR(firm_price_rule)
  
  lag_firm_fixed_markup <- LAG(firm_fixed.markup,1)
  
  
  if(VAR(time_t) == tinit){
    
    lag_firm_fixed_markup <- rep(1,quantidade_firmas)
    
    r_implicit <- 1/(1-LAG(firm_leverage_meta,1)) * ( (1+ lag_firm_fixed_markup) * LAG(firm_wage_ref,1) * LAG(firm_coef_tecnico,1) * grau_utilizao_desejado/prod_machine * 1/LAG(firm_price,1) -
                                                        LAG(firm_leverage_meta,1) * LAG(firm_interest,1))
    
    
  }
  
  if(VAR(time_t) > 100){
    
    lag_firm_fixed_markup <- ( 1 + var.markup.ms* ( LAG(firm_ms,1) - LAG(firm_ms,2) )) *
      lag_firm_fixed_markup
    
  }
  
  lag_firm_fixed_markup <- ifelse( LAG(firm_leverage,1) > 1 , sum2(lag_firm_fixed_markup * LAG(firm_ms,1)) , lag_firm_fixed_markup )
  
  firm_fixed.markup <- lag_firm_fixed_markup
  
  
  return(firm_fixed.markup)
}
addfunABM(name)

name <- atualizar_fix_markup_desejado_rnd <- function(firm_profit_rate){
  
  VAR(time_s)
  
  firm_profit_rate_lag <- matrix(LAG(firm_profit_rate,1),quantidade_firmas,1)
  firm_profit_rate <- LAG(firm_profit_rate,1)
  
  ms <- LAG(firm_ms,1)
  lev <- LAG(firm_leverage,1)
  
  if(VAR(time_t) > 10){
    
    taxa_lucro <- LAG(firm_profit,1)/LAG(firm_assets,1)
    
    taxa_lucro <-   LAG(firm_profit,1)/(LAG(firm_assets,1)-LAG(firm_debt_stock,1) + 0.000001)
    
    taxa_lucro_prob <- (1 + (taxa_lucro - min(taxa_lucro))/(max(taxa_lucro) - min(taxa_lucro)))^3 - 1
    
    taxa_lucro_prob[ LAG(firm_vendas,1) < 5] <- min(taxa_lucro_prob)
    
    taxa_lucro_prob <- taxa_lucro_prob/sum2(taxa_lucro_prob)
    
    taxa_lucro_prob[ is.nan(taxa_lucro_prob) | is.na(taxa_lucro_prob)] <- 1/quantidade_firmas
    
    selected <- matrix(Sample(1:quantidade_firmas,5*quantidade_firmas,replace=TRUE,prob=taxa_lucro_prob),quantidade_firmas,5)
    
    # selected <- matrix(list_firms_selected,quantidade_firmas,5)
    
    #### Seleção empresas para ponderar qualidade
    #list_firms_selected2 <- matrix(list_firms_selected,quantidade_firmas,5)
    
    #qual <- LAG(firm_quality,1)
    
    #qual3 <- qual/(rowSums2( matrix(qual[list_firms_selected2[1:100,]],quantidade_firmas,5))/5)
    
    firm_profit_rate <- matrix( rnorm(quantidade_firmas,mean=1,sd=0.01)*firm_profit_rate_lag ,quantidade_firmas,1)
    
    #firm_profit_rate <- firm_profit_rate_lag[ LAG(firm_price_rule,1) == 2  ]
    
    firm_profit_rate[ ms == 0 | lev > 1 ] <- (rowSums2( matrix(firm_profit_rate_lag[selected,],quantidade_firmas,5) )/5)[ ms == 0 | lev > 1 ]
    
  }
  
  if(VAR(time_t) == 1) firm_profit_rate <- seq(0.2,0.5,(0.5 - 0.2)/(quantidade_firmas-1))
  
  firm_fixed.markup <- firm_profit_rate
  
  return(firm_fixed.markup)
}
#addfunABM(name)

name <- atualizar_price_final <- function(firm_price,firm_price_dej){
  
  firm_price <- LAG(firm_price,1)
  firm_price_dej <- VAR(firm_price_dej)
  firm_threshold <- VAR(firm_threshold)
  
  firm_price[ abs(firm_price/firm_price_dej - 1) > firm_threshold ] <- firm_price_dej[ abs(firm_price/firm_price_dej - 1) > firm_threshold ]
  
  return(firm_price)
  
}
addfunABM(name)

name <- atualizar_price_threshold <- function(firm_threshold){
  
  
  firm_threshold <- LAG(firm_threshold,1) # Threshold para ajuste de preços 
  
 # firm_threshold <- (1 + (rbeta(quantidade_firmas,4,4)*0.3 - 0.15)) * firm_threshold # Variação em cada período do threshold
  
 # firm_threshold <- (rbeta(quantidade_firmas,4,4) * 0.03 - 0.015) +  firm_threshold # Variação em cada período do threshold
  
  firm_threshold <- runif(quantidade_firmas,-0.02,+0.02) +  firm_threshold # Variação em cada período do threshold
  
  firm_threshold[ firm_threshold < 0 ] <- 0
  
  ms <- LAG(firm_ms,1) # Market share 
  
  th_subs <- Sample(firm_threshold,quantidade_firmas,replace=TRUE,prob=ms) # Sample para tirar o 
  
  firm_leverage <- LAG(firm_leverage,1)
  
  firm_threshold[ firm_leverage > 1 | ms < 1/quantidade_firmas * 0.1 ] <- th_subs[ firm_leverage > 1 | ms < 1/quantidade_firmas * 0.1 ]
  
  
  if( VAR(time_t) == 6 ) firm_threshold <- runif(quantidade_firmas,0.01,0.2)
  
  if( VAR(time_t) == 6 ) firm_threshold <- 0
  
  
  return(firm_threshold)
}
addfunABM(name)

aux_function_lista <-  function(X){
  
  ret <-  matrix(Sample(quantidade_firmas,5,replace = FALSE,prob = prob1),5,1)
  
  return(ret)
}

name <- atualizar_firms_selected <- function(firm_ms){
  
  prob1 <- LAG(firm_ms,1)
  
  list_firms_selected <- matrix( Sample(quantidade_firmas,10*quantidade_firmas,replace = TRUE,prob = prob1) , quantidade_firmas,10)
  
  #list_firms_selected[[1]] <- t(sapply(matrix(c(1:quantidade_firmas),1,quantidade_firmas), aux_function_lista))
  
  assign("list_firms_selected",list_firms_selected,envir= where("simulationABMFAST2"))
  
  time_s <- 1
  
  return(time_s)
}
addfunABM(name)

aux_function_lista <-  function(X){
  
  ret <-  matrix(Sample(quantidade_firmas,5,replace = FALSE,prob = prob1),5,1)
  
  return(ret)
}

name <- atualizar_firms_price_selected <- function(firm_price,firm_price_dej,firm_ms){
  
  price <- LAG(firm_price,1)
  
  firm_price_selected <- rep(0,quantidade_firmas)
  id.firm <- 1:quantidade_firmas
  
  lista_firm_selected <- list_firms_selected
  
  lengt <- length(list_firms_selected[1,])
  
  for(i in 1:quantidade_firmas){
    
    firm_price_selected[i] <- sum2( price[ lista_firm_selected[i,]] )/lengt
    
  }
  
  return(firm_price_selected)
}
addfunABM(name)

name <- atualizar_firms_quality_selected <- function(firm_quality,firm_ms){
  
  qual <- LAG(firm_quality,1)
  
  firm_quality_selected <- rep(0,quantidade_firmas)
  id.firm <- 1:quantidade_firmas
  
  lista_firm_selected <- list_firms_selected
  
  lengt <- length(list_firms_selected[1,])
  
  for(i in 1:quantidade_firmas){
    
    firm_quality_selected[i] <- sum2(qual[ lista_firm_selected[i,]  ])/lengt
    
  }
  
  return(firm_quality_selected)
}
addfunABM(name)

name <- atualizar_brock_hommes_swichingheuristic <- function(firm_price,firm_quality,firm_price_dej){
  
  profit_real_firms_strat <- rep(0,4)
  
  LAG(firm_receita,8)
  LAG(firm_profit,8)
  LAG(firm_price_rule,8)
  LAG(agg_index_prices,8)
  LAG(firm_machines,8)
  
  if(VAR(time_t) > 100){
    
    for( i in 1:4){
      for(y in 1:1){    
        
        
        if( sum2(firm_machines[,y][firm_price_rule[,y] == i]) == 0){ profit_real_firms_strat[i] <- 0}else{
          
          profit_real_firms_strat[i] <- sum2((firm_profit[,y]/agg_index_prices[,y]/firm_machines[,y])[firm_price_rule[,y] == i]) + profit_real_firms_strat[i]
          
          profit_real_firms_strat[i] <- sum2((firm_profit[,y]/agg_index_prices[,y])[firm_price_rule[,y] == i])/sum2(firm_machines[,y][firm_price_rule[,y] == i])# +
          #profit_real_firms_strat[i]
          
        }
      }
    }
    
    X1 <- profit_real_firms_strat/sum2(profit_real_firms_strat)
    
    # for( i in 1:4){
    #   for(y in 1:8){
    # 
    #     profit_real_firms_strat[i] <- sum2(firm_profit[,y][firm_price_rule[,y] == i])/sum2(firm_receita[,y][firm_price_rule[,y] == i]) * 1/8
    #                                         + profit_real_firms_strat[i]
    #     
    #     if( sum2(firm_receita[,y][firm_price_rule[,y] == i]) == 0) profit_real_firms_strat[i] <- -0.1
    # 
    #   }
    # }
    
    #X1 <- profit_real_firms_strat/mean(profit_real_firms_strat)
    
  }else{ X1 <- rep(1,4)}
  
  lista_past.performance <- matrix(lista_past.performance[[1]],quantidade_firmas,5)
  
  if( any(is.nan(lista_past.performance)) ) break
  
  #
  
  lista_prob.regras <- matrix(lista_prob.regras[[1]],4,quantidade_firmas)
  
  # Performance 
  
  X2 <- t(replicate(quantidade_firmas, X1))
  
  lista_past.performance[,1:4] <- memory.parameter * lista_past.performance[,1:4] + X2
  
  # Valor que entra dentro do exp do modelo de Brock-Hommes:
  
  performance <- par_brockhommes * lista_past.performance[,1:4]
  
  
  X <- ( exp(performance) )/rowSums( exp(performance) )
  
  
  #X <- performance/rowSums(performance)
  
  if(any(is.nan(X)|is.na(X))  ){
    
    X[ is.nan(X) ] <- 1
    
  } 
  
  lista_prob.regras <- param.persistent.strat * lista_prob.regras + (1-param.persistent.strat) * t(X)
  lista_prob.regras[ lista_prob.regras < 0] <- 0.0001
  
  
  firm_prob_costbased <- lista_prob.regras[1,]
  
  # Observação: se no R eu divido uma matriz(a,b) por um vetor com o mesmo tamanho de colunas (b), o cálculo que ocorre é a divisão de cada COLUNA pelo elemento
  # com a mesma posição no vetor. No meu caso, quero dividir os valores das linhas. Assim, transponho a matriz inicial, realizado a divisão, e retorno para posição inicial
  
  #}
  
  assign("lista_past.performance", list(lista_past.performance), envir= where("simulationABMFAST2"))
  assign("lista_prob.regras", list(lista_prob.regras), envir= where("simulationABMFAST2"))
  
  return(firm_prob_costbased)
}
#addfunABM(name)

# Expectativa de inflação

name <- atualizar_brock_hommes_swichingheuristic_inflation_exp <- function(firm_price,firm_quality,firm_price_dej){
  
  firm_prob_naive <- 1:quantidade_firmas
  
  if(VAR(time_t) > 100){
    
    adapt.expectations <-  LAG(agg_inflation,2) + 0.25*(LAG(agg_inflation,2) - LAG(agg_inflation,3))
    
    vec.inflation <- c(LAG(agg_inflation,2),LAG(agg_inflation,3),LAG(agg_inflation,4),LAG(agg_inflation,5),LAG(agg_inflation,6),LAG(agg_inflation,7))
    vec.unemployment <- c(LAG(agg_unemployment_rate,2),LAG(agg_unemployment_rate,3),LAG(agg_unemployment_rate,4),LAG(agg_unemployment_rate,5),LAG(agg_unemployment_rate,6),LAG(agg_unemployment_rate,7))
    
    if( all(vec.unemployment == vec.unemployment[1]) ){
      lm.results <- lm(vec.inflation[2:6] ~ vec.inflation[1:5])
      exp.ols <- matrix(lm.results$coefficients,1,2) %*% c(1,vec.inflation[2])
    }else{
      lm.results <- lm(vec.inflation[2:6] ~ vec.inflation[1:5] + vec.unemployment[2:6])
      exp.ols <- matrix(lm.results$coefficients,1,3) %*% c(1,vec.inflation[2],vec.unemployment[1])
    }
    
    approach.naive <- -(10*(LAG(agg_inflation,1) - LAG(agg_inflation,2)))^2
    approach.adptative <- -(10*(LAG(agg_inflation,1) - adapt.expectations))^2
    approach.tend <- - (10*(LAG(agg_inflation,1) - (0.25*(meta_inflacao) + 0.75 * adapt.expectations)))^2
    approach.ols <- -(10*(LAG(agg_inflation,1) - exp.ols ))^2
    
    lista_past.performance.exp <- matrix(lista_past.performance.exp[[1]],quantidade_firmas,5)
    
    #lista_past.performance <- as.matrix(list(matrix(1,quantidade_firmas,5))[[1]])
    
    lista_prob.exp <- matrix(lista_prob.exp[[1]],4,quantidade_firmas)
    
    #for(i in 1:quantidade_firmas){
    
    # Performance 1: usar o cost-based approach
    # Performance 
    
    presente.performance <- matrix(0,quantidade_firmas,4)
    presente.performance[,1] <- approach.naive
    presente.performance[,2] <- approach.adptative
    presente.performance[,3] <- approach.tend
    presente.performance[,4] <- approach.ols
    
    lista_past.performance.exp_2 <- lista_past.performance.exp
    
    lista_past.performance.exp_2[,1:4] <- memory.parameter * lista_past.performance.exp[,1:4] +  
      + presente.performance
    
    #exp(lista_past.performance[])/sum2(exp(lista_past.performance[[i]]))
    
    X <- ( exp(lista_past.performance.exp_2[,1:4]) )/rowSums( exp(lista_past.performance.exp_2[,1:4]) )
    
    if(any(is.nan(X)|is.na(X)|is.infinite(X))) break
    
    lista_prob.exp <- param.persistent.strat * lista_prob.exp + (1-param.persistent.strat) * t(X)
    
    firm_prob_naive <- lista_prob.exp[1,]
    
    # Observação: se no R eu divido uma matriz(a,b) por um vetor com o mesmo tamanho de colunas (b), o cálculo que ocorre é a divisão de cada COLUNA pelo elemento
    # com a mesma posição no vetor. No meu caso, quero dividir os valores das linhas. Assim, transponho a matriz inicial, realizado a divisão, e retorno para posição inicial
    
    #}
    
    assign("lista_past.performance.exp", list(lista_past.performance.exp), envir= where("simulationABMFAST2"))
    assign("lista_prob.exp", list(lista_prob.exp), envir= where("simulationABMFAST2"))
    
  }
  
  return(firm_prob_naive)
}
#addfunABM(name)

# Effective Mark-up
name <- atualizar_eff_markup <- function(firm_price , firm_custo , firm_vendas){
  
  vendas <- VAR(firm_vendas)
  
  vendas[ vendas != 0] <- VAR(firm_price)[ vendas != 0]/(VAR(firm_custo)[ vendas != 0]/VAR(firm_vendas)[ vendas != 0]) - 1
  
  vendas[ vendas == 0] <- 0
  
  firm_eff_markup <- vendas
  
  return(firm_eff_markup)
}
addfunABM(name)

name <- atualizar_price <- function(firm_custo){
  
  VAR(firm_custo)
  
  firm_price <- 1
  
  return(firm_price)
  
}
#addfunABM(name)

# Atualizar estratégia preço

name <- atualizar_price_strat_revise <- function(firm_profit,firm_price_rule,time_t){
  
  # Variáveis
  
  firm_revise_decision <- LAG(firm_revise,1) + 1
  
  if(time_t > 10){
    
    if( any(firm_revise_decision > 4) ){
      
      value <- round( runif(length(firm_revise_decision[firm_revise_decision > 4]),0,1) )
      
      firm_revise_decision[ firm_revise_decision > 4 ] <- ifelse(value == 0, 0, 1)
      
    }
  }
  
  firm_revise <- ifelse(firm_revise_decision == 0,0,LAG(firm_revise,1) + 1)
  
  return(firm_revise) 
}
#addfunABM(name)

name <- atualizar_price_strat <- function(firm_profit,firm_price_rule,time_t){
  
  firm_price_rule <- VAR(firm_price_rule)
  
  firm_prob_costbased
  
  lista_prob_heuristicas <- lista_prob.regras[[1]]
  
  time_t <- VAR(time_t)
  
  if(time_t == 90){ firm_price_rule <- c(rep(1,25),rep(2,25),rep(3,25),rep(4,25)) }
  
  if(time_t > 100){
    
    for(i in 1:quantidade_firmas){
      if( VAR(firm_revise)[i] == 0 ){
        
        # Se não houver experimento
        
        if(experimet == 0){
          
          firm_price_rule[i] <- Sample(c(1,2,3,4),1,replace=FALSE,prob=lista_prob_heuristicas[,i])
          
        }else{
          
          firm_price_rule[i] <- experimet
          
        }
        # Se houver experimento
      }
    }
    
  }
  
  return(firm_price_rule)
}
#addfunABM(name)

# Estatísticas dos preços

name <- regist_price_change <- function(firm_price){
  
  price_change <- rep(0,quantidade_firmas)
  
  firm_price_change <- VAR(firm_price)/LAG(firm_price,1) - 1
  
  return(firm_price_change)  
}
addfunABM(name)

# Firms finance

name <- atualizar_ativos_firma <- function(firm_machines,firm_cash){
  
  relat.value.maq <- 1:quantidade_firmas
  
  for(i in 1:quantidade_firmas){
    
    c <- lista_maquinas[[i]][1,]
    
    relat.value.maq[i] <- (sum2(c)/length(c))/temp_depreci
    
  }
  
  firm_assets <- relat.value.maq*VAR(firm_machines)*VAR(firmk_price) + VAR(firm_cash) + VAR(firm_estoque) * VAR(firm_price)
  
  return(firm_assets)
}
#addfunABM(name)

# Nova função assets 
name <- atualizar_ativos_firma <- function(firm_machines,firm_cash){
  
  # 1 ativo como ativos líquidos + iliquidos. Valor dado pelo valor reativo do cpaital
  
  # relat.value.maq <- 1:quantidade_firmas
  # 
  # for(i in 1:quantidade_firmas){
  #   
  #   c <- lista_maquinas[[i]][1,]
  #   
  #   relat.value.maq[i] <- (sum2(c)/length(c))/temp_depreci
  #   
  # }
  # 
  # # 2 ativo como total do ativo necessário para a atividade operacional mais necessidades de liquidez
  # 
  # firm_assets <- (relat.value.maq*VAR(firm_machines)*VAR(firmk_price) + VAR(firm_custo_unit) * VAR(firm_coef_tecnico) * VAR(firm_estoque))*(2 * firmk_lb_prod /( prod_machine * rel.k.l )) # Este termo dá a multiplicação do estoque de ativos reais (estoques + máquinas) que dá o ativo total
  # 
  # 
  # firm_assets <- (VAR(firm_machines)*VAR(firmk_price) + VAR(firm_custo_unit) * VAR(firm_coef_tecnico) * VAR(firm_estoque)) * 2
  # 
  # firm_assets <- (VAR(firm_expdem)/prod_machine*VAR(firmk_price) + VAR(firm_custo_unit) * VAR(firm_coef_tecnico) * VAR(firm_estoque)) * 2
  # 
  
  # Ativo como função liquid assets
  
  firm_deficitsurplus <- VAR(firm_deficitsurplus)
  
  #firm_deficitsurplus[ firm_deficitsurplus <0 ] <- 0
  
  #firm_deficitsurplus[ firm_deficitsurplus < 0 | LAG(firm_leverage,1) > LAG(firm_leverage_meta,1) ]  <- 0
  
  
  firm_deficitsurplus[ firm_deficitsurplus < 0  ]  <- 0
  
  # firm_deficitsurplus[    firm_deficitsurplus  >  ( 1 - 1/temp_depreci ) * LAG(firm_debt_stock,1)  +
  #                           VAR(firm_investimento)/2          ] <- (firm_deficitsurplus - ( 1 - 1/temp_depreci ) * LAG(firm_debt_stock,1)  +
  #   VAR(firm_investimento)/2)[    firm_deficitsurplus  >  ( 1  - 1/temp_depreci ) * LAG(firm_debt_stock,1)  +
  #                                   VAR(firm_investimento)/2   ]
  
  
  firm_assets <- LAG(firm_assets,1) + firm_deficitsurplus
  
  return(firm_assets)
}
addfunABM(name)

name <- atualizar_firm_leverage <- function(firm_assets,firm_debt_stock){
  
  assets <- VAR(firm_assets)
  passivo <- VAR(firm_debt_stock)
  
  leverage <- passivo/assets
  
  # passivo/(assets - passivo) - Leverage
  
  firm_leverage <- leverage
  
  firm_leverage[ is.na(firm_leverage) | is.infinite(firm_leverage)] <- 1
  
  return(firm_leverage)
}
addfunABM(name)

name <- atualizar_var.cash_stock <- function(firm_leverage,firm_deficitsurplus,firm_debt_stock){
  
  #firm_cash_lag <- LAG(firm_cash,1)
  
  leverage <- LAG(firm_leverage,1)
  
  lag_firm_deficitsurplus <- LAG(firm_deficitsurplus,1)
  firm_deficitsurplus <- VAR(firm_deficitsurplus)
  
  lag_firm_debt_stock <- LAG(firm_debt_stock,1)
  firm_debt_stock <- VAR(firm_debt_stock)
  
  #firm_deficitsurplus[ firm_deficitsurplus < 0] <- 0
  
  firm_var.cash <- rep(0,quantidade_firmas)
  debt_payment <- rep(0,quantidade_firmas)
  #firm_dividendos <- rep(0,quantidade_firmas)
  
  # firm_var.cash <- (firm_debt_stock - lag_firm_debt_stock) + lag_firm_debt_stock/temp_depreci + firm_deficitsurplus
  
  firm_var.cash <- (firm_debt_stock - lag_firm_debt_stock) + firm_deficitsurplus
  
  # Quanto, se, variou a dívida por motivos outros além 
  # do pagamento acordado
  
  # Alocação para dividendos
  
  return(firm_var.cash)
}
#addfunABM(name)

# Nova função var.cash
name <- atualizar_var.cash_stock <- function(firm_leverage,firm_deficitsurplus,firm_debt_stock){
  
  #firm_cash_lag <- LAG(firm_cash,1)
  
  firm_var.cash <- VAR(firm_cash) - LAG(firm_cash,1)
  
  return(firm_var.cash)
}
addfunABM(name)

name <- atualizar_cash_stock <- function(firm_leverage,firm_deficitsurplus){
  
  firm_cash_lag <- LAG(firm_cash,1)
  var.cash <- VAR(firm_var.cash)
  
  firm_cash <- firm_cash_lag + var.cash
  
  return(firm_cash)
}
#addfunABM(name)

# Nova função cash stock

name <- atualizar_cash_stock <- function(firm_leverage,firm_deficitsurplus){
  
  #firm_cash <- VAR(firm_assets) * (2 * firmk_lb_prod /( prod_machine * rel.k.l ))
  #
  #firm_cash  <- VAR(firm_assets) * ( 1 - 1/(2 * firmk_lb_prod /( prod_machine * rel.k.l )))
  
  #(2 * firmk_lb_prod /( prod_machine * rel.k.l ))
  
  # A = Mult * M
  # A = M + C
  
  # A = A/Mult + c
  # (1 - 1/Mult) * A = C
  
  # M + C = Mult * M
  # 1 + C/M = Mult
  # C = M * (Mult - 1)
  # C = A - M
  # M = A/Mult
  # C = A(1 - 1/Mult)
  
  relat.value.maq <- 1:quantidade_firmas
  
  for(i in 1:quantidade_firmas){
    
    c <- lista_maquinas[[i]][1,]
    
    relat.value.maq[i] <- (sum2(c)/length(c))/temp_depreci
    
  }
  
  #firm_assets <- (relat.value.maq*VAR(firm_machines)*VAR(firmk_price) + VAR(firm_custo_unit) * VAR(firm_coef_tecn) * VAR(firm_estoque))*(2 * firmk_lb_prod /( prod_machine * rel.k.l )) # Este termo dá a multiplicação do estoque de ativos reais (estoques + máquinas) que dá o ativo total
  
  
  #firm_cash <- VAR(firm_assets) - (relat.value.maq*VAR(firm_machines)*VAR(firmk_price) + VAR(firm_custo_unit) * VAR(firm_coef_tecnico) * VAR(firm_estoque))
  
  # Pela definição que Assets são ativos líquidos
  
  firm_cash <- VAR(firm_assets)
  
  return(firm_cash)
}
addfunABM(name)

name <- atualizar_receita <- function(firm_vendas,firm_price){
  
  firm_receita <- VAR(firm_vendas) * VAR(firm_price)
  
  return(firm_receita)
  
}
addfunABM(name)

name <- atualizar_resultado_operacional <- function(firm_receita,firm_custo){
  
  receita <- VAR(firm_receita)
  custo <- VAR(firm_custo)
  
  firm_receita_op <- receita - custo
  
  return(firm_receita_op)
  
}
addfunABM(name)

name <- atualizar_interest_firm <- function(firm_fb, cb_ir){
  
  firm_leverage <- LAG(firm_leverage,1)
  firm_leverage[ firm_leverage > 1] <- 1
  interest_cb <-  ( 1 + VAR(cb_ir))^4 - 1
  
  par_rate_emprestimos <- rep(0,quantidade_firmas)
  
  par_rate_emprestimos[ firm_leverage < 0.25] <- 1.1
  par_rate_emprestimos[ firm_leverage >= 0.25 & firm_leverage < 0.5] <- 1.3
  par_rate_emprestimos[ firm_leverage >= 0.5 & firm_leverage < 0.75] <- 1.7
  par_rate_emprestimos[ firm_leverage >= 0.75 ] <- 2.5
  
  firm_financial_burden <- LAG(firm_fb,1)
  
  firm_financial_burden[ firm_financial_burden == 0 ] <- 0.0000000000000000001
  
  covered_ratio <- LAG(firm_receita_op,1)/firm_financial_burden
  
  par_rate_emprestimos[ covered_ratio < 0.5] <- 0.1076
  par_rate_emprestimos[ covered_ratio >= 0.5 & covered_ratio < 1.5 ] <- 0.0778
  par_rate_emprestimos[ covered_ratio >= 1.5 & covered_ratio < 2.5 ] <- 0.0378
  par_rate_emprestimos[ covered_ratio >= 2.5 & covered_ratio < 3.5 ] <- 0.0215
  par_rate_emprestimos[ covered_ratio >= 3.5 & covered_ratio < 4.5 ] <- 0.0159
  par_rate_emprestimos[ covered_ratio >= 4.5 & covered_ratio < 6 ] <- 0.0140
  par_rate_emprestimos[ covered_ratio >= 6 & covered_ratio < 9.5 ] <- 0.0103
  par_rate_emprestimos[ covered_ratio >= 9.5 ] <- 0.0075
  
  
  #firm_interest <- LAG(firm_fb,1)/firm_receita_lag + VAR(cb_ir)
  
  # firm_interest <- (1 + par_rate_emprestimos + firm_leverage/10) * interest_cb
  
  firm_interest <- ( 1 + par_rate_emprestimos + interest_cb)^(1/4) - 1 # Tirei o efeito de alavancagem, endividamento, etc, para simplificar
  
  return(firm_interest)
}
addfunABM(name)

name <- atualizar_financial_burden <- function(firm_interest,firm_debt_stock){
  
  #firm_fb <- LAG(firm_debt_stock,1) * VAR(firm_interest) + LAG(firm_debt_stock,1)/(temp_depreci)
  
  firm_fb <- LAG(firm_debt_stock,1) * VAR(firm_interest) + LAG(firm_debt_stock,1)/(temp_depreci)
  
  return(firm_fb)
}
addfunABM(name)

name <- atualizar_lucro <- function(firm_receita_op,firm_fb){
  
  rec_op <-  VAR(firm_receita_op)
  fb <- VAR(firm_fb)
  
  firm_profit <- rec_op - fb - VAR(firm_RD_invest)
  
  return(firm_profit)
}
addfunABM(name)

name <- atualizar_firm_deficit <- function(firm_profit,firm_RD_invest,firm_investimento,firm_dividendos){
  
  firm_deficitsurplus <- VAR(firm_profit) - VAR(firm_investimento) - VAR(firm_dividendos)
  
  #firm_deficitsurplus <- VAR(firm_fb) * ( LAG(firm_leverage,1)/LAG(firm_leverage_meta,1) )
  
  return(firm_deficitsurplus)
}
addfunABM(name)

# Dinâmica da dívida das firmas 

name <- atualizar_firm_debt_stock <- function(firm_deficitsurplus,firm_fb,firm_debt_stock,firm_dividendos){
  
  # Variáveis 
  #firm_deficitsurplus <- VAR(firm_deficitsurplus)
  #firm_debt_stock <- LAG(firm_debt_stock,1)
  #leverage <- LAG(firm_leverage,1)
  
  # Função
  
  # Destino do excesso de recursos. 
  
  #firm_debt_stock[ firm_deficitsurplus < 0 ] <- (1 - 1/(temp_depreci)) * lag_debt_stock[ firm_deficitsurplus < 0] - firm_deficitsurplus[ firm_deficitsurplus < 0]
  #firm_debt_stock[ firm_deficitsurplus > 0 ] <- (1 - 1/(temp_depreci)) * lag_debt_stock[ firm_deficitsurplus > 0] - firm_deficitsurplus[ firm_deficitsurplus < 0]
  
  # Se a firma teve um surplus:
  
  #firm_deficitsurplus[ firm_deficitsurplus > 0] <- ifelse( debt_payment_percent_supr * firm_deficitsurplus[ firm_deficitsurplus > 0] > (1 - 1/(temp_depreci)) * firm_debt_stock[ firm_deficitsurplus > 0 ] ,
  #                                                         (1 - 1/(temp_depreci)) * firm_debt_stock[ firm_deficitsurplus > 0 ],
  #                                                         debt_payment_percent_supr * firm_deficitsurplus[ firm_deficitsurplus > 0]
  #)
  
  #firm_deficitsurplus[ firm_deficitsurplus > 0] <- 1/2 * (1 - financial_restriction) * leverage[ firm_deficitsurplus > 0 ] * firm_deficitsurplus[ firm_deficitsurplus > 0]
  #firm_deficitsurplus[ firm_deficitsurplus < 0] <- (1 - (1 - financial_restriction) * leverage[ firm_deficitsurplus < 0 ]) * firm_deficitsurplus[ firm_deficitsurplus < 0]
  
  #LAG(firm_debt_stock,1)/(temp_depreci)
  
  #firm_debt_pos_dist <- - (1/temp_depreci) * LAG(firm_debt_stock,1) - VAR(firm_deficitsurplus) + LAG(firm_debt_stock,1)
  
  firm_debt_pos_dist <-  - VAR(firm_deficitsurplus) + LAG(firm_debt_stock,1) # - (1/temp_depreci) * LAG(firm_debt_stock,1)
  
  firm_debt_pos_dist[firm_debt_pos_dist <0] <- 0
  
  # Mudança da estrutura de capital
  
  firm_debt_stock <- firm_debt_pos_dist
  
  return(firm_debt_stock)
} 
#addfunABM(name)

# EStoque de dívida determinada pela diferença de var ativos e resto de recursos 
name <- atualizar_firm_debt_stock <- function(firm_deficitsurplus,firm_fb,firm_debt_stock,firm_dividendos){
  
  firm_debt_pos_dist <-  VAR(firm_assets) - LAG(firm_assets,1) + LAG(firm_debt_stock,1) - VAR(firm_deficitsurplus)
  
  firm_debt_pos_dist[firm_debt_pos_dist <0] <- 0
  
  # Mudança da estrutura de capital
  
  firm_debt_stock <- firm_debt_pos_dist
  
  return(firm_debt_stock)
} 
#addfunABM(name)

# EStoque de dívida determinado por resultado do caixa, dinâmica da dívida
name <- atualizar_firm_debt_stock <- function(firm_deficitsurplus,firm_fb,firm_debt_stock,firm_dividendos){
  
  firm_deficitsurplus <- VAR(firm_deficitsurplus)
  
  creditworthness <- (VAR(firm_debt_stock) + LAG(firm_debt_stock,1))/(VAR(firm_receita_op) + LAG(firm_receita_op,1))
  
  VAR(firm_fb)/VAR(firm_receita_op)
  
  #firm_deficitsurplus[ firm_deficitsurplus > 0] <- 0
  
  firm_deficitsurplus[ firm_deficitsurplus > 0  ]  <- 0
  # 
  # firm_deficitsurplus[    firm_deficitsurplus  >  ( 1 - 1/temp_depreci ) * LAG(firm_debt_stock,1)  +
  #                           VAR(firm_investimento)/2          ] <- (( 1 - 1/temp_depreci ) * LAG(firm_debt_stock,1)  +
  #   VAR(firm_investimento)/2)[    firm_deficitsurplus  >  ( 1 - 1/temp_depreci ) * LAG(firm_debt_stock,1)  +
  #                                   VAR(firm_investimento)/2          ] # Aqui é uma conta complicada basicamente em que, se o DC for maior que o
  # # encargo da dívida, quer dizer que há mais recurso do qeu o necessário para quitar toda a dívida. Nesse caso , 
  
  
  firm_debt_pos_dist <-  ( 1 - 1/temp_depreci ) * LAG(firm_debt_stock,1)  - # Dinâmica pela taxa de juros, descontado o pagamento principal
    firm_deficitsurplus - # Resultado déficit/surplus das firmas. 
    ifelse( LAG(firm_leverage,1) > 1 , LAG(firm_debt_stock,1) ,0 ) # Se a firma quebrou, toda a sua dívida é paga.
  
  firm_debt_pos_dist[firm_debt_pos_dist <0] <- 0
  
  firm_debt_stock <- firm_debt_pos_dist
  
  return(firm_debt_stock)
} 
addfunABM(name)

# Dividendos

name <- meta_leverage <- function(firm_leverage,firm_interest,firm_receita_op){
  
  # Fonte : Artigo do Alberto Russo.
  
  firm.interest <- LAG(firm_interest,1) + 1/temp_depreci
  #firm.interest <- ifelse(firm.interest < 0.05 , 0.05 , firm.interest)
  
  firm.receita.op <- ifelse( LAG(firm_receita_op,1) <0 , 0 , LAG(firm_receita_op,1) )
  
  firm_leverage_meta <- LAG(firm_leverage_meta,1)
  
  firm_leverage_obs <- LAG(firm_leverage,1)
  firm_leverage_obs[ firm_leverage_obs == 1] <- 0.99
  
  lag_profit_rate <- LAG(firm_profit_rate,1)
  
  k_stock <- LAG(firm_expdem,1) /( prod_machine * grau_utilizao_desejado ) * LAG(firmk_price,1)
  
  k_stock[ k_stock <  LAG(firmk_price,1) ] <- LAG(firmk_price,1)
  
  profit_rate_realized <- LAG(firm_profit,1)/k_stock
  
  profit_rate_realized <- ( 1 + LAG(firm_interest,1) )^4- 1 + 0.05

  # denominador <- (firm.interest * LAG(firm_debt_stock,1))  + profit_rate_realized * (LAG(firm_assets,1) - LAG(firm_debt_stock,1))   # Encargo financeiro + Lucro
  # denominador[ denominador == 0] <- 0.05
  # 
  # Sentido <- (firm.interest * firm_leverage_meta) < lag_profit_rate*(1 - firm_leverage_meta)
  # 
  # dif <- abs((firm.interest * firm_leverage_obs)/((lag_profit_rate+0.0000001)*(1 - firm_leverage_obs))-1) > 0.1
  # 
  # 
  # #firm_leverage_meta[ firm.receita.op/denominador > 1.02  ] <- 1.1 * firm_leverage_meta[ firm.receita.op/denominador > 1.02  ] 
  # #firm_leverage_meta[ firm.receita.op/denominador < 0.98  ] <- 0.9 * firm_leverage_meta[ firm.receita.op/denominador < 0.98  ] 
  # 
  # firm_leverage_meta[Sentido == TRUE & dif == TRUE]  <- 1.15 * firm_leverage_meta[Sentido == TRUE & dif == TRUE] 
  # firm_leverage_meta[Sentido == FALSE & dif == TRUE] <- 0.85 * firm_leverage_meta[Sentido == FALSE & dif == TRUE]  
  
  firm_leverage_meta <- 0.1 * (profit_rate_realized/ (profit_rate_realized + ((1 + LAG(firm_interest,1))^4-1) ) ) + 0.9 * firm_leverage_meta
  
  firm_leverage_meta[ firm_leverage_meta > 0.9] <- 0.9
  firm_leverage_meta[ firm_leverage_meta < 0.1] <- 0.1
  
  # firm_leverage_meta <- rep(0.5, quantidade_firmas)
  # 
  # 
  # ((LAG(firm_fb,1) + LAG(firm_RD_invest,1))/LAG(firm_receita_op,1) - 1)
  
  
  max_debt <- (LAG(firm_receita,1) - LAG(firm_RD_invest,1) - LAG(firm_custo,1))/LAG(firm_interest,1)
  
  lev_rule_2 <- max_debt/LAG(firm_assets,1)
  
  lev_rule_2[ lev_rule_2 < 0] <- 0
  
  
  firm_leverage_meta <- ifelse(firm_leverage_meta < lev_rule_2 , firm_leverage_meta, lev_rule_2 )
  
  
  
  return(firm_leverage_meta)
}
addfunABM(name)

# Nova nova função dividendos

name <- atualz_div <- function(firm_var.cash,firm_deficitsurplus,firm_leverage){
  
  firm_dividendos <- rep(0,quantidade_firmas) # Se assumisse todo o resultado (positivo ou negativo)
  
  lag_firm_profit <- LAG(firm_profit,1)
  leverage <- LAG(firm_leverage,1)
  
  meta.debt <- LAG(firm_leverage_meta,1) * LAG(firm_assets,1)
  dividendos <- meta.debt -  LAG(firm_debt_stock,1) 
  
  LEV <- leverage > LAG(firm_leverage_meta,1)
  
  df_antes_dividendo <- LAG(firm_profit,1) - (LAG(firmk_price,1) * LAG(firm_expdem,1) * prod_machine)/temp_depreci - LAG(firm_investimento,1) -
    LAG(firm_RD_invest,1)
  
  
  firm_dividendos <- ( LAG(firm_leverage_meta,1) - leverage + 0.5)*1*df_antes_dividendo # O parâmetro vem de: Agent Based-Stock Flow Consistent Macroeconomics: Towards a
  # Benchmark Model - https://www.siecon.org/sites/siecon.org/files/oldfiles/uploads/2015/10/Caiani.pdf
  
  #firm_dividendos <- max(0.9 * df_antes_dividendo,0)
  
  adj <- ifelse( abs( LAG(firm_leverage,1) - LAG(firm_leverage_meta,1)) > 0.005 , LAG(firm_leverage,1)/LAG(firm_leverage_meta,1) - 1 , 0 )
  
  firm_dividendos <- LAG(firm_profit,1) - (LAG(firmk_price,1) * LAG(firm_expdem,1)/prod_machine)/temp_depreci - LAG(firm_investimento,1) -
    LAG(firm_RD_invest,1) -  VAR(firm_fb) * ( adj )
  
  dividends2 <- rep(0,quantidade_firmas)
  
  #dividends2 <- LAG(firm_assets,1) * ( LAG(firm_leverage_meta,1)  - LAG(firm_leverage,1) )
  #dividends2[ LAG(firm_leverage_meta,1)  < LAG(firm_leverage,1)  ] <- 0
  
  #dividend_policy_1 <- dividend_policy * ( 1 - ( LAG(firm_leverage,1) - LAG(firm_leverage_meta,1) ) )
  
  dividend_policy_1 <- 0.8
  
  dividends <- cbind(dividend_policy_1 * ((LAG(firm_profit,1) - (LAG(firmk_price,1) * LAG(firm_expdem,1)/prod_machine)/temp_depreci ) +
                                            (LAG(firm_profit,2) - (LAG(firmk_price,2) * LAG(firm_expdem,2)/prod_machine)/temp_depreci )+
                                            (LAG(firm_profit,3) - (LAG(firmk_price,3) * LAG(firm_expdem,3)/prod_machine)/temp_depreci )+
                                            (LAG(firm_profit,4) - (LAG(firmk_price,4) * LAG(firm_expdem,4)/prod_machine)/temp_depreci ))/4 + dividends2
                     ,0)
  
  # dividends <- cbind(dividend_policy_1 * ((LAG(firm_profit,1)  ) +
  #                                           (LAG(firm_profit,2)  )+
  #                                           (LAG(firm_profit,3)  )+
  #                                           (LAG(firm_profit,4)  ))/4 + dividends2
  #                    ,0)
  # 
  dividends <- rowMaxs(dividends)
  
  dividends <-  dividend_policy_1 * 2 * ( LAG(firm_leverage_meta,1) - LAG(firm_leverage,1) ) * LAG(firm_assets,1) + dividends
  
  dividends[ dividends < 0] <- 0


  firm_dividendos <- dividends  # Dividendos mais o aporte de capital para investimento
  
  
  return(firm_dividendos)
}
addfunABM(name)

# Substituir empresa falida

name <- atualizar_falida <- function(firm_cash, capitalist_riqueza, firm_leverage,firm_debt_stock,firm_quality ){
  
  VAR(firm_RD_invest_acum)
  
  cash <- LAG(firm_cash,1)
  
  num_maq <- LAG(firm_machines,1)
  
  capitalist_riqueza <- VAR(capitalist_riqueza)
  
  #firm_quality <- VAR(firm_quality)
  
  firm_lev <- firm_leverage
  
  firm_leverage <- LAG(firm_leverage,1)
  
  firm_ms <- LAG(firm_ms,1)
  
  firm_debt_stock <- firm_debt_stock
  
  agg_empresas_falidas <- 0
  
  if( any(firm_leverage > 1 | firm_ms == 0) ){
    
    agg_empresas_falidas <- length( firm_leverage[ firm_leverage > 1 ] )
    
    #firm_leverage[ firm_leverage > 1 ] <- ceiling( 3 * VAR(firmk_price)) # 3 MÁQUINAS INICIAIS 
    
    firm_RD_invest_acum[,1][ firm_leverage > 1 | firm_ms == 0 ] <- 0
    
    firm_quality[,1][ firm_leverage > 1 | firm_ms == 0 ] <- sum2(firm_quality[,1])/length(firm_quality[,1]) # Atualização da qualidade da nova firma
    
    #firm_expdem[,1][ firm_ms == 0] <- 3 # Reduz a demanda esperada para 3
    #firm_expdem[,2][ firm_ms == 0] <- 3 # Reduz a demanda esperada para 3
    
    firm_cash[,1][ firm_leverage > 1 | firm_ms == 0 ] <- 3 * firm_expdem[,1][firm_leverage > 1 | firm_ms == 0]/prod_machine * VAR(firmk_price) + 4 * VAR(firm_custo)[firm_leverage > 1 | firm_ms == 0] # Capitalização das novas firmas
    
    firm_debt_stock[,1][ firm_leverage > 1 | firm_ms == 0  ] <- 0 # Estoque de dívida
    
    firm_lev[,1][ firm_leverage > 1 | firm_ms == 0  ] <- 0 # Firma alavancagem
    
    #firm_fb[ firm_leverage > 1  ] <- 0
    
    #capitalist_riqueza <- matrix(capitalist_riqueza - agg_empresas_falidas * ceiling( mean.default(num_maq) * VAR(firmk_price)),1,1)
    
    #assign("capitalist_riqueza",capitalist_riqueza,envir=where("simulationABMFAST2"))
    
    assign("firm_leverage",firm_lev,envir=where("simulationABMFAST2"))
    #assign("firm_expdem",firm_expdem,envir=where("simulationABMFAST2"))
    #assign("firm_RD_invest_acum",firm_RD_invest_acum,envir=where("simulationABMFAST2"))
    assign("firm_cash",firm_cash,envir=where("simulationABMFAST2"))
    assign("firm_debt_stock",firm_debt_stock,envir=where("simulationABMFAST2"))
    assign("firm_quality",firm_quality,envir=where("simulationABMFAST2"))
    
  }
  
  return(agg_empresas_falidas)
}
#addfunABM(name)

name <- atualizar_falida <- function(firm_cash, capitalist_riqueza, firm_leverage,firm_debt_stock,firm_quality ){
  
  firm_leverage_lag <- LAG(firm_leverage,1)
  
  stat <- rep(0,quantidade_firmas)
  stat[ firm_leverage_lag > 1] <- 1
  
  agg_empresas_falidas <- sum2( stat)
  
  return(agg_empresas_falidas)
}
addfunABM(name)

# Porcentagem do dinheiro na forma de CASH 

name <- atualizar_percent_cash <- function(cb_ir){
  
  capitalist_cash_percent <- lambda_zero_cash - lambda_cash_capitalist * LAG(cb_ir,1)
  
  return(capitalist_cash_percent)
}
addfunABM(name)

# Famílias - tabalhadores

name <- atualizar_workers_income <- function(workers_salario,workers_riqueza,cb_ir,gov_unem_ben){
  
  #workers_income <- VAR(workers_salario) + ifelse( VAR(workers_empregador) == 0 , VAR(gov_unem_ben) , 0  ) + (par_rate_deposits * VAR(cb_ir)) * 0 * LAG(workers_riqueza,1)
  
  workers_income <- VAR(workers_salario) + ifelse( VAR(workers_empregador) == 0 , VAR(gov_unem_ben) , 0  )
  
  return(workers_income)
  
}
addfunABM(name)

name <- atualizar_workers_disposable_income <- function(workers_income,gov_ir){
  
  workers_disposable_income <- (1 - VAR(gov_ir))*VAR(workers_income)
  
  return(workers_disposable_income)
  
}
addfunABM(name)

name <- atualizar_workers_consumption <- function(workers_disposable_income,workers_riqueza){
  
  workers_consumo <- pro_consumir_wage * VAR(workers_disposable_income) + pro_consumir_wealth_workers * LAG(workers_riqueza,1)
  
  return(workers_consumo)
}
addfunABM(name)

name <- atualizar_workers_riqueza <- function(workers_income,workers_consumo){
  
  workers_riqueza <- VAR(workers_disposable_income) - VAR(workers_consumo) + LAG(workers_riqueza,1)
  
  return(workers_riqueza)
}
addfunABM(name)

# Capitalista

name <- atualizar_dividendos <- function(firm_deficitsurplus,bank_profit){
  
  capitalist_dividendos <- sum2( VAR(firm_dividendos) ) + LAG(bank_profit,1) + LAG(firmk_profit,1)
  
  return(capitalist_dividendos)
}
addfunABM(name)

name <- atualizar_capitalist_income <- function(capitalist_dividendos,capitalist_riqueza,cb_ir,firm_RD_invest){
  
  dividendos <- VAR(capitalist_dividendos)
  
  if(dividendos < 0) dividendos <- 0
  
  capitalist_income <- dividendos + sum2( VAR(firm_RD_invest) ) +
    (par_rate_deposits * VAR(cb_ir)) * ( 1 - LAG(capitalist_cash_percent,1))  * LAG( capitalist_riqueza, 1)
  
  # Excesso de $ que ficou no caixa das firmas. Não foi utilizado para reduzir dívida ( == 0), Ativo é constante
  
  #Excess.Money <- LAG(firm_assets,1) - LAG(firm_assets,2) - LAG(firm_debt_stock,1) + LAG(firm_debt_stock,2) - LAG(firm_deficitsurplus,1)
  
  #capitalist_income <- sum2(Excess.Money[ Excess.Money < 0]) + capitalist_income
  
  
  return(capitalist_income)
  
}
addfunABM(name)

name <- atualizar_capitalist_disposable_income <- function(capitalist_income,gov_ir){
  
  # Variáveis
  capitalist_income <- VAR(capitalist_income)
  gov_ir <- VAR(gov_ir)
  
  # Função
  
  capitalist_disposable_income <- ifelse( capitalist_income > 0 , (1 - gov_ir)*capitalist_income, capitalist_income)
  
  return(capitalist_disposable_income)
  
}
addfunABM(name)


name <- atualizar_capitalist_consumption <- function(capitalist_disposable_income,capitalist_riqueza){
  
  capitalist_disposable_income <- VAR(capitalist_disposable_income)
  capitalist_consumo <- 0
  
  if( capitalist_disposable_income > 0){
    
    capitalist_consumo <- pro_consumir_dividends * capitalist_disposable_income + pro_consumir_wealth_cap *  LAG( capitalist_riqueza, 1)
    
    # W = Y - 0.6Y - 0.05 W + W
    # 0.05/(1- 0.6) = Y/W
    # pro_consumir_wealth/(1-pro_consumir_dividends) Y/W
    
    
  }else{
    
    if( LAG( capitalist_riqueza, 1) > 0  ){
      
      capitalist_consumo <- pro_consumir_wealth_cap *  LAG( capitalist_riqueza, 1)
      
    }
  }
  
  return(capitalist_consumo)
}
addfunABM(name)

name <- atualizar_capitalist_riqueza <- function(capitalist_income,capitalist_consumo,firm_cash,firm_leverage){
  
  #num_maq <- LAG(firm_machines,2)
  #firmk_price <- LAG(firmk_price,1)
  #agg_empresas_falidas <- LAG(agg_empresas_falidas,1)
  
  firm_debt_pos_dist <- - LAG(firm_deficitsurplus,1) + LAG(firm_debt_stock,1) + 1.1 * VAR(firm_interest) * LAG(firm_debt_stock,1)
  
  
  dividendos <- VAR(capitalist_dividendos) # No caso dos dividendos forem negativos. Isto é, se o capitalista injetar $ nas firmas 
  if(dividendos >= 0) dividendos <- 0 # Se dividendos positivos, então anular, pq já foi contabilizado na renda disponível
  
  capitalist_riqueza <- VAR(capitalist_disposable_income) - VAR(capitalist_consumo)  + dividendos +
    0.5 * (sum(VAR(firm_demand_disp)) - sum2(VAR(firm_vendas)*VAR(firm_price))) + LAG(capitalist_riqueza,1) - 
    sum2(ifelse( LAG(firm_leverage,1) > 1 , LAG(firm_debt_stock,1), 0 ))
  
  #0.5*sum2(ifelse( LAG(firm_leverage,1) > 1 , ( firm_debt_pos_dist + 10 * LAG(firm_custo,1) + 10 * LAG(firmk_price,1) ), 0 ))
  #+ #+ # Renda que não foi gasta entra na riqueza do capitalista. 
  #- ifelse( any( LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0) , 3 * sum2( VAR(firm_expdem)[ LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0])/prod_machine * firmk_price ,0 )  + 
  #- ifelse( any( LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0) , 4 * sum2( VAR(firm_custo)[LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0 ]), 0) +
  #+ ifelse( any( LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0) , sum2( LAG(firm_cash,1)[ LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0] ) , 0) - # Saída de recursos aplicados em nova empresa, entrada vindos da empresa que faliu
  #- ifelse( any( LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0) , sum2( (1 - 1/temp_depreci)*LAG(firm_debt_stock,1)[ LAG(firm_leverage,1) > 1 | VAR(firm_ms) == 0] ) , 0) +
  # ifelse( LAG(capitalist_riqueza,1)/LAG(agg_nom_demand,1) < 0.5 , LAG(agg_nom_demand,1) * 2 , 0)
  # sum2(VAR(firm_price) * VAR(firm_vendas_nr)) # Renda que não foi gasta entra na riqueza do capitalista. 
  
  return(capitalist_riqueza)
}
addfunABM(name)

# Banco central

name <- atualizar_interest_rate <- function(agg_inflation,agg_unemployment_rate){
  
  
  cb_ir1 <- (1- inertia_cb) * (r + LAG(agg_inflation,1) + par_reac_infla*((1 + LAG(agg_inflation,1) - meta_inflacao )^2-1) - par_reac_emprego * ((1 +  LAG(agg_unemployment_rate,1) - meta_emprego)^2-1 ))
  
  #cb_ir1 <- (1- inertia_cb) * (r + LAG(agg_inflation,1) + VAR(cb_par_reac_infla)*((1 + LAG(agg_inflation,1) - meta_inflacao )^2-1) )
  
  #cb_ir1 <- (1- inertia_cb) * (r + LAG(agg_inflation,1) + par_reac_infla*(LAG(agg_inflation,1)/meta_inflacao - 1 ) - par_reac_emprego * (LAG(agg_unemployment_rate,1)/meta_emprego-1 ) )
  
  #cb_ir1 <- (1- inertia_cb) * (r + LAG(agg_inflation,1) + par_reac_infla*( ( 1 + LAG(agg_inflation,1) - meta_inflacao )^2 - 1 ) )
  
  cb_ir2 <- inertia_cb * LAG(cb_ir,1)
  
  cb_ir <- cb_ir1 + cb_ir2
  
  if(cb_ir < 0) cb_ir <- 0
  
  #cb_ir <- ( 1 + par_reac_infla*( LAG(agg_inflation,1) - meta_inflacao ) ) * LAG(cb_ir,1)
  
  #if(cb_ir < 0) cb_ir <- 0
  
  #if(LAG(agg_nom_demand,1)/(sum2( LAG(firm_price,1) * LAG(firm_capacidade_producao,1))) > 1 ) cb_ir <- 0
  
  return(cb_ir)
  
}
addfunABM(name)

name <- atualizar_reac_bc <- function(agg_inflation,cb_par_reac_infla){
  
  #cb_ir1 <- (1- inertia_cb) * (r + LAG(agg_inflation,1) + par_reac_infla*((1 + LAG(agg_inflation,1) - meta_inflacao )^2-1) - par_reac_emprego * ((1 +  LAG(agg_unemployment_rate,1) - meta_emprego)^2-1 ))
  
  cb_par_reac_infla <- ifelse( abs((1 + LAG(agg_inflation,1) - meta_inflacao)^4-1) > 0.03 ,  ( 1 + 0.05 * sqrt(( LAG(agg_inflation,1) - meta_inflacao )^2)) * LAG(cb_par_reac_infla,1) , 0.999 * LAG(cb_par_reac_infla,1))
  
  cb_par_reac_infla[ cb_par_reac_infla < 0.5 ] <- 0.5
  
  return(cb_par_reac_infla)
  
}
addfunABM(name)

# Governo 

name <- atualizar_gov_exp <- function(gov_unem_ben,workers_empregador,gov_consumption){
  
  auxio <- rep(0,quantidade_workers)
  
  auxio[ VAR(workers_empregador) == 0 ] <- VAR(gov_unem_ben)
  
  gov_expense <- sum2( auxio ) + VAR(gov_consumption)
  
  return(gov_expense)
  
}
addfunABM(name)

name <- atualizar_gov_con <- function(agg_unemployment_rate, agg_inflation){
  
  gov_consumption_base <- (1 + (LAG(agg_inflation,1))) * LAG(gov_consumption,1)
  
  # 
  # up <- meta_emprego + 0.025
  # inf <- meta_emprego - 0.025
  # 
  # 
  # if( LAG(agg_unemployment_rate,1) > up  ){  gov_consumption <-  ( 1 + reac_gov*(( 1 + LAG(agg_unemployment_rate,1) - up)^2-1) - reac_gov_infla * ((1 + LAG(agg_inflation,1) - meta_inflacao )^2 - 1)) * gov_consumption_base  }
  # if( LAG(agg_unemployment_rate,1) <= up &   LAG(agg_unemployment_rate,1) >= inf ){  gov_consumption <-  ( 1 - reac_gov_infla * (( LAG(agg_inflation,1) - meta_inflacao )^2 - 1) ) * gov_consumption_base            }
  # if( LAG(agg_unemployment_rate,1) < inf  ){ gov_consumption <- ( 1 + reac_gov*(( 1 + LAG(agg_unemployment_rate,1) - inf)^2-1) - reac_gov_infla * ((1 + LAG(agg_inflation,1) - meta_inflacao )^2 - 1)) * gov_consumption_base  }
  # 
  # #gov_consumption <- ( 1 + reac_gov*( LAG(agg_unemployment_rate,1) - meta_emprego + LAG(agg_inflation,1))) * LAG(gov_consumption,1)
  
  #gov_consumption <- (1-gov_inertia) * -( LAG(cb_ir,1)*LAG(gov_debt,1) + sum2( ifelse( VAR(workers_empregador) == 0 , VAR(gov_unem_ben) , 0 ) ) - LAG(gov_tributos,1)) + gov_inertia * LAG(gov_consumption,1)
  
  
  # gov_consumption <- ( 1 + reac_gov*(( 1 + LAG(agg_unemployment_rate,1) - meta_emprego)^2-1) - reac_gov_infla * ((1 + LAG(agg_inflation,1) - meta_inflacao )^2 - 1)) * gov_consumption_base
  # 
  # gov_consumption <- ( 1 + reac_gov*(( LAG(agg_unemployment_rate,1) / meta_emprego -1 )) - reac_gov * ((LAG(agg_inflation,1)/ meta_inflacao - 1))) * gov_consumption_base
  
  
  if( abs( LAG(agg_unemployment_rate,1) - meta_emprego ) > 0.005 ){

    gov_consumption <- ( 1 + reac_gov*(( LAG(agg_unemployment_rate,1) - meta_emprego )) ) * gov_consumption_base

  }else{

    gov_consumption <- gov_consumption_base

  }
  
  
  # if(VAR(time_t) > 15){ gov_consumption <- gov_consumption_base * ( 1 - reac_gov * (LAG(gov_consumption,1)/LAG(agg_nom_demand,1) - 0.2)) }else{
  # 
  #   gov_consumption  <- gov_consumption_base
  # 
  # }
  
  #gov_consumption <- ifelse( LAG(agg_nom_demand,1)/sum2(LAG(firm_capacidade_producao,1) * LAG(firm_price,1)) > 1 , gov_consumption_base*0.95, gov_consumption)
  
  return(gov_consumption)
  
}
addfunABM(name)

name <- atualizar_gov_unem_ben <- function(agg_inflation){
  
  gov_unem_ben <- par_unem.ben * ( 1 + LAG(agg_inflation,1)) * mean(LAG(workers_salario,1)[ LAG(workers_salario,1) > 0])
  
  return(gov_unem_ben)
  
}
addfunABM(name)

name <- atualizar_gov_tributos <- function(capitalist_income,workers_income){
  
  if(VAR(capitalist_income) > 0){
    
    gov_tributos <- VAR(gov_ir)*( sum2(VAR(workers_income)) + VAR(capitalist_income) )
    
  }else{
    
    gov_tributos <- VAR(gov_ir)*(sum2(VAR(workers_income)))
    
  }
  
  if(gov_tributos < 0) gov_tributos <- 0
  
  return(gov_tributos)
  
}
addfunABM(name)

name <- atualizar_gov_txa_tributos <- function(gov_deficit,agg_nom_demand,gov_ir){
  
  Demand <- LAG(agg_nom_demand,1)
  Deficit <- LAG(gov_deficit,1)
  gov_ir <- LAG(gov_ir,1)
  
  # up <- meta_emprego + 0.025
  # inf <- meta_emprego - 0.025
  # 
  # if(VAR(time_t) > 10){ 
  #   
  #   if(abs(LAG(agg_unemployment_rate,1) - meta_emprego) > 0.005){
  #     
  #     gov_ir <- (1 - reac_gov*(LAG(agg_unemployment_rate,1) - meta_emprego))*gov_ir
  #     
  #   }else{
  #     
  #     gov_ir <- gov_ir 
  #   }
  #   
  # }
  # #   if( LAG(agg_unemployment_rate,1) > up) gov_ir <- (1 - 0.05)*gov_ir
  # #   if(LAG(agg_unemployment_rate,1) < inf) gov_ir <- (1 + 0.05)*gov_ir
  # # }
  # # 
  # # gov_ir <- ifelse( gov_ir > 0.5 , 0.5 , gov_ir )
  # # gov_ir <- ifelse( gov_ir < 0.05 , 0.05 , gov_ir )
  
  return(gov_ir)
  
}
addfunABM(name)

name <- atualizar_gov_deficit <- function(gov_tributos,gov_expense,cb_ir,gov_debt,cb_profit){
  
  taxes <- VAR(gov_tributos)
  profit_central_bank <- VAR(cb_profit)
  gover_expenses <- VAR(gov_expense)
  basic_interest_rate <- VAR(cb_ir)
  gov_debt_stock <- LAG(gov_debt,1) 
  
  gov_deficit <- - taxes - profit_central_bank + gover_expenses + basic_interest_rate*gov_debt_stock - 0.5 * (sum(VAR(firm_demand_disp)) - sum2(VAR(firm_vendas)*VAR(firm_price)))
  
  return(gov_deficit)
  
}
addfunABM(name)

name <- atualizar_gov_debt_stock <- function(gov_debt,gov_deficit){
  
  firm_debt_pos_dist <- LAG(firm_debt_stock,1) 
  
  gov_debt <- VAR(gov_deficit) + LAG(gov_debt,1) #+ 
    + 0.5*sum2(ifelse( LAG(firm_leverage,1) > 1 , ( firm_debt_pos_dist ), 0 ))
  
  # Aqui acima estava assumindo que o governo assumia metade do prejuízo da firma
  
  return(gov_debt)
  
}
addfunABM(name)

# Funções agregadas

name <- atualizar_inflation <- function(firm_price){
  
  if( VAR(time_t) > 1 ){
    
    lag_vendas <- LAG(firm_vendas,1)
    vendas <- VAR(firm_vendas)
    
    agg_inflation <- sum2( VAR(firm_price) * vendas / sum2(vendas) )/ sum2( LAG(firm_price,1) * lag_vendas/sum2(lag_vendas) ) -1
    
  }else{ agg_inflation <- 0  }
  
  agg_inflation <- round(agg_inflation,5)
  
  return(agg_inflation)
}
addfunABM(name)

name <- index_precos <- function(agg_inflation){
  
  agg_index_prices <- (1 + VAR(agg_inflation)) * LAG(agg_index_prices,1)
  
  return(agg_index_prices)
}
addfunABM(name)

name <- atualizar_agg_demand <- function(workers_consumo, capitalist_consumo,gov_consumption){
  
  agg_nom_demand <- sum2(VAR(workers_consumo)) + VAR(capitalist_consumo) + VAR(gov_consumption) + sum2(VAR(firm_investimento) + VAR(firm_RD_invest))
  
  return(agg_nom_demand)
}
addfunABM(name)

name <- compute_gdp <- function(agg_nom_demand,agg_index_prices){
  
  agg_gdp <- VAR(agg_nom_demand)/VAR(agg_index_prices)
  
  return(agg_gdp)
}
addfunABM(name)

name <- atualizar_agg_unem <- function(workers_empregador){
  
  agg_unemployment_rate <- 1 - length( VAR(workers_empregador)[ VAR(workers_empregador) != 0])/quantidade_workers
  
  return(agg_unemployment_rate)
}
addfunABM(name)

name <- compute_investment <- function(firm_investimento,firm_RD_invest,agg_index_prices){
  
  agg_investment <- (sum2(VAR(firm_investimento) + VAR(firm_RD_invest)))/VAR(agg_index_prices)
  
  return(agg_investment)
}
addfunABM(name)

name <- compute_consumption <- function(workers_consumo,capitalist_consumo,agg_index_prices){
  
  agg_consumption <- (sum2(VAR(workers_consumo)) + VAR(capitalist_consumo))/VAR(agg_index_prices)
  
  return(agg_consumption)
}
addfunABM(name)

name <- agg_real_wage <- function(workers_consumo,capitalist_consumo,agg_index_prices){
  
  wages <- VAR(workers_salario)
  wages <- mean( wages[ VAR(workers_empregador) != 0]   )
  
  agg_real_wage <- wages/VAR(agg_index_prices)
  
  return(agg_real_wage)
}
addfunABM(name)

name <- agg_real_wage_Var <- function(workers_consumo,capitalist_consumo,agg_index_prices){
  
  agg_var_real_wage <- VAR(agg_real_wage)/LAG(agg_real_wage,1) - 1
  
  return(agg_var_real_wage)
}
addfunABM(name)

name <- goverment_consumption <- function(gov_consumption,agg_index_prices){
  
  gov_consumption_real <- VAR(gov_expense)/VAR(agg_index_prices)
  
  return(gov_consumption_real)
}
addfunABM(name)

name <- agg_labor_prod <- function(gov_consumption,agg_index_prices){
  
  agg_labor_prod <- 0
  
  if(sum2(VAR(firm_tot_funcionarios)) > 0){
    
    agg_labor_prod <- sum2(VAR(firm_producao))/sum2(VAR(firm_tot_funcionarios))
    
  }
  
  return(agg_labor_prod)
}
addfunABM(name)

name <- agg_capacity_util_function <- function(gov_consumption,agg_index_prices){
  
  agg_capacity_util <- sum2(VAR(firm_producao))/sum2(VAR(firm_machines) * prod_machine)
  
  return(agg_capacity_util)
}
addfunABM(name)

# BANK

name <- atualizar_interet_income <- function( firm_fb , gov_debt ){
  
  bank_receita <- sum2( LAG(firm_debt_stock,1) * VAR(firm_interest) ) + VAR(cb_ir) * LAG(gov_debt,1)
  
  return(bank_receita)
}
addfunABM(name)

name <- atualizar_deposits <- function(bank_deposits,capitalist_riqueza,workers_riqueza,capitalist_cash_percent,firmk_profit){
  
  # Nessa função, eu consido os depósitos existentes no período anterior! 
  
  LAG(bank_deposits,1) # Para criar matriz de lag maior
  
  # Variáveis
  
  cash_percente <- VAR(capitalist_cash_percent)
  cap_riqueza <- VAR(capitalist_riqueza)
  workers_riqueza <- sum2(VAR(workers_riqueza))
  firmk_lucro <- VAR(firmk_profit)
  
  # Fórmulas
  
  #bank_deposits <- (1 - cash_percente) * cap_riqueza + workers_riqueza +
  #  ifelse((VAR(firmk_profit))>0,VAR(firmk_profit),0)
  
  # Supondo que apenas os capitalistas mantem depósitos
  
  bank_deposits <- (1 - cash_percente) * cap_riqueza + ifelse((VAR(firmk_profit))>0,VAR(firmk_profit),0)
  
  return(bank_deposits)
  
}
addfunABM(name)

name <- atualizar_emprestimos_banco_bc <- function(bank_deposits,firm_debt_stock){
  
  # Variáveis
  bank_deposits <- VAR(bank_deposits) - LAG(bank_deposits,1)
  bank_lend <- sum2(VAR(firm_debt_stock) - LAG(firm_debt_stock,1)) - ifelse((VAR(firmk_profit) )<0,VAR(firmk_profit),0)# + 
  # + ifelse(LAG(firmk_profit,1)<0,LAG(firmk_profit,1),0)
  # Bank lend: variação do estoque de dívida das firmas. Se o lucro for negativo, deve ter um aumento POSITIVO da dívida.
  bank_debt <- VAR(gov_debt) - LAG(gov_debt,1)
  
  bank_emprestimos_bc <- ( bank_lend + bank_debt - bank_deposits ) + ( - VAR(bank_profit) + LAG(bank_profit,1) ) + LAG(bank_emprestimos_bc,1)
  
  #bank_emprestimos_bc <- VAR(gov_debt) + sum2(VAR(firm_debt_stock)) - ifelse(VAR(firmk_profit)<0,VAR(firmk_profit),0) - VAR(bank_deposits) + ( - VAR(bank_profit) + LAG(bank_profit,1) )
  
  # Lag do bank profit entra como +. SE o banco teve um lucro POSITIVO em t-1, ele é distribuído ao capitalista. Saída de caixa = aumento do empréstimo.
  # Se o banco teve um prejuízo, ele é capitalizado inteiramente pelo capitalista. 
  # Resultado presente do lucro do banco, vira caixa. Que por sua vez, precisa de uma compensação no passivo, aumentando o empréstimo.
  # Já o lucro passado é debitado do caixa, pq se tornou dividendo. Assim, se resultado é positivo, vira um aumento do empréstimo.
  # Se ficar negativo, significa que o banco está depositando no banco central.
  
  return(bank_emprestimos_bc)
}
addfunABM(name)

name <- atualizar_pgto_juros_depositantes <- function(bank_deposits,cb_ir){
  
  # Variáveis
  
  bank_deposits <- ( 1 - LAG(capitalist_cash_percent,1))  * LAG( capitalist_riqueza, 1) + sum(LAG(workers_riqueza,1))
  
  # Fórmulas
  
  # Só paga juros para o capitalista e trabalhadores.
  # Não paga para a empresa produtora de bens de capital para não complicar. 
  
  bank_pgto_juros <- par_rate_deposits * VAR(cb_ir) * bank_deposits
  
  
  return(bank_pgto_juros)
}
addfunABM(name)

name <- atualizar_bank_profit <- function(bank_pgto_juros,cb_ir,bank_emprestimos_bc,bank_receita){
  
  # Variáveis
  bank_pgto_juros <- VAR(bank_pgto_juros)
  bank_emprestimos_bc <- LAG(bank_emprestimos_bc,1)
  bank_receita <- VAR(bank_receita)
  
  bank_profit <- bank_receita - bank_pgto_juros - VAR(cb_ir)*bank_emprestimos_bc
  
  return(bank_profit)
}
addfunABM(name)

name <- atualizar_bc_bank_profit <- function(cb_ir,bank_emprestimos_bc){
  
  # Variáveis
  
  bank_emprestimos_bc <- LAG(bank_emprestimos_bc,1)
  
  cb_profit <- VAR(cb_ir)*bank_emprestimos_bc # Se ficar negativo, significa que o BC está tendo uma perda. 
  # Perda, pq o banco privado está depositando dinheiro no banco, não emprestando do banco central
  
  return(cb_profit)
}
addfunABM(name)

# Firma produtora de bens de capital

name <- atualizar_firmk_labor_demand <- function(firm_machine_order){
  
  orders_received <- LAG(firm_machine_order,1) + LAG(firm_machine_order,2) + LAG(firm_machine_order,3) + LAG(firm_machine_order,4) +
    LAG(firm_machine_order,5)
  
  firmk_labor_demand <- ceiling( sum2(orders_received)/(5*firmk_lb_prod) + trab.fixos.bensk)
  
  return(firmk_labor_demand)
}
addfunABM(name)

name <- firmk_production_1 <- function(firm_machine_order,lista_funcionarios){
  
  VAR(firm_machines_dp) # Marcador para rodar a depreciação das máquinas antes de realizar a distribuição de novas 
  
  firmk_production <- 0
  
  orders_received <- VAR(firm_machine_order)
  
  if( sum2(lista_funcionarios[[quantidade_firmas+1]][2,]) > 0 ){ # Checa número de funcionários da firma
    # Entra no ciclo de produção da firma. O produz o limite possível ou produz o total dado pelas encomendas
    
    firmk_production <-  firmk_lb_prod * ( sum2(lista_funcionarios[[quantidade_firmas+1]][2,]) - trab.fixos.bensk )
    
    firmk_production <- ifelse(  sum2(orders_received) > firmk_production , firmk_production  ,  sum2( orders_received  ))
    
  }
  
  # Condições inicias do ciclo
  tot_produced <- firmk_production
  dist <- 0
  
  # Salva lista de máquinas dentro da função
  
  lista_maquinas <- lista_maquinas
  
  if( firmk_production > 0 ){ # Se a produção foi positova
    if( sum2(orders_received)> 0 ){ # Se a lista de pedidos é maior do que um
      
      for( i in 2:length( lista_orders_machines[[1]][1,] )){ # Para cada um dos pedidos presentes na lista
        
        if( lista_orders_machines[[1]][2,i] <= (tot_produced - dist) ){ # Se o total produzido menos distribuiído é maior do que o pedido
          
          new_machines <- matrix( rep( c(temp_depreci,prod_machine,rel.k.l) , lista_orders_machines[[1]][2,i] ) , 3 , lista_orders_machines[[1]][2,i] ) # Número de máquinas novas igual do pedido
          
          dist <- dist + length(new_machines[1,]) # Adiciona ao número distribuido
          
          lista_maquinas[[ lista_orders_machines[[1]][1,i] ]] <- cbind( lista_maquinas[[ lista_orders_machines[[1]][1,i] ]] , new_machines )  # Adiciona na lista de máquinas da firma
          
          # lista_orders_machines[[1]][2,i] <- 0 # Zera o número do pedidos registrado na lista de pedidos
          
          # firm_machine_order[ lista_orders_machines[[1]][1,i],1 ] <- 0 # Zera o número do pedidos registrado pela firma em "firm_machine_order"
          
          if(dist == tot_produced) break; # Se o total distríbuido já igual ao produzido. Parar
          
        }else{ # Se o número de bens ainda disponíveis é menor do que o total do pedido novo
          
          new_machines <- matrix(  rep( c(temp_depreci,prod_machine,rel.k.l) ,tot_produced - dist  ) , 3 , (tot_produced - dist) )
          
          lista_maquinas[[ lista_orders_machines[[1]][1,i] ]] <- cbind(lista_maquinas[[ lista_orders_machines[[1]][1,i] ]],new_machines)
          
          # lista_orders_machines[[1]][2,i] <- lista_orders_machines[[1]][2,i] - (tot_produced + dist)
          
          dist <- dist + (tot_produced - dist)
          
          # firm_machine_order[ lista_orders_machines[[1]][1,i]   ,1] <-  lista_orders_machines[[1]][2,i] - tot_produced + dist
          
          if(dist == tot_produced) break;
        }
      } # Final distribuição
      
      
      
      # Limpeza da lista de pedidos
      # Seleciona a primeira linha da lista com pedidos ativos
      #second_part <- lista_orders_machines[[1]][1,][ lista_orders_machines[[1]][2,1:length(lista_orders_machines[[1]][2,])] != 0 ]
      # Seleiciona a segunda linha com pedidos ativos
      #first_part <- lista_orders_machines[[1]][2,][ lista_orders_machines[[1]][2,1:length(lista_orders_machines[[1]][2,])] != 0 ]
      # Junta ambas as listas
      #third_part <- matrix(rbind(second_part , first_part) , 2 , length(rbind(second_part , first_part))/2 )
      # Adiciona o primeiro termo na lista que é nulo (para manter uma matriz)
      #four <- cbind( matrix(c(0,0),2,1) , third_part )
      # Se a lista tiver se tornado nula, para manter o tipo de matriz:
      #if(length(four)==2){
      #  
      #  four <- matrix(c(0,0),2,1)
      #  
      #}
      
      # Assing a lista de pedidos para o ambiente global
      #lista_orders_machines <- list(four)
      
      #assign("lista_orders_machines",lista_orders_machines,envir = where("simulationABMFAST2"))
      
      # Assign a variável de pedidos ativos
      #assign("firm_machine_order",firm_machine_order,envir = where("simulationABMFAST2"))
      
      
    }
  }
  
  # Criação de máquinas para o caso de firmas novas após falÊncia
  
  #if( any( LAG(firm_ms,1) == 0 ) ){
  #  
  #  identificador <- which( LAG(firm_ms,1) == 0 )
  #  
  #  for(z in 1:length(identificador)){
  #    
  #    lista_maquinas[[ identificador[z] ]] <- matrix( rep( c(temp_depreci,prod_machine,rel.k.l) , ceiling(10/prod_machine) ) ,
  #                                                    3 , ceiling(10/prod_machine) )
  #    
  #  }
  #}
  
  # Assign nova lista de máquinas das firmas 
  
  assign("lista_maquinas",lista_maquinas,envir = where("simulationABMFAST2"))
  
  return(firmk_production)
  
}
#addfunABM(name)

name <- firm_new_machines <- function(firm_machine_order,lista_funcionarios){
  
  firmk_production <- 0
  
  VAR(workers_empregador) # Flag para rodar primeiro no mercado de trabalho
  
  orders_received <- LAG(firm_machine_order,1)
  
  firm_new_machines <- rep(0,quantidade_firmas)
  
  if( sum2(orders_received) > 0 ){
    
    if( sum2(lista_funcionarios[[quantidade_firmas+1]][2,]) > 0 ){ # Checa número de funcionários da firma
      # Entra no ciclo de produção da firma. O produz o limite possível ou produz o total dado pelas encomendas
      
      firmk_production <-  firmk_lb_prod * ( sum2(lista_funcionarios[[quantidade_firmas+1]][2,]) - trab.fixos.bensk )
      
      firmk_production <- ifelse(  sum2(orders_received) > firmk_production , firmk_production  ,  sum2( orders_received  ))
      
    }
    
    # Distribuição das máquinas
    
    firm_new_machines <- floor(( orders_received/sum2(orders_received) * firmk_production ))
    
  }
  
  return(firm_new_machines)
  
}
addfunABM(name)

name <- firmk_production_2 <- function(firm_machine_order,lista_funcionarios){
  
  # Distribuição das máquinas
  
  firmk_production <- sum2(VAR(firm_new_machines))
  
  return(firmk_production)
  
}
addfunABM(name)

name <- atualizar_firmk_price <- function(firmk_production,workers_salario){
  
  # wage_cost <- 0
  # 
  # #if( length(lista_funcionarios[[quantidade_firmas+1]][1,]) > 1){
  # #  for(i in 2:length(lista_funcionarios[[quantidade_firmas+1]][1,])){
  # #    
  # #    pos <- lista_funcionarios[[quantidade_firmas+1]][1,i]
  # #    wage_cost <- workers_salario[pos,1] + wage_cost
  # #    
  # #  }
  # #}
  # #
  # #firmk_price <- 1
  # #  
  # #  if(VAR(firmk_production) > 0){
  # #    
  # #    firmk_price <- 2 * wage_cost/VAR(firmk_production)
  # #    
  # #  }
  # 
  # markup <- sum2(LAG(firm_fixed.markup,1)*LAG(firm_ms,1))
  # 
  # M <- multiplicador_prod_setor_capital/20 * 1/1.5 * prod_machine * temp_depreci * (1 + markup)
  # 
  # M <- 3 * (1 + markup)
  # 
  # #markup <- 0.25
  # 
  # firmk_lb_prod <- 1/VAR(firm_coef_tecnico)[1] * multiplicador_prod_setor_capital
  # 
  # firmk_price <- M * VAR(firmk_wage_ref)/firmk_lb_prod 
  
  
  firmk_price <- prod_machine * grau_utilizao_desejado * 1/(1 - VAR(cb_ir)) * mean(LAG(firm_price,1)) * (1 + LAG(agg_inflation,1))
  
  
  #firmk_price <- VAR(agg_index_prices)
  
  return(firmk_price)
  
}
addfunABM(name)

name <- atualizar_firmk_wage_ref <- function(firmk_production,firm_machine_order){
  
 #  lag_firmk_wage <- LAG(firmk_wage_ref,1)
 #  
 #  ordens <- sum2(LAG(firm_machine_order,2))
 #  prod <- LAG(firmk_production,1)
 #  
 # # firmk_wage_ref <- 1.5 * mean(LAG(firm_wage_ref,1)) * ( 1 + LAG(agg_inflation,1))
 #  
 #  if(LAG(firmk_production,1) > 0){
 #  
 #    firmk_wage_ref <- (1 + par_var_wages * (sum(LAG(firm_machine_order,1))/LAG(firmk_production,1) - 1.02 )) * LAG(firmk_wage_ref,1)
 #  
 #  }else{
 #    
 #    firmk_wage_ref <- (1 + par_var_wages * ( sum(LAG(firm_machine_order,1)) * 0.01 - 0.02 )) * LAG(firmk_wage_ref,1)
 #    
 #  }
 #  
  
  firmk_wage_ref <-  VAR(firmk_price) * firmk_lb_prod/(1 + firmk_lb_prod/(1/firm_coef_tecnico[1,1]) * 0.5 * mean(VAR(firm_fixed.markup)))
  
  return(firmk_wage_ref)
  
}
addfunABM(name)

name <- firmk_receita <- function(firmk_production,firmk_price){
  
  firmk_receita <- VAR(firmk_production) * VAR(firmk_price)
  
  return(firmk_receita)
  
}
addfunABM(name)

name <- firmk_custo <- function(workers_empregador,workers_salario){
  
  empregador <- VAR(workers_empregador)
  workers_salario <- VAR(workers_salario)
  
  firmk_cost <- sum2(workers_salario[ empregador == quantidade_firmas+1])
  
  return(firmk_cost)
}
addfunABM(name)

name <- firmk_profit <- function(firmk_cost,firmk_receita){
  
  firmk_profit <- VAR(firmk_receita) - VAR(firmk_cost)  #- LAG(firmk_profit,1)
  # O lag é o pagamento de dividendo ou a transferência dos capitalsitas para a empresa.
  
  return(firmk_profit)
}
addfunABM(name)

# Depreciação

aux_depreciation <- function(i){
  
  lista_maquinas[[i]][1, ] <- lista_maquinas[[i]][1, ] - 1
  
  condition <- lista_maquinas[[i]][1,] > 0
  
  len <- length( lista_maquinas[[i]][1,][  lista_maquinas[[i]][1,] > 0  ] )
  
  if(len < 2){
    
    lista_maquinas[[i]] <- matrix( c(temp_depreci,prod_machine,rel.k.l) , 3, 1)
    
  }else{
    
    lista_maquinas[[i]] <-  lista_maquinas[[i]][1:3,condition]
  }
  
  return( lista_maquinas[[i]] )
  
}

name <- depreciation_machines <- function(lista_maquinas,firm_machines){
  
  firm_machines <- LAG(firm_machines,1) # Maq ANTES
  firm_machines_pos <- firm_machines # Maq DEPOIS
  
  for(i in 1:quantidade_firmas){
    
    lista_maquinas[[i ]][1,] <- lista_maquinas[[i]][1,] - 1
    
    m <- matrix(lista_maquinas[[i]],3)
    
    lista_maquinas[[i ]] <- matrix( m[ , m[1,] > 0] , 3) # Selecção de todas as colunas em que, na linha 1, o valor é maior do que 0
    
    if( length( lista_maquinas[[i ]][1,] ) < 2 ){
      
      lista_maquinas[[i]] <- matrix( c(temp_depreci,prod_machine,rel.k.l) , 3, 1)
      
    }
    
    firm_machines_pos[i] <- length( lista_maquinas[[i]][1,] )
    
  }
  
  #lista_maquinas <- lapply(1:quantidade_firmas,aux_depreciation)
  
  assign("lista_maquinas",lista_maquinas,envir=where("simulationABMFAST2"))
  
  # for(i in 1:quantidade_firmas){
  #   
  #   firm_machines_pos[i] <- length( lista_maquinas[[i]][1,] )
  #   
  # }
  
  firm_machines_dp <- firm_machines - firm_machines_pos # Maq.ANTES - Maq.DEPOIS = Maq.Depreciadas
  
  return(firm_machines_dp)
}
#addfunABM(name)

# Controle de tempo

name <- control_time <- function(time_t){
  
  time_t <- VAR(time_t) + 1
  
  return(time_t)
}
addfunABM(name)

#   Cc + Cw + Sw + Sc + G = W + Profits + Dist. + T


# Distribuição de renda 
name <- functional_distribut <- function(capitalist_disposable_income,workers_disposable_income,gov_tributos,cb_profit){
  
  #total_income <- VAR(capitalist_disposable_income) + sum2(VAR(workers_disposable_income)) + VAR(gov_tributos) + VAR(cb_profit)
  
  #workers_income <- sum2(VAR(workers_disposable_income))/total_income
  #VAR(capitalist_disposable_income)/total_income
  #(VAR(gov_tributos) + VAR(cb_profit))/total_income
  
  workers_income <- sum2(VAR(workers_disposable_income))/VAR(agg_nom_demand)
  
  agg_function_dist <- workers_income
  
  return(agg_function_dist)
}
addfunABM(name)

# Estrutura do modelo 
check_estrutura_modelo(list_funcoes)



## Comentários: 

# Critério: taxa de lucro
# Aumentar período de ajuste de estratégia

# ---------  Parâmetros----------------------------------------------------------------------------------------------------------------------------
# parâmetros:

# demand parameter - influencia da demanda passada
par.pas.dem <- 0.2 # Dweck = 1 https://www.scielo.br/j/rbe/a/hbWsTLKn4CMxbPVLVq6pNRN/?format=pdf&lang=pt
# Caiani = 0.25
# Andrea, Lílian = média móvel 4 últimos períodos


# Influência market-share passado
market.inertia <- 0.95

# Efeito da qualidade relativa e preço relativo sobre o market-share

effect.qual.mk <- 2
effect.price.mk <- 1
effect.ms.mk <- 0.5

temp_depreci <- 40
parameter_trab_indireto <- 0.01

grau_utilizao_desejado <- 0.8

firm_estoque_desejado_perc <- 0.2
estoque_desejado_perc <- 0.15

quantidade_firmas <- 200
quantidade_workers <- 30 * quantidade_firmas # 40 trabalhadores por firma
periodos_simulacao <- 1

dif_salarial_emp_des <- 0.2

# Financial restriction
financial_restriction <- 0.2  # Quanto maior, menor a restrição

debt_payment_percent_supr <- 0.1 # Quanto a firma usa do surplus em cash para pagar dívida

# Inovação - firm con
porc_gasto_PD <- 0
par_exp_firm_qual <- 0.01 # DO K+S
innov_dificculty <- 0.35 # Quanto menor, mais difícil.
innov_imitation <- 0.3 # Quanto menor, mais difícil

#
par_exp_firm_qual_immitate <- 0.003 # Nelson-Winter
par_exp_firm_qual_innov <- 0.0015 # Nelson-Winter

par_exp_firm_qual_immitate <- 0.03 # K+S
par_exp_firm_qual_innov <- 0.03 # K+S


#par_exp_firm_qual_immitate <- 0 # Do K+S
#par_exp_firm_qual_innov <- 0# Do K+S


# Estratégias de preços
effect.prob.price <- 0
param.persistent.strat <- 0.9 # Do artigo de expectativas 
memory.parameter <- 0.7 # Do artigo de expectativas 

#param.persistent.strat <- 0.8 # Do artigo de expectativas
#memory.parameter <- 0.5 # Do artigo de expectativas

# Parâmetros consumo

pro_consumir_wage <- 0.75
pro_consumir_wealth_workers <- 0.25

pro_consumir_dividends <- 0.5
pro_consumir_wealth_cap <- 0.1


lambda_cash_capitalist <- -2
lambda_zero_cash <- 0.3 # % que ficam em dinheiro

ir <- 0.25 # Imposto de renda

# Parâmetros banco
par_rate_deposits <- 0.5
par_rate_emprestimos <- 0.01
trab.fixos.bensk <- 1

# Banco central
#r <- (1 + 0.02)^(1/4) -1 # real natural rate
r <- (1+ 1/temp_depreci)^(1/4)-1
r <- 0
inertia_cb <- 0.8 # central bank inertia

par_reac_infla = 1.25
meta_inflacao = (1.02)^(1/4)-1
par_reac_emprego = 0
meta_emprego = 0.05

# Wage policy
max_readjustment_wage_ref <- 0.2
#firm.past.cost <- 0.8 # Importância salário período passado para o sala´rio de referência atual
par_behav_workers <- 0.01

# gov_inertica
gov_inertia <- 0.95
reac_gov <- 0.5
reac_gov_infla <- 0.1

# Produtividade máquinas
prod_machine <- 1/(3/2) # Y/K

prod_machine <- 3.5 # FRED : Gross Domestic Product/Current-Cost Net Stock of Fixed Assets: Private: Nonresidential: Equipment

rel.k.l <- 4.16 # K/L

prod_machine * rel.k.l

# GDp per capita: Q4 2010

1000000 * 0.12 

# Y/K : 2010

2.88

# K/L

(1000000 * 0.12 )/2.88


# Produtividade trabalhador empresa produtora de capital

multiplicador_prod_setor_capital <- 40

# Y/K * K/L 
# Invetido = L/Y

coef_tecnico <- (parameter_trab_indireto/prod_machine + 1/(prod_machine * rel.k.l))*1  # L = C * Y # L = k K/prod_machine + Ld | Ld = Y(prod_machine * rel.k.l)

# PRodutividade 
# 1/coef_tecnico
# 
# firmk_lb_prod <- 1/coef_tecnico * multiplicador_prod_setor_capital # Y/L (bens de consumo sector) * Multiplicador
# 
# firmk_lb_prod <- (rel.k.l * 15 )/ temp_depreci

multiplicador_prod_setor_capital <- 3
firmk_lb_prod <- multiplicador_prod_setor_capital * 1/coef_tecnico

multiplicador_prod_setor_capital/20 * 1/1.5 * prod_machine * temp_depreci 


# k K = k Y/prod + Ld = K/Y *Y/L * L
# Y = Y/K * K/L * L
1/(prod_machine * rel.k.l)


# K/Y = 3
# K = 


# CU = coef_tecnico * Wage_médio 

# wL/Y = coef_tecnico igual à inversa da produvidade do trabalho

# L = Ld + Li = Y/a + kK = Y/a + k* Y/v= (1/a + k/v)Y 

# Y/K = v -> Y/v=k


# Conclusão para chegar no ativo desejado

# Parâmetros política de preços

fixed.markup <- (1/effect.price.mk) * (1/(1- market.inertia)) * (1/quantidade_firmas)

par.adj.markup <- 0.04 # Vem do K+S
threshold <- 0.1
par.adj.mk_util <- 0.001

market.inerti <- 0.95

par_unem.ben <- 0.4 # https://www.sciencedirect.com/science/article/pii/S0165188915301020

fixed.markup <- 1 # Para que a distribuição de renda inicial seja 50%, 50%

# Variáveis dos parâmetros da dinâmica do market-share

effect.price.mk <- 3
effect.qual.mk <- 3
effect.pricevar.mk <- effect.qual.mk
effect.delay.mk <- 6
effect.ms.mk <- 0.5
var.change.ms <- 0.1
effect.pricefix.mk <- 0.1

# Política dividendos

#firm_dividendos[ df_antes_dividendo > 0 & LEV == TRUE  ] <- 0.9 * df_antes_dividendo[ df_antes_dividendo > 0 & LEV == TRUE  ]
#firm_dividendos[ df_antes_dividendo > 0 & LEV == FALSE  ] <- 1 * df_antes_dividendo[ df_antes_dividendo > 0 & LEV == FALSE  ]
#firm_dividendos[ df_antes_dividendo < 0 & LEV == TRUE  ] <- 0.6 * df_antes_dividendo[ df_antes_dividendo < 0 & LEV == TRUE   ]
#firm_dividendos[ df_antes_dividendo < 0 & LEV == FALSE  ] <- 0.2 * df_antes_dividendo[ df_antes_dividendo < 0 & LEV == FALSE  ]

div_TT <- 0.9
div_TF <- 1.1
div_FT <- 0.3
div_FF <- 0.1

# Salários

infla_indexation <- 0.85

par_var_salario_max <- (1 + 0.25)^(1/4) - 1

par_var_salario_max = (1-infla_indexation)/(0.5*(1/meta_inflacao + meta_emprego)*(1- meta_emprego))

par.empreg.var <- 0.1

par_limit_wage_ref_firm <- 0.02

### Variáveis de ajuste p/ calibragem

multiplicador_prod_setor_capital <- 3

rel.k.l <- 30
prod_machine <- 3.5
temp_depreci <- 40

coef_tecnico <- (1/(rel.k.l * prod_machine) + parameter_trab_indireto /prod_machine)
firmk_lb_prod <- 1/coef_tecnico * multiplicador_prod_setor_capital


max_readjustment_wage_ref <- 0.1
firm.past.cost <- 0.8



# ---------  Condições iniciais   ---------------------------------------------------------------------------------------------------------------

tinit <- funcao_detectar_maior_lag(list_funcoes) + 1

# Criar matrizes apenas para vari?veis de estado. Se forem par?metros, criar acima


init <- c( ceiling( quantidade_workers*5/quantidade_firmas ) , 
           ceiling(quantidade_workers*5/quantidade_firmas * 1.1) , 
           ceiling(quantidade_workers*5/quantidade_firmas),
           0,
           ceiling(quantidade_workers*5/quantidade_firmas)/grau_utilizao_desejado,
           grau_utilizao_desejado,
           1/quantidade_firmas)


# Condicoes inicias


prod_machine
parameter_trab_indireto


par_var_salario_max <- (1+0.03)^(1/4)-1

taxa_inicio <- meta_emprego # Taxa de desemprego no início
L <- quantidade_workers*(1-taxa_inicio) # Trabalhadores empregados
produtividade <- (1/(coef_tecnico)) # L = Coef Y
producao <-  ceiling( L*produtividade/ quantidade_firmas )# producao por empresa
expdem <- ceiling(producao/(1+estoque_desejado_perc)) #demanda esperada por emrpesa
capacidade_producao <- ceiling(producao/grau_utilizao_desejado) # capacidad de producao
num_maquinas <- ceiling(capacidade_producao/prod_machine) # número de máquinas

# Supondo
price <- 10

fixed.markup <- 0.6

# WAGE = P/(1+MARKUP) * EXPDEMANDA/TRAB.NECESSARIO 

wage <- (price/(1+fixed.markup)*expdem/ceiling( (capacidade_producao/prod_machine) * parameter_trab_indireto + capacidade_producao*grau_utilizao_desejado/(prod_machine * rel.k.l) ))[1]

# PRICE = (1 + markup) * Wage * coef.tecnico

wage = price/((1+fixed.markup)*coef_tecnico)


gov_unem_ben_init <- 0.4 * wage # Valor do parâmetro 

# Custo unit inicial:
custounitinicial <- wage * ceiling( (capacidade_producao/prod_machine) * parameter_trab_indireto + capacidade_producao*grau_utilizao_desejado/(prod_machine * rel.k.l) )[1] / expdem
custounitinicial <- wage * coef_tecnico

estoque <- round(producao - expdem,0)

agg_supply <- sum( producao * quantidade_firmas * price )

gov_ir <- 0.25

ex_demand <- agg_supply - pro_consumir_wage * (1 - gov_ir) * ( wage * quantidade_workers * taxa_inicio + (1 - taxa_inicio) * quantidade_workers * gov_unem_ben_init) # Consumo do governo + cpaitalist

goverment_init_expense <- ex_demand * 0.2

capitalist_renda_disponivel_init <- (1 - gov_ir) * (agg_supply - wage * quantidade_workers * taxa_inicio)

riqueza_accum_past <- (1/pro_consumir_wealth_cap)*(ex_demand - capitalist_renda_disponivel_init * pro_consumir_dividends - goverment_init_expense ) #  ex_demand/10 == consumo governo

capitalits_consumo <- pro_consumir_wealth_cap * riqueza_accum_past + capitalist_renda_disponivel_init * pro_consumir_dividends

riqueza_accum <- capitalist_renda_disponivel_init - capitalits_consumo + riqueza_accum_past


wage * 1.1 * quantidade_firmas * num_maquinas # riqueza acumulada pelo número de máquinas


init <- c( expdem , 
           producao , 
           expdem,
           0,
           capacidade_producao,
           grau_utilizao_desejado,
           1/quantidade_firmas)


#num_maquinas*quantidade_firmas + # Cash firmas

#div_empresas <- num_maquinas
div_governo <- riqueza_accum # Dívida do governo e riqueza dos capitalistas devem ser iguais. São os únicos estoques existentes no sistema.
# Outros agentes o saldo financeiro de ativos e passivos é nulo
riqueza_capitaltias <- riqueza_accum

depositos <- (1 - lambda_zero_cash ) * riqueza_capitaltias
cash_capitalistas <- riqueza_capitaltias - depositos

#emprestimos_bc <- div_empresas + div_governo - depositos

cash_firmas <- num_maquinas*((1+fixed.markup*2)*wage*1.5/firmk_lb_prod)* quantidade_firmas # Cash firms == Debt firms

# Cash firmas = Maq * PreçoMaquinas * QuantidadeFirmas

div_empresas <- cash_firmas
#apply(firm_debt_stock,MARGIN=2,sum)

emprestimos_bc <- cash_firmas + cash_capitalistas # Empréstimos BC = estoque de moeda 

div_governo <- emprestimos_bc - div_empresas + depositos #

cash_firmas + cash_capitalistas == emprestimos_bc

emprestimos_bc + depositos == cash_firmas + div_governo # Cash_firmas == Divi

#div_governo + apply(firm_debt_stock,MARGIN=2,sum)[1:2] - bank_emprestimos_bc - bank_deposits

div_governo == riqueza_capitaltias

# Resolver para a dívida do governo, mais fácil


# Empréstimso BC = estoque de Moeda

# ---------  Criação dos agentes  ############################

# Criação banco
create_agent(name=c("bank"), statevariables = c("receita","deposits","emprestimos_bc",
                                                "pgto_juros","profit"),
             initialvariables = c(0,depositos,
                                  emprestimos_bc,
                                  0,0),
             quantidade_agentes=1
)

# Criação firmas
create_agent(name=c("firm"), statevariables = c("expdem","producao","vendas","vendas_nr",
                                                "capacidade_producao","grau_utilizacao","ms",
                                                "demand_disp",
                                                "assets","leverage","cash",
                                                "price_rule",
                                                "custo_unit",
                                                "estoque",
                                                "quality",
                                                "price","price_dej","price_change",
                                                "mkup.movel","par.adj.mk_util","eff_markup",
                                                "investimento","machines_dp","new_machines",
                                                "machine_order",
                                                "demanda_trabalhadores",
                                                "par.adj.markup","fixed.markup","threshold",
                                                "mkup.movel.ms",
                                                "receita","receita_op","custo","porc_gasto_PD","RD_invest","RD_invest_acum",
                                                "margemprod","estoque_desejado_perc","media_price",
                                                "labor_productivity","labor_demand",
                                                "deficitsurplus","debt_stock","profit","interest","fb",
                                                "money_stock",
                                                "machines",
                                                "dividendos",
                                                "var.cash",
                                                "revise",
                                                "RD_innov_acum","RD_immitate_acum","RD_immitate","RD_innov",
                                                "leverage_meta",
                                                "coef_tecnico",
                                                "prob.cost.approach",
                                                "prob_costbased",
                                                "price_selected",
                                                "quality_selected",
                                                "wage_ref",
                                                "exp_profit",
                                                "profit_rate",
                                                "prob_naive",
                                                "tot_funcionarios",
                                                "E"),
             initialvariables = c(init, # Vetor com variáveis iniciais até o MS
                                  1,#demand disponível
                                  cash_firmas/quantidade_firmas,0.5,cash_firmas/quantidade_firmas, # "assets","leverage","cash"
                                  1, # "price_rule"
                                  custounitinicial, # Custo unit inicial
                                  estoque, # "estoque",
                                  1, # Quality
                                  price,price,0, # Preço, preço desejado e mudança de preço
                                  1,0.001,1, #"mkup.movel","par.adj.mk_util","eff_markup", # Mark-up móvel vem do
                                  0,0,0, # "investimento","machines_dp","new_machines
                                  0, #machine_order
                                  0, # "demanda_trabalhadores"
                                  par.adj.markup,fixed.markup,threshold, # "par.adj.markup","fixed.markup","threshold",
                                  1,# mkup.movel.ms
                                  0,0,wage*coef_tecnico*producao/quantidade_firmas,0.05,0,0, # "receita","receita_op","custo","porc_gasto_PD","RD_invest","RD_invest_acum",
                                  0.1,estoque_desejado_perc,1, # "margemprod","estoque_desejado_perc","media_price",
                                  0,0, # "labor_productivity","labor_demand",
                                  0,cash_firmas/quantidade_firmas,1,0.2,0, # "deficitsurplus","debt_stock","profit","interest","fb",
                                  cash_firmas/quantidade_firmas, #money_stock = debt stock
                                  num_maquinas,
                                  0, 
                                  0,#"var.cash"
                                  1,# Tempo sem revisão
                                  0,0,0,0,
                                  0.5,
                                  coef_tecnico,
                                  0.5,
                                  0.5,
                                  1, # Price selected
                                  1,# Quality selected
                                  wage,# Wage ref
                                  1,#EXP.profit
                                  0.2,
                                  0.25,# Firm_prob_naive 
                                  10,# Total de funcionários inicialmente
                                  1), # Competitividade inicial
             quantidade_agentes=quantidade_firmas
)

# Criação firma produtora de bens de capital
create_agent(name=c("firmk"), statevariables = c("labor_demand","production","price","receita","cost","profit","wage_ref"),
             initialvariables = c(0,0,price,0,0,0,wage),
             quantidade_agentes=1
)


# Criação famílias trabalhadoras
create_agent(name=c("workers"), statevariables = c("income","riqueza","consumo","disposable_income",
                                                   "salario","data_base","juros",
                                                   "prop_consumir_renda","prop_consumir_riqueza",
                                                   "empregador","salario_desej"),
             initialvariables = c(0,0,0,0,
                                  wage,0,0,
                                  pro_consumir_wage,pro_consumir_wealth_workers,
                                  0,wage-0.1),
             quantidade_agentes=quantidade_workers
)

# Criação capitalista
create_agent(name=c("capitalist"), statevariables = c("income","riqueza","consumo","disposable_income",
                                                      "prop_consumir_renda","prop_consumir_riqueza",
                                                      "juros","dividendos",
                                                      "cash_percent"
),
initialvariables = c(0,riqueza_capitaltias,0,0,
                     pro_consumir_dividends,pro_consumir_wealth_cap,
                     0,0,lambda_zero_cash),
quantidade_agentes=1
)

# Riqueza = Ativos disponíveis para o capitalista na forma de Cash e Depósitos

# Criação banco central
create_agent(name=c("cb"), statevariables = c("ir","profit","par_reac_infla"),
             initialvariables = c(0,0,par_reac_infla),
             quantidade_agentes=1
)

# Criação governo 
create_agent(name=c("gov"), statevariables = c("unem_ben","expense","consumption",
                                               "tributos","debt","deficit","ir",
                                               "consumption_real"),
             initialvariables = c(gov_unem_ben_init,0,goverment_init_expense,
                                  0,div_governo,0,ir,0),
             quantidade_agentes=1
)

# Criação variáveis agregadas
create_agent(name=c("agg"), statevariables = c("unemployment_rate","gdp","nom_demand",
                                               "inflation","index_prices","empresas_falidas",
                                               "function_dist","consumption","investment",
                                               "capacity_util","labor_prod","real_wage","var_real_wage"),
             initialvariables = c(0,0,0,meta_inflacao,1,0,0,0,0,0,0,1,0),
             quantidade_agentes=1
)

# Criação variáveis TIME
create_agent(name=c("time"), statevariables = c("t","s"),
             initialvariables = c(0,1),
             quantidade_agentes=1
)



cond_inicial_maquinas <- matrix(ceiling(runif(num_maquinas,5,temp_depreci)),1,num_maquinas) # Tempo de vida máquinas
cond_inicial_maquinas2 <- matrix(prod_machine,1,num_maquinas) # Produtividade máquinas
cond_inicial_maquinas3 <- matrix(rel.k.l,1,num_maquinas) # K/L

cond_inicial_maquinas <- rbind(cond_inicial_maquinas,cond_inicial_maquinas2)
cond_inicial_maquinas <- rbind(cond_inicial_maquinas,cond_inicial_maquinas3)

create_list_objects(name=c("lista_maquinas"), quantidade_firmas, cond_inicial_maquinas)
# 1- Tempo de vida. 2- Produtividade da máquina. 3- Número de trabalhadores que podem manusear

create_list_objects(name=c("lista_funcionarios"), (quantidade_firmas+1), matrix(c(0,0),2,1))
# 1- Identificador. 2- Marcador

create_list_objects(name=c("lista_orders_machines"), 1, matrix(c(0,0),2,1))
# 1- Identificador. 2- Quantidade

create_list_objects(name=c("lista_past.performance"), 1, matrix(rep(1,quantidade_firmas*5),quantidade_firmas,5))

# 5 heuríssticas + probabilidades 

# 1- Performance regra 1 2- Performance regra 2

create_list_objects(name=c("lista_prob.regras"), 1, matrix(rep(c(0.25,0.25,0.25,0.25),quantidade_firmas),4,quantidade_firmas))
# 1- Performance regra 1 2- Performance regra 2

create_list_objects(name=c("list_firms_selected"), 1, matrix(c(0),quantidade_firmas,5))
# 1- Performance regra 1 2- Performance regra 2

#### Expectativas de inflação
create_list_objects(name=c("lista_prob.exp"), 0.25, matrix(rep(c(0.25,0.25,0.25,0.25),quantidade_firmas),4,quantidade_firmas))

create_list_objects(name=c("lista_past.performance.exp"), 1, matrix(rep(100,quantidade_firmas*5),quantidade_firmas,5))


# Checagens: 

check_functions(list_funcoes)

# Condições iniciais

for(y in 1:quantidade_firmas){
  
  X1 <- Sample( which(workers_empregador[,1] == 0) , (L*taxa_inicio)/quantidade_firmas )
  X2 <- rep(1,L/quantidade_firmas)
  
  X <- rbind(X1,X2)
  
  lista_funcionarios[[y]] <- cbind( lista_funcionarios[[y]] , X)
  
  for(i in 1:length(workers_empregador[1,])){
    
    workers_empregador[,i][X] <- y
    
  }
  
}

#assign("lista_funcionarios",lista_funcionarios,envir = where("simulationABMFAST2") )

firm_price_rules <- Sample(1,quantidade_firmas,replace=T)

# firm_price_rule <- matrix(Sample(c(1,2,4),quantidade_firmas,replace=T),quantidade_firmas,1)


# Simulação


periodos_simulacao <- 9

lista_variaveis_salvar <- list("firm_expdem","agg_unemployment_rate","agg_gdp","firm_estoque","firm_custo",
                               "firm_producao","firm_vendas","agg_inflation","cb_ir","agg_nom_demand","firm_machines","firm_receita",
                               "firm_capacidade_producao","firm_machine_order","firmk_production",
                               #"workers_consumo","capitalist_consumo",
                               "capitalist_riqueza",
                               "capitalist_cash_percent",
                               "bank_emprestimos_bc",
                               "bank_deposits",
                               "firm_debt_stock","firm_cash","firm_deficitsurplus","firm_dividendos","firm_interest",
                               "firm_vendas_nr","firm_grau_utilizacao","firm_quality",
                               "firm_price_rule","firm_fixed.markup","firm_par.adj.markup","firm_threshold",
                               "firm_eff_markup","firm_mkup.movel","firm_price_change",
                               "firm_profit",
                               "gov_deficit","gov_debt","firm_leverage","gov_consumption",
                               "cb_ir","agg_index_prices",
                               "firm_assets",
                               "firm_cash",
                               "firmk_price",
                               #"workers_riqueza","firmk_price","firm_price","firm_custo","firm_investimento","firm_labor_demand",
                               "firm_custo","firm_custo_unit","firm_price",
                               "agg_empresas_falidas","agg_function_dist",
                               #"firmk_production","firm_machines_dp","firm_deficitsurplus",
                               "firm_ms",
                               "gov_ir",
                               "firmk_profit",
                               "bank_profit",
                               "firm_fb",
                               "workers_salario",
                               "firm_leverage",
                               "firm_quality",
                               "workers_salario",
                               "firm_receita_op",
                               "firm_prob_costbased",
                               "cb_profit",
                               "gov_tributos",
                               "firm_profit_rate",
                               "firm_leverage_meta",
                               "capitalist_consumo",
                               "firm_labor_demand",
                               "firm_tot_funcionarios",
                               "firm_RD_invest",
                               "firm_investimento",
                               "firm_wage_ref",
                               "agg_consumption","agg_investment",
                               "agg_capacity_util","agg_labor_prod","gov_consumption_real","agg_var_real_wage","gov_expense")


check_variaveis_salvar(lista_variaveis_salvar)

check_estrutura_modelo(list_funcoes) # SEMPRE RODAR ESSA FUNÇÃO. ELA ALTERA A 
# ORDEM DA LISTA DE FUNÇÕES. PRECISO COLOCAR DEPOIS DA FUNÇÃO SIMULAÇÃO

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

############# SIMULAÇÃO INICIAL ##############


lista_variaveis_salvar2 <- list("agg_unemployment_rate","workers_empregador","workers_salario_desej","workers_salario","firm_wage_ref",
                                "firm_profit","firm_assets","agg_inflation")

# Simul

#DEBBUG <- 0 # Roda sem qualquer tipo de DEBBUG. Simulação é rápida
#DEBBUG <- 1 # Roda buscando identificar problemas. Necessita de objeto extra chamado "list.names.check".
#DEBBUG <- 2 # Roda buscando identificar problemas. Ao final calcula tempo de execução de cada função. 

list.names.check <- c("agg_gdp","agg_function_dist","capitalist_riqueza","agg_index_prices","firm_debt_stock")


lista_variaveis_salvar <- list("agg_gdp","agg_consumption","agg_investment","gov_consumption_real","agg_unemployment_rate",
                               "agg_capacity_util","agg_labor_prod","agg_inflation","agg_var_real_wage","cb_ir", 
                               "firm_price","firm_ms","firm_quality","firm_E","agg_empresas_falidas","firm_wage_ref","firm_custo_unit",
                               "firm_profit","firm_assets","firm_expdem","firmk_production","firm_vendas","capitalist_consumo",
                               "capitalist_riqueza","firm_wage_ref","gov_consumption","firm_labor_demand","firm_dividendos",
                               "bank_profit","firm_machine_order","firm_machines","firm_capacidade_producao","firm_leverage",
                               "firm_price_dej","workers_salario","firm_receita","firm_custo","firm_fb","firm_tot_funcionarios",
                               "firm_debt_stock","capitalist_disposable_income","capitalist_income","gov_ir","firm_estoque",
                               "firm_profit","firm_investimento","firm_new_machines","firm_fixed.markup","agg_function_dist",
                               "firm_price_rule","firmk_price","firm_producao",
                               "gov_debt","agg_nom_demand","agg_index_prices","firm_RD_invest","workers_empregador","workers_salario_desej","gov_expense",
                               "firm_threshold","firm_interest")


check_variaveis_salvar(lista_variaveis_salvar)

##############################
# Para modelo se tornar estável, com firmas homogêneas e sem flutuações (se torna um stock-flow chato):
### Set:
# Efeito sobre o markup do market-share = 0
# Distribuição de dividendos = 0.1 * ( dif leverages)* ativo
# Investimento em RD = 0. deixando a Qualdidade igual entre firmas
# Tempo de depreciação = 10000. Investimento em máquinas se torna nulo.
# firm.past.cost <- 1. Tira efeito de dinâmica do custo interno
# max_readjustment_wage_ref = 0 . Ajuste máximo do salário. Mantêm salários iguais
# Não ha qualquer reajuste salarial por conta de inflação passada ou esperada
# Governo ajusta com emprego, BC com inflação
# Meta de alavancagem fixa em 50%.
# Reação do gov precisa ser bem pequena. 0.05 sobre desvios do deseprego. Pensando que é trimestralmente, faz sentido.

# Única fonte de heterogeneidade gerada é pelo diferencial de vendas entre as firmas, por conta da indivisibilidade dos produtos.

##### 1º passo para ciclo = investimento
# Apenas alterando o Tempo de preciação para 40.
# Multiplicador de produtividade e rel.k.l foram as variáveis chave para a calibragem.
# Salário 1.5 acima da média
# Mark-up consistente para manter a participação do investmento, se distribuido uniformemente, igual a 5% 
# Premissa com base em :  Real Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment / GDP
# Parâmetros para gerar resutlados ok
# multiplicador_prod_setor_capital <- 4
# rel.k.l <- 30
# prod_machine <- 3.5
# temp_depreci <- 40

# Pode existir nova camada de heterogeneidade, por conta do acesso diferencial à novas maquinas.

##### 2º passo para ciclo = mudanças de preços

# Alterando a possibilidade de mudanças nos salários por meio max_readjustment_wage_ref e firm.past.cost
# max_readjustment_wage_ref <- 0.1
# firm.past.cost <- 0.8
# par_var_wages <- 2
# infla_indexation <- 0.8

# Aqui é criada nova camada de heterogeneidade entre as firmas, por conta do diferencial de custos.

##### 3º passo para ciclo = investimento em RD

max_readjustment_wage_ref <- 0.1
firm.past.cost <- 0.8
par_var_wages <- 2
infla_indexation <- 0.8


# ____________________________________________________________________________________________________________________


multiplicador_prod_setor_capital <- 1

rel.k.l <- 30
prod_machine <- 3.5
temp_depreci <- 40

coef_tecnico <- (1/(rel.k.l * prod_machine) + parameter_trab_indireto /prod_machine)
firmk_lb_prod <- 1/coef_tecnico * multiplicador_prod_setor_capital

qtfirma_encom <- sum(firm_expdem[,1])/prod_machine/temp_depreci

# Wage policy

max_readjustment_wage_ref <- 0.2
firm.past.cost <- 0.5
par_wage_ref <- 0.1

# Parâmetros consumo

pro_consumir_wage <- 1
pro_consumir_wealth_workers <- 0.1

pro_consumir_dividends <- 0.6
pro_consumir_wealth_cap <- 0.05

par_var_wages <- 0.2
infla_indexation <- 0.8

par.pas.dem <- 0.1

dividend_policy <- 0.8

meta_emprego <- 0.05

effect.qual.mk <- 3
effect.price.mk <- 3
effect.pricevar.mk <- 1
effect.delay.mk <- 6
effect.pricefix.mk <- 0.1

# Innovation 

# Price strategies
var.markup.ms <- 0.2

# Inovação - firm con
porc_gasto_PD <- 0.04

# STR labor market

min.deprec <- 4



####

#lista_variaveis_salvar <- list("agg_gdp","agg_consumption","agg_investment","gov_consumption_real","agg_unemployment_rate",
#                            "agg_capacity_util","agg_labor_prod","agg_inflation","agg_var_real_wage","cb_ir")

experimet <- 0


# Todas = 0
# Market =1
# Quality = 2
# Lucro =3 
# Estoque = 4

####### SIMULAÇÃO ########

DEBBUG <- 0
periodos_simulacao <- 1000

start1 <- Sys.time()
set.seed(3)
results <- simulationABMFAST2(periodos_simulacao,lista_variaveis_salvar = lista_variaveis_salvar,DEBBUG = DEBBUG)
end1 <- Sys.time()
end1 - start1

plot(apply(results$firm_threshold * results$firm_ms,MARGIN=2,sum),type="l",ylim=c(0,0.3))

plot(apply(resultados[[3]]$firm_threshold * resultados[[3]]$firm_ms,MARGIN=2,sum),type="l",ylim=c(0,0.3))
  lines(apply(resultados[[3]]$firm_threshold ,MARGIN=2,mean),col="red")
  lines(resultados[[3]]$firm_threshold[300,])

plot( apply( results[[3]]$firm_ms, MARGIN=2,max ), type="l")

plot( results[[5]]$agg_function_dist[1,])
  lines( results[[3]]$agg_inflation[1,])

plot( results[[3]]$capitalist_consumo[1,]/results[[3]]$agg_nom_demand[1,] )

plot( apply(results$firm_vendas,MARGIN=2,sum) ,type="l" , col="green")
  lines( apply(results$firm_capacidade_producao,MARGIN=2,sum) , col="red")
  lines( apply(results$firm_expdem,MARGIN=2,sum) , col="blue")
  lines( results$agg_unemployment_rate[1,]*1000000)
  
  
(3 * results$firm_interest)

  
plot( apply( results[[3]]$firm_leverage * results[[3]]$firm_ms, MARGIN=2,sum) , type="l" )
  
  

results <- simulationMULTABM(5,DEBBUG=0)


plot(apply(results$firm_threshold,MARGIN=2,mean),type="l")

plot(results$agg_unemployment_rate[1,],type="l")

plot(results$capitalist_riqueza[1,]/results$agg_nom_demand[1,],type="l")


df <- data.frame(results$agg_inflation[1,100:1000],results$agg_unemployment_rate[1,100:1000])

names(df) <- c("Nome1","Nome2")

ggplot(data=df , aes(x=Nome1,y=Nome2)) +
  geom_point(shape=12, color="red", size=3) + 
  geom_smooth(method = "lm")# +
  xlim(0,0.1) + ylim(0,0.1)



plot(results$cb_ir[1,],type="l",ylim=c(0,0.1))

plot( apply(results$firm_vendas,MARGIN=2,sum) ,type="l" , ylim=c(0,800000))
  lines( apply(results$firm_capacidade_producao,MARGIN=2,sum)  )
  
  plot(apply(results$firm_machine_order,MARGIN=2,sum)*results$firmk_price[1,]/results$agg_nom_demand[1,],type="l")

  
  plot(apply(results$firm_labor_demand,MARGIN=2,sum)/apply(results$firm_tot_funcionarios,MARGIN=2,sum) - 1, results$agg_unemployment_rate[1,], ylim = c(0,0.2),xlim = c(0,0.3))
  
  
  plot(results$firm_machine_order[1,]/results$agg_nom_demand[1,])
  
  
  
plot( apply(results$firm_leverage * results$firm_ms,MARGIN=2,sum) ,type="l" )

plot(results$firm_leverage[150,] , type="l")
  lines(results$firm_machine_order[150,]/1000 , col="green")


plot( results$firm_receita[190,] ,type="l" , col="blue")
  lines( ( results$firm_custo[190,] ),type="l",col="red")
  
plot( results$firm_leverage[5,] , type="l")

plot(results$firm_threshold[1,],type="l")



firm_interest[,1000]

matrix_leverage <- matrix(0,m,periodos_simulacao)
matrix_leverage_sd <- matrix(0,m,periodos_simulacao)

plot(apply(resultados[[1]]$firm_leverage,MARGIN=2,sd) + apply(resultados[[1]]$firm_leverage,MARGIN=2,mean) , type="l")

for(i in 1:m){
  
  matrix_leverage[i,] <- apply(resultados[[i]]$firm_leverage,MARGIN=2,mean)
  matrix_leverage_sd[i,] <- apply(resultados[[i]]$firm_leverage,MARGIN=2,sd)
  
}

plot(apply(matrix_leverage,MARGIN=2,mean))

hist(matrix_leverage[,1000],100)



###

plot(apply(results$firm_ms,MARGIN=2,max),type="l",ylim=c())

plot(apply(results$firm_quality/apply(results$firm_quality,MARGIN=2,mean),MARGIN=2,sd))

plot(apply(results$firm_quality[500:1000]/apply(results$firm_quality,MARGIN=2,mean)[500:1000],MARGIN=2,med))

plot(apply(results$firm_price,MARGIN=2,min)[10:1000]/apply(results$firm_price,MARGIN=2,mean)[10:1000],type="l",ylim=c(0,2.5))

lines(apply(results$firm_quality,MARGIN=2,max)[10:1000]/apply(results$firm_quality,MARGIN=2,mean)[10:1000],type="l")


plot(apply(results$firm_E,MARGIN=2,mean),type="l")


plot(results$firm_ms[,3000],ylim=c(0,0.01))

hist(results$firm_E[,1000])

freq_price <- matrix(0,quantidade_firmas,999)

for(i in 2:999){

  freq_price[,i-1] <- results$firm_price[,i]/results$firm_price[,i-1] != 1

}

freq_med <- rollmean(freq_price,12)

plot(apply(freq_med,MARGIN=2,max)*12)


  
plot(apply(abs(freq_price[,(999- 20*4):999]),MARGIN=1,sum),results$firm_ms[,1000])

summary(lm( results$firm_ms[,3000] ~  apply(abs(freq_price[,(2999-30):2999]),MARGIN=1,sum) ))




plot(resultados[[i]]$firm_threshold[,500],
     (resultados[[i]]$firm_receita[,500] - resultados[[i]]$firm_custo[,500])/resultados[[i]]$firm_receita[,500],ylim=c(-0.1,0.6))


plot(resultados[[i]]$firm_vendas[,500]/resultados[[i]]$firm_expdem[,500],
     (resultados[[i]]$firm_receita[,500] - resultados[[i]]$firm_custo[,500])/resultados[[i]]$firm_receita[,500]
     )

plot(resultados[[i]]$firm_profit[193,]/resultados[[i]]$firm_receita[193,])

plot(resultados[[i]]$firm_expdem[193,])
  plot(resultados[[i]]$firm_vendas[193,])
  
  plot(resultados[[i]]$firm_debt_stock[193,])



profit_rate <- resultados[[i]]$firm_profit[,1000]/(resultados[[i]]$firm_machines[,1000] * resultados[[i]]$firmk_price[,1000])
ms <- resultados[[i]]$firm_ms[,1000]
thres <- resultados[[i]]$firm_threshold[,1000]
thres_2 <- thres^2

# Take the assumed values and fit into the model.
model <- nls(profit_rate ~ b1*thres^2+ b2*thres + b3,start = list(b1 = 1,b2 = 3,b3=0))

summary(lm( (ms) ~ thres + thres_2 ))

plot(resultados[[i]]$firm_threshold[1,],type="l")

plot(apply(resultados[[i]]$firm_threshold * resultados[[i]]$firm_ms,MARGIN=2,sum ),type="l")

plot(results$firm_price[1,2:101]/results$firm_price[1,1:100]-1,ylim=c(0,1))
  lines(results$firm_ms[1,]*100)
  lines(results$firm_E[1,]*1)


plot((1+resultados[[1]]$cb_ir[1,])^4-1,col="red",type="l",ylim=c(0,0.2))
  lines((1+resultados[[1]]$agg_inflation[1,])^4-1)
  lines(rep((1+meta_inflacao)^4-1,length(results$cb_ir[1,])),col="green")
  lines(results$agg_unemployment_rate[1,],type="l",col="blue")
  
plot(results$firm_assets[1,],type="l")
  lines(results$firm_debt_stock[1,])
  lines(results$firm_investimento[1,],col="green")
  lines(results$firm_profit[1,],col="blue")
  lines(results$firm_RD_invest[1,],col="red")
  
plot(results$firm_expdem[1,],type="l")
  lines(results$firm_vendas[1,])
  
plot(apply(results$firm_machine_order,MARGIN=2,sum),type="l")
  lines(apply(results$firm_new_machines,MARGIN=2,sum),type="l",col="red")
  lines(results$firmk_production[1,],col="green")
  lines(results$agg_unemployment_rate[1,]*1000,col="blue")
  
plot(apply(results$firm_expdem,MARGIN=2,sum),type="l")
  lines(apply(results$firm_vendas,MARGIN=2,sum),col="red")
  
plot(apply(results$firm_machine_order,MARGIN=2,sum))
  


plot(results$capitalist_consumo[1,]/results$agg_nom_demand[1,],type="l",ylim=c(0,0.5))
  lines(results$gov_consumption[1,]/results$agg_nom_demand[1,])
  lines(results$agg_investment[1,]/results$agg_nom_demand[1,])

plot(results$agg_empresas_falidas[1,],type="l")
  
plot(apply(results$firm_threshold,MARGIN=2,mean),type="l")
  lines(apply(results$firm_threshold,MARGIN=2,med),col="green")
  lines(apply(results$firm_threshold,MARGIN=2,mean) + apply(results$firm_threshold,MARGIN=2,sd),col="red")
  lines(apply(results$firm_threshold * results$firm_ms,MARGIN=2,sum),col="blue")
  lines(apply(results$firm_threshold,MARGIN=2,mean) - apply(results$firm_threshold,MARGIN=2,sd),col="red")
  lines(results$firm_threshold[1,])
  lines(results$firm_threshold[2,])
  lines(results$firm_threshold[3,])
  lines(results$firm_threshold[4,])
  
 hist( results$firm_threshold[,1000] ,50)
  
  
 dados_y <- apply(results$firm_threshold,MARGIN=2,mean)

  dados_x <- 1:length(apply(results$firm_threshold,MARGIN=2,mean))
  
  plot(dados_x,dados_y)
  
  dados_z <- dados_x * dados_x
  
  plot(dados_y ~ dados_x)
  
  dados_y2 <- dados_y[30:1000]
  
  dados_x2 <- dados_x[30:1000]
  
  nls(dados_y2 ~ a*dados_x2 + b, start = list(a = -0.1,b=0.1),
      algorithm = 'port', 
      control = nls.control(maxiter = 2000,tol = 200))
  
  nls(dados_y2 ~ a /log(dados_x2) + b , start = list(a = 0.1,b = 0.02),
      algorithm = 'port', 
      control = nls.control(maxiter = 2000,tol = 200))
  
  nls(dados_y2 ~ a / dados_x2^0.02 + b , start = list(a = 0.1,b = 0.02),
      algorithm = 'port', 
      control = nls.control(maxiter = 20000,tol = 200000))
  
  y_function <- -0.0274 * log(30:3000) +  0.22021
  y1_function <- 1.529 /(30:3000)^0.02 -1.300
  y2_function <-  0.74411 /log(30:3000) + -0.07096 
  y3_function <- -7.005e-05 * 30:3000 +   9.040e-02 
    
  plot(-0.03137 * log(30:2000) +  0.24523)
  
  plot(dados_y2,ylim=c(0,0.15),xlim=c(0,3000))
    lines(y_function,col="red")
    lines(y1_function,col="blue")
    lines(y2_function,col="green")
    lines(y3_function,col="purple")
    
    
    plot(forecast(dados_y2,1000))
    
    
  plot(-0.1 * log(1:100))
  
  y_test <- (0.1 * log(100:1000) + 0.02)
  
  plot(dados_x2,dados_y2)
  
  plot(0.1/log(1:100))
  
  
  lines(dados_y2)
  
  
  x_test <- 1:50
  
  plot(y_test ~ x_test , ylim=c(0,0.15))
  
  plot( log(y_test) ~ (x_test  ))
  
  d <- rep(1,901)
  
  lm(log(dados_y[100:1000]) ~ log(dados_x[100:1000] ))
  
  
  
  
  plot(log(dados_y[100:1000]) ~ dados_x[100:1000])
  
  plot( -2.265313 * (1:100)^(-0.001379 ))

  
  
  plot((dados_x),log(dados_y))
  
  f2 <- fitModel(dados_y ~ B + A * log(dados_x) )
  
  lm( dados_y ~  log(dados_x) )
  
  
  exp( -0.01709 * 400 + 0.15676 )
  
  
  gf_point(dados_y ~ dados_x) %>% splice.plot(f2(dados_x) ~dados_x)
  
  
  
fit <- auto.arima((apply(results$firm_threshold,MARGIN=2,mean)))
  
plot(forecast(fit,h=100))
  
  
plot(results$firm_assets[1,],type="l")
  lines(results$firm_debt_stock[1,])

plot(results$agg_empresas_falidas[1,],type="l")

plot(results$capitalist_consumo[1,]/results$agg_nom_demand[1,])
  
plot(results$agg_function_dist[1,])
  
plot(apply(results$firm_fixed.markup * results$firm_ms,MARGIN=2,sum),type="l")


hist(results$firm_threshold[,1000],100)

plot(results$firm_threshold[,1000],results$firm_ms[,1000])



plot(results$firm_price[1,150:180])
  lines(results$firm_price_dej[1,150:180])
  
  plot(results$firm_ms[1,150:180],type="l")
  

results$workers_empregador[3000,]

results$workers_salario[800,]/results$workers_salario_desej[800,]

results$firm_wage_ref[112,]/results$workers_salario_desej[800,]


results$workers_empregador[3000,145:150]

plot(results$workers_salario_desej[3000,145:150],type="l",ylim=c(0,800))
lines(results$workers_salario[3000,145:155])

plot(results$firm_wage_ref[6,145:155],type="l")
lines(results$workers_salario_desej[3000,145:155])

plot(apply(results$workers_salario,MARGIN=2,mean)/results$agg_index_prices[1,],type="l")


plot((1+resultados[[1]]$cb_ir[1,])^4 - 1,type="l",ylim=c(-0.2,0.4))
lines((1+resultados[[1]]$agg_inflation[1,])^4-1,col="red")
lines(resultados[[1]]$agg_unemployment_rate[1,],col="blue")
lines(rep((1+meta_inflacao)^(1/4) -1 ,periodos_simulacao))
lines(resultados[[1]]$gov_expense[1,]/resultados[[1]]$agg_nom_demand[1,],col="green")
lines(apply(resultados[[1]]$workers_salario_desej,MARGIN=2,mean)/apply(results$firm_wage_ref * results$firm_ms,MARGIN=2,sum) - 1,type="l",col="green")

plot((1+results$cb_ir[1,])^4 - 1,type="l",ylim=c(-0.2,0.4))
lines((1+results$agg_inflation[1,])^4-1,col="red")
lines(results$agg_unemployment_rate[1,],col="blue")
lines(rep((1+meta_inflacao)^4 -1 ,periodos_simulacao))
lines(results$gov_expense[1,]/results$agg_nom_demand[1,])
lines(apply(results$workers_salario_desej,MARGIN=2,mean)/apply(results$firm_wage_ref * results$firm_ms,MARGIN=2,sum) - 1,type="l",col="green")


plot(apply(results$workers_salario_desej,MARGIN=2,mean)[200:500],col="blue")
lines(apply(results$firm_wage_ref * results$firm_ms,MARGIN=2,sum)[200:500])
lines(apply(results$workers_salario,MARGIN=2,mean)[200:500],col="red")
lines(results$agg_index_prices[1,]*500,col="green")

# Pq o custo está tão menor do que o salário?

# Pq o salário de referência está mto acima do efetivo

(firm_tot_funcionarios * firm_wage_ref)/firm_custo
firm_wage_ref/firm_custo_unit


plot(results$gov_consumption[1,]/results$agg_nom_demand[1,],type="l",ylim=c(0,0.3),xlim=c(800,1000))
lines(results$agg_unemployment_rate[1,],type="l",col="blue")
lines(results$capitalist_consumo[1,]/results$agg_nom_demand[1,],type="l",ylim=c(0,0.3),xlim=c(800,1000))

plot(results$agg_unemployment_rate,results$agg_function_dist,ylim=c(0.5,0.8))

plot(apply(results$firm_assets,MARGIN=2,sum)/results$agg_nom_demand[1,],type="l",ylim=c(0,3))
lines(apply(results$firm_debt_stock,MARGIN=2,sum)/results$agg_nom_demand[1,],type="l")

plot(apply(results$firm_leverage,MARGIN=2,mean),type="l",ylim=c(0,1))
lines(results$cb_ir[1,]*2)

plot(results$capitalist_riqueza[1,]/results$agg_nom_demand[1,],type="l")
plot(results$gov_debt[1,]/results$agg_nom_demand[1,],type="l")
plot(apply(results$firm_debt_stock,MARGIN = 2,sum)/results$agg_nom_demand[1,],type="l")


plot(apply(results$workers_salario,MARGIN=2,sum)/results$agg_nom_demand[1,],type="l")

plot(results$agg_empresas_falidas[1,])

plot(apply(results$firm_debt_stock,MARGIN = 2,sum)/results$agg_nom_demand[1,],type="l")
plot(apply(results$firm_assets,MARGIN = 2,sum)/results$agg_nom_demand[1,],type="l")

plot(results$gov_debt[1,]/results$agg_nom_demand[1,],type="l")

plot(apply(results$firm_fixed.markup,MARGIN=2,mean),type="l")

plot(apply(results$workers_salario,MARGIN=2,sum)/results$agg_nom_demand[1,],type="l")

plot((apply(results$firm_investimento,MARGIN=2,sum)+apply(results$firm_RD,MARGIN = 2,sum))/results$agg_nom_demand[1,],type="l")

plot(apply(results$firm_fb/results$firm_receita,MARGIN=2,mean),type="l")

plot(apply(results$firm_ms,MARGIN=2,max),ylim=c(0,0.015),type="l")
lines(apply(results$firm_ms,MARGIN=2,mean))
lines(apply(results$firm_ms,MARGIN=2,min))

plot( apply(results$firm_debt_stock,MARGIN=2,sum)/apply(results$firm_assets,MARGIN=2,sum) ,type="l")

plot( apply(results$firm_leverage * results$firm_ms,MARGIN=2,sum) , type="l" )

plot( results$agg_capacity_util[1,100:1000] , type="l")

plot(results$firm_debt_stock[10,1:periodos_simulacao],type="l")
lines(results$firm_assets[10,1:periodos_simulacao])

plot(results$firm_leverage[1,],type="l")

lines(results$firm_profit[10,1:periodos_simulacao])

plot(results$firm_profit[10,1:periodos_simulacao]/results$firm_receita[10,1:periodos_simulacao],type="l")

plot(results$firm_dividendos[10,1:periodos_simulacao]/results$firm_profit[10,1:periodos_simulacao],type="l")

plot((results$firm_fixed.markup[10,1:periodos_simulacao]),type="l")




sum(sum(firm_assets[,1]) + sum(workers_riqueza[,1]) + (1-capitalist_cash_percent[,1])*capitalist_riqueza[,1])
bank_emprestimos_bc[,1]

bank_emprestimos_bc[,1]/sum(sum(firm_assets[,1]) + (capitalist_cash_percent[,1])*capitalist_riqueza[,1])

bank_emprestimos_bc == apply(firm_assets,MARGIN=2,sum)[1:2] + 
  capitalist_cash_percent * capitalist_riqueza + 
  apply(workers_riqueza,MARGIN=2,sum) + bank_profit # Check 1 - Empréstimos BC == Money Stock

(apply((firm_assets - firm_debt_stock),MARGIN=2,sum)[1:2] + capitalist_riqueza + apply(workers_riqueza,MARGIN=2,sum) + bank_profit + firmk_profit) / gov_debt # Check 2 - Riqueza == Dívida

bank_deposits + bank_emprestimos_bc + bank_profit == gov_debt + apply(firm_debt_stock,MARGIN=2,sum)[1:2]

# Check fluxo dos trabalhadores 

apply(workers_riqueza,MARGIN=2,sum) 

apply(workers_income,MARGIN=2,sum) * (1 - gov_ir) - apply(workers_consumo,MARGIN=2,sum)

# Check fluxo capitalistas

capitalist_riqueza[,1] -  capitalist_riqueza[,2] - (VAR(agg_nom_demand) - sum2(VAR(firm_vendas)*VAR(firm_price)))

capitalist_disposable_income[,1] - capitalist_consumo[,1]

# Check fluxo firms de bens de consumo


apply(firm_deficitsurplus,MARGIN=2,sum)

asset.dyn <- apply(firm_assets,MARGIN=2,sum)[1] - apply(firm_assets,MARGIN=2,sum)[2]

debt.dyn <- apply(firm_debt_stock,MARGIN=2,sum)[1] - apply(firm_debt_stock,MARGIN=2,sum)[2]

round(asset.dyn,5) == round(apply(firm_deficitsurplus,MARGIN=2,sum)[1],5)

# Check banco

bank_receita[,1] - bank_pgto_juros - VAR(cb_ir) * LAG(bank_emprestimos_bc,1)

gov_debt + apply(firm_debt_stock,MARGIN=2,sum)[1:2] == bank_emprestimos_bc + bank_deposits + bank_profit


# Estoque de dinheiro

(capitalist_cash_percent * capitalist_riqueza + apply(firm_assets,MARGIN=2,sum)[1:2] ) - bank_emprestimos_bc

# Ou seja, tem empréstimos de mais. Logo, tenho depósitos de menos.
((1-capitalist_cash_percent) * capitalist_riqueza + apply(workers_riqueza,MARGIN=2,sum)) - bank_deposits

gov_expense - gov_consumption[1] + apply(firm_custo,MARGIN=2,sum)[1] - apply(workers_income,MARGIN=2,sum)[1]


FLOWMATRIX <- matrix(0,20,7)

rownames(FLOWMATRIX) <- c("W","Div","Taxes","Transfer","PC","GC","Inv.M","Inv.R","CBProf","Load int","Bond int","Depos. Int","Reserv.Int",
                          "Cash","Loans","Bonds","Deposits","Reserves","Sum Income Flows","Sum Var.Stock")


# TRABALHADOR
FLOWMATRIX[1,1] <- sum(VAR(workers_salario))
FLOWMATRIX[3,1] <- -VAR(gov_ir) * sum(VAR(workers_income))
FLOWMATRIX[4,1] <- VAR(gov_expense) - VAR(gov_consumption)
FLOWMATRIX[5,1] <- -sum(VAR(workers_consumo))
FLOWMATRIX[12,1] <- sum( LAG(workers_riqueza,1) * VAR(cb_ir) * par_rate_deposits)
FLOWMATRIX[14,1] <- sum(VAR(workers_riqueza) - LAG(workers_riqueza,1))

# CAPITALISTA
FLOWMATRIX[2,2] <- sum(VAR(capitalist_dividendos))
FLOWMATRIX[3,2] <- -VAR(gov_ir) * sum(VAR(capitalist_income))
FLOWMATRIX[5,2] <- -VAR(capitalist_consumo) + (sum(VAR(firm_demand_disp)) - sum2(VAR(firm_vendas)*VAR(firm_price)))
FLOWMATRIX[8,2] <- sum(VAR(firm_RD_invest))
FLOWMATRIX[12,2] <- + (par_rate_deposits * VAR(cb_ir)) * ( 1 - LAG(capitalist_cash_percent,1))  * LAG( capitalist_riqueza, 1)
FLOWMATRIX[14,2] <- ( VAR(capitalist_cash_percent))  * VAR( capitalist_riqueza) - ( LAG(capitalist_cash_percent,1))  * LAG( capitalist_riqueza, 1)
FLOWMATRIX[17,2] <- ( 1- VAR(capitalist_cash_percent))  * VAR( capitalist_riqueza) - (1-  LAG(capitalist_cash_percent,1))  * LAG( capitalist_riqueza, 1)


# FIRMA C
FLOWMATRIX[1,3] <- -sum(VAR(firm_custo))
FLOWMATRIX[2,3] <- -sum(VAR(firm_dividendos))
FLOWMATRIX[5,3] <- sum(VAR(firm_receita)) - VAR(gov_consumption)
FLOWMATRIX[6,3] <- VAR(gov_consumption)
FLOWMATRIX[7,3] <- - sum(VAR(firm_investimento))
FLOWMATRIX[8,3] <- - sum(VAR(firm_RD_invest))
FLOWMATRIX[10,3] <- - sum(VAR(firm_interest) * LAG(firm_debt_stock,1))
FLOWMATRIX[14,3] <- sum(VAR(firm_assets) - LAG(firm_assets,1))
FLOWMATRIX[15,3] <- - sum(VAR(firm_debt_stock) - LAG(firm_debt_stock,1))

sum(VAR(firm_deficitsurplus))

# FIRMA K
FLOWMATRIX[1,4] <- -sum(VAR(firmk_cost))
FLOWMATRIX[2,4] <- -sum(LAG(firmk_profit,1))
FLOWMATRIX[7,4] <- sum(VAR(firmk_receita))
FLOWMATRIX[17,4] <- ifelse( VAR(firmk_profit) > 0, VAR(firmk_profit),0) - ifelse(LAG(firmk_profit,1) > 0, LAG(firmk_profit,1),0)
FLOWMATRIX[15,4] <- (ifelse( VAR(firmk_profit) <0, VAR(firmk_profit),0) - ifelse(LAG(firmk_profit,1) <0, LAG(firmk_profit,1),0))


# BANCO
FLOWMATRIX[2,5] <- -LAG(bank_profit,1)
FLOWMATRIX[10,5] <- VAR(bank_receita) - VAR(cb_ir)*LAG(gov_debt,1)
FLOWMATRIX[11,5] <- VAR(cb_ir)*LAG(gov_debt,1)
FLOWMATRIX[12,5] <- -VAR(bank_pgto_juros)
FLOWMATRIX[13,5] <- - VAR(cb_ir)*LAG(bank_emprestimos_bc,1)
FLOWMATRIX[15,5] <- sum( VAR(firm_debt_stock) - LAG(firm_debt_stock,1) ) - (ifelse( VAR(firmk_profit) <0, VAR(firmk_profit),0) - ifelse(LAG(firmk_profit,1) <0, LAG(firmk_profit,1),0))
FLOWMATRIX[16,5] <- VAR(gov_debt) - LAG(gov_debt,1)
FLOWMATRIX[17,5] <- -(VAR(bank_deposits) - LAG(bank_deposits,1))
FLOWMATRIX[18,5] <- -( VAR(bank_emprestimos_bc) - LAG(bank_emprestimos_bc,1))

# BANCO CENTRAL
FLOWMATRIX[9,6] <- -VAR(cb_profit)
FLOWMATRIX[13,6] <- VAR(cb_ir)*LAG(bank_emprestimos_bc,1)
FLOWMATRIX[14,6] <- -(VAR(capitalist_cash_percent) * VAR(capitalist_riqueza) + sum(VAR(firm_assets)) - sum(LAG(firm_assets,1)) -
                        LAG(capitalist_cash_percent,1) * LAG(capitalist_riqueza,1)) - sum(VAR(workers_riqueza) - LAG(workers_riqueza,1))
FLOWMATRIX[18,6] <- ( VAR(bank_emprestimos_bc) - LAG(bank_emprestimos_bc,1))

# GOVERMENT 
FLOWMATRIX[3,7] <- VAR(gov_tributos)
FLOWMATRIX[4,7] <- -(VAR(gov_expense) - VAR(gov_consumption))
FLOWMATRIX[6,7] <- -(VAR(gov_consumption))
FLOWMATRIX[9,7] <- (VAR(cb_profit))
FLOWMATRIX[11,7] <- -VAR(cb_ir)*LAG(gov_debt,1)
FLOWMATRIX[16,7] <- -(VAR(gov_debt) - LAG(gov_debt,1))

FLOWMATRIX[19,] <- colsums(FLOWMATRIX[1:13,])
FLOWMATRIX[20,] <- - colsums(FLOWMATRIX[14:18,])



round(colsums(FLOWMATRIX),1)
Rfast::rowsums(FLOWMATRIX)

FLOWMATRIX <- cbind(FLOWMATRIX,round(Rfast::rowsums(FLOWMATRIX),2))

FLOWMATRIX <- rbind(FLOWMATRIX,round(colsums(FLOWMATRIX),2))

FLOWMATRIX[21,] <- round(FLOWMATRIX[20,] + FLOWMATRIX[19,],3)

colnames(FLOWMATRIX) <- c("Worker","Capitalist","Firm.C","Firm.K","Bank","Central bank","Goverment","Sum")

FLOWMATRIX

### Check Stock

sum(FLOWMATRIX[20,])
sum(FLOWMATRIX[19,])

(sum(VAR(workers_riqueza)) + VAR(capitalist_riqueza) + sum( VAR(firm_assets) - VAR(firm_debt_stock) ) + VAR(firmk_profit) + VAR(bank_profit) ) / VAR(gov_debt)


sum(VAR(firm_deficitsurplus) + 1/periodos_simulacao * LAG(firm_debt_stock,1))




# # # # # # #     


corr <- corr.function3.bk(results=resultados,GDP = c("agg_gdp"),Nomes = c("agg_gdp","agg_consumption","agg_investment","gov_consumption_real","agg_unemployment_rate",
                                                                          "agg_capacity_util","agg_labor_prod","agg_inflation","agg_var_real_wage","cb_ir"),
                          Titulo=c("gdp","con","invest","gov_cons","unem","util","prod","inflation","real_Wage","interest"),start=ceiling(periodos_simulacao*3/4),periodos=6,
                          LAGS=c(1,1,1,1,0,0,1,0,0,0,0)) # Lags para definir o log de quem deve ser tirado

# Toda  variável que estimar em logaritmo deve ser multiplicada por 100.


plot(results$firm_leverage[1,1:200],type="l")

plot(results$firm_debt_stock[1,1:200],type="l")
lines(results$firm_assets[1,1:200],col="green")

plot( results$firm_profit[1,1:200],type="l")

plot( results$firm_machine_order[1,1:200],type="l")


# Inflação, taxa de juros e desemprego

plot(resultados[[1]]$agg_inflation[1,800:periodos_simulacao],type="l",ylim=c(-0.02,0.1))
lines(resultados[[1]]$cb_ir[1,800:periodos_simulacao],type="l",col="blue")

plot(resultados[[1]]$agg_unemployment_rate[1,750:periodos_simulacao],type="l",ylim=c(0.05,0.25))


plot( resultados[[1]]$agg_gdp[1,1600:2000] ,type="l")
lines( resultados[[1]]$agg_consumption[1,1600:2000] ,type="l")

plot( log( resultados[[1]]$agg_consumption[1,1600:2000] ) ,type="l" , ylim=c(13,16))
lines( log( resultados[[1]]$agg_gdp[1,1600:2000] ) ,type="l")

plot( log( resultados[[1]]$gov_consumption_real[1,1600:2000] ))



plot( apply(resultados[[98]]$firm_leverage,MARGIN=2,sd) )


plot(resultados[[98]]$firm_assets[1,1:1800],type="l")
lines(resultados[[98]]$firm_debt_stock[1,1:1800],col="green")

plot(resultados[[98]]$firm_fixed.markup[190,],type="l")


hist(resultados[[1]]$firm_vendas[,1000]/resultados[[1]]$firm_tot_funcionarios[,1000])
hist(resultados[[1]]$firm_capacidade_producao[,1000]/resultados[[1]]$firm_tot_funcionarios[,1000])

corr$Table.Dp[,1]*100
Matrix.Empiric[,1]

plot(resultados[[1]]$firm_threshold[1,])

  plot(apply(resultados[[1]]$firm_threshold,MARGIN=2,mean),type="l")



######
variable_tested <- 1

par(mfrow = c(2, 2))

plot(-6:6,corr$Table.Corr[1,],type="l",ylim=c(-1,1),main="GDP",xlab="",ylab="",lwd=2,cex.axis=1.2 )
seq <- -6:6
arrows(seq,corr$Table.Corr[1,] + 2*corr$Table.SD.Corr[1,],seq,
       corr$Table.Corr[1,] - 2*corr$Table.SD.Corr[1,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[1,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[2,],type="l",ylim=c(-1,1),main="Consumption",xlab="",ylab="",lwd=2,cex.axis=1.2)
seq <- -6:6
arrows(seq,corr$Table.Corr[2,] + 2*corr$Table.SD.Corr[2,],seq,
       corr$Table.Corr[2,] - 2*corr$Table.SD.Corr[2,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[2,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[3,],type="l",ylim=c(-1,1),main="Investiment",xlab="",ylab="",lwd=2,cex.axis=1.2
)
seq <- -6:6
arrows(seq,corr$Table.Corr[3,] + 2*corr$Table.SD.Corr[3,],seq,
       corr$Table.Corr[3,] - 2*corr$Table.SD.Corr[3,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[3,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[4,],type="l",ylim=c(-1,1),main="Gov.Cons.",xlab="",ylab="",lwd=2,cex.axis=1.2
)
seq <- -6:6
arrows(seq,corr$Table.Corr[4,] + 2*corr$Table.SD.Corr[4,],seq,
       corr$Table.Corr[4,] - 2*corr$Table.SD.Corr[4,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[4,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[5,],type="l",ylim=c(-1,1),main="Unemployment",xlab="",ylab="",lwd=2,cex.axis=1.2)
seq <- -6:6
arrows(seq,corr$Table.Corr[5,] + 2*corr$Table.SD.Corr[5,],seq,
       corr$Table.Corr[5,] - 2*corr$Table.SD.Corr[5,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[5,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[6,],type="l",ylim=c(-1,1),main="Capacity Utilization",xlab="",ylab="",lwd=2,cex.axis=1.2)
seq <- -6:6
arrows(seq,corr$Table.Corr[6,] + 2*corr$Table.SD.Corr[6,],seq,
       corr$Table.Corr[6,] -2* corr$Table.SD.Corr[6,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[6,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[10,],type="l",ylim=c(-1,1),main="Interest rate",xlab="",ylab="",lwd=2,cex.axis=1.2)
seq <- -6:6
arrows(seq,corr$Table.Corr[10,] + 2*corr$Table.SD.Corr[10,],seq,
       corr$Table.Corr[10,] - 2*corr$Table.SD.Corr[10,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[10,2:14],col="red",lwd=2)

plot(-6:6,corr$Table.Corr[8,],type="l",ylim=c(-1,1),main="Inflation",xlab="",ylab="",lwd=2,cex.axis=1.2)
seq <- -6:6
arrows(seq,corr$Table.Corr[8,] + 2*corr$Table.SD.Corr[8,],seq,
       corr$Table.Corr[8,] - 2*corr$Table.SD.Corr[8,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[8,2:14],col="red",lwd=2)

###### Gráfico de uma simulação das heurísticas

hist(firm_price_rule)

rule1 <- 1:periodos_simulacao 
rule2 <- 1:periodos_simulacao
rule3 <- 1:periodos_simulacao
rule4 <- 1:periodos_simulacao

for(i in 1:periodos_simulacao){
  
  rule1[i] <- sum(resultados[[5]]$firm_price_rule[,i] == 1)
  rule2[i] <- sum(resultados[[5]]$firm_price_rule[,i] == 2)
  rule3[i] <- sum(resultados[[5]]$firm_price_rule[,i] == 3)
  rule4[i] <- sum(resultados[[5]]$firm_price_rule[,i] == 4)
  
}


df <- data.frame(rule1/quantidade_firmas*100,rule2/quantidade_firmas *100,rule3/quantidade_firmas*100,rule4/quantidade_firmas*100,1:length(rule4))

names(df) <- c("rule1","rule2","rule3","rule4","time")

one_simulation_rules_graph <- ggplot(df) +
  geom_line(aes(x=time,y=rule1),size=1) +
  geom_line(aes(x=time,y=rule2),col="red",size=1) +
  geom_line(aes(x=time,y=rule3),col="green",size=1) +
  geom_line(aes(x=time,y=rule4),col="blue",size=1) +
  labs(y="% of firms using a rule",x="",size=1) +
  theme_light() +
  theme(text = element_text(size=10)) +
  xlim(100,1000)

names(df) <- c("MS","Qual","Prof","Dem","Time")

one_simulation_rules_graph <- ggplot(df) +
  geom_line(aes(x=Time,y=MS,col="MS"),size=0.8) +
  geom_line(aes(x=Time,y=Qual,col="Qual"),size=0.8) +
  geom_line(aes(x=Time,y=Dem,col="Dem"),size=0.8) +
  geom_line(aes(x=Time,y=Prof,col="Prof"),size=0.8) +
  ylab("% of firms using a rule") +
  xlim(100,1000) +
  ylim(0,80) +
  theme(text = element_text(size=10),legend.title = element_blank()) +
  theme_light() +
  scale_colour_manual(name='',
                      breaks=c('MS', 'Qual','Dem','Prof'),
                      values=c('MS'='black', 'Qual'='blue','Dem'='green','Prof'='red'))


ggsave(one_simulation_rules_graph, filename = "one_simulation_rules_graph.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


cor.test(rule1[200:1000],results$agg_inflation[1,200:1000])

plot(rule1[100:periodos_simulacao],type="l",ylim=c(0,200),lwd=2,xlab="Simulation periods",
     ylab="Number of firms using one of the heuristics") # Market-share
lines(rule2[100:periodos_simulacao],col="blue",lwd=2)          # Quality
lines(rule3[100:periodos_simulacao],col="green",lwd=2)         # Lucro
lines(rule4[100:periodos_simulacao],col="red",lwd=2)           # Estoque
legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
       col=c("black", "blue","green","red"), lty=1, cex=1)

rule1 <- 1:periodos_simulacao 
rule2 <- 1:periodos_simulacao
rule3 <- 1:periodos_simulacao
rule4 <- 1:periodos_simulacao

for(i in 1:periodos_simulacao){
  
  rule1[i] <- sum(results$firm_price_rule[,i] == 1)
  rule2[i] <- sum(results$firm_price_rule[,i] == 2)
  rule3[i] <- sum(results$firm_price_rule[,i] == 3)
  rule4[i] <- sum(results$firm_price_rule[,i] == 4)
  
}


plot(rule1,type="l")
lines(rule2)

cor(results$agg_inflation[1,],rule4/quantidade_firmas)

cor.test(results[[1]]$cb_ir[1,],apply(results[[1]]$firm_fixed.markup,MARGIN=2,mean))


hist(VAR(firm_profit)/VAR(firm_receita),30,xlim=c(-0.3,0.5))

sum(VAR(firm_profit)/VAR(firm_receita) * VAR(firm_ms))

median(VAR(firm_profit)/VAR(firm_receita))

plot(results$agg_empresas_falidas[1,],type="l")

hist(results$firm_ms[,200],200)

plot(apply(results$firm_debt_stock,MARGIN=2,sum2)/apply(results$firm_assets,MARGIN=2,sum2),type="l")

lines(apply(results$firm_fixed.markup,MARGIN=2,mean)*100,type="l")

plot(apply(results$firm_fixed.markup,MARGIN=2,sd),type="l",ylim=c(0,0.3))
plot(apply(results$firm_fixed.markup,MARGIN=2,mean)[30:periodos_simulacao],type="l")

init <- round(periodos_simulacao/2)


sd(log(resultados[[1]]$agg_gdp[1,init:periodos_simulacao]))
sd(log(resultados[[1]]$agg_investment[1,init:periodos_simulacao]))
sd(log(resultados[[1]]$agg_consumption[1,init:periodos_simulacao]))
sd(log(resultados[[1]]$gov_consumption_real[1,init:periodos_simulacao]))

plot(apply(resultados[[1]]$workers_salario,MARGIN=2,sum)/resultados[[1]]$agg_nom_demand[1,],type="l")


###### simulABM.conditions.parameters ######


install.packages("dgof")
library(dgof)

# Função c/ número e tamanho da simulação endógenas

simulABM.conditions.parameters <- function(lista_variaveis_salvar2,resultados){
  
  lista_variaveis_salvar <- lista_variaveis_salvar2
  
  resultados.data <- matrix(0,length(lista_variaveis_salvar2),3) # Matriz para guardar os dados. Coluna 1= Número de simulações. 2 = número de períodos
  
  row.names(resultados.data) <- lista_variaveis_salvar2
  colnames(resultados.data) <- c("Number simulations","Number of periods","Transient period")
  
  m <- length(resultados)
  
  periodos_simulacao <- ncol( resultados[[1]][[1]] ) 
  
  for(z in 1:length(lista_variaveis_salvar)){ # Ciclo para coletar os dados que serão analisados
    
    Nmax <- 0
    
    matriz.dados <- matrix(0,length(lista_variaveis_salvar2),length(resultados[[1]][[1]][1,])) # 
    
    matriz.dados.por.simul <- matrix(0,m,periodos_simulacao)
    
    for(i in 1:m){ # Se não for um agente único. Agregação.
      
      X <- eval(parse(text = gsub(" ","",paste("resultados[[i]]$",lista_variaveis_salvar[[z]])) ))
      
      if(nrow(X) > 1){
        
        X <- apply(X,MARGIN=2,mean)
        
      }
      
      matriz.dados.por.simul[i,] <- X
      
    }
    
    # Primeira etapa, checar se eu tenho um número suficiente de estimativas.
    # 1.1 - Checar se a variável segue uma distribuição normal
    # 1.2 - Se sim, checar se o invervalo de confiança dentro do máximo
    
    # Confiança que o valor populacional vai estar dentro do intervalo, para checar se eu estou tenho um steady-state
    # Divido a simulação em 3 períodos. 1/3,1/3 e 1/3
    # Testo se a média do 2º período é igual à do 3º período.
    
    mean(matriz.dados.por.simul[,periodos_simulacao])
    sd(matriz.dados.por.simul[,periodos_simulacao])
    hist(matriz.dados.por.simul[, round(periodos_simulacao/3):round(2/3*periodos_simulacao)])
    hist(matriz.dados.por.simul[, round(periodos_simulacao*2/3):periodos_simulacao])
    
    
    Means <- matrix(colMeans2(matriz.dados.por.simul),1,periodos_simulacao)
    
    Means.Matrix <- matrix( Means, 10, periodos_simulacao )
    
    difference <-  matriz.dados.por.simul
    
    for(i in 1:length(resultados[[1]][[1]][1,])){
      
      difference[,i] <- matriz.dados.por.simul[,i] - Means[i]
      
      #difference[,i]  <- difference[,i] / Means[i] - 1
      
    }
    
    # CI = X +- 1.98 * s/sqrt(N)
    # (CI - X) max 2 * s
    # (0.05 - 1) X = 1.98 * s
    
    # 0.05 = 1.98 * s/sqrt(N)
    
    # N = ((1.98/0.05) * s )^2
    
    
    Dados <- matrix(0,2,periodos_simulacao) # Matriz para guardar resultados 
    
    for(y in 1:periodos_simulacao){ # PRimeiro cíclo. Para avaliar a margem de erro
      
      # Cálculo da margem de erro
      
      Margin_Error <- qt(ci + (1 - ci)/2, df = length(matriz.dados.por.simul[,y]) - 1) * sd(matriz.dados.por.simul[,y])/sqrt(length(matriz.dados.por.simul[,y]))
      
      if(  mean(matriz.dados.por.simul[,y]) < 1  ){ # Se as séries forem menores do que um. MArgem de erro deve ser menor do que 5%
        
        mean <- 0.05
        
      }else{ # Se for maior do que um, calcular a margem de erro
        
        mean <- sqrt(( 0.05 * mean(matriz.dados.por.simul[,y]) )^2)
        
      }
      
      if( Margin_Error  > mean ){ # Se a margem de erro for maior do que o máximo
        
        print( lista_variaveis_salvar[[z]] ) # Print variável que tem desvio padrão acima do máximo
        
        # Determinação do número de N necessários para simular
        N <- round( ( qt(ci + (1 - ci)/2, df = length(matriz.dados.por.simul[,y]) - 1) * sd(matriz.dados.por.simul[,y])/(0.05 * mean(matriz.dados.por.simul[,y]) ) )^2 )
        
        Dados[1,y] <- Margin_Error # Coloco na matriz dedados
        Dados[2,y] <- N # Coloco na matriz de dados
        
      }
      
    }
    
    if( max(Dados[2,]) > Nmax ){ Nmax <- max(Dados[2,]) } # Se o N é maior do que o Nmax, novo NMax
    
    
    if(Nmax != 0) resultados.data[z,1] <- Nmax # Se o NMax for maior do que 0, coloco na matriz do output o valor de N recomendado
    
    # Se o Nmax == 0, significa que tenho uma amostra suficiente para ter um intervalo de confiança.
    # com o tamanho máximo desejado. Passo para o teste do steady-state.
    
    if(Nmax == 0){ # Ciclo para avaliar o steady-state
      
      medias <- 0:length(matriz.dados.por.simul[1,])
      
      end_transiente <- 0
      
      medias_11 <- round(1/2*periodos_simulacao):round(3/4*periodos_simulacao)
      medias_21 <- (round(periodos_simulacao*3/4)+1):periodos_simulacao
      
      pos1 <- 0
      pos2 <- 0
      
      end_transiente_accum <- 0
      
      
      for(num_simul in 1:m){
        
        print(paste("Análise fase transiente.Variável:",lista_variaveis_salvar[[z]],"Simul:",num_simul))
        
        for(i in (tinit*3):(periodos_simulacao-1)){
          
          
          medias_1 <-  (matriz.dados.por.simul[,(i-1)])
          medias_2 <-  (matriz.dados.por.simul[,(i)])
          
          medias_1 <-  (matriz.dados.por.simul[num_simul,(tinit*3):(i+1)])
          medias_2 <-  (matriz.dados.por.simul[num_simul,(i):periodos_simulacao])
          
          # if( i >= round(8/10*periodos_simulacao) & i <= round(9/10*periodos_simulacao) ){
          #   
          #   pos1 <- 1 + pos1
          #   
          #   medias_11[pos1] <- mean((matriz.dados.por.simul[,i]))
          #   
          # }
          # 
          # if( i >= (round(9/10*periodos_simulacao) + 1) ){
          #   
          #   print(pos2)
          #   
          #   pos2 <- pos2 + 1
          #   
          #   medias_21[pos2] <- mean((matriz.dados.por.simul[,i]))
          #   
          # }
          
          welch.test <- t.test(medias_1, medias_2)
          
          if( welch.test$p.value > 0.05 & end_transiente == 0 ){ end_transiente <- i + 1  }
          if( welch.test$p.value  < 0.05 ){ end_transiente <- 0  }
          
        }
        
        end_transiente_accum <- end_transiente + end_transiente_accum
      }
      
      resultados.data[z,3] <- round(end_transiente_accum/m,0)
      
      ks.test.result_accum <- 0
      
      for(num_simul in 1:m){
        
        segundaparte <- matriz.dados.por.simul[num_simul, round(1/3*periodos_simulacao):round(2/3*periodos_simulacao)] # SEgunda parte da simulação
        tercaparte <- matriz.dados.por.simul[num_simul, round(periodos_simulacao*2/3):round(periodos_simulacao)] # Terceira parte da simulação
        
        segundaparte <- matriz.dados.por.simul[num_simul, round(periodos_simulacao/3):round(2/3*periodos_simulacao)] # SEgunda parte da simulação
        tercaparte <- matriz.dados.por.simul[num_simul, round(periodos_simulacao*2/3):round(periodos_simulacao)] # Terceira parte da simulação
        
        
        if(all(segundaparte == segundaparte[1]) == FALSE){
          
          AD.test2parte <- ad.test(segundaparte)# Teste de normalidade
          print(AD.test2parte[[2]])
          
        } 
        if(all(tercaparte == tercaparte[1]) == FALSE){
          
          AD.test3parte <- ad.test(tercaparte)# Teste de normalidade
          print(AD.test3parte[[2]])
          
        } 
        
        #if( AD.test2parte[[2]] > 0.05 & AD.test3parte[[3]] > 0.05 ){
        
        # Welch’s t-test
        
        if( all(segundaparte == segundaparte[1])  == FALSE | all(tercaparte == tercaparte[1]) == FALSE){
          
          welch.test <- t.test(segundaparte, tercaparte)
          
          welch.test <- t.test(medias_11, medias_21, conf.level = 0.95, alternative = c("two.sided"))
          
          df <- data.frame(period=c("First","Second"),
                           value=c(mean(medias_11),mean(medias_21)))
          
          p<-ggplot(data=df, aes(x=period, y=value)) +
            geom_bar(stat="identity", color="blue", fill="steelblue")+
            theme_minimal()
          
          
        }
        
        # Kolmogorov-Smirnov Test
        
        
        ks.test.result <- stats::ks.test(c(medias_11), c(medias_21),exact = NULL,
                                         alternative = c("two.sided"))
        
        ks.test.result <- stats::ks.test(c(segundaparte), c(tercaparte),exact = NULL,
                                         alternative = c("two.sided"))
        
        ks.test.result_accum <- ks.test.result$p.value + ks.test.result_accum
        
        
        if(  ks.test.result[2] > 0.05 ) resultados.data[z,2] <- 1 # Se, pelo teste de Kolmogorov-Smirnov as distribuições forem iguais
        
        plot(ecdf(segundaparte),
             xlim = range(c(segundaparte, tercaparte)),
             col = "blue")
        plot(ecdf(tercaparte),
             add = TRUE,
             lty = "dashed",
             col = "red")
        
        #plot(ecdf(medias_11),
        #     xlim = range(c(medias_11, medias_21)),
        #     col = "blue")
        #plot(ecdf(medias_21),
        #     add = TRUE,
        #     lty = "dashed",
        #     col = "red")
        
      }
      
      ks.test.result_accum <- ks.test.result_accum/m
      
      if(  ks.test.result_accum > 0.05 ) resultados.data[z,2] <- 1
      
    }
  }
  
  return(resultados.data)
  
}

install.packages("nortest")
library(nortest)

#m <- 30 # Número de simulações
#http://127.0.0.1:46569/graphics/plot_zoom_png?width=1536&height=824
# Variáveis que precisam ter desvio padrão menor do que o estabelecido e precisam chegar no steady state
lista_variaveis_salvar2 <- list("agg_unemployment_rate","agg_gdp","agg_function_dist","agg_inflation")

#periodos_simulacao <- 500
#seed <- seq(10,10000,(10000-10)/m)

Marcador.Resultado <- 0
ci <- 0.95

#lista_variaveis_salvar <- c("agg_gdp","agg_inflation","agg_unemployment_rate")

while( Marcador.Resultado == 0){
  
  #resultados <- simulationMULTABM(m,DEBBUG=0)
  resultados.data <-  simulABM.conditions.parameters(lista_variaveis_salvar2,resultados)
  
  print(resultados.data)
  
  if(sum(resultados.data[,2]) == length(resultados.data[,2]) & sum(resultados.data[,1]) == 0 ) Marcador.Resultado <- 1; print(m) ; print(periodos_simulacao)
  
  if( sum(resultados.data[,1]) != 0 ) m <- m + 10
  seed <- seq(10,10000,(10000-10)/m)
  
  if( sum(resultados.data[,2]) != length(resultados.data[,2]) ) periodos_simulacao <- periodos_simulacao + 90 ; m  <- m + 10
  
}

plot(resultados[[1]]$agg_gdp[1,],type="l")

for(i in 2:m){
  lines(resultados[[i]]$agg_gdp[1,])
}


############### Resultados de mútiplias simulações 

resettinglistsagents()

#  param.persistent.strat <- 0.8
#  par_brockhommes <- 10


setwd("C:/Users/Nikolas/OneDrive/Documentos/R/Teste GIt/arquives")

m <- 100
periodos_simulacao <- 1000
DEBBUG <- 0

resultados <- simulationMULTABM(m,DEBBUG = 0)

save(resultados,file="resultados_hommes.Rbin",compress=T)

load("resultados_hommes.Rbin")


######## Estatísticas sobre microdados e preços  #########

m <- 100

freq <- matrix(0,quantidade_firmas,periodos_simulacao-1)

freq_simuls <- matrix(0,m,periodos_simulacao-1)

freq_increases <- matrix(0,quantidade_firmas,periodos_simulacao-1)

freq_increases_simuls <- matrix(0,m,periodos_simulacao-1)

frac_increses <- matrix(0,quantidade_firmas,periodos_simulacao-1)

frac_increases_simuls <- matrix(0,m,periodos_simulacao-1)

freq_decreses <- matrix(0,quantidade_firmas,periodos_simulacao-1)

freq_decre_simuls <- matrix(0,m,periodos_simulacao-1)

freq_small <- matrix(0,quantidade_firmas,periodos_simulacao-1)

freq_small_simuls <- matrix(0,m,periodos_simulacao-1)

cov_simuls_freq <- matrix(0,m,1)
cov_simuls_size <- matrix(0,m,1)

freq_change_simuls <- matrix(0,m,1)

for(y in 1:100){
  
  print(y)
  
  for(i in 2:periodos_simulacao){
    
    freq[,i-1] <- ifelse( abs( resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1 ) > 0 , 1, 0)
    
    freq_increases[,i-1] <- ifelse( resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1  > 0 , resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1, 0)
    
    if(sum(freq[,i-1]) >0 ) frac_increases_simuls[y,i-1] <- length( freq_increases[,i-1][ freq_increases[,i-1] > 0 ] )/sum(freq[,i-1])
    
    freq_increases_simuls[y,i-1] <- sum( freq_increases[,i-1][ freq_increases[,i-1] > 0 ] )/length( freq_increases[,i-1][ freq_increases[,i-1] > 0 ] )
    
    freq_decreses[,i-1] <- ifelse( resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1  < 0 , resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1, 0)
    
    freq_decre_simuls[y,i-1] <- sum( freq_decreses[,i-1][ freq_decreses[,i-1] < 0 ] )/length( freq_decreses[,i-1][ freq_decreses[,i-1] < 0 ] )
    
    freq_small[,i-1] <- ifelse( abs( resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1 ) > 0 & abs( resultados[[y]]$firm_price[,i]/resultados[[y]]$firm_price[,i-1] - 1 ) < 0.05, 1, 0)
    
  }
  
  freq_change_simuls[y,1] <- mean( apply(freq,MARGIN=1,sum)/(periodos_simulacao-1))
  
  freq_simuls[y,] <- apply(freq,MARGIN=2,sum)/quantidade_firmas
  
  freq_small_simuls[y,] <- (apply(freq_small,MARGIN=2,sum)/quantidade_firmas)/(apply(freq,MARGIN=2,sum)/quantidade_firmas)
  
  cov_simuls_freq[y,] <- cor.test( resultados[[y]]$agg_inflation[1,800:periodos_simulacao] , freq_simuls[y,799:(periodos_simulacao-1)])$estimate
  
  cov_simuls_size[y,] <- cor.test( resultados[[y]]$agg_inflation[1,800:periodos_simulacao] , freq_increases_simuls[y,799:(periodos_simulacao-1)])$estimate
  
  
}

# Frequency of price change: 

mean(freq_change_simuls * 1/3)
sd((freq_change_simuls * 1/3))

# Implied duration

mean(1/(freq_change_simuls) * 3)
sd(1/(freq_change_simuls ) * 3)


# Porcentagem de ajustes que são aumentos

mean(apply(frac_increases_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean))
sd((apply(frac_increases_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean)))


# Média aumento dos preços quando há aumento

size_increses_simuls <- mean(apply(freq_increases_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean))
size_increses_simuls
sd((apply(freq_increases_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean)))

# Média redução dos preços quando há aumento

freq_decre_simuls[ is.nan(freq_decre_simuls)] <- 0

size_decreases_simuls <- mean(apply(freq_decre_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean))
sd(apply(freq_decre_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean))

### 

freq_small_change_simuls <- mean(apply(freq_small_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean))
freq_small_change_simuls
sd(apply(freq_small_simuls[,(periodos_simulacao - 250):(periodos_simulacao - 1)],MARGIN=1,mean))

cov_infla_1 <- mean(cov_simuls_freq)
cov_infla_2 <- mean(cov_simuls_size)


1/mean(freq_change_simuls) * 3 # Número de meses


plot(apply(freq_simuls,MARGIN=2,mean),type="l")
lines(apply(freq_simuls,MARGIN=2,mean) + apply(freq_simuls,MARGIN=2,sd))
lines(apply(freq_simuls,MARGIN=2,mean) - apply(freq_simuls,MARGIN=2,sd))

plot(1/(apply(freq_simuls,MARGIN=2,mean)))


plot(apply(freq_increases_simuls,MARGIN=2,mean),type="l",ylim=c(-0.1,0.15))
lines(apply(freq_increases_simuls,MARGIN=2,mean) + apply(freq_increases_simuls,MARGIN=2,sd))
lines(apply(freq_increases_simuls,MARGIN=2,mean) - apply(freq_increases_simuls,MARGIN=2,sd))

lines(apply(freq_decre_simuls,MARGIN=2,mean),type="l")
lines(apply(freq_decre_simuls,MARGIN=2,mean) + apply(freq_decre_simuls,MARGIN=2,sd))
lines(apply(freq_decre_simuls,MARGIN=2,mean) - apply(freq_decre_simuls,MARGIN=2,sd))

plot(apply(freq_small_simuls,MARGIN=2,mean),type="l")
lines(apply(freq_small_simuls,MARGIN=2,mean) + apply(freq_small_simuls,MARGIN=2,sd))
lines(apply(freq_small_simuls,MARGIN=2,mean) - apply(freq_small_simuls,MARGIN=2,sd))

sd(apply(freq_small_simuls,MARGIN=2,mean)[!is.nan(apply(freq_small_simuls,MARGIN=2,mean))])


mean(cov_simuls)


mean(apply(frac_increases_simuls,MARGIN=2,mean))
sd((apply(frac_increases_simuls,MARGIN=2,mean)))


######## Eff.Markup firms ######

matrix_dados_markup <- matrix(0,m,periodos_simulacao)

for(i in 1:m){
  
  print(i)

  matrix_dados_markup[i,] <- apply((resultados[[i]]$firm_price/(resultados[[i]]$firm_custo/resultados[[i]]$firm_expdem)),MARGIN=2,med)

}


VAR(firm_custo_unit) * VAR(firm_coef_tecnico) * 0.8

eff_mark <- apply(resultados[[i]]$firm_price/(resultados[[i]]$firm_custo_unit * firm_coef_tecnico[,1] * 0.8),MARGIN=2,mean) - 1
desired_mark <- apply(resultados[[i]]$firm_fixed.markup,MARGIN=2,mean)
seq <- 1:length(desired_mark)

df <- data.frame(eff_mark,desired_mark,seq)

ggplot(df) +
  geom_line(aes(seq,eff_mark)) +
  geom_line(aes(seq,desired_mark)) +
  ylim(0.5,1.25) + xlim(11,1000)




######## Resultados ds distribuições de estratégias #####



regra1 <- 100:periodos_simulacao
regra2 <- 100:periodos_simulacao
regra3 <- 100:periodos_simulacao
regra4 <- 100:periodos_simulacao

for(i in 100:periodos_simulacao){
  
  regra1[i] <- sum(resultados[[1]]$firm_price_rule[,i] == 1)
  regra2[i] <- sum(resultados[[1]]$firm_price_rule[,i] == 2)
  regra3[i] <- sum(resultados[[1]]$firm_price_rule[,i] == 3)
  regra4[i] <- sum(resultados[[1]]$firm_price_rule[,i] == 4)
  
  
}


df <- data.frame(regra1,regra2,regra3,regra4,1:periodos_simulacao)

names(df) <- c("MS","Qual","Dem","Prof","Time")

ggplot(df) +
  geom_line(aes(x=Time,y=MS,col="MS"),col="black",size=0.8) +
  geom_line(aes(x=Time,y=Qual,col="Qual"),size=0.8) +
  geom_line(aes(x=Time,y=Dem,col="Dem"),size=0.8) +
  geom_line(aes(x=Time,y=Prof,col="Prof"),size=0.8) +
  ylim(0,125) +
  xlim(100,periodos_simulacao) +
  ylab("Number of firms") +
  theme(text = element_text(size=10)) +
  theme_light() +
  scale_colour_manual(name='Subtitle',
                      breaks=c('MS', 'Qual','Dem','Prof'),
                      values=c('MS'='black', 'Qual'='blue','Dem'='green','Prof'='red'))




plot(regra1,type="l",ylim=c(0,150))
lines(regra2,col="red")
lines(regra3,col="blue")
lines(regra4,col="green")


#### Gráfico com as correlações ####

corr.function3.bk <- function(results,GDP,Nomes,
                              Titulo,start,periodos,LAGS,frequency){
  
  lowP      <- 6      # bandpass filter minimum period
  highP     <- 32     # bandpass filter maximum period
  bpfK      <- 12     # bandpass filter order
  lags      <- 1      # lags to analyze
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
      
      x <- bkfilter( ts(((x)),frequency = frequency) , pl = lowP, pu = highP, nfix = bpfK )
      y <- bkfilter( ts(((y)),frequency = frequency) , pl = lowP, pu = highP, nfix = bpfK )
      
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

corr <- corr.function3.bk(results = resultados,GDP = c("agg_gdp"),Nomes = c("agg_gdp","agg_consumption","agg_investment","gov_consumption_real","agg_unemployment_rate",
                                                                  "agg_capacity_util","agg_labor_prod","agg_inflation","agg_var_real_wage","cb_ir"),
                          Titulo=c("gdp","con","invest","gov_cons","unem","util","prod","inflation","real_Wage","interest"),start=ceiling(periodos_simulacao*3/4),periodos=6,
                          LAGS=c(1,1,1,1,0,0,1,0,0,0,0)) # Lags para definir o log de quem deve ser tirado


variable_tested <- 6

plot(-6:6,corr$Table.Corr[variable_tested,],type="l",ylim=c(-1,1),main=  row.names(corr$Table.Corr)[vriable_tested] )
seq <- -6:6
arrows(seq,corr$Table.Corr[variable_tested,] + corr$Table.SD.Corr[variable_tested,],seq,
       corr$Table.Corr[variable_tested,] - corr$Table.SD.Corr[variable_tested,],angle = 90,length=0.05,code=3)
lines(seq,Matrix.Empiric[variable_tested,2:14],col="red")

resultados[[1]]$firm_capacidade_producao[,1000]/resultados[[1]]$firm_capacidade_producao[,1000]

#### Correlation graphs ####

df <- t(corr$Table.Corr)
df <- data.frame(df,-6:+6)
df <- data.frame(df, t(corr$Table.SD.Corr))
df <- data.frame(df,t(Matrix.Empiric)[2:14,])

names.sd <- 0:10

for( i in 1:10){
  
  print(i)
  
  print(gsub(" ","",paste("sd.",names(df)[i])))
  
  names.sd[i] <- gsub(" ","",paste("sd.",names(df)[i]))
  
}


names(df) <- c(names(df)[1:10],"seq",names.sd[1:10],row.names(Matrix.Empiric))

# Primeiro gráfico de correlações

p1 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=gdp.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - 2*sd.gdp, ymax=gdp + 2*sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="",y="GDP")+
  theme(text = element_text(size=10))

p2 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=con),size=0.75) +
  geom_line(aes(x=seq,y=consumption.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=con - 2*sd.con, ymax=con + 2*sd.con), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="",y="Consumption")+
  theme(text = element_text(size=10))

p3 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=invest),size=0.75) +
  geom_line(aes(x=seq,y=investment.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=invest - 2*sd.invest, ymax=invest + 2*sd.invest), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1.4) +
  labs(x="",y="Investment")+
  theme(text = element_text(size=10))

p4 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gov_cons),size=0.75) +
  geom_line(aes(x=seq,y=goverment.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gov_cons - 2*sd.gov_cons, ymax=gov_cons + 2*sd.gov_cons), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1.9) +
  labs(x="",y="Gov.Cons")+
  theme(text = element_text(size=10))

graphcorr1 <- grid.arrange(p1,p2,p3,p4,nrow=2)

ggsave(graphcorr1, filename = "graphcorr1.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')

# Segundo gráfico de correlações

p1 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=unem),size=0.75) +
  geom_line(aes(x=seq,y=unemploymeny.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=unem - 2*sd.unem, ymax=unem + 2*sd.unem), position=position_dodge(), color="black",size=0.75) + 
  theme_light() +
  xlim(-6,6) +
  ylim(-1.5,1.5) +
  labs(x="",y="Unem") +
  theme(text = element_text(size=10))

p2 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=util),size=0.75) +
  geom_line(aes(x=seq,y=cu.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=util - 2*sd.util, ymax=util + 2*sd.util), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="",y="Cap.Util")+
  theme(text = element_text(size=10))

p3 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=interest),size=0.75) +
  geom_line(aes(x=seq,y=ff.sw),col="red",size=0.75) +
  #geom_line(aes(x=seq,y=inflation),col="blue",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=interest - 2*sd.interest, ymax=interest + 2*sd.interest), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1.5,1.5) +
  labs(x="",y="Interest rate")+
  theme(text = element_text(size=10))

p4 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=inflation),size=0.75) +
  geom_line(aes(x=seq,y=cpi.sw),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=inflation - 2*sd.inflation, ymax=inflation + 2*sd.inflation), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1.2,1) +
  labs(x="",y="Inflation")+
  theme(text = element_text(size=10))


graphcorr2 <- grid.arrange(p1,p2,p3,p4,nrow=2)

ggsave(graphcorr2, filename = "graphcorr2.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')

#### Tabela standard deviation ####

tabela.DP <- cbind(round(corr$Table.Dp*100,2),Matrix.Empiric[,1],round(round(corr$Table.Dp*100,2)/Matrix.Empiric[,1],2))

colnames(tabela.DP) <- c("Obs [1]","Empirical [2]","[1]/[2]")

row.names(tabela.DP) <- c("GDP","Consumption","Investment","Goverment.Cons","Unem.rate","Cap.Util",
                          "Labor prod.","Inflation","Real wage rate","Base interest rate")

xtable(tabela.DP)


###### ###Estatísticas de preços vs variáveis macros #########

# Matrix - participation of the 4 strategies vs the macro variables. 
# Unemployment, GDP, inflation, Interest rate, Functional distribution, investment

matrix.correlation.results.per.simul <- rep(list(matrix(0,m,7)),4)
matrix.correlation.results.pvalue <- rep(list(matrix(0,m,7)),4)

Matrix.correlations.results <- matrix(0,4,7)
Matrix.correlations.results.pvalue <- matrix(0,4,7)

matrix.participation.mean.sd <- rep(list(matrix(0,m,2)),4)
matrix.participation.final <- matrix(0,4,2)


for(i in 1:m){
  
  part <- matrix(0,4,periodos_simulacao)
  
  init <- round(periodos_simulacao*1/2)
  
  for(y in 1:4){
    
    for(z in init:(periodos_simulacao)){
      
      part[y,z] <- sum( resultados[[i]]$firm_price_rule[,z] == y  )/quantidade_firmas
      
      print(part[y,z])
      
    }
    
    matrix.participation.mean.sd[[y]][i,1] <- mean(part[y,init:periodos_simulacao])
    matrix.participation.mean.sd[[y]][i,2] <- sd(part[y,init:periodos_simulacao])
    
    matrix.correlation.results.per.simul[[y]][i,1] <- cor( resultados[[i]]$agg_unemployment_rate[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )
    matrix.correlation.results.per.simul[[y]][i,2] <- cor( log(resultados[[i]]$agg_gdp[1,init:periodos_simulacao]),part[y,init:periodos_simulacao] )
    matrix.correlation.results.per.simul[[y]][i,3] <- cor( resultados[[i]]$agg_inflation[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )
    matrix.correlation.results.per.simul[[y]][i,4] <- cor( resultados[[i]]$cb_ir[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )
    matrix.correlation.results.per.simul[[y]][i,5] <- cor( resultados[[i]]$agg_function_dist[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )
    matrix.correlation.results.per.simul[[y]][i,6] <- cor( log(resultados[[i]]$agg_investment[1,init:periodos_simulacao]),part[y,init:periodos_simulacao] )
    matrix.correlation.results.per.simul[[y]][i,7] <- cor( apply(resultados[[i]]$firm_fixed.markup[,init:periodos_simulacao],MARGIN=2,mean),part[y,init:periodos_simulacao] )
    
    matrix.correlation.results.pvalue[[y]][i,1] <- cor.test( resultados[[i]]$agg_unemployment_rate[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )$p.value
    matrix.correlation.results.pvalue[[y]][i,2] <- cor.test( resultados[[i]]$agg_gdp[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )$p.value
    matrix.correlation.results.pvalue[[y]][i,3] <- cor.test( resultados[[i]]$agg_inflation[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )$p.value
    matrix.correlation.results.pvalue[[y]][i,4] <- cor.test( resultados[[i]]$cb_ir[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )$p.value
    matrix.correlation.results.pvalue[[y]][i,5] <- cor.test( resultados[[i]]$agg_function_dist[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )$p.value
    matrix.correlation.results.pvalue[[y]][i,6] <- cor.test( resultados[[i]]$agg_investment[1,init:periodos_simulacao],part[y,init:periodos_simulacao] )$p.value
    matrix.correlation.results.pvalue[[y]][i,7] <- cor.test( apply(resultados[[i]]$firm_fixed.markup[,init:periodos_simulacao],MARGIN=2,mean),part[y,init:periodos_simulacao] )$p.value
    
  }
}


for(i in 1:4){
  
  matrix.correlation.results.per.simul[[i]] <- matrix(matrix.correlation.results.per.simul[[i]][ !is.na(matrix.correlation.results.per.simul[[i]])  ],,7)
  matrix.correlation.results.pvalue[[i]] <- matrix(matrix.correlation.results.pvalue[[i]][ !is.na(matrix.correlation.results.pvalue[[i]])  ],,7)
}

for(y in 1:4){
  
  Matrix.correlations.results[y,] <- apply(matrix.correlation.results.per.simul[[y]],MARGIN=2,mean)
  Matrix.correlations.results.pvalue[y,] <- apply(matrix.correlation.results.pvalue[[y]],MARGIN=2,mean)
  matrix.participation.final[y,] <-  round(apply(matrix.participation.mean.sd[[y]],MARGIN=2,median),2)
  
}

hist( matrix.correlation.results.pvalue[[y]][,1],50)

Matrix.correlations.results <- round(Matrix.correlations.results,2)
Matrix.correlations.results.pvalue <- round(Matrix.correlations.results.pvalue,4)

row.names(Matrix.correlations.results) <- c("Market share","Quality","Profitability","Demand")
colnames(Matrix.correlations.results) <- c("Unemployment", "GDP", "Inflation","Interest rate","Wage/Income","Investment","Markup")

row.names(matrix.participation.final) <- c("Market share","Quality","Profitability","Demand")
colnames(matrix.participation.final) <- c("Mean","SD")



xtable(matrix.participation.final)

xtable(t(Matrix.correlations.results))

## Participation

rule.mean.part <- rep(list(matrix(0,m,periodos_simulacao)),4)

rule.mean.part.pondms <- rep(list(matrix(0,m,periodos_simulacao)),4)

for(i in 1:m){
  for(y in 1:periodos_simulacao){
    for(z in 1:4){
      
      rule.mean.part[[z]][i,y] <- sum( resultados[[i]]$firm_price_rule[,y] == z )
      
    }
  }
}

for(i in 1:m){
  for(y in 1:periodos_simulacao){
    for(z in 1:4){
      
      print(i)
      
      rule.mean.part.pondms[[z]][i,y] <- sum( resultados[[i]]$firm_price_rule[,y][ resultados[[i]]$firm_price_rule[,y] == z ]/z * 
                                                resultados[[i]]$firm_ms[,y][ resultados[[i]]$firm_price_rule[,y] == z] )
      
    }
  }
}

plot( apply( rule.mean.part.pondms[[1]] , MARGIN=2, mean)[100:periodos_simulacao] ,type="l",ylim=c(0,1))
lines( (apply(rule.mean.part[[1]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao] , col="red" ) # Market-share
#lines( (apply(rule.mean.part[[1]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao] + (apply(rule.mean.part[[1]],MARGIN=2,sd)/quantidade_firmas)[100:periodos_simulacao] , col="red" ) # Market-share
lines( (apply(rule.mean.part[[2]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao] , col="green" ) # Quality
#lines( (apply(rule.mean.part[[2]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao] + (apply(rule.mean.part[[2]],MARGIN=2,sd)/quantidade_firmas)[100:periodos_simulacao], col="green" ) # Quality
lines( (apply(rule.mean.part[[3]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao] , col="blue" ) # Profitability
lines( (apply(rule.mean.part[[4]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao] , col="yellow" ) # Demand


df <- data.frame((apply(rule.mean.part[[1]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao],
                 (apply(rule.mean.part[[2]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao],
                 (apply(rule.mean.part[[3]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao],
                 (apply(rule.mean.part[[4]],MARGIN=2,mean)/quantidade_firmas)[100:periodos_simulacao],
                 100:periodos_simulacao)


# Gráfico de participação média das estratégias

names(df) <- c("MS","Qual","Profit","Demand","Seq")


mean_heuristic_graph <- ggplot(df) +
  geom_line(aes(x=Seq,y=MS,col="MS"),col="black",size=0.8) +
  geom_line(aes(x=Seq,y=Qual,col="Qual"),size=0.8) +
  geom_line(aes(x=Seq,y=Demand,col="Dem"),size=0.8) +
  geom_line(aes(x=Seq,y=Profit,col="Prof"),size=0.8) +
  ylim(0,0.5) +
  xlim(100,periodos_simulacao) +
  ylab("Percentage") +
  xlab("Time") +
  theme(text = element_text(size=10)) +
  theme_light() +
  scale_colour_manual(name='',
                      breaks=c('MS', 'Qual','Dem','Prof'),
                      values=c('MS'='black', 'Qual'='blue','Dem'='green','Prof'='red'))



ggsave(mean_heuristic_graph, filename = "mean_heuristic_graph.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')



# Começar daqui para ciclo
result.ccf1 <- rep(list(matrix(0,m,13)),4)

init <- round(periodos_simulacao*1/2)

for(h in 1:4){
  for(i in 1:m){
    
    x <- bkfilter( ts(((  rule.mean.part[[h]][i,][init:periodos_simulacao]   )))/quantidade_firmas , pl = lowP, pu = highP, nfix = bpfK )$cycle
    y <- bkfilter( ts(((  log(resultados[[i]]$agg_gdp[1,][init:periodos_simulacao])  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
    
    x <- x[ is.na(x) == FALSE]
    y <- y[ is.na(y) == FALSE]
    
    
    grangertest(x,y, order = 6)
    
    
    teste <- ccf1_INTERN(x,y,lags.max = 6)
    
    result.ccf1[[h]][i,1:13] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
    
  }
}

for(i in 1:4){
  
  col.size <- length( result.ccf1[[i]][1,] )
  
  data <- result.ccf1[[i]][ !is.na(result.ccf1[[i]]) ]
  
  result.ccf1[[i]] <- matrix(data,,col.size)
  
  
}



rule1.cycle <-  apply(result.ccf1[[1]],MARGIN=2,mean)
rule2.cycle <-  apply(result.ccf1[[2]],MARGIN=2,mean)
rule3.cycle <-  apply(result.ccf1[[3]],MARGIN=2,mean)
rule4.cycle <-  apply(result.ccf1[[4]],MARGIN=2,mean)

rule1.cycle.sd <-  apply(result.ccf1[[1]],MARGIN=2,sd)
rule2.cycle.sd <-  apply(result.ccf1[[2]],MARGIN=2,sd)
rule3.cycle.sd <-  apply(result.ccf1[[3]],MARGIN=2,sd)
rule4.cycle.sd <-  apply(result.ccf1[[4]],MARGIN=2,sd)

seq <- -6:6

seq2 <- rep(0,13)

df <- data.frame(rule1.cycle,rule1.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p1 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Market share heuristic",y="") +
  theme(text = element_text(size=10))


df <- data.frame(rule2.cycle,rule2.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p2 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Quality heuristic",y="") +
  theme(text = element_text(size=10))


df <- data.frame(rule3.cycle,rule3.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p3 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Profitability heuristic",y="") +
  theme(text = element_text(size=10))

df <- data.frame(rule4.cycle,rule4.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p4 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Demand heuristic",y="") +
  theme(text = element_text(size=10))


graphcorr2 <- grid.arrange(p1,p2,p3,p4,nrow=2)

ggsave(graphcorr2, filename = "graph_corr_heuristics_gdp.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')



plot(seq,rule1.cycle,ylim=c(-0.5,0.5),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6)
legend(1, 200, legend=c("Market share"),col=c("black"), lty=1, cex=1)
arrows(seq,rule1.cycle + rule1.cycle.sd,seq,
       rule1.cycle- rule1.cycle.sd,angle = 90,length=0.05,code=3)
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)


plot(seq,rule2.cycle,ylim=c(-0.5,0.5),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6,col=c("blue"))
arrows(seq,rule2.cycle + rule2.cycle.sd,seq,
       rule2.cycle- rule2.cycle.sd,angle = 90,length=0.05,code=3,col=c("blue"))
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)
legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
       col=c("black", "blue","green","red"), lty=1, cex=1)

plot(seq,rule3.cycle,ylim=c(-0.4,0.4),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6,col=c("green"))
arrows(seq,rule3.cycle + rule3.cycle.sd,seq,
       rule3.cycle- rule3.cycle.sd,angle = 90,length=0.05,code=3,col=c("green"))
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)
legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
       col=c("black", "blue","green","red"), lty=1, cex=1)

plot(seq,rule4.cycle,ylim=c(-0.4,0.4),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6,col=c("red"))
arrows(seq,rule4.cycle + rule4.cycle.sd,seq,
       rule4.cycle- rule4.cycle.sd,angle = 90,length=0.05,code=3,col=c("red"))
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)+
  legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
         col=c("black", "blue","green","red"), lty=1, cex=1)




# Começar daqui para ciclo p/ analisar mark-up
result.ccf1 <- rep(list(matrix(0,m,13)),4)

init <- round(periodos_simulacao*1/2)

for(h in 1:4){
  for(i in 1:m){
    
    print(paste("H:",h,"i:",i))
    
    x <- bkfilter( ts(((  rule.mean.part[[h]][i,][init:periodos_simulacao]   )))/quantidade_firmas , pl = lowP, pu = highP, nfix = bpfK )$cycle
    y <- bkfilter( ts(((  apply(resultados[[i]]$firm_fixed.markup,MARGIN=2,mean)[init:periodos_simulacao]  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
    
    x <- x[ is.na(x) == FALSE]
    y <- y[ is.na(y) == FALSE]
    
    
    X <- cbind(x,y)
    
    qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(X)))
    
    
    plot(y,type="l",ylim=c(-0.3,0.3))
    lines(x)
    
    teste <- ccf1_INTERN(x,y,lags.max = 6)
    
    result.ccf1[[h]][i,1:13] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
    
  }
}

for(i in 1:4){
  
  col.size <- length( result.ccf1[[i]][1,] )
  
  data <- result.ccf1[[i]][ !is.na(result.ccf1[[i]]) ]
  
  result.ccf1[[i]] <- matrix(data,,col.size)
  
  
}



rule1.cycle <-  apply(result.ccf1[[1]],MARGIN=2,mean)
rule2.cycle <-  apply(result.ccf1[[2]],MARGIN=2,mean)
rule3.cycle <-  apply(result.ccf1[[3]],MARGIN=2,mean)
rule4.cycle <-  apply(result.ccf1[[4]],MARGIN=2,mean)

rule1.cycle.sd <-  apply(result.ccf1[[1]],MARGIN=2,sd)
rule2.cycle.sd <-  apply(result.ccf1[[2]],MARGIN=2,sd)
rule3.cycle.sd <-  apply(result.ccf1[[3]],MARGIN=2,sd)
rule4.cycle.sd <-  apply(result.ccf1[[4]],MARGIN=2,sd)

seq <- -6:6

seq2 <- rep(0,13)

df <- data.frame(rule1.cycle,rule1.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p1 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Market share heuristic",y="") +
  theme(text = element_text(size=10))


df <- data.frame(rule2.cycle,rule2.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p2 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Quality heuristic",y="") +
  theme(text = element_text(size=10))


df <- data.frame(rule3.cycle,rule3.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p3 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.75,0.5) +
  labs(x="Profitability heuristic",y="") +
  theme(text = element_text(size=10))

df <- data.frame(rule4.cycle,rule4.cycle.sd,seq,seq2)

names(df) <- c("gdp","sd.gdp","seq","linecross")

p4 <- ggplot(data=df) +
  geom_line(aes(x=seq,y=gdp),size=0.75) +
  geom_line(aes(x=seq,y=linecross),col="red",size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=gdp - sd.gdp, ymax=gdp + sd.gdp), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-0.5,0.5) +
  labs(x="Demand heuristic",y="") +
  theme(text = element_text(size=10))


graphcorr2 <- grid.arrange(p1,p2,p3,p4,nrow=2)

ggsave(graphcorr2, filename = "graph_corr_heuristics_markup.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


plot(seq,rule1.cycle,ylim=c(-0.3,0.3),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6)
legend(1, 200, legend=c("Market share"),col=c("black"), lty=1, cex=1)
arrows(seq,rule1.cycle + rule1.cycle.sd,seq,
       rule1.cycle- rule1.cycle.sd,angle = 90,length=0.05,code=3)
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)


plot(seq,rule2.cycle,ylim=c(-0.5,0.5),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6,col=c("blue"))
arrows(seq,rule2.cycle + rule2.cycle.sd,seq,
       rule2.cycle- rule2.cycle.sd,angle = 90,length=0.05,code=3,col=c("blue"))
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)
legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
       col=c("black", "blue","green","red"), lty=1, cex=1)

plot(seq,rule3.cycle,ylim=c(-0.3,0.3),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6,col=c("green"))
arrows(seq,rule3.cycle + rule3.cycle.sd,seq,
       rule3.cycle- rule3.cycle.sd,angle = 90,length=0.05,code=3,col=c("green"))
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)
legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
       col=c("black", "blue","green","red"), lty=1, cex=1)

plot(seq,rule4.cycle,ylim=c(-0.3,0.3),type="lty",ylab="",lwd=2,xlab="",cex.axis=1.6,col=c("red"))
arrows(seq,rule4.cycle + rule4.cycle.sd,seq,
       rule4.cycle- rule4.cycle.sd,angle = 90,length=0.05,code=3,col=c("red"))
lines(seq,rep(0,13),col=c("black"),lty=c("dotted"),lwd=2,xlab="",cex.axis=1.6)+
  legend(1, 200, legend=c("Market share", "Quality","Profitabilty","Demand"),
         col=c("black", "blue","green","red"), lty=1, cex=1)


result.ccf4 <- matrix(0,m,13)


for(i in 1:m){
  
  print(paste("H:",h,"i:",i))
  
  x <- bkfilter( ts(((  log(resultados[[i]]$agg_gdp[init:periodos_simulacao])   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  y <- bkfilter( ts(((  apply(resultados[[i]]$firm_fixed.markup,MARGIN=2,mean)[init:periodos_simulacao]  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  
  x <- x[ is.na(x) == FALSE]
  y <- y[ is.na(y) == FALSE]
  
  plot(y,type="l",ylim=c(-0.3,0.3))
  lines(x)
  
  teste <- ccf1_INTERN(x,y,lags.max = 6)
  
  result.ccf4[i,1:13] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
  
}


gdp_mkup.cycle <-  apply(result.ccf4,MARGIN=2,mean)

plot(gdp_mkup.cycle,type="l")


#arrows(seq,corr$Table.Corr[variable_tested,] + corr$Table.SD.Corr[variable_tested,],seq,
#       corr$Table.Corr[variable_tested,] - corr$Table.SD.Corr[variable_tested,],angle = 90,length=0.05,code=3)


x <- bkfilter( ts(((x))) , pl = lowP, pu = highP, nfix = bpfK )
y <- bkfilter( ts(((y))) , pl = lowP, pu = highP, nfix = bpfK )

x <- x$cycle[ is.na(x$cycle) == FALSE]
y <- y$cycle[ is.na(y$cycle) == FALSE]

result.ccf <- ccf1_INTERN(x,y,lags.max = periodos)

Matrices[[name]][i,] <- t( as.matrix(result.ccf$acf))



plot(apply(resultados[[4]]$firm_leverage * resultados[[1]]$firm_ms,MARGIN=2,sum),type="l")

plot(resultados[[4]]$agg_function_dist[1,])


######## Relação da rigidez de preços com o ciclo econômico ######

# Começar daqui para ciclo
result.ccf1 <- matrix(0,m,13)

init <- round(periodos_simulacao*1/2)


for(i in 1:m){
    
    
    firm_thres_av <- apply(resultados[[i]]$firm_threshold,MARGIN=2,mean)
    
    x <- bkfilter( ts(((  firm_thres_av[init:periodos_simulacao]   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
    y <- bkfilter( ts(((  log(resultados[[i]]$agg_gdp[1,][init:periodos_simulacao])  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
    
    x <- x[ is.na(x) == FALSE]
    y <- y[ is.na(y) == FALSE]
    
    #plot(x,type="l",ylim=c(-0.05,0.05))
     # lines(y)
    
      #cor(x,y)
      
    grangertest(x,y, order = 6)
    grangertest(y,x, order = 6)
    
    
    teste <- ccf1_INTERN(x,y,lags.max = 6)
    
    result.ccf1[i,] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
    
}


plot(apply(result.ccf1,MARGIN=2,mean),type="l")





##### Gráficos  ######

graph.function(results=resultados,c("agg_gdp","agg_unemployment_rate","agg_inflation","agg_function_dist","firm_leverage","firm_threshold"),
                Titulo=c("Aggregate GDP","Unemployment rate","Aggregate inflation","Agg.Func.Dist","Ave.Firm.Indebt","Ave.Firm.Thres"),start=250,
                LOG=c(1,0,0,0,0,0,0))


graph.function(results=resultados,c("firm_threshold"),
               Titulo=c("Ave.Firm.Thres"),start=periodos_simulacao-900,
               LOG=c(0))


graph_fixed_markup <- graph.function(results=resultados,c("firm_fixed.markup"),
                                     Titulo=c("Ave.Eff.Markup"),start=100,
                                     LOG=c(0))

ggsave(graph_fixed_markup, filename = "grahs_gdp.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')

graph.function(results=resultados2,c("agg_inflation"),
               Titulo=c("Ave.Inflation rate"),start=750,
               LOG=c(0))


graph.function(results=resultados,c("cb_ir"),
               Titulo=c("Ave.Base.Interest rate"),start=750,
               LOG=c(0))

graph.function(results=resultados2,c("agg_unemployment_rate"),
               Titulo=c("Ave.Unem rate"),start=750,
               LOG=c(0))

graph.function(results=resultados2,c("agg_empresas_falidas"),
               Titulo=c("Number of bankrupt firms"),start=750,
               LOG=c(0))


matrix.sd <- matrix(0,30,3)

for(i in 1:30){
  
  ts(resultados[[i]]$agg_inflation[1,1:periodos_simulacao],frequency = 4)
  
  variable <- bkfilter( ts( resultados[[i]]$agg_inflation[1,1:periodos_simulacao]) , pl = 6, pu =36 , nfix=12 , drift = FALSE )
  variable2 <- (variable$cycle[ is.na(variable$cycle) == FALSE])
  
  plot(variable$cycle[500:950,1],type="l")
  lines(variable$trend)
  
  variable$x - variable$cycle - variable$trend
  
  Matrix.Empiric
  
  plot(resultados[[i]]$agg_unemployment_rate[1,1:periodos_simulacao],type="l")
  
  matrix.sd[i,1] <- sd(resultados[[i]]$agg_gdp[1,200:periodos_simulacao]/mean(resultados[[1]]$agg_gdp[1,200:periodos_simulacao])-1)
  matrix.sd[i,2] <- sd(resultados[[i]]$agg_inflation[1,200:periodos_simulacao]/mean(resultados[[i]]$agg_inflation[1,200:periodos_simulacao])-1)
  matrix.sd[i,3] <- sd(resultados[[i]]$agg_unemployment_rate[1,200:periodos_simulacao]/mean(resultados[[i]]$agg_unemployment_rate[1,200:periodos_simulacao])-1)
  
}

apply(matrix.sd,MARGIN=2,mean)

##### Gráficos dos ciclos das variáveis macroeconômicas####

x <- bkfilter( ts(((  log(resultados[[1]]$agg_investment[1,750:1000])   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
y <- bkfilter( ts(((  log(resultados[[1]]$agg_consumption[1,750:1000])  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
z <- bkfilter( ts(((  log(resultados[[1]]$gov_expense[1,750:1000])  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
k <- bkfilter( ts(((  log(resultados[[1]]$agg_gdp[1,750:1000])  ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle

x <- x[ is.na(x) == FALSE]
y <- y[ is.na(y) == FALSE]
z <- z[ is.na(z) == FALSE]
k <- k[ is.na(k) == FALSE]

seq <- (750+12):(1000-12)

df <- data.frame(x,y,z,k,seq)

cycle_graph <- ggplot(data=df) +
  geom_line(aes(seq,x,col="Investment"),size=0.3) +
  geom_line(aes(seq,y,col="Consumption"),size=0.3) +
  geom_line(aes(seq,z,col="Gov.Exp"),size=0.3) +
  labs(x="Time",y="Filtered series") +
  theme_light() +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        legend.position = c(0.2,0.2))  +
  ggtitle("(b) Cycle (bpf)") +
  scale_colour_manual(name = 'Legend',
                      breaks=c('Investment', 'Consumption','Gov.Exp'),
                      values=c('Investment'='red', 'Consumption'='blue','Gov.Exp' = 'black'))



    Matrix.Mean <- matrix(0,m,periodos_simulacao)
    Matrix.Sd <- matrix(0,m,periodos_simulacao)
    
    for(Simul in 1:m){
      for(time in 1:periodos_simulacao){
        

          Matrix.Mean[Simul,time] <- log(resultados[[Simul]]$agg_gdp[,time])
          Matrix.Sd[Simul,time] <- log(resultados[[Simul]]$agg_gdp[,time])
          
        }
      }
    
    start <- 750
    
    one <- Matrix.Mean[1,start:periodos_simulacao]
    mean <- apply(Matrix.Mean,MARGIN=2,med)[start:periodos_simulacao]
    sd <- apply(Matrix.Mean,MARGIN=2,sd)[start:periodos_simulacao]
    df_graph <- data.frame(mean,sd,start:periodos_simulacao,one)
    names(df_graph) <- c("mean","sd","time","one")
    
    gdp_graph <- ggplot(data=df_graph)+
      geom_line(aes(time,one),col='blue',size=0.3)+
      scale_linetype_manual(values=c("twodash")) +
      geom_line(aes(time,mean+sd),linetype="dashed",col="red",size=0.3) +
      geom_line(aes(time,mean - sd),linetype="dashed",col="red",size=0.3) +
      geom_line(aes(time,mean),size=1)+
      theme_bw() +
      labs( y = "Ave.Agg.GDP (log)" , x = "Time") +
      theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5)) +
      ggtitle("(a) Level (logs) ")

    
    

graphcorr2 <- grid.arrange(gdp_graph,cycle_graph,ncol=2)

ggsave(graphcorr2, filename = "grahs_gdp.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


ipca <- (1 + rollmean(resultados[[1]]$agg_inflation[1,500:1000],4))^4 - 1
interest <- (1 + rollmean(resultados[[1]]$cb_ir[1,500:1000],4))^4 - 1
inflation_target <- rep(0.02,498)
unemployment_rate <- resultados[[1]]$agg_unemployment_rate[1,503:1000]
seq <- 1:498

df <- data.frame(ipca,interest,inflation_target,unemployment_rate,seq)

ggplot(data=df) +
  geom_line(aes(x=seq,y=ipca)) +
  geom_line(aes(x=seq,y=interest),col="red",size=1) +
  labs(x="",y="") +
  theme_light() +
  theme(text = element_text(size=20))


ggplot(data=df) +
  geom_line(aes(x=seq,y=unemployment_rate),col="blue",size=1)

#### Concentração de mercado ####


max_ms_matrix <- matrix(0,m,periodos_simulacao)
HH_index_matrix <- matrix(0,m,periodos_simulacao)

for(i in 1:100){
  
  max_ms_matrix[i,] <- apply(resultados[[i]]$firm_ms,MARGIN=2,max)
  HH_index_matrix[i,] <- apply(resultados[[i]]$firm_ms^2,MARGIN=2,sum)
}


mean_max_ms_matrix_matrix <- apply(max_ms_matrix,MARGIN=2,mean)
sd_max_ms_matrix_matrix <- apply(max_ms_matrix,MARGIN=2,sd)
seq <- 1:periodos_simulacao

df <- data.frame(mean_max_ms_matrix_matrix[100:periodos_simulacao],sd_max_ms_matrix_matrix[100:periodos_simulacao],
                 seq[100:periodos_simulacao],rep(1/quantidade_firmas,length(100:periodos_simulacao)))

names(df) <- c("mean","sd","time","rep")

max_ms_graph <- ggplot(df) +
  geom_line(aes(x=time,y=mean,color="black"),color="black") +
  geom_line(aes(x=time,y=rep,color="blue"),color="blue") +
  geom_line(aes(x=time,y=mean + sd,color="red"),color="red") +
  geom_line(aes(x=time,y=mean - sd),color="red") +
  labs(x="Time",y="% of the market") +
  theme_light() +
  ggtitle("(b) Max. market share") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5))


mean_HH_matrix_matrix <- apply(HH_index_matrix,MARGIN=2,mean)
sd_HH_matrix_matrix <- apply(HH_index_matrix,MARGIN=2,sd)
seq <- 1:periodos_simulacao

df <- data.frame(mean_HH_matrix_matrix[100:periodos_simulacao],sd_HH_matrix_matrix[100:periodos_simulacao],
                 seq[100:periodos_simulacao],rep(1/quantidade_firmas,length(100:periodos_simulacao)))

names(df) <- c("mean","sd","time","rep")

max_HH_graph <- ggplot(df) +
  geom_line(aes(x=time,y=mean,color="black"),color="black") +
  geom_line(aes(x=time,y=rep,color="blue"),color="blue") +
  geom_line(aes(x=time,y=mean + sd,color="red"),color="red") +
  geom_line(aes(x=time,y=mean - sd),color="red") +
  labs(x="Time",y="Index") +
  theme_light() +
  ggtitle("(a) HH Index") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5))

### Gilbrat law

df <- data.frame(c(resultados[[1]]$firm_ms[,1000],
                   resultados[[1]]$firm_ms[,1000-4],
                   resultados[[1]]$firm_ms[,1000-8],
                   resultados[[1]]$firm_ms[,1000-12]))

df <- data.frame(c(resultados[[1]]$firm_ms[,1000-4],
                   resultados[[1]]$firm_ms[,1000-8]))

names(df) <- "growth"

df_g <- data.frame(log(df$growth) - med(log(df$growth)))

df_g <- data.frame(c(resultados[[1]]$firm_ms[,1000]/resultados[[1]]$firm_ms[,1000-4] -1,
                     resultados[[1]]$firm_ms[,1000-4]/resultados[[1]]$firm_ms[,1000-8] -1))

names(df_g) <- "growth"

df <- data.frame(df,df_g)

summary(lm(df$growth.1 ~ df$growth))

ggplot(data=df , aes(x=growth,y=growth.1)) +
  geom_point(shape=1, color="red", size=3) + 
  geom_smooth(method = "lm") +
  theme_light() +
  labs(x="",y="")


graphcorr1 <- grid.arrange(max_HH_graph,max_ms_graph,ncol=2)

ggsave(graphcorr1, filename = "two_graphs_market_concentration.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


#### 

hist(resultados[[1]]$firm_new_machines/resultados[[1]]$firm_machines)

plot(resultados[[1]]$firm_machines[,1000],resultados[[1]]$firm_vendas[,1000])

plot(log(resultados[[1]]$firm_ms[,1000]),log(201 - rank(resultados[[1]]$firm_ms[,1000])))

JarqueBeraTest(resultados[[1]]$firm_ms[,1000])

#### Firms market-share #####

df <- data.frame(c(resultados[[1]]$firm_ms[,1000],
                   resultados[[1]]$firm_ms[,1000-4],
                   resultados[[1]]$firm_ms[,1000-8],
                   resultados[[1]]$firm_ms[,1000-12]))

hist(resultados[[1]]$firm_E[,1000],50)


names(df) <- c("growth")

firms_market_share_graph <- ggplot(data=df,aes(growth)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white",bins=25) +
  stat_function(fun = dnorm, args = list(mean=mean(df$growth), sd=sd(df$growth)),col="blue",size=2) +
  geom_density(size=2,col="green") +
  theme_light() +
  labs(x="Market share",y="Density") +
  ggtitle("(a) Market share distribution ") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5))


###

df <- data.frame(c(resultados[[1]]$firm_E[,1000],
                   resultados[[1]]$firm_E[,1000-4],
                   resultados[[1]]$firm_E[,1000-8],
                   resultados[[1]]$firm_E[,1000-12]))




for(i in 1:4){
  
  resultados[[1]]$firm_price[,1000 - (i*4)]
  
  y <- 1000 - (i*4)
  
  mudancas <- rep(0,quantidade_firmas)
  mudancas2 <- rep(0,quantidade_firmas)
  
  for(z in 1:24){
    
    mudancas <- ifelse( resultados[[1]]$firm_price[,y - z ]/resultados[[1]]$firm_price[,y - z - 1] - 1 != 0 , 1, 0) + mudancas
    
    mudancas2 <- resultados[[1]]$firm_price[,y - z ]/resultados[[1]]$firm_price[,y - z - 1] - 1 + mudancas2
    
  }
  
  
}


plot(mudancas2,resultados[[1]]$firm_E[,1000] )


summary(lm( resultados[[1]]$firm_E[,1000] ~ mudancas))



resultados[[1]]$firm_price[,1000]/resultados[[1]]$firm_price[,999] - 1




hist(resultados[[1]]$firm_E[,1000],50)


names(df) <- c("growth")

mean(df$growth)> med(df$growth)


symmetry.test(
  df$growth,
  side = c("right"),
  boot = TRUE,
  B = 1000,
  q = 8/9
)


firms_market_share_graph_2 <- ggplot(data=df,aes(growth)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white",bins=25) +
  stat_function(fun = dnorm, args = list(mean=mean(df$growth), sd=sd(df$growth)),col="blue",size=2) +
  geom_density(size=2,col="green") +
  theme_light() +
  labs(x="Competitiveness",y="Density") +
  ggtitle("(b) Firms' compet. distribution ") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5))

graphcorr1 <- grid.arrange(firms_market_share_graph,firms_market_share_graph_2,ncol=2)

ggsave(graphcorr1, filename = "ms_distribution.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')

#### Investment spikes ####

matrix_graphs <- matrix(0,2,periodos_simulacao/2)

for(i in (periodos_simulacao/2):periodos_simulacao){
  
  k_relation <- resultados[[1]]$firm_new_machines[,i]/resultados[[1]]$firm_machines[,i]
  
  matrix_graphs[1,i - 500] <- sum(k_relation > 0.25)
  matrix_graphs[2,i - 500] <- sum(k_relation < 0.05)
  
}

plot(matrix_graphs[2,]/quantidade_firmas,ylim=c(0,1),type="l")
lines(matrix_graphs[1,]/quantidade_firmas)

df <- data.frame(  resultados[[1]]$firm_new_machines[,i]/resultados[[1]]$firm_machines[,i] )

names(df) <- "spike"

graph_investment_spike <- ggplot(data=df,aes(spike)) +
  geom_histogram(aes(y = ..density..)) +
  theme_light() +
  labs(x="New Machines / Total Machines",y="Density") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5)) +
  ggtitle("(b) Investment/Mach.Stock")



#graphcorr1 <- grid.arrange(firms_market_share_graph,graph_investment_spike,ncol=2)

ggsave(graph_investment_spike, filename = "ms_spikes.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


#### RD investment #####

matrix_tot_invest <- matrix(0,m,periodos_simulacao)

for(i in 1:m){
  
  matrix_tot_invest[i,] <- apply(resultados[[i]]$firm_RD_invest,MARGIN=2,sum)
  
}


result.ccf4 <- matrix(0,m,13)


for(i in 1:m){
  
  x <- bkfilter( ts(((  log( resultados[[i]]$agg_gdp[init:periodos_simulacao] )   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  y <- bkfilter( ts(((  log(apply(resultados[[i]]$firm_RD_invest,MARGIN=2,sum)[init:periodos_simulacao]  )))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  
  x <- x[ is.na(x) == FALSE]
  y <- y[ is.na(y) == FALSE]
  
  plot(y,type="l",ylim=c(-0.3,0.3))
  lines(x)
  
  teste <- ccf1_INTERN(x,y,lags.max = 6)
  
  result.ccf4[i,1:13] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
  
}

mean_RD <- apply(result.ccf4,MARGIN=2,mean)
sd_RD <-   apply(result.ccf4,MARGIN=2,sd)
seq <- -6:6

df <- data.frame(mean_RD,sd_RD,seq)


rd_invest <-  ggplot(data=df) +
  geom_line(aes(x=seq,y=mean_RD),size=0.75,col="blue") +
  geom_errorbar(mapping=aes(x=seq, ymin=mean_RD - 2*sd_RD, ymax=mean_RD + 2*sd_RD), position=position_dodge(), color="blue",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="Time",y="RD investment")+
  theme(text = element_text(size=10))



#### Total leverage ####


result.ccf4 <- matrix(0,m,13)


for(i in 1:m){
  
  x <- bkfilter( ts(((  log( resultados[[i]]$agg_gdp[init:periodos_simulacao] )   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  y <- bkfilter( ts(((  (apply(resultados[[i]]$firm_leverage * resultados[[i]]$firm_ms,MARGIN=2,sum)[init:periodos_simulacao]  )) )) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  
  x <- x[ is.na(x) == FALSE]
  y <- y[ is.na(y) == FALSE]
  
  summary(lm(x ~ y))
  
  plot(x,type="l",ylim=c(-0.05,0.05))
  lines(y,col="red")
  
  teste <- ccf1_INTERN(y,x,lags.max = 6)
  
  result.ccf4[i,1:13] <- t(as.matrix(ccf1_INTERN(y,x,lags.max = 6)$acf))
  
}

mean_RD <- apply(result.ccf4,MARGIN=2,mean)
sd_RD <-   apply(result.ccf4,MARGIN=2,sd)
seq <- -6:6

df <- data.frame(mean_RD,sd_RD,seq)


leverage_gdp_graph <- ggplot(data=df) +
  geom_line(aes(x=seq,y=mean_RD),size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=mean_RD - 2*sd_RD, ymax=mean_RD + 2*sd_RD), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="",y="GDP")+
  theme(text = element_text(size=10))


#### Total debt GDP ####


matrix_tot_invest <- matrix(0,m,periodos_simulacao)

for(i in 1:m){
  
  matrix_tot_invest[i,] <- apply(resultados[[i]]$firm_debt_stock,MARGIN=2,sum)
  
}


result.ccf4 <- matrix(0,m,13)


for(i in 1:m){
  
  x <- bkfilter( ts(((  log( resultados[[i]]$agg_gdp[init:periodos_simulacao] )   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  y <- bkfilter( ts(((  log(apply(resultados[[i]]$firm_debt_stock,MARGIN=2,sum)[init:periodos_simulacao]  )))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  #y <- bkfilter( ts(((  (apply(resultados[[i]]$firm_debt_stock,MARGIN=2,sum)[init:periodos_simulacao]  )/(apply(resultados[[i]]$agg_nom_demand,MARGIN=2,sum)[init:periodos_simulacao]  ) ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  
  x <- x[ is.na(x) == FALSE]
  y <- y[ is.na(y) == FALSE]
  
  summary(lm(x ~ y))
  
  plot(y,type="l",ylim=c(-0.3,0.3))
  lines(x)
  
  teste <- ccf1_INTERN(x,y,lags.max = 6)
  
  result.ccf4[i,1:13] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
  
}

mean_RD <- apply(result.ccf4,MARGIN=2,mean)
sd_RD <-   apply(result.ccf4,MARGIN=2,sd)
seq <- -6:6

df <- data.frame(mean_RD,sd_RD,seq)


debt_gdp_graph <- ggplot(data=df) +
  geom_line(aes(x=seq,y=mean_RD),size=0.75,col="red") +
  geom_errorbar(mapping=aes(x=seq, ymin=mean_RD - 2*sd_RD, ymax=mean_RD + 2*sd_RD),col="red", position=position_dodge(),size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="Time",y="Debt/GDP") +
  theme(text = element_text(size=12))


graphcorr1 <- grid.arrange(rd_invest,debt_gdp_graph,ncol=2)

ggsave(graphcorr1, filename = "RD_debt_cross_gdp.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


##### Taxa de juros e correlação com o produto #####

matrix_tot_invest <- matrix(0,m,periodos_simulacao)

for(i in 1:m){
  
  matrix_tot_invest[i,] <- apply(resultados[[i]]$cb_ir,MARGIN=2,sum)
  
}


result.ccf4 <- matrix(0,m,13)


for(i in 1:m){
  
  x <- bkfilter( ts(((  log( resultados[[i]]$agg_gdp[init:periodos_simulacao] )   ))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  y <- bkfilter( ts(((  log(apply(resultados[[i]]$firm_fb,MARGIN=2,sum)[init:periodos_simulacao] /
                              apply(resultados[[i]]$firm_receita,MARGIN=2,sum)[init:periodos_simulacao] )))) , pl = lowP, pu = highP, nfix = bpfK )$cycle
  
  x <- x[ is.na(x) == FALSE]
  y <- y[ is.na(y) == FALSE]
  
  plot(y,type="l",ylim=c(-0.05,0.05))
  lines(x,col="red")
  
  lm( y ~ x )
  
  teste <- ccf1_INTERN(x,y,lags.max = 6)
  
  result.ccf4[i,1:13] <- t(as.matrix(ccf1_INTERN(x,y,lags.max = 6)$acf))
  
}

mean_RD <- apply(result.ccf4,MARGIN=2,mean)
sd_RD <-   apply(result.ccf4,MARGIN=2,sd)
seq <- -6:6

df <- data.frame(mean_RD,sd_RD,seq)

ggplot(data=df) +
  geom_line(aes(x=seq,y=mean_RD),size=0.75) +
  geom_errorbar(mapping=aes(x=seq, ymin=mean_RD - 2*sd_RD, ymax=mean_RD + 2*sd_RD), position=position_dodge(), color="black",size=0.75)+ 
  theme_light() +
  xlim(-6,6) +
  ylim(-1,1) +
  labs(x="",y="GDP")+
  theme(text = element_text(size=10))


##### GDP Growth-Rate Distribution #####

df <- data.frame(diff(resultados[[1]]$agg_gdp[1,500:1000])/resultados[[1]]$agg_gdp[1,(501-4):(1000-4)])

names(df) <- "growh"

qplot(x =growh,data=df, geom="auto")

ggplot(data=df,aes(growh)) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  stat_function(fun = dnorm, args = list(mean=mean(df$growh), sd=sd(df$growh)),col="darkred",size=2) +
  #stat_function(fun = dlaplace, args = list(x=1, mu=0,b=1),col="darkred",size=1)

dlaplace(1,0,1)

JarqueBeraTest(df$growh)  



####### Estabilidade dos estoques #######


cap_riq_matrix <- matrix(0,m,periodos_simulacao)

for(i in 1:100){
  
  cap_riq_matrix[i,] <- resultados[[i]]$capitalist_riqueza[1,]/resultados[[i]]$agg_nom_demand[1,]
  
}


mean_cap_riq_matrix <- apply(cap_riq_matrix,MARGIN=2,mean)
sd_cap_riq_matrix <- apply(cap_riq_matrix,MARGIN=2,sd)
seq <- 1:periodos_simulacao

df <- data.frame(mean_cap_riq_matrix[100:periodos_simulacao],sd_cap_riq_matrix[100:periodos_simulacao],seq[100:periodos_simulacao])

names(df) <- c("mean","sd","time")

cap_riqeza_graph <- ggplot(df) +
  geom_line(aes(x=time,y=mean,color="black"),color="black") +
  geom_line(aes(x=time,y=mean + sd,color="red"),color="red") +
  geom_line(aes(x=time,y=mean - sd),color="red") +
  labs(x="Time",y="Ratio") +
  theme_light() +
  ggtitle("(b) Wealth/GDP") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5))

#### 


gov_debt_matrix <- matrix(0,m,periodos_simulacao)

for(i in 1:100){
  
  gov_debt_matrix[i,] <- resultados[[i]]$gov_debt[1,]/resultados[[i]]$agg_nom_demand[1,]
  
}

mean_gov_debt <- apply(gov_debt_matrix,MARGIN=2,mean)
sd_gov_debt <- apply(gov_debt_matrix,MARGIN=2,sd)
seq <- 1:periodos_simulacao

df <- data.frame(mean_gov_debt[100:periodos_simulacao],sd_gov_debt[100:periodos_simulacao],seq[100:periodos_simulacao])

names(df) <- c("mean","sd","time")

gov_debt_graph <- ggplot(df) +
  geom_line(aes(x=time,y=mean,color="black"),color="black") +
  geom_line(aes(x=time,y=mean + sd,color="red"),color="red") +
  geom_line(aes(x=time,y=mean - sd),color="red") +
  labs(x="Time",y="Ratio") +
  theme_light() +
  ggtitle("(a) Gov.Debt/GDP") +
  theme(text = element_text(size=10),plot.title = element_text(hjust = 0.5))

two_graphs_stock <- grid.arrange(gov_debt_graph,cap_riqeza_graph,nrow=1)

ggsave(two_graphs_stock,filename="two_graphs_stock.png", dpi = 300, type = 'cairo',
       width = 16, height = 16*0.6, units = 'cm')


var_gov_debt <- diff(mean_gov_debt)[100:999]/mean_gov_debt[100:999]

# var_gov_debt <- mean_gov_debt

length(var_gov_debt)

segundaparte <- var_gov_debt[(900-2*225-1):(900-226)]
tercaparte <- var_gov_debt[(900-225):900]

stats::ks.test(c(segundaparte), c(tercaparte),exact = NULL,
               alternative = c("two.sided"))

plot(ecdf(segundaparte),
     xlim = range(c(segundaparte, tercaparte)),
     col = "blue")
plot(ecdf(tercaparte),
     add = TRUE,
     lty = "dashed",
     col = "red")

dist <- data.frame(tercaparte,segundaparte)

names(dist) <- c("X","Y")

ggplot(data=dist,aes(X)) +
  geom_histogram(aes(X),colour = 1, fill = "white") +
  geom_histogram(aes(Y),colour = 1, fill = "grey") #+
geom_density(kernel="gaussian",color="darkblue", fill="lightblue")




####

stats_change_rule <- matrix(0,m,periodos_simulacao)


for(i in 1:m){
  for(y in 1:quantidade_firmas){
    for(t in 2:periodos_simulacao){
      
      if( resultados[[i]]$firm_price_rule[y,t] != resultados[[i]]$firm_price_rule[y,t-1] ){
        
        stats_change_rule[i,t] <- stats_change_rule[i,t] + 1
        
      }
    }
  }
}


plot(apply(stats_change_rule,MARGIN=2,mean)/quantidade_firmas,type="l")


hist(resultados[[40]]$firm_vendas[,1000],100)

plot(log(resultados[[40]]$firm_vendas[,1000]),200 - rank(resultados[[40]]$firm_ms[,1000]))

hist(resultados[[75]]$firm_ms[,1000],50)
hist(resultados[[75]]$firm_tot_funcionarios[,1000],20)

plot((resultados[[1]]$firm_receita[,1000] - resultados[[1]]$firm_custo[,1000])/resultados[[1]]$firm_receita[,1000],resultados[[1]]$firm_vendas[,1000])

#############


variables <- c("agg_unemployment_rate","agg_inflation","agg_gdp","agg_function_dist")
Names <- c("Unemployment","Inflation","GDP","Function Income Dist")


init <- periodos_simulacao - 200
end <- periodos_simulacao

LN <- c(0,0,1,0)

function_stability_series_model <- function(LN,init,end,variables,Names){
  
  matrix.results.stacionaty <- matrix(0,5,length(variables))
  
  colnames(matrix.results.stacionaty) <- Names
  
  row.names(matrix.results.stacionaty) <- c("Tau","Crit.val Tau","Phi","Crit.val Phi","P-value")
  
  
  for(y in 1:length(variables)){
    
    
    matrix.stats.stac <- matrix(0,length(resultados),5)
    
    
    for(i in 1:length(resultados)){
      
      
      serie <- gsub(" ","",paste("resultados[[",i,"]]$",variables[y]))
      
      serie <- eval(parse(text=serie))[,init:end]
      
      if(LN[y] == 1){
        
        serie <- log10(serie)
        
      }
      
      df.test <- urca::ur.df(serie,lags = 2, selectlags = "AIC",type="drift")
      
      df.test@teststat[[1]]
      
      df.test1 <- tseries::adf.test(serie,k=1)
      
      matrix.stats.stac[i,5] <- df.test1$p.value
      
      matrix.stats.stac[i,1] <- df.test@teststat[[1]]
      matrix.stats.stac[i,2] <- df.test@cval[[3]]
      
      matrix.stats.stac[i,3] <- df.test@teststat[[2]]
      matrix.stats.stac[i,4] <- df.test@cval[[4]]
      
      
    }
    
    matrix.results.stacionaty[,y] <- round( apply(matrix.stats.stac, MARGIN=2, mean) , 2)
    
  }
  
  
  
  return(matrix.results.stacionaty)
  
  
}   


table.stab <- function_stability_series_model(LN,init,end,variables,Names)


#############


matrix.mean.num_firm_folllow_strat <- matrix(0,m,periodos_simulacao)


rule1 <- 1:periodos_simulacao 
rule2 <- 1:periodos_simulacao
rule3 <- 1:periodos_simulacao
rule4 <- 1:periodos_simulacao

for(z in 1:m){
  for(y in 1:4){
    for(i in 1:periodos_simulacao){
      
      matrix.mean.num_firm_folllow_strat[z,i] <- sum(resultados[[z]]$firm_price_rule[,i] == y)
      
      
    }
  }
}

plot(matrix.mean.num_firm_folllow_strat[4,]/200,type="l")

plot(results[[1]]$firm_price_rule[1,])




#############

Mean <- apply(results$firm_quality,MARGIN=2,mean)

Adj <- results$firm_quality


for(i in 1:periodos_simulacao){
  
  Adj[,i] <- Adj[,i]/Mean[i]
  
}


sum(results$firm_price_rule[,1000] == 1)


plot(apply( results$firm_price_rule == 1 , MARGIN=2,sum)/quantidade_firmas,type="l")


plot(apply(Adj, MARGIN=2, sd),type="l",ylim=c(0,0.5))
lines(results$agg_inflation[1,]*20,col="red")
lines(apply(results$firm_prob_costbased,MARGIN=2,mean),col="blue")

cor(apply(Adj, MARGIN=2, sd),results$agg_inflation[1,])
cor(apply(results$firm_prob_costbased,MARGIN=2,mean),results$agg_inflation[1,])
cor(apply(Adj, MARGIN=2, sd),apply(results$firm_prob_costbased,MARGIN=2,mean))

### 

sort(data.time.function)


# Pk K * 2

plot( results$firm_price[1,]/results$firmk_price[1,] , type="l" )


plot( results$firm_profit[1,30:periodos_simulacao]/results$firm_assets[1,30:periodos_simulacao] , type="l" )

plot(apply(results$firm_dividendos,MARGIN=2,sum)/apply(results$firm_assets,MARGIN=2,sum),type="l")

plot(results$agg_unemployment_rate[1,100:periodos_simulacao],type="l")
lines(results$agg_investment[1,100:periodos_simulacao]/results$agg_index_prices[1,100:periodos_simulacao])

plot(results$firm_dividendos[1,30:periodos_simulacao]/results$firm_assets[1,30:periodos_simulacao] , type="l" )

plot(results$firm_ms[1,],type="l")
lines(results$firm_leverage[1,]/100,type="l")


plot(results$agg_inflation[1,10:periodos_simulacao],type="l",ylim=c(-0.005,0.01))
lines(results$cb_ir[1,10:periodos_simulacao],col="red")

plot(results$agg_unemployment_rate[1,],type="l")

plot( results$firm_assets[1,10:periodos_simulacao] ,type="l",ylim=c(0,max(results$firm_assets[1,10:periodos_simulacao])*1.1))
lines( results$firm_debt_stock[1,10:periodos_simulacao] ,type="l",col="red")

plot(results$agg_empresas_falidas[1,]/quantidade_firmas,type="l")  

plot(apply(results$firm_price_rule,MARGIN=2,mean),type = "l")

plot(results$firm_debt_stock[1,10:periodos_simulacao]/results$firm_assets[1,10:periodos_simulacao],type="l")


DF.test <- urca::ur.df( results$agg_gdp[1,] )


variables <- cbind(c(0,diff(results$agg_gdp[1,20:450])),results$agg_inflation[1,20:450])

model <- vars::VAR(variables)

summary(model)

vars::roots(model)



lista_variaveis_salvar

which( lista_variaveis_salvar == "firm_custo_unit")

variables <- matrix(0,1,2)



for( i in 1:round(length(lista_variaveis_salvar))){
  
  print(i)
  
  Text <- gsub(" ","",paste("results$",lista_variaveis_salvar[[i]]))
  
  Var <- eval(parse(text = Text))
  
  Var <- Var[,(350):450]
  
  if( is.null(nrow(Var)) == FALSE ){
    
    Var <- apply(Var, MARGIN=2, mean)
    
  }
  
  test <- summary(urca::ur.df(Var))
  
  if( is.nan(test@teststat[1]) == FALSE){
    
    if( sqrt((test@cval[2])^2) > sqrt( test@teststat[1]^2 ) ){
      
      Var <- diff(Var)
      
      test <- summary(urca::ur.df(Var))
      
      if( sqrt((test@cval[2])^2) > sqrt( test@teststat[1]^2 ) ) print( lista_variaveis_salvar[[i]] ) ; break
      
    }
  }
  
  
  
  
  for( i in 1:round(length(lista_variaveis_salvar))){
    
    print(i)
    
    Text <- gsub(" ","",paste("results$",lista_variaveis_salvar[[i]]))
    
    Var <- eval(parse(text = Text))
    
    Var <- Var[,(350):450]
    
    if( is.null(nrow(Var)) == FALSE ){
      
      Var <- apply(Var, MARGIN=2, mean)
      
    }
    
    test <- summary(urca::ur.df(Var))
    
    if( is.nan(test@teststat[1]) == FALSE){
      
      if( sqrt((test@cval[2])^2) > sqrt( test@teststat[1]^2 ) ){
        
        Var <- c(0,diff(Var))
        
      }
      
      
      if(i == 1){  Variables <- matrix(Var,length(Var),1) ; colnames(Variables) <- lista_variaveis_salvar[[i]] 
      
      regress.model <- lm(Var[2:length(Var)] ~ Var[1:(length(Var)-1)])
      
      regress.model$coefficients[2]
      
      coeficientes <- regress.model$coefficients[2]
      names(coeficientes) <- lista_variaveis_salvar[[i]] 
      
      
      }else{
        
        Nomes_antes <- colnames(Variables) 
        
        Variables <- cbind(Var,Variables)
        
        colnames(Variables) <- c( lista_variaveis_salvar[[i]] , Nomes_antes)
        
        regress.model <- lm(Var[2:length(Var)] ~ Var[1:(length(Var)-1)])
        
        regress.model$coefficients[2]
        
        coeficient <- regress.model$coefficients[2]
        
        coeficientes <- cbind(coeficientes,coeficient)
        
        colnames(coeficientes) <- c( lista_variaveis_salvar[[i]] , Nomes_antes)
        
      }
      
    }
    
  }
  
  
  coeficientes[ coeficientes > 1]
  
  colnames( coeficientes )[ which(coeficientes > 1) ]
  
  coeficientes[ which(coeficientes > 1) ]
  
  plot(Variables.df$firm_ms , type="l")
  
  plot(results$firm_ms[1,],type="l")
  
  
  test <- summary(urca::ur.df(Variables.df$agg_function_dist))
  
  
  plot(results$bank_deposits[350:450],type="l")
  
  
  plot(Variables.df$agg_nom_demand,type = "l")
  
  
  Variables.df <- data.frame(Variables)
  
  colnames(Variables) <- seq(1, ncol(Variables) , 1)
  
  
  
  Variables.model <- cbind( Variables.df$agg_gdp, Variables.df$agg_inflation , Variables.df$agg_investment,
                            Variables.df$firm_assets , Variables.df$workers_salario, Variables.df$agg_labor_prod,
                            Variables.df$firm_investimento)
  
  
  MoDeL <- vars::VAR(Variables.model)
  
  vars::roots(MoDeL)
  
  
  plot(apply(results$firm_dividendos,MARGIN=2,sum)/results$agg_nom_demand[1,],type="l")
  
  
  plot(results$firm_deficitsurplus[1,])
  plot(results$firm_dividendos[1,])
  
  plot( results$firm_dividendos[1,]/results$firm_deficitsurplus[1,] , type="l")
  
  results$firm_dividendos[1,30:50]
  results$firm_deficitsurplus[1,30:50]
  
  round( results$firm_dividendos[1,]/results$firm_deficitsurplus[1,],3)
  
  
  plot( results$firm_cash[1,30:200] / results$firm_assets[1,30:200] , type="l")
  
  plot(results$agg_inflation[1,50:100],type="l")
  plot(results$firm_leverage[1,30:100],type="l")
  
  capitalist_consumo
  
  plot(results$firm_dividendos[1,],type="l")
  
  plot(results$firm_receita_op[1,30:periodos_simulacao],type="l")
  plot(results$firm_deficitsurplus[1,30:periodos_simulacao]/results$firm_receita_op[1,30:100],type = "l")
  plot(results$firm_assets[1,30:periodos_simulacao],type="l",ylim=c(0,1000))
  lines(results$firm_debt_stock[1,30:periodos_simulacao],type="l",col="red")
  lines(results$firm_investimento[1,30:periodos_simulacao],type="l",col="blue")
  
  plot(results$firm_leverage[1,30:periodos_simulacao],type="l")
  
  plot( results$firm_debt_stock[1,30:periodos_simulacao]/results$firm_assets[1,30:periodos_simulacao] , type="l")
  
  plot( results$firm_dividendos[1,30:periodos_simulacao]/(results$firm_assets[1,30:periodos_simulacao] - results$firm_debt_stock[1,30:periodos_simulacao]),type="l")
  
  results$firm_receita[1,30:periodos_simulacao]/results$firm_assets[1,30:periodos_simulacao]
  
  
  plot(results$agg_empresas_falidas[1,30:100],type="l")
  
  ### Plots uma simulação
  
  plot(results$firm_profit[1,],type="l")
  
  series1 <- results$capitalist_riqueza[1,(ncol(results$capitalist_riqueza) - 10):ncol(results$capitalist_riqueza)]
  
  series2 <- results$agg_gdp[1,(ncol(results$agg_gdp) - 10):ncol(results$agg_gdp)]
  
  series <- cbind(series1,series2)
  
  plot(results$capitalist_cash_percent[1,],type="l")
  
  
  plot(results$agg_investment[1,]/results$agg_nom_demand[1,],type="l")
  
  which( firm_investimento[,1] == max(firm_investimento[,1]) )
  
  plot(results$firm_investimento[152,],type="l")
  
  plot(results$firm_capacidade_producao[152,],type="l")
  lines(results$firm_expdem[152,],col="red")
  
  plot(results$firm_leverage[152,],type="l") 
  lines(rep(1,ncol(results$firm_leverage)))
  
  lines(results$firmk_price[1,],col="red") 
  
  lines(results$agg_index_prices[1,])
  lines(results$capitalist_riqueza[1,]/1000000)
  lines(results$agg_empresas_falidas[1,]/10,col="green")
  
  plot(results$agg_investment[1,]/results$agg_nom_demand[1,],type = "l")
  plot(results$capitalist_consumo[1,]/results$agg_nom_demand[1,],type = "l")
  
  plot( apply( results$workers_salario , MARGIN=2, sum )/results$agg_nom_demand[1,] , type="l")
  
  plot(results$cb_ir[1,],type="l",ylim=c(-0.03,0.04))
  lines(results$agg_inflation[1,],type="l",col="red")
  
  
  plot(results$capitalist_riqueza[1,]/results$agg_nom_demand[1,],type="l")
  
  plot(apply(results$firm_fixed.markup,MARGIN = 2,mean),type="l",ylim=c(0,0.5))
  
  plot(apply(results$agg_inflation,MARGIN=2,mean),col="red",type="l")
  
  unem <- results$agg_unemployment_rate[1,30:40]
  
  test <- lm(series1[5:11] ~ series1[4:10])
  
  acf(series1)
  
  test$coefficients
  
  
  library(vars)
  
  VARselect(series)
  
  model <- vars::VAR(series)
  
  vars::roots(model)
  
  
  diff <- urca::ur.df(series)
  
  summary(diff)
  
  periodos_simulacao/20
  
  plot(diff(results$agg_unemployment_rate[1,]),type="l")
  
  
  lm(results$agg_gdp[1,(ncol(results$agg_gdp) - 30):ncol(results$agg_gdp)] ~ results$agg_gdp[1,(ncol(results$agg_gdp) - 31):(ncol(results$agg_gdp)-1)])
  
  
  
  plot( apply(results$firm_capacidade_producao,MARGIN=2,sum) , type="l")
  
  
  
  ############### Calibração  ################
  
  ### Matrix.Empiric ####
  
  #Antes da análise as variáveis de quantidade, que mostram crescimento exponencial, é tirado o log das variáveis
  # Preços e salários são transformadas tirando o logaritmo e a diferença dos logaritmos.
  
  # Fonte: Business cycle fluctuations in U.S. macroeconomic time series
  # https://www.nber.org/system/files/working_papers/w6528/w6528.pdf
  
  # SD , -6 ,-5, -4, -3, -2, -1, 0, 1, 2, 3, 4,5,6
  
  gdp.sw <- c(-0.29,-0.18,0.03,0.33,0.66,0.91)
  
  # STD e cros autocorrelation with gdp
  gdp.sw <- c(1.66, gdp.sw, 1, rev(gdp.sw))
  
  # Consumo total
  consumption.sw <- c(-0.39,-0.28,-0.07,0.21,0.51,0.76,0.9,0.89,0.75,0.53,0.29,0.09,-0.06)
  consumption.sw <- c(1.26,consumption.sw)
  
  # Investimento total
  
  investment.sw <- c(-0.34,-0.19,0.04,0.32,0.61,0.82,0.89,0.73,0.49,0.23,-0.01,-0.2,-0.31)
  investment.sw <- c(4.97,investment.sw)
  
  # Goverment purchases
  
  goverment.sw <- c(0.30,0.25,0.22,0.21,0.21,0.19,0.15,0.03,-0.1,-0.2,-0.23,-0.19,-0.09)
  goverment.sw <- c(2.49,goverment.sw)
  
  # Unemployment rate
  
  unemploymeny.sw <- c(0.13,-0.03,-0.27,-0.55,-0.8,-0.93,-0.89,-0.69,-0.39,-0.07,0.19,0.33,0.37)
  unemploymeny.sw <- c(0.76,unemploymeny.sw)
  
  # Vacancies
  
  
  # Capacity utilization
  
  cu.sw <- c(-0.37,-0.23,0.01,0.31,0.63,0.86,0.93,0.83,0.59,0.29,0.02,-0.16,-0.25)
  cu.sw <- c(3.07,cu.sw)
  
  # Average labour productivity
  
  lbprod.sw <- c(-0.49,-0.6,-0.58,-0.41,-0.11,0.24,0.53,0.7,0.72,0.62,0.47,0.32,0.21)
  lbprod.sw <- c(1.05,lbprod.sw)
  
  # Consumer price index - rate 
  
  cpi.sw <- c(0.34,0.47,0.58,0.64,0.62,0.52,0.35,0.14,-0.08,-0.27,-0.4,-0.48,-0.51)
  cpi.sw <- c(1.44,cpi.sw)
  
  # Real wage rate - rate of change
  
  rwr.sw <- c(-0.05,-0.13,-0.18,-0.18,-0.13,-0.05,0.04,0.08,0.08,0.04,0,-0.04,-0.05)
  rwr.sw <- c(1.1,rwr.sw)
  
  # Federal funds rate
  
  ff.sw <- c(0.26,0.38,0.5,0.6,0.63,0.56,0.38,0.13,-0.16,-0.41,-0.6,-0.69,-0.71)
  ff.sw <- c(1.47,ff.sw)
  
  # Monetary base 
  
  mb.sw<- c()
  mb.sw<- c()
  
  # Matrix 
  
  Matrix.Empiric <- rbind(gdp.sw,consumption.sw,investment.sw,goverment.sw,
                          unemploymeny.sw,cu.sw,lbprod.sw,cpi.sw,rwr.sw,
                          ff.sw)
  
  div_TT <- 0.9
  div_TF <- 1
  div_FT <- 0.6
  div_FF <- 0.2
  
  Matrix.Empiric[,1] - corr$Table.Dp[,1]*100
  
  # Variáveis
  
  install.packages("mFilter")
  install.packages("GA")
  library(mFilter)
  library("GA")
  
  periodos_simulacao <- 300
  
  
  
  MAX <- c(0.05,10,0.9,  1,1.5, 0.1,0.8,3,0.5)
  MIN <- c(0   ,5,0.25,0.8,0.5,0.03,0.4,0.2,0.05)
  
  X <- (MAX - MIN)/2 + MIN
  
  
  names_parameters <- c("sd_labor","min.deprec","dividend_policy",
                        "infla_indexation","par_var_wages",
                        "max_readjustment_wage_ref",
                        "firm.past.cost","par_reac_infla","var.markup.ms")
  
  
  names(MAX) <- names_parameters
  names(MIN) <- names_parameters
  
  for( i in 1:length(ls())){
    
    
    print(paste( ls()[i],object.size(eval(parse( text = ls()[i]))) ))
    
  }
  
  rm(simulationABM)
  rm(results)
  rm(simulationABMFAST)
  rm(simulationABMFAST2OLD)
  
  
  
  install.packages("doParallel")
  install.packages("Rlab")
  
  library(Rlab)
  library(parallel)
  library(doParallel)
  
  
  lista_variaveis_salvar <- list("agg_gdp","agg_consumption","agg_investment","gov_consumption_real","agg_unemployment_rate",
                                 "agg_capacity_util","agg_labor_prod","agg_inflation","agg_var_real_wage","cb_ir")
  
  check_variaveis_salvar(lista_variaveis_salvar)
  
  
  DEBBUG <- 0
  
  simulationMULTABM_SIMPLIFICADO <- function(m){
    
    gc() # limpa o lixo para liberar espaço
    
    print("Starting the simulation. Wait")
    
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
      
      print(list[[i]])
      
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
      
      DEBBUG <- 1
      
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
  
  test <- runif(length(MAX),0,1)*(MAX - MIN) + MIN
  
  Matrix.Empiric
  
  # Suggestions <- c(par.pas.dem,effect.price.mk,effect.qual.mk,reac_gov,reac_gov_infla,
  #                  par_reac_infla,par_reac_emprego,multiplicador_prod_setor_capital,
  #                  div_TT,div_TF,div_FT,div_FF,par_limit_wage_ref_firm,lambda_cash_capitalist,lambda_zero_cash,
  #                  pro_consumir_wealth_cap,pro_consumir_dividends,
  #                  gov_inertia,inertia_cb,r,par_unem.ben,effect.prob.price)
  
  # Suggestions <- c(par.pas.dem,effect.price.mk,effect.qual.mk,reac_gov,reac_gov_infla,
  #                  par_reac_infla,par_reac_emprego,multiplicador_prod_setor_capital,
  #                  div_TT,div_TF,div_FT,div_FF,par_limit_wage_ref_firm,lambda_cash_capitalist,lambda_zero_cash,
  #                  pro_consumir_wealth_cap,pro_consumir_dividends,
  #                  gov_inertia,inertia_cb,r,par_unem.ben,effect.prob.price)
  
  
  Suggestions <- c(sd_labor,min.deprec,dividend_policy,
                   infla_indexation,par_var_wages,
                   max_readjustment_wage_ref,
                   firm.past.cost,par_reac_infla,var.markup.ms)
  
  periodos_simulacao <- 500
  
  simulABMGA(Suggestions)
  
  install.packages("GA")
  library(GA)
  
  GA <- GA::ga(type = "real-valued", fitness = simulABMGA, lower = MIN, upper = MAX , popSize=20, maxiter = 20 , run = 10,
               parallel = FALSE , suggestions = Suggestions)
  
  plot(GA)
  solution <- summary(GA)[[11]]
  Suggestions <- solution
  
  # Suggestions
  
  #sd_labor min.deprec dividend_policy infla_indexation par_var_wages max_readjustment_wage_ref firm.past.cost par_reac_infla var.markup.ms
  #[1,] 0.04249319   8.325118        0.676561        0.8745078      1.163887                0.03414753      0.5878666       1.953584     0.3196382
  
  colnames(Suggestions) <- names_parameters
  names(MAX) <- names_parameters
  names(MIN) <- names_parameters
  
  
  #solution <- c(0.05,0.02,0.01,0.01,0.005,0.1,0.05,5,0.9,1,0.6,
  #              0.2642036,0.02082593,-2,0.3,0.05,0.6,0.9,0.8,0.02135402,0.25)
  
  
  for(i in 1:length(solution)){
    
    assign(names_parameters[i],solution[i],envir=globalenv())
    
  }
  
  
  
  
  
  test_function <- function(K){
    
    
    
    for(i in 1:periodos_simulacao){
      
      K <- K + K
      
      
    }
    
    
    return( K )
    
  }
  
  
  if(!require(tseries)){install.packages('tseries')}
  
  jarque.bera.test(resultados[[1]]$firm_tot_funcionarios[,1000])  
  
  hist(resultados[[1]]$firm_vendas[,1000],100)
  
  
  hist((resultados[[1]]$firm_receita[,1000] - resultados[[1]]$firm_custo[,1000])/resultados[[1]]$firm_receita[,1000],200)
  
  
  
#### Experimetents
  
  
  best_option
  
  
  
  
  
  