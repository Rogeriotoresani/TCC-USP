
#LENDO ARQUIVO SIMIL
SIMIL <- read.csv2("Dados_Simil_GIHAB-VT_7902_2024-01-28_172751.CSV", header=TRUE)

#FILTRANDO APENAS INFORMAÇÕES DE VITÓRIA, SERRA E VILA VELHA QUE SE REFIRAM A LAUDO DE AVALIAÇÃO E APARTAMENTO
SIMIL <- filter(SIMIL, SIMIL$Município=="VITORIA" | SIMIL$Município=="SERRA" | SIMIL$Município=="VILA VELHA" & SIMIL$Grupo.de.Imóveis== "Unidade em Prédio" &
                          SIMIL$Tipo.de.Peça.Técnica=="Laudo de Avaliação" & 
                          SIMIL$Categoria.do.Imóvel..tipo.de.imóvel.== "Apartamento")

#SELECIONANDO AS VARIÁVEIS PERTINENTES
SIMIL <- SIMIL %>% dplyr::select(Ordenador, Ano, CEP, Bairro, Logradouro, Nº,  Bloco, Nº.da.Unidade, Município,  
                                                 `Coordenadas.Grau.Decimal`,
                                                 Total.Geral.de.Vagas, Quarto.s...dormitórios., Total.Geral.de.Vagas, 
                                                 Área.considerada.para.avaliação..m2., R..m2, Data.de.Refência.da.Avaliação,
                                                 Valor.de.Avaliação..R.., Padrão.das.Edificações.na.Região,
                                                 Aquecimento.Solar, Equipamentos.de.Segurança, Espaço.com.Churrasqueira,
                                                 Mini.Quadra.Esportiva, Piscina, Portaria.e.ou.Guarita, Salão.de.Festas,
                                                 Outros.Salões.de.Lazer, Sauna...Ofurô...Hidromassagem, Quadra.Poliesportiva,
                                                 Pilotis, Acabamento.Fachada.Principal.da.Edificação, Esquadrias.Fachada.Principal.da.Edificação,
                                                 Padrão.de.acabamento.da.Unidade, Estado.de.Conservação.da.Unidade,
                                                 Idade.Estimada.da.edificação.como.um.todo, Nº.de.Unidades.no.Andar.da.UP,
                                                 Nº.de.pavimentos.do.Prédio..UP., Estado.de.Conservação.do.prédio..UP.,
                                                 )

#CONVERTENDO AS VARIAVEIS CATEGORICAS EM FACTOR
SIMIL$Padrão.das.Edificações.na.Região<- as.factor(SIMIL$Padrão.das.Edificações.na.Região)
SIMIL$Aquecimento.Solar<- as.factor(SIMIL$Aquecimento.Solar)
SIMIL$Equipamentos.de.Segurança<- as.factor(SIMIL$Equipamentos.de.Segurança)
SIMIL$Espaço.com.Churrasqueira<- as.factor(SIMIL$Espaço.com.Churrasqueira)
SIMIL$Mini.Quadra.Esportiva<- as.factor(SIMIL$Mini.Quadra.Esportiva)
SIMIL$Piscina<- as.factor(SIMIL$Piscina)
SIMIL$Portaria.e.ou.Guarita<- as.factor(SIMIL$Portaria.e.ou.Guarita)
SIMIL$Salão.de.Festas<- as.factor(SIMIL$Salão.de.Festas)
SIMIL$Outros.Salões.de.Lazer<- as.factor(SIMIL$Outros.Salões.de.Lazer)
SIMIL$Sauna...Ofurô...Hidromassagem<- as.factor(SIMIL$Sauna...Ofurô...Hidromassagem)
SIMIL$Quadra.Poliesportiva<- as.factor(SIMIL$Quadra.Poliesportiva)
SIMIL$Pilotis<- as.factor(SIMIL$Pilotis)
SIMIL$Acabamento.Fachada.Principal.da.Edificação<- as.factor(SIMIL$Acabamento.Fachada.Principal.da.Edificação)
SIMIL$Esquadrias.Fachada.Principal.da.Edificação<- as.factor(SIMIL$Esquadrias.Fachada.Principal.da.Edificação)
SIMIL$Padrão.de.acabamento.da.Unidade<- as.factor(SIMIL$Padrão.de.acabamento.da.Unidade)
SIMIL$Estado.de.Conservação.da.Unidade<- as.factor(SIMIL$Estado.de.Conservação.da.Unidade)
SIMIL$Idade.Estimada.da.edificação.como.um.todo<- as.factor(SIMIL$Idade.Estimada.da.edificação.como.um.todo)
SIMIL$Estado.de.Conservação.do.prédio..UP.<- as.factor(SIMIL$Estado.de.Conservação.do.prédio..UP.)

#CONVERTENDO AS VARIAVEIS DE TEXTO EM CHARACTER
SIMIL$Ordenador<- as.character(SIMIL$Ordenador)
SIMIL$Ano<- as.character(SIMIL$Ano)
SIMIL$CEP<- as.character(SIMIL$CEP)

#CONVERTENDO AS VARIAVEIS DE DATA EM DATE
SIMIL$Data.de.Refência.da.Avaliação<-as.Date(SIMIL$Data.de.Refência.da.Avaliação,format = '%d/%m/%Y')

#FILTRANDO INTERVALOS PARA ELIMINAR DADOS INCONSISTENTES
SIMIL <- filter(SIMIL, SIMIL$Total.Geral.de.Vagas<=5)
SIMIL <- filter(SIMIL, SIMIL$Quarto.s...dormitórios.>=1 & SIMIL$Quarto.s...dormitórios.<=5)
SIMIL <- filter(SIMIL, SIMIL$Área.considerada.para.avaliação..m2.>=30 & SIMIL$Área.considerada.para.avaliação..m2.<=400)                
SIMIL <- filter(SIMIL, SIMIL$R..m2>=2000)
SIMIL <- filter(SIMIL, SIMIL$Padrão.das.Edificações.na.Região =="Normal (forte predominância)" | SIMIL$Padrão.das.Edificações.na.Região =="Normal (c/ aspectos de alto)" |
  SIMIL$Padrão.das.Edificações.na.Região == "Normal (c/ aspectos de baixo)" | SIMIL$Padrão.das.Edificações.na.Região == "Alto (por predominância)" |
  SIMIL$Padrão.das.Edificações.na.Região == "Baixo")
SIMIL <- SIMIL %>% mutate(Aquecimento.Solar = NULL)
SIMIL <- filter(SIMIL, SIMIL$Acabamento.Fachada.Principal.da.Edificação == "Pintura/Textura" | SIMIL$Acabamento.Fachada.Principal.da.Edificação == "Pastilha" | 
                SIMIL$Acabamento.Fachada.Principal.da.Edificação == "Cerâmica de Boa Qualidade" | SIMIL$Acabamento.Fachada.Principal.da.Edificação == "Granito ou Compatível"
                | SIMIL$Acabamento.Fachada.Principal.da.Edificação =="Porcelanato")
SIMIL <- filter(SIMIL, SIMIL$Esquadrias.Fachada.Principal.da.Edificação == "Alumínio" | SIMIL$Esquadrias.Fachada.Principal.da.Edificação =="Ferro"
                | SIMIL$Esquadrias.Fachada.Principal.da.Edificação == "Madeira" | SIMIL$Esquadrias.Fachada.Principal.da.Edificação =="Vidro Temperado")
SIMIL <- filter(SIMIL, SIMIL$Padrão.de.acabamento.da.Unidade == "Normal (forte predominância)" | SIMIL$Padrão.de.acabamento.da.Unidade =="Normal (c/ aspectos de baixo)" |
                SIMIL$Padrão.de.acabamento.da.Unidade=="Normal (c/ aspectos de alto)" | SIMIL$Padrão.de.acabamento.da.Unidade =="Regular (reparos simples)" |
                SIMIL$Padrão.de.acabamento.da.Unidade=="Alto (superior, luxo)")
SIMIL <- filter(SIMIL, SIMIL$Estado.de.Conservação.da.Unidade == "Bom (aparência de novo)" | SIMIL$Estado.de.Conservação.da.Unidade =="Bom (aparência de usado)" |
                  SIMIL$Estado.de.Conservação.da.Unidade=="Regular (reparos importantes)" | SIMIL$Estado.de.Conservação.da.Unidade =="Regular (reparos simples)")
SIMIL <- filter(SIMIL, SIMIL$Idade.Estimada.da.edificação.como.um.todo == "> 5 <= 10" | SIMIL$Idade.Estimada.da.edificação.como.um.todo =="> 10 <= 20" |
                  SIMIL$Idade.Estimada.da.edificação.como.um.todo =="<= 5" | SIMIL$Idade.Estimada.da.edificação.como.um.todo =="> 20 <= 50"| 
                  SIMIL$Idade.Estimada.da.edificação.como.um.todo =="> 50 <= 100")
SIMIL <- filter(SIMIL, SIMIL$Nº.de.Unidades.no.Andar.da.UP >=1 & SIMIL$Nº.de.Unidades.no.Andar.da.UP <=12)
SIMIL <- filter(SIMIL, SIMIL$Nº.de.pavimentos.do.Prédio..UP. >=2 & SIMIL$Nº.de.pavimentos.do.Prédio..UP. <=30)
SIMIL <- filter(SIMIL, SIMIL$Estado.de.Conservação.do.prédio..UP. == "Bom (aparência de novo)" | SIMIL$Estado.de.Conservação.do.prédio..UP. == "Bom (aparência de usado)" |
                  SIMIL$Estado.de.Conservação.do.prédio..UP. == "Regular (reparos importantes)" | SIMIL$Estado.de.Conservação.do.prédio..UP. == "Regular (reparos simples)")


SIMIL$Data.de.Refência.da.Avaliação<-as.numeric(SIMIL$Data.de.Refência.da.Avaliação)

#RETIRANDO OBSERVAÇÕES QUE CONTENHAM N/A
SIMIL <- na.omit(SIMIL)

#separar longitude e latitude em variáveis diferentes
SIMIL <- SIMIL %>% separate(Coordenadas.Grau.Decimal, c("latitude", "longitude"), sep = ",")

#EXPORTAR
write.csv(SIMIL, "SIMIL.csv") #exporta os dados para csv

#IMPORTAR
SIMIL_RENDA <- read_excel("resultado interseção QGIS.xlsx")

#CRIANDO ID COMPOSTO POR ORDENADOR E ANO PARA GERAR UM ID UNICO
SIMIL_RENDA$ID <- paste(SIMIL_RENDA$Ordenador, SIMIL_RENDA$Ano, sep = "")
SIMIL$ID <- paste(SIMIL$Ordenador, SIMIL$Ano, sep = "")

#CRIANDO DF COM ID E RENDA
RENDA <- SIMIL_RENDA %>% dplyr::select(ID, MEDIA, GEOCODIGO)

#CRUZANDO DADOS DA PLANILHA DE RENDA COM A PLANILHA DO SIMIL ATRAVÉS DO PIVÔ "ID"
SIMIL <- left_join(RENDA,SIMIL, by = "ID")
SIMIL <- unique(SIMIL)

#Ajustando nome e posição das variáveis e retirando observaçÕes com renda zero pois denotam erro
SIMIL <- rename(SIMIL, Renda = MEDIA, Setor_censitario = GEOCODIGO)
SIMIL <- relocate(SIMIL, Renda, .after = longitude)
SIMIL$Renda<- as.numeric(SIMIL$Renda)
SIMIL$Setor_censitario<- as.character(SIMIL$Setor_censitario)
SIMIL <- filter(SIMIL, SIMIL$Renda>0) 

# ESTUDO DAS CORRELAÇÕES
chart.Correlation((SIMIL[14:20]), histogram = TRUE)

#Transformando as variáveis categóricas em variáveis Dummy
SIMIL_DUMMIES <- fastDummies::dummy_columns(.data = SIMIL, select_columns = c("Padrão.das.Edificações.na.Região", 
                                      "Estado.de.Conservação.do.prédio..UP.", "Idade.Estimada.da.edificação.como.um.todo",
                                      "Equipamentos.de.Segurança", "Espaço.com.Churrasqueira", "Mini.Quadra.Esportiva",
                                      "Piscina", "Portaria.e.ou.Guarita", "Salão.de.Festas", "Outros.Salões.de.Lazer",
                                      "Sauna...Ofurô...Hidromassagem", "Quadra.Poliesportiva", "Pilotis", 
                                      "Acabamento.Fachada.Principal.da.Edificação", "Esquadrias.Fachada.Principal.da.Edificação",
                                      "Padrão.de.acabamento.da.Unidade", "Estado.de.Conservação.da.Unidade"),
                                      remove_most_frequent_dummy = TRUE, ignore_na = FALSE, remove_selected_columns = TRUE)

#Retirando variáveis nominais
Dados_Saneados <- SIMIL_DUMMIES %>% dplyr::select(- Ano, - Ordenador, - CEP, - Bairro, - Logradouro, - Nº, - Bloco, - Nº.da.Unidade, - Município,  
                                 - latitude, - longitude, - Setor_censitario, - SIMIL_DUMMIES$MEDIA.x, - SIMIL_DUMMIES$GEOCODIGO.x, 
                                 - SIMIL_DUMMIES$MEDIA.y, - SIMIL_DUMMIES$GEOCODIGO.y)



# ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA

#Modelagem com todas as variáveis
modelo_regressao <- lm(formula = R..m2 ~ . - ID - Valor.de.Avaliação..R.., Dados_Saneados)

# PROCEDIMENTO STEPWISE
step_regressao <- step(modelo_regressao, k = 3.841459)

# TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE
#Teste de Shapiro-Francia (o teste não foi possivel pois a amostra possui mais de 
#5 mil obsercações)
#sf.test(step_regressao$residuals) #função 'sf.test' do pacote 'nortest'
hist(step_regressao$residuals) #opção para verificação visual da normalidade dos resíduos

#Plotando os resíduos do modelo step_regressao 
Dados_Saneados %>%
  mutate(residuos = step_regressao$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
Dados_Saneados %>%
  mutate(residuos = step_regressao$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_regressao$residuals),
                            sd = sd(step_regressao$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE) - forma não-paramétrica para estimar a
#função densidade de probabilidade de uma variável aleatória
SIMIL_DUMMIES %>%
  ggplot() +
  geom_density(aes(x = step_regressao$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()


# DIAGNÓSTICO DE HETEROCEDASTICIDADE
#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(step_regressao)
#função 'ols_test_breusch_pagan' do pacote 'olsrr'
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)
#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#Adicionando fitted values e resíduos do modelo 
Dados_Saneados$fitted_step <- step_regressao$fitted.values
Dados_Saneados$residuos_step <- step_regressao$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
Dados_Saneados %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

#R2, RMSE e MAE
caret::postResample(Dados_Saneados$fitted_step, Dados_Saneados$R..m2)


##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
Dados_Saneados$bcRm2 <- (((Dados_Saneados$R..m2 ^ lambda_BC$lambda) - 1) / 
                                   lambda_BC$lambda)


#Estimando um novo modelo múltiplo com dummies
modelo_bc_SIMIL <- lm(formula = bcRm2 ~ . -ID -R..m2 -fitted_step
                           -residuos_step - Valor.de.Avaliação..R.., 
                           data = Dados_Saneados)

#Aplicando o procedimento Stepwise
step_bc_SIMIL <- step(modelo_bc_SIMIL, k = 3.841459)

summary(step_bc_SIMIL)

#Teste de Shapiro-Francia (não dá certo pois a amostra > 3000)
#sf.test(step_bc_SIMIL$residuals) #função 'sf.test' do pacote 'nortest'
hist(step_bc_SIMIL$residuals) #apenas para verificação visual da distribuição dos resíduos


#Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teórica
Dados_Saneados %>%
  mutate(residuos = step_bc_SIMIL$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_SIMIL$residuals),
                            sd = sd(step_bc_SIMIL$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE)
Dados_Saneados %>%
  ggplot() +
  geom_density(aes(x = step_bc_SIMIL$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_SIMIL)

#Adicionando fitted values e resíduos do modelo 
Dados_Saneados$fitted_step_novo <- step_bc_SIMIL$fitted.values
Dados_Saneados$residuos_step_novo <- step_bc_SIMIL$residuals

#Transformaação inversa do Box-Cox para os fitted values
Dados_Saneados$fitted_transformado <- (Dados_Saneados$fitted_step_novo * -0.7199462+1)^(1/-0.7199462)

#R2, RMSE e MAE
caret::postResample(Dados_Saneados$fitted_step_novo, Dados_Saneados$bcRm2)
caret::postResample(Dados_Saneados$fitted_transformado, Dados_Saneados$R..m2 )

#Gráfico que relaciona resíduos e fitted values do modelo 'step_bc_planosaude'
Dados_Saneados %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()


######CALCULANDO R2, RMSE E MAE DA REGRESSÃO SISDEA
#IMPORTAR
SISDEA <- read_excel("Y e fitted values SISDEA.xlsx")
SISDEA <- na.omit(SISDEA)
SISDEA <- filter(SISDEA, SISDEA$Estimado<=15000)
caret::postResample(SISDEA$Observado, SISDEA$Estimado)
SISDEA$Observado <- as.numeric(SISDEA$Observado)
SISDEA$Estimado <- as.numeric(SISDEA$Estimado)
summary(SISDEA)

mean(Dados_Saneados$R..m2)
