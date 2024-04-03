#ARVORE DE REGRESSÃO E RANDON FOREST


#LENDO A BASE
SIMIL <- read_excel("SIMIL.xlsx")
SIMIL <- read.csv2("SIMIL.CSV", header=TRUE)

#RETIRANDO VARIÁVEIS UTILIZADAS SOMENTE NA ANÁLISE DA REGRESSÃO
SIMIL <- SIMIL[,-c(49,48,47,46,45)]

#CRIANDO BASE DE TREINO E DE TESTE
set.seed(123)
bool_treino <- stats::runif(dim(SIMIL)[1])>.25

treino <- SIMIL[bool_treino,]
teste  <- SIMIL[!bool_treino,]


#MODELANDO ARVORE
set.seed(123)
arvore <- rpart::rpart(R..m2 ~ . - ID -Valor.de.Avaliação..R..,
                       data=treino,
                       xval=1,
                       control = rpart.control(cp = 0, 
                                               maxdepth = 10)
)

parvore_treino <- predict(arvore, treino)
parvore_teste <- predict(arvore, teste)


caret::postResample(parvore_treino, treino$R..m2)
caret::postResample(parvore_teste, teste$R..m2)


# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores


# Rodar uma árvore bem grande
treino_combinado <- treino

set.seed(123)
arvore_grande <- rpart::rpart(R..m2 ~. - ID -Valor.de.Avaliação..R.., 
                              data=treino_combinado,
                              control=rpart.control(maxdepth = 30, 
                                                    cp=0,
                                                    xval=10))

pgrande_treino <- predict(arvore_grande, treino_combinado)
pgrande_teste <- predict(arvore_grande, teste)


caret::postResample(pgrande_treino, treino$R..m2)
caret::postResample(pgrande_teste, teste$R..m2)


# Esse comando nos dá todos os possíveis custos de complexidade
#   e os respectivos erros em validação cruzada (usando k-fold)
tab_cp <- rpart::printcp(arvore_grande)
# Aqui uma visualização gráfica do CP vs erro em validação cruzada
rpart::plotcp(arvore_grande)

# Com esse comando vamos pegar o melhor CP na validação cruzada
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

# E rodar a melhor árvore
set.seed(123)
arvore_tunada <- rpart::rpart(R..m2 ~. - ID -Valor.de.Avaliação..R.., 
                              data=treino_combinado,
                              control=rpart.control(maxdepth = 30, 
                                                    cp=cp_min,
                                                    xval=0))

# A árvore foi tunada usando o k-fold (abrindo mão o mínimo de dados)
# Agora vamos avaliá-la na base de testes (uma base isenta)
pTunada_treino <- predict(arvore_tunada, treino_combinado)
pTunada_teste <- predict(arvore_tunada, teste)

caret::postResample(pTunada_treino, treino$R..m2)
caret::postResample(pTunada_teste, teste$R..m2)

# Vamos agora avaliar um Random Forest para comparar
set.seed(123)
rf <- randomForest::randomForest(
  R..m2 ~. - ID -Valor.de.Avaliação..R..,
  data = treino,
  ntree = 10
)

# Vamos avaliar esse Random Forest
pRF_treino <- predict(rf, treino_combinado)
pRF_teste  <- predict(rf, teste)
caret::postResample(pRF_treino, treino$R..m2)
caret::postResample(pRF_teste, teste$R..m2)

#####################################
#  Tunando o random-forest      #
#####################################

# Definir os hiperparâmetros para o grid search
# Criar o grid de hiperparâmetros
hyperparameters <- expand.grid(mtry = c(3, 4, 5, 6, 7, 8))

# Definir a função de controle para validação cruzada
ctrl <- trainControl(method = "cv", # CV indica "k-fold cross validation"
                     number = 5)  # 5 é o número de "folds"

# Realizar o grid search com validação cruzada
set.seed(123)
gridsearch_kfold <- train(R..m2 ~. - ID -Valor.de.Avaliação..R.., 
                          data = treino_combinado, 
                          method = "rf", 
                          trControl = ctrl, 
                          tuneGrid = hyperparameters)

gridsearch_kfold$finalModel
print(gridsearch_kfold)
plot(gridsearch_kfold)

p_rftunada <- predict(gridsearch_kfold, teste)
p_rftunada_treino <- predict(gridsearch_kfold, treino)

caret::postResample(p_rftunada, teste$R..m2)
caret::postResample(p_rftunada_treino, treino$R..m2)



#####################################
# Tunando o random-forest      #
#####################################

# Definir os hiperparâmetros para o grid search
# Criar o grid de hiperparâmetros
hyperparameters2 <- expand.grid(mtry = c(15, 20, 25, 30, 35, 40))


# Definir a função de controle para validação cruzada
ctrl2 <- trainControl(method = "cv", # CV indica "k-fold cross validation"
                     number = 15)  # 5 é o número de "folds"

# Realizar o grid search com validação cruzada
set.seed(123)
gridsearch_kfold2 <- train(R..m2 ~. - ID -Valor.de.Avaliação..R.., 
                          data = treino_combinado, 
                          method = "rf", 
                          trControl = ctrl2, 
                          tuneGrid = hyperparameters2)

gridsearch_kfold2$finalModel

p_rftunada2 <- predict(gridsearch_kfold2, teste)
p_rftunada_treino2 <- predict(gridsearch_kfold2, treino)

caret::postResample(p_rftunada2, teste$R..m2)
caret::postResample(p_rftunada_treino2, treino$R..m2)



