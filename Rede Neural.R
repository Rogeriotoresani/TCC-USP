#REDES NEURAIS E DEEP LEARNING

#LENDO A BASE
SIMIL <- read.csv2("SIMIL.CSV", header=TRUE)

#RETIRANDO VARIÁVEIS UTILIZADAS SOMENTE NA ANÁLISE DA REGRESSÃO
SIMIL <- SIMIL[,-c(1,50,49,48,47,46,45)]

# colunas quantitativas para padronizar entre 0 e 1
cols <- c("Renda", 'Área.considerada.para.avaliação..m2.', 'Data.de.Refência.da.Avaliação', 
          'Nº.de.Unidades.no.Andar.da.UP', 'Nº.de.pavimentos.do.Prédio..UP.', 'R..m2',
          'Total.Geral.de.Vagas', 'Quarto.s...dormitórios.')

# função para padronizar
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# padronizar variaveis quantitativas
SIMIL[cols] <- lapply(SIMIL[cols], range01)

#CRIANDO BASE DE TREINO E DE TESTE
set.seed(123)
bool_treino <- stats::runif(dim(SIMIL)[1])>.25

treino <- SIMIL[bool_treino,]
teste  <- SIMIL[!bool_treino,]

#Rede Neural simples
rn1 <- neuralnet(R..m2 ~ . -ID - Valor.de.Avaliação..R.. ,
                data = treino,
                linear.output = TRUE,
                threshold = 0.01)

treino1 <- predict(rn1,treino)
teste1 <- predict(rn1,teste)

caret::postResample(teste1, teste$R..m2)
caret::postResample(treino1, treino$R..m2)

#Rede Neural com camada uma camada oculta com 2 neurônios
rn2 <- neuralnet(R..m2 ~ . -ID - Valor.de.Avaliação..R.. ,
                data = treino,
                hidden = c(3,4),
                linear.output = TRUE,
#                stepmax = 1e7,
                threshold = 0.01)
treino2 <- predict(rn2,treino)
teste2 <- predict(rn2,teste)
caret::postResample(teste2, teste$R..m2)
caret::postResample(treino2, treino$R..m2)

#Rede Neural com camada uma camada oculta com 15 neurônios
rn3 <- neuralnet(R..m2 ~ . -ID - Valor.de.Avaliação..R.. ,
                 data = treino,
                 hidden = c(15),
                 linear.output = TRUE,
#                 stepmax = 1e7,
                 threshold = 0.01
                  )

treino3 <- predict(rn3,treino)
teste3 <- predict(rn3,teste)
caret::postResample(teste3, teste$R..m2)
caret::postResample(treino3, treino$R..m2)

#Rede Neural com camada duas camada oculta com 7 e 3 neurônios
rn5 <- neuralnet(R..m2 ~ . -ID - Valor.de.Avaliação..R.. ,
                 data = treino,
                 hidden = c(7,3),
                 linear.output = TRUE,
                 #                 stepmax = 1e7,
                 threshold = 0.1
)
treino5 <- predict(rn5,treino)
teste5 <- predict(rn5,teste)

caret::postResample(teste5, teste$R..m2)
caret::postResample(treino5, treino$R..m2)

#Rede Neural com camada três camadas ocultas com 7, 12 e 15 neurônios
rn7 <- neuralnet(R..m2 ~ . -ID - Valor.de.Avaliação..R.. ,
                 data = treino,
                 hidden = c(7, 12, 15),
                 linear.output = TRUE,
                 #                 stepmax = 1e7,
                 threshold = 0.1
)

treino7 <- predict(rn7,treino)
teste7 <- predict(rn7,teste)
caret::postResample(teste7, teste$R..m2)
caret::postResample(treino7, treino$R..m2)



# Tunando a RNA

# CV Grid Search para 'tunar' o parâmetro decay
nnGrid <-  expand.grid(.size=7, .decay = c(0, .005, .01, 0.015, 0.02, 0.025))

ctrl <- caret::trainControl(method='cv')

?train

nnfit <- caret::train(R..m2 ~ . - Valor.de.Avaliação..R..,
                      data=treino,
                      method='nnet',
                      tuneGrid=nnGrid,
                      trControl=ctrl,
                      maxit=1000,
                      verboseIter = FALSE
)
modelo.final <- nnfit$finalModel
pred <- predict(modelo.final, treino)
Predteste <- predict(modelo.final, teste)

caret::postResample(pred, treino$R..m2)
caret::postResample(Predteste, teste$R..m2)

plot(modelo.final)





nnGrid3<-  expand.grid(.size=c(15), .decay = c(0, .005, .01, 0.015, 0.02, 0.025))
nnfit3 <- caret::train(R..m2 ~ . - Valor.de.Avaliação..R..,
                       data=treino,
                       method='nnet',
                       tuneGrid=nnGrid3,
                       trControl=ctrl,
                       maxit=1000,
                       verboseIter = FALSE
)

modelo.final3 <- nnfit3$finalModel

pred3 <- predict(modelo.final3, treino)
Predteste3 <- predict(modelo.final3, teste)

caret::postResample(pred3, treino$R..m2)
caret::postResample(Predteste3, teste$R..m2)

