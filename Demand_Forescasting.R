# O projeto consiste na previsão de demanda de consumo de produtos de padaria de várias lojas 
# do grupo bimbo. Os dados estão presentes na plataforma: 
# https://www.kaggle.com/c/grupo-bimbo-inventory-demand

# Carregando pacotes
library(data.table)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(caret)

# Semana — número da semana (From Thursday to Wednesday) (de quinta-feira para quarta-feira)
# Agencia_ID — ID do depósito de vendas
# Canal_ID — ID do canal de vendas
# Ruta_SAK — ID da rota (Several routes = Sales Depot) (varias rotas = depósito de vendas)
# Cliente_ID — ID do cliente (podem existir mais de um cliente com o mesmo ID, pois possuem 
# nomes similares)
# NombreCliente — Client name
# Producto_ID — Product ID
# NombreProducto — Product Name
# Venta_uni_hoy — Sales unit this week (integer)
# Venta_hoy — Sales this week (unit: pesos)
# Dev_uni_proxima — Returns unit next week (integer)
# Dev_proxima — Returns next week (unit: pesos) (produtos que voltaram e não foram vendidos)
# Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)
# Demanda_uni_equil = Venta_uni_hoy - Dev_uni_proxima

################### Criando amostra ##################################
##### Código utilizado uma única vez #################################
######################################################################

# Carregando o conjunto de dados com fread devido a quantidade de 
SaleData <- fread("train.csv", sep = ",")

# Número de linhas do dataset
nrow(SaleData)

# Vamos retirar uma amostra sem reposição
set.seed(1)
amostra <- sample(1:nrow(SaleData), 100000, replace = FALSE)

# Novo dataframe
SampleSale <- SaleData[amostra,]

# Salvando os dados de SampleSale
write.csv(SampleSale, "BimboData.csv")
#######################################################################

# Carregando
SampleSale <- read_csv("BimboData.csv")
SampleSale <- SampleSale[,-1]
View(SampleSale)

# Visualização do dataframe
head(SampleSale)

# Tipos de dados
str(SampleSale)

# Verificando se há valores NA
sapply(SampleSale, function(x) sum(is.na(x)))

# As variáveis Venta_uni_hoy, Venta_hoy, Dev_uni_proxima, Dev_proxima, Demanda_uni_equil
# são numéricas e as outras categóricas
# Vamos avaliar as variáveis neméricas
NumVar <- SampleSale[,c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima", "Dev_proxima", 
                        "Demanda_uni_equil")]

# Verificando a relação entre as variáveis
# É possível ver as que existem outliers e as variáveis são correlacionados
ggplot(data = NumVar) +
  geom_point(mapping = aes(x = Venta_hoy, y = Demanda_uni_equil)) +
  ggtitle("Scatter plot Demanda hoje por vendas (R$) hoje")

# Relacionamento altamente linear
ggplot(data = NumVar) +
  geom_point(mapping = aes(x = Venta_uni_hoy, y = Demanda_uni_equil)) +
  ggtitle("Scatter plot Demanda hoje por vendas (unidades) hoje")

# As variáveis não tem uma relação linear, pois não podem existir valores negativos de 
# Demanda_uni_equil
ggplot(data = NumVar) +
  geom_point(mapping = aes(x = Dev_uni_proxima, y = Demanda_uni_equil)) +
  ggtitle("Scatter plot Demanda hoje por retornos (unidades) na próxima semana")

# Nenhuma relação linear
ggplot(data = NumVar) +
  geom_point(mapping = aes(x = Dev_proxima, y = Demanda_uni_equil)) +
  ggtitle("Scatter plot Demanda hoje por retornos (R$) na próxima semana")

# Avaliando a relação entre variáveis numéricas
M <- cor(NumVar)
corrplot(M, method = "circle")

# Gráfico de boxplot para verificar a dispersão dos dados
ggplot(data = NumVar) +
  geom_boxplot(mapping = aes(y = Demanda_uni_equil))

# Verificando variáveis numéricas
CatVar <- SampleSale[,-c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima", "Dev_proxima")]

# Visualizando os dados
head(CatVar)

# Transformando as variáveis categóricas em factor
Cat <- data.frame(lapply(CatVar[,-c("Demanda_uni_equil")], factor))
str(Cat)

df.Demand.Forecasting <- cbind(Cat, CatVar[,"Demanda_uni_equil"])

head(df.Demand.Forecasting)

str(df.Demand.Forecasting)

# Demanda_uni_equil pela semana
ggplot(data = df.Demand.Forecasting) +
  geom_bar(mapping = aes(x = Semana, weight = Demanda_uni_equil, fill = Semana))

# Quantidade de cada semana
ggplot(data = df.Demand.Forecasting) +
  geom_bar(mapping = aes(x = Semana, fill = Semana))

# Demanda_uni_equil pelo Canal_ID
ggplot(data = df.Demand.Forecasting) +
  geom_bar(mapping = aes(x = Canal_ID, weight = Demanda_uni_equil, fill = Canal_ID))

# Quantidade de Canal_ID
ggplot(data = df.Demand.Forecasting) +
  geom_bar(mapping = aes(x = Canal_ID, fill = Canal_ID))

# Os cinco produtos com maiores demandas
Prod_5_mais <- SampleSale %>%
  group_by(Producto_ID) %>%
  summarise(Quant_vend_prod = sum(Demanda_uni_equil)) %>%
  arrange(desc(Quant_vend_prod))

head(Prod_5_mais)

# Gráfico dos 5 maiores produtos demandados por semana
df.Demand.Forecasting %>%
  filter(Producto_ID %in% c(2425,1284,1278,43285,1240,36610)) %>%
  group_by(Semana, Producto_ID) %>%
  arrange(Semana, Producto_ID) %>%
  summarise(Demanda_semana = sum(Demanda_uni_equil)) %>%
  ggplot(mapping = aes(x = Semana, y = Demanda_semana, group = Producto_ID, 
                       colour = Producto_ID)) +
  geom_point() +
  geom_line() + theme_minimal()

# Os cinco maiores demandas por Clientes
Cliente_5_mais <- SampleSale %>%
  group_by(Cliente_ID) %>%
  summarise(Quant_vend_cliente = sum(Demanda_uni_equil)) %>%
  arrange(desc(Quant_vend_cliente))

head(Cliente_5_mais)

# Os 5 maiores clientes em demandas
df_cliente_5_mais <- df.Demand.Forecasting %>%
  arrange(Semana, Cliente_ID) %>%
  group_by(Semana, Cliente_ID) %>%
  summarise(Demanda_semana = sum(Demanda_uni_equil))

# Quantidade de semanas de cliente (só um cliente teve demanda todos os dias da semana)
df_cliente_5_mais %>%
  group_by(Cliente_ID) %>%
  summarise(quant_cliente_semana = n()) %>%
  arrange(desc(quant_cliente_semana)) %>%
  head()

# Gráfico dos 5 maiores demandas de clientes por semana 
# (semanas sem demanda representam demanda zero, ou seja, não compra nem devolução de produto)
df.Demand.Forecasting %>%
  filter(Cliente_ID %in% c(653378, 827594, 1973961, 2502084, 2418007, 4419474)) %>%
  group_by(Semana, Cliente_ID) %>%
  arrange(Semana, Cliente_ID) %>%
  summarise(Demanda_semana = sum(Demanda_uni_equil)) %>%
  ggplot(mapping = aes(x = Semana, y = Demanda_semana, group = Cliente_ID, 
                       colour = Cliente_ID)) +
  geom_point() +
  geom_line() + theme_minimal()

# Os cinco produtos com maiores demandas
Agencia_5_mais <- SampleSale %>%
  group_by(Agencia_ID) %>%
  summarise(Quant_vend_prod = sum(Demanda_uni_equil)) %>%
  arrange(desc(Quant_vend_prod))

head(Agencia_5_mais)

# Gráfico dos 5 maiores demandas para a agência por semana
df.Demand.Forecasting %>%
  filter(Agencia_ID %in% c(1129,1911,1279,1347,1912,1312)) %>%
  group_by(Semana, Agencia_ID) %>%
  arrange(Semana, Agencia_ID) %>%
  summarise(Demanda_semana = sum(Demanda_uni_equil)) %>%
  ggplot(mapping = aes(x = Semana, y = Demanda_semana, group = Agencia_ID, 
                       colour = Agencia_ID)) +
  geom_point() +
  geom_line() + theme_minimal()

# Engenharia de atributos - substituindo categorias com alta cardinalidade pela frequência de
# cada categoria no data frame. Variáveis a serem encodadas: Agencia_ID, Ruta_SAK, Cliente_ID,
# Producto_ID.
df.Demand.Forecasting2 <- df.Demand.Forecasting %>%
  add_count(Producto_ID, name = "Freq_produto") %>%
  add_count(Agencia_ID, name = "Freq_Agencia") %>%
  add_count(Ruta_SAK, name = "Freq_ruta") %>%
  add_count(Cliente_ID, name = "Freq_cliente")

df.Demand.Forecasting2 <- df.Demand.Forecasting2[,-c(2, 4, 5, 6)]

head(df.Demand.Forecasting2)
View(df.Demand.Forecasting2)

# Descrição estatística das novas variáveis
summary(df.Demand.Forecasting2)

# Removendo outliers
df.Demand.Forecasting3 <- df.Demand.Forecasting2 %>%
  filter(Demanda_uni_equil < 200)

nrow(df.Demand.Forecasting3)

summary(df.Demand.Forecasting3)

head(df.Demand.Forecasting3)

dfM <- melt(df.Demand.Forecasting3)
ggplot(dfM, mapping = aes(x=value)) +
  geom_histogram(bins = 15) +
  facet_wrap(~variable)

# Normalizando o data set (mix-max scalling)
preproc <- preProcess(df.Demand.Forecasting3[,c(1:2,4:7)], method = c("range"))
DataNorm <- predict(preproc, df.Demand.Forecasting3[,c(1:2,4:7)])

summary(DataNorm)

View(DataNorm)

df.Demand.Norm <- cbind(DataNorm, df.Demand.Forecasting3[, 3])
head(df.Demand.Norm)

# Como existem muitos valore muito próximos a zero, então diminui eles de 1
df.Demand.Norm$Freq_cliente <- 1 - df.Demand.Norm$Freq_cliente

# Criando Modelo
set.seed(1)
amostra2 <- sample(1:nrow(df.Demand.Norm), 50000, replace = FALSE)

df_demanda <- df.Demand.Norm[amostra2,]

##################################################################################
######################## Cross Validation ########################################
##################################################################################
set.seed(1)
linhas <- sample(1:nrow(df_demanda), 0.7*nrow(df_demanda), 
                 replace = FALSE)

train_data <- df_demanda[linhas,]
test_data <- df_demanda[-linhas,]
head(train_data)
head(test_data)

# Modelo support vector machine
library(e1071)

model <- svm(data = train_data, Demanda_uni_equil ~ .)
summary(model)
pred <- fitted(model)
Comp <- cbind(train_data$Demanda_uni_equil, pred)
View(Comp)
RMSLE <- sqrt(1/length(train_data$Demanda_uni_equil)*
                 sum((log(pred + 1) - 
                        log(train_data$Demanda_uni_equil + 1))^2))
RMSLE

# Modelo Regressão Linear
model2 <- lm(data = train_data, Demanda_uni_equil ~ .)
summary(model2)
predli2 <- fitted(model2)
RMSLE2 <- sqrt(1/length(train_data$Demanda_uni_equil)*
                sum((log(predli2 + 1) - 
                       log(train_data$Demanda_uni_equil + 1))^2))
RMSLE2

# Modelo árvore de decisão
library(rpart)
library(rpart.plot)

model3 <- rpart(Demanda_uni_equil ~ ., data = train_data, method = "anova")
rpart.plot(model3)
plotcp(model3)
predtree <- predict(model3, type = "vector")

RMSLE3 <- sqrt(1/length(train_data$Demanda_uni_equil)*
                 sum((log(predtree + 1) - 
                        log(train_data$Demanda_uni_equil + 1))^2))
RMSLE3

# Modelo random forest
library(randomForest)

model4 <- randomForest(data = train_data, Demanda_uni_equil ~ .)
summary(model4)
pred4 <- predict(model4)
plot(model4)

RMSLE4 <- sqrt(1/length(train_data$Demanda_uni_equil)*
                sum((log(pred4 + 1) - 
                       log(train_data$Demanda_uni_equil + 1))^2))
RMSLE4

# Modelo de redes neurais (não convergiu no meu PC)
library(neuralnet)

train_data_net <- train_data
train_data_net$Semana <- as.integer(train_data_net$Semana)
train_data_net$Canal_ID <- as.integer(train_data_net$Canal_ID)
model5 <- neuralnet(data = train_data_net, Demanda_uni_equil ~ ., 
                    threshold = 0.01, hidden = c(2,5), linear.output = F)
plot(model5, rep = 'best')
pred <- predict(model5, train_data_net)
View(pred)
RMSLE5 <- sqrt(1/length(train_data_net$Demanda_uni_equil)*
                 sum((log(pred + 1) - 
                        log(train_data_net$Demanda_uni_equil + 1))^2))
RMSLE5










