# Título:
# Interações biológicas em redes - Análise em R
# Autores:
#   Daniel A. Carvalho
#   Carolina V. C. Silva
#   Gabriel G. Barbosa

## Proposta de atividade
#  observação na natureza e uso de ferramentas de
#  análises
#
#  Desenhando a rede e testando a estrutura

## Roteiro

### 1. Configuração do R Studio

# Primeiro, altere o seu diretório para a pasta
# com os dados coletados em campo ou a matriz
# disponibilizada (Small, 1976)
#
# clicking "Session" > "Set
# working directory" > "Choose directory"

### 2. Pacotes R necessários

# Nós vamos utilizar os seguintes pacotes:
# `bipartite`, `iNEXT`, `vegan` e `ggplot2`

#### Instalação dos pacotes

install.packages("bipartite")
install.packages("iNEXT")
install.packages("vegan")
install.packages("ggplot2")

#### Utilização dos pacotes

library(bipartite)
library(iNEXT)
library(vegan)
library(ggplot2)

### 3. Matriz de interações

# Vamos incluir a matriz de interações utilizando
# a seguinte linha:

# Incluindo a matriz
small <-read.table("small.txt", header=TRUE)

### 4. Explorando a rede

# Note que nas linhas estão as espécies de plantas
# e nas colunas os visitantes florais

# linhas
rownames(small)
# colunas
colnames(small)

### 5. Visualizando

# A linha abaixo exibe os diversos parâmetros que
# podem ser utilizados com a função `plotweb()`

help(plotweb)

# Você pode personalizar várias coisas. Por
# exemplo, a linha abaixo visualiza a rede pronta:

# visualização da rede
plotweb(small, text.rot=90, col.low="darkgreen",
        col.high="red", bor.col.interaction=NA,
        col.interaction="grey70")

### 6. Cálculo de índices e métricas

# Agora vamos calcular alguns índices/métricas de
# redes. Para métricas a nível de rede, podemos
# usar a função `networklevel()`

# Exibir os diversos parâmetros que podem ser
# utilizados com a função `networklevel`

help(networklevel)

# Utilizaremos o parâmetro `small`
networklevel(small)

# Podemos avaliar cada métrica individualmente de
# acordo com nossos interesses:

#### 6.1 Conectância:

# A conectância indica uma média geral do nível de
# generalização da rede, ou seja uma razão entre o
# número observado de interações e o número
# observado de interações potenciais.

# Se uma espécie de beija-flor interage com todas
# as plantas, essa interação teria o valor máximo
# de conectância

networklevel(small, index="connectance")

#### 6.2 H2' (especialização)

# H2' é a métrica que representa a especialização
# da rede essa métrica indica o quão especialista
# são as espécies da rede (interagem sempre com a
# mesma espécie)

# Podemos calcular mais deum índice usando o
# argumento "c" - vamos calcular conectância e H2'

networklevel(small, index=c("connectance", "H2"))

#### 6.3 Aninhamento/Nestedness

# Esse índice varia de 0 a 100 e indica o quão
# aninhada a sua rede é.

# Geralmente é utilizado o valor 0.5 para definir
# se a rede é muito ou pouca aninhada

networklevel(small, index="weighted NODF")

# Nesse exemplo não podemos dizer que a rede é
# aninhada por ter baixo valor de aninhamento

#### 6.4 Modularidade

# Já quanto a modularidade, podemos dizer que a
# nossa rede tem a estrutura modular pelo valor
# observado ser  > 0.5

mod.small<-computeModules(small, method="Beckett", steps=10E7)
mod.small<-metaComputeModules(small, N=5, method="Beckett",steps = 10E7)
mod.small@likelihood

### 7. Modelos nulos

# Utilizamos os modelos nulos para verificar a
# significância do valor encontrado de cada
# métrica, uma vez que as essas métricas não são
# independentes de propriedades intrínsecas da
# rede, como por exemplo o tamanho: quanto maior
# ou menor a sua rede, pode afetar ao acaso as
# métricas calculadas

# O valor de p = 0.05 é utilizado para verificar
# se as métricas são originadas por algum fator
# ecológico ou meramente ao acaso, por isso
# utilizamos redes geradas ao acaso e comparamos
# os valores das métricas com os valores
# observados:

# Exibe os paramêtros que são aceitos pela função
# `nullmodel()`

help(nullmodel)

# Calcule de novo o valor de H2' para a rede:

H2.small <-networklevel(small, index="H2")

# Vamos gerar 100 redes aleatórias:

small.random<- nullmodel(small,N=100,method="r2dtable")

# Calcule o H2' para cada uma das matrizes:

H2.ram.small <- unlist(sapply(small.random, networklevel, index="H2"))

# Gere o valor de p:

p.value <- sum(H2.small<H2.ram.small)/100
p.value

# Como o valor de p foi menor que 0.05, podemos
# dizer que a rede é especializada ou seja, temos
# mais interações especializadas entre os
# beija-flor e as plantas, do que interações
# generalistas

#### 7.1 Conectância com modelos nulos

# Agora vamos calcular a conectância com modelos
# nulos

conec.small <-networklevel(small, index="connectance")

conec.small.random<- nullmodel(small,N=100,method="r2dtable")

conec.ram.small <- unlist(sapply(conec.small.random, networklevel, index="connectance"))

p.value <- sum(conec.small<conec.ram.small)/100
p.value

# Note que o valor de p é 1, ou seja, a rede não
# possui conectância significativa

# Para aninhamento (NODF), nós podemos usar um
# modelo nulo próprio para a matriz:

oecosimu(small, nestednodf,method="r1",nsimul=100)

# Note na tabela de resultados que o valor de P é
# maior que 0.05, ou seja a rede não é aninhada

#### 7.1 Modularidade com modelos nulos

# Para modularidade:

mod.small@likelihood

nulls <- nullmodel(small, N=100, method="r2d")
modules.nulls <- sapply(nulls, computeModules, step=10E7)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)

p.value <- sum(mod.small@likelihood<like.nulls) / 100
p.value

# Aqui podemos ver que o valor de p foi menor que
# 0.05, então a rede é modular.

# Existem diversas métricas de redes que podem ser
# utilizadas e exploradas de acordo com a pergunta
# ecológica que você estiver interessado em
# responder.

# Nessa função podemos observar várias delas

networklevel(small)

# Se tiver interesse em se aprofundar nas demais
# métricas ver em Lewinsohn et al., 2006

# Lewinsohn, Thomas & Loyola (2006). Matrizes,
# redes e ordenações: a detecção de estrutura em
# comunidades interativas. Oecologia Brasiliensis,
# ISSN 1981-9366, Vol. 10, Nº. 1, 2006 (Ejemplar
# dedicado a: II Simpósio de Ecologia Teórica).
# 10. 10.4257/oeco.2006.1001.06.

