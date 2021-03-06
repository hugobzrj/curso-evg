# Exerc�cios


cidade<-c("Bras�lia", "Rio de Janeiro", "S�o Paulo", "Santos")
temperatura<-c(15,14,25,30,25)
temperatura[1:3]

cidade[5]<-"Curitiba"
cidade
UF<-factor(c("DF","RJ","SP","SP","PR"))

grau.instrucao<-factor(c("NM","S","NM","F"),
                       levels<-c("F","NM","S"),
                       ordered=TRUE)
grau.instrucao

pessoa<-list(sexo="M", cidade="Bras�lia", idade=20)
pessoa[1]
pessoa[[1]]

pessoa
pessoa[["idade"]]<-22
pessoa

pessoa[c("cidade","idade")]

cidade
df<-data.frame(cidade,temperatura)
df
#
df[1,2]
# todas linhas de uma coluna
df[,1]
# todas colunas de uma linha
df[1,]
df
df[c(1:2),c(1:2)]

names(df)
dim(df)
str(df)

df$cidade
df['cidade']

m<-matrix(seq(1:9),nrow=3)
m

m2<-matrix(seq(1:25),
           ncol=5,
           byrow=TRUE,
           dimnames=list(c(seq(1:5)),
                         c('A','B','C','D','E')))
m2

# for

for (i in seq(12)){
  print(i)
}

# while

i<-0
while( i<=10) {
  print(i)
  i=i+1
}

# if

x=10
if (x>0) {
  print("N�mero positivo")
}

nota=9
if (nota >=7) {
  print("Aprovado")
} else if (nota>5 && nota <7) {
  print("Recupera��o")
} else {
  print("Reprovado")
}

# Fun��es


par.impar<- function(num) {
  if ((num%%2)==0) {
    return("Par!")
  } else 
    return("�mpar!")
}

par.impar(5)

# Utilizando apply

x<-seq(1:9)
matriz<-matrix(x,ncol=3)
matriz

result1<-apply(matriz,1,sum)
result1

result1<-apply(matriz,2,sum)
result1

# lapply

numeros.p<-c(2,4,6,8,10,12)
numeros.i<-c(1,3,5,7,9,11)
numeros<-list(numeros.p,numeros.i)
numeros

lapply(numeros,mean)
sapply(numeros,mean)

# Gr�ficos

carros<-mtcars[,c(1,2,9)]
head(carros)


hist(carros$mpg)
plot(carros$mpg,carros$cyl)

install.packages("ggplot2")
library(ggplot2)

ggplot(carros,aes(am))+geom_bar()

# JOINS - Unir conjunto de dados

df1<-data.frame(Produto=c(1,2,3,5), Pre�o=c(15,10,25,20))
head(df1)

df2<-data.frame(Produto=c(1,2,3,4), Nome=c("A","B","C","D"))
head(df2)

install.packages("dplyr")
library(dplyr)

df3<- left_join(df1,df2,"Produto")
df3

df4<- right_join(df1,df2,"Produto")
df4

df5<-inner_join(df1,df2,"Produto")
df5

# Selecionar dados

head(iris)

library("dplyr")

# Informa��es do banco de dados

glimpse(iris)

#Filter

versicolor<-filter(iris, Species=="versicolor")
dim(versicolor)
versicolor
slice(iris,5:10)

# Select colunas

select(iris,2:4)

# Retirar coluna
select(iris,-Sepal.Width)

# Inserindo nova coluna com opera��o

iris2<-mutate(iris, nova.coluna=Sepal.Length + Sepal.Width)
iris2

iris2[,c("Sepal.Length","Sepal.Width","nova.coluna")]

# Ordenar

select(iris,Sepal.Length) %>% arrange(Sepal.Length)

# GroupBY

iris %>% group_by(Species) %>% summarise(mean(Sepal.Length))

# Transformando dados

library(tidyr)

# Qde de Vendas por ano e produto

dfDate<-data.frame(Produto=c('A','B','C'),
                   A.2015=c(10,12,20),
                   A.2016=c(20,25,35),
                   A.2017=c(15,20,30))

dfDate

# Alterando linhas e Colunas

dfDate2<-gather(dfDate,"Ano","Quantidade",2:4)
dfDate2

dfDate3<-separate(dfDate2, Ano, c("A","Ano"))
dfDate3
dfDate3<- dfDate3[-2]
dfDate3

# Acrescentar coluna m�s

dfDate3$Mes<-c("01","02","03")
dfDate3

dfDate4<- dfDate3 %>% unite(Data,Mes,Ano,sep="/")
dfDate4



# Exemplo Pr�tico VIAGEM

# Baixando a base

viagens<-read.csv2(file="2019_viagem.csv",sep=";",dec=",")

# Verificando dados baixados

head(viagens)
View(viagens)
dim(viagens)

summary(viagens$Valor.passagens)

# Verificar cada coluna

glimpse(viagens)



### TRANSFORMA��O DOS DADOS OBTIDOS

#Convertendo o tipo do dato para tipo Date

?as.Date
# Estavam como factor

viagens$data.inicio <- as.Date(viagens$Per�odo...Data.de.in�cio, "%d/%m/%Y")

glimpse(viagens)


#Formatando a data de inicio - para utilizar apenas Ano/M�s

?format

viagens$data.inicio.formatada <- format(viagens$data.inicio, "%Y-%m")

viagens$data.inicio.formatada

### EXPLORA��O DOS DADOS 

#Gerando histograma da coluna passagens

hist(viagens$Valor.passagens)

#Outro exemplo de histograma - filtrando valores

#Para esse exemplo ser�o utilizadas as fun��es filter e select
?dplyr::filter
?dplyr::select

#Filtrando os valores das passagens - apenas passagens entre 200 e 5000

passagens_fitro <- viagens %>%
  select(Valor.passagens) %>%
  filter(Valor.passagens >= 200 & Valor.passagens <= 5000) 


passagens_fitro
hist(passagens_fitro$Valor.passagens)


#Verificando os valores min, max, m�dia... da coluna valor
summary(viagens$Valor.passagens)


#Visualizando os valores em um boxplot
boxplot(viagens$Valor.passagens)

#Visualizando os valores das passagens - filtro de 200 a 5000
boxplot(passagens_fitro$Valor.passagens)

#Calculando o desvio padr�o
sd(viagens$Valor.passagens)

#Verificar se existem valores n�o preenchidos nas colunas do dataframe
?is.na

?colSums

colSums(is.na(viagens))

#Verifcar a quantidade de categorias da coluna Situa��o 

#Converter para factor
viagens$Situa��o <- factor(viagens$Situa��o)


str(viagens$Situa��o)

#Verificar quantidade de registros em cada categoria
table(viagens$Situa��o)

#Obtendo os valores em percentual de cada categoria
prop.table(table(viagens$Situa��o))*100


### Visualiza��o dos resultados

# 1 - Qual � o valor gasto por �rg�o em passagens?

#Criando um dataframe com os 15 �rg�os que gastam mais
library(dplyr)
p1 <- viagens %>%
  group_by(Nome.do.�rg�o.superior) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

p1

#Alterando o nome das colunas
names(p1) <- c("orgao", "valor")

p1


#Plotando os dados com o ggplot

library(ggplot2)
ggplot(p1, aes(x = reorder(orgao, valor), y = valor))+
  geom_bar(stat = "identity")+
  coord_flip()+ 
  labs(x = "Valor", y = "�rg�os")



# 2 - Qual � o valor gasto por cidade?

#Criando um dataframe com as 15 cidades que gastam mais
p2 <- viagens %>%
  group_by(Destinos) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

p2

#Alterando o nome das colunas
names(p2) <- c("destino", "valor")
p2

#Criando o gr�fico

ggplot(p2, aes(x = reorder(destino, valor), y = valor))+
  geom_bar(stat = "identity", fill = "#0ba791")+
  geom_text(aes(label = valor), vjust = 0.3, size = 3)+
  coord_flip()+
  labs(x = "Valor", y = "Destino")

options(scipen = 999)

# 3 - Qual � a quantidade de viagens por m�s?

#Criando um dataframe com a quantidade de viagens por Ano/m�s
p3 <- viagens %>%
  group_by(data.inicio.formatada) %>%
  summarise(qtd = n_distinct(Identificador.do.processo.de.viagem))

head(p3)

#Criando o gr�fico
ggplot(p3, aes(x = data.inicio.formatada, y = qtd, group = 1))+
  geom_line()+
  geom_point()



#Exemplo de utiliza��o do R markdown

#Na linguagem R � poss�vel usar o Markdown para formatar os seus relat�rios


#Instala��o do Rmarkdown
install.packages("rmarkdown")
install.packages('tinytex')
library(tinytex)

#esta linha poder� levar alguns minutos para terminar a execu��o
#acompanhe o progresso na aba console e aguarde a instala��o ser finalizada

tinytex::install_tinytex()


# Pr�ximos passos

# Criar um arquivo R Markdown: File > New File RMarkdown
# Criar script
# Gerar Relat�rio: Knit > Knit to PDF

