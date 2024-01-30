library(readr)

#========GLICOSE============================
dados_glicose <- read.csv("glicose.csv")
View(dados_glicose)     
ggpairs(dados_glicose[,c(1,2,3)]) #jejum
ggpairs(dados_glicose[,c(4,5,6)]) #após ingestão de açũcar
ggpairs(dados_glicose)

# MVN Package (observar os plots, qqplots)
library(MVN)

result <-mvn(data = dados_glicose, mvnTest = "royston")


#matriz de covariancias estimada
S<-cov(dados_glicose);S

library(corrplot)
Mcor <-cor(dados_glicose)
corrplot(Mcor, type="upper",method="ellipse",tl.pos="tl")
corrplot(Mcor,add=TRUE,type="lower",method="number",
         tl.pos="n",cl.pos = "n")->p
corrRect(p,clus=c(5,3))

if (!require("candisc")){
  install.packages("candisc");
  library(candisc)}

dadosP<-scale(dados_glicose,center=TRUE,scale = TRUE)
X<-dadosP[,1:3] # grupo 1 
Y<-dadosP[,4:6] # grupo 2 
cc<- cancor(X,Y)
summary(cc)


library(CCA)
library(CCP)

ccyx <- cc(X,Y)
names(ccyx)
v.ca <-ccyx$cor;v.ca

# Coeficientes padronizados

s1 <- diag(sqrt(diag(cov(dados_glicose[,1:3]))))
s1 %*% ccyx$xcoef
s2 <- diag(sqrt(diag(cov(dados_glicose[,4:6]))))
s2%*% ccyx$ycoef



# Gráfico de barras
#barplot(ccyx$cor, xlab = "Dimensao",
        ylab = "Correlacao canonica", names.arg = 1:2, ylim = c(0,1))


#Teste de significância

n<-50;
p<-q<-3;

#Lambda de Wilks
p.asym(rho, n, p, q, tstat = "Wilks")
#Traço de Pillai
p.asym(rho, n, p, q, tstat = "Pillai")
#Traço de Hoteling
p.asym(rho, n, p, q, tstat = "Hotelling")
#Traço de gcr de Roy
p.asym(rho, n, p, q, tstat = "Roy")

#Gráficos
res1 <- p.asym(rho, n, p, q)
plot.new()
plt.asym(res1,rhostart=1)
plt.asym(res1,rhostart=2)
plt.asym(res1,rhostart=3)

loadings<- comput(X,Y,ccyx)



res<-data.frame(c("Ca","Mg","SB","t"),
                rbind(loadings$corr.X.xscores,loadings$corr.Y.xscores),
                rbind(loadings$corr.X.yscores,loadings$corr.Y.yscores))
colnames(res)<-c("Var.Originais","U_hat_1","U_hat_2","V_hat_1",
                 "V_hat_2")
res

#Proporcao da explicacao de cada 
#grupo que foi atribuida as 
#variaveis canonicas selecionadas
#correspondentes no mesmo grupo

#PVTE Uk:
pvte.u <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*100;pvte.u 

#76.32021


#PVTE Vk:
pvte.v <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*100;pvte.v

#85.33364

#85,3%  da variacao no segundo grupo de 
#variaveis originais padronizadas 
#foi explicada pela variavel canonica 1
#e 76,3% da variacao no grupo 1 foi 
#explicada pela primeira variavel 
#canonica do mesmo grupo



#=========SUSHI============
dados_sushi <- read.delim(file = "sushi_canonico.txt")
View(dados_sushi)
library(ggplot2)
library(GGally)
ggpairs(dados_sushi)
ggpairs(dados_sushi[,c(3,4,5,7,8,9,10)])
pm <- ggpairs(
  dados_sushi,
  upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
  lower = list(continuous = ggally_points, combo = ggally_dot_no_facet))

pm

# MVN Package (observar os plots, qqplots)
library(MVN)

result <-mvn(data = dados_sushi, mvnTest = "royston")

#matriz de covariancias estimada
S<-cov(dados_sushi);S

library(corrplot)
plot.new()
Mcor <-cor(dados_sushi)
corrplot(Mcor, type="upper",method="ellipse",tl.pos="tl")
corrplot(Mcor,add=TRUE,type="lower",method="number",
         tl.pos="n",cl.pos = "n")->p
corrRect(p,clus=c(5,3))

if (!require("candisc")){
  install.packages("candisc");
  library(candisc)}

dadosP<-scale(dados_sushi,center=TRUE,scale = TRUE)
X<-dadosP[,1:2] # grupo 1
Y<-dadosP[,3:10] # grupo 2


cc<- cancor(X,Y)
summary(cc)




library(CCA)
library(CCP)

ccyx <- cc(X,Y)
names(ccyx)
v.ca <-ccyx$cor;v.ca

# Coeficientes padronizados

s1 <- diag(sqrt(diag(cov(dados_sushi[,1:2]))))
s1 %*% ccyx$xcoef
s2 <- diag(sqrt(diag(cov(dados_sushi[,3:10]))))
s2%*% ccyx$ycoef



# Gráfico de barras
#barplot(ccyx$cor, xlab = "Dimensao",
ylab = "Correlacao canonica", names.arg = 1:2, ylim = c(0,1))


#Teste de significância

n<-30;
p<-q<-3;

#Lambda de Wilks
p.asym(rho, n, p, q, tstat = "Wilks")
#Traço de Pillai
p.asym(rho, n, p, q, tstat = "Pillai")
#Traço de Hoteling
p.asym(rho, n, p, q, tstat = "Hotelling")
#Traço de gcr de Roy
p.asym(rho, n, p, q, tstat = "Roy")



