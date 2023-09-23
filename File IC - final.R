#1. ANÁLISE DOS DADOS
#install.packages("tidyverse")
library(tidyverse)

#1.1 preparação do tema no ggplot
tema <- function(base_size=8,base_family="sans"){theme_bw(base_size = base_size, base_family = base_family)+
    theme(
      axis.line = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.ticks = element_blank(), 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      legend.position = "bottom", 
      panel.background = element_rect(fill = NA), 
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      plot.margin = unit(c(0,0,0,0), "lines")
    )
}

meu_tema <- function(base_size =5, base_family = "sans"){
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA),
      strip.background = element_rect(fill = "#FA8072", color = "#FA8072", size =0.5),
      strip.text = element_text(face = "bold", size = 5, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      legend.margin = margin(0.5,0.5,0.5,0.5)
    )
}

theme_set(meu_tema())

#1.2 carregamento do dataset
arritmia <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data", sep = ",")
arritmia[arritmia == "?"] <- NA

arritmia[-280] <- lapply(arritmia[-280], as.character)
arritmia[-280] <- lapply(arritmia[-280], as.numeric)

  #operação de indexação que seleciona as colunas nominais 
col_nominais=arritmia[,c(280,2,22:27,34:39,46:51,58:63,70:75,82:87,94:99,106:111,118:123,130:135,142:147,154:159)]

col_nominais[]<-lapply(col_nominais, as.factor)

colnames(col_nominais)[1] <- "class"

#1.3 rótulos de arritmia
ggplot(col_nominais, aes(x = as.factor(class)))+geom_bar(aes(fill=as.factor(class)), alpha = 0.7,show.legend = F)+meu_tema(10)+scale_x_discrete("16 rótulos de Arritimia Cardíaca")+geom_text(stat='count',aes(label=..count..),vjust=-0.5)

#1.4 simplificação em dois rótulos
col_nominais$class<-ifelse(col_nominais$class == "1", "saudável", "arritmia")
col_nominais$class =as.factor(col_nominais$class)

ggplot(col_nominais, aes(x = class))+geom_bar(aes(fill=as.factor(class)), alpha = 0.7,show.legend = F)+meu_tema(10)+geom_text(stat='count',aes(label=..count..),hjust=5,size=10,color="white")+coord_flip()+scale_fill_manual(values=c("#F08080","#40E0D0"))

col_nominais%>%gather(V2:V159,key="Feature",value="Level")%>%ggplot(aes(x=Feature,fill=class,color=class))+geom_bar(position="fill",alpha=0.7)+scale_fill_manual(values=c("#F08080","#40E0D0"))+scale_color_manual(values=c("#d80053","#006fd8"))+facet_wrap(~Level)+coord_polar()+scale_x_discrete("Levels (0 or 1)")

  #operação de indexação que seleciona as colunas nominais 
col_numericas=arritmia[,c(1,3:21,28:33,40:45,52:57,64:69,76:81,88:93,100:105,112:117,124:129,136:141,148:153,160:279)]

#1.5 verificar se há valores nulos
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("ggplot2")

valores_nulos <- function(x){
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2)) +
    geom_bar(aes(y=(..count..),fill=value),alpha=0.7)+scale_fill_manual(values=c("#DCDCDC","#DC143C"),name = "",
                                                                        labels = c("Presente","Faltando"))+
    theme_minimal(5)+
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Observations")+coord_flip()
}

col_numericas%>%valores_nulos()

  #removemos por falta de valores
col_numericas=col_numericas[,-c(11,12,13,14,15)]

#1.6 verificamos a variância 
#install.packages("matrixStats")
library(matrixStats)

variancias <- data.frame(feature = colnames(col_numericas),
                      variance = colVars(as.matrix(col_numericas)))

subset(variancias, variance==0)

subset(variancias, variance>=20)%>%
  mutate(feature = factor(feature, levels = colnames(col_numericas)))%>%
  ggplot(aes(x = reorder(feature,variance), y = variance))+
  geom_point(aes(color=variance), alpha = 0.7)+
  theme_minimal(5)+
  scale_color_gradient2(low="gold",high="#8A2BE2",mid="#DC143C",midpoint = 700)+scale_x_discrete("Variáveis Numéricas")+coord_polar()

  #remover variáveis
df=cbind(col_nominais,col_numericas)

removed=c("V22", "V165", "V140", "V20", "V68", "V146", "V133", "V85", "V158", "V145", "V132", "V84", "V157", "V46", "V155", "V142", "V72", "V144", "V152", "V86", "V38", "V70", "V205", "V275", "V265")

df=df[,!(names(df) %in% removed)] 

#1.7 correlação entre as variáveis
#install.packages("corrplot")
#install.packages("RColorBrewer")
library(corrplot)
library(RColorBrewer)

matriz_correlaçao=as.matrix(cor(use="pairwise.complete.obs",method="spearman",as.matrix(col_numericas)))

matriz_correlaçao%>%corrplot(.,hclust.method ="ward.D2",type="lower",method="color",tl.col="black", tl.srt=45,tl.cex=0.5,col=rev(brewer.pal(n=10, name="RdBu")))

#install.packages("igraph")
library(igraph)

diag(matriz_correlaçao)<-0

#install.packages("ggraph")
library(ggraph)

m=matriz_correlaçao

cdf=data.frame(row=rownames(m)[row(m)[upper.tri(m)]], 
               col=colnames(m)[col(m)[upper.tri(m)]], 
               corr=m[upper.tri(m)])

cdf=subset(cdf,abs(corr)>0.5)

cdf$Close=cdf$corr>=0.7
names(cdf)=c("from","to","correlação","Close")


graph<-graph_from_data_frame(cdf)

ggraph(graph, layout = 'fr')+geom_edge_density(aes(fill = Close))+geom_node_point()+geom_node_text(aes(label = name),nudge_x=,nudge_y=0.1,color="grey")+geom_edge_link(aes(colour = correlação),width=1,alpha=0.5)+tema()+scale_edge_fill_manual(values=c("white","#FF7F50"))+scale_edge_color_gradient(high="#FF8C00",low="#00BFFF")

#1.8 divisão em conjunto de treino e teste
#install.packages("caret")
set.seed(1234)

idTrain=caret::createDataPartition(y=df$class,p=351/452,list=FALSE)
treino=df[idTrain,]
teste=df[-idTrain,]

sp1=df%>%ggplot(aes(x=class,fill=class))+stat_count(color="black",alpha=0.7,show.legend = F)+scale_fill_manual(values=c("#FF7F50","#40E0D0"))+coord_flip()+ggtitle("Origin")+geom_text(stat='count',aes(label=..count..),hjust=5,size=5,color="white")+theme_bw(10)

sp2=treino%>%ggplot(aes(x=class,fill=class))+stat_count(color="black",alpha=0.7,show.legend = F)+scale_fill_manual(values=c("#FF7F50","#40E0D0"))+coord_flip()+ggtitle("Train")+geom_text(stat='count',aes(label=..count..),hjust=5,size=5,color="white")+theme_bw(10)

sp3=teste%>%ggplot(aes(x=class,fill=class))+stat_count(color="black",alpha=0.7,show.legend = F)+scale_fill_manual(values=c("#FF7F50","#40E0D0"))+coord_flip()+ggtitle("Test")+geom_text(stat='count',aes(label=..count..),hjust=5,size=5,color="white")+theme_bw(10)

gridExtra::grid.arrange(sp1,sp2,sp3,ncol=1)

#2. APLICAÇÃO DE RANDON FOREST
#install.packages("h2o")
library(h2o)

h2o.init(nthreads = -1,max_mem_size ="4g")

wtreino=as.h2o(treino)
wteste=as.h2o(teste)

features=setdiff(colnames(wtreino),"class")

#2.1 Replicando Randon Forest 100 vezes
set.seed(12345)
rfseed=sample(10000,100,replace=F)

res=NULL
pdf=NULL

for (i in 1:100){
  
  rfmod=h2o.randomForest(x = features,
                         y = "class",
                         training_frame = wtreino,nfolds=10,
                         fold_assignment = "Stratified",
                         balance_classes = TRUE,
                         ntrees = 100, max_depth = 50,mtries = -1,sample_rate = 0.7,
                         stopping_metric = "misclassification",
                         stopping_tolerance = 0.01,
                         stopping_rounds = 3,
                         keep_cross_validation_fold_assignment = F,
                         keep_cross_validation_predictions=F,
                         score_each_iteration = TRUE,
                         seed=rfseed[i])
  
  pdftemp=predict(rfmod,wteste)%>%as_tibble()%>%mutate(.,Id=row.names(teste),Truth=teste$class,Accuracy=ifelse(teste$class ==.$predict, "Correto", "Errado"),Model=i)
  
  vimp=rfmod@model$variable_importances%>%as_tibble()%>%mutate(.,Seed=rfseed[i],Model=i)
  
  res=rbind(res,vimp)
  
  pdf=rbind(pdf,pdftemp)
  
}

res=as_tibble(res)

res$Model=as.factor(res$Model)

#2.2 Verificando acurácia
pdf$Id=as.factor(pdf$Id)

pdf%>%ggplot(aes(x=Model,y=reorder(Id,arritmia),fill=arritmia))+geom_tile()+theme_bw()+scale_fill_gradient2(low="#00BFFF",mid="#F0E68C",high="#DC143C",midpoint = 0.5)+facet_wrap(~Truth,shrink=TRUE,scale="free",ncol=1)+theme_bw(8)+theme(axis.text.x = element_text(angle =45, hjust = 1))



pdf%>%ggplot(aes(x=Model,y=reorder(Id,1-arritmia),fill=Accuracy))+geom_tile()+theme_bw()+scale_fill_manual(values=c("skyblue","#DC143C"))+theme_bw(8)+theme(axis.text.x = element_text(angle =45, hjust = 1))+facet_wrap(~Truth,shrink=TRUE,scale="free",ncol=1)+scale_y_discrete("Idx")

#2.3 Probabilidade média prevista e a precisão média 
pdf2=NULL

#install.packages("Hmisc")
for (i in 1:100){
  Idt=pdf$Id[i]
  dtemp=subset(pdf,Id==Idt)
  sum=Hmisc::describe(dtemp)
  
  predprob=sum$arritmia$counts[[5]]
  acc=sum$Accuracy$values[[2]][1]/100
  wrong=sum$Accuracy$values[[2]][2]/100
  
  pdftemp=cbind.data.frame(Id=Idt,predprob,acc,wrong)
  pdf2=rbind(pdf2,pdftemp)
}

#2.4 Mediana da probabilidade prevista 
pdf2$class=teste$class
pdf2$predprob=as.numeric(as.character(pdf2$predprob))

pdf2$Id=as.factor(pdf2$Id)

pdf2$wrong[is.na(pdf2$wrong)] <- 0

pdf2%>%ggplot(aes(x=reorder(Id,predprob),y=predprob,fill=predprob))+geom_bar(alpha=0.7,stat="identity")+theme_bw(8)+scale_fill_gradient2(low="gold",mid="#DC143C",high="#8A2BE2",midpoint = 0.4)+facet_wrap(~class,shrink = TRUE,ncol=1,scales = "free")+geom_hline(yintercept = 0.5,linetype=2,size=1)

#2.5 Porcentagem de predições corretas/erradas
pdf2%>%gather(wrong,acc,key="Accuracy",value="Prediction")%>%ggplot(aes(x=reorder(Id,-predprob),y=Prediction,fill=Accuracy))+geom_bar(alpha=0.7,position="fill",stat="Identity")+theme_bw(8)+facet_wrap(~class,shrink = TRUE,ncol=1,scales = "free")+scale_fill_manual(values=c("skyblue","red"))+scale_x_discrete("Id")

#2.6 Dados importantes 
#2.6.1 Escores de importância relativa
res%>%ggplot(aes(x=reorder(Model,relative_importance),y=reorder(variable,-relative_importance),fill=relative_importance,color=relative_importance))+geom_tile(show.legend = F)+theme_bw()+scale_fill_gradient2(low="white",mid="red",high="purple",midpoint = 50)+scale_color_gradient2(low="white",mid="red",high="purple",midpoint = 50)+theme_bw(5)+theme(axis.text.x = element_text(angle =45, hjust = 1))+scale_x_discrete("Modelos")+scale_y_discrete("Recursos")

#2.6.2 Escores de importância em escala
res%>%ggplot(aes(x=reorder(Model,scaled_importance),y=reorder(variable,-scaled_importance),fill=scaled_importance,color=scaled_importance))+geom_tile(show.legend = F)+theme_bw()+scale_fill_gradient2(low="white",mid="gold",high="red",midpoint = 0.5)+scale_color_gradient2(low="white",mid="gold",high="red",midpoint =0.5)+theme_bw(5)+theme(axis.text.x = element_text(angle =45, hjust = 1))+scale_x_discrete("Modelos")+scale_y_discrete("Recursos")

#2.7 Pontuação de importância média para cada recurso
res2=NULL

#install.packages("psych")
for (i in 1:253){
  dtemp=subset(res,variable==features[i])
  sum=psych::describe(dtemp)
  rei=sum[[5]][2]
  sci=sum[[5]][3]
  pct=sum[[5]][4]
  
  rtemp=cbind.data.frame(rei,sci,pct,var=dtemp[[1]][1])
  res2=rbind(res2,rtemp)
  
}

res2%>%ggplot(aes(x=reorder(var,rei),y=rei,fill=rei,color=rei))+geom_segment(aes(x=reorder(var,rei),xend=var,y=0,yend=rei),size=1,show.legend = F)+geom_point(size=1,show.legend = F)+scale_x_discrete("Features")+scale_y_continuous("Relative importance")+coord_flip()+scale_fill_gradient(low="#9400D3",high="#FF7F50")+scale_color_gradient(low="#9400D3",high="#FF7F50")+ggtitle("Relative importance")+theme_bw(5)

#2.8 150 variáveis mais fortes em nossa entrada
res3=res2%>%arrange(desc(rei))%>%.[c(1:150),]

res3%>%ggplot(aes(x=reorder(var,rei),y=rei,fill=rei,color=rei))+geom_segment(aes(x=reorder(var,rei),xend=var,y=0,yend=rei),size=1,show.legend = F)+geom_point(size=1,show.legend = F)+scale_x_discrete("Features")+scale_y_continuous("Relative importance")+coord_flip()+scale_fill_gradient(low="blue",high="#FF7F50")+scale_color_gradient(low="blue",high="#FF7F50")+ggtitle("Relative importance")+theme_bw(5)


#2.9 Randon forest com as variáveis mais fortes
features2=as.vector(res3$var)

rfmod=h2o.randomForest(x = features2,
                       y = "class",
                       training_frame = wtreino,nfolds=10,
                       fold_assignment = "Stratified",
                       balance_classes = TRUE,
                       ntrees = 100, max_depth = 50,mtries = -1,sample_rate = 0.8,
                       stopping_metric = "misclassification",
                       stopping_tolerance = 0.01,
                       stopping_rounds = 3,
                       keep_cross_validation_fold_assignment = F,
                       keep_cross_validation_predictions=F,
                       score_each_iteration = TRUE,
                       seed=333)

#3. CONCLUSÃPO DO MODELO
#3.1 Eficiência do modelo
install.packages("mlr")
library(mlr)

task=makeClassifTask(data=teste,target="class")

dummylrner=makeLearner("classif.rpart",predict.type ="prob")
dummymod=train(dummylrner,task)
dummypred=predict(dummymod,task)

RFPRED=predict(rfmod,wteste)%>%as.data.frame()
RFPRED=RFPRED%>%mutate(.,id=c(1:100),truth=teste$class,prob.arritmia=.[,2],prob.healthy=.[,3],response=.$predict)%>%.[,-c(1:3)]

pred3=dummypred
pred3$data=RFPRED

measure=list(mlr::bac,mlr::auc,mlr::acc,mlr::f1,mlr::ppv,mlr::npv,mlr::tnr,mlr::tpr)

p3=performance(pred3,measure)

rf=p3%>%as.vector()

labs=c("BAC","AUC","ACC","F1","PPV","NPV","TPR","TNR")

scores=list("RandomForest"=rf*10)

install.packages("radarchart")
library(radarchart)

chartJSRadar(scores = scores, labs = labs, maxScale = 10)

#3.2 Fraqueza do modelo
measure2=list(ber,brier,fnr,fpr,mmce,fdr)

e3=performance(pred3,measure2)

rf=e3%>%as.vector()

labs=c("BER","BRIER","FNR","FPR","MMCE","FDR")

scores=list("RandomForest"=rf)

chartJSRadar(scores = scores, labs = labs, maxScale =0.8)
