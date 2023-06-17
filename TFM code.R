## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------
#Libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(ggcorrplot)
library(caret)
library(MASS)
library(ggfortify)
library(ggpubr)
library(GGally)
library(lattice)
library(h2o)
library(rgl)
library(factoextra)
library(cluster)
library(statip)
library(moments)
library(devtools)
library(klaR)
library(corrplot)
library(tidyverse)
library(psych)
library(skimr)
library(tidyr)
library(e1071) 
library(VGAM)
library(heatmaply)
library(lmtest)
library(car)
library(boot)
library(statmod)
library(ResourceSelection)
library(Epi)
library(pROC)
library(BAS)
library(rstanarm)
library(bayesplot)
library(loo)
library(projpred)
library(posterior)


## -------------------------------------------------------------------------------------------------------------------------------------
#DATA SET CONSTRUCTION AND PREPROCESSING


## -------------------------------------------------------------------------------------------------------------------------------------
#Load original play-by-play dataset
pbp=RMdSG_ACB_PbP <- readRDS("RMdSG_ACB_PbP.rds")
#Set encoding to UTF-8
pbp$Descripcion=iconv(pbp$Descripcion, from="ISO-8859-1", to="UTF-8")


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 03-04 season
#Filter the season and create empty vectors to store the variables
pbp34=pbp %>% filter(ed=="2003-2004")
par34=sort(unique(pbp34$partid))
t2l_34=numeric(length(par34))
t2v_34=numeric(length(par34))
t3l_34=numeric(length(par34))
t3v_34=numeric(length(par34))
t1l_34=numeric(length(par34))
t1v_34=numeric(length(par34))
fpl_34=numeric(length(par34))
fpv_34=numeric(length(par34))
rol_34=numeric(length(par34))
rov_34=numeric(length(par34))
rdl_34=numeric(length(par34))
rdv_34=numeric(length(par34))
pl_34=numeric(length(par34))
pv_34=numeric(length(par34))
resl_34=numeric(length(par34))
resv_34=numeric(length(par34))
V_34=numeric(length(par34))
efl_34=numeric(length(par34))
efv_34=numeric(length(par34))
t1adl_34=numeric(length(par34))
t1adv_34=numeric(length(par34))
adjl_34=numeric(length(par34))
adjv_34=numeric(length(par34))
dif_34=numeric(length(par34))

#Create each variable
for(i in 1:length(par34)){
  par=pbp34[pbp34$partid==par34[i],]
  eq=unique(na.omit(pbp34[pbp34$partid==par34[i],]$eq))
  t2l_34[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_34[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_34[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_34[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_34[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_34[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_34[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_34[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_34[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_34[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_34[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_34[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_34[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_34[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_34[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_34[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_34[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_34[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_34[[i]]>resv_34[[i]]){
    V_34[i]="1"
  }
  else if(resl_34[[i]]<resv_34[[i]]){
    V_34[i]="0"
  }
  else{
    V_34[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_34[i]=l1[(nrow(t)+1)]
  t1adv_34[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_34[i]=resl_34[i]*100/posl
  adjv_34[i]=resv_34[i]*100/posv
  
  dif_34[i]=abs(resl_34[i]-resv_34[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 04-05 season
#Filter the season and create empty vectors to store the variables
pbp45=pbp %>% filter(ed=="2004-2005")
par45=sort(unique(pbp45$partid))
t2l_45=numeric(length(par45))
t2v_45=numeric(length(par45))
t3l_45=numeric(length(par45))
t3v_45=numeric(length(par45))
t1l_45=numeric(length(par45))
t1v_45=numeric(length(par45))
fpl_45=numeric(length(par45))
fpv_45=numeric(length(par45))
rol_45=numeric(length(par45))
rov_45=numeric(length(par45))
rdl_45=numeric(length(par45))
rdv_45=numeric(length(par45))
pl_45=numeric(length(par45))
pv_45=numeric(length(par45))
resl_45=numeric(length(par45))
resv_45=numeric(length(par45))
V_45=numeric(length(par45))
efl_45=numeric(length(par45))
efv_45=numeric(length(par45))
t1adl_45=numeric(length(par45))
t1adv_45=numeric(length(par45))
adjl_45=numeric(length(par45))
adjv_45=numeric(length(par45))
dif_45=numeric(length(par45))

#Create each variable
for(i in 1:length(par45)){
  par=pbp45[pbp45$partid==par45[i],]
  eq=unique(na.omit(pbp45[pbp45$partid==par45[i],]$eq))
  t2l_45[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_45[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_45[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_45[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_45[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_45[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_45[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_45[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_45[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_45[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_45[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_45[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_45[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_45[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_45[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_45[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_45[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_45[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_45[[i]]>resv_45[[i]]){
    V_45[i]="1"
  }
  else if(resl_45[[i]]<resv_45[[i]]){
    V_45[i]="0"
  }
  else{
    V_45[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_45[i]=l1[(nrow(t)+1)]
  t1adv_45[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_45[i]=resl_45[i]*100/posl
  adjv_45[i]=resv_45[i]*100/posv
  
  dif_45[i]=abs(resl_45[i]-resv_45[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 05-06 season
#Filter the season and create empty vectors to store the variables
pbp56=pbp %>% filter(ed=="2005-2006")
par56=sort(unique(pbp56$partid))
t2l_56=numeric(length(par56))
t2v_56=numeric(length(par56))
t3l_56=numeric(length(par56))
t3v_56=numeric(length(par56))
t1l_56=numeric(length(par56))
t1v_56=numeric(length(par56))
fpl_56=numeric(length(par56))
fpv_56=numeric(length(par56))
rol_56=numeric(length(par56))
rov_56=numeric(length(par56))
rdl_56=numeric(length(par56))
rdv_56=numeric(length(par56))
pl_56=numeric(length(par56))
pv_56=numeric(length(par56))
resl_56=numeric(length(par56))
resv_56=numeric(length(par56))
V_56=numeric(length(par56))
efl_56=numeric(length(par56))
efv_56=numeric(length(par56))
t1adl_56=numeric(length(par56))
t1adv_56=numeric(length(par56))
adjl_56=numeric(length(par56))
adjv_56=numeric(length(par56))
dif_56=numeric(length(par56))

#Create each variable
for(i in 1:length(par56)){
  par=pbp56[pbp56$partid==par56[i],]
  eq=unique(na.omit(pbp56[pbp56$partid==par56[i],]$eq))
  t2l_56[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_56[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_56[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_56[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_56[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_56[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_56[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_56[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_56[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_56[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_56[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_56[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_56[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_56[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_56[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_56[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_56[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_56[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_56[[i]]>resv_56[[i]]){
    V_56[i]="1"
  }
  else if(resl_56[[i]]<resv_56[[i]]){
    V_56[i]="0"
  }
  else{
    V_56[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_56[i]=l1[(nrow(t)+1)]
  t1adv_56[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_56[i]=resl_56[i]*100/posl
  adjv_56[i]=resv_56[i]*100/posv
  
  dif_56[i]=abs(resl_56[i]-resv_56[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 06-07 season
#Filter the season and create empty vectors to store the variables
pbp67=pbp %>% filter(ed=="2006-2007")
par67=sort(unique(pbp67$partid))
t2l_67=numeric(length(par67))
t2v_67=numeric(length(par67))
t3l_67=numeric(length(par67))
t3v_67=numeric(length(par67))
t1l_67=numeric(length(par67))
t1v_67=numeric(length(par67))
fpl_67=numeric(length(par67))
fpv_67=numeric(length(par67))
rol_67=numeric(length(par67))
rov_67=numeric(length(par67))
rdl_67=numeric(length(par67))
rdv_67=numeric(length(par67))
pl_67=numeric(length(par67))
pv_67=numeric(length(par67))
resl_67=numeric(length(par67))
resv_67=numeric(length(par67))
V_67=numeric(length(par67))
efl_67=numeric(length(par67))
efv_67=numeric(length(par67))
t1adl_67=numeric(length(par67))
t1adv_67=numeric(length(par67))
adjl_67=numeric(length(par67))
adjv_67=numeric(length(par67))
dif_67=numeric(length(par67))

#Create each
for(i in 1:length(par67)){
  par=pbp67[pbp67$partid==par67[i],]
  eq=unique(na.omit(pbp67[pbp67$partid==par67[i],]$eq))
  t2l_67[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_67[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_67[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_67[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_67[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_67[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_67[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_67[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_67[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_67[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_67[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_67[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_67[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_67[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_67[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_67[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_67[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_67[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_67[[i]]>resv_67[[i]]){
    V_67[i]="1"
  }
  else if(resl_67[[i]]<resv_67[[i]]){
    V_67[i]="0"
  }
  else{
    V_67[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_67[i]=l1[(nrow(t)+1)]
  t1adv_67[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_67[i]=resl_67[i]*100/posl
  adjv_67[i]=resv_67[i]*100/posv
  
  dif_67[i]=abs(resl_67[i]-resv_67[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 07-08 season
#Filter the season and create empty vectors to store the variables
pbp78=pbp %>% filter(ed=="2007-2008")
par78=sort(unique(pbp78$partid))
t2l_78=numeric(length(par78))
t2v_78=numeric(length(par78))
t3l_78=numeric(length(par78))
t3v_78=numeric(length(par78))
t1l_78=numeric(length(par78))
t1v_78=numeric(length(par78))
fpl_78=numeric(length(par78))
fpv_78=numeric(length(par78))
rol_78=numeric(length(par78))
rov_78=numeric(length(par78))
rdl_78=numeric(length(par78))
rdv_78=numeric(length(par78))
pl_78=numeric(length(par78))
pv_78=numeric(length(par78))
resl_78=numeric(length(par78))
resv_78=numeric(length(par78))
V_78=numeric(length(par78))
efl_78=numeric(length(par78))
efv_78=numeric(length(par78))
t1adl_78=numeric(length(par78))
t1adv_78=numeric(length(par78))
adjl_78=numeric(length(par78))
adjv_78=numeric(length(par78))
dif_78=numeric(length(par78))

#Create each variable
for(i in 1:length(par78)){
  par=pbp78[pbp78$partid==par78[i],]
  eq=unique(na.omit(pbp78[pbp78$partid==par78[i],]$eq))
  t2l_78[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_78[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_78[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_78[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_78[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_78[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_78[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_78[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_78[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_78[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_78[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_78[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_78[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_78[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_78[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_78[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
 
  resl_78[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_78[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_78[[i]]>resv_78[[i]]){
    V_78[i]="1"
  }
  else if(resl_78[[i]]<resv_78[[i]]){
    V_78[i]="0"
  }
  else{
    V_78[i]="E"
  } 
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_78[i]=l1[(nrow(t)+1)]
  t1adv_78[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_78[i]=resl_78[i]*100/posl
  adjv_78[i]=resv_78[i]*100/posv
  
  dif_78[i]=abs(resl_78[i]-resv_78[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 08-09 season
#Filter the season and create empty vectors to store the variables
pbp89=pbp %>% filter(ed=="2008-2009")
par89=sort(unique(pbp89$partid))
t2l_89=numeric(length(par89))
t2v_89=numeric(length(par89))
t3l_89=numeric(length(par89))
t3v_89=numeric(length(par89))
t1l_89=numeric(length(par89))
t1v_89=numeric(length(par89))
fpl_89=numeric(length(par89))
fpv_89=numeric(length(par89))
rol_89=numeric(length(par89))
rov_89=numeric(length(par89))
rdl_89=numeric(length(par89))
rdv_89=numeric(length(par89))
pl_89=numeric(length(par89))
pv_89=numeric(length(par89))
resl_89=numeric(length(par89))
resv_89=numeric(length(par89))
V_89=numeric(length(par89))
efl_89=numeric(length(par89))
efv_89=numeric(length(par89))
t1adl_89=numeric(length(par89))
t1adv_89=numeric(length(par89))
adjl_89=numeric(length(par89))
adjv_89=numeric(length(par89))
dif_89=numeric(length(par89))

#Create each variable
for(i in 1:length(par89)){
  par=pbp89[pbp89$partid==par89[i],]
  eq=unique(na.omit(pbp89[pbp89$partid==par89[i],]$eq))
  t2l_89[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_89[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_89[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_89[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_89[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_89[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_89[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_89[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_89[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_89[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_89[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_89[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_89[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_89[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_89[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_89[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_89[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_89[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_89[[i]]>resv_89[[i]]){
    V_89[i]="1"
  }
  else if(resl_89[[i]]<resv_89[[i]]){
    V_89[i]="0"
  }
  else{
    V_89[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_89[i]=l1[(nrow(t)+1)]
  t1adv_89[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_89[i]=resl_89[i]*100/posl
  adjv_89[i]=resv_89[i]*100/posv
  
  dif_89[i]=abs(resl_89[i]-resv_89[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 09-10 season
#Filter the season and create empty vectors to store the variables
pbp910=pbp %>% filter(ed=="2009-2010")
par910=sort(unique(pbp910$partid))
t2l_910=numeric(length(par910))
t2v_910=numeric(length(par910))
t3l_910=numeric(length(par910))
t3v_910=numeric(length(par910))
t1l_910=numeric(length(par910))
t1v_910=numeric(length(par910))
fpl_910=numeric(length(par910))
fpv_910=numeric(length(par910))
rol_910=numeric(length(par910))
rov_910=numeric(length(par910))
rdl_910=numeric(length(par910))
rdv_910=numeric(length(par910))
pl_910=numeric(length(par910))
pv_910=numeric(length(par910))
resl_910=numeric(length(par910))
resv_910=numeric(length(par910))
V_910=numeric(length(par910))
efl_910=numeric(length(par910))
efv_910=numeric(length(par910))
t1adl_910=numeric(length(par910))
t1adv_910=numeric(length(par910))
adjl_910=numeric(length(par910))
adjv_910=numeric(length(par910))
dif_910=numeric(length(par910))

#Create each variable
for(i in 1:length(par910)){
  par=pbp910[pbp910$partid==par910[i],]
  eq=unique(na.omit(pbp910[pbp910$partid==par910[i],]$eq))
  t2l_910[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_910[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_910[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_910[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_910[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_910[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_910[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_910[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_910[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_910[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_910[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_910[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_910[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_910[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_910[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_910[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_910[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_910[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_910[[i]]>resv_910[[i]]){
    V_910[i]="1"
  }
  else if(resl_910[[i]]<resv_910[[i]]){
    V_910[i]="0"
  }
  else{
    V_910[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_910[i]=l1[(nrow(t)+1)]
  t1adv_910[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_910[i]=resl_910[i]*100/posl
  adjv_910[i]=resv_910[i]*100/posv
  
  dif_910[i]=abs(resl_910[i]-resv_910[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 10-11 season
#Filter the season and create empty vectors to store the variables
pbp1011=pbp %>% filter(ed=="2010-2011")
par1011=sort(unique(pbp1011$partid))
t2l_1011=numeric(length(par1011))
t2v_1011=numeric(length(par1011))
t3l_1011=numeric(length(par1011))
t3v_1011=numeric(length(par1011))
t1l_1011=numeric(length(par1011))
t1v_1011=numeric(length(par1011))
fpl_1011=numeric(length(par1011))
fpv_1011=numeric(length(par1011))
rol_1011=numeric(length(par1011))
rov_1011=numeric(length(par1011))
rdl_1011=numeric(length(par1011))
rdv_1011=numeric(length(par1011))
pl_1011=numeric(length(par1011))
pv_1011=numeric(length(par1011))
resl_1011=numeric(length(par1011))
resv_1011=numeric(length(par1011))
V_1011=numeric(length(par1011))
efl_1011=numeric(length(par1011))
efv_1011=numeric(length(par1011))
t1adl_1011=numeric(length(par1011))
t1adv_1011=numeric(length(par1011))
adjl_1011=numeric(length(par1011))
adjv_1011=numeric(length(par1011))
dif_1011=numeric(length(par1011))

#Create each variable
for(i in 1:length(par1011)){
  par=pbp1011[pbp1011$partid==par1011[i],]
  eq=unique(na.omit(pbp1011[pbp1011$partid==par1011[i],]$eq))
  t2l_1011[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_1011[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_1011[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_1011[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_1011[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_1011[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_1011[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_1011[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_1011[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_1011[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_1011[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_1011[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_1011[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_1011[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_1011[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_1011[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_1011[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_1011[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_1011[[i]]>resv_1011[[i]]){
    V_1011[i]="1"
  }
  else if(resl_1011[[i]]<resv_1011[[i]]){
    V_1011[i]="0"
  }
  else{
    V_1011[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_1011[i]=l1[(nrow(t)+1)]
  t1adv_1011[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_1011[i]=resl_1011[i]*100/posl
  adjv_1011[i]=resv_1011[i]*100/posv
  
  dif_1011[i]=abs(resl_1011[i]-resv_1011[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 11-12 season
#Filter the season and create empty vectors to store the variables
pbp1112=pbp %>% filter(ed=="2011-2012")
par1112=sort(unique(pbp1112$partid))
t2l_1112=numeric(length(par1112))
t2v_1112=numeric(length(par1112))
t3l_1112=numeric(length(par1112))
t3v_1112=numeric(length(par1112))
t1l_1112=numeric(length(par1112))
t1v_1112=numeric(length(par1112))
fpl_1112=numeric(length(par1112))
fpv_1112=numeric(length(par1112))
rol_1112=numeric(length(par1112))
rov_1112=numeric(length(par1112))
rdl_1112=numeric(length(par1112))
rdv_1112=numeric(length(par1112))
pl_1112=numeric(length(par1112))
pv_1112=numeric(length(par1112))
resl_1112=numeric(length(par1112))
resv_1112=numeric(length(par1112))
V_1112=numeric(length(par1112))
efl_1112=numeric(length(par1112))
efv_1112=numeric(length(par1112))
t1adl_1112=numeric(length(par1112))
t1adv_1112=numeric(length(par1112))
adjl_1112=numeric(length(par1112))
adjv_1112=numeric(length(par1112))
dif_1112=numeric(length(par1112))

#Create each variable
for(i in 1:length(par1112)){
  par=pbp1112[pbp1112$partid==par1112[i],]
  eq=unique(na.omit(pbp1112[pbp1112$partid==par1112[i],]$eq))
  t2l_1112[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_1112[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_1112[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_1112[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_1112[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_1112[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_1112[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_1112[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_1112[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_1112[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_1112[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_1112[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_1112[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_1112[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_1112[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_1112[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_1112[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_1112[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_1112[[i]]>resv_1112[[i]]){
    V_1112[i]="1"
  }
  else if(resl_1112[[i]]<resv_1112[[i]]){
    V_1112[i]="0"
  }
  else{
    V_1112[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_1112[i]=l1[(nrow(t)+1)]
  t1adv_1112[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_1112[i]=resl_1112[i]*100/posl
  adjv_1112[i]=resv_1112[i]*100/posv
  
  dif_1112[i]=abs(resl_1112[i]-resv_1112[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate data for the 12-13 season
#Filter the season and create empty vectors to store the variables
pbp1213=pbp %>% filter(ed=="2012-2013")
par1213=sort(unique(pbp1213$partid))
t2l_1213=numeric(length(par1213))
t2v_1213=numeric(length(par1213))
t3l_1213=numeric(length(par1213))
t3v_1213=numeric(length(par1213))
t1l_1213=numeric(length(par1213))
t1v_1213=numeric(length(par1213))
fpl_1213=numeric(length(par1213))
fpv_1213=numeric(length(par1213))
rol_1213=numeric(length(par1213))
rov_1213=numeric(length(par1213))
rdl_1213=numeric(length(par1213))
rdv_1213=numeric(length(par1213))
pl_1213=numeric(length(par1213))
pv_1213=numeric(length(par1213))
resl_1213=numeric(length(par1213))
resv_1213=numeric(length(par1213))
V_1213=numeric(length(par1213))
efl_1213=numeric(length(par1213))
efv_1213=numeric(length(par1213))
t1adl_1213=numeric(length(par1213))
t1adv_1213=numeric(length(par1213))
adjl_1213=numeric(length(par1213))
adjv_1213=numeric(length(par1213))
dif_1213=numeric(length(par1213))

#Create each variable
for(i in 1:length(par1213)){
  par=pbp1213[pbp1213$partid==par1213[i],]
  eq=unique(na.omit(pbp1213[pbp1213$partid==par1213[i],]$eq))
  t2l_1213[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2")))
  t2v_1213[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2")))
  t3l_1213[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))
  t3v_1213[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))
  t1l_1213[i]=100*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  t1v_1213[i]=100*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  fpl_1213[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Falta Personal"))
  fpv_1213[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Falta Personal"))
  rol_1213[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))
  rov_1213[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))
  rdl_1213[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Defensivo"))
  rdv_1213[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Defensivo"))
  pl_1213[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))
  pv_1213[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))
  efl_1213[i]=100*((sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3"))))
  efv_1213[i]=100*((sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+1.5*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3")))/(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3"))))
  
  resl_1213[i]=sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))
  resv_1213[i]=sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+2*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+3*sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))
  if(resl_1213[[i]]>resv_1213[[i]]){
    V_1213[i]="1"
  }
  else if(resl_1213[[i]]<resv_1213[[i]]){
    V_1213[i]="0"
  }
  else{
    V_1213[i]="E"
  }
  
  t=par%>%filter(Descripcion=="Canasta de 2" | Descripcion=="Canasta de 1" | Descripcion=="Canasta de 3")
  l=numeric((nrow(t)+1))
  v=numeric((nrow(t)+1))
  l[1]=0
  v[1]=0
  l1=numeric((nrow(t)+1))
  v1=numeric((nrow(t)+1))
  l1[1]=0
  v1[1]=0
  for(j in 2:(nrow(t)+1)){
    if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+1
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+2
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+3
      v[j]=v[[j-1]]+0
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+1
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 2"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+2
    }
    else if(t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 3"){
      l[j]=l[[j-1]]+0
      v[j]=v[[j-1]]+3
    }
  }
  
  for(j in 2:(nrow(t)+1)){
    if((l[j]-v[j])>0 & t[j-1,]$eq==eq[1] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+1
      v1[j]=v1[j-1]+0
    }
    else if((v[j]-l[j])>0 & t[j-1,]$eq==eq[2] & t[j-1,]$Descripcion=="Canasta de 1"){
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+1
    }
    else {
      l1[j]=l1[j-1]+0
      v1[j]=v1[j-1]+0
    }
  }
  
  t1adl_1213[i]=l1[(nrow(t)+1)]
  t1adv_1213[i]=v1[(nrow(t)+1)]
  
  posl=(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[1],]$Descripcion=="Intento fallado de 1")))
  posv=(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 2"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 3"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 3")))-sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Rebote Ofensivo"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Pérdida"))+0.454*(sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Canasta de 1"))+sum(na.omit(par[par$eq==eq[2],]$Descripcion=="Intento fallado de 1")))
  adjl_1213[i]=resl_1213[i]*100/posl
  adjv_1213[i]=resv_1213[i]*100/posv
  
  dif_1213[i]=abs(resl_1213[i]-resv_1213[i])
}


## -------------------------------------------------------------------------------------------------------------------------------------
#Create a matrix with every variable for each season
t34=cbind(t2l_34, t2v_34, t3l_34, t3v_34, t1l_34, t1v_34, fpl_34, fpv_34, rol_34, rov_34, rdl_34, rdv_34, pl_34, pv_34, efl_34, efv_34, t1adl_34, t1adv_34, adjl_34, adjv_34, dif_34, V_34)
t45=cbind(t2l_45, t2v_45, t3l_45, t3v_45, t1l_45, t1v_45, fpl_45, fpv_45, rol_45, rov_45, rdl_45, rdv_45, pl_45, pv_45, efl_45, efv_45, t1adl_45, t1adv_45, adjl_45, adjv_45, dif_45, V_45)
t56=cbind(t2l_56, t2v_56, t3l_56, t3v_56, t1l_56, t1v_56, fpl_56, fpv_56, rol_56, rov_56, rdl_56, rdv_56, pl_56, pv_56, efl_56, efv_56, t1adl_56, t1adv_56, adjl_56, adjv_56, dif_56, V_56)
t67=cbind(t2l_67, t2v_67, t3l_67, t3v_67, t1l_67, t1v_67, fpl_67, fpv_67, rol_67, rov_67, rdl_67, rdv_67, pl_67, pv_67, efl_67, efv_67, t1adl_67, t1adv_67, adjl_67, adjv_67, dif_67, V_67)
t78=cbind(t2l_78, t2v_78, t3l_78, t3v_78, t1l_78, t1v_78, fpl_78, fpv_78, rol_78, rov_78, rdl_78, rdv_78, pl_78, pv_78, efl_78, efv_78, t1adl_78, t1adv_78, adjl_78, adjv_78, dif_78, V_78)
t89=cbind(t2l_89, t2v_89, t3l_89, t3v_89, t1l_89, t1v_89, fpl_89, fpv_89, rol_89, rov_89, rdl_89, rdv_89, pl_89, pv_89, efl_89, efv_89, t1adl_89, t1adv_89, adjl_89, adjv_89, dif_89, V_89)
t910=cbind(t2l_910, t2v_910, t3l_910, t3v_910, t1l_910, t1v_910, fpl_910, fpv_910, rol_910, rov_910, rdl_910, rdv_910, pl_910, pv_910, efl_910, efv_910, t1adl_910, t1adv_910, adjl_910, adjv_910, dif_910, V_910)
t1011=cbind(t2l_1011, t2v_1011, t3l_1011, t3v_1011, t1l_1011, t1v_1011, fpl_1011, fpv_1011, rol_1011, rov_1011, rdl_1011, rdv_1011, pl_1011, pv_1011, efl_1011, efv_1011, t1adl_1011, t1adv_1011, adjl_1011, adjv_1011, dif_1011, V_1011)
t1112=cbind(t2l_1112, t2v_1112, t3l_1112, t3v_1112, t1l_1112, t1v_1112, fpl_1112, fpv_1112, rol_1112, rov_1112, rdl_1112, rdv_1112, pl_1112, pv_1112, efl_1112, efv_1112, t1adl_1112, t1adv_1112, adjl_1112, adjv_1112, dif_1112, V_1112)
t1213=cbind(t2l_1213, t2v_1213, t3l_1213, t3v_1213, t1l_1213, t1v_1213, fpl_1213, fpv_1213, rol_1213, rov_1213, rdl_1213, rdv_1213, pl_1213, pv_1213, efl_1213, efv_1213, t1adl_1213, t1adv_1213, adjl_1213, adjv_1213, dif_1213, V_1213)


#Create the final dataset joining the matrices of each season
newpbp=rbind(t34, t45, t56, t67, t78, t89, t910, t1011, t1112, t1213)
#Turn into data frame and rename the columns
newpbp=as.data.frame(newpbp)
colnames(newpbp)=c("%2 home", "%2 away", "%3 home", "%3 away", "%1 home", "%1 away", "fouls home", "fouls away", 
                   "offreb home", "offreb away", "defreb home", "defreb away", "to home", "to away", "effective home", 
                   "effective away", "ft ahead home", "ft ahead away", "off eff home", "off eff away", "difference", "winner")
#Set the type of the variables
newpbp$`fouls home`=as.numeric(newpbp$`fouls home`)
newpbp$`fouls away`=as.numeric(newpbp$`fouls away`)
newpbp$`offreb home`=as.numeric(newpbp$`offreb home`)
newpbp$`offreb away`=as.numeric(newpbp$`offreb away`)
newpbp$`defreb home`=as.numeric(newpbp$`defreb home`)
newpbp$`defreb away`=as.numeric(newpbp$`defreb away`)
newpbp$`to home`=as.numeric(newpbp$`to home`)
newpbp$`to away`=as.numeric(newpbp$`to away`)
newpbp$`effective home`=as.numeric(newpbp$`effective home`)
newpbp$`effective away`=as.numeric(newpbp$`effective away`)
newpbp$`%2 home`=as.numeric(newpbp$`%2 home`)
newpbp$`%2 away`=as.numeric(newpbp$`%2 away`)
newpbp$`%3 home`=as.numeric(newpbp$`%3 home`)
newpbp$`%3 away`=as.numeric(newpbp$`%3 away`)
newpbp$`%1 home`=as.numeric(newpbp$`%1 home`)
newpbp$`%1 away`=as.numeric(newpbp$`%1 away`)
newpbp$`ft ahead home`=as.numeric(newpbp$`ft ahead home`)
newpbp$`ft ahead away`=as.numeric(newpbp$`ft ahead away`)
newpbp$`off eff home`=as.numeric(newpbp$`off eff home`)
newpbp$`off eff away`=as.numeric(newpbp$`off eff away`)
newpbp$`difference`=as.numeric(newpbp$`difference`)


## -------------------------------------------------------------------------------------------------------------------------------------
#See the proportion of matches that end up in a tie
sum(newpbp$winner=="E")/3110

#Set the decimal places
newpbp=newpbp %>% mutate(across(where(is.numeric), ~ round(., 3)))
#Eliminate the games ending up in a tie
newpbp=newpbp[-(which(newpbp$winner=="E")),]
#Set the response as a factor
newpbp$winner=as.factor(newpbp$winner)


## -------------------------------------------------------------------------------------------------------------------------------------
#DISTRIBUTION AND CORRELATION OF THE VARIABLES


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`offreb home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`offreb away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Offensive rebound distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=20, y=0.06, label="Home", fill="aquamarine")+annotate("label", x=20, y=0.05, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`defreb home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`defreb away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Defensive rebound distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=35, y=0.06, label="Home", fill="aquamarine")+annotate("label", x=35, y=0.052, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`%2 home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`%2 away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Two point field goals distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=75, y=0.03, label="Home", fill="aquamarine")+annotate("label", x=75, y=0.026, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`%3 home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`%3 away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Three point field goals distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=75, y=0.02, label="Home", fill="aquamarine")+annotate("label", x=75, y=0.017, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`%1 home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`%1 away`), alpha=0.25, color="darkred", fill="orange")+labs(title = " Free throw distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=40, y=0.02, label="Home", fill="aquamarine")+annotate("label", x=40, y=0.017, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`fouls home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`fouls away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Fouls distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=35, y=0.06, label="Home", fill="aquamarine")+annotate("label", x=35, y=0.05, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`to home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`to away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Turnovers distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=25, y=0.06, label="Home", fill="aquamarine")+annotate("label", x=25, y=0.05, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`effective home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`effective away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Effective field goal percentage distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=80, y=0.04, label="Home", fill="aquamarine")+annotate("label", x=80, y=0.035, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`ft ahead home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`ft ahead away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Free throws ahead distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=30, y=0.06, label="Home", fill="aquamarine")+annotate("label", x=30, y=0.05, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`off eff home`))+geom_density(alpha=0.25, color="darkblue", fill="aquamarine")+geom_density(aes(x=`off eff away`), alpha=0.25, color="darkred", fill="orange")+labs(title = "Offensive efficiency distribution")+theme_classic()+theme(plot.title = element_text(hjust = 0.5))+ annotate("label", x=150, y=0.02, label="Home", fill="aquamarine")+annotate("label", x=150, y=0.017, label="Away", fill="orange")


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot(newpbp, aes(x=`difference`, fill=`winner`))+geom_density(alpha=0.25)


## -------------------------------------------------------------------------------------------------------------------------------------
kable(table(newpbp$winner))


## -------------------------------------------------------------------------------------------------------------------------------------
corr <- round(cor(newpbp[,-22]), 1)
ggcorrplot(corr, hc.order = TRUE,
   outline.col = "black",
   ggtheme = ggplot2::theme_bw,
   colors = c("#6D9EC1", "white", "#E46726"))


## -------------------------------------------------------------------------------------------------------------------------------------
#TRAIN-TEST PARTITION


## ---- warning=FALSE-------------------------------------------------------------------------------------------------------------------
set.seed(73)
part=createDataPartition(newpbp$winner, p=0.8, list = FALSE)
train=newpbp[part,]
test=newpbp[-part,]


## -------------------------------------------------------------------------------------------------------------------------------------
#LOGISTIC REGRESSION


## ---- warning=FALSE-------------------------------------------------------------------------------------------------------------------
#Model with all the variables
modall=glm(winner~., data=train, family = binomial)
summary(modall)
#AIC, BIC and deviance explained
AIC(modall)
BIC(modall)
100*(modall$null.deviance-modall$deviance)/modall$null.deviance


## ---- warning=FALSE-------------------------------------------------------------------------------------------------------------------
#Stepwise selection
stepAIC(modall, direction="both", trace=FALSE)
modstep=glm(formula = winner ~ `%2 home` + `%3 home` + `offreb home` + 
    `offreb away` + `defreb home` + `defreb away` + `to home` + 
    `to away` + `effective away` + `ft ahead home` + `ft ahead away` + 
    `off eff home` + `off eff away`, family = binomial, data = train)
#AIC, BIC and deviance explained
AIC(modstep)
BIC(modstep)
100*(modstep$null.deviance-modstep$deviance)/modstep$null.deviance
summary(modstep)


## -------------------------------------------------------------------------------------------------------------------------------------
#Compare the models
#Anova
anova(modstep, modall, test="Chisq")

#lrt
lrt=2*(logLik(modall)-logLik(modstep))
df=modstep$df.residual-modall$df.residual
pchisq(lrt,df, lower.tail = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------
#Assumptions

#Durbin-Watson test
dwtest(modstep)
durbinWatsonTest(modstep)

#Diagnostic plots
glm.diag.plots(modstep)


## -------------------------------------------------------------------------------------------------------------------------------------
#Lineality and normality
par(mfrow=c(1,2))
plot(train$`%2 away`,qres.binom(modstep), xlab="%2",ylab="Quantile residuals")
qqnorm(qres.binom(modstep))
abline(a=0,b=1,col="red")


## -------------------------------------------------------------------------------------------------------------------------------------
#Hosmer-Lemeshow goodnes of fit test
hoslem.test(ifelse(train$winner=="1", 1, 0), predict(modstep,type="response"))


## -------------------------------------------------------------------------------------------------------------------------------------
#Predicted probabilities
prob=predict(modstep, newdata=test, type="response")
head(prob)
#Label the predicted probabilities
prediction = factor(ifelse(prob>0.5, 1, 0))
#Accuracy
t=confusionMatrix(prediction, test$winner)$table
t
sum(diag(t))/nrow(test)


## -------------------------------------------------------------------------------------------------------------------------------------
#ROC curves
#The ROC function
ROC(form=winner ~ `%2 home` + `%3 home` + `offreb home` + `offreb away` + `defreb home` + `defreb away` + `to home` + `to away` +
    `effective away` + `ft ahead home` + `ft ahead away` + `off eff home` + 
    `off eff away`, data=train, plot="ROC",lwd=3,cex=1.5)


## -------------------------------------------------------------------------------------------------------------------------------------
#Generate predicted probabilities
predicted_probabilities <- predict(modstep, newdata = test, type = "response")

#Create a ROC curve from the predicted probabilities
roc_curve <- roc(response = test$winner, predictor = predicted_probabilities)

#Plot the ROC curve
plot(roc_curve)


## -------------------------------------------------------------------------------------------------------------------------------------
#BAYESIAN ADAPTIVE SAMPLING


## -------------------------------------------------------------------------------------------------------------------------------------
#Fit the model
aa <- bas.glm(winner ~ .,
  family=binomial,
  data = train,
  betaprior = beta.prime(n=NULL),
  modelprior = beta.binomial(1,1), initprobs = "Uniform", method="BAS", n.models = 25000)
#Save it
save(aa, file="aa.RData")


## -------------------------------------------------------------------------------------------------------------------------------------
#Load the model
load(file="aa.RData")
#Plot graphical summaries of the model
plot(aa, ask=F)
#Marginal posterior inclusion probabilities
plot(aa, which =4, ask = FALSE, caption = "", sub.caption = "")


## -------------------------------------------------------------------------------------------------------------------------------------
#Summary of the model
aa
options(width = 80)
summary(aa)


## -------------------------------------------------------------------------------------------------------------------------------------
#Best 20 models
image(aa)


## -------------------------------------------------------------------------------------------------------------------------------------
#Model coefficients
coef.aa <- coef(aa)
#Plots of the posterior distributions averaging over all of the models
plot(coef.aa, ask = F)
#Credible intervals for the coefficients
confint(coef.aa)
plot(confint(coef.aa, parm = 2:22))


## -------------------------------------------------------------------------------------------------------------------------------------
#Credible intervals using different estimators
plot(confint(coef(aa, estimator = "BMA"), parm=2:22))
plot(confint(coef(aa, estimator = "HPM"), parm=2:22))
plot(confint(coef(aa, estimator = "MPL"), parm=2:22))


## -------------------------------------------------------------------------------------------------------------------------------------
#Predict using BMA
BMA <- predict(aa, estimator = "BMA", newdata=test)

#Best models and variables included in the models
names(BMA)
BMA$bestmodel[1:10]
BMA$best.vars
variable.names(BMA)


## -------------------------------------------------------------------------------------------------------------------------------------
#Predict using BPM estimator
BPM <- predict(aa, estimator = "BPM", newdata=test)
#Variables included in the best model
BPM$bestmodel
variable.names(BPM)


## -------------------------------------------------------------------------------------------------------------------------------------
#Predicted probabilities
new.pred <- predict(aa, newdata = test, estimator = "BPM", type="response")
#Label the predicted probabilities
newprediction = factor(ifelse(new.pred$fit>0.5, 1, 0))
#Accuracy
newt=confusionMatrix(newprediction, test$winner)$table
newt
sum(diag(newt))/nrow(test)


## -------------------------------------------------------------------------------------------------------------------------------------
#PROJECTION PREDICTIVE FEATURE SELECTION


## -------------------------------------------------------------------------------------------------------------------------------------
#Rename the columns because rstanarm does not handle well the spaces of the variables' names

colnames(train)=c("twohome", "twoaway", "threehome", "threeaway", "fthome", "ftaway", "foulshome", "foulsaway", "offrebhome", "offrebaway", "defrebhome", "defrebaway", "tohome", "toaway", "effectivehome", "effectiveaway", "ftaheadhome", "ftaheadaway", "offeffhome", "offeffaway", "difference", "winner")
colnames(test)=c("twohome", "twoaway", "threehome", "threeaway", "fthome", "ftaway", "foulshome", "foulsaway", "offrebhome", "offrebaway", "defrebhome", "defrebaway", "tohome", "toaway", "effectivehome", "effectiveaway", "ftaheadhome", "ftaheadaway", "offeffhome", "offeffaway", "difference", "winner")

#Fit the model
modstanarm=stan_glm(winner~., data=train, family=binomial(link="logit"), prior=normal(0,1), prior_intercept = student_t(df = 7, location = 0, scale = 2.5), seed=73)
summary(modstanarm)
round(posterior_interval(modstanarm, prob = 0.95), 2)


## -------------------------------------------------------------------------------------------------------------------------------------
#95% posterior intervals
mcmc_areas(as.matrix(modstanarm), prob_outer = .95)
mcmc_areas(as.matrix(modstanarm), prob_outer = .95, pars=c("(Intercept)"))
mcmc_areas(as.matrix(modstanarm), prob_outer = .95, pars=c("twohome", "twoaway", "threehome", "threeaway", "fthome", "ftaway", "foulshome"))
mcmc_areas(as.matrix(modstanarm), prob_outer = .95, pars=c("foulsaway", "offrebhome", "offrebaway", "defrebhome", "defrebaway", "tohome", "toaway"))
mcmc_areas(as.matrix(modstanarm), prob_outer = .95, pars=c("effectivehome", "effectiveaway", "ftaheadhome", "ftaheadaway", "offeffhome", "offeffaway", "difference"))


## -------------------------------------------------------------------------------------------------------------------------------------
#Null model for comparison
modnull <- stan_glm(winner ~ 1, family=binomial(link="logit"), data = train, seed=73)


## -------------------------------------------------------------------------------------------------------------------------------------
#Comparison of both models via leave-one-out cross-validation
(lootot <- loo(modstanarm, k_threshold = 0.7, save_psis = TRUE))
(loo0 <- loo(modnull, save_psis = TRUE))
loo_compare(loo0, lootot)


## -------------------------------------------------------------------------------------------------------------------------------------
#Variable selection via 5-fold CV.
cvvs1 <- cv_varsel(
  modstanarm,
  validate_search = TRUE,
  ###
  nterms_max = 22,
  seed = 73,
  cv_method='kfold'
)
#Save and load it
save(cvvs1, file="cvvs1.RData")
load(file="cvvs1.RData")


## -------------------------------------------------------------------------------------------------------------------------------------
#Plot performance statistics based on submodel size
plotcvvs1=plot(cvvs1, stats = c("elpd", "acc", "auc"), deltas = TRUE, seed = 73)
save(plotcvvs1, file="plotcvvs1.RData")
load(file="plotcvvs1.RData")
plotcvvs1


## -------------------------------------------------------------------------------------------------------------------------------------
#Suggested size for each performance statistic
suggest_size(cvvs1, stat = "acc", seed = 73)
suggest_size(cvvs1, stat = "auc", seed = 73)
suggest_size(cvvs1, stat = "elpd", seed = 73)


## -------------------------------------------------------------------------------------------------------------------------------------
#Summary of the plot of the performance statistics
print(summary(cvvs1, stats = c("elpd", "acc", "auc"), deltas = TRUE, seed = 73),
      digits = 1)


## -------------------------------------------------------------------------------------------------------------------------------------
#Predictor solution path for the variable selection
(soltrms <- solution_terms(cvvs1))
#First four selected variables
(soltrms_final <- head(soltrms, 4))


## -------------------------------------------------------------------------------------------------------------------------------------
#Deterministic projection of the reference model’s posterior distribution onto the parameter space of the final submodel.
prj <- project(modstanarm, solution_terms = soltrms_final)
prj_mat <- as.matrix(prj)

#Posterior intervals of the selected variables.
round(colMeans(prj_mat),1)
round(posterior_interval(prj_mat),1)
mcmc_areas(prj_mat, pars = "(Intercept)")
mcmc_areas(prj_mat, pars = c("ftaheadaway", "ftaheadhome", "offeffaway", "offeffhome"))


## -------------------------------------------------------------------------------------------------------------------------------------
#The projection as draws matrix to get summary statistics
prj_drws <- as_draws_matrix(prj_mat)
prj_smmry <- summarize_draws(
  prj_drws,
  "median", "mad", function(x) quantile(x, probs = c(0.025, 0.975))
)
#Coerce to a data frame
prj_smmry <- as.data.frame(prj_smmry)
print(prj_smmry, digits = 1)


## -------------------------------------------------------------------------------------------------------------------------------------
#Marginal distribution of the projected posterior
bayesplot_theme_set(ggplot2::theme_bw())
mcmc_intervals(prj_mat) +
  ggplot2::coord_cartesian(xlim = c(-3, 3))


## -------------------------------------------------------------------------------------------------------------------------------------
#Marginal distribution of the reference model for comparision
refm_mat <- as.matrix(modstanarm)
mcmc_intervals(refm_mat, pars = colnames(prj_mat)) +
  ggplot2::coord_cartesian(xlim = c(-3, 3))


## -------------------------------------------------------------------------------------------------------------------------------------
#Probabilities & prediction
prob_linpred <- proj_linpred(prj, newdata = test, integrated = TRUE, transform = TRUE)$pred
linpred=factor(ifelse(prob_linpred>=0.5, 1, 0))
#Accuracy
ttlin=confusionMatrix(linpred, test$winner)$table
ttlin
sum(diag(ttlin))/nrow(test)



