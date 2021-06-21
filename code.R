#install.packages("GGally")
library("GGally")
library(ggplot2)
library(aplpack)

###############################################################3

football<-read.csv("~/Football/英超.csv",encoding = 'UTF8')

library(GGally)
library(ggplot2)
ggparcoord(football,columns=c(2,8,9,10,11,12,13,14,19,34),groupColumn=5)+scale_color_brewer(palette="Accent")

foot_qian<-subset(football,位置=='前锋')
ggparcoord(foot_qian,columns=c(2,8,9,10,11,12,13,14,19,34),groupColumn=8)+scale_color_gradient(low='yellow',high='blue')+theme_light()
#good1

#1 散点图矩阵
ggpairs(football[,c(2,6,7,8,9,10,11,12,13,14,18,19,34)])
ggscatmat(football[,c(2,6,7,8,9,10,11,12,13,14,18,19,34)])
pairs(football[,c(2,6,7,8,9,10,11,12,13,14,18,19,34)])

ggscatmat(football[,c(2,6,9,10,11,12,14,18,19,20,34)])


#研究实际问题：下一年进球34数量与什么有关
#球员属性 2年龄 5位置
#出场数据 6出场-13抢断 18射门-20射门成功率
#犯规情况 14越位-17黄牌
#进球方式 21头球进球-26赢得点球机会
#防守数据 27拦截-30后场解围
#头球争顶 31、32

# 球员进球数~出场概况：包括出场、首发、出场时间、传球、抢断
ggpairs(football[,c(6:8,11,13,9,34)])

## 球员进球数~进攻数据 包括进球、下一年进球、助攻、过人、射门、射正、射门成功率、头球争顶成功。
ggpairs(football[,c(10,12,18:20,31,9,34)])
library(corrgram)
corrgram(football[,c(10,12,18:19,9,34)],upper.panel=panel.pie)
ggcorr(football[,c(10,12,18:19,9,34)],label = TRUE,method=c("pairwise", "kendall"))
## 球员进球数~犯规情况 包括越位、犯规、红牌、黄牌
ggpairs(football[,c(14:17,9,34)])
ggcorr(football[,c(14:17,9,34)],label = TRUE,method=c("pairwise", "kendall"))
## 球员进球数~进球方式：包括头球进球、左脚进球、右脚进球、直接任意球进球、点球、赢得点球机会。
ggpairs(football[,c(21:26,9,34)])
## 球员进球数~防守数据：包括拦截、解围、头球解围、后场解围。
ggpairs(football[,c(27:30,9,34)])
#1
boxplot(下一年进球~位置,data=football)
boxplot(进球~位置,data=football)
ggpairs(football[,c(2,7:13,34)])

ft_forward<-subset(football,位置=='前锋')
ft_back<-subset(football,位置=='后卫')
ft_center<-subset(football,位置=='中场')
ggpairs(ft_forward[,c(2,7,8,10:13,9,34)])

ggpairs(football[,c(18:20,9,34)])
ggpairs(ft_forward[,c(18:20,9,34)])
#2
ggpairs(football[,c(14:17,9,34)])
ggpairs(ft_forward[,c(14:17,9,34)])
#3
ggpairs(ft_forward[,c(21:26,9,34)])
ggpairs(ft_forward[,c(27:31,9,34)])


# 2年龄 12过人 9进球 18射门 19射正 26获得点球机会
ggparcoord(ft_forward,columns=c(2,12,18,19,26,9,34),groupColumn=2)+scale_color_gradient(low='orange',high='blue')+theme_light()
ggparcoord(ft_forward,columns=c(2,12,18,19,26,9,34))
ggparcoord(football,columns=c(2,12,18,19,9,34),groupColumn=5)+scale_color_brewer(palette="Accent")
ggpairs(ft_forward[,c(2,12,18,19,26,9,34)])
ggpairs(football[,c(2,12,18,19,26,9,34)])
library(aplpack)
faces(ft_forward[,c(2,6:31,34)],face.type=0)
faces(ft_back[,c(2,12,18,19,9,34)],face.type=0)
faces(ft_center[,c(2,12,18,19,9,34)],face.type=0)





faces(ft_forward[,c(9,34,2,14,13,12,8,11,18,19,26,29,31,27,28)],face.type=0)
faces(ft_center[,c(9,34,2,14,13,12,8,11,18,19,26,29,31,27,28)],face.type=0)
faces(ft_back[,c(9,34,2,14,13,12,8,11,18,19,26,29,31,27,28)],face.type=0)
#可以挑30个人来画、放在前面——研究进球重点研究前锋


```{r echo=FALSE,message=FALSE,fig.width=6.5,fig.height=4.3}
faces(ft_forward[,c(9,34,2,14,13,12,8,11,18,19,26,29,31,27,28)],face.type=0,print.info=FALSE)
```


```{r echo=FALSE,message=FALSE,fig.width=6.5,fig.height=4.3}
set.seed(1234)
faces(ft_center[sample(1:nrow(ft_center),30),c(9,34,2,14,13,12,8,11,18,19,26,29,31,27,28)],face.type=0,print.info=FALSE)
```

```{r echo=FALSE,message=FALSE,fig.width=6.5,fig.height=4，3}
set.seed(1234)
faces(ft_back[sample(1:nrow(ft_back),30),c(9,34,2,14,13,12,8,11,18,19,26,29,31,27,28)],face.type=0,print.info=FALSE)
```
##年龄、助攻、过人、射门、射正、进球、左脚进球、右脚进球和获得点球机会

ggparcoord(foot_qian,columns=c(2,10,12,18,19,9,22,23,26,34),groupColumn=34)+scale_color_gradient(low='yellow',high='purple')+theme_light()+geom_line(size=2)
