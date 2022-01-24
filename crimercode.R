########## 1.요인분석
#어떤 문항끼리 묶이는지 확인
#1차 탐색적 요인분석
reg<-read.csv("C:/Users/user/Desktop/법무부/사회적응데이터.csv",head=TRUE)
attach(reg)
f1<-c("V19_1","V19_2","V19_3","V19_4","V19_5","V19_6","V19_7","V19_8","V19_9",
      "V20_1","V20_2","V20_3","V20_9","V20_10",
      "V21_1","V21_2","V21_3","V21_4","V21_5","V21_6","V21_7","V21_8",
      "V22_1","V22_2","V22_3","V22_4","V22_6","V22_7","V22_8",
      "V23_2","V23_3","V23_4","V23_5","V23_6","V23_7","V23_8","V23_9",
      "V24_3","V24_4",
      "V25_1","V25_2","V25_3","V25_4","V25_5","V25_6","V25_7",
      "V26_1","V26_3","V26_4","V26_5","V26_6","V26_7","V26_8","V26_10")
f_1<-reg[f1]

#구형성 검정
library(psych)
#bartlett검정
cortest.bartlett(R=cor(f_1[,]), n=nrow(f_1))

#KMO검정
KMO(f_1) 

#주성분분석+promax회전방법
fac_1<-principal(f_1,rotate="none")
fac_1$values
print(principal(f_1,10,rotate="promax"),digits=2)

#가족관계 문항 역코딩
reg$V22_6_re[V22_6==1]<-4
reg$V22_6_re[V22_6==2]<-3
reg$V22_6_re[V22_6==3]<-2
reg$V22_6_re[V22_6==4]<-1

reg$V22_7_re[V22_7==1]<-4
reg$V22_7_re[V22_7==2]<-3
reg$V22_7_re[V22_7==3]<-2
reg$V22_7_re[V22_7==4]<-1

reg$V22_8_re[V22_8==1]<-4
reg$V22_8_re[V22_8==2]<-3
reg$V22_8_re[V22_8==3]<-2
reg$V22_8_re[V22_8==4]<-1

#2차 탐색적 요인분석 
f2<-c("V19_1","V19_2","V19_3","V19_4","V19_5","V19_6","V19_7","V19_8","V19_9",
      "V20_2","V20_3","V20_9","V20_10",
      "V21_1","V21_2","V21_3","V21_4","V21_5","V21_6","V21_7","V21_8",
      "V22_1","V22_2","V22_3","V22_4","V22_6_re","V22_7_re","V22_8_re",
      "V23_2","V23_3","V23_4","V23_5","V23_6","V23_7","V23_8","V23_9",
      "V24_3","V24_4",
      "V25_1","V25_2","V25_3","V25_4","V25_5","V25_6","V25_7",
      "V26_1","V26_3","V26_4","V26_5","V26_6","V26_7","V26_8","V26_10")
f_2<-reg[f2]

########## 2.구형성 검정
#bartlett검정
cortest.bartlett(R=cor(f_2[,]), n=nrow(f_2))
#KMO검정
KMO(f_2) 

#주성분분석+promax회전방법
fac_2<-principal(f_2,rotate="none")
fac_2$values
print(principal(f_2,10,rotate="promax"),digits=2)


## 내적일치도 확인
#문항 중 삭제할만한 문항이 있는지 확인 by Cronbach's Alpha Coefficient
library(psych)
ss<-read.csv("C:/Users/user/Desktop/법무부/사회적응데이터.csv",head=TRUE)
attach(ss)

#[회복탄력성-통제성]문항
s1<-c("V20_1","V20_2","V20_3","V20_4")
pcas1<-ss[s1]
alpha(pcas1,na.rm=TRUE)

#[회복탄력성-긍정성]문항
s3<-c("V20_5","V20_6")
pcas3<-ss[s3]
alpha(pcas3,na.rm=TRUE,check.keys=TRUE)

#[회복탄력성-사회성]문항
s4<-c("V20_9","V20_10")
pcas4<-ss[s4]
alpha(pcas4,na.rm=TRUE)

#[회복탄력성]문항
s134<-c("V20_1","V20_2","V20_3","V20_4","V20_5","V20_6","V20_9","V20_10")
pcas134<-ss[s134]
alpha(pcas134,na.rm=TRUE,check.keys=TRUE)

#[직업적 자기효능감]문항
s_24<-c("V24_3","V24_4")
pcas_24<-ss[s_24]
alpha(pcas_24,na.rm=TRUE,check.keys=TRUE)


#[회복탄련성+긍정적 분노대응]문항
s12<-c("V19_6","V19_7","V19_8","V19_9","V20_1","V20_2","V20_3")
pcas12<-ss[s12]
alpha(pcas12,na.rm=TRUE)


########## 3.구성타당도
library(lavaan)
reg<-read.csv("C:/Users/user/Desktop/법무부/사회적응데이터.csv",head=TRUE)
attach(reg)

#모형3 적합도 검정(전체문항을 하위요인10개로 묶어 하나의 모델로 적합)
model_3<-"
      q19_anger_negative=~ 1*V19_1+V19_2+V19_3+V19_4+V19_5                                        
      q19_anger_positive=~ 1*V19_6+V19_7+V19_8+V19_9                             
      q20_recover=~ 1*V20_2+V20_3+V20_9+V20_10                                   
      q21_confi_posi=~ 1*V21_1+V21_2+V21_3+V21_4                                
      q21_confi_nega=~ 1*V21_5+V21_6+V21_7+V21_8                    
      q22_family_trust=~ 1*V22_1+V22_2+V22_3+V22_4
      q22_family_pred=~ 1*V22_6_re+V22_7_re+V22_8_re       
      q23_support=~ 1*V23_2+V23_3+V23_4+V23_5+V23_6+V23_7+V23_8+V23_9            
      q24_job=~ 1*V24_3+V24_4                                          
      q25_cognition=~ 1*V25_1+V25_2+V25_3+V25_4+V25_5+V25_6+V25_7  
      
      #factor variance
      q19_anger_negative~~ q19_anger_positive+q20_recover+
      q21_confi_posi+q21_confi_nega+q22_family_trust+q22_family_pred+q23_support+q24_job+
      q25_cognition
      
      q19_anger_positive~~ q20_recover+
      q21_confi_posi+q21_confi_nega+q22_family_trust+q22_family_pred+q23_support+q24_job+
      q25_cognition
      
      q20_recover~~ q21_confi_posi+q21_confi_nega+q22_family_trust+q22_family_pred+q23_support+q24_job+
      q25_cognition
      
      q21_confi_posi~~ q21_confi_nega+q22_family_trust+q22_family_pred+q23_support+q24_job+
      q25_cognition
      
      q21_confi_nega~~ q22_family_trust+q22_family_pred+q23_support+q24_job+
      q25_cognition
      
      q22_family_trust~~ q22_family_pred+q23_support+q24_job+
      q25_cognition
      
      q22_family_pred~~ q23_support+q24_job+
      q25_cognition
      
      q23_support~~ q24_job+
      q25_cognition
      
      q24_job~~ q25_cognition"

#Test statistic(χ2), Degrees of freedom(df), CFI, TLI, AIC, RMSEA 확인
fit_3<-lavaan(model_3,data=reg,auto.var=TRUE)
summary(fit_3,fit.measures=TRUE)

##수렴타당도
#비표준화계수, S.E
summary(fit_3,fit.measures=TRUE)

#표준화계수
Est<-parameterEstimates(fit_3, ci = FALSE, standardized = TRUE)
subset(Est, op == "=~")

#subset()함수의 std.all값 (외관 관측변수의 표준화된 요인부하량)
q19_anger_negative_a <- c(0.748,0.637,0.734,0.752,0.625)
q19_anger_positive_a <- c(0.703,0.672,0.766,0.75)
q20_recover_a <- c(0.651,0.615,0.701,0.709)
q21_confi_negative_a <- c(0.755,0.711,0.737,0.752)
q21_confi_positive_a <- c(0.732,0.683,0.84,0.664)
q22_family_trust_a <- c(0.842,0.865,0.886,0.823)
q22_family_pred_a <- c(0.697,0.817,0.737)
q23_support_a <- c(0.744,0.787,0.788,0.755,0.83,0.804,0.828,0.772)
q25_cognition_a <- c(0.617,0.408,0.701,0.811,0.918,0.903,0.834)
q24_job_a <- c(0.877,0.907)

#위 summary()함수의 Variances의 Estimate 값(외관 관측변수의 측정오차)
q19_anger_negative_a_e <-c(0.230,0.386,0.225,0.251,0.415)    
q19_anger_positive_a_e <-c(0.265,0.282,0.195,0.203)
q20_recover_a_e <-c(0.256,0.280,0.247,0.216)
q21_confi_negative_a_e <-c(0.229,0.244,0.205,0.211)
q21_confi_positive_a_e <-c(0.335,0.365,0.175,0.322)
q22_family_trust_a_e <-c(0.213,0.172,0.149,0.208)
q22_family_pred_a_e <-c(0.322,0.212,0.308)
q23_support_a_e <-c(0.226,0.182,0.180,0.247,0.160,0.162,0.139,0.198)
q24_job_a_e <-c(0.095,0.075)
q25_cognition_a_e <-c(0.408,0.627,0.303,0.183,0.093,0.109,0.203)

#CR구하는 식
sum(q19_anger_negative_a)^2/(sum(q19_anger_negative_a)^2+sum(q19_anger_negative_a_e))  
sum(q19_anger_positive_a)^2/(sum(q19_anger_positive_a)^2+sum(q19_anger_positive_a_e)) 
sum(q20_recover_a)^2/(sum(q20_recover_a)^2+sum(q20_recover_a_e)) 
sum(q21_confi_negative_a)^2/(sum(q21_confi_negative_a)^2+sum(q21_confi_negative_a_e)) 
sum(q21_confi_positive_a)^2/(sum(q21_confi_positive_a)^2+sum(q21_confi_positive_a_e)) 
sum(q22_family_trust_a)^2/(sum(q22_family_trust_a)^2+sum(q22_family_trust_a_e)) 
sum(q22_family_pred_a)^2/(sum(q22_family_pred_a)^2+sum(q22_family_pred_a_e)) 
sum(q23_support_a)^2/(sum(q23_support_a)^2+sum(q23_support_a_e)) 
sum(q24_job_a)^2/(sum(q24_job_a)^2+sum(q24_job_a_e)) 
sum(q25_cognition_a)^2/(sum(q25_cognition_a)^2+sum(q25_cognition_a_e)) 

#평균분산추출(AVE)
sum(q19_anger_negative_a^2)/(sum(q19_anger_negative_a^2)+sum(q19_anger_negative_a_e))
sum(q19_anger_positive_a^2)/(sum(q19_anger_positive_a^2)+sum(q19_anger_positive_a_e))
sum(q20_recover_a^2)/(sum(q20_recover_a^2)+sum(q20_recover_a_e))
sum(q21_confi_positive_a^2)/(sum(q21_confi_positive_a^2)+sum(q21_confi_positive_a_e))
sum(q21_confi_negative_a^2)/(sum(q21_confi_negative_a^2)+sum(q21_confi_negative_a_e))
sum(q22_family_trust_a^2)/(sum(q22_family_trust_a^2)+sum(q22_family_trust_a_e))
sum(q22_family_pred_a^2)/(sum(q22_family_pred_a^2)+sum(q22_family_pred_a_e))
sum(q23_support_a^2)/(sum(q23_support_a^2)+sum(q23_support_a_e))
sum(q24_job_a^2)/(sum(q24_job_a^2)+sum(q24_job_a_e))
sum(q25_cognition_a^2)/(sum(q25_cognition_a^2)+sum(q25_cognition_a_e))

##상관계수구하기
#문항별 척도로 묶기
reg$AVG19_anger_nega<-(V19_1+V19_2+V19_3+V19_4+V19_5)/5
reg$AVG19_anger_posi<-(V19_6+V19_7+V19_8+V19_9)/4
reg$AVG20_recover<-(V20_2+V20_3+V20_9+V20_10)/4
reg$AVG21_confi_posi<-(V21_1+V21_2+V21_3+V21_4)/4
reg$AVG21_confi_nega<-(V21_6+V21_7+V21_8)/3
reg$AVG22_family_trust<-(V22_1+V22_2+V22_3+V22_4)/4
reg$AVG22_family_pred<-(V22_6_re+V22_7_re+V22_8_re)/3
reg$AVG23_support<-(V23_2+V23_3+V23_4+V23_5+V23_6+V23_7+V23_8+V23_9)/8
reg$AVG24_job<-(V24_3+V24_4)/2
reg$AVG25_cognition<-(V25_1+V25_2+V25_3+V25_5+V25_6+V25_7)/6


########## 4.로지스틱 회귀분석
crime2<-read.csv("C:/Users/user/Desktop/법무부/걍 이 데이터쓰세요.csv",head=TRUE)
#train,test 나누기
set.seed(500) #난수 초기값 고정
trainIndex=createDataPartition(crime2$상습, p=0.8)$Resample1
train=crime2[trainIndex, ]
test=crime2[-trainIndex, ]
nrow(train)
nrow(test)
reg<-glm(상습~factor(V1)+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V27+V29+V30+
             부정적분노대응+긍정적분노대응+회복탄력성+부정적자기개념+긍정적자기개념+낙인인식+가족믿음+가족예측성+직업적자기효능감+
             음주빈도+음주문제,family = "binomial",data=train)
#변수선택방법: 후진제거법
step(reg,direction = "backward")


reg_re<-glm(상습~factor(V1)+V2+V8+V9+V10+V12+V16+V18+V29+
                낙인인식+가족믿음+가족예측성
              ,family = "binomial",data=train)
Probability = predict(reg_re, newdata = test, type = 'response')
PREDICTED_C = ifelse(Probability > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)
confusionMatrix(as.factor(test$상습),PREDICTED_C)
#F1스코어로 예측력 판단
res=F1_Score(as.factor(test$상습), PREDICTED_C)
print(res)

########## 5.ROC분석
library(pROC)
fit<-glm(상습~factor(V1)+V2+V7+V8+V9+V10+V11+V12+V15+V16+V18+V27+V29+V30+
             부정적분노대응+긍정적분노대응+회복탄력성+부정적자기개념+긍정적자기개념+낙인인식+가족믿음+가족예측성+직업적자기효능감+
             음주빈도+음주문제,family = "binomial",data=train)
train$상습<-as.factor(train$상습)

attach(train)
train$상습_sum<-V1+V2+V7+V8+V9+V10+V11+V12+V15+V16+V18+V27+V29+V30+부정적분노대응+
  긍정적분노대응+회복탄력성+부정적자기개념+긍정적자기개념+낙인인식+가족믿음+가족예측성+직업적자기효능감+음주빈도+음주문제

#가장 위험한 상습변수의 점수 합을 기준
train$상습_15[train$상습_sum>=15]<-1 
train$상습_15[train$상습_sum<15]<-0
train$상습_16[train$상습_sum>=16]<-1 
train$상습_16[train$상습_sum<16]<-0
train$상습_20[train$상습_sum>=20]<-1 
train$상습_20[train$상습_sum<20]<-0
train$상습_17[train$상습_sum>=17]<-1 
train$상습_17[train$상습_sum<17]<-0
train$상습_18[train$상습_sum>=18]<-1 
train$상습_18[train$상습_sum<18]<-0
detach(train)
attach(train)

a<-train[(train$상습==0),] #일반군
b<-train[(train$상습==1),] #집중군
cbind(a$상습,a$상습_sum,b$상습,b$상습_sum)

rocplot<-roc(상습_16~ fitted(fit) ,,levels=c(0,1), data=train, direction=">")
plot.roc(rocplot,legacy.axes = TRUE)
auc(rocplot)
