eco<-read.csv("C:/data/econo.csv",fileEncoding = "euc-kr") #윈도우 파일을 맥에서 가져오기위해 인코딩
# eco<-read.csv("C:/data/econo.csv")
eco
str(eco)
dim(eco)
attach(eco)
eco1<-eco[2:120,c(1,4,7,10,13,16,19,22,25,28,31,34,37)] # 데이터 정제
str(eco1)
dim(eco1)
names(eco1)

#---------------------------------------------------------------------------------------------------------------------------
# 상관관계 +데이터사이언스(VIF)
eco_cor<-eco1[,-c(1,2)] # 독립변수만 추출(데이터 정제)
names(eco_cor)
cor<-cor(eco_cor)
cor # 독립변수간의 상관관계
heatmap(cor) #상관관계 시각화
# +데이터사이언스(VIF) 독립변수
#-------------------------------데이터사이언스---------------------------------------------------------------------------------------
cor^2
vif_eco<-1/(1-cor^2) # 독립변수간의 VIF (기준 10이상을 다중공산성을 띄는 독립변수상태)
boxplot(vif_eco) # 독립변수간의 다중공산성은 뛰지 않음(이상치(outlier)는 10이하 무시)

#---------------------------------------------------------------------------------------------------------------------------
# 선형회귀(다중선형회귀)요약(결정계수=모델의 설명력) +데이터사이언스(MSE,RMSE=>모델 모형평가 / AIC=>모델의 적합도평가 및 필요한 독립변수 지정)

# 설명
# Rsquared= (회귀모형의 설명력), p-value= (평균보다 회귀모델을 사용하는 예측력이 더 낫다) 
#F-stat(평균보다 회귀모델이 F비(몇배) 더 낫다.(좋다)) 
lm_eco<-eco1[,-1] #데이터 정제(종속변수와 독립변수만 추출)
names(lm_eco)
dim(lm_eco)
str(lm_eco)

##분석목적## # 모델의 요약을 통해 설명력을 평가 R-squared(모델설명력)

# 독립변수 유효 유(*~***)/무==p-value 값이 같음 , R-squared(모델설명력), F비, p-value(유효 유/무)
# 예시) # re- 독립변수에 따른 유/무, R^2=0.55, F-stat=28.46배, p-value=유(유효하다)

# 다중선형회귀(모든 독립변수)요약
summary(lm(lm_eco$KOSPI.prop~.,data=lm_eco)) # re-독립변수에 따른 유/무, 0.61, 15.38배, 유

# 다중선형회귀(모든 독립변수)시 유효한 독립변수
#(DAWOO.prop, KOR.ECONO.prop, KOR.BOND.prop, BILL.prop, USA.ECONO.prop, WTI.prop )를 다중선형회귀
summary(lm(lm_eco$KOSPI.prop~lm_eco$DAWOO.prop+lm_eco$KOR.ECONO.prop+lm_eco$BILL.prop+lm_eco$WTI.prop+lm_eco$KOR.BOND.prop+lm_eco$USA.ECONO.prop,data=lm_eco))
# re-독립변수에 따른 유/무, 0.59, 27.63배, 유
# 오히려 전체독립변수를 사용한 모델보다 모델의 설명력이 떨어짐 (0.61>0.59)

# 단순선형회귀
# 유효한 독립변수(p-value 오름차순 =>더 유효한 순서)
summary(lm(lm_eco$KOSPI.prop~lm_eco$DAWOO.prop,data=lm_eco)) # 유***, 0.46, 100.8배, 유
summary(lm(lm_eco$KOSPI.prop~lm_eco$BILL.prop,data=lm_eco)) # 유***, 0.22, 33.88배, 유 
summary(lm(lm_eco$KOSPI.prop~lm_eco$WTI.prop,data=lm_eco)) # 유***, 0.14, 19.5배, 유
summary(lm(lm_eco$KOSPI.prop~lm_eco$KOR.ECONO.prop,data=lm_eco)) # 유***, 0.12, 17.37배, 유
summary(lm(lm_eco$KOSPI.prop~lm_eco$COPPER.prop,data=lm_eco)) # 유**, 0.07, 9배, 유
# 유효치 않은 독립변수
summary(lm(lm_eco$KOSPI.prop~lm_eco$USA_BOND.prop,data=lm_eco)) # 무, 0.003, 0.37배, 무
summary(lm(lm_eco$KOSPI.prop~lm_eco$KOR.BOND.prop,data=lm_eco)) # 무, 0.018, 2.26배, 무 
summary(lm(lm_eco$KOSPI.prop~lm_eco$USA.ECONO.prop,data=lm_eco)) # 무, 0.004, 0.48배, 무
summary(lm(lm_eco$KOSPI.prop~lm_eco$APART.prop,data=lm_eco)) # 무, 0.009, 1.11배, 무
summary(lm(lm_eco$KOSPI.prop~lm_eco$GOLD.prop,data=lm_eco)) # 무, 0.02, 3.1배, 무
summary(lm(lm_eco$KOSPI.prop~lm_eco$KOR.CPI.prop,data=lm_eco)) # 무, 0.004, 0.48배, 무
# 단순선형회귀시 유효한 독립변수(BILL.prop, COPPER.prop, WTI.prop, DAWOO.prop, KOR.ECONO.prop)를 다중선형회귀
summary(lm(lm_eco$KOSPI.prop~lm_eco$COPPER.prop+lm_eco$DAWOO.prop+lm_eco$KOR.ECONO.prop+lm_eco$BILL.prop+lm_eco$WTI.prop,data=lm_eco))
# re-독립변수에 따른 유/무, 0.55, 28.46배, 유
# 오히려 전체독립변수를 사용한 모델보다 모델의 설명력이 떨어짐 (0.61>0.55)

# 다중선형회귀와 단순선형회귀시 겹치는 유효한 독립변수(DAWOO.prop, KOR.ECONO.prop, BILL.prop, WTI.prop)를 다중선형회귀
summary(lm(lm_eco$KOSPI.prop~lm_eco$DAWOO.prop+lm_eco$KOR.ECONO.prop+lm_eco$BILL.prop+lm_eco$WTI.prop,data=lm_eco))
# re-독립변수에 따른 유/무, 0.55, 35.86배, 유
# 오히려 전체독립변수를 사용한 모델보다 모델의 설명력이 떨어짐 (0.61>0.55)

## 선형휘귀모델을 요약하여 나온 결정계수(R^2=R-squared)값(모델의 설명력(적합도))은 
## 모든 독립변수를 포함한 모델의 설명력이 제일 좋았다!! (모든 다중선형의 모델 p-value는 같았다.(유효하다))
## 평균보다 모델을 이용하는 예측하는 F비는 
## 다중선형회귀와 단순선형회귀시 겹치는 유효한 독립변수를 이용한 모델이 가장 높았다.

#----------------------------------------------------------데이터사이언스------------------------------------------------------------
##분석목적## # 모델 평가(평균제곱오차 / 제곱근평균제곱오차)

# MSE(평균제곱오차) / RMSE=sprt(MSE)-> 관찰하기 편해서 이용 +데이터사이언스
# 4가지의 다중선형회귀의 모델을 평가

# 1) 다중선형회귀(모든 독립변수) 요약
# 2) 다중선형회귀(모든 독립변수)시 유효한 독립변수를 다중선형회귀 요약
# 3) 단순선형회귀시 유효한 독립변수를 다중선형회귀 요약
# 4) 다중선형회귀와 단순선형회귀시 겹치는 유효한 독립변수를 다중선형회귀 요약
lm_1<-lm(lm_eco$KOSPI.prop~.,data=lm_eco) # 1
lm_2<-lm(lm_eco$KOSPI.prop~lm_eco$DAWOO.prop+lm_eco$KOR.ECONO.prop+lm_eco$BILL.prop+lm_eco$WTI.prop+lm_eco$KOR.BOND.prop+lm_eco$USA.ECONO.prop,data=lm_eco) # 2
lm_3<-lm(lm_eco$KOSPI.prop~lm_eco$COPPER.prop+lm_eco$DAWOO.prop+lm_eco$KOR.ECONO.prop+lm_eco$BILL.prop+lm_eco$WTI.prop,data=lm_eco) # 3
lm_4<-lm(lm_eco$KOSPI.prop~lm_eco$DAWOO.prop+lm_eco$KOR.ECONO.prop+lm_eco$BILL.prop+lm_eco$WTI.prop,data=lm_eco) # 4
mse_1<-sum(residuals(lm_1)^2)/length(residuals(lm_1)) # MSE-1
mse_2<-sum(residuals(lm_2)^2)/length(residuals(lm_2)) # MSE-2
mse_3<-sum(residuals(lm_3)^2)/length(residuals(lm_3)) # MSE-3
mse_4<-sum(residuals(lm_4)^2)/length(residuals(lm_4)) # MSE-4
sqrt(mse_1) # RMSE-1
sqrt(mse_2) # RMSE-2
sqrt(mse_3) # RMSE-3
sqrt(mse_4) # RMSE-4
# re-모델평가의 지표인 MSE(RMSE)는 모든 독립변수를 이용한 모델이 가장 값이 낮았다

#--------------------------------------------데이터사이언스---------------------------------------------------------------------------
##분석목적## #모형의 성능평가
# AIC 모델성능평가 (값이 작을수록 좋은 모형)
# AIC 요약을하여 지정된 독립변수에서 유효한 변수 확인
# 4가지의 다중선형회귀의 모델을 평가

# 1) 다중선형회귀(모든 독립변수) 요약
# 2) 다중선형회귀(모든 독립변수)시 유효한 독립변수를 다중선형회귀 요약
# 3) 단순선형회귀시 유효한 독립변수를 다중선형회귀 요약
# 4) 다중선형회귀와 단순선형회귀시 겹치는 유효한 독립변수를 다중선형회귀 요약
library(MASS)

# 단계적선택법
aic_1<-stepAIC(lm_1,direction = "both")
summary(aic_1)
# 지정된 변수에서 유효한 변수(BILL.prop, WTI.prop, DAWOO.prop, KOR.BOND.prop, KOR.ECONO.prop)
aic_2<-stepAIC(lm_2,direction = "both")
summary(aic_2)
# 지정된 변수에서 유효한 변수(DAWOO.prop, KOR.ECONO.prop, BILL.prop, KOR.BOND.prop, WTI.prop)
aic_3<-stepAIC(lm_3,direction = "both")
summary(aic_3)
# 지정된 변수에서 유효한 변수(DAWOO.prop, BILL.prop, KOR.ECONO.prop)
aic_4<-stepAIC(lm_4,direction = "both")
summary(aic_4)
# 지정된 변수에서 유효한 변수(DAWOO.prop, BILL.prop, KOR.ECONO.prop)

# 단계적선택법으로 모든 독립변수를 가지고 회귀하여 요약한 설명력(결정계수)이 1)번이 가장 높았다(0.60)
# 그러나 평균보다 모델이용을 하는 F비는 3), 4)번이 값이 같으며, 가장 높았다

#--------------------------------------------------------------데이터 사이언스----------------------------------------------------------
##분석목적## # 모델 평가(평균제곱오차 / 제곱근평균제곱오차)

# MSE(평균제곱오차) / RMSE=sprt(MSE)-> 관찰하기 편해서 이용 +데이터사이언스
# 4가지의 다중선형회귀의 모델을 평가

# 1) 다중선형회귀(모든 독립변수) 요약
# 2) 다중선형회귀(모든 독립변수)시 유효한 독립변수를 다중선형회귀 요약
# 3) 단순선형회귀시 유효한 독립변수를 다중선형회귀 요약
# 4) 다중선형회귀와 단순선형회귀시 겹치는 유효한 독립변수를 다중선형회귀 요약
aic_mse1<-sum(residuals(aic_1)^2)/length(residuals(aic_1))
aic_mse2<-sum(residuals(aic_2)^2)/length(residuals(aic_2))
aic_mse3<-sum(residuals(aic_3)^2)/length(residuals(aic_3))
aic_mse4<-sum(residuals(aic_4)^2)/length(residuals(aic_4))
sqrt(aic_mse1)
sqrt(aic_mse2)
sqrt(aic_mse3)
sqrt(aic_mse4)

# re-모델평가의 지표인 MSE(RMSE)는 모든 독립변수를 이용한 모델이 가장 값이 낮았다