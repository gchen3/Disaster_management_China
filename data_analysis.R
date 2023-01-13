### Data analysis for disaster management project ###
### Use China subnational data ###
### Preliminary results ###

### Library ###
library(foreign)
library(haven)
library(dplyr)

?### Input data ###
disaster_finance <- read_dta("database3.dta")

### Construct variables ###
disaster_finance_cleaned <- disaster_finance %>% 
          group_by(procode, year) %>% 
          filter(year>2016 & year<2021 & procode!=26) %>% 
          mutate (
           
             ##财政状况##
            
            pcrevenue = revenue/population,		#人均财政收入=地方财政收入/总人口#
            pcexpenditure = expenditure/population,		#人均财政支出=地方财政支出/总人口#
            deficit = expenditure-revenue,		 #财政赤字=地方财政支出-地方财政收入#
            pcdeficit = deficit/population,		#人均财政赤字=地方财政赤字/总人口#
            riskdefictgdp = deficit/gdp,		 #财政赤字风险=地方财政赤字/GDP#
            fispressure = deficit/revenue,		 #财政压力=地方财政赤字/地方财政收入#
            fiscapacity = (revenue-expenditure)/revenue, #财政能力=财政盈余/地方财政收入#
            fiscapacityzhi = (revenue-expenditure)/expenditure, #财政能力=财政盈余/地方财政支出#
            pcdebtgov = debtgov/population,		#人均政府债券余额=地方政府债券余额/总人口#
            riskgovgdp = debtgov/gdp,		 #政府债券负债率=地方政府债券余额/GDP#
            riskgovrev = debtgov/revenue,		 #政府债券债务率=地方政府债券余额/地方财政收入#
            
            pcdebtct = debtct/population,		 #人均城投债余额=地方城投债余额/总人口#
            riskctgdp = debtct/gdp,				#城投债负债率=城投债余额/GDP#
            riskctrev = debtct/revenue,				#城投债债务率=城投债余额/地方财政收入#
            
            debtsum = debtgov+debtct,		 #地方政府债务总额=政府债券余额+城投债余额#
            pcdebtsum = debtsum/population,		#人均债务余额=地方政府债务总额/总人口#
            risksumgdp = debtsum/gdp,		 #政府负债率=地方政府债务总额/GDP#
            risksumrev = debtsum/revenue,		 #政府债务率=地方政府债务总额/地方财政收入#
            
            pctransfer = transfer/population,		 #人均转移支付=转移支付/总人口#
            transferdepend = transfer/revenue,		 #转移支付依赖度=转移支付/地方财政收入#
            
            ##自然灾害受灾情况##
            
            #pcloss = disasterloss/population,				#人均自然灾害直接经济损失=自然灾害直接经济损失/总人口#
            #pcloss3 =(L.pcloss+L2.pcloss+L3.pcloss)/3,				 #过去3年人均自然灾害直接经济损失的均值#
            #pcloss5 =(L.pcloss+L2.pcloss+L3.pcloss+L4.pcloss+L5.pcloss)/5 #过去5年人均自然灾害直接经济损失的均值#,		,		 
            
            ##财政分权变量情况##
            
            fd = revenue/expenditure,		 #财政自主度= 地方财政收入/地方财政支出# 
            fdshou = revenue/revenuecentral,		 #财政收入分权= 地方财政收入/中央财政收入#
            fdzhi = expenditure/expenditurecentral, #财政支出分权= 地方财政支出/中央财政支出#
            
            ##控制变量情况##
            
            pcgdp = gdp/population,		 #人均GDP=GDP/总人口#
            urban = urbanpop/population,		 #城镇化水平=城镇人口/总人口#
            ndindusty = secondindustry/gdp,		 #产业结构2=第二产业增加值/GDP#
            thdindusty = thirdindusty/gdp,		#产业结构3=第三产业增加值/GDP#
            investgdp = socialinvest/gdp,				#固定资产投资占比=固定资产投资额/GDP#
            pcinvest = socialinvest/population, #人均固定资产投资=固定资产投资额/总人口#
            opengdp = imexport/gdp,           #对外开放程度=进出口贸易总额/GDP#
            fdigdp = foreigninvest/gdp,				#外商直接投资占比=外商直接投资额/GDP#
            pcfdi = foreigninvest/population,		 #人均外商直接投资=外商直接投资额/总人口#
            
            pcfinanceloan = financeloan/population,		#人均金融机构贷款余额#
            financeloangdp = financeloan/gdp,		 #金融机构贷款余额占比=金融机构贷款余额/GDP#
            
            lowallowancepop = lowallowance/population,  #城乡最低生活保障人数占比=城乡最低生活保障人数/总人口#
            agingpop2 = agingpopburden*0.01,		#原老龄人口抚养比单位为%，这里将其转换为直接的小数#
            
            #gdpgrowth = (gdp-L.gdp)/L.gdp
            #fdigrowth = (fdi-L.fdi)/L.fdi
            #investgrowth = (invest-L.invest)/L.invest
            #popgrowth = (population-L.population)/L.population
            
            ##应急预算情况##
            
            pcemergency = emergencybudget/population, #人均应急预算=应急预算/总人口#
            pcreservefee = reservefee/population, #人均预备费=预备费/总人口#
            emergencyzhi = emergencybudget/budgetexpenditure, #应急预算占比=年初应急预算/年初一般公共预算支出#
            reservefeezhi = reservefee/budgetexpenditure, #预备费占比=年初预备费/年初一般公共预算支出#
            
            ## 对部分变量取对数 ##
            
            lnrevenue = log(revenue),		 #地方财政收入对数#
            lnexpenditure = log(expenditure), #地方财政支出对数#
            lndeficit = log(deficit),		 #财政赤字对数#
            lndebtgov = log(debtgov),		 #地方政府债券余额对数#
            lndebtct = log(debtct),				#城投债余额对数#
            lndebtsum = log(debtsum),		 #地方政府债务余额对数#
            
            lnpcrevenue = log(pcrevenue),		 #人均地方财政收入对数#
            lnpcexpenditure = log(pcexpenditure), #人均地方财政支出对数#
            lnpcdeficit = log(pcdeficit),		 #人均财政赤字对数#
            lnpcdebtgov = log(pcdebtgov),		 #人均地方政府债券余额对数#
            lnpcdebtct = log(pcdebtct),		#人均城投债余额对数#
            lnpcdebtsum = log(pcdebtsum),		 #人均地方政府债务余额对数#
            
            lndisasterloss = log(disasterloss),		#自然灾害直接经济损失对数#
            #lnpcloss = log(pcloss),				#人均自然灾害直接经济损失对数#
            #lnpcloss3 = log(pcloss3),				#过去3年人均自然灾害直接经济损失对数#
            #lnpcloss5 = log(pcloss5),				#过去5年人均自然灾害直接经济损失对数#
            
            lnpcgdp = log(pcgdp),				 #人均GDP对数#
            lnpctransfer = log(pctransfer),				#人均转移支付对数#
            lnpcfdi = log(pcfdi),				 #人均外商直接投资对数#
            lnpcinvest = log(pcinvest),				#人均固定资产投资对数#
            lnpopdensity = log(popdensity),				#人口密度对数#
            lnhealthbed = log(healthbed),		 #每万人医疗机构床位数对数#
            lnpopulation = log(population),			 #人口规模对数#
            lnpcfinanceloan = log(pcfinanceloan),		 #人均金融机构贷款余额对数#
            lninternet = log(internet),				#互联网宽带接入用户对数#
            lnpastaff = log(pastaff),				 #公共管理、社会保障和社会组织城镇单位就业人员#
            lnlowallowance = log(lowallowance),		#城乡最低生活保障人数对数#
            lnselforganize = log(selforganize),		 #自组织单位数对数#
            
            lnpcemergency = log(pcemergency),		 #人均应急预算对数#
            lnpcreservefee = log(pcreservefee),		#人均预备费对数#
            
            lndisasterloss=ifelse(lndisasterloss==., 0, lndisasterloss)
          )

disaster_finance_filled <- disaster_finance_cleaned %>% 
             mutate
  

