cd "C:\Users\johnk\OneDrive\Desktop\Objective2WhyMFIsConvert"
use "data.dta" 
set more off, perm
xtset mfiid year
set matsize 8000
**by mfiid, sort: egen rounds = count( year )
//NB: Average is 7, while median is 5 and mode is 2
//Regressions with cooperatives

*** Make factors 
encode age, generate(age1) label(age)
drop age
rename age1 age 

label define currentlegalstatus 0 "NGO" 1 "Bank" 2 "Credit Union/ Cooperative" 3 "NBFI" 4 "Rural Bank"
encode currentlegalstatus, generate(currentlegalstatus1) label(currentlegalstatus)
drop currentlegalstatus
rename currentlegalstatus1 currentlegalstatus 

label define currentlegaldummy 0 "NGO" 1 "others"
encode currentlegaldummy, generate(currentlegaldummy1) label(currentlegaldummy)
drop currentlegaldummy
rename currentlegaldummy1 currentlegaldummy 

encode legal_tradition, generate(legal_tradition1) label(legal_tradition)
drop legal_tradition
rename legal_tradition1 legal_tradition

encode region, generate(region1) label(region)
drop region
rename region1 region

*** Check the variables for stationarity ----
xtunitroot fisher assets, dfuller trend lags(1)
xtunitroot fisher asset_structure, dfuller trend lags(1)
xtunitroot fisher education, dfuller trend lags(1)
xtunitroot fisher kkm, dfuller trend lags(1)
xtunitroot fisher pcrdbgdp, dfuller trend lags(1)
xtunitroot fisher stmktcap, dfuller trend lags(1)
xtunitroot fisher gdp_growth_annual, dfuller trend lags(1)

**Winsorize the independent variables
winsor2 assets, suffix(_w) cuts(10 90) by(assets)
winsor2 asset_structure, suffix(_w) cuts(10 90) by(year)
winsor2 fdev, suffix(_w) cuts(10 90) by(year)
winsor2 kkm, suffix(_w) cuts(10 90) by(year)
winsor2 education, suffix(_w) cuts(10 90) by(year)
winsor2 gdp_growth_annual, suffix(_w) cuts(10 90) by(year)

** Logit models 
mlogit currentlegalstatus ib2.age ib2.legal_tradition asset_structure lassets fdev kkm education i.year, nolog
est sto a11
*******************************
*margins, dydx(age legal_tradition asset_structure fdev education) at(kkm=( -1 (0.2) 1.2))
*marginsplot, xlabel(-1(0.2)1.2) name(kkm0) noci nolabels unique legend(off)
*margins, dydx(age kkm asset_structure fdev education) at(legal_tradition=( 1 2 3))
*marginsplot, name(legal_tradition0)
*margins, dydx(legal_tradition kkm asset_structure fdev education) at(age=(1 3 2))
*marginsplot, name(age0)
*margins, dydx(age legal_tradition kkm asset_structure education) at(fdev =(0.3 (10) 200))
*marginsplot, xlabel(0.3(10)200) name(fdev0)
*margins, dydx(age legal_tradition kkm asset_structure fdev) at(education =(0.07 (0.1) 1.05))
*marginsplot, xlabel(0.07(0.1)1.05) name(education0)
*margins, dydx(age legal_tradition kkm education fdev) at(asset_structure =(0.07 (0.1) 0.9))
*marginsplot, xlabel(0.07(0.1)0.9) name(asset_structure0)


*graph combine kkm0 legal_tradition0 age0 fdev0, ycommon
*graph combine education0 asset_structure0 lassets0, ycommon

**With fewer years 
**Logit
mlogit currentlegalstatus ib2.age ib2.legal_tradition asset_structure lassets fdev kkm education i.year if count >= 3, nolog
est sto a12
mlogit currentlegalstatus ib2.age ib2.legal_tradition asset_structure lassets fdev kkm education i.year if count >= 5 , nolog
est sto a13

** Without cooperatives 
****Logit 
mlogit currentlegalstatus ib2.age ib2.legal_tradition asset_structure lassets fdev kkm education i.year if currentlegalstatus  != 2, nolog
est sto c11

**Winsorized data
** Logit with coop
mlogit currentlegalstatus ib2.age ib2.legal_tradition asset_structure_w lassets_w fdev_w kkm_w education_w i.year, nolog
est sto c12
** Logit without coop
mlogit currentlegalstatus ib2.age ib2.legal_tradition asset_structure_w lassets_w fdev_w kkm_w education_w i.year if currentlegalstatus  != 2, nolog
est sto c13

****************************************************************************************************************************************************
*****************************************************************************************************************************************************
**Output without winsorized data- logit and probit 
esttab a11 a12 a13 c11 using mlogit_full.rtf, se replace
esttab c12 c13 using mlogit_full_wins.rtf, se replace
*******************************************************************************************************************************************************
*****************************************************************************************************************************************************
**Define a variable legal dummy with NGO == 0, and 1 otherwise
*gen legal_dummy = "Other" if currentlegalstatus != 1
*replace legal_dummy = "NGO" if currentlegalstatus == 1

*label define legal_dummy 0 "NGO" 1 "Other" 
*encode legal_dummy, generate(legal_dummy1) label(legal_dummy)
*drop legal_dummy
*rename legal_dummy1 legal_dummy 


** Run mixed effects regressions 
** Logit models 
melogit currentlegaldummy ib2.age ib2.legal_tradition assets kkm pcrdbgdp stmktcap gdp_growth_annual i.year
est sto t11
*******************************
margins, dydx(age legal_tradition lassets fdev education) at(kkm=( -1 (0.4) 1.2))
marginsplot, xlabel(-1(0.2)1.2) name(kkm) noci legend(on)
margins, dydx(kkm lassets fdev education) at(legal_tradition=(2 1 3))
marginsplot, noci name(legal_tradition)
margins, dydx(kkm lassets fdev education) at(age=(3 1 2))
marginsplot, name(age) noci
margins, dydx(age legal_tradition kkm lassets education) at(fdev =(0.3 (20) 200))
marginsplot, xlabel(0.3(10)200) noci name(fdev)
margins, dydx(age legal_tradition kkm lassets fdev) at(education =(0.07 (0.2) 1.05))
marginsplot, xlabel(0.07(0.1)1.05) noci name(education)
margins, dydx(age legal_tradition kkm education fdev) at(lassets =(0.07 (0.2) 0.9))
marginsplot, xlabel(0.00(0.1)1) noci name(lassets)

graph combine kkm legal_tradition, ycommon
graph combine age asset_structure lassets, ycommon
graph combine fdev education, ycommon



** Less years  
melogit currentlegaldummy ib2.age ib2.legal_tradition ib1.region assets kkm pcrdbgdp stmktcap i.year
est sto t12




** Probit Models 
meprobit legal_dummy ib2.age ib2.legal_tradition asset_structure lassets fdev kkm education i.year, nolog
est sto t15

meprobit legal_dummy ib2.age ib2.legal_tradition asset_structure lassets fdev kkm education i.year if currentlegalstatus != 2, nolog
est sto t16


esttab t11 t12 t13 t14 t15 t16 t17 using mixedlogit.rtf, se replace
*esttab t17 using mixedlogit_winsorized.rtf, se replace



** Winsorized data 

















