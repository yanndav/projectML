use "$Data/Uganda ELA Panel wide.dta" 

**We use only those observations that are present at baseline
keep if baseline==1
count




/*============================================================================================================
								Step 2: Preparing the data 
===============================================================================================================*/


* replacing values 
replace M_idealbaby_ageF=. if M_idealbaby_ageF<16
replace RM_idealbaby_ageF=. if RM_idealbaby_ageF<16
replace M_idealdaughter_marry=. if M_idealdaughter_marry<16
replace RM_idealdaughter_marry=. if RM_idealdaughter_marry<16
replace M_idealson_marry=. if M_idealson_marry<16
replace RM_idealson_marry=. if RM_idealson_marry<16
replace Rempowerment=. if follow_up==0
replace Qempowerment=. if endline==0
replace RRhiv_skillsALT=. if follow_up==0
replace QRhiv_skillsALT=. if endline==0
replace Rempl=. if follow_up==0
replace Qempl=. if endline==0
replace Rselfempl=. if follow_up==0
replace Qselfempl=. if endline==0
replace Rany_iga=. if follow_up==0
replace Qany_iga=. if endline==0
replace Rincome_year_ind=. if follow_up==0
replace Qincome_year_ind=. if endline==0

* generating variables
gen age_Imarry=M_marrywhen+age
replace age_Imarry=. if age_Imarry<16 | age_Imarry>50
gen Rage_Imarry=RM_marrywhen
replace Rage_Imarry=. if Rage_Imarry<16 | Rage_Imarry>50
gen M_marrywhox=M_marrywho
gen RM_marrywhox=RM_marrywho

* generating variables for heterogeneity analysis
gen rural=.
replace rural=1 if branchno==6 | branchno==7 | branchno==8 | branchno==9 | branchno==10
replace rural=0 if branchno==1 | branchno==2 | branchno==3 | branchno==4 | branchno==5
sum HHAssetvalue_total, d
gen rich=.
replace rich=1 if HHAssetvalue_total>r(p50) & HHAssetvalue_total!=.
replace rich=0 if HHAssetvalue_total<=r(p50) & HHAssetvalue_total!=.
gen below16=.
replace below16=1 if age<=15 & age!=.
replace below16=0 if age>15 & age!=.






/*============================================================================================================
								Step 3: Generating Indexes
===============================================================================================================*/

*************************************
* generate Q indexes (endline)
*************************************

*** IGA Index
* generate zscores of the variables
foreach var in QEntrep_total Qany_iga Qselfempl Qempl QExpenditure_totDF {
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if endline==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & endline==1	
	sum z_`var' if treatment==0 & panel==1
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & endline==1
}
*averaging
egen iga=rowmean(z_QEntrep_total z_Qany_iga z_Qselfempl z_Qempl z_QExpenditure_totDF)

*** Control Over Body Index
* shorten variable names to use z_score and reverse vars so that higher values indicate better outcomes
gen QM_chi=-QM_children
gen Qpart=-Qpartner
gen QR_sexu=-QR_sexunwilling
gen Qsex_p=Qsex_pregnancy
gen QRhiv_s=QRhiv_skillsALT
gen Qalways_c=Qalways_condom
gen Qother_c=Qother_contraceptive
*gen zscores of the variables
foreach var in QM_chi Qpart QR_sexu Qsex_p QRhiv_s Qalways_c Qother_c{
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if endline==1  
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & endline==1
	sum z_`var' if treatment==0 & panel==1
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & endline==1
}
*averaging 
egen control_body=rowmean(z_QM_chi z_Qpart z_QR_sexu z_Qsex_p z_QRhiv_s z_Qalways_c z_Qother_c)

*** Aspiration Index
* shorten variable names to use z_score and reverse vars so that higher values indicate better outcomes
gen QM_i_ageF = QM_idealmarry_ageF
gen QM_i_ageM =QM_idealmarry_ageM
gen QM_baby_no =-QM_idealbaby_no
gen QM_baby_ageF = QM_idealbaby_ageF
rename QM_idealdaughter_marry QM_idealdaught_marry
gen QM_daught =QM_idealdaught_marry
gen QM_son =QM_idealson_ma
* generate zscores of the variables
foreach var in Qempowerment QM_i_ageF QM_i_ageM QM_baby_no QM_baby_ageF QM_daught QM_son{
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if endline==1  
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & endline==1	
	sum z_`var' if treatment==0 & panel==1
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & endline==1
}
*averaging
egen aspiration=rowmean(z_QM_i_ageF z_QM_i_ageM z_QM_baby_no z_QM_baby_ageF z_QM_daught z_QM_son)

*** generate zscores of all the indexes
foreach var in  control_body aspiration iga{
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if endline==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & endline==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & endline==1
drop `var'
rename z_`var' Q`var'
}





*************************************
* generating R indexes (follow-up)
*************************************

*** IGA Index
foreach var in REntrep_total Rany_iga Rselfempl Rempl RExpenditure_totDF {
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
}
*averaging
egen iga=rowmean(z_REntrep_total z_Rany_iga z_Rselfempl z_Rempl z_RExpenditure_totDF)

*** Control Over Body Index
*shorten variable names to use z_score and reverse vars so that higher values indicate better outcomes
gen RM_chi=-RM_children
gen Rpart=-Rpartner
gen RR_sexu=-RR_sexunwilling
gen Rsex_p=Rsex_pregnancy
gen RRhiv_s=RRhiv_skillsALT
gen Ralways_c=Ralways_condom
gen Rother_c=Rother_contraceptive
* generate zscores of the variables
foreach var in RM_chi Rpart RR_sexu Rsex_p RRhiv_s Ralways_c Rother_c{
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
}
*averaging 
egen control_body=rowmean(z_RM_chi z_Rpart z_RR_sexu z_Rsex_p z_RRhiv_s z_Ralways_c z_Rother_c)

*** Aspiration Index
* shorten variable names to use z_score and reverse vars so that higher values indicate better outcomes
gen RM_i_ageF = RM_idealmarry_ageF
gen RM_i_ageM =RM_idealmarry_ageM
gen RM_baby_no =-RM_idealbaby_no
gen RM_baby_ageF = RM_idealbaby_ageF
gen RM_daught =RM_idealdaughter_marry
gen RM_son =RM_idealson_ma
* generate zscores of the variables
foreach var in Rempowerment RM_i_ageF RM_i_ageM RM_baby_no RM_baby_ageF RM_daught RM_son {
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
}
* averaging 
egen aspiration=rowmean(z_RM_i_ageF z_RM_i_ageM z_RM_baby_no z_RM_baby_ageF z_RM_daught z_RM_son)

*** generate zscores of all the indexes
foreach var in  control_body aspiration iga {
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
drop `var'
rename z_`var' R`var'
}



*************************************
* Generating Baseline indexes
*************************************

*** IGA Index
* generate zscores of the variables
foreach var in Entrep_total any_iga selfempl empl Expenditure_totDF {
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
}
*averaging
egen iga=rowmean(z_Entrep_total z_any_iga z_selfempl z_empl z_Expenditure_totDF)
* generate zscores of the variables for non-panel observations
foreach var in Entrep_total any_iga selfempl empl Expenditure_totDF {
    sum `var' if treatment==0
    gen zALL_`var'=(`var'-r(mean))/r(sd)
    sum zALL_`var' if treatment==1
    replace zALL_`var'=r(mean) if treatment==1 & zALL_`var'==.
	sum zALL_`var' if treatment==0
    replace zALL_`var'=r(mean) if treatment==0 & zALL_`var'==.
}
*averaging
egen igaALL=rowmean(zALL_Entrep_total zALL_any_iga zALL_selfempl zALL_empl zALL_Expenditure_totDF)

*** Control Over Body Index
* shorten variable names to use z_score and reverse vars so that higher values indicate better outcomes
gen M_chi=-M_children
gen part=-partner
gen R_sexu=-R_sexunwilling
gen sex_p=sex_pregnancy
gen Rhiv_s=Rhiv_skillsALT
gen always_c=always_condom
gen other_c=other_contraceptive
* generate zscores of the variables
foreach var in M_chi part R_sexu sex_p Rhiv_s always_c other_c{
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
}
*averaging
egen control_body=rowmean(z_M_chi z_part z_R_sexu z_sex_p z_Rhiv_s z_always_c z_other_c)
* generate zscores of the variables for non-panel observations
foreach var in M_chi part R_sexu sex_p Rhiv_s always_c other_c{
    sum `var' if treatment==0
    gen zALL_`var'=(`var'-r(mean))/r(sd)
    sum zALL_`var' if treatment==1
    replace zALL_`var'=r(mean) if treatment==1 & zALL_`var'==.
	sum zALL_`var' if treatment==0
    replace zALL_`var'=r(mean) if treatment==0 & zALL_`var'==.
}
*averaging 
egen control_bodyALL=rowmean(zALL_M_chi zALL_part zALL_R_sexu zALL_sex_p zALL_Rhiv_s zALL_always_c zALL_other_c)

*** Aspiration Index
*shorten variable names to use z_score and reverse vars so that higher values indicate better outcomes
gen M_i_ageF = M_idealmarry_ageF
gen M_i_ageM =M_idealmarry_ageM
gen M_baby_no =-M_idealbaby_no
gen M_baby_ageF = M_idealbaby_ageF
gen M_daught =M_idealdaughter_marry
gen M_son =M_idealson_ma
* generate zscores of the variables
foreach var in empowerment M_i_ageF M_i_ageM M_baby_no M_baby_ageF M_daught M_son{
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
}
*averaging
egen aspiration=rowmean(z_M_i_ageF z_M_i_ageM z_M_baby_no z_M_baby_ageF z_M_daught z_M_son)
* generate zscores of the variables for non-panel observations
foreach var in empowerment M_i_ageF M_i_ageM M_baby_no M_baby_ageF M_daught M_son{
    sum `var' if treatment==0
    gen zALL_`var'=(`var'-r(mean))/r(sd)
    sum z_`var' if treatment==1
    replace zALL_`var'=r(mean) if treatment==1 & zALL_`var'==.
	sum z_`var' if treatment==0 
    replace zALL_`var'=r(mean) if treatment==0 & zALL_`var'==.
}
*averaging
egen aspirationALL=rowmean(zALL_M_i_ageF zALL_M_i_ageM zALL_M_baby_no zALL_M_baby_ageF zALL_M_daught zALL_M_son)

*** generate zscores of all the indexes
foreach var in control_body aspiration iga {
    sum `var' if treatment==0 & panel==1
    gen z_`var'=(`var'-r(mean))/r(sd) if follow_up==1
    sum z_`var' if treatment==1 & panel==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==. & follow_up==1
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==. & follow_up==1
drop `var'
rename z_`var' `var'
}
foreach var in control_bodyALL aspirationALL igaALL {
    sum `var' if treatment==0
    gen z_`var'=(`var'-r(mean))/r(sd)
    sum z_`var' if treatment==1
    replace z_`var'=r(mean) if treatment==1 & z_`var'==.
	sum z_`var' if treatment==0 & panel==1 
    replace z_`var'=r(mean) if treatment==0 & z_`var'==.
drop `var'
rename z_`var' `var'
}

*** renaming variables
rename M_ablework_ifmarried M_ablework_if
rename M_wanttowork_ifmarried M_wanttowork_if
rename M_idealdaughter_marry M_idealdaught_marry
rename RM_idealdaughter_marry RM_idealdaught_marry


* BRANCH DUMMIES
xi, prefix(_B) i.branch_name


save "$Data/ELA.dta",replace
