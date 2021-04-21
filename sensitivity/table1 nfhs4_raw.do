*do file to replicate estimates in col 2, 3, 4 in table 1 of the paper 
*ag, feb 16

*generate nfhs 3 estimates 

	*set directory

	global dir "C:\Cloud\OneDrive - Emory University\data\NFHS\NFHS5 Papers\worsening despite wash\tables"
	*global server "C:\Users\aashi\Dropbox\My PC (PSCStat02)\Desktop\nfhs5_distict_nl" 
	global datadir "T:\IHProjs\Global Diabetes Research Center\Patel\Data\dhs_program\IA"
	
	
	use "$datadir\IAPR74DT\IAPR74FL.DTA", clear
	
	** survey set
	gen psu =    hv021
	gen strata = hv023
	gen weight = hv005 / 100000

	svyset psu [pw = weight], strata(strata) vce(linearized) singleunit(scaled)
	
*keep and drop 

	keep if hc1 <60

*create variables 	
		
	** child age in months
	recode hc1 (0/5 = 1 "<6") (6/8 = 2 "6-8") (9/11 = 3 "9-11") (12/17 = 4 "12-17") ///
	(18/23 = 5 "18-23") (24/35 = 6 "24-35") (36/47 = 7 "36-47") /// 
	(48/59 = 8 "48-59"), gen(child_age)
	label var child_age "Child age (months)"
	label val child_age child_age

	** child age in months 2
	recode hc1 (0/4 = 1 "<5") (5/9 = 2 "5-9") (10/15 = 3 "10-15") (16/19 = 4 "16-19") ///
	(20/25 = 5 "20-25") (26/35 = 6 "26-35") (36/49 = 7 "36-49") /// 
	(50/59 = 8 "50-59"), gen(child_age2)
	label var child_age2 "Child age in months"
	label val child_age2 child_age2

	** stunting = Height-for-age
	cap drop stunting
	gen stunting=0 if hv103==1             
	replace stunting=. if hc70>=9996 
	replace stunting=1 if hc70<-200 & hv103==1
	label define stunting 0"Not stunting" 1"Stunting"
	label var stunting "Stunting children"
	label val stunting stunting

	** wasting = Weight-for-height
	gen wasting=0 if hv103==1 
	replace wasting=. if hc72>=9996 
	replace wasting=1 if hc72<-200 & hv103==1
	label define wasting 0"Not wasting" 1"Wasting"
	label var wasting "Wasting children"
	label val wasting wasting

	** underweight = Weight-for-age
	gen underweight=0 if hv103==1              
	replace underweight=. if hc71>=9996 
	replace underweight=1 if hc71<-200 & hv103==1
	label define underweight 0"Not underweight" 1"Underweight"
	label var underweight "Underweight children"
	label val underweight underweight
	
	** overweight
	gen overweight=0 if hv103==1            
	replace overweight=. if hc72>=9996
	replace	overweight=1 if hc72>200 & hc72<9996 & hv103==1 
	label define overweight 0 "Not overweight" 1 "Overweight"
	label var overweight "Overweight children"
	label val overweight overweight
	
	** anemia 
	gen anemia = 0 if hv103==1 & hc1>5 & hc1<60
	replace anemia = 1 if hc56<110 & hv103==1 & hc1>5 & hc1<60
	replace anemia = . if hc56==.
	label define anemia 0 "not anemic" 1 "anemic"
	label var anemia "anemic children"
	label val anemia anemia
	
	
* recode and rename 

	rename hc27 sex
	rename hv270 wealth
	rename hv025 residence
	rename hv024 region
	rename shdist district
	
*collapse at the district level 

	preserve 
	collapse stunting wasting underweight overweight anemia [aw=weight], by(district)
	
	replace stunting = stunting * 100
	replace wasting = wasting * 100
	replace underweight = underweight * 100
	replace overweight = overweight * 100
	replace anemia = anemia * 100 
	
	format stunting wasting underweight overweight anemia %5.2f
	save "$dir/ajcn reviewer/nfhs4_district_nutrition.dta", replace 
	restore

*for 22 states, generate rural urban overweight 

	keep if inlist(region, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
	*21 states and UTs here
	*22nd is ladakh, already included 
	
	replace region = 8 if region == 9
	replace region = 37 if district == 4 | district == 3 
	
	preserve 
	keep if residence == 1
	collapse stunting wasting underweight overweight anemia [aw=weight], by(region)
	
	replace stunting = stunting * 100
	replace wasting = wasting * 100
	replace underweight = underweight * 100
	replace overweight = overweight * 100
	replace anemia = anemia * 100 
	
	format stunting wasting underweight overweight anemia %5.2f
	save "$dir/ajcn reviewer/nfhs4_nutrition_state_urban.dta", replace 
	restore

	preserve 
	keep if residence == 2
	collapse stunting wasting underweight overweight anemia [aw=weight], by(region)
	
	replace stunting = stunting * 100
	replace wasting = wasting * 100
	replace underweight = underweight * 100
	replace overweight = overweight * 100
	replace anemia = anemia * 100 
	
	format stunting wasting underweight overweight anemia %5.2f
	save "$dir/ajcn reviewer/nfhs4_nutrition_state_rural.dta", replace 
	restore
	
	
	*urban
	sum stunting [aw=weight] if residence == 1
	sum underweight [aw=weight] if residence == 1
	sum anemia [aw=weight] if residence == 1
	sum wasting [aw=weight] if residence == 1
	sum overweight [aw=weight] if residence == 1
	
	
	*rural
	sum stunting [aw=weight] if residence == 2
	sum underweight [aw=weight] if residence == 2
	sum anemia [aw=weight] if residence == 2
	sum wasting [aw=weight] if residence == 2
	sum overweight [aw=weight] if residence == 2
	
	*total
	sum stunting [aw=weight]
	sum underweight [aw=weight]
	sum anemia [aw=weight]
	sum wasting [aw=weight]
	sum overweight [aw=weight]
	
	* Saving the datasets
		keep psu hv021 strata hv023 weight hv002 hv003 hv004 hv005 child_age child_age2 hc1 stunting wasting underweight overweight anemia hv103 hc70 hc71 hc72 hc56 sex wealth residence region district
		save "C:\Cloud\OneDrive - Emory University\data\NFHS\NFHS5 Papers\worsening despite wash\tables\ajcn reviewer\nfhs4_raw.dta"
	