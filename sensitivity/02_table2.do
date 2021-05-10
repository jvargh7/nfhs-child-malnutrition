*do file to replicate estimates in col 2, 3, 4 in table 2 of the paper 
*ag, may 7 

*generate nfhs 3 estimates 

	*set directory

	global dir "C:\Users\aashi\Dropbox\research\nfhs5_wash"
	global server "C:\Users\aashi\Dropbox\My PC (PSCStat02)\Desktop\nfhs5_distict_nl" 
	
	
*nfhs 3 
	
	global datadir "C:\Users\aashi\Dropbox\Lekha Jokha\Data\NFHS\NFHS 3 STATA"


	*lliteracy 

			
		use "$datadir\iair52dt - individual\IAIR52FL.dta", clear	

		gen rc_litr=0
		replace rc_litr=1 if v106==3 | v155==1 | v155==2	
		label values rc_litr yesno
		label var rc_litr "Literate - higher than secondary or can read part or whole sentence"

		tab rc_litr [aw=v005]

			preserve
			collapse rc_litr [aw = v005], by(v024)
			save "$dir/do/rep/output/nfhs3_literacy_state_overall.dta", replace
			restore 

			
			preserve
			collapse rc_litr if v025 == 1 [aw = v005] , by(v024)
			save "$dir/do/rep/output/nfhs3_literacy_state_urban.dta", replace
			restore 
		
			preserve
			collapse rc_litr  if v025 == 2 [aw = v005], by(v024)
			save "$dir/do/rep/output/nfhs3_literacy_state_rural.dta", replace
			restore 
	
		
		

		
		
		*use "$datadir\iapr52dt - members\IAPR52FL.dta", clear
		
		*gen eduyr=hv108 if hv103==1 & inrange(hv105,6,99) & inrange(hv108,0,96) & hv104==2
		
		*gen edu_10yr = (eduyr >= 10) if eduyr!=.
		
		*preserve
		*collapse edu_10yr if edu_10yr!=. [aw = hv005], by(hv024)

		
		*save "$dir/do/rep/output/nfhs3_edu_10yr_f_state.dta", replace
		*restore

		*keep if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		
		*tab edu_10yr [aw=hv005]
	
	
	*marriage 
		use "$datadir\iair52dt - individual\IAIR52FL.dta", clear	
		
		
		recode v511 (.=0) (0/17 = 1 "yes") (18/49 = 0 "no"), gen (ms_afm_18)
		replace ms_afm_18 = . if v012<20 | v012 > 24
		label var ms_afm_18 "First marriage by age 18 among women ages 20-24"



		keep if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		tab ms_afm_18 [aw=v005]

			collapse ms_afm_18 if ms_afm_18!=. [aw = v005], by(v024)
			save "$dir/do/rep/output/nfhs3_marriage_18_state.dta", replace

		
	*sex ratio at birth 
		use "$datadir\iabr52dt - birth\IABR52FL.dta", clear

		**** child's age ****
		gen age = v008 - b3
		
		drop if age > 59
		
		*create a sex ratio at birth label 
		
		gen female = b4 == 2 
		
		preserve
		collapse female [aw = v005], by(v024)

		gen srb = (female / (1 - female)) * 1000
		
		save "$dir/do/rep/output/nfhs3_srb_state.dta", replace
		
		restore

		
		keep if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		tab female [aw=v005]
		di ((.4770) / (1 - .4770)) * 1000
		
		
	use "$datadir\iapr52dt - members\IAPR52FL.dta", clear
	
	svyset hv001 [pw=hv005], strata(hv023)

	
	*keep usual members 
	keep if hv102 == 1 
	
	*improved sanitatio
	recode hv205 ///
	44 = 23 ///
	, gen (ph_sani_type)
	
	recode ph_sani_type . = 99

	label define ph_sani_type	11	 "flush - to piped sewer system" ///
								12	 "flush - to septic tank"		///
								13	 "flush - to pit latrine"		///
								14	 "flush - to somewhere else"	///
								15	 "flush - don't know where/unspecified"			///
								21	 "pit latrine - ventilated improved pit (vip)"	///
								22	 "pit latrine - with slab"		///
								23	 "pit latrine - without slab / open pit" 		///
								31	 "no facility/bush/field/river/sea/lake" 		///
								41	 "composting toilet"			///
								42	 "bucket toilet"				///
								43	 "hanging toilet/latrine"		///
								51	 "other improved"				///
								96	 "other"						///
								99 	 "missing"
	label values ph_sani_type ph_sani_type
	label var ph_sani_type "Type of sanitation"
	
// create improved sanitation indicator
	recode ph_sani_type (11/13 15 21 22 41 51 = 1 "improved sanitation") (14 23 42 43 96 = 2 "unimproved sanitation") (31 = 3 "open defecation") (99=.), gen(ph_sani_improve)
	label var ph_sani_improve "Improved sanitation"
	cap replace ph_sani_improve = 2 if hv225==1 


	*keep if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)

	tab ph_sani_improve [aw=hv005]
	gen improve_sanitation_dummy = ph_sani_improve == 1 if ph_sani_improve!=.
	

	svy: mean improve_sanitation_dummy if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
	svy: mean improve_sanitation_dummy, over(hv024)
	
// improved drinking water

	cap gen ph_wtr_source = hv201
	recode ph_wtr_source . = 99

	cap label define ph_wtr_source 	11   "piped into dwelling"				///
									12	 "piped to yard/plot" 				///
									13	 "public tap/standpipe" 			///
									14	 "piped to neighbor"				///
									15	 "piped outside of yard/lot" 		///
									21	 "tube well or borehole" 			///
									30	 "well - protection unspecified" 	///
									31	 "protected well" 					///
									32	 "unprotected well"					///
									40	 "spring - protection unspecified" 	///
									41	 "protected spring" 				///
									42	 "unprotected spring"				///
									43	 "surface water (river/dam/lake/pond/stream/canal/irrigation channel)" ///
									51	 "rainwater"						///
									61	 "tanker truck"						///
									62	 "cart with small tank, cistern, drums/cans" ///
									65	 "purchased water"					///
									71	 "bottled water"					///
									72	 "purified water, filtration plant" ///
									73	 "satchet water"					///
									96	 "other"							///			
									99	 "missing"			
	cap label values ph_wtr_source ph_wtr_source
	cap label var ph_wtr_source "Source of drinking water"
	*/
	
*clean fuels 

	
	

// improved water source
recode ph_wtr_source (11/15 21 31 41 51 61/73 = 1 "improved water") (30 32 40 42 43 96 = 0 "unimproved/surface water") (99=99 "missing"), gen(ph_wtr_improve)
label var ph_wtr_improve "Improved Water Source"

tab ph_wtr_improve [aw=hv005] 

gen water_improve = ph_wtr_improve == 1 
gen sani_improve = ph_sani_improve == 1 

	tab ph_sani_improve [aw=hv005]
	
svy: mean water_improve if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
svy: mean water_improve, over(hv024)
	
*clean fuel 

gen ph_cook_clean= inrange(hv226,1,4) 
label values ph_cook_clean yesno
label var ph_cook_clean "Using clean fuel for cooking"

svy: mean ph_cook_clean if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
svy: mean ph_cook_clean, over(hv024)

*insurance 

gen insurance = sh64 == 1
svy: mean insurance if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
svy: mean insurance, over(hv024) 


*electricity 
		tab hv206 [aw=hv005] 
		
		rename hv206 electricity
	
		svy: mean electricity if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		svy: mean electricity, over(hv024)
	
	
	
*nfhs 3, literacy file 

	
use "$datadir\iair52dt - individual\IAIR52FL.dta", clear	

gen rc_litr=0
replace rc_litr=1 if v106==3 | v155==1 | v155==2	
label values rc_litr yesno
label var rc_litr "Literate - higher than secondary or can read part or whole sentence"

tab rc_litr [aw=v005]

		preserve
		collapse rc_litr [aw = v005], by(v024)
		save "$dir/out/nfhs3_literacy_state_overall.dta", replace
		restore 

		
		preserve
		collapse rc_litr if v025 == 1 [aw = v005] , by(v024)
		save "$dir/out/nfhs3_literacy_state_urban.dta", replace
		restore 
	
		preserve
		collapse rc_litr  if v025 == 2 [aw = v005], by(v024)
		save "$dir/out/nfhs3_literacy_state_rural.dta", replace
		restore 

	*survey set the data 
		svyset v001 [pw=v005], strata(v023)

		svy: mean rc_litr if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		
		svy: mean rc_litr, over(v024)

		*education 
		gen edu_10yr = 0
		replace edu_10yr = 1 if inrange(v133,10,95)
		
		svy: mean edu_10yr if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		
		svy: mean edu_10yr, over(v024)		
		
*marriage
recode v511 (.=0) (0/17 = 1 "yes") (18/49 = 0 "no"), gen (ms_afm_18)
replace ms_afm_18 = . if v012<20 | v012 > 24
label var ms_afm_18 "First marriage by age 18 among women ages 20-24"

gen not_married_18 = 1 if ms_afm_18 == 0 
replace not_married_18 = 0 if ms_afm_18 == 1

*keep if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
*tab ms_afm_18 [aw=v005]

		svy: mean not_married_18 if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		svy: mean ms_afm_18, over(v024)


	collapse ms_afm_18 if ms_afm_18!=. [aw = v005], by(v024)
	save "$dir/out/nfhs3_marriage_18_state.dta", replace

		
		
		
*education 

	use "$datadir\iapr52dt - members\IAPR52FL.dta", clear
	
	gen eduyr=hv108 if hv103==1 & inrange(hv105,6,99) & inrange(hv108,0,96) & hv104==2
	
	gen edu_10yr = (eduyr >= 10) if eduyr!=.
	svyset hv001 [pw=hv005], strata(hv023)

	
	
	preserve
	collapse edu_10yr if edu_10yr!=. [aw = hv005], by(hv024)

	
	save "$dir/out/nfhs3_edu_10yr_f_state.dta", replace
	restore

	*keep if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
	
	tab edu_10yr [aw=hv005]

	svy: mean edu_10yr if inlist(hv024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
	svy: mean edu_10yr, over(hv024)
	
	
*marriage 

	
	
*nfhs 3, birth history 

	use "$datadir\iabr52dt - birth\IABR52FL.dta", clear

	**** child's age ****
	gen age = v008 - b3
	
	drop if age > 59
	
	*create a sex ratio at birth label 
	
	gen female = b4 == 2 
	
	preserve
	collapse female [aw = v005], by(v024)

	gen srb = (female / (1 - female)) * 1000
	
	save "$dir/out/nfhs3_srb_state.dta", replace
	
	restore
	
	svyset v001 [pw=v005], strata(v023)


	
	*keep if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
	svy: mean female if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
	di ((.4770) / (1 - .4770)) * 1000
	di ((.4686956) / (1 - .4686956)) * 1000	
	di ((.4852405) / (1 - .4852405)) * 1000
	
	svy: mean female, over(v024)


	

		
*nfhs 4 
	
	global datadir "C:\Users\aashi\Dropbox\Lekha Jokha\Data\NFHS"
	
	use "$datadir\nfhs 4\data\IAPR71DT\IAPR74FL.DTA", clear


	*keep usual residents 

		keep if hv102 == 1 

		
	*keep 22 states and UTs 

		*keep if inlist(hv024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)


	*water 
		cap gen ph_wtr_source = hv201

		recode ph_wtr_source . = 99
	
		cap label define ph_wtr_source 	11   "piped into dwelling"				///
										12	 "piped to yard/plot" 				///
										13	 "public tap/standpipe" 			///
										14	 "piped to neighbor"				///
										15	 "piped outside of yard/lot" 		///
										21	 "tube well or borehole" 			///
										30	 "well - protection unspecified" 	///
										31	 "protected well" 					///
										32	 "unprotected well"					///
										40	 "spring - protection unspecified" 	///
										41	 "protected spring" 				///
										42	 "unprotected spring"				///
										43	 "surface water (river/dam/lake/pond/stream/canal/irrigation channel)" ///
										51	 "rainwater"						///
										61	 "tanker truck"						///
										62	 "cart with small tank, cistern, drums/cans" ///
										65	 "purchased water"					///
										71	 "bottled water"					///
										72	 "purified water, filtration plant" ///
										73	 "satchet water"					///
										96	 "other"							///			
										99	 "missing"			
		cap label values ph_wtr_source ph_wtr_source
		cap label var ph_wtr_source "Source of drinking water"
		*/

		// improved water source
		recode ph_wtr_source (11/15 21 31 41 51 61/73 = 1 "improved water") (30 32 40 42 43 96 = 0 "unimproved/surface water") (99=99 "missing"), gen(ph_wtr_improve)
		label var ph_wtr_improve "Improved Water Source"

		*tab 
		tab ph_wtr_improve [aw=hv024]
		
		svyset hv001 [pw=hv005], strata(hv023) singleunit(centered)
		svy: mean ph_wtr_improve if inlist(hv024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)

		svy: mean ph_wtr_improve, over(hv024)
		svy: mean ph_wtr_improve, over(shdistri)

		

	*sanitation 

		recode hv205 ///
		44 = 23 ///
		, gen (ph_sani_type)
		
		recode ph_sani_type . = 99

		label define ph_sani_type	11	 "flush - to piped sewer system" ///
									12	 "flush - to septic tank"		///
									13	 "flush - to pit latrine"		///
									14	 "flush - to somewhere else"	///
									15	 "flush - don't know where/unspecified"			///
									21	 "pit latrine - ventilated improved pit (vip)"	///
									22	 "pit latrine - with slab"		///
									23	 "pit latrine - without slab / open pit" 		///
									31	 "no facility/bush/field/river/sea/lake" 		///
									41	 "composting toilet"			///
									42	 "bucket toilet"				///
									43	 "hanging toilet/latrine"		///
									51	 "other improved"				///
									96	 "other"						///
									99 	 "missing"
		label values ph_sani_type ph_sani_type
		label var ph_sani_type "Type of sanitation"
		
		recode ph_sani_type (11/13 15 21 22 41 51 = 1 "improved sanitation") (14 23 42 43 96 = 2 "unimproved sanitation") (31 = 3 "open defecation") (99=.), gen(ph_sani_improve)
		label var ph_sani_improve "Improved sanitation"
		cap replace ph_sani_improve = 2 if hv225==1 
		
		tab ph_sani_improve [aw=hv005]
		gen
		
		gen sani_improve = ph_sani_improve == 1 

		svy: mean sani_improve if inlist(hv024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean sani_improve, over(hv024)
		svy: mean sani_improve, over(shdistri)


		
	*electricity

		tab hv206 [aw=hv005] 
				
		rename hv206 electricity
		
		svy: mean electricity if inlist(hv024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean electricity, over(hv024)
		svy: mean electricity, over(shdistri)
				
		gen water_improve = ph_wtr_improve == 1 
		gen sani_improve = ph_sani_improve == 1 
			
	*states 
	replace hv024 = 8 if hv024 == 9	
	replace hv024 = 37 if shdistri == 4 | shdistri == 3 
			
		preserve
		collapse electricity water_improve sani_improve [aw = hv005], by(hv024)
		save "$dir/do/rep/output/nfhs4_wash_state_overall.dta", replace
		restore 

		preserve
		collapse electricity water_improve sani_improve if hv025 == 1 [aw = hv005], by(hv024)
		save "$dir/do/rep/output/nfhs4_wash_state_urban.dta", replace
		restore 
		
		preserve
		collapse electricity water_improve sani_improve if hv025 == 2 [aw = hv005], by(hv024)
		save "$dir/do/rep/output/nfhs4_wash_state_rural.dta", replace
		restore 
		
		preserve 
		collapse electricity water_improve sani_improve [aw = hv005], by(hv024 shdistri)
		save "$dir/do/rep/output/nfhs4_wash_district.dta", replace
		restore
		
	*insurance 
			gen insurance = sh54 == 1 
					
		svy: mean insurance if inlist(hv024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean insurance, over(hv024)
		svy: mean insurance, over(shdistri)
		
	*clean fuel 
	
			cap drop ph_cook_clean
			gen ph_cook_clean= inrange(hv226,1,4) 
			label var ph_cook_clean "Using clean fuel for cooking"

							
		svy: mean ph_cook_clean if inlist(hv024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean ph_cook_clean, over(hv024)
		svy: mean ph_cook_clean, over(shdistri)
		
	*individual level data
	
		use "$datadir\nfhs 4\data\IAIR71DT\IAIR71FL.DTA", clear
	
		*literacy
		svyset v001 [pw=v005], strata(v023) singleunit(centered)
		
		gen rc_litr=0
		replace rc_litr=1 if v106==3 | v155==1 | v155==2	
		label values rc_litr yesno
		label var rc_litr "Literate - higher than secondary or can read part or whole sentence"

		tab rc_litr [aw=v005]
		svy: mean rc_litr if inlist(v024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean rc_litr, over(v024)
		svy: mean rc_litr, over(sdistri)
		
		*marriage 
		recode v511 (.=0) (0/17 = 1 "yes") (18/49 = 0 "no"), gen (ms_afm_18)
		replace ms_afm_18 = . if v012<20 | v012 > 24
		label var ms_afm_18 "First marriage by age 18 among women ages 20-24"
		
		gen not_married_18 = 1 if ms_afm_18 == 0 
		replace not_married_18 = 0 if ms_afm_18 == 1 

		svy: mean not_married_18 if inlist(v024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean not_married_18, over(v024)
		svy: mean not_married_18, over(sdistri)

		*education 
			
			*education 
		gen edu_10yr = 0
		replace edu_10yr = 1 if inrange(v133,10,95)
		
		svy: mean edu_10yr if inlist(v024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		svy: mean edu_10yr, over(v024)
		svy: mean edu_10yr, over(sdistri)
		

		
		
		*sex ratio births
		
		use "C:\Users\aashi\Dropbox\Lekha Jokha\Data\NFHS\nfhs 4\data\IABR71DT\IABR71FL.DTA", clear
		
		**** child's age ****
		gen age = v008 - b3
		
		drop if age > 59
		
		*create a sex ratio at birth label 
		
		gen female = b4 == 2 
		
		svyset v001 [pw=v005], strata(v023) singleunit(centered)
		svy: mean female if inlist(v024, 1,2, 4, 5, 8, 9, 10, 11, 13, 14, 16, 17, 18, 20, 21, 22, 23, 24, 30, 32, 35, 36)
		di (.4795608 / (1-.4795608)) * 1000
		di (.4751417 / (1-.4751417)) * 1000
		di (.4839799 / (1-.4839799)) * 1000

		
		svy: mean female, over(v024)
		svy: mean female, over(sdistri)
		
		
		
		
		*preserve
		*collapse female [aw = v005], by(v024)

		*gen srb = (female / (1 - female)) * 1000
		
		
		
		
		*keep if inlist(v024, 1, 2, 10, 11, 13, 14, 15, 16, 17, 18, 19, 24, 27, 28, 29, 30, 32)
		*tab ms_afm_18 [aw=v005]

		*	collapse ms_afm_18 if ms_afm_18!=. [aw = v005], by(v024)
		*	save "$dir/do/rep/output/nfhs3_marriage_18_state.dta", replace


		
	
*remaining nfhs 4 and 5 

	*total
	use "$dir\do\rep\input\nfhs5_summary_sheets.dta", clear
	
	encode round, gen(wave)
	
	encode nfhs5_state, gen(state_id)
	
	rename *, lower

	*need to bring in weights 
	
	merge m:1 state_id using "$dir\do\rep\input\state_pop.dta"
	
	*calculations 
		*women who are literate

		sum s14 [aw=btotl_2015] if wave == 2
		sum s14 [aw=btotl_2019] if wave == 3

	*married 

		sum s20 [aw=btotl_2015] if wave == 2
		sum s20 [aw=btotl_2019] if wave == 3

	*clean fuel 

		sum s10 [aw=btotl_2015] if wave == 2
		sum s10 [aw=btotl_2019] if wave == 3
		
	*sanitation 
	
		sum s09 [aw=btotl_2019] if wave == 3
	
	*insurance 

		sum s12 [aw=btotl_2015] if wave == 2
		sum s12 [aw=btotl_2019] if wave == 3

	*schooling 

		sum s16 [aw=btotl_2015] if wave == 2
		sum s16 [aw=btotl_2019] if wave == 3
		
	*sex ratio at birth 

		sum s04 [aw=b0004_2015] if wave == 2
		sum s04 [aw=b0004_2019] if wave == 3

	*drinking water 

		sum s08 [aw=btotl_2015] if wave == 2
		sum s08 [aw=btotl_2019] if wave == 3

*nfhs 4 and 5, rural 

	*rural 
	
	use "$dir\do\rep\input\nfhs5_rural_summary_sheets.dta", clear
		
	encode round, gen(wave)
	
	encode nfhs5_state, gen(state_id)
	
	rename *, lower

	*need to bring in weights 
	
	merge m:1 state_id using "$dir\do\rep\input\state_pop.dta"

	
	*calculations 
	*women who are literate

		sum s14 [aw=btotl_2015] if wave == 1
		sum s14 [aw=btotl_2019] if wave == 2

	*married 

		sum s20 [aw=btotl_2015] if wave == 1
		sum s20 [aw=btotl_2019] if wave == 2

	*clean fuel 

		sum s10 [aw=btotl_2015] if wave == 1
		sum s10 [aw=btotl_2019] if wave == 2
		
	*sanitation 
	
		sum s09 [aw=btotl_2019] if wave == 2
		sum s09 [aw=btotl_2019] if wave == 3

	
	*insurance 

		sum s12 [aw=btotl_2015] if wave == 1
		sum s12 [aw=btotl_2019] if wave == 2

	*schooling 

		sum s16 [aw=btotl_2015] if wave == 1
		sum s16 [aw=btotl_2019] if wave == 2
		
	*sex ratio at birth 

		sum s04 [aw=b0004_2015] if wave == 1
		sum s04 [aw=b0004_2019] if wave == 2

	*drinking water 

		sum s08 [aw=btotl_2015] if wave == 1
		sum s08 [aw=btotl_2019] if wave == 2
		
*urban
	
	use "$dir\do\rep\input\nfhs5_urban_summary_sheets.dta", clear
		
	encode round, gen(wave)
	
	encode nfhs5_state, gen(state_id)
	
	rename *, lower

	*need to bring in weights 
	
	merge m:1 state_id using "$dir\do\rep\input\state_pop.dta"

	
	*calculations 
	*women who are literate

		sum s14 [aw=btotl_2015] if wave == 1
		sum s14 [aw=btotl_2019] if wave == 2

	*married 

		sum s20 [aw=btotl_2015] if wave == 1
		sum s20 [aw=btotl_2019] if wave == 2

	*clean fuel 

		sum s10 [aw=btotl_2015] if wave == 1
		sum s10 [aw=btotl_2019] if wave == 2
		
	*sanitation 
	
		sum s09 [aw=btotl_2019] if wave == 2
	
	*insurance 

		sum s12 [aw=btotl_2015] if wave == 1
		sum s12 [aw=btotl_2019] if wave == 2

	*schooling 

		sum s16 [aw=btotl_2015] if wave == 1
		sum s16 [aw=btotl_2019] if wave == 2
		
	*sex ratio at birth 

		sum s04 [aw=b0004_2015] if wave == 1
		sum s04 [aw=b0004_2019] if wave == 2

	*drinking water 

		sum s08 [aw=btotl_2015] if wave == 1
		sum s08 [aw=btotl_2019] if wave == 2
