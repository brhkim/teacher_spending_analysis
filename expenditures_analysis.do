/*	

	Title: expenditures_analysis.do
	Author: Brian Heseung Kim (@brhkim)
	Date Modified: 21-03-11
	
	Description: The following do-file conducts the teacher out-of-pocket 
	spending analysis as described in the report, "Supporting Students at Any 
	Cost? Examining the Dynamics of Teacher Out-of-Pocket Spending, Student 
	Demographics, and Teacher Autonomy." Note that this requires the 
	restricted-use data from NCES for the Schools and Staffing Survey waves in
	2007-2008 and 2011-2012. Please let me know if you have any concerns,
	suggestions, or questions!
	
*/

//Basic file setup
	clear all
	set more off
	set matsize 10000
	set maxvar 10000

//Change the working directory
	cd "C:/FILEPATHREDACTED/21-02-22 Teacher Expenditure Analysis"

//Load the 07-08 data
	use "data/raw/pubtea07.dta", clear
	
	//Generate a year variable indicator
		gen year=2007
		
	//Rename the relevant analytic variables for ease of merging data later
		rename T0268 expenditures
		rename T0343 basesalary
		rename T0025 mainassignment
		rename T0290 c_materials
		rename T0280 c_texts
		rename T0281 c_content
		rename T0282 c_techs
		rename T0283 c_grade
		rename T0284 c_disc
		rename T0285 c_hw
	
	//Drop irrelevant variables
		keep CNTLNUMT MINENR TFNLWGT TREPWT* expenditures basesalary mainassignment PGMTYPE CHARFLAG NSLAPP_S year EARNSCH EARNALL URBANS12 NEWTCH RACETH_T t_* c_*

//Load the 11-12 data, conduct the same data preparation steps as for the 07-08 data
	preserve
		use "data/raw/pubtea11.dta", clear
		gen year=2011
		rename T0399 expenditures
		rename T0508 basesalary
		rename T0025 mainassignment
		rename T0439 c_materials
		rename T0427 c_texts
		rename T0428 c_content
		rename T0429 c_techs	
		rename T0430 c_grade
		rename T0431 c_disc
		rename T0432 c_hw
		
		keep CNTLNUMT MINENR TFNLWGT TREPWT* expenditures basesalary mainassignment PGMTYPE CHARFLAG NSLAPP_S year EARNSCH EARNALL URBANS12 NEWTCH RACETH_T t_* c_*
			
		//Save a temporary file to merge in
		tempfile tmp
		save `tmp', replace
	restore

//Append the 11-12 data to the presently open 07-08 data
	append using `tmp'

//Set the dataset as survey data according to the various recommendations by NCES
	svyset CNTLNUMT [pweight=TFNLWGT], brrweight(TREPWT*) vce(brr) mse

//Create global variables for ease of looping later
	global moneyvars "basesalary EARNALL EARNSCH expenditures"

	//Sample restriction: full-time teacher at regular, non-charter schools who earn a non-zero base salary
	global sampleset "mainassignment==1 & PGMTYPE==1 & CHARFLAG==2 & basesalary>0"


//Basic variable cleaning and transformation

	//Race/ethnicity bucket quartile splits
		//REMEMBER -8 IS MISSING
		//replace RACE = . if RACE==-8
		gen racequart4 = .
		replace racequart4=1 if MINENR>=0 & MINENR<25
		replace racequart4=2 if MINENR>=25 & MINENR<50
		replace racequart4=3 if MINENR>=50 & MINENR<75
		replace racequart4=4 if MINENR>=75 & MINENR<=100
		
		gen racequart2 = .
		replace racequart2=1 if MINENR>=0 & MINENR<50
		replace racequart2=2 if MINENR>=50 & MINENR<=100
	
	//FRPL bucket quartile splits
		gen frplquart4 = .
		replace frplquart4=1 if NSLAPP_S>=0 & NSLAPP_S<25
		replace frplquart4=2 if NSLAPP_S>=25 & NSLAPP_S<50
		replace frplquart4=3 if NSLAPP_S>=50 & NSLAPP_S<75
		replace frplquart4=4 if NSLAPP_S>=75 & NSLAPP_S<=100
		
		gen frplquart2 = .
		replace frplquart2=1 if NSLAPP_S>=0 & NSLAPP_S<50
		replace frplquart2=2 if NSLAPP_S>=50 & NSLAPP_S<=100
		
	//Income-adjusted expenditures
		gen propspend_sch = expenditures/EARNSCH
		gen propspend_all = expenditures/EARNALL
		gen propspend_base = expenditures/basesalary
		
	//Any spending
		gen anyspend=(expenditures!=0)
		
	//Autonomy survey variables
		//Flip values of the materials question (lower is greater agreement)
			gen c_materials2=5-c_materials
		
		//Generate an index average of the relevant survey questions
			egen c_avg = rowmean(c_texts c_content c_techs c_materials2)
		
		//Create 3-level autonomy buckets per NCES style
			gen auto_a = 0
			replace auto_a=1 if c_avg>=3
			replace auto_a=2 if (c_texts==4 & c_content==4 & c_techs==4 & c_materials2==4)
			
		//Create 2-level median split for autonomy buckets
			_pctile c_avg [pweight=TFNLWGT], p(50)
			local split50 = r(r1)
			gen auto_b = (c_avg>=`split50')
		
		//Create 3-level tercile split for autonomy buckets
			_pctile c_avg [pweight=TFNLWGT], p(33)
			local split33 = r(r1)
			_pctile c_avg [pweight=TFNLWGT], p(66)
			local split66 = r(r1)
			
			gen auto_c = 0
			replace auto_c=1 if c_avg>=`split33'
			replace auto_c=2 if c_avg>=`split66'
		
	//Do some basic inflation adjustments on the money variables
		gen cpi_quarter_tmp=string(year)+"q4"
		gen cpi_quarter=quarterly(cpi_quarter_tmp, "YQ", 2020)
		format cpi_quarter %tq
		drop cpi_quarter_tmp
		
		preserve
			use "data/raw/cpi_crosswalk.dta", clear
			sum cpi if cpi_quarter== tq(2019q4)
			local cpi_reference = r(mean)
		restore
		
		merge m:1 cpi_quarter using "data/raw/cpi_crosswalk.dta", keep(match master)
		
		foreach var of global moneyvars {
			gen `var'_adj = `var'
			replace `var'_adj = . if `var'_adj == -8
			
			replace `var'_adj = (`var'_adj * `cpi_reference') / cpi
		}

		
//Save checkpoint
	save "data/build/teacher_expenditures_build.dta", replace
		
	use "data/build/teacher_expenditures_build.dta", clear
		
//Run regressions for RQ1: Does teacher spending relate to student racial/ethnic minority status?
	//Prepare looping across number of race/ethnicity/FRPL bins
		local bingroups "4 2"
		
	//Prepare output local for file replacement
		local replace "replace"
	
	//Loop across outcome variables of interest
	foreach outcome of varlist propspend_sch propspend_all propspend_base expenditures anyspend {
		//Loop across race/ethnicity/FRPL bin specifications, 4- or 2-bucket
		foreach bingroup of local bingroups { 
			//Loop across the inclusion of teacher experience variable as a control
			forvalues experience = 0/1 {
				//Loop across the inclusion of school urbanicity variable as a control
				forvalues urbanicity = 0/1 {
					//Set decimal reporting for the clean output tables
						local decimals = 4

						if "`outcome'" == "expenditures" {
							local decimals = 0
						}
						if "`outcome'" == "anyspend" {
							local decimals = 2
						}
					
					//Set the controls for regression based on which loop we're in
						local controls "i.racequart`bingroup'"
						if `experience'==1 {
							local controls "`controls' NEWTCH"
						}
						if `urbanicity'==1 {
							local controls "`controls' i.URBANS12"
						}
					
					//First regress super simple race-only analysis
						svy: regress `outcome' `controls' i.year if $sampleset
						outreg2 using "output/results1_raw.xls", excel `replace' symbol(**,*,+) bdec(`decimals') sdec(`decimals') ctitle("`outcome'_`bingroup'_exp`experience'_urb`urbanicity'")
						esttab using "output/`outcome'_`bingroup'_exp`experience'_urb`urbanicity'.csv", replace wide plain cells("b p ci_l ci_u")
						
						local replace "append"
					
					//Set the interacted controls for regression based on which loop we're in
						local controls "i.frplquart`bingroup'#i.racequart`bingroup'"
						if `experience'==1 {
							local controls "`controls' NEWTCH"
						}
						if `urbanicity'==1 {
							local controls "`controls' i.URBANS12"
						}
					
					//Run the interacted race-frpl buckets regression
						svy: regress `outcome' `controls' i.year if $sampleset
						outreg2 using "output/results1_raw.xls", excel `replace' symbol(**,*,+) bdec(`decimals') sdec(`decimals') ctitle("`outcome'_int_`bingroup'_exp`experience'_urb`urbanicity'")
						esttab using "output/`outcome'_int_`bingroup'_exp`experience'_urb`urbanicity'.csv", replace wide plain cells("b p ci_l ci_u")
						
					
				}
			}
		}
	}
		
		
//RQ3: How does teacher autonomy relate to teacher spending?
	//Conduct basic cronbach's alpha test for indices of teacher autonomy used here
		alpha c_materials2 c_texts c_content c_techs, asis item
		
		/* Results

		Test scale = mean(unstandardized items)
																	average
									 item-test     item-rest       interitem
		Item         |  Obs  Sign   correlation   correlation     covariance      alpha
		-------------+-----------------------------------------------------------------
		c_materials2 | 75737   +       0.5056        0.1848        .3675558      0.6963
		c_texts      | 75737   +       0.8028        0.5384        .1517976      0.4427
		c_content    | 75737   +       0.7988        0.5368         .154807      0.4445
		c_techs      | 75737   +       0.6247        0.4339        .2899137      0.5635
		-------------+-----------------------------------------------------------------
		Test scale   |                                             .2410185      0.6256
		-------------------------------------------------------------------------------
		*/
			
	//Compare against cronbach's alpha test for all teacher classroom-related survey questions
		alpha c_materials2 c_texts c_content c_techs c_grade c_disc c_hw, asis item

		/* Results

		Test scale = mean(unstandardized items)
																	average
									 item-test     item-rest       interitem
		Item         |  Obs  Sign   correlation   correlation     covariance      alpha
		-------------+-----------------------------------------------------------------
		c_materials2 | 75737   +       0.4387        0.2095        .2109867      0.7500
		c_texts      | 75737   +       0.7117        0.5033        .1499282      0.6810
		c_content    | 75737   +       0.7306        0.5350        .1457775      0.6701
		c_techs      | 75737   +       0.6868        0.5733        .1764081      0.6730
		c_grade      | 75737   +       0.6529        0.5319        .1815101      0.6811
		c_disc       | 75737   +       0.5747        0.4135        .1885389      0.7007
		c_hw         | 75737   +       0.5826        0.4452        .1912254      0.6968
		-------------+-----------------------------------------------------------------
		Test scale   |                                             .1777678      0.7264
		-------------------------------------------------------------------------------
		*/

//Run regressions for RQ3
	local bingroups "4 2"
	local replace "replace"
	
	//Loop over each potential index for teacher autonomy
	foreach autovar of varlist auto_a auto_b auto_c {
		//Loop over each outcome variable of interest for teacher spending
		foreach outcome of varlist propspend_sch propspend_all propspend_base expenditures anyspend {
			//Loop over race/ethnicity/FRPL bucket specification, 4- or 2-bucket
			foreach bingroup of local bingroups { 
				//Loop over inclusion of teacher experience variable as a control
				forvalues experience = 0/1 {
					//Loop over school urbanicity variable as a control
					forvalues urbanicity = 0/1 {
						
						//Set decimal reporting for the clean output tables
							local decimals = 4

							if "`outcome'" == "expenditures" {
								local decimals = 0
							}
							if "`outcome'" == "anyspend" {
								local decimals = 2
							}
						
						//Run regression just examining simple relationship between autonomy and spending
							svy: regress `outcome' i.`autovar' i.year if $sampleset
							outreg2 using "output/rq3/results3_raw.xls", excel `replace' symbol(**,*,+) bdec(`decimals') sdec(`decimals') ctitle("`outcome'_`bingroup'_exp`experience'_urb`urbanicity'")
							esttab using "output/rq3/3_`autovar'_`outcome'_unc_`bingroup'_exp`experience'_urb`urbanicity'.csv", replace wide plain cells("b p ci_l ci_u")
							
							local replace "append"
						
						//Now include additional student demographic controls: race/ethnicity only
							local controls "i.racequart`bingroup'"
							if `experience'==1 {
								local controls "`controls' NEWTCH"
							}
							if `urbanicity'==1 {
								local controls "`controls' i.URBANS12"
							}
							
							svy: regress `outcome' i.`autovar' `controls' i.year if $sampleset
							outreg2 using "output/rq3/results3_raw.xls", excel `replace' symbol(**,*,+) bdec(`decimals') sdec(`decimals') ctitle("`outcome'_`bingroup'_exp`experience'_urb`urbanicity'")
							esttab using "output/rq3/3_`autovar'_`outcome'_unint_`bingroup'_exp`experience'_urb`urbanicity'.csv", replace wide plain cells("b p ci_l ci_u")
							
						//Now include additional student demographic controls: race/ethnicity interacted with FRPL
							local controls "i.frplquart`bingroup'#i.racequart`bingroup'"
							if `experience'==1 {
								local controls "`controls' NEWTCH"
							}
							if `urbanicity'==1 {
								local controls "`controls' i.URBANS12"
							}
							
							svy: regress `outcome' i.`autovar' `controls' i.year if $sampleset
							outreg2 using "output/rq3/results3_raw.xls", excel `replace' symbol(**,*,+) bdec(`decimals') sdec(`decimals') ctitle("`outcome'_int_`bingroup'_exp`experience'_urb`urbanicity'")
							esttab using "output/rq3/3_`autovar'_`outcome'_int_`bingroup'_exp`experience'_urb`urbanicity'.csv", replace wide plain cells("b p ci_l ci_u")
							
					}
				}
			}
		}
	}
