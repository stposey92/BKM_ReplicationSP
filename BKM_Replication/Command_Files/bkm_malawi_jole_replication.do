 * Do Job Networks Disadvantage Women? Evidence from a Recruitment Experiment in Malawi;
 /*Created: 10/10/2016 */
 /*Last edited by author: 12/22/2016*/
/* Minor edits by Sean Posey: 10/8/2020 */
/* adjust this as necessary. This works on all OS when running in batch mode, but may not work in interactive mode */
cd "C:\Users\Public\Replication\Replication\BKM_Replication"
local pwd : pwd // put your path up to BKM_Replication folder
global rootdir  "`pwd'"
global logdir "$rootdir/Documents/logs"

local name "First Name, Last Name"

cap mkdir $logdir

/* check if the author creates a log file. If not, adjust the following code fragment */

local c_date = c(current_date)
local cdate = subinstr("`c_date'", " ", "_", .)
local c_time = c(current_time)
local ctime = subinstr("`c_time'", ":", "_", .)

log using "$logdir/logfile_`cdate'_`ctime'.log", replace text

/* It will provide some info about how and when the program was run */
/* See https://www.stata.com/manuals13/pcreturn.pdf#pcreturn */
local variant = cond(c(MP),"MP",cond(c(SE),"SE",c(flavor)) )  
// alternatively, you could use 
// local variant = cond(c(stata_version)>13,c(real_flavor),"NA")  

di "=== SYSTEM DIAGNOSTICS ==="
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `variant'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"
di "=========================="


/* install any packages locally */
capture mkdir "$rootdir/ado"
sysdir set PERSONAL "$rootdir/ado/personal"
sysdir set PLUS     "$rootdir/ado/plus"
sysdir set SITE     "$rootdir/ado/site"
sysdir

/* add packages to the macro */

* *** Add required packages from SSC to this list ***
    local ssc_packages ""
    // local ssc_packages "estout boottest"
    
    if !missing("`ssc_packages'") {
        foreach pkg in `ssc_packages' {
            dis "Installing `pkg'"
            ssc install `pkg', replace
        }
    }

    * Install packages using net
    *  net install yaml, from("https://raw.githubusercontent.com/gslab-econ/stata-misc/master/")
net install estout, from( http://fmwww.bc.edu/RePEc/bocode/e/)
/* other commands */

/* after installing all packages, it may be necessary to issue the mata mlib index command */
	mata: mata mlib index


set more off

clear
set mem 500m
set more off
set matsize 800

capture mkdir "$rootdir/Documents/tables" // Create new Directories for the tables and graphs
capture mkdir "$rootdir/Documents/graphs"

#delimit;
cd "$rootdir/Original_Data";
use "Recruitment_ReplicationData.dta",clear;
tab location, gen(l);
gen lweek = l2*OP_week;
egen OP_age10 = cut(OP_agegrp), at(10 20 30 40 50);
replace OP_age10 = 60 if OP_age10==.;
global controls= "i.OP_date";

bys OP_date: egen frac_female = mean(OP_female);
gen OP_bias = OP_male_ra_score-OP_female_ra_score;
egen OP_bias_group = cut(OP_bias), at(-10 0 1 2 3 10) icodes;
for var frac_female OP_bias OP_bias_group: gen perf_X = treat_perf*X;
eststo clear;




*********************************************************************************;
******** Table 1: Gender Distributions of OP and Referrals *********;
*********************************************************************************;

ge REF_malequalify= (1-REF_female)*REF_alt_nofeedback_qual;

ge REF_femqualify= REF_alt_nofeedback_qual*REF_female;

** Panel A **;

*first row blank;
local r1c1="A. OP Characteristics"; local r1c2=""; local r1c3=""; local r1c4=""; local r1c5="";
#delimit;
*fraction of ops;
local r2c1="Fraction of OPs";
local r2c2 1;
*sum OP_female if  !missing(OP_qualify);
sum OP_female  ;
local r2c4=r(mean);
local r2c3=`r2c2'-`r2c4';
local r2c5="";

*op is qualified;
local r3c1="OP is Qualified";
sum OP_alt_nofeedback_qual if  !missing(OP_female);
local r3c2=r(mean);
reg OP_alt_nofeedback_qual OP_female ;
local r3c3=_b[_cons];
local r3c4=_b[_cons]+_b[OP_female];
test OP_female;
local r3c5=r(p);

*n;
local r4c1="N";
count if !missing(OP_qualify) & !missing(OP_female);
local r4c2=r(N);
count if  !missing(OP_qualify) & OP_female==0;
local r4c3=r(N);
count if  !missing(OP_qualify) & OP_female==1;
local r4c4=r(N);
local r4c5="";

** Panel B **;

*first row blank;
local r5c1="B. OP Characteristics: Either Gender Treatments"; local r5c2=""; local r5c3=""; local r5c4=""; local r5c5="";

*fraction of ops;
local r6c1="Fraction of OPs";
local r6c2 1;
sum OP_female if OP_treatment_gender==3 ;
local r6c4=r(mean);
local r6c3=`r6c2'-`r6c4';
local r6c5="";

*op is qualified;
local r7c1="OP is Qualified";
sum OP_alt_nofeedback_qual if OP_treatment_gender==3 & !missing(OP_female);
local r7c2=r(mean);
reg OP_alt_nofeedback_qual OP_female if OP_treatment_gender==3 ;
local r7c3=_b[_cons];
local r7c4=_b[_cons]+_b[OP_female];
test OP_female;
local r7c5=r(p);

*n;
local r8c1="N";
count if OP_treatment_gender==3 & !missing(OP_qualify) & !missing(OP_female);
local r8c2=r(N);
count if OP_treatment_gender==3  & !missing(OP_qualify) & OP_female==0;
local r8c3=r(N);
count if OP_treatment_gender==3 & !missing(OP_qualify) & OP_female==1;
local r8c4=r(N);
local r8c5="";

** Panel C **;

*fifth row blank;
local r9c1="C. Referral Characteristics: Either Gender Treatments"; local r9c2=""; local r9c3=""; local r9c4=""; local r9c5="";

*referral is female;
local r10c1="Referral is Female";
sum REF_female if OP_treatment_gender==3   & !missing(REF_qualify)  & !missing(OP_female);
local r10c2=r(mean);
*reg REF_female OP_female if OP_treatment_gender==3 & !missing(REF_qualify);
reg REF_female OP_female if OP_treatment_gender==3 & !missing(REF_qualify)  & !missing(OP_female) ;
local r10c3=_b[_cons];
local r10c4=_b[_cons]+_b[OP_female];
test OP_female;
local r10c5=r(p);

*referral is qualified;
local r11c1="Referral is Qualified";
sum REF_alt_nofeedback_qual if OP_treatment_gender==3  & !missing(REF_female)  & !missing(OP_female);
local r11c2=r(mean);
reg REF_alt_nofeedback_qual OP_female if OP_treatment_gender==3  & !missing(REF_female);
local r11c3=_b[_cons];
local r11c4=_b[_cons]+_b[OP_female];
test OP_female;
local r11c5=r(p);

*referral is qualified male;
local r12c1="Referral is Qualified Male";
sum REF_malequalify if OP_treatment_gender==3 & !missing(OP_female);
local r12c2=r(mean);
reg REF_malequalify OP_female if OP_treatment_gender==3 ;
local r12c3=_b[_cons];
local r12c4=_b[_cons]+_b[OP_female];
test OP_female;
local r12c5=r(p);

*referral is qualified female;
local r13c1="Referral is Qualified Female";
sum REF_femqualify if OP_treatment_gender==3  & !missing(OP_female);
local r13c2=r(mean);
reg REF_femqualify OP_female if OP_treatment_gender==3 ;
local r13c3=_b[_cons];
local r13c4=_b[_cons]+_b[OP_female];
test OP_female;
local r13c5=r(p);

*n;
local r14c1="N";
count if OP_treatment_gender==3 & !missing(REF_female)&!missing(OP_female);
local r14c2=r(N);
count if OP_treatment_gender==3    & !missing(REF_female)  & OP_female==0;
local r14c3=r(N);
count if OP_treatment_gender==3  & !missing(REF_qualify) & !missing(REF_female)  & OP_female==1;
local r14c4=r(N);
local r14c5="";

** Panel D **;

*eleventh row blank;
local r15c1="D. Referral Characteristics, Fixed Fee Treatments"; local r15c2=""; local r15c3=""; local r15c4=""; local r15c5="";

*referral is female;
local r16c1="Referral is Female";
sum REF_female if OP_treatment_gender==3 & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_qualify)  & !missing(OP_female);
local r16c2=r(mean);
reg REF_female OP_female if OP_treatment_gender==3  & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_qualify);
local r16c3=_b[_cons];
local r16c4=_b[_cons]+_b[OP_female];
test OP_female;
local r16c5=r(p);

*referral is qualified;
local r17c1="Referral is Qualified";
sum REF_alt_nofeedback_qual if OP_treatment_gender==3 & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_female)  & !missing(OP_female);
local r17c2=r(mean);
reg REF_alt_nofeedback_qual OP_female if OP_treatment_gender==3   & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_female);
local r17c3=_b[_cons];
local r17c4=_b[_cons]+_b[OP_female];
test OP_female;
local r17c5=r(p);

*referral is qualified male;
local r18c1="Referral is Qualified Male";
sum REF_malequalify if OP_treatment_gender==3 & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(OP_female);
local r18c2=r(mean);
reg REF_malequalify OP_female if OP_treatment_gender==3  & (OP_treatment_payment==1 | OP_treatment_payment==2);
local r18c3=_b[_cons];
local r18c4=_b[_cons]+_b[OP_female];
test OP_female;
local r18c5=r(p);

*referral is qualified female;
local r19c1="Referral is Qualified Female";
sum REF_femqualify if OP_treatment_gender==3  & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(OP_female);
local r19c2=r(mean);
reg REF_femqualify OP_female if OP_treatment_gender==3  & (OP_treatment_payment==1 | OP_treatment_payment==2);
local r19c3=_b[_cons];
local r19c4=_b[_cons]+_b[OP_female];
test OP_female;
local r19c5=r(p);

*n;
local r20c1="N";
count if OP_treatment_gender==3  & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_qualify) & !missing(REF_female) & !missing(OP_female);
local r20c2=r(N);
count if OP_treatment_gender==3  & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_qualify) & !missing(REF_female)  & OP_female==0;
local r20c3=r(N);
count if OP_treatment_gender==3 & (OP_treatment_payment==1 | OP_treatment_payment==2) & !missing(REF_qualify) & !missing(REF_female)  & OP_female==1;
local r20c4=r(N);
local r20c5="";

** Panel E **;

*seventeenth row blank;
local r21c1="E. Referral Characteristics, Perf Treatments"; local r21c2=""; local r21c3=""; local r21c4=""; local r21c5="";

*referral is female;
local r22c1="Referral is Female";
sum REF_female if OP_treatment_gender==3 & (OP_treatment_payment==3) & !missing(REF_qualify)  & !missing(OP_female);
local r22c2=r(mean);
reg REF_female OP_female if OP_treatment_gender==3  & (OP_treatment_payment==3) & !missing(REF_qualify);
local r22c3=_b[_cons];
local r22c4=_b[_cons]+_b[OP_female];
test OP_female;
local r22c5=r(p);

*referral is qualified;
local r23c1="Referral is Qualified";
sum REF_alt_nofeedback_qual if OP_treatment_gender==3 & (OP_treatment_payment==3) & !missing(REF_female)  & !missing(OP_female);
local r23c2=r(mean);
reg REF_alt_nofeedback_qual OP_female if OP_treatment_gender==3 & (OP_treatment_payment==3) & !missing(REF_female);
local r23c3=_b[_cons];
local r23c4=_b[_cons]+_b[OP_female];
test OP_female;
local r23c5=r(p);

*referral is qualified male;
local r24c1="Referral is Qualified Male";
sum REF_malequalify if OP_treatment_gender==3  & (OP_treatment_payment==3) & !missing(OP_female);
local r24c2=r(mean);
reg REF_malequalify OP_female if OP_treatment_gender==3  & (OP_treatment_payment==3);
local r24c3=_b[_cons];
local r24c4=_b[_cons]+_b[OP_female];
test OP_female;
local r24c5=r(p);

*referral is qualified female;
local r25c1="Referral is Qualified Female";
sum REF_femqualify if OP_treatment_gender==3& (OP_treatment_payment==3) & !missing(OP_female);
local r25c2=r(mean);
reg REF_femqualify OP_female if OP_treatment_gender==3 & (OP_treatment_payment==3);
local r25c3=_b[_cons];
local r25c4=_b[_cons]+_b[OP_female];
test OP_female;
local r25c5=r(p);

*n;
local r26c1="N";
count if OP_treatment_gender==3  & (OP_treatment_payment==3) & !missing(REF_qualify) & !missing(REF_female) & !missing(OP_female);
local r26c2=r(N);
count if OP_treatment_gender==3 & (OP_treatment_payment==3) & !missing(REF_qualify) & !missing(REF_female)  & OP_female==0;
local r26c3=r(N);
count if OP_treatment_gender==3 & (OP_treatment_payment==3) & !missing(REF_qualify) & !missing(REF_female)  & OP_female==1;
local r26c4=r(N);
local r26c5="";


** Outsheet **;

preserve;

clear;
set obs 26;
gen str c1=""; gen str c2=""; gen str c3=""; gen str c4=""; gen str c5="";

forval r = 1/26 {;
forval c = 1/5 {;
replace c`c'="`r`r'c`c''" if _n==`r';
};
};

*rename (c1 c2 c3 c4 c5) (entity All_CAs Male_CAs Female_CAs p_value);
outsheet using "$rootdir/Documents/tables/table1_gender_distributions_of_CAs_and_refferals.csv", comma replace;
restore;



*********************************************************************************;
******* Table 2: Male CA's Referral Choices ******;
*********************************************************************************;

xi: reg OP_makesref treat_female treat_either i.OP_date if OP_gender==1;
sum OP_makesref if treat_female==0 & treat_either==0 & OP_gender==1;
	local mean=r(mean);
eststo OP_makesref1, addscalars(mean `mean');


xi: reg OP_makesref treat_female treat_either treat_perf perftreat_female perftreat_either i.OP_date if OP_gender==1;
sum OP_makesref if treat_female==0 & treat_either==0 & treat_perf==0 & OP_gender==1;
	local mean=r(mean);
eststo OP_makesref3, addscalars(mean `mean');

xi: reg REF_alt_nofeedback_qual treat_female treat_either i.OP_date if OP_gender==1;
sum REF_altqual if treat_female==0 & treat_either==0 & OP_gender==1;
	local mean=r(mean);
eststo REF_altqual1, addscalars(mean `mean');

xi: reg REF_alt_nofeedback_qual treat_female treat_either treat_perf perftreat_female perftreat_either i.OP_date if OP_gender==1;
sum REF_altqual if treat_female==0 & treat_either==0 & treat_perf==0 & OP_gender==1;
	local mean=r(mean);
eststo REF_altqual3, addscalars(mean `mean');

noisily esttab  OP_makesref1 OP_makesref3 REF_altqual1 REF_altqual3 using "$rootdir/Documents/tables/table2_male_CAs_referral_choices.csv",
  cells(b(star fmt(%9.6f)) se(fmt(%9.6f))) starlevel(* .1 ** .05 *** .01) 
	stardetach stats(N mean, fmt(%9.3g))  keep(treat_female treat_either treat_perf perftreat_female perftreat_either )
	order( treat_female treat_either treat_perf perftreat_female perftreat_either ) style(tab) varwidth(8) modelwidth(8) plain label replace;
eststo clear;



*********************************************************************************;
******* Table 3: Screening of Male CAs on Different Characteristics ******;
*********************************************************************************;
foreach i in REF_alt_math REF_alt_ravens REF_alt_computer REF_alt_language REF_feedbackpoints REF_totalpoints REF_tertiary REF_survey_experience {;

	xi: reg `i' treat_female treat_perf perftreat_female treat_either perftreat_either $controls if OP_gender==1;
	sum `i' if OP_gender==1;
	local mean=r(mean);
	local sd=r(sd);
	eststo `i'_M, addscalars(mean `mean' sd `sd');
};

noisily esttab REF_survey_experience*M REF_tertiary*M REF_alt_math*M REF_alt_language*M REF_alt_ravens*M REF_alt_computer*M REF_totalpoints*M REF_feedbackpoints*M 
   using "$rootdir/Documents/tables/table3_screening_of_male_CAs_on_different_characteristics.csv", 
  cells(b(star fmt(%9.6f)) se(fmt(%9.6f))) starlevel(* .1 ** .05 *** .01) 
	stardetach stats(mean sd N, fmt(%9.3g))  keep(treat_female treat_perf perftreat_female treat_either perftreat_either )
	order(treat_female treat_either treat_perf perftreat_female perftreat_either ) style(tab) varwidth(8) modelwidth(8) plain label replace;
eststo clear;

	
*********************************************************************************;
******** Tables 4: Female CA's Referral Choices *********;
*********************************************************************************;

xi: reg OP_makesref treat_female treat_either i.OP_date if OP_gender==2;
sum OP_makesref if treat_female==0 & treat_either==0 & OP_gender==2;
	local mean=r(mean);
eststo OP_makesref2, addscalars(mean `mean');

xi: reg OP_makesref treat_female treat_either treat_perf perftreat_female perftreat_either i.OP_date if OP_gender==2;
sum OP_makesref if treat_female==0 & treat_either==0 & treat_perf==0 & OP_gender==2;
	local mean=r(mean);
eststo OP_makesref4, addscalars(mean `mean');

xi: reg REF_alt_nofeedback_qual treat_female treat_either i.OP_date if OP_gender==2;
sum REF_altqual if treat_female==0 & treat_either==0 & OP_gender==2;
	local mean=r(mean);
eststo REF_altqual2, addscalars(mean `mean');
	
xi: reg REF_alt_nofeedback_qual treat_female treat_either treat_perf perftreat_female perftreat_either i.OP_date if OP_gender==2;
sum REF_altqual if treat_female==0 & treat_either==0 & treat_perf==0 & OP_gender==2;
	local mean=r(mean);
eststo REF_altqual4, addscalars(mean `mean');


noisily esttab OP_makesref2 OP_makesref4  REF_altqual2 REF_altqual4 using "$rootdir/Documents/tables/table4_female_CAs_referral_choices.csv",
  cells(b(star fmt(%9.6f)) se(fmt(%9.6f))) starlevel(* .1 ** .05 *** .01) 
	stardetach stats(N mean , fmt(%9.3g))  keep(treat_female treat_either treat_perf perftreat_female perftreat_either )
	order(treat_female treat_either treat_perf perftreat_female perftreat_either ) style(tab) varwidth(8) modelwidth(8) plain label replace;
eststo clear;


*********************************************************************************;
******* Table 5: Screening of Female CAs on Different Characteristics ******;
*********************************************************************************;

foreach i in REF_alt_math REF_alt_ravens REF_alt_computer REF_alt_language REF_feedbackpoints REF_totalpoints REF_tertiary REF_survey_experience {;

	xi: reg `i' treat_female treat_perf perftreat_female treat_either perftreat_either $controls if OP_gender==2;
	sum `i' if OP_gender==2;
	local mean=r(mean);
	local sd=r(sd);
	eststo `i'_F, addscalars(mean `mean' sd `sd');
};
noisily esttab REF_survey_experience*F REF_tertiary*F REF_alt_math*F REF_alt_language*F REF_alt_ravens*F REF_alt_computer*F REF_totalpoints*F REF_feedbackpoints*F 
   using "$rootdir/Documents/tables/table5_screening_of_female_CAs_on_different_characteristics.csv", 
  cells(b(star fmt(%9.6f)) se(fmt(%9.6f))) starlevel(* .1 ** .05 *** .01) 
	stardetach stats(mean sd N, fmt(%9.3g)) keep(treat_female treat_perf perftreat_female treat_either perftreat_either )
	order(treat_female treat_either treat_perf perftreat_female perftreat_either ) style(tab) varwidth(8) modelwidth(8) plain label replace;
	
	
*********************************************************************************;
******* Table 6: Competition Incentives in the Fixed Fee Treatments***;
*********************************************************************************;

eststo clear;

xi: reg OP_alt_nofeedback_qual treat_rel $controls if OP_gender==1 & treat_perf==0;
eststo OP_altqual1;

xi: reg REF_alt_nofeedback_qual treat_rel $controls if OP_gender==1 & treat_perf==0;
eststo REF_altqual1;

xi: reg REF_alt_nofeedback_qual treat_rel treat_female treat_either reltreat_female reltreat_either $controls if OP_gender==1 & treat_perf==0;
eststo REF_altqual2;

xi: reg OP_alt_nofeedback_qual treat_rel $controls if OP_gender==2 & treat_perf==0;
eststo OP_altqual2;

xi: reg REF_alt_nofeedback_qual treat_rel $controls if OP_gender==2 & treat_perf==0;
eststo REF_altqual3;

xi: reg REF_alt_nofeedback_qual treat_rel treat_female treat_either reltreat_female reltreat_either $controls if OP_gender==2 & treat_perf==0;
eststo REF_altqual4;

noisily esttab OP_altqual1 REF_altqual1 REF_altqual2 OP_altqual2 REF_altqual3 REF_altqual4 using "$rootdir/Documents/tables/table6_competition_incentives_in_the_fixed_fee_treatments.csv",
  cells(b(star fmt(%9.6f)) se(fmt(%9.6f))) starlevel(* .1 ** .05 *** .01) 
	stardetach keep(treat_rel treat_female treat_either reltreat_female reltreat_either)
	order(treat_rel treat_female treat_either reltreat_female reltreat_either) style(tab) varwidth(8) modelwidth(8) plain label replace;

eststo clear;


*********************************************************************************;
******** Figures 1-7 *********;
*********************************************************************************;

twoway (kdensity OP_alt_nofeedback_score if OP_gender==1, lpattern(dash)) (kdensity OP_alt_nofeedback_score if OP_gender==2), legend(label (1 "Male CAs") label( 2 "Female CAs")) 
	xtitle("CA's overall (corrected) score") ytitle("kernel density estimate") title(Figure 1: CA Ability by Gender) graphregion(fcolor(white)) legend(region(lwidth(none)));
graph export "$rootdir/Documents/graphs/fig1_CA_ability_by_gender.png", replace;
graph export "$rootdir/Documents/graphs/fig1_CA_ability_by_gender.pdf", replace;

twoway (lpoly REF_female OP_alt_nofeedback_score if OP_treatment_gender==3&OP_gender==1, lpattern(dash)) 
	(lpoly REF_female OP_alt_nofeedback_score if OP_treatment_gender==3 & OP_gender==2), legend(label (1 "Referrals of Male CAs") 
	label( 2 "Referrals of Female CAs")) xtitle("CA's overall (corrected) score") ytitle("Referral is Female") 
	title("Figure 3: Gender choice in referrals, by CA performance") graphregion(fcolor(white)) legend(region(lwidth(none)));
graph export "$rootdir/Documents/graphs/fig3_gender_choice_in_referrals_by_CA_performance.png", replace;
graph export "$rootdir/Documents/graphs/fig3_gender_choice_in_referrals_by_CA_performance.pdf", replace;



twoway (kdensity REF_alt_nofeedback_score if OP_treatment_gender==1&OP_gender==1&treat_perf==0, lpattern(dash)) 
	(kdensity REF_alt_nofeedback_score if OP_treatment_gender==2 & OP_gender==1 & treat_perf==0), legend(label (1 "Men who must refer men") 
	label( 2 "Men who must refer women")) xtitle("Referral's overall (corrected) score") ytitle("Kernel density estimate") 
	title(Figure 4: Men's Fixed Fee Referrals) graphregion(fcolor(white))legend(region(lwidth(none)));
graph export "$rootdir/Documents/graphs/fig4_mens_fixedfee_referrals.png", replace;
graph export "$rootdir/Documents/graphs/fig4_mens_fixedfee_referrals.pdf", replace;
ksmirnov REF_alt_overall_score if treat_either==0 & OP_gender==1, by(treat_female);


twoway (lpoly REF_alt_nofeedback_qual OP_alt_nofeedback_score if OP_treatment_gender==3&OP_gender==1, lpattern(dash)) 
	(lpoly REF_alt_nofeedback_qual OP_alt_nofeedback_score if OP_treatment_gender==3 & OP_gender==2), legend(label (1 "Referrals of Male CAs") 
	label( 2 "Referrals of Female CAs")) xtitle("CA's overall (corrected) score") ytitle("Referral's qualification rate") 
	title("Figure 6: Referral qualification rate, by CA performance") graphregion(fcolor(white)) legend(region(lwidth(none)));
graph export "$rootdir/Documents/graphs/fig6_referral_qualification_rate_by_CA_performance.png", replace;
graph export "$rootdir/Documents/graphs/fig6_referral_qualification_rate_by_CA_performance.pdf", replace;





twoway (kdensity REF_alt_nofeedback_score if OP_treatment_gender==1&OP_gender==2&treat_perf==0, lpattern(dash)) 
	(kdensity REF_alt_nofeedback_score if OP_treatment_gender==2 & OP_gender==2 & treat_perf==0), legend(label (1 "Women who must refer men") 
	label( 2 "Women who must refer women")) xtitle("Referral's overall (corrected) score") ytitle("Kernel density estimate") 
	title(Figure 7: Women's Fixed Fee Referrals) graphregion(fcolor(white)) legend(region(lwidth(none)));
graph export "$rootdir/Documents/graphs/fig7_womens_fixedfee_referrals.png", replace;
graph export "$rootdir/Documents/graphs/fig7_womens_fixedfee_referrals.pdf", replace;



*********************************************************************************;
*** Appendix Table 1: Summary Statistics and Randomization Check ***;
*********************************************************************************;

local outfilename = "$rootdir/Documents/tables/table_a1_summary_statistics_and_randomization_check.csv";

global satis_list = "OP_age OP_alt_nofeedback_qual OP_alt_nofeedback_score OP_survey_experience OP_tertiary 
OP_mscemath OP_msceenglish OP_JobCompscore 
OP_mathscore OP_ravensscore OP_languagescore  OPpracticalz  OP_computerscore OP_feedbackpoints";

global indep_list ="treat_female treat_either treat_perf perftreat_female perftreat_either";
global nv = wordcount("$satis_list");
disp "$nv";


local i = 0;
foreach var of varlist $satis_list {;
    local i = `i' + 1;
    global outname`i' = "`var'";
    global label`i': variable label `var';
    };



global regout = "depends deplabel malemeansd pvalue_male nobs_male femalemeansd pvalue_female nobs_female";
qui for any depends deplabel: gen str1 X = "";
qui for any malemeansd pvalue_male nobs_male femalemeansd pvalue_female nobs_female: gen X=.;


forvalues i = 1/$nv {;	
	
    replace depends = "${outname`i'}" if _n == ((`i' * 2+1) - 2);
    replace deplabel = "${label`i'}" if _n == ((`i' * 2+1) - 2);

	xi: reg ${outname`i'} treat_female treat_either treat_perf perftreat_female perftreat_either i.OP_date if OP_gender==1;
	test treat_female treat_either treat_perf perftreat_female perftreat_either;
	replace pvalue_male=r(p) if _n == ((`i' * 2+1)  - 2);
	replace nobs_male=e(N) if _n == ((`i' * 2+1)  - 2);

	sum ${outname`i'} if e(sample);
	replace malemeansd=r(mean) if _n == ((`i' * 2+1)  - 2);
	replace malemeansd=r(sd) if _n == ((`i' * 2+1)  - 1);
	
	xi: reg ${outname`i'} treat_female treat_either treat_perf perftreat_female perftreat_either i.OP_date if OP_gender==2;
	test treat_female treat_either treat_perf perftreat_female perftreat_either;
	replace pvalue_female=r(p) if _n == ((`i' * 2+1)  - 2);
	replace nobs_female=e(N) if _n == ((`i' * 2+1)  - 2);

	sum ${outname`i'} if e(sample);
	replace femalemeansd=r(mean) if _n == ((`i' * 2+1)  - 2);
	replace femalemeansd=r(sd) if _n == ((`i' * 2+1)  - 1);

};

*****************************
OUTSHEETING
*****************************;
preserve;
keep $regout;
drop if _n > $nv * 3+ 50;

outsheet using `outfilename', comma replace;
restore;

*********************************************************************************;
*** Appendix Figure A1 and A2: OP gender and qualification rates, by session in training center;
*********************************************************************************;
bys tc trials_in_tc: egen mean_OP_gend = mean(OP_female);
bys trials_in_tc: egen OP_male_qualrate_tc = mean(OP_alt_nofeedback_qual) if OP_female==0;
bys trials_in_tc: egen OP_female_qualrate_tc = mean(OP_alt_nofeedback_qual) if OP_female==1;
bys trials_in_tc: egen OP_males_all= count(OP_alt_nofeedback_qual) if OP_female==0;
bys trials_in_tc: egen OP_females_all= count(OP_alt_nofeedback_qual) if OP_female==1;

twoway (scatter mean_OP_gend trials_in_tc if tc==2 [w= num_OP_obs], mfcolor(bg) msymbol(Oh) xtitle("Session") ytitle("Percent Female") title("Fig A1: Fraction women among CAs over time")) (scatter mean_OP_gend trials_in_tc if tc==3 [w= num_OP_obs], mfcolor(bg)  msymbol(Dh)) (scatter mean_OP_gend trials_in_tc if tc==4 [w= num_OP_obs], mfcolor(bg) msymbol(Sh)), legend(label(1 "Lilongwe Center 1") label(2 "Lilongwe Center 2") label(3 "Blantyre"));
graph export "$rootdir/Documents/graphs/figA1_gender_bysession.png", replace;
graph export "$rootdir/Documents/graphs/figA1_gender_bysession.pdf", replace;

twoway  (scatter OP_male_qualrate trials_in_tc [w=OP_males_all], msymbol(Oh) xtitle("Session") ytitle("Qualification rate") title("Fig A2: CA qualification rate over time"))  (scatter OP_female_qualrate trials_in_tc [w=OP_females_all], msymbol(Dh)), legend(label(1 "Male CAs") label(2 "Female CAs"));
graph export "$rootdir/Documents/graphs/figA2_performance_bysession.png", replace;
graph export "$rootdir/Documents/graphs/figA2_performance_bysession.pdf", replace;
*********************************************************************************;
*** Appendix Figure A3: Referral Qualifies, By Female CA performance ***;
*********************************************************************************;
twoway (lpoly REF_alt_nofeedback_qual OP_alt_nofeedback_score if OP_gender==2&treat_female==1&treat_perf==0, lpattern(dash) ) 
	(lpoly REF_alt_nofeedback_qual OP_alt_nofeedback_score if OP_gender==2&treat_male==1&treat_perf==0)
	(lpoly REF_alt_nofeedback_qual OP_alt_nofeedback_score if OP_gender==2&treat_female==1&treat_perf==1, lpattern(dash) lwidth(1.15)) 
	(lpoly REF_alt_nofeedback_qual OP_alt_nofeedback_score if OP_gender==2&treat_male==1&treat_perf==1, lwidth(1.15)), yscale(range(0,0.8))legend(label (1 "Women who must refer women," "fixed fees") 
	label( 2 "Women who must refer men," "fixed fees") label(3 "Women who must refer women," "performance pay") label( 4 "Women who must refer men," "performance pay"))   xtitle("CA's overall (corrected) score") 
	ytitle("Referral qualifies")title("Figure A3: Referral Qualifies, by Female CA performance") 
	graphregion(fcolor(white)) legend(region(lwidth(none)));
graph export "$rootdir/Documents/graphs/figA3_referral_qualifies_by_female_CA_performance.png", replace;
graph export "$rootdir/Documents/graphs/figA3_referral_qualifies_by_female_CA_performance.pdf", replace;


ksmirnov REF_alt_nofeedback_score if treat_either==0 & OP_gender==2, by(treat_female);

log close

	




