***KSD HI Credential Data Cleaning

cd "C:\Users\hpoquett\Desktop\KSD Data"
insheet using Copy_of_HI.csv

ssc install egenmore
ssc install catplot
***Create consistent values for CRED_CDE
tab cred_cde

*Consolidating duplicate certification codes
foreach mhi in "MHIF" {
replace cred_cde = "MHI" if index(cred_cde, "`mhi'")
}

foreach phi in "PHIF" {
replace cred_cde = "PHI" if index(cred_cde, "`phi'")
}

foreach psd in "PSDF" {
replace cred_cde = "PSD" if index(cred_cde, "`psd'")
}

***Drop unnecessary certifications that are not relevant to D/HH/HI
drop if cred_cde == "AJHI"
drop if cred_cde == "B02"
drop if cred_cde == "B05"
drop if cred_cde == "B10"
drop if cred_cde == "C02"
drop if cred_cde == "MSD"
drop if cred_cde == "MSDF"
drop if cred_cde == "MSDI"
drop if cred_cde == "PSD"
drop if cred_cde == "SSMF"
drop if cred_cde == "TECD"
drop if cred_cde == "WCDI"

***Identify school years in which a teacher's certification is valid
*Reformat dates in Stata format

foreach date in effec_dt exp_dt {
gen `date'_num = date(`date', "MDY")
format `date'_num %td
drop `date'
rename `date'_num `date'
}

*Generate variables indicating school year and month certificate became effective or expired.
foreach date in effec_dt exp_dt {
gen `date'_year = year(`date')
gen `date'_month = month(`date')
}

*Aligning certification dates with school year
replace effec_dt_year = effec_dt_year + 1 if effec_dt_month>=5 & effec_dt_month<=12

replace exp_dt_year = exp_dt_year + 1 if exp_dt_month>=10 & exp_dt_month<=12

*Drop observations missing both effective date and expiration date
drop if mi(effec_dt_year) & mi(exp_dt_year)

*Assign same year to both effective and expired dates if one is missing.
replace effec_dt_year = exp_dt_year if effec_dt_year==. & exp_dt_year!=.

replace exp_dt_year = effec_dt_year if exp_dt_year==. & effec_dt_year!=.

drop *month

*Create observation row for every combination of teacher, certification code, and expiration date.
fillin psn_id cred_cde exp_dt_year

gen effec_year = effec_dt_year if _fillin==0
gen exp_year = exp_dt_year if _fillin==0

foreach yr in effec_year exp_year {
egen max_`yr' = max(`yr'), by(psn_id cred_cde)
}

gen school_year = exp_dt_year

drop if school_year<max_effec_year
drop if school_year>max_exp_year

drop max* _fillin exp_year effec_year effec_dt_year exp_dt_year effec_dt exp_dt

drop if school_year<2006
drop if school_year>2017

***Tab number of certifications in each cert area by year:
tab cred_cde school_year

***Create Labels for graphs

label variable school_year "School year"
label variable cred_cde "Certification Type"

***Graph number of certifications by year
*Generate indicators for each type of certification
gen cohi = 0
replace cohi = 1 if cred_cde == "COHI"
gen e02 = 0
replace e02 = 1 if cred_cde == "E02"
gen e05 = 0
replace e05 = 1 if cred_cde == "E05"
gen e15 = 0
replace e15 = 1 if cred_cde== "E15"
gen himr = 0
replace himr = 1 if cred_cde == "HIMR"
gen khi = 0
replace khi = 1 if cred_cde == "KHI"
gen khii = 0
replace khii = 1 if cred_cde == "KHII"
gen khil = 0
replace khil = 1 if cred_cde == "khil"
gen khir = 0
replace khir = 1 if cred_cde == "KHIR"
gen khs = 0
replace khs = 1 if cred_cde == "KHS"
gen khsi = 0
replace khsi = 1 if cred_cde == "KHSI"
gen mhi = 0
replace mhi = 1 if cred_cde == "MHI"
gen phi = 0
replace phi = 1 if cred_cde == "PHI"
gen s15 = 0
replace s15 = 1 if cred_cde == "S15"
gen t10 = 0
replace t10 = 1 if cred_cde == "T10"
gen tehi = 0
replace tehi = 1 if cred_cde == "TEHI"
gen tphi = 0
replace tphi = 1 if cred_cde == "TPHI"
gen xhi = 0
replace xhi = 1 if cred_cde == "XHI"
gen xhip = 0
replace xhip = 1 if cred_cde == "XHIP"
gen xwhi = 0
replace xwhi = 1 if cred_cde == "XWHI"


*Create indicator for any certification
gen cred=0 
replace cred = 1 if  cohi==1 | e02==1 | e05==1 | himr==1 | khi==1 | khii==1 | khil==1 | khir==1 | khs==1 | khsi==1 | mhi==1 | phi==1 | s15==1 | t10==1 | tehi==1 | tphi==1 | xhi==1 | xhip==1 | xwhi==1
tab cred school_year if cred==1
*Graph total number of certifications per year
twoway histogram school_year if cred==1, frequency discrete title("Total number of Deaf/Hard of Hearing/Hearing Impaired" "Teaching Certifications active" "in Kentucky between 2006-2017")

*Graph number of certifications in 2016 by cert type
catplot cred_cde if school_year==2016, recast(bar) title("Number of certifications in 2016 by certification type")
tab cred_cde if school_year==2016

***Graph number of certifications per year by certification for three most utilized certifications
tab khi school_year
twoway histogram school_year if khi==1, frequency discrete title("Number of Professional Certificates" "Teaching Exceptional Children" "Hearing Impaired, Grades Primary Through 12")

tab phi school_year
twoway histogram school_year if phi==1, frequency discrete title("Number of Provisional Certificates" "Teachers Of Exceptional Children" "Hearing Impaired, Grades K-12")

tab xwhi school_year
twoway histogram school_year if xwhi==1, frequency discrete title("Waiver For Teaching Exceptional Children" "Hearing Impaired, Grades Primary Through 12, 1 Year")


*Graph proportion of certifications between 2012-2016

gen other = 0
replace other =1 if cohi==1 | e02==1 | e05==1 | himr==1 | khii==1 | khil==1 | khir==1 | khs==1 | khsi==1 | mhi==1 | s15==1 | t10==1 | tehi==1 | tphi==1 | xhi==1 | xhip==1
graph pie khi phi xwhi other if school_year==2012, title("Distribution of Certifications in 2012") plabel(_all name) legend(off)
graph pie khi phi xwhi other if school_year==2013, title("Distribution of Certifications in 2013") plabel(_all name) legend(off)
graph pie khi phi xwhi other if school_year==2014, title("Distribution of Certifications in 2014") plabel(_all name) legend(off)
graph pie khi phi xwhi other if school_year==2015, title("Distribution of Certifications in 2015") plabel(_all name) legend(off)
graph pie khi phi xwhi other if school_year==2016, title("Distribution of Certifications in 2016") plabel(_all name) legend(off)
graph pie khi phi xwhi other if school_year==2017, title("Distribution of Certifications in 2017") plabel(_all name) legend(off)

*Tab of Certs with sign language
tab cred_cde school_year if cred_cde=="KHS"
tab cred_cde school_year if cred_cde=="KHSI"
tab cred_cde school_year if cred_cde=="XHIP"

*Graph of certs with sign language
histogram school_year if khs==1 & school_year<=2017, frequency discrete title("Number of Professional Certificates for Teachers" "of Exceptional Children--Hearing Impaired" "with Sign Proficiency, Grades Primary through 12")

**Tab of emergency certifications
tab xhi school_year
tab xhip school_year


***Identify the number of teachers with active licenses in each year
*Tag duplicate teacher ids and count distinct teachers per year

sort psn_id 
egen tag_psn_06=tag(psn_id) if school_year==2006
egen tag_psn_07=tag(psn_id) if school_year==2007
egen tag_psn_08=tag(psn_id) if school_year==2008
egen tag_psn_09=tag(psn_id) if school_year==2009
egen tag_psn_10=tag(psn_id) if school_year==2010
egen tag_psn_11=tag(psn_id) if school_year==2011
egen tag_psn_12=tag(psn_id) if school_year==2012
egen tag_psn_13=tag(psn_id) if school_year==2013
egen tag_psn_14=tag(psn_id) if school_year==2014
egen tag_psn_15=tag(psn_id) if school_year==2015
egen tag_psn_16=tag(psn_id) if school_year==2016

count if tag_psn_06==1
count if tag_psn_07==1
count if tag_psn_08==1
count if tag_psn_09==1
count if tag_psn_10==1
count if tag_psn_11==1
count if tag_psn_12==1
count if tag_psn_13==1
count if tag_psn_14==1
count if tag_psn_15==1
count if tag_psn_16==1

egen count_psn_06A = count(tag_psn_06) if tag_psn_06==1
tab count_psn_06A
egen count_psn_07 = count(tag_psn_07) if tag_psn_07==1
tab count_psn_07
egen count_psn_08 = count(tag_psn_08) if tag_psn_08==1
tab count_psn_08
egen count_psn_09 = count(tag_psn_09) if tag_psn_09==1
tab count_psn_09
egen count_psn_10 = count(tag_psn_10) if tag_psn_10==1
tab count_psn_10
egen count_psn_11 = count(tag_psn_11) if tag_psn_11==1
tab count_psn_11
egen count_psn_12 = count(tag_psn_12) if tag_psn_12==1
tab count_psn_12
egen count_psn_13 = count(tag_psn_13) if tag_psn_13==1
tab count_psn_13
egen count_psn_14 = count(tag_psn_14) if tag_psn_14==1
tab count_psn_14
egen count_psn_15 = count(tag_psn_15) if tag_psn_15==1
tab count_psn_15
egen count_psn_16 = count(tag_psn_16) if tag_psn_16==1
tab count_psn_16

*Tag duplicate teachers to identify type of multiple certifications
duplicates tag psn_id if school_year==2006, generate(dup_2006)
list cred_cde if dup_2006==1
tab psn_id cred_cde if dup_2006==1
tab psn_id if school_year==2006

duplicates tag psn_id if school_year==2007, generate(dup_2007)
list cred_cde if dup_2007==1
tab psn_id cred_cde if dup_2007==1
tab psn_id if school_year==2007

duplicates tag psn_id if school_year==2008, generate(dup_2008)
list cred_cde if dup_2008==1
tab psn_id cred_cde if dup_2008==1
tab psn_id if school_year==2008

duplicates tag psn_id if school_year==2009, generate(dup_2009)
list cred_cde if dup_2009==1
tab psn_id cred_cde if dup_2009==1
tab psn_id if school_year==2009


*has triplicate, use quietly code to identify triplicates
duplicates tag psn_id if school_year==2010, generate(dup_2010)
list cred_cde if dup_2010==1
tab psn_id cred_cde if dup_2010==1
tab psn_id if school_year==2010

sort psn_id school_year
quietly by psn_id school_year: gen dup = cond(_N==1, 0,_n)
tabulate dup if school_year==2010
tabulate cred_cde if dup>=1 & school_year==2010


duplicates tag psn_id if school_year==2011, generate(dup_2011)
list cred_cde if dup_2011==1
tab psn_id cred_cde if dup_2011==1
tab psn_id if school_year==2011

duplicates tag psn_id if school_year==2012, generate(dup_2012)
list cred_cde if dup_2012==1
tab psn_id cred_cde if dup_2012==1
tab psn_id if school_year==2012

duplicates tag psn_id if school_year==2013, generate(dup_2013)
list cred_cde if dup_2013==1
tab psn_id cred_cde if dup_2013==1
tab psn_id if school_year==2013

duplicates tag psn_id if school_year==2014, generate(dup_2014)
list cred_cde if dup_2014==1
tab psn_id cred_cde if dup_2014==1
tab psn_id if school_year==2014

duplicates tag psn_id if school_year==2015, generate(dup_2015)
list cred_cde if dup_2015==1
tab psn_id cred_cde if dup_2015==1
tab psn_id if school_year==2015

duplicates tag psn_id if school_year==2016, generate(dup_2016)
list cred_cde if dup_2016==1
tab psn_id cred_cde if dup_2016==1
tab psn_id if school_year==2016

duplicates tag psn_id if school_year==2017, generate(dup_2017)
list cred_cde if dup_2017==1
tab psn_id cred_cde if dup_2017==1
tab psn_id if school_year==2017

tab psn_id if school_year==2017 & xwhi==0 & khir==0 & xhi==0
tab cred_cde if school_year==2017



 