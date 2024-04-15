* Author: Eddie (Yidi) Wu
* ECON2400 Paper draft

clear all

* Need to install the sdid package
* ssc install sdid, replace

* Set WD and load data
* cd "path"

* Load data
import delim using "../data/sc_data.csv", clear

* Preprocessing
generate treated = 0
replace treated = 1 if country == "UK" & yearqtr >= 2016.50

encode(country), gen(country_id)

* Run SDID
#delimit ;
sdid rgdp_index country time_var treated, vce(placebo) reps(50)
		seed(987) method(sdid)
		graph g1on g1_opt(xtitle("") ylabel(-20(5)20) scheme(plotplainblind)) 
		g2_opt(ylabel(60(20)140) xlabel(1(5)67) ytitle("Real GDP per capita index") 
				xtitle("Time") scheme(plotplainblind));
		* graph_export(sdid_, .png)
#delimit cr

#delimit ;
sdid rhhgdi_index country time_var treated, vce(placebo) reps(50)
		seed(987) method(sdid)
		graph g1on g1_opt(xtitle("") ylabel(-20(5)20) scheme(plotplainblind)) 
		g2_opt(ylabel(60(20)140) xlabel(1(5)67) ytitle("Real gross disposable income per capita index") 
				xtitle("Time") scheme(plotplainblind));
		* graph_export(sdid_, .png)
#delimit cr

eststo sdid_1: sdid rgdp_index country time_var treated, vce(placebo) seed(987) reps(50)
eststo sdid_2: sdid rhhgdi_index country time_var treated, vce(placebo) seed(987) reps(50)
esttab sdid_1 sdid_2 using "../output/sdid_table.tex", starlevel ("*" 0.10 "**" 0.05 "***" 0.01) b(%-9.3f) se(%-9.3f) replace
