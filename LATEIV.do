** Compliance LATE Analysis (fraction)**
import delimited "~/data/IVdata.csv", clear // You can obtain it from running MainAnalysis.R file
replace complier = 1 if district == "Tonkolili"
replace complier = 0.972 if district == "Falaba"
replace complier = 0.961 if district == "Karene"
replace complier = 0.927 if district == "Kono"
replace complier = 0.891 if district == "Pujehun"

encode facility_type, generate(Ftype)


reg normconsump hf_pk quarterid Ftype complier
est store a

ivregress 2sls normconsump hf_pk quarterid Ftype (complier=z), first
est store aa

ivregress 2sls normconsump hf_pk quarterid Ftype (complier=z)
est store aaa
estat endogenous
estat firststage
mat fstat=r(singleresults)
estadd scalar fs=fstat[1,4]

esttab a aa aaa using table1.tex, collabels(none)  cells(b(star fmt(3) vacant({--})))  label replace stats(N r2 fs, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")  labels(`"Observations"' `"\(R^2\)"' `"\(F\)"')) drop(_cons)
