
global data "C:/Users/cbaehr/Box Sync/MacArthur_Winter2019"
global results "C:/Users/cbaehr/Box Sync/MacArthur_Winter2019/results"

use "$data/processed_data/panel.dta", clear

bysort cell_id (year): gen ndvi_2000 = ndvi[21]
by cell_id: egen proximity_control = max(trt)

replace trt = 0 if missing(trt)

outreg2 using "$results/summary_stats.doc", replace sum(log)
rm "$results/summary_stats.txt"

***

cgmreg per_loss trt, cluster(dist_name year)
outreg2 using "$results/main_models.doc", replace noni nocons addtext("Year FEs", N, "Grid cell FEs", N)

reghdfe per_loss trt, cluster(dist_name year) absorb(year)
outreg2 using "$results/main_models.doc", append noni nocons addtext("Year FEs", Y, "Grid cell FEs", N)

reghdfe per_loss trt, cluster(dist_name year) absorb(cell_id year)
outreg2 using "$results/main_models.doc", append noni nocons addtext("Year FEs", Y, "Grid cell FEs", Y)

reghdfe per_loss trt mintemp meantemp maxtemp minprecip meanprecip maxprecip, cluster(dist_name year) absorb(cell_id year)
outreg2 using "$results/main_models.doc", append noni nocons addtext("Year FEs", Y, "Grid cell FEs", Y)

rm "$results/main_models.txt"
