
global data "Box Sync/MacArthur_Winter2019"

use "$data/processed_data/panel.dta", clear

bysort cell_id (year): gen ndvi_2000 = ndvi[21]
by cell_id: egen proximity_control = max(trt)

replace trt = 0 if missing(trt)

reghdfe per_loss trt proximity_control ndvi_2000 ndvi_pretrend wdpapct_2000 concessionpct_all ///
	plantation_pct mintemp meantemp maxtemp minprecip meanprecip maxprecip gpw ntl slope ///
	elevation urbtravtime i.year, cluster(dist_name year) absorb(cell_id)

