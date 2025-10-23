MODULE constants
  use nrtype
  implicit none

  DOUBLE PRECISION, parameter	:: sec_per_day=86400.0_dp		!seconds in a day
  DOUBLE PRECISION, parameter	:: sec_per_hour=3600.0_dp		!seconds in an hour
  DOUBLE PRECISION, parameter	:: cfs_cms=0.0283168_dp  		!cubic feet per second to cubic meters per second
  DOUBLE PRECISION, parameter	:: c_p=1.013e-3_dp			!specific heat of air at constant pressure (MJ kg-1 oC-1)
  DOUBLE PRECISION, parameter	:: l_v=2.501_dp				!latent heat of vaporization (MJ kg-1)
  DOUBLE PRECISION, parameter	:: tadj=0.002361_dp			!latent heat adjustment for temp above 0oC (MJ kg-1)
  DOUBLE PRECISION, parameter	:: e=0.622_dp				!ratio of weight of water to dry air
  DOUBLE PRECISION, parameter	:: sbc=4.903e-9_dp			!stefan-boltzmann constant (MJ K-4 m-2 day-1)
  DOUBLE PRECISION, parameter	:: gsc=0.0820_dp			!solar constant (MJ m-2 min-1)
  DOUBLE PRECISION, parameter	:: slope_svpc_a=0.04145_dp		!constant for slope of saturation vapor pressure curve
  DOUBLE PRECISION, parameter	:: slope_svpc_b=0.06088_dp		!exponent contant for slope of saturation vapor pressure curve
  DOUBLE PRECISION, parameter	:: e_sa=0.6108_dp			!saturation vapor pressure constant (Tetens 1930) gives e_s in kPa
  DOUBLE PRECISION, parameter	:: e_sb=17.27_dp			!saturation vapor pressure constant
  DOUBLE PRECISION, parameter	:: e_sc=237.3_dp			!saturation vapor pressure constant
  DOUBLE PRECISION, parameter	:: sun_e_inv_d=0.033_dp			!constant for earth-sun inverse distance
  DOUBLE PRECISION, parameter	:: sol_dec_a=0.409_dp			!constant for solar declination calc
  DOUBLE PRECISION, parameter	:: sol_dec_b=1.39_dp			!constant for solar declination calc
  DOUBLE PRECISION, parameter	:: clear_sky_a=0.75_dp			!constant for clear sky shortwave
  DOUBLE PRECISION, parameter	:: clear_sky_b=2e-5_dp			!constant for clear sky shortwave
  DOUBLE PRECISION, parameter	:: w_to_mj=0.0864_dp			!conversion of W m-2 to MJ m-2
  DOUBLE PRECISION, parameter	:: net_lw_a=0.34_dp			!constant for net longwave out
  DOUBLE PRECISION, parameter	:: net_lw_b=0.14_dp			!constant for net longwave out
  DOUBLE PRECISION, parameter	:: net_lw_c=1.35_dp			!constant for net longwave out
  DOUBLE PRECISION, parameter	:: net_lw_d=0.35_dp			!constant for net longwave out
  DOUBLE PRECISION, parameter	:: sfc_pres_a=33.86_dp			!constant for sfc pressure calc (snow-17)
  DOUBLE PRECISION, parameter	:: sfc_pres_b=29.9_dp			!constant for sfc pressure calc (snow-17)
  DOUBLE PRECISION, parameter	:: sfc_pres_c=0.335_dp			!constant for sfc pressure calc (snow-17)
  DOUBLE PRECISION, parameter	:: sfc_pres_d=0.00022_dp		!constant for sfc pressure calc (snow-17)
  DOUBLE PRECISION, parameter	:: sfc_pres_e=2.4_dp			!constant for sfc pressure calc (snow-17)

END MODULE constants
