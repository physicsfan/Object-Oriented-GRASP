module physical_constants

  ! Preamble:
  ! =========
  ! Physical constants used throuout the OO-GRASP program.
  ! Everything is public here.

  use program_constants
  
  implicit none

  public

  ! Physics constants and units
  real(dp), parameter :: AMU_Value          = 1.6605402e-27_dp     ! kg
  real(dp), parameter :: Atmosphere_Pres    = 9.80665e+04_dp       ! Pa
  real(dp), parameter :: Avogadro           = 6.0221367e+23_dp     ! 1/mol
  real(dp), parameter :: Bohr_Magneton      = 9.2740154e-24_dp     ! J/T
  real(dp), parameter :: Bohr_Radius        = 5.29177249e-11_dp    ! m
  real(dp), parameter :: Boltzmann          = 1.380657e-23_dp      ! J/K
  real(dp), parameter :: c_Light            = 2.997924580e+08_dp   ! m/s
  real(dp), parameter :: Electron_Compton   = 2.42631058e-12_dp    ! m
  real(dp), parameter :: Electron_Angular   = 5.2729e-35_dp        ! J*s
  real(dp), parameter :: Electron_Charge    =-1.60217738e-19_dp    ! Coul
  real(dp), parameter :: Electron_Mass_Rest = 9.1093897e-31_dp     ! kg
  real(dp), parameter :: Electron_Moment    = 9.2847700e-24_dp     ! J/T
  real(dp), parameter :: Electron_Radius    = 2.81794092e-15_dp    ! m
  real(dp), parameter :: Faraday            = 9.6485309e+04_dp     ! C/mo
  real(dp), parameter :: G_Universal        = 6.67260e-11_dp       ! m^3/(s^2*kg)
  real(dp), parameter :: Light_Year         = 9.46073e+15_dp       ! m
  real(dp), parameter :: Mech_equiv_Heat    = 4.185e+03_dp         ! J/kcal
  real(dp), parameter :: Molar_Volume       = 0.02241410_dp        ! m^3/mol
  real(dp), parameter :: Neutron_Mass       = 1.6749286e-27_dp     ! kg
  real(dp), parameter :: Permeability       = 1.25663706143e-06_dp ! H/m
  real(dp), parameter :: Permittivity       = 8.85418781762e-12_dp ! F/m
  real(dp), parameter :: Planck_Const       = 6.6260754e-34_dp     ! J*s
  real(dp), parameter :: Proton_Mass        = 1.6726230e-27_dp     ! kg
  real(dp), parameter :: Proton_Moment      = 1.41060761e-26_dp    ! J/T
  real(dp), parameter :: Quantum_charge_r   = 4.13556e+12_dp       ! J*s/C
  real(dp), parameter :: Rydberg_inf        = 1.0973731534e+07_dp  ! 1/m
  real(dp), parameter :: Rydberg_Hydrogen   = 1.09678e+07_dp       ! 1/m
  real(dp), parameter :: Std_Atmosphere     = 1.01325e+05_dp       ! Pa
  real(dp), parameter :: Stefan_Boltzmann   = 5.67050e-08_dp       ! W/(m^2*K^4)
  real(dp), parameter :: Thomson_cross_sect = 6.65165e-29_dp       ! m^2
  real(dp), parameter :: Universal_Gas_C    = 8.314510_dp          ! J/mol*K

  ! Astronomy Constants and units
  real(dp), parameter :: AU_Earth_Sun       = 1.4959787e+11_dp     ! m
  real(dp), parameter :: Anomal_Month       = 27.5546_dp           ! days
  real(dp), parameter :: Anomal_Year        = 365.2596_dp          ! days
  real(dp), parameter :: Dracon_Month       = 27.2122_dp           ! days
  real(dp), parameter :: Earth_G            = 9.80665_dp           ! m/s^2
  real(dp), parameter :: Earth_Mass         = 5.974e+24_dp         ! kg
  real(dp), parameter :: Earth_Radius_Eq    = 6.37814e+6_dp        ! m
  real(dp), parameter :: Earth_Radius_Mean  = 6.3781e+6_dp         ! m
  real(dp), parameter :: Earth_Radius_Polar = 6.356755e+6_dp       ! m
  real(dp), parameter :: Julian_Year        = 365.25_dp            ! days
  real(dp), parameter :: Rotation_Day       = 23.93447222_dp       ! hours
  real(dp), parameter :: Siderial_Day       = 23.93446944_dp       ! hours
  real(dp), parameter :: Siderial_Month     = 27.3217_dp           ! days
  real(dp), parameter :: Siderial_Ratio     = 1.0027379092558_dp
  real(dp), parameter :: Siderial_Year      = 365.2564_dp          ! days
  real(dp), parameter :: Solar_Day          = 24.06571111_dp       ! hours
  real(dp), parameter :: Synodic_Month      = 29.5306_dp           ! days
  real(dp), parameter :: Tropical_Year      = 365.2422_dp          ! days

end module physical_constants
  
