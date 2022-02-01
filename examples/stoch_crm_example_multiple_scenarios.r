# ------------------------------------------------------------------------------
# Code Purpose: stoch_crm() example usage for multiple scenarios with input
#               parameters originally stored in wide formatted tables.
#
# Two Goals:
#   1. Reshape input parameter tables used in the previous implementation to fit
#   stoch_crm()'s new interface. Potentially, code proposed here has wider
#   applicability, as the format of these tables is likely to be the preferred
#   tabular structure for users to gather input parameters for multiple
#   scenarios.
#
#   2. Application to multiple scenarios (species and windfarm/turbines features)
# ------------------------------------------------------------------------------

# --------------------------------------------------------- #
# ----      Reshaping into list-column data frames       ----
# --------------------------------------------------------- #
#
# Here embracing list-columns tibbles, but lists could be used instead

# bird features
bird_pars <- bird_pars_wide_example %>%
  dplyr::relocate(Flight, .after = dplyr::last_col()) %>%
  tidyr::pivot_longer(AvoidanceBasic:Prop_CRH_ObsSD) %>%
  dplyr::mutate(
    par = dplyr::if_else(grepl("SD|sd|Sd", name), "sd", "mean"),
    feature = gsub("SD|sd|Sd","", name)
  ) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(names_from = par, values_from = value) %>%
  tidyr::nest(pars = c(mean, sd)) %>%
  tidyr::pivot_wider(names_from = feature, values_from = pars) %>%
  tibble::add_column(prop_upwind = 0.5)

# bird densities - provided as mean and sd Parameters for Truncated Normal lower
# bounded at 0
dens_pars <- dens_tnorm_wide_example %>%
  tibble::add_column(
    dens_opt = rep("tnorm", nrow(.)),
    .after = 1
  ) %>%
  tidyr::pivot_longer(Jan:DecSD) %>%
  dplyr::mutate(
    par = dplyr::if_else(grepl("SD|sd|Sd", name), "sd", "mean"),
    month = gsub("SD|sd|Sd","", name)
  ) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(names_from = par, values_from = value) %>%
  tidyr::nest(mth_dens = c(month, mean, sd))

# FHD data from Johnson et al (2014) for the species under analysis
gen_fhd_boots <- generic_fhd_bootstraps[bird_pars$Species]

# seasons definitions (made up)
season_dt <- list(
  Arctic_Tern = data.frame(
    season_id = c("breeding", "feeding", "migrating"),
    start_month = c("May", "Sep", "Jan"),
    end_month = c("Aug", "Dec", "Apr")),
  Black_headed_Gull = data.frame(
    season_id = c("breeding", "feeding", "migrating"),
    start_month = c("Jan", "May", "Oct"),
    end_month = c("Apr", "Sep", "Dec")),
  Black_legged_Kittiwake = data.frame(
    season_id = c("breeding", "feeding", "migrating"),
    start_month = c("Dec", "Mar", "Sep"),
    end_month = c("Feb", "Aug", "Nov"))
)


# turbine parameters
## address operation parameters first
trb_opr_pars <- turb_pars_wide_example %>%
  dplyr::select(TurbineModel, JanOp:DecOpSD) %>%
  tidyr::pivot_longer(JanOp:DecOpSD) %>%
  dplyr::mutate(
    month = substr(name, 1, 3),
    par = dplyr::case_when(
      grepl("SD|sd|Sd", name) ~ "sd",
      grepl("Mean|MEAN|mean", name) ~ "mean",
      TRUE ~ "pctg"
    )
  ) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(names_from = par, values_from = value) %>%
  tidyr::nest(
    wind_avbl = c(month, pctg),
    trb_dwntm = c(month, mean, sd)
  )

## address turbine features and subsequently merge operation parameters
trb_pars <- turb_pars_wide_example %>%
  dplyr::select(TurbineModel:windSpeedSD ) %>%
  dplyr::relocate(RotorSpeedAndPitch_SimOption, .after = 1) %>%
  tidyr::pivot_longer(RotorRadius:windSpeedSD) %>%
  dplyr::mutate(
    par = dplyr::if_else(grepl("SD|sd|Sd", name), "sd", "mean"),
    feature = gsub("(SD|sd|Sd)|(Mean|MEAN|mean)","", name)
  ) %>%
  dplyr::select(-name) %>%
  tidyr::pivot_wider(names_from = par, values_from = value) %>%
  tidyr::nest(pars = c(mean, sd)) %>%
  tidyr::pivot_wider(names_from = feature, values_from = pars) %>%
  dplyr::left_join(., trb_opr_pars)

# windspeed, rotation speed and blade pitch relationship
wndspd_rtn_ptch_example

# windfarm parameters
wf_pars <- data.frame(
  wf_id = c("wf_1", "wf_2"),
  n_turbs = c(200, 400),
  wf_width = c(4, 10),
  wf_lat = c(55.8, 55.0),
  td_off = c(2.5, 2),
  large_array_corr = c(FALSE, TRUE)
)


# -------------------------------------------------------------- #
# ----      Run stoch_crm() for multiple scenarios           ----
# -------------------------------------------------------------- #

# Set up scenario combinations
scenarios_specs <- tidyr::expand_grid(
  spp = bird_pars$Species,
  turb_id = trb_pars$TurbineModel,
  wf_id = wf_pars$wf_id
) %>%
  #tidyr::unite("scenario_id", remove = FALSE, sep = " | ")
  tibble::add_column(
    scenario_id = paste0("scenario_", 1:nrow(.)),
    .before = 1)


# Set up progress bar for the upcoming iterative mapping step
pb <- progress::progress_bar$new(
  format = "Running Scenario: :what [:bar] :percent eta: :eta",
  width = 100,
  total = nrow(scenarios_specs)
  )

# Map stoch_crm() to each scenario specification via purrr::pmap
outputs <- scenarios_specs %>%
  purrr::pmap(function(scenario_id, spp, turb_id, wf_id, ...){

    pb$tick(tokens = list(what = scenario_id))

    # params for current species
    c_spec <- bird_pars %>%
      dplyr::filter(Species == {{spp}}) # {{}} to avoid issues with data masking

    # density for current species
    c_dens <- dens_pars %>%
      dplyr::filter(Species == {{spp}})

    # params for current turbine scenario
    c_turb <- trb_pars %>%
      dplyr::filter(TurbineModel == {{turb_id}})

    # params for current windfarm scenario
    c_wf <- wf_pars %>%
      dplyr::filter(wf_id == {{wf_id}})

    # inputs in list-columns need to be unlisted, either via `unlist()` or
    # indexing `[[1]]`
    # switching off `verbose`, otherwise console will be
    # cramped with log messages
    stoch_crm(
      model_options = c(1, 2, 3),
      n_iter = 1000,
      flt_speed_pars = c_spec$Flight_Speed[[1]],
      body_lt_pars = c_spec$Body_Length[[1]],
      wing_span_pars = c_spec$Wingspan[[1]],
      avoid_bsc_pars = c_spec$AvoidanceBasic[[1]],
      avoid_ext_pars = c_spec$AvoidanceExtended[[1]],
      noct_act_pars = c_spec$Nocturnal_Activity[[1]],
      prop_crh_pars = c_spec$Prop_CRH_Obs[[1]],
      bird_dens_opt = c_dens$dens_opt,
      bird_dens_dt = c_dens$mth_dens[[1]],
      flight_type = c_spec$Flight,
      prop_upwind = c_spec$prop_upwind,
      gen_fhd_boots = gen_fhd_boots[[spp]],
      n_blades = c_turb$Blades,
      rtr_radius_pars = c_turb$RotorRadius[[1]],
      air_gap_pars = c_turb$HubHeightAdd[[1]],
      bld_width_pars = c_turb$BladeWidth[[1]],
      rtn_pitch_opt = c_turb$RotorSpeedAndPitch_SimOption,
      bld_pitch_pars = c_turb$Pitch[[1]],
      rtn_speed_pars = c_turb$RotationSpeed[[1]],
      windspd_pars = c_turb$windSpeed[[1]],
      rtn_pitch_windspd_dt = wndspd_rtn_ptch_example,
      trb_wind_avbl = c_turb$wind_avbl[[1]],
      trb_downtime_pars = c_turb$trb_dwntm[[1]],
      wf_n_trbs = c_wf$n_turbs,
      wf_width = c_wf$wf_width,
      wf_latitude = c_wf$wf_lat,
      tidal_offset = c_wf$td_off,
      lrg_arr_corr = c_wf$large_array_corr,
      verbose = FALSE,
      seed = 1234,
      out_format = "summaries",
      out_sampled_pars = FALSE,
      out_period = "seasons",
      season_specs = season_dt[[spp]],
      log_file = NULL
    )
  })

# close progress bar
pb$terminate()

# identify elements of output list
names(outputs) <- scenarios_specs$scenario_id

outputs



