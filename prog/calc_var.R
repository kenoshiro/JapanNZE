# Final energy
df$all %<>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Liq_Oil','Fin_Ene_SolidsCoa','Fin_Ene_Gas_Fos'),name_new='Fin_Ene_Fos')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Ind_Liq_Bio','Fin_Ene_Ind_SolidsBio'),name_new='Fin_Ene_Ind_Bio')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Ind_Liq_Hyd_syn','Fin_Ene_Ind_Gas_Hyd_syn'),name_new='Fin_Ene_Ind_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Res_and_Com_Liq_Hyd_syn','Fin_Ene_Res_and_Com_Gas_Hyd_syn'),name_new='Fin_Ene_Res_and_Com_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Liq_Hyd_syn','Fin_Ene_Tra_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Pss_Liq_Hyd_syn','Fin_Ene_Tra_Pss_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Pss_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Fre_Liq_Hyd_syn','Fin_Ene_Tra_Fre_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Fre_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Roa_Liq_Hyd_syn','Fin_Ene_Tra_Roa_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Roa_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Rai_Liq_Hyd_syn','Fin_Ene_Tra_Rai_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Rai_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Shi_Liq_Hyd_syn','Fin_Ene_Tra_Shi_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Shi_Liq_and_Gas_Hyd_syn')) %>% 
    bind_rows(fcalc_sum(vars=c('Fin_Ene_Tra_Avi_Liq_Hyd_syn','Fin_Ene_Tra_Avi_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Avi_Liq_and_Gas_Hyd_syn'))

df$all %<>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Ele'),name_new='Fin_Ene_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Fos'),name_new='Fin_Ene_Share_Fos')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Bio'),name_new='Fin_Ene_Share_Bio')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Hyd'),name_new='Fin_Ene_Share_Hyd')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Hyd_syn'),name_new='Fin_Ene_Share_Hyd_syn'))

# Electrification
df$all %<>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Ele'),name_new='Fin_Ene_Ind_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Ele'),name_new='Fin_Ene_Tra_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Ele'),name_new='Fin_Ene_Res_and_Com_Share_Ele')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Hyd','Fin_Ene_Hyd_syn'),name_new='Fin_Ene_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Ind','Fin_Ene_Ind_Hyd','Fin_Ene_Ind_Liq_and_Gas_Hyd_syn'),name_new='Fin_Ene_Ind_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Tra','Fin_Ene_Tra_Hyd','Fin_Ene_Tra_Liq_and_Gas_Hyd_syn'),name_new='Fin_Ene_Tra_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene_Res_and_Com','Fin_Ene_Res_and_Com_Hyd','Fin_Ene_Res_and_Com_Liq_and_Gas_Hyd_syn'),name_new='Fin_Ene_Res_and_Com_Share_Hyd_Car')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_SolidsBio'),name_new='Fin_Ene_Share_Bio')) %>% 
    bind_rows(fcalc_share(vars=c('Fin_Ene','Fin_Ene_Heat'),name_new='Fin_Ene_Share_Heat'))

# Power generation
df$all %<>% 
    bind_rows(fcalc_sum(vars=c('Sec_Ene_Ele_SolarPV','Sec_Ene_Ele_Win','Sec_Ene_Ele_Oce'),name_new='Sec_Ene_Ele_VRE')) %>% 
    bind_rows(fcalc_sum(vars=c('Sec_Ene_Ele_NonBioRen','Sec_Ene_Ele_Bio'),name_new='Sec_Ene_Ele_Ren'))
df$all %<>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_VRE'),name_new='Sec_Ene_Ele_Share_VRE')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_NonBioRen'),name_new='Sec_Ene_Ele_Share_NonBioRen')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Ren'),name_new='Sec_Ene_Ele_Share_Ren')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Fos'),name_new='Sec_Ene_Ele_Share_Fos')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Fos_wo_CCS'),name_new='Sec_Ene_Ele_Share_Fos_wo_CCS')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Fos_w_CCS'),name_new='Sec_Ene_Ele_Share_Fos_w_CCS')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Nuc'),name_new='Sec_Ene_Ele_Share_Nuc')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Bio'),name_new='Sec_Ene_Ele_Share_Bio')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Bio_wo_CCS'),name_new='Sec_Ene_Ele_Share_Bio_wo_CCS')) %>% 
    bind_rows(fcalc_share(vars=c('Sec_Ene_Ele','Sec_Ene_Ele_Bio_w_CCS'),name_new='Sec_Ene_Ele_Share_Bio_w_CCS'))

# Primary energy
df$all %<>% bind_rows(fcalc_sum(vars=c('Prm_Ene_NonBioRen','Prm_Ene_Bio'),name_new='Prm_Ene_Ren'))
df$all %<>% bind_rows(fcalc_share(vars=c('Prm_Ene','Prm_Ene_Ren'),name_new='Prm_Ene_Share_Ren')) %>% 
    bind_rows(fcalc_share(vars=c('Prm_Ene','Prm_Ene_NonBioRen'),name_new='Prm_Ene_Share_NonBioRen')) %>% 
    bind_rows(fcalc_share(vars=c('Prm_Ene','Prm_Ene_Bio'),name_new='Prm_Ene_Share_Bio')) %>% 
    bind_rows(fcalc_share(vars=c('Prm_Ene','Prm_Ene_Nuc'),name_new='Prm_Ene_Share_Nuc')) %>% 
    bind_rows(fcalc_share(vars=c('Prm_Ene','Prm_Ene_Fos'),name_new='Prm_Ene_Share_Fos'))

df$all %<>% 
    bind_rows(fcalc_share(vars=c('Prm_Ene','Prm_Ene_Oil','Prm_Ene_Coa','Prm_Ene_Gas','Prm_Ene_Sec_Ene_Trd'),name_new='Imp_Dep_Prm_Ene'))

# Emissions
df$all %<>% bind_rows(fcalc_sum(vars=c('Emi_CO2_Ene_Dem_Ind','Emi_CO2_Ene_Dem_AFO'),name_new='Emi_CO2_Ene_Dem_Ind_and_AFO')) %>% 
    bind_rows(fcalc_sum(vars=c('Emi_CO2_Ind_Pro','Emi_CO2_Oth','Emi_CH4_CO2-eq','Emi_N2O_CO2-eq','Emi_F-G'),name_new='Emi_Non_ene_CO2')) %>% 
    bind_rows(fcalc_sum(vars=c('Car_Seq_CCS_Fos','Car_Seq_CCS_Ind_Pro'),name_new='Car_Seq_CCS_Fos_and_Ind_Pro')) %>% 
    bind_rows(fcalc_sum(vars=c('Emi_CH4_CO2-eq','Emi_N2O_CO2-eq','Emi_F-G'),name_new='Emi_non_CO2'))

# Policy cost
df$all %<>% bind_rows(fcalc_cumulate(df_in=df$all,var='Pol_Cos_Add_Tot_Ene_Sys_Cos',name_new='Pol_Cos_Cum_Dis',intrate=0.03,p_year=2020,unit_new='billion US$',startyr=2021,endyr=2050)) %>% 
    bind_rows(fcalc_cumulate(df_in=df$all,var='GDP_MER',name_new='GDP_MER_Cum_Dis',intrate=0.03,p_year=2020,unit_new='billion US$',startyr=2021,endyr=2050))
df$all %<>% 
    bind_rows(fcalc_share(vars=c('GDP_MER_Cum_Dis','Pol_Cos_Cum_Dis'),name_new='Pol_Cos_Cum_Dis_per_GDP'))

df$all %<>% bind_rows(fcalc_share(df_in=df$all,var=c('GDP_MER','Pol_Cos_Add_Tot_Ene_Sys_Cos'),name_new='Pol_Cos_per_GDP',unit_new='%'))

df$all %<>% bind_rows(fcalc_sum(vars=c('Inv_Add_Ene_Sup_Ele_Bio_w_CCS','Inv_Add_Ene_Sup_Hyd_Bio_w_CCS','Inv_Add_Dir_Air_Cap'),name_new='Inv_Add_NETs'))

df$all %<>% bind_rows(fcalc_sum(vars=c('Imp_Ene_Prm_Ene_Coa','Imp_Ene_Prm_Ene_Gas','Imp_Ene_Prm_Ene_Oil','Imp_Ene_Sec_Ene','Imp_Emi_All'),name_new='Imp_Ene_and_Emi'))


df$all %<>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Sup_Ele',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Sup_Hyd',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Sup_Oth',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Dem_Ind',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Dem_Res_and_Com',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Dem_Tra',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Oth',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Sup_Ele_Bio_w_CCS',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Ene_Sup_Hyd_Bio_w_CCS',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_Dir_Air_Cap',startyr=2021,unit_new='billion US$')) %>% 
    bind_rows(fcalc_cumulate(var='Inv_Add_NETs',startyr=2021,unit_new='billion US$'))
