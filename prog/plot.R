mytheme <- list()
mytheme$theme_set1 <- theme_bw()+theme(text=element_text(family='Helvetica'),
                                       plot.title=element_text(size=8.5),
                                       panel.grid=element_blank(),
                                       panel.border=element_blank(),
                                       axis.line=element_line(color='black',linewidth=.3),
                                       axis.ticks=element_line(color='black',linewidth=.3),
                                       axis.text=element_text(size=8),
                                       axis.title=element_text(size=9),
                                       legend.title=element_text(size=9),
                                       legend.text=element_text(size=8),
                                       strip.background=element_blank())
lst$scenario <- c('NZE-CR','NZE-AF','NZE-LD','NZE-IM','NZE-ET','NDC80')
df$scen_col <- c('NZE-CR'='#66C2A5','NZE-AF'='#FC8D62','NZE-LD'='#8DA0CB','NZE-IM'='#E78AC3','NZE-ET'='#A6D854','NDC80'='#B3B3B3')


# Fig.1 -----------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~ColorFill,~ColorLine,
                  'Emi_CO2_Ene_Sup','CO2-Energy Supply','moccasin','transparent',
                  'Emi_CO2_Ene_Dem_Ind','CO2-Industry','salmon','transparent',
                  'Emi_CO2_Ene_Dem_Res_and_Com','CO2-Buildings','lightsteelblue','transparent',
                  'Emi_CO2_Ene_Dem_Tra','CO2-Transportation','darkolivegreen2','transparent',
                  'Emi_CO2_Ind_Pro','CO2-Industrial\nprocesses','sandybrown','transparent',
                  'Emi_CO2_Oth','CO2-DACCS','lightskyblue3','transparent',
                  'Emi_non_CO2','Non-CO2','grey','transparent',
                  'Trd_Emi_All_Vol','Emission allowances','transparent','grey50')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col_fill <- as.character(df$var$ColorFill); names(col_fill) <- as.character(df$var$Variable)
col_line <- as.character(df$var$ColorLine); names(col_line) <- as.character(df$var$Variable)
p$emi_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    filter(Value!=0) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable,color=Variable),position='stack',stat='identity')+
    geom_hline(yintercept=0,color='grey20',linewidth=.3)+
    labs(x=NULL,y=expression(paste('GHG emissions (Mt-',CO[2],'eq ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col_fill,labels=leg,name=NULL)+
    scale_color_manual(values=col_line,labels=leg,name=NULL)+
    guides(fill=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Legend,~ColorFill,
                  'Emi_CO2_Ene_Sup','Energy Supply','moccasin',
                  'Emi_CO2_Ene_Dem_Ind','Industry','salmon',
                  'Emi_CO2_Ene_Dem_Res_and_Com','Buildings','lightsteelblue',
                  'Emi_CO2_Ene_Dem_Tra','Transportation','darkolivegreen2')
p$emi_CO2_sec <- filter(df$all,Year>=2020) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario),Legend=factor(Legend,levels=df$var$Legend)) %>% 
    ggplot()+
    geom_hline(yintercept=0,linewidth=.3)+
    geom_path(aes(x=Year,y=Value,color=Scenario),show.legend=T)+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    facet_grid(.~Legend)+
    labs(x=NULL,y=expression(paste(CO[2],' emissions (Mt-',CO[2],' ',yr^{`-1`},')')))+
    scale_color_manual(values=df$scen_col,name=NULL)+
    mytheme$theme_set1+theme(legend.position='bottom',axis.text.x=element_text(angle=45,hjust=1))+
    guides(color=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Legend,~Color,~Axis,
                  'Car_Cap_Dir_Air_Cap','Direct air','lightsteelblue','Cap',
                  'Car_Cap_Bio_Ene_Sup','Bioenergy','darkolivegreen2','Cap',
                  'Car_Cap_Ind_Pro','Industrial process','sandybrown','Cap',
                  'Car_Cap_Fos_Ene_Dem_Ind','Industry','salmon','Cap',
                  'Car_Cap_Fos_Ene_Sup','Energy supply','moccasin','Cap',
                  'Car_Uti','Utilization','thistle2','Seq',
                  'Car_Seq_Geo_Sto','Underground storage','grey','Seq')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$ccs_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=df$var$Variable),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    mutate(Value=if_else(Axis=='Seq',-Value,Value)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    geom_hline(yintercept=0,color='grey20',linewidth=.3)+
    labs(x=NULL,y=expression(paste('CCUS (Mt-',CO[2],' ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

df$var <- tribble(~Variable,~Legend,~Color,
                  'Car_Seq_Geo_Sto_Bio','BECCS','darkolivegreen2',
                  'Car_Seq_Geo_Sto_Dir_Air_Cap','DACCS','lightsteelblue')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$cdr_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(x=NULL,y=expression(paste('CDR (Mt-',CO[2],' ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',strip.background=element_blank(),axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

p$l1 <- get_legend(p$emi_CO2_sec+theme(legend.position='right'))
p$tmp1 <- plot_grid(p$emi_2050,p$emi_CO2_sec+theme(legend.position='none'),
                    nrow=1,axis='l',align='vh',rel_widths=c(1,1.3),labels=c('a','b'),label_size=12)
p$tmp2 <- plot_grid(p$ccs_2050,p$cdr_2050,p$l1,nrow=1,axis='tb',align='h',rel_widths=c(1,1,.3),labels=c('c','d',''),label_size=12)
p$tmp <- plot_grid(p$tmp1,p$tmp2,ncol=1,rel_heights=c(1,.9))
ggsave(filename='output/fig1.png',plot=p$tmp,width=190,height=130,units='mm',dpi=300,bg='white')
ggsave(filename='output/fig1.eps',plot=p$tmp,width=190,height=130,units='mm')


# Fig.2 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Gas_Fos','Gas','moccasin',
                  'Fin_Ene_Bio','Biomass','darkolivegreen2',
                  'Fin_Ene_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Heat','Heat','salmon',
                  'Fin_Ene_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Hyd_syn','Synfuels','orchid',
                  'Fin_Ene_Oth','Other','grey90')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$fec_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    ylim(0,13.5)+
    labs(title=NULL,x=NULL,y=expression(paste('Final energy (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

df$var  <- tribble(~Variable,~Legend,~Color,
                   'Fin_Ene_Ind_Liq_Fos','Oil','sandybrown',
                   'Fin_Ene_Ind_SolidsCoa','Coal','grey70',
                   'Fin_Ene_Ind_Gas_Fos','Gas','moccasin',
                   'Fin_Ene_Ind_Bio','Biomass','darkolivegreen2',
                   'Fin_Ene_Ind_Ele','Electricity','lightsteelblue',
                   'Fin_Ene_Ind_Heat','Heat','salmon',
                   'Fin_Ene_Ind_Hyd','Hydrogen','thistle2',
                   'Fin_Ene_Ind_Liq_and_Gas_Hyd_syn','Synfuels','orchid',
                   'Fin_Ene_Ind_Oth','Other','grey90')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$fec_ind <- filter(df$all,Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    filter(Year==2050) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(title='Industry',x=NULL,y=expression(paste('Final energy (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='bottom',axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

df$var  <- tribble(~Variable,~Legend,~Color,
                   'Fin_Ene_Res_and_Com_Liq','Liquids','sandybrown',
                   'Fin_Ene_Res_and_Com_SolidsCoa','Coal','grey70',
                   'Fin_Ene_Res_and_Com_Gas_Fos','Gas','moccasin',
                   'Fin_Ene_Res_and_Com_SolidsBio','Biomass','darkolivegreen2',
                   'Fin_Ene_Res_and_Com_Ele','Electricity','lightsteelblue',
                   'Fin_Ene_Res_and_Com_Heat','Heat','salmon',
                   'Fin_Ene_Res_and_Com_Hyd','Hydrogen','thistle2',
                   'Fin_Ene_Res_and_Com_Liq_and_Gas_Hyd_syn','Synfuels','orchid',
                   'Fin_Ene_Res_and_Com_Oth','Other','grey90')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$fec_bui <- filter(df$all,Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    filter(Year==2050) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(title='Buildings',x=NULL,y=expression(paste('Final energy (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='bottom',axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

df$var <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Liq_Oil','Liquids','sandybrown',
                  'Fin_Ene_Tra_Liq_Coa','Coal','grey70',
                  'Fin_Ene_Tra_Liq_Nat_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Liq_Bio','Biomass','darkolivegreen2',
                  'Fin_Ene_Tra_Gas','Gas','Peru',
                  'Fin_Ene_Tra_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Liq_and_Gas_Hyd_syn','Synfuels','orchid',
                  'Fin_Ene_Tra_Oth','Other','grey90')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$fec_tra <- filter(df$all,Variable%in%df$var$Variable) %>% 
    inner_join(df$var,by='Variable') %>% 
    filter(Year==2050) %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(title='Transport',x=NULL,y=expression(paste('Final energy (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='bottom',axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

p$fin_ene_path <- filter(df$all,Variable=='Fin_Ene') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year>=2010) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=Scenario))+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    ylim(0,13.5)+
    labs(title=NULL,x=NULL,y=expression(paste('Final energy (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',legend.background=element_blank())+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))

p$fin_ene_ele <- filter(df$all,Variable=='Fin_Ene_Share_Ele') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year>=2010) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=Scenario))+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent)+
    labs(title=NULL,x=NULL,y='Electrification rate')+
    mytheme$theme_set1+theme(legend.position='right')+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Variable2,
                  'Fin_Ene_Ind_Share_Ele','Electricity',
                  'Fin_Ene_Ind_Share_Hyd_Car','H2+Synfuels')
p$fin_ene_ind_ele <- filter(df$all,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario),Variable2=factor(Variable2,levels=df$var$Variable2)) %>% 
    ggplot()+
    geom_point(aes(x=Variable2,y=Value,color=Scenario),shape=21,fill='transparent')+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent)+
    labs(title=NULL,x=NULL,y='Final energy share')+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))
df$var <- tribble(~Variable,~Variable2,
                  'Fin_Ene_Res_and_Com_Share_Ele','Electricity',
                  'Fin_Ene_Res_and_Com_Share_Hyd_Car','H2+Synfuels')
p$fin_ene_bui_ele <- filter(df$all,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario),Variable2=factor(Variable2,levels=df$var$Variable2)) %>% 
    ggplot()+
    geom_point(aes(x=Variable2,y=Value,color=Scenario),shape=21,fill='transparent')+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent)+
    labs(title=NULL,x=NULL,y='Final energy share')+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))
df$var <- tribble(~Variable,~Variable2,
                  'Fin_Ene_Tra_Share_Ele','Electricity',
                  'Fin_Ene_Tra_Share_Hyd_Car','H2+Synfuels')
p$fin_ene_tra_ele <- filter(df$all,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario),Variable2=factor(Variable2,levels=df$var$Variable2)) %>% 
    ggplot()+
    geom_point(aes(x=Variable2,y=Value,color=Scenario),shape=21,fill='transparent')+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent)+
    labs(title=NULL,x=NULL,y='Final energy share')+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))

p$l_fin_ene_path <- get_legend(p$fin_ene_path)
p$l_fec_2050 <- get_legend(p$fec_2050)
p$tmp1 <- plot_grid(p$fin_ene_path+theme(legend.position='none'),
                    p$fec_2050+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank(),axis.title.y=element_blank(),legend.position='none'),
                    p$fin_ene_ele+theme(legend.position='none'),p$l_fin_ene_path,p$l_fec_2050,
                    nrow=1,rel_widths=c(1,.5,1,.45,.45),axis='tb',align='h',labels=c('a','','b'),label_size=12)
p$fec_sec <- plot_grid(p$fec_ind+theme(legend.position='none'),p$fin_ene_ind_ele+theme(legend.position='none'),
                       p$fec_bui+theme(legend.position='none'),p$fin_ene_bui_ele+theme(legend.position='none'),
                       p$fec_tra+theme(legend.position='none'),p$fin_ene_tra_ele+theme(legend.position='none'),
                       nrow=1,rel_widths=c(1,.7,1,.7,1,.7),axis='tb',align='h',labels=c('c','','d','','e',''),label_size=12)
p$tmp <- plot_grid(p$tmp1,p$fec_sec,ncol=1,rel_heights=c(1,1))
ggsave(filename='output/fig2.png',plot=p$tmp,bg='white',width=190,height=130,units='mm',dpi=300)
ggsave(filename='output/fig2.eps',plot=p$tmp,width=190,height=130,units='mm')


# Fig.3 -------------------------------------------------------------------

p$sec_ele_path <- filter(df$all,Variable=='Sec_Ene_Ele') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year>=2010) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=Scenario))+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    ylim(0,8.5)+
    labs(title=NULL,x=NULL,y=expression(paste('Power generation (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right')+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Oil_wo_CCS','Oil w/o CCS','sandybrown',
                  'Sec_Ene_Ele_Oil_w_CCS','Oil w/ CCS','tan3',
                  'Sec_Ene_Ele_Coa_wo_CCS','Coal w/o CCS','grey50',
                  'Sec_Ene_Ele_Coa_w_CCS','Coal w/ CCS','grey30',
                  'Sec_Ene_Ele_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                  'Sec_Ene_Ele_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',
                  'Sec_Ene_Ele_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Sec_Ene_Ele_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                  'Sec_Ene_Ele_Hyd','Hydro','lightsteelblue',
                  'Sec_Ene_Ele_Geo','Geothermal','peru',
                  'Sec_Ene_Ele_Solar','Solar','lightsalmon',
                  'Sec_Ene_Ele_Win','Wind','lightskyblue3',
                  'Sec_Ene_Ele_Oce','Ocean','paleturquoise3',
                  'Sec_Ene_Ele_Hyd_GT','Hydrogen','thistle3',
                  'Sec_Ene_Ele_Oth','Other','grey')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$sec_ele_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    ylim(0,8.5)+
    labs(title=NULL,x=NULL,y=expression(paste('Power generation (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

df$var <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Hyd_Coa_wo_CCS','Domestic-Coal w/o CCS','grey50',
                  'Sec_Ene_Hyd_Coa_w_CCS','Domestic-Coal w/ CCS','grey30',
                  'Sec_Ene_Hyd_Gas_wo_CCS','Domestic-Gas w/o CCS','lightgoldenrod',
                  'Sec_Ene_Hyd_Gas_w_CCS','Domestic-Gas w/ CCS','lightgoldenrod3',
                  'Sec_Ene_Hyd_Bio_wo_CCS','Domestic-Biomass w/o CCS','darkolivegreen2',
                  'Sec_Ene_Hyd_Bio_w_CCS','Domestic-Biomass w/ CCS','darkolivegreen4',
                  'Sec_Ene_Hyd_Ele','Domestic-electricity','lightsteelblue',
                  'Trd_Sec_Ene_Imp_Amm_Vol','Import-ammonia','thistle2',
                  'Trd_Sec_Ene_Imp_Syn_Vol','Import-synfuel','orchid')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$sec_hyd_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(title=NULL,x=NULL,y=expression(paste('Hydrogen supply (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

p$l_tmp <- plot_grid(get_legend(p$sec_hyd_2050),get_legend(p$sec_ele_path),ncol=1)
p$tmp <- plot_grid(p$sec_ele_path+theme(legend.position='none'),
                   p$sec_ele_2050+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank()),
                   p$sec_hyd_2050+theme(legend.position='none'),p$l_tmp,
                   nrow=1,rel_widths=c(1,1.4,.8,.8),axis='tb',align='h',labels=c('a','','b'),label_size=12)
ggsave(filename='output/fig3.png',plot=p$tmp,width=190,height=75,units='mm',dpi=300)
ggsave(filename='output/fig3.eps',plot=p$tmp,width=190,height=75,units='mm')


# Fig.4 -------------------------------------------------------------------

df$var <- tribble(~Variable,~Legend,~Color,
                  'Prm_Ene_Oil','Oil','sandybrown',
                  'Prm_Ene_Coa','Coal','grey50',
                  'Prm_Ene_Gas','Gas','lightgoldenrod',
                  'Prm_Ene_Nuc','Nuclear','moccasin',
                  'Prm_Ene_Bio','Biomass','darkolivegreen2',
                  'Prm_Ene_Hyd','Hydro','lightsteelblue',
                  'Prm_Ene_Geo','Geothermal','peru',
                  'Prm_Ene_Solar','Solar','lightsalmon',
                  'Prm_Ene_Win','Wind','lightskyblue3',
                  'Prm_Ene_Oce','Ocean','paleturquoise3',
                  'Prm_Ene_Oth','Other','grey',
                  'Prm_Ene_Sec_Ene_Trd','Secondary energy import','thistle2')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$prm_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(title=NULL,x=NULL,y=expression(paste('Primary energy (EJ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

p$imp_path <- filter(df$all,Variable=='Imp_Dep_Prm_Ene') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year>=2010) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=Scenario))+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    scale_y_continuous(limits=c(0,NA),labels=scales::percent)+
    labs(title=NULL,x=NULL,y='Import dependency')+
    mytheme$theme_set1+theme(legend.position='right')+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))

df$var <- tribble(~Variable,~Legend,~Color,
                  'Imp_Ene_Prm_Ene_Coa','Coal','grey50',
                  'Imp_Ene_Prm_Ene_Gas','Natural gas','lightgoldenrod',
                  'Imp_Ene_Prm_Ene_Oil','Oil','sandybrown',
                  'Imp_Ene_Sec_Ene','Secondary energy','thistle2',
                  'Imp_Emi_All','Emission allowances','darkolivegreen2')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$imp1 <- filter(df$all,Variable%in%df$var$Variable,Scenario=='NZE-CR') %>% 
    filter(Year%in%seq(2010,2050,10)) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable))) %>% 
    ggplot()+
    geom_bar(aes(x=Year,y=Value,fill=Variable),position='stack',stat='identity')+
    ylim(0,200)+
    labs(title=NULL,x=NULL,y=expression(paste('Energy imports (billion US$ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1))+
    scale_fill_manual(values=col,labels=leg,name=NULL)
p$imp2 <- filter(df$all,Variable%in%df$var$Variable) %>% 
    filter(Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    ylim(0,200)+
    labs(title=NULL,x=NULL,y=NULL)+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank(),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

p$imp_cos_path <- filter(df$all,Variable=='Imp_Ene_and_Emi') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year>=2010) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=Scenario))+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    ylim(0,200)+
    labs(title=NULL,x=NULL,y=expression(paste('Energy imports (billion US$ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right')+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))

p$l1 <- get_legend(p$prm_2050+guides(fill=guide_legend(ncol=3)))
p$tmp1 <- plot_grid(p$imp_path+theme(legend.position=c(.25,.35),legend.background=element_blank()),
                    p$prm_2050+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,.7),axis='tb',align='h',labels=c('a',''),label_size=12)
p$tmp2 <- plot_grid(p$tmp1,p$l1,ncol=1,rel_heights=c(1,.3))

p$l1 <- get_legend(p$imp2+guides(fill=guide_legend(nrow=2)))
p$tmp1 <- plot_grid(p$imp_cos_path+theme(legend.position='none'),p$imp2+theme(legend.position='none'),
                    nrow=1,rel_widths=c(1,.55),axis='tb',align='h',labels=c('b',''),label_size=12)
p$tmp3 <- plot_grid(p$tmp1,p$l1,ncol=1,rel_heights=c(1,.2))
p$tmp <- plot_grid(p$tmp2,p$tmp3,nrow=1,rel_widths=c(1,.85))
ggsave(filename='output/fig4.png',plot=p$tmp,bg='white',width=190,height=100,units='mm',dpi=300)
ggsave(filename='output/fig4.eps',plot=p$tmp,bg='white',width=190,height=100,units='mm')


# Fig.5 -------------------------------------------------------------------

p$car_pri <- filter(df$all,Variable=='Prc_Car') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year>=2020) %>% 
    ggplot()+
    geom_path(aes(x=Year,y=Value,color=Scenario))+
    geom_point(aes(x=Year,y=Value,color=Scenario),shape=21,fill='white')+
    ylim(0,NA)+
    labs(title=NULL,x=NULL,y=expression(paste('Carbon prices (US$ t-',{CO[2]}^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right')+
    scale_color_manual(values=df$scen_col,name=NULL)+
    guides(color=guide_legend(title=NULL))
p$cum_inv <- filter(df$all,Variable=='Pol_Cos_Cum_Dis') %>% 
    mutate(Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    filter(Year==2050) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value),fill='grey50',stat='identity')+
    labs(title=NULL,x=NULL,y='Cumulative costs (billion US$)')+
    mytheme$theme_set1+theme(legend.position=c(.3,.9),axis.text.x=element_text(angle=45,hjust=1))

df$var <- tribble(~Variable,~Legend,~Color,
                  'Inv_Add_Ene_Sup_Ele','Electricity','lightsteelblue',
                  'Inv_Add_Ene_Sup_Hyd','Hydrogen','thistle2',
                  'Inv_Add_Ene_Sup_Oth','Other energy supply','moccasin',
                  'Inv_Add_Ene_Dem_Ind','Industry','salmon',
                  'Inv_Add_Ene_Dem_Res_and_Com','Buildings','lightskyblue3',
                  'Inv_Add_Ene_Dem_Tra','Transport','darkolivegreen2',
                  'Inv_Add_Oth','Other','grey90')
leg <- as.character(df$var$Legend); names(leg) <- as.character(df$var$Variable)
col <- as.character(df$var$Color); names(col) <- as.character(df$var$Variable)
p$inv_2050 <- filter(df$all,Variable%in%df$var$Variable,Year==2050) %>% 
    inner_join(df$var,by='Variable') %>% 
    mutate(Variable=factor(Variable,levels=rev(df$var$Variable)),Scenario=factor(Scenario,levels=lst$scenario)) %>% 
    ggplot()+
    geom_bar(aes(x=Scenario,y=Value,fill=Variable),position='stack',stat='identity')+
    labs(title=NULL,x=NULL,y=expression(paste('Investment (billion US$ ',yr^{`-1`},')')))+
    mytheme$theme_set1+theme(legend.position='right',axis.text.x=element_text(angle=45,hjust=1),legend.key.size=unit(5,'mm'))+
    scale_fill_manual(values=col,labels=leg,name=NULL)

p$tmp <- plot_grid(p$car_pri+theme(legend.position=c(.3,.75)),p$cum_inv+theme(legend.position='none'),p$inv_2050,
                   nrow=1,axis='tb',align='h',labels=c('a','b','c'),rel_widths=c(1,.8,1.4),label_size=12)
ggsave(filename='output/fig5.png',plot=p$tmp,width=190,height=75,units='mm',dpi=300)
ggsave(filename='output/fig5.eps',plot=p$tmp,width=190,height=75,units='mm')
