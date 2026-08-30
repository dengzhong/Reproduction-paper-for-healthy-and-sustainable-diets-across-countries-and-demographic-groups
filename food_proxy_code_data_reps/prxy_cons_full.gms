*-------------------------------------------------------------------------------
*        FBS proxy
*-------------------------------------------------------------------------------

*        steps:
*        - energy-correct FBS estimates 
*        - add processed foods from GDD
*        - add processed foods from SUA
*        - socio-demographic trends from GDD
*        - energy-correct socio-demographic trends

*-------------------------------------------------------------------------------
*        Energy correction
*-------------------------------------------------------------------------------

*        calculate energy ratio relative to IOM estimates:

parameter energy_ratio_FBS     energy adjustment;

energy_ratio_FBS("FBS_IOM",r,"BTH","all-a",year,stats)
         $ FBS_intake("kcal/d_w","all-fg",r,year)
         = IOM_energy(r,"BTH","all-a","all-u",year,stats)
         / FBS_intake("kcal/d_w","all-fg",r,year);
         
*        apply ratios to move energy intake to IOM estimates:
         
parameter FBS_intake_IOM   FBS food intake adjusted for IOM EER (grams per day);

FBS_intake_IOM(unit_wp,fg_FBS,r,year,stats)
         = FBS_intake(unit_wp,fg_FBS,r,year)
         * energy_ratio_FBS("FBS_IOM",r,"BTH","all-a",year,stats);
         
*        keep food supply (i.e., with waste) unadjusted:

FBS_intake_IOM(unit_sp,fg_FBS,r,year,stats)
         = FBS_intake(unit_sp,fg_FBS,r,year);

*-------------------------------------------------------------------------------
*        Processed foods from GDD
*-------------------------------------------------------------------------------

*        calculate processing ratios based on GDD estimates:

set      fg_GDD_grains  /prc_grains, whole_grains/
         fg_GDD_redmeat /prc_meat, red_meat/;

*        total red meat needs to be recalculated:

GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"total_red_meat")
         = GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"red_meat")
         + GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"prc_meat");
         
*        calculate processing ratios:

parameter prc_ratio        processing ratios;

prc_ratio("grains","whole_grains",r,age,sex,urban,edu,year,stats)
         $ GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"grains")
         = GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"whole_grains")
         / GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"grains");

prc_ratio("grains","prc_grains",r,age,sex,urban,edu,year,stats)
         $ GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"grains")
         = GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"prc_grains")
         / GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"grains");         

prc_ratio("red_meat","prc_meat",r,age,sex,urban,edu,year,stats)
         $ GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"total_red_meat")
         = GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"prc_meat")
         / GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"total_red_meat");         

prc_ratio("red_meat","red_meat",r,age,sex,urban,edu,year,stats)
         $ GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"total_red_meat")
         = GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"red_meat")
         / GDD_intake_IOM(r,age,sex,urban,edu,year,stats,"total_red_meat");

*        allocate to new parameter:

parameter FBS_intake_GDD   energy-corrected FBS intake with GDD detail;

FBS_intake_GDD("prcd",unit_p,fg_GDD_grains,r,year,stats)
         = prc_ratio("grains",fg_GDD_grains,r,"all-a","BTH","all-u","all-e",year,"mean")
         * sum(fg_grains, FBS_intake_IOM(unit_p,fg_grains,r,year,stats));

FBS_intake_GDD("prcd",unit_p,"prc_meat",r,year,stats)
         = prc_ratio("red_meat","prc_meat",r,"all-a","BTH","all-u","all-e",year,"mean")
         * sum(fg_redmeat, FBS_intake_IOM(unit_p,fg_redmeat,r,year,stats));

FBS_intake_GDD("prcd",unit_p,"red_meat",r,year,stats)
         = sum(fg_redmeat, FBS_intake_IOM(unit_p,fg_redmeat,r,year,stats))
         - FBS_intake_GDD("prcd",unit_p,"prc_meat",r,year,stats);

*-------------------------------------------------------------------------------
*        Processed foods from SUA
*-------------------------------------------------------------------------------
         
*        copy unprocessed/primary commodity equivalents:

FBS_intake_GDD("prim",unit_p,fg_FBS,r,year,stats)
         = FBS_intake_IOM(unit_p,fg_FBS,r,year,stats);
         
*        add other FBS foods of interest:

*parameter cns_SUA consumption of foods from SUA;
*set       fg_SUA  /yoghurt, cheese/;

FBS_intake_GDD("prcd",unit_wp,fg_SUA,r,year,stats)
         = cns_SUA(r,fg_SUA,unit_wp,year)
         * energy_ratio_FBS("FBS_IOM",r,"BTH","all-a",year,stats);

FBS_intake_GDD("prcd",unit_sp,fg_SUA,r,year,stats)
         = cns_SUA(r,fg_SUA,unit_sp,year);

*        calculate remaining milk:

FBS_intake_GDD("prcd","kcal/d_w","milk_actl",r,year,stats)
         = FBS_intake_GDD("prim","kcal/d_w","milk",r,year,stats)
         - FBS_intake_GDD("prcd","kcal/d_w","yoghurt",r,year,stats)
         - FBS_intake_GDD("prcd","kcal/d_w","cheese",r,year,stats);
         
FBS_intake_GDD("prcd","g/d_w","milk_actl",r,year,stats)
         $ FBS_intake_GDD("prim","kcal/d_w","milk",r,year,stats)
         = FBS_intake_GDD("prcd","kcal/d_w","milk_actl",r,year,stats)
         * FBS_intake_GDD("prim","g/d_w","milk",r,year,stats)
         / FBS_intake_GDD("prim","kcal/d_w","milk",r,year,stats);         

*        same for supply:

FBS_intake_GDD("prcd","kcal/d","milk_actl",r,year,stats)
         = FBS_intake_GDD("prim","kcal/d","milk",r,year,stats)
         - FBS_intake_GDD("prcd","kcal/d","yoghurt",r,year,stats)
         - FBS_intake_GDD("prcd","kcal/d","cheese",r,year,stats);
         
FBS_intake_GDD("prcd","g/d","milk_actl",r,year,stats)
         $ FBS_intake_GDD("prim","kcal/d","milk",r,year,stats)
         = FBS_intake_GDD("prcd","kcal/d","milk_actl",r,year,stats)
         * FBS_intake_GDD("prim","g/d","milk",r,year,stats)
         / FBS_intake_GDD("prim","kcal/d","milk",r,year,stats);

*-------------------------------------------------------------------------------
*        Socio-demographic trends
*-------------------------------------------------------------------------------

*        construct FBS proxy for food intake with socio-demographic detail:

parameter FBS_intake_socio_ini        initial FBS proxy for food intake with all socio-demographic trends
          pop_socio                   population stats with socio-demographic trends;

*        all socio-demographic trends:

FBS_intake_socio_ini(type,unit_wp,fg_FBS_e,r,age,sex,urban,edu,year,stats)
         = FBS_intake_GDD(type,unit_wp,fg_FBS_e,r,year,stats)
         * sum(fg_GDD$map_fg_GDD(fg_FBS_e,fg_GDD), GDD_ratio_age_sex_urban_edu(r,age,sex,urban,edu,year,fg_GDD));

FBS_intake_socio_ini("prim","kcal/d_w","all-fg",r,age,sex,urban,edu,year,stats)
         = sum(fg_FBS_s, FBS_intake_socio_ini("prim","kcal/d_w",fg_FBS_s,r,age,sex,urban,edu,year,stats));

pop_socio(r,age,sex,urban,edu,year)
         = GDD_pop(r,age,sex,urban,edu,year);

*-------------------------------------------------------------------------------
*        Energy correction socio-demographic trends
*-------------------------------------------------------------------------------

*        energy-correct one more time to make sure age groups are calibrated correctly:

*        calculate adjustment ratios:

parameter energy_ratio_socio        energy adjustment;

energy_ratio_socio("FBS_IOM",r,age,sex,urban,edu,year,stats)
         $ FBS_intake_socio_ini("prim","kcal/d_w","all-fg",r,age,sex,urban,edu,year,"mean")
         = IOM_energy(r,sex,age,urban,year,stats)
         / FBS_intake_socio_ini("prim","kcal/d_w","all-fg",r,age,sex,urban,edu,year,"mean");
         
*        apply ratios to move energy intake to IOM estimates:

parameter FBS_intake_socio          FBS food intake adjusted for IOM EER;

FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,urban,edu,year,stats)
         = FBS_intake_socio_ini(type,unit_wp,fg_FBS_e,r,age,sex,urban,edu,year,"mean")
         * energy_ratio_socio("FBS_IOM",r,age,sex,urban,edu,year,stats);
         
*        re-aggregate projections:

FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a",sex,urban,edu,year,stats)
         $ sum(age_s, pop_socio(r,age_s,sex,urban,edu,year))
         = sum(age_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age_s,sex,urban,edu,year,stats) * pop_socio(r,age_s,sex,urban,edu,year))
         / sum(age_s, pop_socio(r,age_s,sex,urban,edu,year));

FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,"BTH",urban,edu,year,stats)
         $ sum(sex_s, pop_socio(r,age,sex_s,urban,edu,year))
         = sum(sex_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex_s,urban,edu,year,stats) * pop_socio(r,age,sex_s,urban,edu,year))
         / sum(sex_s, pop_socio(r,age,sex_s,urban,edu,year));

FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,"all-u",edu,year,stats)
         $ sum(urban_s, pop_socio(r,age,sex,urban_s,edu,year))
         = sum(urban_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,urban_s,edu,year,stats) * pop_socio(r,age,sex,urban_s,edu,year))
         / sum(urban_s, pop_socio(r,age,sex,urban_s,edu,year));

FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,urban,"all-e",year,stats)
         $ sum(edu_s, pop_socio(r,age,sex,urban,edu_s,year))
         = sum(edu_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,urban,edu_s,year,stats) * pop_socio(r,age,sex,urban,edu_s,year))
         / sum(edu_s, pop_socio(r,age,sex,urban,edu_s,year));

*-------------------------------------------------------------------------------

*        check socio-demographic aggregation:

parameter agg_chk_FBS check of aggregation;
          
set       stats_chk_FBS(stats)       /mean/
          fg_chk_FBS(*)              /vegetables/
          type_chk_FBS(type)         /prim/
          unit_chk_FBS(unit_wp)      /"g/d_w"/
          yrs_chk_FBS                ;
          yrs_chk_FBS(year)          = yes;

agg_chk_FBS("FBS_SOC",r,"age_chk",year,stats,fg_FBS_e,type,unit_wp)$(yrs_chk_FBS(year) and stats_chk_FBS(stats) and fg_chk_FBS(fg_FBS_e) and type_chk_FBS(type) and unit_chk_FBS(unit_wp))          
         = FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a","BTH","all-u","all-e",year,stats) * pop_socio(r,"all-a","BTH","all-u","all-e",year)
         - sum(age_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age_s,"BTH","all-u","all-e",year,stats) * pop_socio(r,age_s,"BTH","all-u","all-e",year));

agg_chk_FBS("FBS_SOC",r,"sex_chk",year,stats,fg_FBS_e,type,unit_wp)$(yrs_chk_FBS(year) and stats_chk_FBS(stats) and fg_chk_FBS(fg_FBS_e) and type_chk_FBS(type) and unit_chk_FBS(unit_wp))          
         = FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a","BTH","all-u","all-e",year,stats) * pop_socio(r,"all-a","BTH","all-u","all-e",year)
         - sum(sex_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a",sex_s,"all-u","all-e",year,stats) * pop_socio(r,"all-a",sex_s,"all-u","all-e",year));
         
agg_chk_FBS("FBS_SOC",r,"urban_chk",year,stats,fg_FBS_e,type,unit_wp)$(yrs_chk_FBS(year) and stats_chk_FBS(stats) and fg_chk_FBS(fg_FBS_e) and type_chk_FBS(type) and unit_chk_FBS(unit_wp))          
         = FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a","BTH","all-u","all-e",year,stats) * pop_socio(r,"all-a","BTH","all-u","all-e",year)
         - sum(urban_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a","BTH",urban_s,"all-e",year,stats) * pop_socio(r,"all-a","BTH",urban_s,"all-e",year));
         
agg_chk_FBS("FBS_SOC",r,"edu_chk",year,stats,fg_FBS_e,type,unit_wp)$(yrs_chk_FBS(year) and stats_chk_FBS(stats) and fg_chk_FBS(fg_FBS_e) and type_chk_FBS(type) and unit_chk_FBS(unit_wp))          
         = FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a","BTH","all-u","all-e",year,stats) * pop_socio(r,"all-a","BTH","all-u","all-e",year)
         - sum(edu_s, FBS_intake_socio(type,unit_wp,fg_FBS_e,r,"all-a","BTH","all-u",edu_s,year,stats) * pop_socio(r,"all-a","BTH","all-u",edu_s,year));

*-------------------------------------------------------------------------------
*        Food availability 
*-------------------------------------------------------------------------------

*        use socio-demographic trends to re-insert waste:

parameter FBS_ratio_socio           socio-demographic ratio based on FBS proxy;

FBS_ratio_socio(type,fg_FBS_e,r,age,sex,urban,edu,year)
         $ FBS_intake_socio(type,"g/d_w",fg_FBS_e,r,"all-a","BTH","all-u","all-e",year,"mean")
         = FBS_intake_socio(type,"g/d_w",fg_FBS_e,r,age,sex,urban,edu,year,"mean")
         / FBS_intake_socio(type,"g/d_w",fg_FBS_e,r,"all-a","BTH","all-u","all-e",year,"mean");

*        apply to supply data:

parameter FBS_supply_socio          FBS food supply by socio-demographics;

FBS_supply_socio(type,unit_sp,fg_FBS_e,r,age,sex,urban,edu,year,stats)
         = FBS_intake_GDD(type,unit_sp,fg_FBS_e,r,year,stats)
         * FBS_ratio_socio(type,fg_FBS_e,r,age,sex,urban,edu,year);
         
FBS_supply_socio(type,"kcal/d","all-fg",r,age,sex,urban,edu,year,stats)
         = sum(fg_FBS_s, FBS_supply_socio(type,"kcal/d",fg_FBS_s,r,age,sex,urban,edu,year,stats));

*-------------------------------------------------------------------------------
*        Estimates with less detail
*-------------------------------------------------------------------------------

*        focus on age and sex trends:

parameter FBS_intake_age_sex        FBS proxy for food intake with age and sex trends
          FBS_supply_age_sex        FBS proxy for food supply with age and sex trends
          pop_age_sex               population stats with age and sex trends;
          
*        focus on age and sex trends:
         
FBS_intake_age_sex(type,unit_wp,fg_FBS_e,r,age,sex,year,stats)
         = FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,"all-u","all-e",year,stats);

FBS_supply_age_sex(type,unit_sp,fg_FBS_e,r,age,sex,year,stats)
         = FBS_supply_socio(type,unit_sp,fg_FBS_e,r,age,sex,"all-u","all-e",year,stats);
         
pop_age_sex(r,age,sex,year)
         = pop_socio(r,age,sex,"all-u","all-e",year);
         
*-------------------------------------------------------------------------------

*        aggregate to age classes:

parameter FBS_intake_socio_agg      FBS proxy for food intake with all socio-demographic trends
          FBS_supply_socio_agg      FBS proxy for food supply with all socio-demographic trends
          pop_socio_agg             population stats with socio-demographic trends
          
          FBS_intake_age_sex_agg    FBS proxy for food intake with age and sex trends
          FBS_supply_age_sex_agg    FBS proxy for food supply with age and sex trends
          pop_age_sex_agg           population stats with age and sex trends;

FBS_intake_socio_agg(type,unit_wp,fg_FBS_e,r,age_5y,sex,urban,edu,year,stats)
         $ sum(age$map_age_5y(age,age_5y), pop_socio(r,age,sex,urban,edu,year))
         = sum(age$map_age_5y(age,age_5y), FBS_intake_socio(type,unit_wp,fg_FBS_e,r,age,sex,urban,edu,year,stats) * pop_socio(r,age,sex,urban,edu,year))
         / sum(age$map_age_5y(age,age_5y), pop_socio(r,age,sex,urban,edu,year));

FBS_supply_socio_agg(type,unit_sp,fg_FBS_e,r,age_5y,sex,urban,edu,year,stats)
         $ sum(age$map_age_5y(age,age_5y), pop_socio(r,age,sex,urban,edu,year))
         = sum(age$map_age_5y(age,age_5y), FBS_supply_socio(type,unit_sp,fg_FBS_e,r,age,sex,urban,edu,year,stats) * pop_socio(r,age,sex,urban,edu,year))
         / sum(age$map_age_5y(age,age_5y), pop_socio(r,age,sex,urban,edu,year));
         
pop_socio_agg(r,age_5y,sex,urban,edu,year)
         = sum(age$map_age_5y(age,age_5y), pop_socio(r,age,sex,urban,edu,year));

FBS_intake_age_sex_agg(type,unit_wp,fg_FBS_e,r,age_5y,sex,year,stats)
         $ sum(age$map_age_5y(age,age_5y), pop_age_sex(r,age,sex,year))
         = sum(age$map_age_5y(age,age_5y), FBS_intake_age_sex(type,unit_wp,fg_FBS_e,r,age,sex,year,stats) * pop_age_sex(r,age,sex,year))
         / sum(age$map_age_5y(age,age_5y), pop_age_sex(r,age,sex,year));

FBS_supply_age_sex_agg(type,unit_sp,fg_FBS_e,r,age_5y,sex,year,stats)
         $ sum(age$map_age_5y(age,age_5y), pop_age_sex(r,age,sex,year))
         = sum(age$map_age_5y(age,age_5y), FBS_supply_age_sex(type,unit_sp,fg_FBS_e,r,age,sex,year,stats) * pop_age_sex(r,age,sex,year))
         / sum(age$map_age_5y(age,age_5y), pop_age_sex(r,age,sex,year));
         
pop_age_sex_agg(r,age_5y,sex,year)
         = sum(age$map_age_5y(age,age_5y), pop_age_sex(r,age,sex,year));
         
*-------------------------------------------------------------------------------

*        prepare parameter for csv output:

parameter intake_grams
          intake_kcals
          pop
          demand;

intake_grams(type,"g/d",fg_FBS_e,r,age_5y,sex,urban,year,stats)
         = FBS_intake_socio_agg(type,"g/d_w",fg_FBS_e,r,age_5y,sex,urban,"all-e",year,stats);
   
intake_kcals(type,"kcal/d",fg_FBS_e,r,age_5y,sex,urban,year,stats)
         = FBS_intake_socio_agg(type,"kcal/d_w",fg_FBS_e,r,age_5y,sex,urban,"all-e",year,stats);


pop(r,age_5y,sex,urban,year)
         = pop_socio_agg(r,age_5y,sex,urban,"all-e",year);
         

demand(type,"g/d",fg_FBS_e,r,age_5y,sex,urban,year)
         = FBS_supply_socio_agg(type,"g/d",fg_FBS_e,r,age_5y,sex,urban,"all-e",year,"mean");
   
demand(type,"kcal/d",fg_FBS_e,r,age_5y,sex,urban,year)
         = FBS_supply_socio_agg(type,"kcal/d",fg_FBS_e,r,age_5y,sex,urban,"all-e",year,"mean");
         
*-------------------------------------------------------------------------------
*        Database
*-------------------------------------------------------------------------------
         
*        write to gdx file:

execute_unload '%output_dir%/FBS_proxy_%year%.gdx',
         prc_ratio,
         FBS_intake_socio, FBS_supply_socio, pop_socio,
         FBS_intake_age_sex, FBS_supply_age_sex, pop_age_sex,
         FBS_intake_socio_agg, FBS_supply_socio_agg, pop_socio_agg,
         FBS_intake_age_sex_agg, FBS_supply_age_sex_agg, pop_age_sex_agg, 
         FBS_intake_GDD, FBS_intake_IOM, FBS_intake, g2kcal_yrs,
         agg_chk_FBS, energy_ratio_FBS, energy_ratio_socio,
         intake_grams, intake_kcals, pop, demand;
*$exit

*-------------------------------------------------------------------------------

*        write to CSV:

EmbeddedCode Connect:
- GAMSReader:
    symbols:
        - name: intake_grams
        - name: intake_kcals
        - name: pop
        - name: demand
- CSVWriter:
    file: %output_dir%/intake_grams_%year%.csv
    name: intake_grams
    setHeader: type,unit,food_group,region,age,sex,residence,year,stats,value
- CSVWriter:
    file: %output_dir%/intake_kcals_%year%.csv
    name: intake_kcals
    setHeader: type,unit,food_group,region,age,sex,residence,year,stats,value
- CSVWriter:
    file: %output_dir%/pop_%year%.csv
    name: pop
    setHeader: region,age,sex,residence,year,value
- CSVWriter:
    file: %output_dir%/demand_%year%.csv
    name: demand
    setHeader: type,unit,food_group,region,age,sex,residence,year,value
endEmbeddedCode

*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------
