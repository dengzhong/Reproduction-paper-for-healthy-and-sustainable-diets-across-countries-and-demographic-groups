*-------------------------------------------------------------------------------
*        Data for detailed FBS proxy
*-------------------------------------------------------------------------------

parameter FBS_intake_data  FBS intake data
          FBS_pop_data     population in thousands
          g2kcal_data      data on conversion factors;

$gdxin  %input_dir%/all_raw_data.gdx
$load   FBS_intake_data, FBS_pop_data, g2kcal_data        

*        assign data:

parameter FBS_intake       FBS intake data
          FBS_pop          population in thousands
          g2kcal_yrs       conversion factor from grams per day to kcals per day;
          
FBS_intake(unit_p,fg_FBS,r,year)
         = FBS_intake_data(r,fg_FBS,unit_p,year);
         
FBS_pop(r,year)
         = FBS_pop_data(r,year);

g2kcal_yrs(fg_FBS,r,year)
         = g2kcal_data(r,fg_FBS,"kcal/g",year);
         
         
*        add conversion factors for aggregate food groups:

set       fg_redmeat       /beef, lamb, pork/
          fg_fish          /fish_freshw, fish_pelag, fish_demrs, fish_marine, fish_aquatic, fish_aquaticplant, crustaceans, cephalopods, othr_molluscs/
          fg_fruits        /orange, lemon, grapefruit, citrus, coconuts, pineapple, dates, apple, grapes, preserved_olives, othr_fruits, banana, plantains/
          fg_nuts_seeds    /nuts, groundnut, seed_sunflower, seed_rape, seed_cotton, seed_sesame, seed_palmkernel, seed_oilcrop/
          fg_grains        /wheat, rice, maize, barley, rye, oats, millet, sorghum, othr_grains/
          ;

g2kcal_yrs("red_meat",r,year)
         $ sum(fg_FBS$fg_redmeat(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year))
         = sum(fg_FBS$fg_redmeat(fg_FBS), FBS_intake("kcal/d_w",fg_FBS,r,year))
         / sum(fg_FBS$fg_redmeat(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year));
         
g2kcal_yrs("fish",r,year)
         $ sum(fg_FBS$fg_fish(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year))
         = sum(fg_FBS$fg_fish(fg_FBS), FBS_intake("kcal/d_w",fg_FBS,r,year))
         / sum(fg_FBS$fg_fish(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year));

g2kcal_yrs("fruits",r,year)
         $ sum(fg_FBS$fg_fruits(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year))
         = sum(fg_FBS$fg_fruits(fg_FBS), FBS_intake("kcal/d_w",fg_FBS,r,year))
         / sum(fg_FBS$fg_fruits(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year));
         
g2kcal_yrs("nuts_seeds",r,year)
         $ sum(fg_FBS$fg_nuts_seeds(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year))
         = sum(fg_FBS$fg_nuts_seeds(fg_FBS), FBS_intake("kcal/d_w",fg_FBS,r,year))
         / sum(fg_FBS$fg_nuts_seeds(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year));
         
g2kcal_yrs("grains",r,year)
         $ sum(fg_FBS$fg_grains(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year))
         = sum(fg_FBS$fg_grains(fg_FBS), FBS_intake("kcal/d_w",fg_FBS,r,year))
         / sum(fg_FBS$fg_grains(fg_FBS), FBS_intake("g/d_w",fg_FBS,r,year));
         
*-------------------------------------------------------------------------------

*        load SUA data (for processed foods):

parameter cns_SUA consumption of foods from SUA;
set       fg_SUA  /yoghurt, cheese/;

$gdxin   %input_dir%/all_raw_data.gdx
$load    cns_SUA

*-------------------------------------------------------------------------------
*        Previous compiled data (exit before if in run-through mode)
*-------------------------------------------------------------------------------

*        IOM data:

parameter EER_GDD, pop_GDD;

$gdxin   %input_dir%/IOM_data_%year%.gdx
$load    EER_GDD, pop_GDD

*        assign data:

parameter IOM_energy    age-aggregated energy requirements
          IOM_pop       age-aggregated population stats;
          
IOM_energy(r,sex,age,urban,year,stats)
         = EER_GDD(r,sex,age,urban,year,stats); 

IOM_pop(r,sex,age,year,urban)
         = pop_GDD(r,sex,age,year,urban);    

*-------------------------------------------------------------------------------

*        GDD data:
         
parameter GDD_intake_IOM                     energy-corrected GDD intake
          GDD_ratio_age_sex_urban_edu        socio-demographic adjustment ratios
          GDD_pop                            GDD population data;
          
$gdxin %input_dir%/GDD_data_%year%.gdx
$load  GDD_intake_IOM, GDD_ratio_age_sex_urban_edu, GDD_pop  

*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------
