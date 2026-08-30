*-------------------------------------------------------------------------------
*        Sets
*-------------------------------------------------------------------------------

*        ordering:

set      order_age  /
         "all-a", "<1", 1*100, "2-5", "6-10", "11-14", "15-19", "20-24", "25-29",
         "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
         "70-74", "75-79", "80-84", "85-89", "90-94", "95+" ,"85+",
         mean, mid, 0-20, 20-40, 40-60, 60-80, "80+"
         /
         order_fgs  /
         wheat, rice, maize, barley, rye, oats, millet, sorghum, othr_grains,
         potato, cassava, sweet_potato, othr_roots, yams,
         tomato, onion, othr_vegetables,
         orange, lemon, grapefruit, citrus, coconuts, pineapple, dates,
         apple, grapes, preserved_olives, othr_fruits,
         banana, plantains,
         beans, peas, othr_pulse,
         soyabeans,
         nuts, groundnut,
         seed_sunflower, seed_rape, seed_cotton, seed_sesame, seed_palmkernel, seed_oilcrop,
         oil_soyabeans, oil_groundnut, oil_sunflower, oil_rape, oil_cotton, oil_sesame, oil_olive, oil_ricebran, oil_maize, oil_oilcrop,
         oil_palmkernel, oil_palm, oil_coconut,
         sugar_cane, sugar_beet, sugar_non, raw_sugar, sweeteners, honey,
         poultry, beef, lamb, pork, othr_meat,offals,
         milk, eggs, fat_ani, oil_fish_body, oil_fish_liver, butter, cream, 
         fish_freshw, fish_pelag, fish_demrs, fish_marine, fish_aquatic, fish_aquaticplant, crustaceans, cephalopods, othr_molluscs,
         coffee, cocoa, tea,
         pepper, pimento, cloves, othr_spices,
         wine, beer, beverage_ferment, beverage_alcoholic,
         food_Infant, miscellaneous,
         whole_grains, prc_grains, red_meat, prc_meat,
         juice, fruits_actl, SSBs, sugar_actl,
         yoghurt, cheese, "milk_actl"
         /;

*-------------------------------------------------------------------------------

*        countries:
         
sets     r_input(*)                     countries;

$gdxin   %input_dir%/rgs_inputs.gdx 
$load    r_input=r

*        allocate country definitions:

sets     r(*)              countries
         ctrl_flag_lbl     control labels /yes, no, "1", "0"/;
scalar   set_ctr_flag      single-country run flag /0/;

set_ctr_flag$(sameas("%set_ctr%","yes") or sameas("%set_ctr%","1")) = 1;
         
if(      not set_ctr_flag, 
         r(r_input)                 = yes;
         );

*        constrain to one country if control is enabled:

if(      set_ctr_flag,
         r("%ctr_sel%")             = yes;
         );

*-------------------------------------------------------------------------------

*        age groups:

set      age_agg           aggregate age groups       /
         "all-a"           all ages
         0-9               children
         10-19             adolescents
         20-39             young adults
         40-64             middle-age adults
         "65+"             senior adults
         20+               all adults
         /
         age_agg_s  aggregate age groups       /
         0-9               children
         10-19             adolescents
         20-39             young adults
         40-64             middle-age adults
         "65+"             senior adults
         /
         age_5y            age groups in five year intervals /
         "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39"
         "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
         "80+", "all-a"
         /
         age_sy            age in single year brackets plus aggregate   /
         "all-a", "<1", 1*94, 95
         /
         age_sy_s(age_sy)  age in single year brackets (summable)        /
         "<1", 1*94, 95
         /
         age               age groups of GDD /
         "all-a", "<1", "1", "2-5", "6-10", "11-14", "15-19", "20-24", "25-29",
         "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
         "70-74", "75-79", "80-84", "85-89", "90-94", "95+"
         /
         age_s(age)        age groups of GDD (summable) /
         "<1", "1", "2-5", "6-10", "11-14", "15-19", "20-24", "25-29",
         "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
         "70-74", "75-79", "80-84", "85-89", "90-94", "95+"
         /
         map_age_GDD(age,age_sy)         /
         "all-a"."all-a"
         "<1"   ."<1"
         "1"    .1
         "2-5"  .(2*5)
         "6-10" .(6*10)
         "11-14".(11*14)
         "15-19".(15*19)
         "20-24".(20*24)
         "25-29".(25*29)
         "30-34".(30*34)
         "35-39".(35*39)
         "40-44".(40*44)
         "45-49".(45*49)
         "50-54".(50*54)
         "55-59".(55*59)
         "60-64".(60*64)
         "65-69".(65*69)
         "70-74".(70*74)
         "75-79".(75*79)
         "80-84".(80*84)
         "85-89".(85*89)
         "90-94".(90*94)
         "95+"."95"
         /
         map_age_agg(age,age_agg)         /
         "all-a"."all-a"
         "<1"."0-9"
         "1"."0-9"
         "2-5"."0-9"
         "6-10"."0-9"
         "11-14"."10-19"
         "15-19"."10-19"
         "20-24"."20-39"
         "25-29"."20-39"
         "30-34"."20-39"
         "35-39"."20-39"
         "40-44"."40-64"
         "45-49"."40-64"
         "50-54"."40-64"
         "55-59"."40-64"
         "60-64"."40-64"
         "65-69"."65+"
         "70-74"."65+"
         "75-79"."65+"
         "80-84"."65+"
         "85-89"."65+"
         "90-94"."65+"
         "95+"."65+"
         /
         map_age_5y(age,age_5y)         /
         "all-a"."all-a"
         "<1"."0-4"
         "1"."0-4"
         "2-5"."0-4"
         "6-10"."5-9"
         "11-14"."10-14"
         "15-19"."15-19"
         "20-24"."20-24"
         "25-29"."25-29"
         "30-34"."30-34"
         "35-39"."35-39"
         "40-44"."40-44"
         "45-49"."45-49"
         "50-54"."50-54"
         "55-59"."55-59"
         "60-64"."60-64"
         "65-69"."65-69"
         "70-74"."70-74"
         "75-79"."75-79"
         "80-84"."80+"
         "85-89"."80+"
         "90-94"."80+"
         "95+"."80+"
         /;
         
*-------------------------------------------------------------------------------

*        other sets:

set      sex               sex      /
         BTH, FML, MLE
         /
         sex_s(sex)        summable sexes /
         FML, MLE
         /
         edu               educational levels         /
         all-e, low, medium, high
         /
         edu_s(edu)        summable educational levels         /
         low, medium, high
         /
         urban             urbanisation      /
         all-u, rural, urban
         /
         urban_s(urban)    summable urbanisation      /
         rural, urban
         /
         stats             stats    /
         mean, low, high
         /
         PAL_itm_p         physcial activity levels   /
         all-PAL, sedentary, low_activity, active, vry_active
         /
         year              year of analysis   /
         1990, 1995, 2000, 2005, 2010, 2015, 2016, 2017, 2018,
         2019, 2020, 2021, 2022
         /;
                    
*-------------------------------------------------------------------------------

*        GDD sets:

set      fg_GDD            GDD food groups   /
         fruits, vegetables, roots, potatoes, othr_roots, legumes, nuts, grains, prc_grains, whole_grains, 
         total_meat, total_red_meat, red_meat, prc_meat, fish, eggs, dairy, milk, yoghurt, cheese, juice, SSBs, 
         tea_coffee, sodium, SFA, MUFA, n6, n3_plant, n3_seafood, energy
         /
         yrs_GDD           years with GDD data        /
         1990, 1995, 2000, 2005, 2010, 2015, 2018
         /
         fg_FBS_pct        FBS food groups for forward projection  /
         wheat
         rice
         maize
         barley
         rye
         oats
         millet
         sorghum
         othr_grains
         potato
         cassava
         sweet_potato
         othr_roots
         yams
         tomato
         onion
         othr_vegetables
         orange
         lemon
         grapefruit
         citrus
         coconuts
         pineapple
         dates
         apple
         grapes
         preserved_olives
         othr_fruits
         banana
         plantains
         beans
         peas
         othr_pulse
         soyabeans
         nuts
         groundnut
         seed_sunflower
         seed_rape
         seed_cotton
         seed_sesame
         seed_palmkernel
         seed_oilcrop
         oil_soyabeans
         oil_groundnut
         oil_sunflower
         oil_rape
         oil_cotton
         oil_sesame
         oil_olive
         oil_ricebran
         oil_maize
         oil_oilcrop
         oil_palmkernel
         oil_palm
         oil_coconut
         sugar_cane
         sugar_beet
         sugar_non
         raw_sugar
         sweeteners
         honey
         poultry
         beef
         lamb
         pork
         othr_meat
         offals
         milk
         eggs
         fat_ani
         oil_fish_body
         oil_fish_liver
         butter
         cream
         fish_freshw
         fish_pelag
         fish_demrs
         fish_marine
         fish_aquatic
         fish_aquaticplant
         crustaceans
         cephalopods
         othr_molluscs
         coffee
         cocoa
         tea
         pepper
         pimento
         cloves
         othr_spices
         wine
         beer
         beverage_ferment
         beverage_alcoholic
         food_Infant
         miscellaneous
         total
         /
         unit_wp           unit of intake    /
         "g/d_w", "kcal/d_w"
         /
         unit_sp           unit of supply    /
         "g/d", "kcal/d"
         /
         unit_p            all units         /
         "g/d_w", "kcal/d_w", "g/d", "kcal/d"
         /
         yrs_FBS           years with FBS data        /
         2010, 2018, 2020
         /
         map_fg_FBS(fg_GDD,fg_FBS_pct) mapping of food groups from FBS to GDD/
         fruits.(orange, lemon, grapefruit, citrus, coconuts, pineapple, dates, apple, grapes, preserved_olives, othr_fruits, banana, plantains)
         vegetables.(tomato, onion, othr_vegetables)
         roots.(potato, cassava, sweet_potato, othr_roots, yams)
         potatoes.(potato, cassava, sweet_potato, othr_roots, yams)
         othr_roots.(potato, cassava, sweet_potato, othr_roots, yams)
         legumes.(beans, peas, othr_pulse,soyabeans)
         nuts.(nuts, groundnut, seed_sunflower, seed_rape, seed_cotton, seed_sesame, seed_palmkernel, seed_oilcrop)
         grains.(wheat, rice, maize, barley, rye, oats, millet, sorghum, othr_grains)
         prc_grains.(wheat, rice, maize, barley, rye, oats, millet, sorghum, othr_grains)
         whole_grains.(wheat, rice, maize, barley, rye, oats, millet, sorghum, othr_grains)
         total_red_meat.(beef, lamb, pork)
         prc_meat.(beef, lamb, pork)
         red_meat.(beef, lamb, pork)
         total_meat.(beef, lamb, pork, offals, othr_meat, fish_freshw, fish_pelag, fish_demrs, fish_marine, fish_aquatic, fish_aquaticplant, crustaceans, cephalopods, othr_molluscs)
         fish.(fish_freshw, fish_pelag, fish_demrs, fish_marine, fish_aquatic, fish_aquaticplant, crustaceans, cephalopods, othr_molluscs)
         eggs.eggs
         dairy.milk
         milk.milk
         yoghurt.milk
         cheese.milk
         juice.(sugar_cane, sugar_beet, sugar_non, raw_sugar, sweeteners, honey)
         SSBs.(sugar_cane, sugar_beet, sugar_non, raw_sugar, sweeteners, honey)
         tea_coffee.(coffee, cocoa, tea)
         SFA.(oil_palmkernel, oil_palm, oil_coconut)
         MUFA.(oil_soyabeans, oil_groundnut, oil_sunflower, oil_rape, oil_cotton, oil_sesame, oil_olive, oil_ricebran, oil_maize, oil_oilcrop)
         n6.(oil_soyabeans, oil_groundnut, oil_sunflower, oil_rape, oil_cotton, oil_sesame, oil_olive, oil_ricebran, oil_maize, oil_oilcrop)
         n3_plant.(oil_soyabeans, oil_groundnut, oil_sunflower, oil_rape, oil_cotton, oil_sesame, oil_olive, oil_ricebran, oil_maize, oil_oilcrop)
         n3_seafood.(fish_freshw, fish_pelag, fish_demrs, fish_marine, fish_aquatic, fish_aquaticplant, crustaceans, cephalopods, othr_molluscs)
         sodium.total
         energy.total
         /;

*-------------------------------------------------------------------------------
   
*        FBS sets:

set      fg_FBS            aggregated food groups from FBS /
         all-fg
         wheat
         rice
         maize
         barley
         rye
         oats
         millet
         sorghum
         othr_grains
         potato
         cassava
         sweet_potato
         othr_roots
         yams
         tomato
         onion
         othr_vegetables
         orange
         lemon
         grapefruit
         citrus
         coconuts
         pineapple
         dates
         apple
         grapes
         preserved_olives
         othr_fruits
         banana
         plantains
         beans
         peas
         othr_pulse
         soyabeans
         nuts
         groundnut
         seed_sunflower
         seed_rape
         seed_cotton
         seed_sesame
         seed_palmkernel
         seed_oilcrop
         oil_soyabeans
         oil_groundnut
         oil_sunflower
         oil_rape
         oil_cotton
         oil_sesame
         oil_olive
         oil_ricebran
         oil_maize
         oil_oilcrop
         oil_palmkernel
         oil_palm
         oil_coconut
         sugar_cane
         sugar_beet
         sugar_non
         raw_sugar
         sweeteners
         honey
         poultry
         beef
         lamb
         pork
         othr_meat
         offals
         milk
         eggs
         fat_ani
         oil_fish_body
         oil_fish_liver
         butter
         cream
         fish_freshw
         fish_pelag
         fish_demrs
         fish_marine
         fish_aquatic
         fish_aquaticplant
         crustaceans
         cephalopods
         othr_molluscs
         coffee
         cocoa
         tea
         pepper
         pimento
         cloves
         othr_spices
         wine
         beer
         beverage_ferment
         beverage_alcoholic
         food_Infant
         miscellaneous
         /
         fg_FBS_s(fg_FBS)  aggregated food groups from FBS (summable);
         fg_FBS_s(fg_FBS)           = yes;
         fg_FBS_s("all-fg")         = no;

set      fg_FBS_add        additional food groups     /
         whole_grains, prc_grains, red_meat, prc_meat,
         juice, fruits_actl, SSBs, sugar_actl,
         yoghurt, cheese, "milk_actl"
         /
         fg_FBS_e          extended food groups from FBS;
         fg_FBS_e(fg_FBS)           = yes;
         fg_FBS_e(fg_FBS_add)       = yes;

set      map_fg_GDD(*,*)   map food groups between GDD and FBS       /
*        (fg_FBS_e,fg_GDD)      map of food groups between FBS and GDD       /
         wheat              .grains
         rice               .grains
         maize              .grains
         barley             .grains
         rye                .grains
         oats               .grains
         millet             .grains
         sorghum            .grains
         othr_grains        .grains
         potato             .potatoes
         cassava            .roots
         sweet_potato       .roots
         othr_roots         .othr_roots
         yams               .roots
         tomato             .vegetables
         onion              .vegetables
         othr_vegetables    .vegetables
         orange             .fruits
         lemon              .fruits
         grapefruit         .fruits
         citrus             .fruits
         coconuts           .fruits
         pineapple          .fruits
         dates              .fruits
         apple              .fruits
         grapes             .fruits
         preserved_olives   .fruits
         othr_fruits        .fruits
         banana             .fruits
         plantains          .fruits
         beans              .legumes
         peas               .legumes
         othr_pulse         .legumes
         soyabeans          .legumes
         nuts               .nuts
         groundnut          .nuts
         seed_sunflower     .nuts
         seed_rape          .nuts
         seed_cotton        .nuts
         seed_sesame        .nuts
         seed_palmkernel    .nuts
         seed_oilcrop       .nuts
         oil_soyabeans      .MUFA
         oil_groundnut      .MUFA
         oil_sunflower      .MUFA
         oil_rape           .MUFA
         oil_cotton         .MUFA
         oil_sesame         .MUFA
         oil_olive          .MUFA
         oil_ricebran       .MUFA
         oil_maize          .MUFA
         oil_oilcrop        .MUFA
         oil_palmkernel     .SFA
         oil_palm           .SFA
         oil_coconut        .SFA
         sugar_cane         .SSBs
         sugar_beet         .SSBs
         sugar_non          .SSBs
         raw_sugar          .SSBs
         sweeteners         .SSBs
         honey              .SSBs
         milk               .dairy
         eggs               .eggs
         fish_freshw        .fish
         fish_pelag         .fish
         fish_demrs         .fish
         fish_marine        .fish
         fish_aquatic       .fish
         fish_aquaticplant  .fish
         crustaceans        .fish
         cephalopods        .fish
         othr_molluscs      .fish    
         poultry            .total_meat
         beef               .total_red_meat
         lamb               .total_red_meat
         pork               .total_red_meat
         offals             .total_meat
         othr_meat          .total_red_meat
         coffee             .tea_coffee
         cocoa              .tea_coffee
         tea                .tea_coffee
         pepper             .energy
         pimento            .energy
         cloves             .energy
         othr_spices        .energy
         wine               .energy
         beer               .energy
         beverage_ferment   .energy
         beverage_alcoholic .energy
         food_Infant        .energy
         miscellaneous      .energy
         fat_ani            .SFA
         oil_fish_body      .n3_seafood
         oil_fish_liver     .n3_seafood
         butter             .dairy
         cream              .dairy
*
         whole_grains       .whole_grains
         prc_grains         .prc_grains
         red_meat           .red_meat
         prc_meat           .prc_meat
         juice              .juice
         fruits_actl        .fruits
         SSBs               .SSBs
         sugar_actl         .SSBs
         yoghurt            .yoghurt
         cheese             .cheese
         "milk_actl"        .milk
         /;
         
set      type              type of processing /
         prim, prcd
         /;

parameter serving(*)         serving sizes      /
         wheat              45
         rice               45
         maize              45
         barley             45
         rye                45
         oats               45
         millet             45
         sorghum            45
         othr_grains        45
         potato             100
         cassava            100
         sweet_potato       100
         othr_roots         100
         yams               100
         tomato             100
         onion              100
         othr_vegetables    100
         orange             100
         lemon              100
         grapefruit         100
         citrus             100
         coconuts           100
         pineapple          100
         dates              100
         apple              100
         grapes             100
         preserved_olives   100
         othr_fruits        100
         banana             100
         plantains          100
         beans              50
         peas               50
         othr_pulse         50
         soyabeans          50
         nuts               25
         groundnut          25
         seed_sunflower     25
         seed_rape          25
         seed_cotton        25
         seed_sesame        25
         seed_palmkernel    25
         seed_oilcrop       25
         oil_soyabeans      15
         oil_groundnut      15
         oil_sunflower      15
         oil_rape           15
         oil_cotton         15
         oil_sesame         15
         oil_olive          15
         oil_ricebran       15
         oil_maize          15
         oil_oilcrop        15
         oil_palmkernel     15
         oil_palm           15
         oil_coconut        15
         sugar_cane         8
         sugar_beet         8
         sugar_non          8
         raw_sugar          8
         sweeteners         8
         honey              8
         poultry            100
         beef               100
         lamb               100
         pork               100
         offals             100
         othr_meat          100
         milk               250
         eggs               50
         fat_ani            15
         oil_fish_liver     15
         oil_fish_body      15
         butter             5
         cream              15
         fish_freshw        100
         fish_pelag         100
         fish_demrs         100
         fish_marine        100
         fish_aquatic       100
         fish_aquaticplant  100
         crustaceans        100
         cephalopods        100
         othr_molluscs      100
         coffee             5
         cocoa              5
         tea                5
         pepper             1.25
         pimento            1.25
         cloves             1.25
         othr_spices        1.25
         wine               140
         beer               140
         beverage_ferment   140
         beverage_alcoholic 140
         food_Infant        5
         miscellaneous      5
*
         whole_grains      45
         prc_grains        45
         red_meat          100
         prc_meat          50
         yoghurt           170
*        average between hard and soft cheese
         cheese            17.5
         "milk_actl"       250
         /;
         
*-------------------------------------------------------------------------------

*        set year of analysis:

scalar year_sel_flag year-selection flag /0/;

year_sel_flag$(sameas("%year_sel%","yes") or sameas("%year_sel%","1")) = 1;

if(year_sel_flag,
year(year)     = no;
year("%year%") = yes;
);

*-------------------------------------------------------------------------------
*-------------------------------------------------------------------------------
