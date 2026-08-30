$title Population-weighted country-level food intake

* Demand_ori contains per-capita intake by country, food, age and sex.
* pop contains the corresponding population counts. This first stage
* aggregates Demand_ori to a country-by-food per-capita intake using the
* population of every age-sex group as the weight.

$if not set inputGdx  $set inputGdx  "%system.fp%all_raw_data_newIntensity.gdx"
$if not set outputGdx $set outputGdx "%system.fp%sensitivity-EAT-Lancet-results.gdx"
$if not set outputXlsx $set outputXlsx "%system.fp%sensitivity-EAT-Lancet-demand.xlsx"

Sets
    i       countries
    j       food items
    age     age groups
    sex     sex groups
;

Parameters
    Demand_ori(i,j,age,sex)  original intake by demographic group
    pop(i,age,sex)           population by demographic group
;

$gdxin %inputGdx%
$load i j age sex Demand_ori pop
$gdxin

Parameters
    country_population(i)       total population represented for each country
    population_weight(i,age,sex) population share of each age-sex group
    Demand_country(i,j)          population-weighted country-level intake
;

country_population(i) = sum((age,sex), pop(i,age,sex));

abort$(sum(i$(country_population(i) <= 0), 1) > 0)
    "At least one country has zero or negative total population",
    country_population;

population_weight(i,age,sex) = pop(i,age,sex) / country_population(i);

Demand_country(i,j) = sum(
    (age,sex),
    Demand_ori(i,j,age,sex) * population_weight(i,age,sex)
);

* Internal check: the demographic weights must sum to one for every country.
Parameter weight_sum(i) sum of demographic weights by country;
weight_sum(i) = sum((age,sex), population_weight(i,age,sex));

abort$(smax(i, abs(weight_sum(i) - 1)) > 1e-10)
    "Population weights do not sum to one for at least one country",
    weight_sum;

Set
    x1(j)     Grains           /wheat, rice, barley, maize, rye, oats, millet, sorghum, othr_grains/
    x2(j)     Root             /cassava, potato, sweet_potato, yams, othr_roots/
    x3(j)     Vegetable        /tomato, onion, othr_vegetables/
    x4(j)     Fruits           /coconuts, preserved_olives, orange, lemon, grapefruit, citrus, banana, plantains, apple, pineapple, dates, grapes, othr_fruits/
    x5(j)     Dairy            /milk/
    x6(j)     Beef&lamb        /beef, lamb/
    x7(j)     Pork             /pork/
    x8(j)     Poultry          /poultry/
    x9(j)     Eggs             /eggs/
    x10(j)    fish             /fish_freshw, fish_demrs, fish_pelag, fish_marine, crustaceans, cephalopods, othr_molluscs, fish_aquatic/
    x11(j)    Legumes          /beans, peas, othr_pulse, soyabeans/
    x12(j)    Nuts             /nuts, groundnut, seed_sunflower, seed_rape, seed_cotton, seed_sesame, seed_oilcrop/
    x13(j)    palm_oil         /oil_palmkernel, oil_palm, oil_coconut/
    x14(j)    Unsat_oils       /oil_soyabeans, oil_groundnut, oil_sunflower, oil_rape, oil_cotton, oil_sesame, oil_olive, oil_ricebran, oil_maize, oil_oilcrop,oil_fish_body, oil_fish_liver/
    x15(j)    Lard             /fat_ani/
    x16(j)    Dairy_fat        /butter, cream/
    x17(j)    Sugar            /sugar_cane, sugar_non, raw_sugar, sweeteners, honey/
;

Set
    eat_group "Aggregated EAT-Lancet food groups"
        /grains, roots, vegetables, fruits, dairy, beef_lamb, pork,
         poultry, eggs, fish, legumes, nuts, palm_oil, unsat_oils,
         lard, dairy_fat, sugar/
    eat_map(eat_group,j) "Mapping from raw foods to EAT-Lancet groups"
    eat_excluded(j) "Raw foods intentionally excluded from the 17 target groups"
        /othr_meat, offals, fish_aquaticplant/
;

eat_map('grains',j)      = x1(j);
eat_map('roots',j)       = x2(j);
eat_map('vegetables',j)  = x3(j);
eat_map('fruits',j)      = x4(j);
eat_map('dairy',j)       = x5(j);
eat_map('beef_lamb',j)   = x6(j);
eat_map('pork',j)        = x7(j);
eat_map('poultry',j)     = x8(j);
eat_map('eggs',j)        = x9(j);
eat_map('fish',j)        = x10(j);
eat_map('legumes',j)     = x11(j);
eat_map('nuts',j)        = x12(j);
eat_map('palm_oil',j)    = x13(j);
eat_map('unsat_oils',j)  = x14(j);
eat_map('lard',j)        = x15(j);
eat_map('dairy_fat',j)   = x16(j);
eat_map('sugar',j)       = x17(j);

* EAT-Lancet 2019 healthy reference diet targets for adults consuming
* approximately 2,500 kcal per day. Units: grams per person per day.
* Legumes = 50 g dry beans/lentils/peas + 25 g soy foods.
* Nuts = 25 g peanuts + 25 g tree nuts.
Parameter EAT_target(eat_group) "EAT-Lancet target intake (g/person/day)" /
    grains       232
    roots         50
    vegetables   300
    fruits       200
    dairy        250
    beef_lamb      7
    pork           7
    poultry       29
    eggs          13
    fish          28
    legumes       75
    nuts          50
    palm_oil       6.8
    unsat_oils    40
    lard           5
    dairy_fat      0
    sugar         31
/;

Parameter
    Demand_country_group(i,eat_group) "Original country intake by EAT-Lancet group"
    EAT_scale(i,eat_group) "Within-group proportional scaling factor"
    Demand_country_EAT(i,j) "Food-item intake after proportional EAT-Lancet scaling"
    Demand_country_EAT_check(i,eat_group) "Adjusted intake re-aggregated by target group"
    EAT_target_gap(i,eat_group) "Adjusted group intake minus its EAT-Lancet target"
    food_conversion(j) "Raw-food to EAT-Lancet edible-equivalent conversion factor"
    map_count(j) "Number of EAT-Lancet groups assigned to each raw food item"
    group_item_count(eat_group) "Number of raw food items within each target group"
    zero_structure_count(eat_group) "Countries with zero original intake and a positive target"
;

* A mapped food item must not be counted in more than one group.
map_count(j) = sum(eat_group$eat_map(eat_group,j), 1);
group_item_count(eat_group) = sum(j$eat_map(eat_group,j), 1);

abort$(smin(eat_group, group_item_count(eat_group)) <= 0)
    "At least one EAT-Lancet group contains no raw food items",
    group_item_count;
abort$(smax(j, map_count(j)) > 1)
    "At least one raw food item is assigned to multiple EAT-Lancet groups",
    map_count;

abort$(sum(j$(map_count(j) = 0 and not eat_excluded(j)), 1) > 0)
    "At least one raw food item is neither mapped nor explicitly excluded",
    map_count;

abort$(sum(j$(map_count(j) > 0 and eat_excluded(j)), 1) > 0)
    "An explicitly excluded food item is also assigned to a target group",
    map_count;

* Original national intake summed over the raw foods belonging to each group.
* Sugar cane is converted to sugar equivalent at a recovery rate of 0.14;
* all other raw foods retain a conversion factor of one.
food_conversion(j) = 1;
food_conversion('sugar_cane') = 0.14;

Demand_country_group(i,eat_group) = sum(
    j$eat_map(eat_group,j),
    Demand_country(i,j) * food_conversion(j)
);

* A positive target cannot be reached by proportional scaling when the
* original group total is zero because no within-group structure exists.
Set zero_structure(i,eat_group) "Positive target but zero original group intake";
zero_structure(i,eat_group) = yes$(
    EAT_target(eat_group) > 0
    and Demand_country_group(i,eat_group) = 0
);
zero_structure_count(eat_group) = sum(i$zero_structure(i,eat_group), 1);

* All foods within a country-group receive the same multiplier, preserving
* their original shares exactly. A zero EAT target gives a multiplier of zero.
EAT_scale(i,eat_group)$Demand_country_group(i,eat_group) =
    EAT_target(eat_group) / Demand_country_group(i,eat_group);

* Keep intentionally excluded foods unchanged. For mapped groups with a
* positive original total, preserve the original within-group structure. If
* the original total is zero, divide the target equally among all raw foods in
* that group because no original structure exists to preserve.
Demand_country_EAT(i,j) = Demand_country(i,j);
Demand_country_EAT(i,j)$map_count(j) = sum(
    eat_group$eat_map(eat_group,j),
    Demand_country(i,j) * EAT_scale(i,eat_group)
    + (EAT_target(eat_group)
        / group_item_count(eat_group)
        / food_conversion(j))
        $zero_structure(i,eat_group)
);

* Re-aggregate the adjusted raw foods and verify all mathematically feasible
* country-group combinations against the targets.
Demand_country_EAT_check(i,eat_group) = sum(
    j$eat_map(eat_group,j),
    Demand_country_EAT(i,j) * food_conversion(j)
);
EAT_target_gap(i,eat_group) =
    Demand_country_EAT_check(i,eat_group) - EAT_target(eat_group);

abort$(smax((i,eat_group), abs(EAT_target_gap(i,eat_group))) > 1e-8)
    "Adjusted group intake failed to reproduce at least one target",
    EAT_target_gap;

execute_unload '%outputGdx%'
    i,
    j,
    eat_group,
    eat_map,
    eat_excluded,
    country_population,
    population_weight,
    Demand_country,
    Demand_country_group,
    EAT_scale,
    food_conversion,
    group_item_count,
    Demand_country_EAT,
    Demand_country_EAT_check,
    EAT_target_gap,
    zero_structure,
    zero_structure_count,
    EAT_target;

* Dense copies used only for Excel export. EPS preserves structurally zero
* cells and all-zero food columns; ExcelWriter converts EPS back to numeric 0.
Parameters
    Demand_country_xlsx(i,j) "Dense Excel copy of original country intake"
    Demand_country_EAT_xlsx(i,j) "Dense Excel copy of adjusted country intake"
;
Demand_country_xlsx(i,j) = Demand_country(i,j);
Demand_country_EAT_xlsx(i,j) = Demand_country_EAT(i,j);
Demand_country_xlsx(i,j)$(Demand_country_xlsx(i,j) = 0) = EPS;
Demand_country_EAT_xlsx(i,j)$(Demand_country_EAT_xlsx(i,j) = 0) = EPS;

* Export the original and EAT-Lancet-adjusted country-by-food matrices to one
* Excel workbook. Countries are rows and all 81 raw food items are columns.
embeddedCode Connect:
- GAMSReader:
    symbols:
      - name: Demand_country_xlsx
      - name: Demand_country_EAT_xlsx
- ExcelWriter:
    file: %outputXlsx%
    valueSubstitutions: {EPS: 0}
    symbols:
      - name: Demand_country_xlsx
        range: Demand_country!A1
      - name: Demand_country_EAT_xlsx
        range: Demand_country_EAT!A1
endEmbeddedCode
