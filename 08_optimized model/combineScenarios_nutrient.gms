$Title Combine scenario diets by country-age-sex with nutrient constraints

option LP=SoPlex;
option Threads=8;

$setglobal ROOT "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/08_optimized model"
$setglobal OUTDIR "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output"
$setglobal OUT_GDX "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00base_weighted_new111_17groups_sugar014.gdx"

Set
    i
    j
    age
    adultAge(age)
    sex
    k /Cost, Structure, Land, GHG, Water, Acid, Eutr/
    b /GrainsMin, GrainsMax, RootMax, VegetableMin, VegetableMax, FruitsMin, FruitsMax,
       DairyMax, BeefLambMax, PorkMax, PoultryMax, EggsMax, FishMax,
       LegumesMin, LegumesMax, NutsMin, NutsMax, PalmOilMax,
       UnsatOilsMin, UnsatOilsMax, LardMax, SugarMax/
;

$gdxin "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/all_raw_data_newIntensity.gdx"
$load i j age sex
$gdxin

Set
    excludedCountry(i) / MUS /
    x1(j)     Grains           /wheat, rice, barley, maize, rye, oats, millet, sorghum, othr_grains/
    x2(j)     Root             /cassava, potato, sweet_potato, yams, othr_roots/
    x3(j)     Vegetable        /tomato, onion, othr_vegetables/
    x4(j)     Fruits           /coconuts, preserved_olives, orange, lemon, grapefruit, citrus, banana, plantains, apple, pineapple, dates, grapes, othr_fruits/
    x5(j)     Dairy            /milk/
    x6(j)     Beef_lamb        /beef, lamb/
    x7(j)     Pork             /pork/
    x8(j)     Poultry          /poultry/
    x9(j)     Eggs             /eggs/
    x10(j)    Fish             /fish_freshw, fish_demrs, fish_pelag, fish_marine, crustaceans, cephalopods, othr_molluscs, fish_aquatic/
    x11(j)    Legumes          /beans, peas, othr_pulse, soyabeans/
    x12(j)    Nuts             /nuts, groundnut, seed_sunflower, seed_rape, seed_cotton, seed_sesame, seed_oilcrop/
    x13(j)    Palm_oil         /oil_palmkernel, oil_palm, oil_coconut/
    x14(j)    Unsat_oils       /oil_soyabeans, oil_groundnut, oil_sunflower, oil_rape, oil_cotton, oil_sesame, oil_olive, oil_ricebran, oil_maize, oil_oilcrop, oil_fish_body, oil_fish_liver/
    x15(j)    Lard             /fat_ani/
    x16(j)    Dairy_fat        /butter, cream/
    x17(j)    Sugar            /sugar_cane, sugar_non, raw_sugar, sweeteners, honey/
;

Parameter
    alpha(k)
    x_result_all(k,i,j,age,sex)
    x_result_new(i,j,age,sex)
    x_point(k,j,age,sex)
    abs_deviation(k,i,j,age,sex)
    distance_result(k,i,age,sex)
    scenario_deviation_i(i,k)
    weighted_scenario_deviation_i(i,k)
    scenario_deviation(k)
    weighted_scenario_deviation(k)
    objective_result_i(i)
    objective_result
    combine_model_status(i)
    combine_solve_status(i)
    KcalAge(j)
    ProteinAge(j)
    CalciumAge(j)
    IronAge(j)
    MagnesiumAge(j)
    CopperAge(j)
    PhosphorusAge(j)
    ZincAge(j)
    ThiaminAge(j)
    NiacinAge(j)
    RiboflavinAge(j)
    FolateAge(j)
    VitaminAAge(j)
    VitaminCAge(j)
    VitaminB6Age(j)
    PotassiumAge(j)
    SugarConversion(j)
;

Parameter
    x_cost(i,j,age,sex)
    x_structure(i,j,age,sex)
    x_land(i,j,age,sex)
    x_ghg(i,j,age,sex)
    x_water(i,j,age,sex)
    x_Acid(i,j,age,sex)
    x_Eutr(i,j,age,sex)
    Kcal_ori(i,j)
    Protein_ori(i,j)
    Calcium_ori(i,j)
    Iron_ori(i,j)
    Magnesium_ori(i,j)
    Copper_ori(i,j)
    Phosphorus_ori(i,j)
    Zinc_ori(i,j)
    Thiamin_ori(i,j)
    Niacin_ori(i,j)
    Riboflavin_ori(i,j)
    Folate_ori(i,j)
    VitaminA_ori(i,j)
    VitaminC_ori(i,j)
    VitaminB6_ori(i,j)
    Potassium_ori(i,j)
    GrainsMin(age)
    GrainsMax(age)
    RootMax(age)
    VegetableMin(age)
    VegetableMax(age)
    FruitsMin(age)
    FruitsMax(age)
    DairyMax(age)
    BeefLambMax(age)
    PorkMax(age)
    PoultryMax(age)
    EggsMax(age)
    FishMax(age)
    LegumesMin(age)
    LegumesMax(age)
    NutsMin(age)
    NutsMax(age)
    PalmOilMax(age)
    UnsatOilsMin(age)
    UnsatOilsMax(age)
    LardMax(age)
    SugarMax(age)
    DietGroupBounds(age,b)
;

Scalar
    alpha_sum
;

$include %ROOT%/Parameter_indexed.gms

$gdxin "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/all_raw_data_newIntensity.gdx"
$load Kcal_ori=calories
$load Protein_ori=Protein
$load Calcium_ori=Calcium
$load Iron_ori=Iron
$load Magnesium_ori=Magnesium
$load Copper_ori=Copper
$load Phosphorus_ori=Phosphorus
$load Zinc_ori=Zinc
$load Thiamin_ori=Thiamin
$load Niacin_ori=Niacin
$load Riboflavin_ori=Riboflavin
$load Folate_ori=Folate
$load VitaminA_ori=VitaminA
$load VitaminC_ori=VitaminC
$load VitaminB6_ori=VitaminB6
$load Potassium_ori=Potassium
$gdxin

Table DietGroupBounds(age,b)
         GrainsMin  GrainsMax  RootMax  VegetableMin  VegetableMax  FruitsMin  FruitsMax  DairyMax  BeefLambMax  PorkMax  PoultryMax  EggsMax  FishMax  LegumesMin  LegumesMax  NutsMin  NutsMax  PalmOilMax  UnsatOilsMin  UnsatOilsMax  LardMax  SugarMax
"0-4"         50.0      270.0    100.0         100.0         +inf      100.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        30.0       150.0     10.0    100.0         6.8           0.0           80.0      5.0      31.0
"5-9"         84.5      270.0    100.0         150.0         +inf      150.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        45.0       150.0     15.0    100.0         6.8           0.0           80.0      5.0      31.0
"10-14"      100.0      270.0    100.0         240.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8           0.0           80.0      5.0      31.0
"15-19"      118.0      270.0    100.0         257.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8           0.0           80.0      5.0      31.0
"20-24"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"25-29"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"30-34"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"35-39"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"40-44"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"45-49"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"50-54"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"55-59"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"60-64"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"65-69"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"70-74"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"75-79"      135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
"80+"        135.0      270.0    100.0         300.0         +inf      200.0       +inf     250.0         14.0     14.0        58.0     25.0     50.0        75.0       150.0     25.0    100.0         6.8          20.0           80.0      5.0      31.0
;

* DietGroupBounds applies only to people aged 20 years and older.
* Nutrient requirement equations remain active for every age group.
adultAge(age) = yes$(
       sameas(age,"20-24")
    or sameas(age,"25-29")
    or sameas(age,"30-34")
    or sameas(age,"35-39")
    or sameas(age,"40-44")
    or sameas(age,"45-49")
    or sameas(age,"50-54")
    or sameas(age,"55-59")
    or sameas(age,"60-64")
    or sameas(age,"65-69")
    or sameas(age,"70-74")
    or sameas(age,"75-79")
    or sameas(age,"80+")
);

GrainsMin(age)    = DietGroupBounds(age,"GrainsMin");
GrainsMax(age)    = DietGroupBounds(age,"GrainsMax");
RootMax(age)      = DietGroupBounds(age,"RootMax");
VegetableMin(age) = DietGroupBounds(age,"VegetableMin");
VegetableMax(age) = DietGroupBounds(age,"VegetableMax");
FruitsMin(age)    = DietGroupBounds(age,"FruitsMin");
FruitsMax(age)    = DietGroupBounds(age,"FruitsMax");
DairyMax(age)     = DietGroupBounds(age,"DairyMax");
BeefLambMax(age)  = DietGroupBounds(age,"BeefLambMax");
PorkMax(age)      = DietGroupBounds(age,"PorkMax");
PoultryMax(age)   = DietGroupBounds(age,"PoultryMax");
EggsMax(age)      = DietGroupBounds(age,"EggsMax");
FishMax(age)      = DietGroupBounds(age,"FishMax");
LegumesMin(age)   = DietGroupBounds(age,"LegumesMin");
LegumesMax(age)   = DietGroupBounds(age,"LegumesMax");
NutsMin(age)      = DietGroupBounds(age,"NutsMin");
NutsMax(age)      = DietGroupBounds(age,"NutsMax");
PalmOilMax(age)   = DietGroupBounds(age,"PalmOilMax");
UnsatOilsMin(age) = DietGroupBounds(age,"UnsatOilsMin");
UnsatOilsMax(age) = DietGroupBounds(age,"UnsatOilsMax");
LardMax(age)      = DietGroupBounds(age,"LardMax");
SugarMax(age)     = DietGroupBounds(age,"SugarMax");

* Sugar-cane intake is converted to sugar equivalent at a recovery rate of 0.14.
SugarConversion(j) = 1;
SugarConversion("sugar_cane") = 0.14;

Positive Variable
    DemandAgeSex(j,age,sex)
    dev_pos(k,j,age,sex)
    dev_neg(k,j,age,sex)
;

Variable
    Deviation
;

Equation
    dvar
    AbsBalance(k,j,age,sex)
    EnergyMinAgeSex(age,sex)
    ProteinMinAgeSex(age,sex)
    CalciumMinAgeSex(age,sex)
    IronMinAgeSex(age,sex)
    MagnesiumMinAgeSex(age,sex)
    CopperMinAgeSex(age,sex)
    PhosphorusMinAgeSex(age,sex)
    ZincMinAgeSex(age,sex)
    ThiaminMinAgeSex(age,sex)
    NiacinMinAgeSex(age,sex)
    RiboflavinMinAgeSex(age,sex)
    FolateMinAgeSex(age,sex)
    VitaminAMinAgeSex(age,sex)
    VitaminCMinAgeSex(age,sex)
    VitaminB6MinAgeSex(age,sex)
    PotassiumMinAgeSex(age,sex)
    grains_min(age,sex)
    grains_max(age,sex)
    root_max(age,sex)
    vegetable_min(age,sex)
    vegetable_max(age,sex)
    fruits_min(age,sex)
    fruits_max(age,sex)
    dairy_max(age,sex)
    beef_lamb_max(age,sex)
    pork_max(age,sex)
    poultry_max(age,sex)
    eggs_max(age,sex)
    fish_max(age,sex)
    legumes_min(age,sex)
    legumes_max(age,sex)
    nuts_min(age,sex)
    nuts_max(age,sex)
    palm_oil_max(age,sex)
    unsat_oils_min(age,sex)
    unsat_oils_max(age,sex)
    lard_max(age,sex)
    sugar_max(age,sex)
;

alpha("Cost")       = 1/7;
alpha("Structure")  = 1/7;
alpha("Land")       = 1/7;
alpha("GHG")        = 1/7;
alpha("Water")      = 1/7;
alpha("Acid")   = 1/7;
alpha("Eutr") = 1/7;

alpha_sum = sum(k, alpha(k));
abort$(abs(alpha_sum - 1) > 1e-9) "Scenario weights must sum to 1.", alpha_sum;

$gdxin "%OUTDIR%/base_Costmin_new.gdx"
$load x_cost=x_result
$gdxin

$gdxin "%OUTDIR%/base_Structuremin_new.gdx"
$load x_structure=x_result
$gdxin

$gdxin "%OUTDIR%/base_Landmin_new.gdx"
$load x_land=x_result
$gdxin

$gdxin "%OUTDIR%/base_GHGmin_new.gdx"
$load x_ghg=x_result
$gdxin

$gdxin "%OUTDIR%/base_Watermin_new.gdx"
$load x_water=x_result
$gdxin

$gdxin "%OUTDIR%/base_Acidmin_new.gdx"
$load x_Acid=x_result
$gdxin

$gdxin "%OUTDIR%/base_Eutrmin_new.gdx"
$load x_Eutr=x_result
$gdxin

x_result_all("Cost",i,j,age,sex)       = x_cost(i,j,age,sex);
x_result_all("Structure",i,j,age,sex)  = x_structure(i,j,age,sex);
x_result_all("Land",i,j,age,sex)       = x_land(i,j,age,sex);
x_result_all("GHG",i,j,age,sex)        = x_ghg(i,j,age,sex);
x_result_all("Water",i,j,age,sex)      = x_water(i,j,age,sex);
x_result_all("Acid",i,j,age,sex)       = x_Acid(i,j,age,sex);
x_result_all("Eutr",i,j,age,sex)       = x_Eutr(i,j,age,sex);

dvar..
    sum((k,j,age,sex),
        alpha(k) * (dev_pos(k,j,age,sex) + dev_neg(k,j,age,sex))) =e= Deviation;

AbsBalance(k,j,age,sex)..
    DemandAgeSex(j,age,sex) - x_point(k,j,age,sex)
    =e= dev_pos(k,j,age,sex) - dev_neg(k,j,age,sex);

EnergyMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * KcalAge(j)) =g= EnergyReq(age,sex);

ProteinMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * ProteinAge(j)) =g= ProteinReq(age,sex);

CalciumMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * CalciumAge(j)) =g= CalciumReq(age,sex);

IronMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * IronAge(j)) =g= IronReq(age,sex);

MagnesiumMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * MagnesiumAge(j)) =g= MagnesiumReq(age,sex);

CopperMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * CopperAge(j)) =g= CopperReq(age,sex);

PhosphorusMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * PhosphorusAge(j)) =g= PhosphorusReq(age,sex);

ZincMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * ZincAge(j)) =g= ZincReq(age,sex);

ThiaminMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * ThiaminAge(j)) =g= ThiaminReq(age,sex);

NiacinMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * NiacinAge(j)) =g= NiacinReq(age,sex);

RiboflavinMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * RiboflavinAge(j)) =g= RiboflavinReq(age,sex);

FolateMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * FolateAge(j)) =g= FolateReq(age,sex);

VitaminAMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * VitaminAAge(j)) =g= VitaminAReq(age,sex);

VitaminCMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * VitaminCAge(j)) =g= VitaminCReq(age,sex);

VitaminB6MinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * VitaminB6Age(j)) =g= VitaminB6Req(age,sex);

PotassiumMinAgeSex(age,sex)..
    sum(j, DemandAgeSex(j,age,sex) * PotassiumAge(j)) =g= PotassiumReq(age,sex);
    
grains_min(age,sex)$adultAge(age)..
    sum(j$x1(j), DemandAgeSex(j,age,sex)) =g= GrainsMin(age);
    
grains_max(age,sex)$adultAge(age)..
    sum(j$x1(j), DemandAgeSex(j,age,sex)) =l= GrainsMax(age);

root_max(age,sex)$adultAge(age)..
    sum(j$x2(j), DemandAgeSex(j,age,sex)) =l= RootMax(age);

vegetable_min(age,sex)$adultAge(age)..
    sum(j$x3(j), DemandAgeSex(j,age,sex)) =g= VegetableMin(age);

vegetable_max(age,sex)$adultAge(age)..
    sum(j$x3(j), DemandAgeSex(j,age,sex)) =l= VegetableMax(age);

fruits_min(age,sex)$adultAge(age)..
    sum(j$x4(j), DemandAgeSex(j,age,sex)) =g= FruitsMin(age);

fruits_max(age,sex)$adultAge(age)..
    sum(j$x4(j), DemandAgeSex(j,age,sex)) =l= FruitsMax(age);

dairy_max(age,sex)$adultAge(age)..
    sum(j$x5(j), DemandAgeSex(j,age,sex)) =l= DairyMax(age);

beef_lamb_max(age,sex)$adultAge(age)..
    sum(j$x6(j), DemandAgeSex(j,age,sex)) =l= BeefLambMax(age);

pork_max(age,sex)$adultAge(age)..
    sum(j$x7(j), DemandAgeSex(j,age,sex)) =l= PorkMax(age);

poultry_max(age,sex)$adultAge(age)..
    sum(j$x8(j), DemandAgeSex(j,age,sex)) =l= PoultryMax(age);

eggs_max(age,sex)$adultAge(age)..
    sum(j$x9(j), DemandAgeSex(j,age,sex)) =l= EggsMax(age);

fish_max(age,sex)$adultAge(age)..
    sum(j$x10(j), DemandAgeSex(j,age,sex)) =l= FishMax(age);

legumes_min(age,sex)$adultAge(age)..
    sum(j$x11(j), DemandAgeSex(j,age,sex)) =g= LegumesMin(age);

legumes_max(age,sex)$adultAge(age)..
    sum(j$x11(j), DemandAgeSex(j,age,sex)) =l= LegumesMax(age);

nuts_min(age,sex)$adultAge(age)..
    sum(j$x12(j), DemandAgeSex(j,age,sex)) =g= NutsMin(age);

nuts_max(age,sex)$adultAge(age)..
    sum(j$x12(j), DemandAgeSex(j,age,sex)) =l= NutsMax(age);

palm_oil_max(age,sex)$adultAge(age)..
    sum(j$x13(j), DemandAgeSex(j,age,sex)) =l= PalmOilMax(age);

unsat_oils_min(age,sex)$adultAge(age)..
    sum(j$x14(j), DemandAgeSex(j,age,sex)) =g= UnsatOilsMin(age);

unsat_oils_max(age,sex)$adultAge(age)..
    sum(j$x14(j), DemandAgeSex(j,age,sex)) =l= UnsatOilsMax(age);

lard_max(age,sex)$adultAge(age)..
    sum(j$x15(j), DemandAgeSex(j,age,sex)) =l= LardMax(age);

sugar_max(age,sex)$adultAge(age)..
    sum(j$x17(j), DemandAgeSex(j,age,sex) * SugarConversion(j)) =l= SugarMax(age);

Model feasibleWeightedModel /
    dvar
    AbsBalance
    EnergyMinAgeSex
    ProteinMinAgeSex
    CalciumMinAgeSex
    IronMinAgeSex
    MagnesiumMinAgeSex
    CopperMinAgeSex
    PhosphorusMinAgeSex
    ZincMinAgeSex
    ThiaminMinAgeSex
    NiacinMinAgeSex
    RiboflavinMinAgeSex
    FolateMinAgeSex
    VitaminAMinAgeSex
    VitaminCMinAgeSex
    VitaminB6MinAgeSex
    PotassiumMinAgeSex
    grains_min
    grains_max
    root_max
    vegetable_min
    vegetable_max
    fruits_min
    fruits_max
    dairy_max
    beef_lamb_max
    pork_max
    poultry_max
    eggs_max
    fish_max
    legumes_min
    legumes_max
    nuts_min
    nuts_max
    palm_oil_max
    unsat_oils_min
    unsat_oils_max
    lard_max
    sugar_max
/;

loop(i$(not excludedCountry(i)),
    KcalAge(j)       = Kcal_ori(i,j);
    ProteinAge(j)    = Protein_ori(i,j);
    CalciumAge(j)    = Calcium_ori(i,j);
    IronAge(j)       = Iron_ori(i,j);
    MagnesiumAge(j)  = Magnesium_ori(i,j);
    CopperAge(j)     = Copper_ori(i,j);
    PhosphorusAge(j) = Phosphorus_ori(i,j);
    ZincAge(j)       = Zinc_ori(i,j);
    ThiaminAge(j)    = Thiamin_ori(i,j);
    NiacinAge(j)     = Niacin_ori(i,j);
    RiboflavinAge(j) = Riboflavin_ori(i,j);
    FolateAge(j)     = Folate_ori(i,j);
    VitaminAAge(j)   = VitaminA_ori(i,j);
    VitaminCAge(j)   = VitaminC_ori(i,j);
    VitaminB6Age(j)  = VitaminB6_ori(i,j);
    PotassiumAge(j)  = Potassium_ori(i,j);

    x_point(k,j,age,sex) = x_result_all(k,i,j,age,sex);

    DemandAgeSex.lo(j,age,sex) = 0;
    DemandAgeSex.up(j,age,sex) = +inf;
    DemandAgeSex.l(j,age,sex) = sum(k, alpha(k) * x_point(k,j,age,sex));
    dev_pos.l(k,j,age,sex)
        = max(0, DemandAgeSex.l(j,age,sex) - x_point(k,j,age,sex));
    dev_neg.l(k,j,age,sex)
        = max(0, x_point(k,j,age,sex) - DemandAgeSex.l(j,age,sex));
    Deviation.l = sum((k,j,age,sex),
        alpha(k) * (dev_pos.l(k,j,age,sex) + dev_neg.l(k,j,age,sex)));

    solve feasibleWeightedModel minimizing Deviation using LP;
    combine_model_status(i) = feasibleWeightedModel.modelstat;
    combine_solve_status(i) = feasibleWeightedModel.solvestat;

    if(  combine_model_status(i) <> %ModelStat.Optimal%
      or combine_solve_status(i) <> %SolveStat.NormalCompletion%,
          execute_unload "%OUTDIR%/debug_weighted_new_17groups_sugar014.gdx",
              i, age, sex, j, k,
              alpha, x_point,
              DemandAgeSex, dev_pos, dev_neg, Deviation,
              combine_model_status, combine_solve_status;
          abort 'Stopped: country-age-sex weighted-combination LP solve failed.';
    );

    x_result_new(i,j,age,sex) = DemandAgeSex.l(j,age,sex);
    abs_deviation(k,i,j,age,sex)
        = dev_pos.l(k,j,age,sex) + dev_neg.l(k,j,age,sex);
    distance_result(k,i,age,sex)
        = sum(j, abs_deviation(k,i,j,age,sex));

    scenario_deviation_i(i,k) = sum((age,sex), distance_result(k,i,age,sex));
    weighted_scenario_deviation_i(i,k) = alpha(k) * scenario_deviation_i(i,k);
    objective_result_i(i) = sum(k, weighted_scenario_deviation_i(i,k));
);

scenario_deviation(k) = sum(i, scenario_deviation_i(i,k));
weighted_scenario_deviation(k) = alpha(k) * scenario_deviation(k);
objective_result = sum(i, objective_result_i(i));

put_utility 'gdxout' / '%OUT_GDX%';
execute_unload x_result_new, abs_deviation, distance_result,
    scenario_deviation_i, weighted_scenario_deviation_i,
    scenario_deviation, weighted_scenario_deviation,
    objective_result_i, objective_result,
    combine_model_status, combine_solve_status;
put_utility 'gdxout' / 'off';
