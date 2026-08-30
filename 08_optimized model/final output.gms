$Title Export optimized and original age-sex results to XLSX


$setglobal RAW_GDX    "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/all_raw_data_newIntensity.gdx"
$setglobal RESULT_GDX "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00base_weighted_new111_17groups_sugar014.gdx"
$setglobal TEMP_GDX   "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00outputall_temp.gdx"
$setglobal COEF_XLSX  "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00Coefficient.xlsx"
$setglobal FML_XLSX   "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00Intake_FML.xlsx"
$setglobal MLE_XLSX   "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00Intake_MLE.xlsx"
$setglobal OTHER_XLSX "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/00other result.xlsx"

Set
    i       country
    j       food item
    age     age group
    sex     gender
    emi     environmental indicator
    n       nutrient
            /Energy, Protein, VitaminC, Calcium, Iron, Magnesium,
             Thiamin, Riboflavin, Folate, Zinc, Phosphorus, Copper,
             VitaminB6, VitaminA, Niacin, Potassium/
;

$gdxin "%RAW_GDX%"
$load i j age sex emi
$gdxin

Parameter
    Price(i,j)
    WasteFactor(i,j)
    pop_ori(i,age,sex)
    Demand_ori(i,j,age,sex)
    Kcal_ori(i,j)
    Protein_ori(i,j)
    VitaminC_ori(i,j)
    Calcium_ori(i,j)
    Iron_ori(i,j)
    Magnesium_ori(i,j)
    Thiamin_ori(i,j)
    Riboflavin_ori(i,j)
    Folate_ori(i,j)
    Zinc_ori(i,j)
    Phosphorus_ori(i,j)
    Copper_ori(i,j)
    VitaminB6_ori(i,j)
    VitaminA_ori(i,j)
    Niacin_ori(i,j)
    Potassium_ori(i,j)
    Intensity_ori(emi,j)
;

$gdxin "%RAW_GDX%"
$load Price
$load WasteFactor=waste
$load pop_ori=pop
$load Demand_ori
$load Kcal_ori=calories
$load Protein_ori=protein
$load VitaminC_ori=vitaminC
$load Calcium_ori=calcium
$load Iron_ori=iron
$load Magnesium_ori=magnesium
$load Thiamin_ori=thiamin
$load Riboflavin_ori=riboflavin
$load Folate_ori=folate
$load Zinc_ori=zinc
$load Phosphorus_ori=phosphorus
$load Copper_ori=copper
$load VitaminB6_ori=vitaminb6
$load VitaminA_ori=vitaminA
$load Niacin_ori=niacin
$load Potassium_ori=potassium
$load Intensity_ori=Intensity
$gdxin

Parameter
    x_result_new(i,j,age,sex)
;

$gdxin "%RESULT_GDX%"
$load x_result_new
$gdxin

Parameter
    Intake(i,j,age,sex)                    'Optimized edible intake'
    OriginalIntake(i,j,age,sex)            'Original edible intake'
    GrossIntake(i,j,age,sex)               'Optimized gross intake after waste adjustment'
    OriginalGrossIntake(i,j,age,sex)       'Original gross intake after waste adjustment'
    NewIntake_0_4_FML(i,j)
    OriIntake_0_4_FML(i,j)
    NewIntake_0_4_MLE(i,j)
    OriIntake_0_4_MLE(i,j)
    NewIntake_5_9_FML(i,j)
    OriIntake_5_9_FML(i,j)
    NewIntake_5_9_MLE(i,j)
    OriIntake_5_9_MLE(i,j)
    NewIntake_10_14_FML(i,j)
    OriIntake_10_14_FML(i,j)
    NewIntake_10_14_MLE(i,j)
    OriIntake_10_14_MLE(i,j)
    NewIntake_15_19_FML(i,j)
    OriIntake_15_19_FML(i,j)
    NewIntake_15_19_MLE(i,j)
    OriIntake_15_19_MLE(i,j)
    NewIntake_20_24_FML(i,j)
    OriIntake_20_24_FML(i,j)
    NewIntake_20_24_MLE(i,j)
    OriIntake_20_24_MLE(i,j)
    NewIntake_25_29_FML(i,j)
    OriIntake_25_29_FML(i,j)
    NewIntake_25_29_MLE(i,j)
    OriIntake_25_29_MLE(i,j)
    NewIntake_30_34_FML(i,j)
    OriIntake_30_34_FML(i,j)
    NewIntake_30_34_MLE(i,j)
    OriIntake_30_34_MLE(i,j)
    NewIntake_35_39_FML(i,j)
    OriIntake_35_39_FML(i,j)
    NewIntake_35_39_MLE(i,j)
    OriIntake_35_39_MLE(i,j)
    NewIntake_40_44_FML(i,j)
    OriIntake_40_44_FML(i,j)
    NewIntake_40_44_MLE(i,j)
    OriIntake_40_44_MLE(i,j)
    NewIntake_45_49_FML(i,j)
    OriIntake_45_49_FML(i,j)
    NewIntake_45_49_MLE(i,j)
    OriIntake_45_49_MLE(i,j)
    NewIntake_50_54_FML(i,j)
    OriIntake_50_54_FML(i,j)
    NewIntake_50_54_MLE(i,j)
    OriIntake_50_54_MLE(i,j)
    NewIntake_55_59_FML(i,j)
    OriIntake_55_59_FML(i,j)
    NewIntake_55_59_MLE(i,j)
    OriIntake_55_59_MLE(i,j)
    NewIntake_60_64_FML(i,j)
    OriIntake_60_64_FML(i,j)
    NewIntake_60_64_MLE(i,j)
    OriIntake_60_64_MLE(i,j)
    NewIntake_65_69_FML(i,j)
    OriIntake_65_69_FML(i,j)
    NewIntake_65_69_MLE(i,j)
    OriIntake_65_69_MLE(i,j)
    NewIntake_70_74_FML(i,j)
    OriIntake_70_74_FML(i,j)
    NewIntake_70_74_MLE(i,j)
    OriIntake_70_74_MLE(i,j)
    NewIntake_75_79_FML(i,j)
    OriIntake_75_79_FML(i,j)
    NewIntake_75_79_MLE(i,j)
    OriIntake_75_79_MLE(i,j)
    NewIntake_80plus_FML(i,j)
    OriIntake_80plus_FML(i,j)
    NewIntake_80plus_MLE(i,j)
    OriIntake_80plus_MLE(i,j)
    NutrientCoef(n,i,j)
    EnergyByFood(i,j,age,sex)
    OriginalEnergyByFood(i,j,age,sex)
    NutrientTotal(i,age,sex,n)
    OriginalNutrientTotal(i,age,sex,n)
    PriceIntake(i,j,age,sex)
    OriginalPriceIntake(i,j,age,sex)
    PriceTotal(i,age,sex)
    OriginalPriceTotal(i,age,sex)
    PriceTotal_AllAgeSex(i)
    OriginalPriceTotal_AllAgeSex(i)
    IntensityImpact(i,j,age,sex,emi)
    OriginalIntensityImpact(i,j,age,sex,emi)
    GHGImpact(i,j,age,sex)
    LandImpact(i,j,age,sex)
    FreshwaterImpact(i,j,age,sex)
    AcidImpact(i,j,age,sex)
    EutrImpact(i,j,age,sex)
    OriginalGHGImpact(i,j,age,sex)
    OriginalLandImpact(i,j,age,sex)
    OriginalFreshwaterImpact(i,j,age,sex)
    OriginalAcidImpact(i,j,age,sex)
    OriginalEutrImpact(i,j,age,sex)
    IntensityTotal(i,age,sex,emi)
    OriginalIntensityTotal(i,age,sex,emi)
;

Intake(i,j,age,sex) = x_result_new(i,j,age,sex);
OriginalIntake(i,j,age,sex) = Demand_ori(i,j,age,sex);

NewIntake_0_4_FML(i,j) = Intake(i,j,"0-4","FML");
OriIntake_0_4_FML(i,j) = OriginalIntake(i,j,"0-4","FML");
NewIntake_0_4_MLE(i,j) = Intake(i,j,"0-4","MLE");
OriIntake_0_4_MLE(i,j) = OriginalIntake(i,j,"0-4","MLE");
NewIntake_5_9_FML(i,j) = Intake(i,j,"5-9","FML");
OriIntake_5_9_FML(i,j) = OriginalIntake(i,j,"5-9","FML");
NewIntake_5_9_MLE(i,j) = Intake(i,j,"5-9","MLE");
OriIntake_5_9_MLE(i,j) = OriginalIntake(i,j,"5-9","MLE");
NewIntake_10_14_FML(i,j) = Intake(i,j,"10-14","FML");
OriIntake_10_14_FML(i,j) = OriginalIntake(i,j,"10-14","FML");
NewIntake_10_14_MLE(i,j) = Intake(i,j,"10-14","MLE");
OriIntake_10_14_MLE(i,j) = OriginalIntake(i,j,"10-14","MLE");
NewIntake_15_19_FML(i,j) = Intake(i,j,"15-19","FML");
OriIntake_15_19_FML(i,j) = OriginalIntake(i,j,"15-19","FML");
NewIntake_15_19_MLE(i,j) = Intake(i,j,"15-19","MLE");
OriIntake_15_19_MLE(i,j) = OriginalIntake(i,j,"15-19","MLE");
NewIntake_20_24_FML(i,j) = Intake(i,j,"20-24","FML");
OriIntake_20_24_FML(i,j) = OriginalIntake(i,j,"20-24","FML");
NewIntake_20_24_MLE(i,j) = Intake(i,j,"20-24","MLE");
OriIntake_20_24_MLE(i,j) = OriginalIntake(i,j,"20-24","MLE");
NewIntake_25_29_FML(i,j) = Intake(i,j,"25-29","FML");
OriIntake_25_29_FML(i,j) = OriginalIntake(i,j,"25-29","FML");
NewIntake_25_29_MLE(i,j) = Intake(i,j,"25-29","MLE");
OriIntake_25_29_MLE(i,j) = OriginalIntake(i,j,"25-29","MLE");
NewIntake_30_34_FML(i,j) = Intake(i,j,"30-34","FML");
OriIntake_30_34_FML(i,j) = OriginalIntake(i,j,"30-34","FML");
NewIntake_30_34_MLE(i,j) = Intake(i,j,"30-34","MLE");
OriIntake_30_34_MLE(i,j) = OriginalIntake(i,j,"30-34","MLE");
NewIntake_35_39_FML(i,j) = Intake(i,j,"35-39","FML");
OriIntake_35_39_FML(i,j) = OriginalIntake(i,j,"35-39","FML");
NewIntake_35_39_MLE(i,j) = Intake(i,j,"35-39","MLE");
OriIntake_35_39_MLE(i,j) = OriginalIntake(i,j,"35-39","MLE");
NewIntake_40_44_FML(i,j) = Intake(i,j,"40-44","FML");
OriIntake_40_44_FML(i,j) = OriginalIntake(i,j,"40-44","FML");
NewIntake_40_44_MLE(i,j) = Intake(i,j,"40-44","MLE");
OriIntake_40_44_MLE(i,j) = OriginalIntake(i,j,"40-44","MLE");
NewIntake_45_49_FML(i,j) = Intake(i,j,"45-49","FML");
OriIntake_45_49_FML(i,j) = OriginalIntake(i,j,"45-49","FML");
NewIntake_45_49_MLE(i,j) = Intake(i,j,"45-49","MLE");
OriIntake_45_49_MLE(i,j) = OriginalIntake(i,j,"45-49","MLE");
NewIntake_50_54_FML(i,j) = Intake(i,j,"50-54","FML");
OriIntake_50_54_FML(i,j) = OriginalIntake(i,j,"50-54","FML");
NewIntake_50_54_MLE(i,j) = Intake(i,j,"50-54","MLE");
OriIntake_50_54_MLE(i,j) = OriginalIntake(i,j,"50-54","MLE");
NewIntake_55_59_FML(i,j) = Intake(i,j,"55-59","FML");
OriIntake_55_59_FML(i,j) = OriginalIntake(i,j,"55-59","FML");
NewIntake_55_59_MLE(i,j) = Intake(i,j,"55-59","MLE");
OriIntake_55_59_MLE(i,j) = OriginalIntake(i,j,"55-59","MLE");
NewIntake_60_64_FML(i,j) = Intake(i,j,"60-64","FML");
OriIntake_60_64_FML(i,j) = OriginalIntake(i,j,"60-64","FML");
NewIntake_60_64_MLE(i,j) = Intake(i,j,"60-64","MLE");
OriIntake_60_64_MLE(i,j) = OriginalIntake(i,j,"60-64","MLE");
NewIntake_65_69_FML(i,j) = Intake(i,j,"65-69","FML");
OriIntake_65_69_FML(i,j) = OriginalIntake(i,j,"65-69","FML");
NewIntake_65_69_MLE(i,j) = Intake(i,j,"65-69","MLE");
OriIntake_65_69_MLE(i,j) = OriginalIntake(i,j,"65-69","MLE");
NewIntake_70_74_FML(i,j) = Intake(i,j,"70-74","FML");
OriIntake_70_74_FML(i,j) = OriginalIntake(i,j,"70-74","FML");
NewIntake_70_74_MLE(i,j) = Intake(i,j,"70-74","MLE");
OriIntake_70_74_MLE(i,j) = OriginalIntake(i,j,"70-74","MLE");
NewIntake_75_79_FML(i,j) = Intake(i,j,"75-79","FML");
OriIntake_75_79_FML(i,j) = OriginalIntake(i,j,"75-79","FML");
NewIntake_75_79_MLE(i,j) = Intake(i,j,"75-79","MLE");
OriIntake_75_79_MLE(i,j) = OriginalIntake(i,j,"75-79","MLE");
NewIntake_80plus_FML(i,j) = Intake(i,j,"80+","FML");
OriIntake_80plus_FML(i,j) = OriginalIntake(i,j,"80+","FML");
NewIntake_80plus_MLE(i,j) = Intake(i,j,"80+","MLE");
OriIntake_80plus_MLE(i,j) = OriginalIntake(i,j,"80+","MLE");

GrossIntake(i,j,age,sex) = Intake(i,j,age,sex) / WasteFactor(i,j);
OriginalGrossIntake(i,j,age,sex) = OriginalIntake(i,j,age,sex) / WasteFactor(i,j);

NutrientCoef("Energy",i,j)     = Kcal_ori(i,j);
NutrientCoef("Protein",i,j)    = Protein_ori(i,j);
NutrientCoef("VitaminC",i,j)   = VitaminC_ori(i,j);
NutrientCoef("Calcium",i,j)    = Calcium_ori(i,j);
NutrientCoef("Iron",i,j)       = Iron_ori(i,j);
NutrientCoef("Magnesium",i,j)  = Magnesium_ori(i,j);
NutrientCoef("Thiamin",i,j)    = Thiamin_ori(i,j);
NutrientCoef("Riboflavin",i,j) = Riboflavin_ori(i,j);
NutrientCoef("Folate",i,j)     = Folate_ori(i,j);
NutrientCoef("Zinc",i,j)       = Zinc_ori(i,j);
NutrientCoef("Phosphorus",i,j) = Phosphorus_ori(i,j);
NutrientCoef("Copper",i,j)     = Copper_ori(i,j);
NutrientCoef("VitaminB6",i,j)  = VitaminB6_ori(i,j);
NutrientCoef("VitaminA",i,j)   = VitaminA_ori(i,j);
NutrientCoef("Niacin",i,j)     = Niacin_ori(i,j);
NutrientCoef("Potassium",i,j)  = Potassium_ori(i,j);

EnergyByFood(i,j,age,sex) = Intake(i,j,age,sex) * NutrientCoef("Energy",i,j);
OriginalEnergyByFood(i,j,age,sex) = OriginalIntake(i,j,age,sex) * NutrientCoef("Energy",i,j);

NutrientTotal(i,age,sex,n) = sum(j, Intake(i,j,age,sex) * NutrientCoef(n,i,j));
OriginalNutrientTotal(i,age,sex,n) = sum(j, OriginalIntake(i,j,age,sex) * NutrientCoef(n,i,j));

PriceIntake(i,j,age,sex) = GrossIntake(i,j,age,sex) * Price(i,j);
OriginalPriceIntake(i,j,age,sex) = OriginalGrossIntake(i,j,age,sex) * Price(i,j);
PriceTotal(i,age,sex) = sum(j, PriceIntake(i,j,age,sex));
OriginalPriceTotal(i,age,sex) = sum(j, OriginalPriceIntake(i,j,age,sex));
PriceTotal_AllAgeSex(i) = sum((age,sex), PriceTotal(i,age,sex));
OriginalPriceTotal_AllAgeSex(i) = sum((age,sex), OriginalPriceTotal(i,age,sex));

IntensityImpact(i,j,age,sex,emi)
    = GrossIntake(i,j,age,sex) * Intensity_ori(emi,j);
OriginalIntensityImpact(i,j,age,sex,emi)
    = OriginalGrossIntake(i,j,age,sex) * Intensity_ori(emi,j);

GHGImpact(i,j,age,sex)        = IntensityImpact(i,j,age,sex,"GHG");
LandImpact(i,j,age,sex)       = IntensityImpact(i,j,age,sex,"Land");
FreshwaterImpact(i,j,age,sex) = IntensityImpact(i,j,age,sex,"Freshwater");
AcidImpact(i,j,age,sex)       = IntensityImpact(i,j,age,sex,"Acid.");
EutrImpact(i,j,age,sex)       = IntensityImpact(i,j,age,sex,"Eutr.");

OriginalGHGImpact(i,j,age,sex)        = OriginalIntensityImpact(i,j,age,sex,"GHG");
OriginalLandImpact(i,j,age,sex)       = OriginalIntensityImpact(i,j,age,sex,"Land");
OriginalFreshwaterImpact(i,j,age,sex) = OriginalIntensityImpact(i,j,age,sex,"Freshwater");
OriginalAcidImpact(i,j,age,sex)       = OriginalIntensityImpact(i,j,age,sex,"Acid.");
OriginalEutrImpact(i,j,age,sex)       = OriginalIntensityImpact(i,j,age,sex,"Eutr.");

IntensityTotal(i,age,sex,emi) = sum(j, IntensityImpact(i,j,age,sex,emi));
OriginalIntensityTotal(i,age,sex,emi) = sum(j, OriginalIntensityImpact(i,j,age,sex,emi));

execute_unload "%TEMP_GDX%",
    Intake, OriginalIntake, GrossIntake, OriginalGrossIntake,
    WasteFactor, Intensity_ori, Price, pop_ori,
    NewIntake_0_4_FML, OriIntake_0_4_FML,
    NewIntake_0_4_MLE, OriIntake_0_4_MLE,
    NewIntake_5_9_FML, OriIntake_5_9_FML,
    NewIntake_5_9_MLE, OriIntake_5_9_MLE,
    NewIntake_10_14_FML, OriIntake_10_14_FML,
    NewIntake_10_14_MLE, OriIntake_10_14_MLE,
    NewIntake_15_19_FML, OriIntake_15_19_FML,
    NewIntake_15_19_MLE, OriIntake_15_19_MLE,
    NewIntake_20_24_FML, OriIntake_20_24_FML,
    NewIntake_20_24_MLE, OriIntake_20_24_MLE,
    NewIntake_25_29_FML, OriIntake_25_29_FML,
    NewIntake_25_29_MLE, OriIntake_25_29_MLE,
    NewIntake_30_34_FML, OriIntake_30_34_FML,
    NewIntake_30_34_MLE, OriIntake_30_34_MLE,
    NewIntake_35_39_FML, OriIntake_35_39_FML,
    NewIntake_35_39_MLE, OriIntake_35_39_MLE,
    NewIntake_40_44_FML, OriIntake_40_44_FML,
    NewIntake_40_44_MLE, OriIntake_40_44_MLE,
    NewIntake_45_49_FML, OriIntake_45_49_FML,
    NewIntake_45_49_MLE, OriIntake_45_49_MLE,
    NewIntake_50_54_FML, OriIntake_50_54_FML,
    NewIntake_50_54_MLE, OriIntake_50_54_MLE,
    NewIntake_55_59_FML, OriIntake_55_59_FML,
    NewIntake_55_59_MLE, OriIntake_55_59_MLE,
    NewIntake_60_64_FML, OriIntake_60_64_FML,
    NewIntake_60_64_MLE, OriIntake_60_64_MLE,
    NewIntake_65_69_FML, OriIntake_65_69_FML,
    NewIntake_65_69_MLE, OriIntake_65_69_MLE,
    NewIntake_70_74_FML, OriIntake_70_74_FML,
    NewIntake_70_74_MLE, OriIntake_70_74_MLE,
    NewIntake_75_79_FML, OriIntake_75_79_FML,
    NewIntake_75_79_MLE, OriIntake_75_79_MLE,
    NewIntake_80plus_FML, OriIntake_80plus_FML,
    NewIntake_80plus_MLE, OriIntake_80plus_MLE,
    NutrientCoef,
    EnergyByFood, OriginalEnergyByFood,
    NutrientTotal, OriginalNutrientTotal,
    PriceIntake, OriginalPriceIntake,
    PriceTotal, OriginalPriceTotal,
    PriceTotal_AllAgeSex, OriginalPriceTotal_AllAgeSex,
    IntensityImpact, OriginalIntensityImpact,
    GHGImpact, LandImpact, FreshwaterImpact, AcidImpact, EutrImpact,
    OriginalGHGImpact, OriginalLandImpact, OriginalFreshwaterImpact,
    OriginalAcidImpact, OriginalEutrImpact,
    IntensityTotal, OriginalIntensityTotal;

execute 'rm -f "%COEF_XLSX%" "%FML_XLSX%" "%MLE_XLSX%" "%OTHER_XLSX%"';

embeddedCode Connect:
- GDXReader:
    file: "%TEMP_GDX%"
    symbols: all

- ExcelWriter:
    file: "%COEF_XLSX%"
    symbols:
      - {name: WasteFactor, range: Waste_Coefficient!A1, columnDimension: 1}
      - {name: Intensity_ori, range: Intensity_Coefficient!A1, columnDimension: 1}
      - {name: Price, range: Price!A1, columnDimension: 1}
      - {name: pop_ori, range: population!A1, columnDimension: 0}

- PythonCode:
    code: |
      from openpyxl import load_workbook
      workbook = load_workbook(r"%COEF_XLSX%")
      population_sheet = workbook["population"]
      population_sheet.insert_rows(1)
      for column, heading in enumerate(["ISO3", "Age", "sex", "value"], start=1):
          population_sheet.cell(row=1, column=column, value=heading)
      workbook.save(r"%COEF_XLSX%")

- ExcelWriter:
    file: "%FML_XLSX%"
    symbols:
      - {name: NewIntake_0_4_FML, range: "New_0-4!A1", columnDimension: 1}
      - {name: NewIntake_5_9_FML, range: "New_5-9!A1", columnDimension: 1}
      - {name: NewIntake_10_14_FML, range: "New_10-14!A1", columnDimension: 1}
      - {name: NewIntake_15_19_FML, range: "New_15-19!A1", columnDimension: 1}
      - {name: NewIntake_20_24_FML, range: "New_20-24!A1", columnDimension: 1}
      - {name: NewIntake_25_29_FML, range: "New_25-29!A1", columnDimension: 1}
      - {name: NewIntake_30_34_FML, range: "New_30-34!A1", columnDimension: 1}
      - {name: NewIntake_35_39_FML, range: "New_35-39!A1", columnDimension: 1}
      - {name: NewIntake_40_44_FML, range: "New_40-44!A1", columnDimension: 1}
      - {name: NewIntake_45_49_FML, range: "New_45-49!A1", columnDimension: 1}
      - {name: NewIntake_50_54_FML, range: "New_50-54!A1", columnDimension: 1}
      - {name: NewIntake_55_59_FML, range: "New_55-59!A1", columnDimension: 1}
      - {name: NewIntake_60_64_FML, range: "New_60-64!A1", columnDimension: 1}
      - {name: NewIntake_65_69_FML, range: "New_65-69!A1", columnDimension: 1}
      - {name: NewIntake_70_74_FML, range: "New_70-74!A1", columnDimension: 1}
      - {name: NewIntake_75_79_FML, range: "New_75-79!A1", columnDimension: 1}
      - {name: NewIntake_80plus_FML, range: "New_80+!A1", columnDimension: 1}
      - {name: OriIntake_0_4_FML, range: "Ori_0-4!A1", columnDimension: 1}
      - {name: OriIntake_5_9_FML, range: "Ori_5-9!A1", columnDimension: 1}
      - {name: OriIntake_10_14_FML, range: "Ori_10-14!A1", columnDimension: 1}
      - {name: OriIntake_15_19_FML, range: "Ori_15-19!A1", columnDimension: 1}
      - {name: OriIntake_20_24_FML, range: "Ori_20-24!A1", columnDimension: 1}
      - {name: OriIntake_25_29_FML, range: "Ori_25-29!A1", columnDimension: 1}
      - {name: OriIntake_30_34_FML, range: "Ori_30-34!A1", columnDimension: 1}
      - {name: OriIntake_35_39_FML, range: "Ori_35-39!A1", columnDimension: 1}
      - {name: OriIntake_40_44_FML, range: "Ori_40-44!A1", columnDimension: 1}
      - {name: OriIntake_45_49_FML, range: "Ori_45-49!A1", columnDimension: 1}
      - {name: OriIntake_50_54_FML, range: "Ori_50-54!A1", columnDimension: 1}
      - {name: OriIntake_55_59_FML, range: "Ori_55-59!A1", columnDimension: 1}
      - {name: OriIntake_60_64_FML, range: "Ori_60-64!A1", columnDimension: 1}
      - {name: OriIntake_65_69_FML, range: "Ori_65-69!A1", columnDimension: 1}
      - {name: OriIntake_70_74_FML, range: "Ori_70-74!A1", columnDimension: 1}
      - {name: OriIntake_75_79_FML, range: "Ori_75-79!A1", columnDimension: 1}
      - {name: OriIntake_80plus_FML, range: "Ori_80+!A1", columnDimension: 1}

- ExcelWriter:
    file: "%MLE_XLSX%"
    symbols:
      - {name: NewIntake_0_4_MLE, range: "New_0-4!A1", columnDimension: 1}
      - {name: NewIntake_5_9_MLE, range: "New_5-9!A1", columnDimension: 1}
      - {name: NewIntake_10_14_MLE, range: "New_10-14!A1", columnDimension: 1}
      - {name: NewIntake_15_19_MLE, range: "New_15-19!A1", columnDimension: 1}
      - {name: NewIntake_20_24_MLE, range: "New_20-24!A1", columnDimension: 1}
      - {name: NewIntake_25_29_MLE, range: "New_25-29!A1", columnDimension: 1}
      - {name: NewIntake_30_34_MLE, range: "New_30-34!A1", columnDimension: 1}
      - {name: NewIntake_35_39_MLE, range: "New_35-39!A1", columnDimension: 1}
      - {name: NewIntake_40_44_MLE, range: "New_40-44!A1", columnDimension: 1}
      - {name: NewIntake_45_49_MLE, range: "New_45-49!A1", columnDimension: 1}
      - {name: NewIntake_50_54_MLE, range: "New_50-54!A1", columnDimension: 1}
      - {name: NewIntake_55_59_MLE, range: "New_55-59!A1", columnDimension: 1}
      - {name: NewIntake_60_64_MLE, range: "New_60-64!A1", columnDimension: 1}
      - {name: NewIntake_65_69_MLE, range: "New_65-69!A1", columnDimension: 1}
      - {name: NewIntake_70_74_MLE, range: "New_70-74!A1", columnDimension: 1}
      - {name: NewIntake_75_79_MLE, range: "New_75-79!A1", columnDimension: 1}
      - {name: NewIntake_80plus_MLE, range: "New_80+!A1", columnDimension: 1}
      - {name: OriIntake_0_4_MLE, range: "Ori_0-4!A1", columnDimension: 1}
      - {name: OriIntake_5_9_MLE, range: "Ori_5-9!A1", columnDimension: 1}
      - {name: OriIntake_10_14_MLE, range: "Ori_10-14!A1", columnDimension: 1}
      - {name: OriIntake_15_19_MLE, range: "Ori_15-19!A1", columnDimension: 1}
      - {name: OriIntake_20_24_MLE, range: "Ori_20-24!A1", columnDimension: 1}
      - {name: OriIntake_25_29_MLE, range: "Ori_25-29!A1", columnDimension: 1}
      - {name: OriIntake_30_34_MLE, range: "Ori_30-34!A1", columnDimension: 1}
      - {name: OriIntake_35_39_MLE, range: "Ori_35-39!A1", columnDimension: 1}
      - {name: OriIntake_40_44_MLE, range: "Ori_40-44!A1", columnDimension: 1}
      - {name: OriIntake_45_49_MLE, range: "Ori_45-49!A1", columnDimension: 1}
      - {name: OriIntake_50_54_MLE, range: "Ori_50-54!A1", columnDimension: 1}
      - {name: OriIntake_55_59_MLE, range: "Ori_55-59!A1", columnDimension: 1}
      - {name: OriIntake_60_64_MLE, range: "Ori_60-64!A1", columnDimension: 1}
      - {name: OriIntake_65_69_MLE, range: "Ori_65-69!A1", columnDimension: 1}
      - {name: OriIntake_70_74_MLE, range: "Ori_70-74!A1", columnDimension: 1}
      - {name: OriIntake_75_79_MLE, range: "Ori_75-79!A1", columnDimension: 1}
      - {name: OriIntake_80plus_MLE, range: "Ori_80+!A1", columnDimension: 1}

- ExcelWriter:
    file: "%OTHER_XLSX%"
    symbols:
      - {name: GrossIntake, range: New_GrossIntake!A1, columnDimension: 0}
      - {name: OriginalGrossIntake, range: Ori_GrossIntake!A1, columnDimension: 0}
      - {name: NutrientCoef, range: Nutrient_Coefficients!A1, columnDimension: 1}
      - {name: EnergyByFood, range: New_Energy_ByFood!A1, columnDimension: 0}
      - {name: OriginalEnergyByFood, range: Ori_Energy_ByFood!A1, columnDimension: 0}
      - {name: NutrientTotal, range: New_Nutrient_Total!A1, columnDimension: 0}
      - {name: OriginalNutrientTotal, range: Ori_Nutrient_Total!A1, columnDimension: 0}
      - {name: PriceIntake, range: New_Price_ByFood!A1, columnDimension: 0}
      - {name: OriginalPriceIntake, range: Ori_Price_ByFood!A1, columnDimension: 0}
      - {name: PriceTotal, range: New_Price_AgeSex!A1, columnDimension: 0}
      - {name: OriginalPriceTotal, range: Ori_Price_AgeSex!A1, columnDimension: 0}
      - {name: PriceTotal_AllAgeSex, range: New_Price_Country!A1, columnDimension: 0}
      - {name: OriginalPriceTotal_AllAgeSex, range: Ori_Price_Country!A1, columnDimension: 0}
      - {name: GHGImpact, range: New_GHG_ByFood!A1, columnDimension: 0}
      - {name: LandImpact, range: New_Land_ByFood!A1, columnDimension: 0}
      - {name: FreshwaterImpact, range: New_Water_ByFood!A1, columnDimension: 0}
      - {name: AcidImpact, range: New_Acid_ByFood!A1, columnDimension: 0}
      - {name: EutrImpact, range: New_Eutr_ByFood!A1, columnDimension: 0}
      - {name: OriginalGHGImpact, range: Ori_GHG_ByFood!A1, columnDimension: 0}
      - {name: OriginalLandImpact, range: Ori_Land_ByFood!A1, columnDimension: 0}
      - {name: OriginalFreshwaterImpact, range: Ori_Water_ByFood!A1, columnDimension: 0}
      - {name: OriginalAcidImpact, range: Ori_Acid_ByFood!A1, columnDimension: 0}
      - {name: OriginalEutrImpact, range: Ori_Eutr_ByFood!A1, columnDimension: 0}
      - {name: IntensityTotal, range: New_Impact_AgeSex!A1, columnDimension: 0}
      - {name: OriginalIntensityTotal, range: Ori_Impact_AgeSex!A1, columnDimension: 0}
endEmbeddedCode
