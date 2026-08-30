$Title data input for optimization; updated to read all data from all_raw_data.gdx

$setglobal RAW_GDX "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/all_raw_data_newIntensity.gdx"

Set
        i           country
        j           Food item
        emi         environment
        age         age group
        sex         gender
;

$gdxin %RAW_GDX%
$load i j emi age sex
$gdxin

Set
        x13(j)    Sugar            /sugar_cane, sugar_non, raw_sugar, sweeteners, honey/
;

Parameter
        pop_ori(i,age,sex)
        wasteRatio_ori(i,j)
        Price(i,j)

        Demand_ori(i,j,age,sex)
* modified for sex dimension: keep raw GDX bounds in separate symbols so they do not conflict with active model bounds
        DemandLB_ori(i,j,age,sex)
        DemandUB_ori(i,j,age,sex)

        Kcal_ori(i,j)
        Protein_ori(i,j)
        Potassium_ori(i,j)
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

        Intensity_ori(emi,j)
;

$gdxin %RAW_GDX%

* Parameters whose names are unchanged in all_raw_data.gdx
$load Price
$load pop_ori=pop
$load wasteRatio_ori=waste

$load Demand_ori
* modified for sex dimension: load raw four-dimensional bounds into *_ori symbols
$load DemandLB_ori=DemandLB
$load DemandUB_ori=DemandUB

$load Kcal_ori=calories
$load Protein_ori=protein
$load Potassium_ori=potassium
$load Calcium_ori=calcium
$load Iron_ori=iron
$load Magnesium_ori=magnesium
$load Copper_ori=copper
$load Phosphorus_ori=phosphorus
$load Zinc_ori=zinc
$load Thiamin_ori=thiamin
$load Niacin_ori=niacin
$load Riboflavin_ori=riboflavin
$load Folate_ori=folate
$load VitaminA_ori=vitaminA
$load VitaminC_ori=vitaminC
$load VitaminB6_ori=vitaminb6

$load Intensity_ori=Intensity

$gdxin

* Optional checks
* display Demand_ori, DemandLB, DemandUB, pop_ori;
