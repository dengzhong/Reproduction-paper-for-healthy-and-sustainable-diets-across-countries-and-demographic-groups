* =========================
* Nutrient minimum constraints
* =========================

EnergyMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Kcal(j))
    =g= EnergyReq(age,sex);

ProteinMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Protein(j))
    =g= ProteinReq(age,sex);

CalciumMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Calcium(j))
    =g= CalciumReq(age,sex);

IronMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Iron(j))
    =g= IronReq(age,sex);

MagnesiumMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Magnesium(j))
    =g= MagnesiumReq(age,sex);

CopperMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Copper(j))
    =g= CopperReq(age,sex);

PhosphorusMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Phosphorus(j))
    =g= PhosphorusReq(age,sex);

ZincMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Zinc(j))
    =g= ZincReq(age,sex);

ThiaminMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Thiamin(j))
    =g= ThiaminReq(age,sex);

NiacinMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Niacin(j))
    =g= NiacinReq(age,sex);

RiboflavinMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Riboflavin(j))
    =g= RiboflavinReq(age,sex);

FolateMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Folate(j))
    =g= FolateReq(age,sex);

VitaminAMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * VitaminA(j))
    =g= VitaminAReq(age,sex);

VitaminCMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * VitaminC(j))
    =g= VitaminCReq(age,sex);

VitaminB6Min(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * VitaminB6(j))
    =g= VitaminB6Req(age,sex);

PotassiumMin(age,sex)..
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
    sum(j, Demand(j,age,sex) * Potassium(j))
    =g= PotassiumReq(age,sex);


* =========================
* Environmental accounting constraints
* =========================

LandUB..
    sum((age,sex,j),
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
        Demand(j,age,sex)
        / wasteRatio(j)
        * PopAge(age,sex)
        * Intensity_ori("Land",j)
    ) / 1000
    =e= Land_total;

GHGUB..
    sum((age,sex,j),
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
        Demand(j,age,sex)
        / wasteRatio(j)
        * PopAge(age,sex)
        * Intensity_ori("GHG",j)
    )
    =e= GHG_total;

WaterUB..
    sum((age,sex,j),
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
        Demand(j,age,sex)
        / wasteRatio(j)
        * PopAge(age,sex)
        * Intensity_ori("Freshwater",j)
    )
    =e= Water_total;

AcidUB..
    sum((age,sex,j),
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
        Demand(j,age,sex)
        / wasteRatio(j)
        * PopAge(age,sex)
        * Intensity_ori("Acid.",j)
    )
    =e= Acid_total;

EutrUB..
    sum((age,sex,j),
* modified for sex dimension: Demand is declared as Demand(j,age,sex)
        Demand(j,age,sex)
        / wasteRatio(j)
        * PopAge(age,sex)
        * Intensity_ori("Eutr.",j)
    )
    =e= Eutr_total;


* =========================
* Total food weight constraints
* =========================

WeightMax..
* modified for sex dimension: enforce total food weight upper bound after summing all age-sex groups
    sum((j,age,sex), Demand(j,age,sex))
    =l= 1.5 * sum((j,age,sex), DemandBase(j,age,sex));

WeightMin..
* modified for sex dimension: enforce total food weight lower bound after summing all age-sex groups
    sum((j,age,sex), Demand(j,age,sex))
    =g= 0.7 * sum((j,age,sex), DemandBase(j,age,sex));
