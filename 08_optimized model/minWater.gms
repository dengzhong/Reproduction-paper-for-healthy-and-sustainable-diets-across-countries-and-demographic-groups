$Title test for optimization; indexed-age version

* modified for sex dimension: use the current optmized workspace that contains the sex-indexed input files
$setglobal ROOT "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/08_optimized model"
$setglobal OUT_GDX "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/base_Watermin_new.gdx"

option Solver=SoPlex;
option Threads=8;

$include %ROOT%/Input.gms
$include %ROOT%/Parameter_indexed.gms
$include %ROOT%/Variables_indexed.gms
$include %ROOT%/Equations_indexed.gms
$include %ROOT%/Constraints_indexed.gms

Set
* modified for sex dimension: exclude countries that should not be solved in the strict batch run
    excludedCountry(i) / MUS /
;

Scalar
    totalPop
    activeCellCount
;

Parameter
* modified for sex dimension: structure counts are now tracked by age and sex
    activeFoodCount(age,sex)
;

dvar..
  Water_total =e= Deviation;

Model mymodel /all/;

loop(i$(not excludedCountry(i)),
*loop(i,
* modified for sex dimension: read the active country directly from the new four-dimensional raw data
    DemandBase(j,age,sex) = Demand_ori(i,j,age,sex);
    DemandLB(j,age,sex)   = DemandLB_ori(i,j,age,sex);
    DemandUB(j,age,sex)   = DemandUB_ori(i,j,age,sex);
    PopAge(age,sex)       = pop_ori(i,age,sex);

* modified for sex dimension: total population now sums over age and sex
    totalPop = sum((age,sex), PopAge(age,sex));

    wasteRatio(j) = wasteRatio_ori(i,j);
    price_food(j) = Price(i,j);

    Kcal(j)       = Kcal_ori(i,j);
    Protein(j)    = Protein_ori(i,j);
    Calcium(j)    = Calcium_ori(i,j);
    Iron(j)       = Iron_ori(i,j);
    Magnesium(j)  = Magnesium_ori(i,j);
    Copper(j)     = Copper_ori(i,j);
    Phosphorus(j) = Phosphorus_ori(i,j);
    Zinc(j)       = Zinc_ori(i,j);
    Thiamin(j)    = Thiamin_ori(i,j);
    Niacin(j)     = Niacin_ori(i,j);
    Riboflavin(j) = Riboflavin_ori(i,j);
    Folate(j)     = Folate_ori(i,j);
    VitaminA(j)   = VitaminA_ori(i,j);
    VitaminC(j)   = VitaminC_ori(i,j);
    VitaminB6(j)  = VitaminB6_ori(i,j);
    Potassium(j)  = Potassium_ori(i,j);

* modified for sex dimension: output calorie intake by age and sex
    intake_ori(i,age,sex) = sum(j, DemandBase(j,age,sex) * Kcal(j));

* modified for sex dimension: initialize and bound Demand(j,age,sex)
    Demand.l(j,age,sex)  = DemandBase(j,age,sex);
    Demand.lo(j,age,sex) = DemandLB(j,age,sex);
    Demand.up(j,age,sex) = DemandUB(j,age,sex);

* modified for sex dimension: validate bounds over food, age, and sex
    if(sum((j,age,sex)$(Demand.lo(j,age,sex) > Demand.up(j,age,sex)), 1),
        put_utility 'log' / '*** Stop at i=' i.tl:0 ': lower bound exceeds upper bound';
        abort 'Stopped: lower bound exceeds upper bound.';
    );

    solve mymodel minimizing Deviation using LP;
    model_status(i) = mymodel.modelstat;
    solve_status(i) = mymodel.solvestat;

    
* modified for sex dimension: use strict solve status checking; stop unless model and solver both finish normally
    if(  mymodel.modelstat <> %ModelStat.Optimal%
      or mymodel.solvestat <> %SolveStat.NormalCompletion%,
          put_utility 'log' / '*** Stop at i=' i.tl:0
                             ' modelstat=' mymodel.modelstat:0
                             ' solvestat=' mymodel.solvestat:0;
          abort 'Stopped: infeasible/unbounded/no solution OR iteration limit exceeded.';
    );
    
* modified for sex dimension: export only strictly successful optimal solutions
    if(  mymodel.modelstat = %ModelStat.Optimal%
     and mymodel.solvestat = %SolveStat.NormalCompletion%,
     
* modified for sex dimension: export result parameters in declared order (i,j,age,sex)
    x_result(i,j,age,sex)        = Demand.l(j,age,sex);
    Demand_adj(i,j,age,sex)      = DemandBase(j,age,sex);
    intake_result(i,age,sex)     = sum(j, Demand.l(j,age,sex) * Kcal(j));
    cost_result_age(i,age,sex)   = sum(j, Demand.l(j,age,sex) * price_food(j));
    cost_ori_age(i,age,sex)      = sum(j, DemandBase(j,age,sex) * price_food(j));

* modified for sex dimension: population-weighted totals now include sex
    cost_result(i) = sum((j,age,sex), Demand.l(j,age,sex)  * price_food(j) * PopAge(age,sex)) / totalPop;
    cost_ori(i)    = sum((j,age,sex), DemandBase(j,age,sex) * price_food(j) * PopAge(age,sex)) / totalPop;

* modified for sex dimension: structure calculations now include food, age, and sex
    activeCellCount = sum((j,age,sex)$(DemandBase(j,age,sex) > 0), 1);
    structure_all(i) = 0;
    if(activeCellCount > 0,
        structure_all(i) = sum((j,age,sex)$(DemandBase(j,age,sex) > 0),
            sqr((Demand.l(j,age,sex) - DemandBase(j,age,sex)) / DemandBase(j,age,sex))) / activeCellCount;
    );

* modified for sex dimension: age-level structure is now age-sex-level structure
    activeFoodCount(age,sex) = sum(j$(DemandBase(j,age,sex) > 0), 1);
    structure_age(i,age,sex) = 0;
    loop((age,sex),
        if(activeFoodCount(age,sex) > 0,
            structure_age(i,age,sex) = sum(j$(DemandBase(j,age,sex) > 0),
                abs(Demand.l(j,age,sex) - DemandBase(j,age,sex)) / DemandBase(j,age,sex)) / activeFoodCount(age,sex);
        );
    );


    cost_change(i) = (cost_result(i) - cost_ori(i)) / cost_ori(i) * 100;

* modified for sex dimension: use new environmental labels and population weights by age-sex
    GHG_percap_result(i) = sum((j,age,sex), Demand.l(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("GHG",j)) / totalPop;
    GHG_percap_ori(i)    = sum((j,age,sex), DemandBase(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("GHG",j)) / totalPop;

    Land_result(i)       = Land_total.l;
    GHG_result(i)        = GHG_total.l;
    Water_result(i)      = Water_total.l;
    Acid_result(i)       = Acid_total.l;
    Eutr_result(i)       = Eutr_total.l;

* modified for sex dimension: use new environmental labels and population weights by age-sex
    Land_ori(i)              = sum((j,age,sex), DemandBase(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("Land",j)) / 1000;
    GHG_ori(i)               = sum((j,age,sex), DemandBase(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("GHG",j));
    Water_ori(i)             = sum((j,age,sex), DemandBase(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("Freshwater",j));
    Acid_ori(i)              = sum((j,age,sex), DemandBase(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("Acid.",j));
    Eutr_ori_result(i)       = sum((j,age,sex), DemandBase(j,age,sex) / wasteRatio(j) * PopAge(age,sex) * Intensity_ori("Eutr.",j));

    dvar_result(i) = Deviation.l;
    );
);

put_utility 'gdxout' / '%OUT_GDX%';
execute_unload;
put_utility 'gdxout' / 'off';
