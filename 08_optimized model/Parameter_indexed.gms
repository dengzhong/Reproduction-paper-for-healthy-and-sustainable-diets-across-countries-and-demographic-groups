Parameter
    DemandBase(j,age,sex)        'Baseline food demand by age and sex group'
    DemandLB(j,age,sex)          'Lower bound of food demand by age and sex group'
    DemandUB(j,age,sex)          'Upper bound of food demand by age group'
    PopAge(age,sex)              'Population by age group'
    price_food(j)          'Food prices in the active country'
    wasteRatio(j)          'Food waste ratio in the active country'
    Kcal(j)                'Energy content'
    Protein(j)             'Protein content'
    Calcium(j)             'Calcium content'
    Iron(j)                'Iron content'
    Magnesium(j)           'Magnesium content'
    Copper(j)              'Copper content'
    Phosphorus(j)          'Phosphorus content'
    Zinc(j)                'Zinc content'
    Thiamin(j)             'Thiamin content'
    Niacin(j)              'Niacin content'
    Riboflavin(j)          'Riboflavin content'
    Folate(j)              'Folate content'
    VitaminA(j)            'Vitamin A content'
    VitaminC(j)            'Vitamin C content'
    VitaminB6(j)           'Vitamin B6 content'
    Potassium(j)           'Potassium content'
    intake_ori(i,age,sex)        'Baseline calorie intake by age group'
    Demand_adj(i,j,age,sex)      'Baseline demand exported by age group'
    x_result(i,j,age,sex)        'Optimized demand exported by age group'
    intake_result(i,age,sex)     'Optimized calorie intake by age group'
    cost_result_age(i,age,sex)   'Optimized cost by age group'
    cost_ori_age(i,age,sex)      'Baseline cost by age group'
    structure_age(i,age,sex)     'Diet structure change by age group'
    cost_result(i)
    cost_ori(i)
    cost_change(i)
    structure_all(i)
    GHG_percap_result(i)
    GHG_percap_ori(i)
    Land_result(i)
    GHG_result(i)
    Water_result(i)
    Acid_result(i)
    Eutr_result(i)
    Land_ori(i)
    GHG_ori(i)
    Water_ori(i)
    Acid_ori(i)
    Eutr_ori_result(i)
    dvar_result(i)
    model_status(i)
    solve_status(i)
;


Parameter
    EnergyReq(age,sex) /
        "0-4".FML 1055, "0-4".MLE 1105,
        "5-9".FML 1520, "5-9".MLE 1600,
        "10-14".FML 1920, "10-14".MLE 2120,
        "15-19".FML 2040, "15-19".MLE 2760,
        "20-24".FML 2200, "20-24".MLE 2800,
        "25-29".FML 2040, "25-29".MLE 2640,
        "30-34".FML 2000, "30-34".MLE 2600,
        "35-39".FML 2000, "35-39".MLE 2600,
        "40-44".FML 2000, "40-44".MLE 2600,
        "45-49".FML 2000, "45-49".MLE 2440,
        "50-54".FML 1840, "50-54".MLE 2400,
        "55-59".FML 1800, "55-59".MLE 2400,
        "60-64".FML 1800, "60-64".MLE 2400,
        "65-69".FML 1800, "65-69".MLE 2240,
        "70-74".FML 1800, "70-74".MLE 2200,
        "75-79".FML 1800, "75-79".MLE 2200,
        "80+".FML 1800, "80+".MLE 2200 /

    ProteinReq(age,sex) /
        "0-4".FML 14, "0-4".MLE 14,
        "5-9".FML 22, "5-9".MLE 22,
        "10-14".FML 43.5, "10-14".MLE 43.5,
        "15-19".FML 48, "15-19".MLE 62,
        "20-24".FML 48, "20-24".MLE 57,
        "25-29".FML 48, "25-29".MLE 57,
        "30-34".FML 48, "30-34".MLE 57,
        "35-39".FML 48, "35-39".MLE 57,
        "40-44".FML 48, "40-44".MLE 57,
        "45-49".FML 48, "45-49".MLE 57,
        "50-54".FML 47, "50-54".MLE 55,
        "55-59".FML 47, "55-59".MLE 55,
        "60-64".FML 47, "60-64".MLE 55,
        "65-69".FML 47, "65-69".MLE 55,
        "70-74".FML 47, "70-74".MLE 55,
        "75-79".FML 47, "75-79".MLE 55,
        "80+".FML 47, "80+".MLE 55 /

    CalciumReq(age,sex) /
        "0-4".FML 600, "0-4".MLE 600,
        "5-9".FML 825, "5-9".MLE 825,
        "10-14".FML 1150, "10-14".MLE 1150,
        "15-19".FML 1200, "15-19".MLE 1200,
        "20-24".FML 1000, "20-24".MLE 1000,
        "25-29".FML 1000, "25-29".MLE 1000,
        "30-34".FML 1000, "30-34".MLE 1000,
        "35-39".FML 1000, "35-39".MLE 1000,
        "40-44".FML 1000, "40-44".MLE 1000,
        "45-49".FML 1000, "45-49".MLE 1000,
        "50-54".FML 1000, "50-54".MLE 1000,
        "55-59".FML 1000, "55-59".MLE 1000,
        "60-64".FML 1000, "60-64".MLE 1000,
        "65-69".FML 1000, "65-69".MLE 1000,
        "70-74".FML 1000, "70-74".MLE 1000,
        "75-79".FML 1000, "75-79".MLE 1000,
        "80+".FML 1000, "80+".MLE 1000 /

    IronReq(age,sex) /
        "0-4".FML 6.2, "0-4".MLE 6.2,
        "5-9".FML 9.6, "5-9".MLE 9.6,
        "10-14".FML 9.4, "10-14".MLE 8.6,
        "15-19".FML 15.6, "15-19".MLE 10.4,
        "20-24".FML 18, "20-24".MLE 8,
        "25-29".FML 18, "25-29".MLE 8,
        "30-34".FML 18, "30-34".MLE 8,
        "35-39".FML 18, "35-39".MLE 8,
        "40-44".FML 18, "40-44".MLE 8,
        "45-49".FML 18, "45-49".MLE 8,
        "50-54".FML 10, "50-54".MLE 8,
        "55-59".FML 8, "55-59".MLE 8,
        "60-64".FML 8, "60-64".MLE 8,
        "65-69".FML 8, "65-69".MLE 8,
        "70-74".FML 8, "70-74".MLE 8,
        "75-79".FML 8, "75-79".MLE 8,
        "80+".FML 8, "80+".MLE 8 /

    MagnesiumReq(age,sex) /
        "0-4".FML 89, "0-4".MLE 89,
        "5-9".FML 152, "5-9".MLE 152,
        "10-14".FML 264, "10-14".MLE 274,
        "15-19".FML 350, "15-19".MLE 408,
        "20-24".FML 310, "20-24".MLE 400,
        "25-29".FML 310, "25-29".MLE 400,
        "30-34".FML 318, "30-34".MLE 416,
        "35-39".FML 320, "35-39".MLE 420,
        "40-44".FML 320, "40-44".MLE 420,
        "45-49".FML 320, "45-49".MLE 420,
        "50-54".FML 320, "50-54".MLE 420,
        "55-59".FML 320, "55-59".MLE 420,
        "60-64".FML 320, "60-64".MLE 420,
        "65-69".FML 320, "65-69".MLE 420,
        "70-74".FML 320, "70-74".MLE 420,
        "75-79".FML 320, "75-79".MLE 420,
        "80+".FML 320, "80+".MLE 420 /

    CopperReq(age,sex) /
        "0-4".FML 0.334, "0-4".MLE 0.334,
        "5-9".FML 0.492, "5-9".MLE 0.492,
        "10-14".FML 0.738, "10-14".MLE 0.738,
        "15-19".FML 0.892, "15-19".MLE 0.892,
        "20-24".FML 0.9, "20-24".MLE 0.9,
        "25-29".FML 0.9, "25-29".MLE 0.9,
        "30-34".FML 0.9, "30-34".MLE 0.9,
        "35-39".FML 0.9, "35-39".MLE 0.9,
        "40-44".FML 0.9, "40-44".MLE 0.9,
        "45-49".FML 0.9, "45-49".MLE 0.9,
        "50-54".FML 0.9, "50-54".MLE 0.9,
        "55-59".FML 0.9, "55-59".MLE 0.9,
        "60-64".FML 0.9, "60-64".MLE 0.9,
        "65-69".FML 0.9, "65-69".MLE 0.9,
        "70-74".FML 0.9, "70-74".MLE 0.9,
        "75-79".FML 0.9, "75-79".MLE 0.9,
        "80+".FML 0.9, "80+".MLE 0.9 /

    PhosphorusReq(age,sex) /
        "0-4".FML 500, "0-4".MLE 500,
        "5-9".FML 700, "5-9".MLE 700,
        "10-14".FML 1250, "10-14".MLE 1250,
        "15-19".FML 1250, "15-19".MLE 1250,
        "20-24".FML 700, "20-24".MLE 700,
        "25-29".FML 700, "25-29".MLE 700,
        "30-34".FML 700, "30-34".MLE 700,
        "35-39".FML 700, "35-39".MLE 700,
        "40-44".FML 700, "40-44".MLE 700,
        "45-49".FML 700, "45-49".MLE 700,
        "50-54".FML 700, "50-54".MLE 700,
        "55-59".FML 700, "55-59".MLE 700,
        "60-64".FML 700, "60-64".MLE 700,
        "65-69".FML 700, "65-69".MLE 700,
        "70-74".FML 700, "70-74".MLE 700,
        "75-79".FML 700, "75-79".MLE 700,
        "80+".FML 700, "80+".MLE 700 /

    ZincReq(age,sex) /
        "0-4".FML 3, "0-4".MLE 3,
        "5-9".FML 6, "5-9".MLE 6,
        "10-14".FML 7, "10-14".MLE 9.25,
        "15-19".FML 7, "15-19".MLE 10,
        "20-24".FML 7, "20-24".MLE 10,
        "25-29".FML 7, "25-29".MLE 10,
        "30-34".FML 7, "30-34".MLE 10,
        "35-39".FML 7, "35-39".MLE 10,
        "40-44".FML 7, "40-44".MLE 10,
        "45-49".FML 7, "45-49".MLE 10,
        "50-54".FML 7, "50-54".MLE 10,
        "55-59".FML 7, "55-59".MLE 10,
        "60-64".FML 7, "60-64".MLE 10,
        "65-69".FML 7, "65-69".MLE 10,
        "70-74".FML 7, "70-74".MLE 10,
        "75-79".FML 7, "75-79".MLE 10,
        "80+".FML 7, "80+".MLE 10 /

    ThiaminReq(age,sex) /
        "0-4".FML 0.6, "0-4".MLE 0.6,
        "5-9".FML 0.75, "5-9".MLE 0.8,
        "10-14".FML 0.95, "10-14".MLE 1.1,
        "15-19".FML 1.1, "15-19".MLE 1.4,
        "20-24".FML 1, "20-24".MLE 1.3,
        "25-29".FML 1, "25-29".MLE 1.2,
        "30-34".FML 1, "30-34".MLE 1.2,
        "35-39".FML 1, "35-39".MLE 1.2,
        "40-44".FML 1, "40-44".MLE 1.2,
        "45-49".FML 1, "45-49".MLE 1.2,
        "50-54".FML 1, "50-54".MLE 1.2,
        "55-59".FML 1, "55-59".MLE 1.2,
        "60-64".FML 1, "60-64".MLE 1.2,
        "65-69".FML 1, "65-69".MLE 1.2,
        "70-74".FML 1, "70-74".MLE 1.2,
        "75-79".FML 1, "75-79".MLE 1.2,
        "80+".FML 1, "80+".MLE 1.2 /

    NiacinReq(age,sex) /
        "0-4".FML 8, "0-4".MLE 8,
        "5-9".FML 10, "5-9".MLE 9.5,
        "10-14".FML 14, "10-14".MLE 12,
        "15-19".FML 17, "15-19".MLE 13,
        "20-24".FML 16, "20-24".MLE 13,
        "25-29".FML 15, "25-29".MLE 12,
        "30-34".FML 15, "30-34".MLE 12,
        "35-39".FML 15, "35-39".MLE 12,
        "40-44".FML 15, "40-44".MLE 12,
        "45-49".FML 15, "45-49".MLE 12,
        "50-54".FML 15, "50-54".MLE 11,
        "55-59".FML 15, "55-59".MLE 11,
        "60-64".FML 15, "60-64".MLE 11,
        "65-69".FML 14, "65-69".MLE 11,
        "70-74".FML 14, "70-74".MLE 11,
        "75-79".FML 14, "75-79".MLE 11,
        "80+".FML 14, "80+".MLE 11 /

    RiboflavinReq(age,sex) /
        "0-4".FML 0.7, "0-4".MLE 0.7,
        "5-9".FML 0.85, "5-9".MLE 0.9,
        "10-14".FML 1.05, "10-14".MLE 1.25,
        "15-19".FML 1.2, "15-19".MLE 1.6,
        "20-24".FML 1.1, "20-24".MLE 1.4,
        "25-29".FML 1.1, "25-29".MLE 1.4,
        "30-34".FML 1.1, "30-34".MLE 1.4,
        "35-39".FML 1.1, "35-39".MLE 1.4,
        "40-44".FML 1.1, "40-44".MLE 1.4,
        "45-49".FML 1.1, "45-49".MLE 1.4,
        "50-54".FML 1, "50-54".MLE 1.3,
        "55-59".FML 1, "55-59".MLE 1.3,
        "60-64".FML 1, "60-64".MLE 1.3,
        "65-69".FML 1, "65-69".MLE 1.3,
        "70-74".FML 1, "70-74".MLE 1.3,
        "75-79".FML 1, "75-79".MLE 1.3,
        "80+".FML 1, "80+".MLE 1.3 /

    FolateReq(age,sex) /
        "0-4".FML 120, "0-4".MLE 120,
        "5-9".FML 160, "5-9".MLE 160,
        "10-14".FML 270, "10-14".MLE 270,
        "15-19".FML 300, "15-19".MLE 300,
        "20-24".FML 300, "20-24".MLE 300,
        "25-29".FML 300, "25-29".MLE 300,
        "30-34".FML 300, "30-34".MLE 300,
        "35-39".FML 300, "35-39".MLE 300,
        "40-44".FML 300, "40-44".MLE 300,
        "45-49".FML 300, "45-49".MLE 300,
        "50-54".FML 300, "50-54".MLE 300,
        "55-59".FML 300, "55-59".MLE 300,
        "60-64".FML 300, "60-64".MLE 300,
        "65-69".FML 300, "65-69".MLE 300,
        "70-74".FML 300, "70-74".MLE 300,
        "75-79".FML 300, "75-79".MLE 300,
        "80+".FML 300, "80+".MLE 300 /

    VitaminAReq(age,sex) /
        "0-4".FML 260, "0-4".MLE 260,
        "5-9".FML 440, "5-9".MLE 440,
        "10-14".FML 620, "10-14".MLE 660,
        "15-19".FML 700, "15-19".MLE 900,
        "20-24".FML 700, "20-24".MLE 900,
        "25-29".FML 700, "25-29".MLE 900,
        "30-34".FML 700, "30-34".MLE 900,
        "35-39".FML 700, "35-39".MLE 900,
        "40-44".FML 700, "40-44".MLE 900,
        "45-49".FML 700, "45-49".MLE 900,
        "50-54".FML 700, "50-54".MLE 900,
        "55-59".FML 700, "55-59".MLE 900,
        "60-64".FML 700, "60-64".MLE 900,
        "65-69".FML 700, "65-69".MLE 900,
        "70-74".FML 700, "70-74".MLE 900,
        "75-79".FML 700, "75-79".MLE 900,
        "80+".FML 700, "80+".MLE 900 /

    VitaminCReq(age,sex) /
        "0-4".FML 20, "0-4".MLE 20,
        "5-9".FML 37.5, "5-9".MLE 37.5,
        "10-14".FML 75, "10-14".MLE 75,
        "15-19".FML 105, "15-19".MLE 90,
        "20-24".FML 110, "20-24".MLE 95,
        "25-29".FML 110, "25-29".MLE 95,
        "30-34".FML 110, "30-34".MLE 95,
        "35-39".FML 110, "35-39".MLE 95,
        "40-44".FML 110, "40-44".MLE 95,
        "45-49".FML 110, "45-49".MLE 95,
        "50-54".FML 110, "50-54".MLE 95,
        "55-59".FML 110, "55-59".MLE 95,
        "60-64".FML 110, "60-64".MLE 95,
        "65-69".FML 110, "65-69".MLE 95,
        "70-74".FML 110, "70-74".MLE 95,
        "75-79".FML 110, "75-79".MLE 95,
        "80+".FML 110, "80+".MLE 95 /

    VitaminB6Req(age,sex) /
        "0-4".FML 0.4, "0-4".MLE 0.4,
        "5-9".FML 0.6, "5-9".MLE 0.6,
        "10-14".FML 1.2, "10-14".MLE 1.2,
        "15-19".FML 1.2, "15-19".MLE 1.6,
        "20-24".FML 1.2, "20-24".MLE 1.5,
        "25-29".FML 1.2, "25-29".MLE 1.5,
        "30-34".FML 1.2, "30-34".MLE 1.5,
        "35-39".FML 1.2, "35-39".MLE 1.5,
        "40-44".FML 1.2, "40-44".MLE 1.5,
        "45-49".FML 1.2, "45-49".MLE 1.5,
        "50-54".FML 1.2, "50-54".MLE 1.5,
        "55-59".FML 1.2, "55-59".MLE 1.5,
        "60-64".FML 1.2, "60-64".MLE 1.5,
        "65-69".FML 1.2, "65-69".MLE 1.5,
        "70-74".FML 1.2, "70-74".MLE 1.5,
        "75-79".FML 1.2, "75-79".MLE 1.5,
        "80+".FML 1.2, "80+".MLE 1.5 /

    PotassiumReq(age,sex) /
        "0-4".FML 1792, "0-4".MLE 1792,
        "5-9".FML 2300, "5-9".MLE 2340,
        "10-14".FML 2300, "10-14".MLE 2600,
        "15-19".FML 2360, "15-19".MLE 3080,
        "20-24".FML 2600, "20-24".MLE 3400,
        "25-29".FML 2600, "25-29".MLE 3400,
        "30-34".FML 2600, "30-34".MLE 3400,
        "35-39".FML 2600, "35-39".MLE 3400,
        "40-44".FML 2600, "40-44".MLE 3400,
        "45-49".FML 2600, "45-49".MLE 3400,
        "50-54".FML 2600, "50-54".MLE 3400,
        "55-59".FML 2600, "55-59".MLE 3400,
        "60-64".FML 2600, "60-64".MLE 3400,
        "65-69".FML 2600, "65-69".MLE 3400,
        "70-74".FML 2600, "70-74".MLE 3400,
        "75-79".FML 2600, "75-79".MLE 3400,
        "80+".FML 2600, "80+".MLE 3400 /
;