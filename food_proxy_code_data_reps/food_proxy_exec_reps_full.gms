;
lpio-[];    *-------------------------------------------------------------------------------
*        Food proxy based on anthropometric measures, dietary surveys, and
*        waste-adjusted food availability data
*-------------------------------------------------------------------------------

*        version: 30 March 2025
*        contact: M Springmann, marco.springmann@ouce.ox.ac.uk

*-------------------------------------------------------------------------------
*        Run-time parameters
*-------------------------------------------------------------------------------

*        enable multiple threads:
option threads=8;

*-------------------------------------------------------------------------------
*        Setup
*-------------------------------------------------------------------------------

*        set directories for code, inputs, output:
$setglobal code_dir        code_reps_full
$setglobal input_dir       input_reps_full
$setglobal output_dir      output_reps_full

*        create output directory and back up code:
$call      mkdir           %output_dir%
$call      mkdir           %output_dir%/%code_dir%
$call      cp              %gams.input%      %output_dir%/%code_dir%
$call      cp              %code_dir%/*.gms  %output_dir%/%code_dir%

*-------------------------------------------------------------------------------
*        Controls
*-------------------------------------------------------------------------------

*        run analysis only for one year and set year of analysis (yes/no or 1/0):
$setglobal year_sel        yes
$setglobal year            2020

*        run analysis for one country (yes/no or 1/0):
$setglobal set_ctr         no
$setglobal ctr_sel         CHN

*-------------------------------------------------------------------------------
*        Develop food proxy for single year
*-------------------------------------------------------------------------------


*        load sets:
$include %code_dir%/prxy_sets_full.gms

*        load data:
$include %code_dir%/prxy_data_full.gms

*        compile food proxy:
$include %code_dir%/prxy_cons_full.gms

$exit
