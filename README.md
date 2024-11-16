# MplusBootstrap
An R package to automate bootstrapping for various mediation and moderated mediation analysis scenarios.

## Installation
```R
install.packages('MplusBootstrap_0.1.0.tar.gz', type='source')
```

## Dependencies
MplusAutomation, dplyr, tidyr, MASS

## Functions
This package provides two main functions: 

1. **load_model(file_name)**

This function loads model results from Mplus output file. If these exists no output file, this function will try to run the input file and reads the output file then. 

User must specify a filename or a path where the Mplus input or output file locates. 

2. **MplusBootstrap(Model, type, first_path, second_path=NULL, multi_level, iv=NULL, m=NULL, w=NULL, xw=NULL, dv=NULL, first_stage_mod=NULL, level_one_mod=NULL, output='Untitled', rep=20000, conf=95)**

This function provides a set of Monte Carlo simulations to bootstrap the mediation or moderated mediation or mediated moderation effect from the Mplus resluts. The template of the monte carlo method in this function is from https://quantpsy.org/medmc/medmc.htm.

User must specify the model, model type ('med', 'mod-med' or 'med-mod'), first/second path ('fixed' or 'path name), multi_level (TRUE or FALSE), and nessacary variable names that will be used. The variable names are case-insensitive. 
Some examples are given below. 

Please refer to [description](man/MplusBootstrap.Rd) for more details. 

## Examples
The example code and output file can be found in [tests/testthat](tests/testthat). 
```R
  library(MplusBootstrap)
  
  # Single Level Mediation
  model <- load_model(file_name = testthat::test_path('examples/example_single'))
  MplusBootstrap(Model=model, type="med", first_path="fixed", second_path="fixed",
                 multi_level=FALSE, iv="age", m="income", w=NULL, xw=NULL, dv="pain",
                 first_stage_mod=NULL, level_one_mod=NULL,
                 output='single', rep=20000, conf=95)

  # Multi-Level Mediation
  ## Fixed-Fixed
  model <- load_model(file_name = testthat::test_path('examples/fixed_fixed'))
  MplusBootstrap(Model=model, type="med", first_path="fixed", second_path="fixed",
                 multi_level=TRUE, iv="x", m="m", w=NULL, xw=NULL, dv="y",
                 first_stage_mod=NULL, level_one_mod=NULL,
                 output='fixed_fixed', rep=20000, conf=95)

  model <- load_model(file_name = testthat::test_path('examples/Q2_fixed'))
  MplusBootstrap(Model=model, type="med", first_path="fixed", second_path="fixed",
                 multi_level=TRUE, iv="lmx", m="eff", w=NULL, xw=NULL, dv="jobperf",
                 first_stage_mod=NULL, level_one_mod=NULL,
                 output='Q2_fixed', rep=20000, conf=95)

  ## Random-Random
  model <- load_model(file_name = testthat::test_path('examples/random_random'))
  MplusBootstrap(Model=model, type="med", first_path="s1", second_path="s2",
                 multi_level=TRUE, iv=NULL, m=NULL, w=NULL, xw=NULL, dv=NULL,
                 first_stage_mod=NULL, level_one_mod=NULL,
                 output='random_random', rep=20000, conf=95)

  model <- load_model(file_name = testthat::test_path('examples/Q2_random'))
  MplusBootstrap(Model=model, type="med", first_path="s12", second_path="s2",
                 multi_level=TRUE, iv=NULL, m=NULL, w=NULL, xw=NULL, dv=NULL,
                 first_stage_mod=NULL, level_one_mod=NULL,
                 output='Q2_random', rep=20000, conf=95)

  # Multi-Level Moderated-Mediation
  ## Fixed-Fixed (Level 1)
  model <- load_model(file_name = testthat::test_path('examples/modmed_alllevel1'))
  MplusBootstrap(Model=model, type="mod-med", first_path="fixed", second_path="fixed",
                 multi_level=TRUE, iv="x", m="m", w="w", xw="cxw", dv="y",
                 first_stage_mod=TRUE, level_one_mod=TRUE,
                 output='modmed_alllevel1', rep=20000, conf=95)

  ## Random-Fixed (Level 1)
  model <- load_model(file_name = testthat::test_path('examples/modmed'))
  MplusBootstrap(Model=model, type="mod-med", first_path="s1", second_path="fixed",
                 multi_level=TRUE, iv="tmx", m="eff", w="lmx", xw="cxw", dv="jbp",
                 first_stage_mod=TRUE, level_one_mod=TRUE,
                 output='modmed_rf_level1', rep=20000, conf=95)

  ## Random-Random (Level 2)
  model <- load_model(file_name = testthat::test_path('examples/modmed_rr_level2'))
  MplusBootstrap(Model=model, type="mod-med", first_path="s1", second_path="s2",
                 multi_level=TRUE, iv=NULL, m=NULL, w="w", xw=NULL, dv=NULL,
                 first_stage_mod=TRUE, level_one_mod=FALSE,
                 output='modmed_rr_level2', rep=20000, conf=95)

  ## Random-Fixed (Level 2)
  model <- load_model(file_name = testthat::test_path('examples/modmed'))
  MplusBootstrap(Model=model, type="mod-med", first_path="s1", second_path="fixed",
                 multi_level=TRUE, iv="tmx", m="eff", w="lmx_mean", xw="cxw", dv="jbp",
                 first_stage_mod=TRUE, level_one_mod=FALSE,
                 output='modmed_rf_level2', rep=20000, conf=95)

  # Multi-Level Mediated-Moderation
  # Level 1 Moderator
  model <- load_model(file_name = testthat::test_path('examples/medmod_level1'))
  MplusBootstrap(Model=model, type="med-mod", first_path=NULL, second_path=NULL,
                 multi_level=TRUE, iv=NULL, m="m", w="w", xw="cxw", dv="y",
                 first_stage_mod=NULL, level_one_mod=TRUE,
                 output='medmod_level1', rep=20000, conf=95)

  # Level 2 Moderator
  model <- load_model(file_name = testthat::test_path('examples/medmod_level2'))
  MplusBootstrap(Model=model, type="med-mod", first_path='s', second_path=NULL,
                 multi_level=TRUE, iv=NULL, m="m", w="w", xw=NULL, dv=NULL,
                 first_stage_mod=NULL, level_one_mod=FALSE,
                 output='medmod_level2', rep=20000, conf=95)

  model <- load_model(file_name = testthat::test_path('examples/Q2'))
  MplusBootstrap(Model=model, type="med-mod", first_path='s', second_path=NULL,
                 multi_level=TRUE, iv=NULL, m="norm", w="teamsize", xw=NULL, dv=NULL,
                 first_stage_mod=NULL, level_one_mod=FALSE,
                 output='Q2_teamsize', rep=20000, conf=95)

  model <- load_model(file_name = testthat::test_path('examples/Q2'))
  MplusBootstrap(Model=model, type="med-mod", first_path='s', second_path=NULL,
                 multi_level=TRUE, iv=NULL, m="norm", w="gclim", xw=NULL, dv=NULL,
                 first_stage_mod=NULL, level_one_mod=FALSE,
                 output='Q2_gclim', rep=20000, conf=95)
```
