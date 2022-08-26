#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} *Statistical Software should list at least one primary reference from published academic literature.*
#' @srrstats {G1.1} *Statistical Software should document whether the algorithm(s) it implements are:* - *The first implementation of a novel algorithm*; or - *The first implementation within **R** of an algorithm which has previously been implemented in other languages or contexts*; or - *An improvement on other implementations of similar algorithms in **R***.
#' @srrstats {G1.2} *Statistical Software should include a* Life Cycle Statement *describing current and anticipated future states of development.*
#' @srrstats {G1.3} *All statistical terminology should be clarified and unambiguously defined.*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#' @srrstats {G1.5} *Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications.*
#' @srrstats {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstats {G2.0a} Provide explicit secondary documentation of any expectations on lengths of inputs
#' @srrstats {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#' @srrstats {G2.1a} *Provide explicit secondary documentation of expectations on data types of all vector inputs.*
#' @srrstats {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstats {G2.3} *For univariate character input:*
#' @srrstats {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' @srrstats {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#' @srrstats {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstats {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstats {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstats {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#' @srrstats {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#' @srrstats {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#' @srrstats {G2.12} *Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either through being removed, converted to equivalent vector columns where appropriate, or some other appropriate treatment such as an informative error. This behaviour should be tested.*
#' @srrstats {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#' @srrstats {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstats {G2.14a} *error on missing data*
#' @srrstats {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstats {G2.14c} *replace missing data with appropriately imputed values*
#' @srrstats {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#' @srrstats {G4.0} *Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffices are automatically generated where not provided.*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstats {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#' @srrstats {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#' @srrstats {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' @srrstats {G5.5} *Correctness tests should be run with a fixed random seed*
#' @srrstats {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstats {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstats {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G4.10-4.12, below).*
#' @srrstats {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstats {G5.8a} *Zero-length data*
#' @srrstats {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
#' @srrstats {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstats {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstats {EA1.0} *Identify one or more target audiences for whom the software is intended*
#' @srrstats {EA1.1} *Identify the kinds of data the software is capable of analysing (see *Kinds of Data* below).*
#' @srrstats {EA1.2} *Identify the kinds of questions the software is intended to help explore.*
#' @srrstats {EA1.3} *Identify the kinds of data each function is intended to accept as input*
#' @srrstats {EA2.0} *EDA Software which accepts standard tabular data and implements or relies upon extensive table filter and join operations should utilise an **index column** system*
#' @srrstats {EA2.1} *All values in an index column must be unique, and this uniqueness should be affirmed as a pre-processing step for all input data.*
#' @srrstats {EA2.2} *Index columns should be explicitly identified, either:*
#' @srrstats {EA2.2a} *by using an appropriate class system, or*
#' @srrstats {EA2.2b} *through setting an `attribute` on a table, `x`, of `attr(x, "index") <- <index_col_name>`.*
#' @srrstats {EA2.3} *Table join operations should not be based on any assumed variable or column names*
#' Join operations and such within the code are done using tables that are generated within the routines
#' @srrstats {EA2.6} *Routines should appropriately process vector data regardless of additional attributes*
#' @srrstats {EA3.0} *The algorithmic components of EDA Software should enable automated extraction and/or reporting of statistics as some sufficiently "meta" level (such as variable or model selection), for which previous or reference implementations require manual intervention.*
#' @srrstats {EA4.0} *EDA Software should ensure all return results have types which are consistent with input types.*
#' @srrstats {EA4.1} *EDA Software should implement parameters to enable explicit control of numeric precision*
#' @srrstats {EA4.2} *The primary routines of EDA Software should return objects for which default `print` and `plot` methods give sensible results. Default `summary` methods may also be implemented.*
#' @srrstats {EA5.2} *Screen-based output should never rely on default print formatting of `numeric` types, rather should also use some version of `round(., digits)`, `formatC`, `sprintf`, or similar functions for numeric formatting according the parameter described in* **EA4.1**.
#' @srrstats {EA5.3} *Column-based summary statistics should always indicate the `storage.mode`, `class`, or equivalent defining attribute of each column.*
#' @srrstats {EA5.4} *All visualisations should ensure values are rounded sensibly (for example, via `pretty()` function).*
#' @srrstats {EA6.0} *Return values from all functions should be tested, including tests for the following characteristics:*
#' @srrstats {EA6.0a} *Classes and types of objects*
#' @srrstats {EA6.0b} *Dimensions of tabular objects*
#' @srrstats {EA6.0c} *Column names (or equivalent) of tabular objects*
#' @srrstats {EA6.0d} *Classes or types of all columns contained within `data.frame`-type tabular objects *
#' @srrstats {EA6.0e} *Values of single-valued objects; for `numeric` values either using `testthat::expect_equal()` or equivalent with a defined value for the `tolerance` parameter, or using `round(..., digits = x)` with some defined value of `x` prior to testing equality.*
#' @srrstats {PD1.0} *Software should provide references justifying choice and usage of particular probability distributions.*
#' Choice of distributions should be outlined in Masden (2015) and MacGregor et al (2018), cited in the documentation
#' @srrstats {PD2.0} *Where possible, software should represent probability distributions using a package for general representation.*
#' @srrstats {PD3.0} *Manipulation of probability distributions should very generally be analytic, with numeric manipulations only implemented with clear justification (ideally including references).*
#' @srrstats {PD3.1} *Operations on probability distributions should generally be contained within separate functions which themselves accept the names of the distributions as one input parameter.*
#' @srrstats {PD4.0} *The numeric outputs of probability distribution functions should be tested, not just output structures. These tests should generally be tests for numeric equality.*
#' @srrstats {PD4.1} *Tests for numeric equality should compare the output of of probability distribution functions with the output of code which explicitly demonstrates how such values are derived (generally defined in the same location in test files).*
#' @srrstats {PD4.2} *All functions constructed in accordance with PD2.1 - that is, which use a fixed distribution, and which name that distribution as an input parameter - should be tested using at least two different distributions.*

#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @srrstatsNA {G1.6} *Software should include code necessary to compare performance claims with alternative implementations in other R packages.*
#' G1.6 not applicable due to no other alternative R packages
#' @srrstatsNA {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' G2.4c not applicable because no conversion to character occurs
#' @srrstatsNA {G2.4d} *explicit conversion to factor via `as.factor()`*
#' G2.4d not applicable because no conversion to factor occurs
#' @srrstatsNA {G2.4e} *explicit conversion from factor via `as...()` functions*
#' G2.4e not applicable because no conversions occur that requires as functions
#' @srrstatsNA {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
#' G2.4d not applicable because no conversion to factor occurs
#' @srrstatsNA {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#' G2.9 not applicable as these conversions don't occur
#' @srrstatsNA {G3.0} *Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality.*
#' G3.0 not applicable as only floating point comparisons are for the unit tests, not for functionality
#' @srrstatsNA {G3.1} *Statistical software which relies on covariance calculations should enable users to choose between different algorithms for calculating covariances, and should not rely solely on covariances from the `stats::cov` function.*
#' G3.1 No covariance calculations in the software
#' @srrstatsNA {G3.1a} *The ability to use arbitrarily specified covariance methods should be documented (typically in examples or vignettes).*
#' G3.1a No covariance calculations in the software
#' @srrstatsNA {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' G5.0 standard datasets cannot be applied to this package
#' @srrstatsNA {G5.3} *For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested.*
#' G5.3 no missing values from functions expected to be returned
#' @srrstatsNA {G5.10} *Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` environment variable.* - The extended tests can be then run automatically by GitHub Actions for example by adding the following to the `env` section of the workflow:
#' @srrstatsNA {G5.11} *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' @srrstatsNA {G5.11a} *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsNA {G5.12} *Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspection, should be described in developer documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#' G5.10 - 5.12 - extended tests not required in this package - no large datasets or long run-times expected
#' @srrstatsNA {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#' G5.9a - the trivial noise would be the same as playing with the seed in this case as sampling is done on the standard deviation
#' @srrstatsNA {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' G5.7 not applicable as no convergence occurs here - this is mostly random sampling
#' @srrstatsNA {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.*
#' G2.16 is handled by the validate_inputs scripts
#' @srrstatsNA {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstatsNA {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstatsNA {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' G5.2 to 5.2b is handled by the validate_inputs scripts

#' @srrstatsTODO {EA2.4} *Use and demand an explicit class system for such input (for example, via the [`DM` package](https://github.com/krlmlr/dm)).*
#' Input is not multitabular
#' @srrstatsTODO {EA2.5} *Ensure all individual tables follow the above standards for Index Columns*
#' Input is not multitabular
#' @srrstatsTODO {EA3.1} *EDA software should enable standardised comparison of inputs, processes, models, or outputs which previous or reference implementations otherwise only enable in some comparably unstandardised form.*
#' No previous software or models available
#' @srrstatsNA {EA5.0} *Graphical presentation in EDA software should be as accessible as possible or practicable. In particular, EDA software should consider accessibility in terms of:*
#' No graphical outputs generated by the package
#' @srrstatsNA {EA5.0a} *Typeface sizes, which should default to sizes which explicitly enhance accessibility*
#' No graphical outputs generated by the package
#' @srrstatsNA {EA5.0b} *Default colour schemes, which should be carefully constructed to ensure accessibility.*
#' No graphical outputs generated by the package
#' @srrstatsTODO {EA5.1} *Any explicit specifications of typefaces which override default values provided through other packages (including the `graphics` package) should consider accessibility*
#' No graphical outputs generated by the package
#' @srrstatsNA {EA5.5} *All visualisations should include units on all axes where such are specified or otherwise obtainable from input data or other routines.*
#' No graphical outputs generated by the package
#' @srrstatsNA {EA5.6} *Any packages which internally bundle libraries used for dynamic visualization and which are also bundled in other, pre-existing R packages, should explain the necessity and advantage of re-bundling that library.*
#' No graphical outputs generated by the package
#' @srrstatsTODO {EA6.1} *The properties of graphical output from EDA software should be explicitly tested, for example via the [`vdiffr` package](https://github.com/r-lib/vdiffr) or equivalent.*
#' No graphical outputs generated by the package
#'
#' @srrstatsNA {PD3.2} *Use of optimisation routines to estimate parameters from probability distributions should explicitly specify and explain values of all parameters, including all uses of default parameters.*
#' No optimisation routines required
#' @srrstatsNA {PD3.3} *Return objects which include values generated from optimisation algorithms should include information on optimisation algorithm and performance, minimally including the name of the algorithm used, the convergence tolerance, and the number of iterations.*
#' No optimisation routines required
#' @srrstatsNA {PD3.4} *Use of routines to integrate probability distributions should explicitly document conditions under which integrals are expected to remain stable, and ideally include pre-processing checks for potentially unstable behaviour.*
#' No integration of probability distributions
#' @srrstatsNA {PD3.5} *Integration routines should only rely on discrete summation where such use can be justified (for example, through providing a literature reference), in which case the following applies:*
#' No integration of probability distributions
#' @srrstatsNA {PD3.5a} *Use of discrete summation to approximate integrals must demonstrate that the Reimann sum has a finite limit (or, equivalently, must explicitly describe the conditions under which the sum may be expected to be finite).*
#' No integration of probability distributions
#' @srrstatsTODO {PD4.3} *Tests of optimisation or integration algorithms should compare default results with results generated with alternative values for every parameter, including all parameters for the chosen algorithm (whether exposed as function inputs or not).*
#' No integration or optimisation of probability distributions
#' @srrstatsTODO {PD4.4} *Tests of optimisation or integration algorithms should compare equivalent results generated with at least one alternative algorithm.*
#' No integration or optimisation of probability distributions
#'
#' @noRd
NULL
