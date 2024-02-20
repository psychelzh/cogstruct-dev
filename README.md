# cogstruct-dev

Cognitive structure analysis project based on [CAMP](https://github.com/CAMP-BNU).

## Projects

Initially, targets package is used in the expect that we could use one pipeline to do all the work. Reality is that there are many different aspects of analysis joined together. The following projects are included:

- `prepare_source_data` and `prepare_source_data_retest`: these two are used to do ETL jobs and some light-weighted analysis (for example: test-retest reliability in `prepare_source_data_retest`).
- `explore_factors` and `confirm_factors`: these two are used to do factor analysis to determine the real cognitive structure analysis jobs. Note they are dependent on the `prepare_source_data` project.
- `optimize_efficiency`: task selection for quicker assessment.
- `g_factor`: Analysis especially for g-factor (number of tasks).
- `prepare_neural`: prepare functional connectivity (FC) data.
- `predict_phenotypes`: predict all the factors based on FC data.
