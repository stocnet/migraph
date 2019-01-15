# Contributing

Contributions to `roctopus`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

## Issues and tests

Please use the issues tracker on Bitbucket to identify any function-related issues.
You can use these issues to track progress on the issue and 
to comment or continue a conversation on that issue.
Currently issue tracking is only open to those involved in the project.

The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

## Bug fixing or adding new code

Independent or assigned code contributions are most welcome.
When writing new code, please follow 
[standard R guidelines](https://www.r-bloggers.com/🖊-r-coding-style-guide/). 
It can help to use packages such as `lintr`, `goodpractice` and `formatR` 
to ensure these are followed.

Currently, commits can only be pushed to Bitbucket where they reference an existing issue.
If no issue exists for the code you have developed, please add an issue first before pushing.
Once the issue exists, you will need to mention the issue number (preceded by a hash symbol: #)
in the commit description:

``` Resolved #31 by adding a new function that does things, also updated documentation ```

Where the issue hash (i.e. #31) is preceded by
`resolve`, `resolves`, `resolved`, `close`, `closes`, `closed`, `fix`, `fixes`, or `fixed`
(capitalised or not),
Bitbucket will automatically updated the status of the issue(s) mentioned.
More details can be found [here](https://confluence.atlassian.com/bitbucket/resolve-issues-automatically-when-users-push-code-221451126.html).
To set this up for Mac or Windows, see [this link](https://confluence.atlassian.com/sourcetreekb/link-to-bitbucket-issue-tracker-from-commits-296911608.html).

Our current syntactical standard is to mention the issue first and then 
provide a short description of what the committed changes do 
in relation to that issue.
Any ancillary changes can be mentioned after a comma.

## Documentation

A final way of contributing to the package is in developing the 
vignettes/articles that illustrate the value added in the package. 
Please contact me with any proposals here.

Please note that the `roctopus` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

