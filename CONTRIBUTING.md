# Contributing

Contributions to `roctopus`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

## Git and Bitbucket

The `gnevar` project is maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

However, one important difference from that link is that 
instead of Github or Gitlab for hosting the remote (online) git repo,
we will be using Bitbucket (which is actually very similar to Github and Gitlab in most respects).
Using git from the command line on your lap- or desktop can be intimidating,
but helpfully Atlassian (the company behind Bitbucket) have released a software interface
for Mac and Windows called [Sourcetree](https://www.sourcetreeapp.com)
that allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly full featured.

The Bitbucket page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, altough this won't be necessary after you have
cloned it on your computer via Sourcetree (if you open the file via Bitbucket, Sourcetree will
not be able to identify the changes you made).

## Sourcetree

### Cloning
Once you have downloaded Sourcetree, the first thing you have to do is to 
clone the remote repository on your computer. 
This is easily done by using point and click from Sourcetree
(file, new, clone from URL) and copying the link [clone](https://lmodoux8@bitbucket.org/jhollway/gnevar.git).
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or master. 
Working on master means you will not be altering the main workflow, whereas 
creating a develop branch will separate your work from modifications 
done on the master branch and be merged later on to the master branch.
Working on master is fine as long as there are not a lot of contributors. 
Once more people are working, it is suitable to create local branches for each 
contributor and merge them to master when controlled by the coordinator 
(via the `Pull request` that will allow other contributor to check your work 
and validate it for merging)
Note that you can still create a develop branch afterwards, 
but make sure to pull all modifications before doing so to avoid conflicts. 
You can find more information on creating a new branch on [branch](https://confluence.atlassian.com/sourcetreekb/branch-management-785325799.html). 

### Pull 
This command allows you to `pull` changes from the remote repository to your local repository on Sourcetree.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose master or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.
Note that you can access and open the files either from the Finder or from Sourcetree. 
Some document might be stored using Large File Storage (LFS) to save space on the repository. 
Sourcetree can identify them automatically by using point and click: 
repository, Git LFS, Pull LFS files

### Commit and Push
Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/master repository.
Note that you can click the "push immediately" box in the commit window 
if you don't want to do it in two steps. 

You can find more information on [commit and push](https://confluence.atlassian.com/sourcetreekb/commit-push-and-pull-a-repository-on-sourcetree-785616067.html)
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/master.

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

