## Test environments

* local R installation, aarch64-apple-darwin20, R 4.4.0
* macOS 14.5 (on Github), R 4.4.1
* Microsoft Windows Server 2022 10.0.20348 (on Github), R 4.4.1
* Ubuntu 22.04.4 (on Github), R 4.4.1

## R CMD check results

0 errors | 0 warnings | 0 notes

* This release is expected to create errors with older versions of manynet,
but should create no errors with the latest version
* Currently binaries of manynet have been built for most flavors, with the exception of r-release-windows-x86_66
* There is expected to be test errors for this flavor until a manynet binary for v1.0.1 has been built
* Since manynet is a Depends for migraph, this should not pose a problem for users
upgrading
