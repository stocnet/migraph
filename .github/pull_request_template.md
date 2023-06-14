# Description

# Checklist:

- Documentation
  - [ ] Any longer functions are commented inline so that it is easier to debug in the future
  - [ ] Any new or modified functions or data have roxygen style documentation in their .R scripts
  - [ ] DESCRIPTION file version is bumped by the appropriate increment (major, minor, patch)
- PR form
  - [ ] Title indicates expected version number
  - [ ] PR description above and the NEWS.md file are aligned
  - [ ] Any closed, fixed, or related issues are referenced and described, e.g. "Fixed #0 by adding A"
- PR checks all pass for latest commit
  - [ ] Package builds on my OS without issues
  - [ ] CodeFactor check: Package improves or maintains good style
  - [ ] Package builds on Mac
  - [ ] Package builds on Windows
  - [ ] Package builds on Linux
  - [ ] CodeCov check: Package improves or maintains good test coverage
