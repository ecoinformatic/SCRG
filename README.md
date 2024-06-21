# SCRG
This repository contains materials broadly related to the Statewide Coastal Restoration Guide (SCRG) project.

* [LINK](https://drive.google.com/drive/folders/1dxKXUQkoeD7i3vd2cvBl1YHHgM1Fhbe7) to Ecoinformatics folder on google drive. 
  * [Data Download Checklist](https://docs.google.com/spreadsheets/d/1Wl3nfnRXcvrp4kp9PQSOQI2tOGlfD9ojrsxOuezafHk/edit#gid=0)
  * [SCRG IQAP Table 3](https://docs.google.com/spreadsheets/d/1jZmUNlY68Eb_SUPPAjriNE0UJUHFtkh_xXRLeNGBI_g/edit#gid=0)

## Set up
### Python Development
So far there isn't a specific version of python necessary, but we recommend >=
3.11. It is also recommended that Python development proceed within a virtual environment to ensure that the code be replicable across machines. Here is the code necessary to set up a python virtualenv and
-install the python dependencies.

```bash
# This creates the virtual environment directory where all the python
# dependencies are saved. The path saved at the end can be saved in
# the SCRG directory as `scrg_pyenv` as this is in the .gitignore file.
python -m venv path/to/scrg_pyenv
## The following lines must be run in the SCRG directory.
# This activates virtual env in LINUX ONLY
source path/to/scrg_pyenv/bin/activate
# This activates virtual env in WINDOWS ONLY
activate
# With the virtual env activated, install the python dependencies
pip install -r requirements.txt
```
### R Development
There is currently no required version of R, but it is recommended that 
the version be > 4, if not >= 4.3. Make sure that your R environment has the following packages:

```r
install.package(sf)
install.package(stringdist)
install.package(nnet)
install.package(testthat)
install.package(devtools)
```
