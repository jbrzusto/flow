# flow
This grab-bag package provides two key scripts for users of digdar:

 - makepol: export a sequence of digdar .dat.gz files as a WAMOS-format .pol file
 - digdar2jpg: export one or more digdar files as jpeg images
 
 The scripts are in the package `inst` directory.  After installing the package,
 you can create links to the scripts in `/usr/local/bin` by doing:
 
 ```bash
 Rscript -e "flow:::postInstall()"
 ```
 
 This requires `sudo` privileges.
 
