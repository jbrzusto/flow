# Radar installation at Cape Sharp Lighthouse for FORCE #

## Radar ##

- **model**: Furuno FR8252
- **antenna**: 6' open array
- **owner**: Phil Taylor's lab at Acadia University

### Installation ###

- mounted on custom-built wooden platform atop pre-existing
concrete pad, on seaward side of auxilliary building.  Platform
is weighed down by 400 lbs of sand bags, wrapped in heavy
contractor waste bags.

- radar cable is threaded through corrugated pool-sweeper hose
for UV and animal protection.  Cable enters lighthouse via
slightly-enlarged pre-existing hole.

- radar power supply, digitizer, and capture computer are in
lighthouse, plugged into AC power.

### Digitizer / Capture Computer ###

"digdar" is a radar digitizing / capture system with these features:

- digitizer is redpitaya with 125 MSPS 14-bit dual channels, one
used for video, the other for trigger; ARP/ACP are input via slow
12-bit ADCs, sampling @ ~ 100 kSPS.  125 MSPS allows for a range-cell
size as small as ~ 1.2 metres

- capture computer is Raspberry Pi Model 3B+, connected to digitizer
via 300 Mbit/s ethernet

- capture computer is running modified software from sensorgnome.org,
based on Raspbian.

### Data Flow ###

- radar rotation rate: ~ 24 RPM
- pulse repetition frequency: ~ 2100 Hz
- pulse length: ~ 80 ns
- digitizer rate: 62.5 MSPS (basic rate decimated by 2)
- samples digitized per pulse: 2500
- range coverage: 6000 metres
- azimuth coverage: full sweep
- 129 sweeps (~ 5 minutes) captured at each 15 minute clock interval
  throughout the day.  i.e. 00:00, 00:15, 00:30, ... 23:45

- sweeps saved on external hard drive in hierarchy of folders named
  `YYYY-MM-DD/HH/`, with filenames like
  `csh-2018-05-25T01-45-02.050Z.jpg`; `csh` is the short identifier
  for 'Cape Sharp', just as 'fvc' is the short identifier for 'FORCE
  Visitor Centre'

- onboard clock kept in sync to UTC via PPS GPS

- approximate storage rotation schedule: every 10 days for a single
  4TB drive

### Data Processing on FORCE HQ Server ###

The USB hard drive from Cape sharp is plugged into the FORCE HQ Server.
In these notes, we assume the drive is mounted at `/media/shared`

There are two scripts installed on FORCE HQ, both from the R package
`flow`, by this author.

**digdar2jpg**: scan convert one or more raw sweep files to jpg
images.  This is useful for getting a quick summary of each 15 minute
period, to determine which data are worth exporting to .pol format

Examples:

```bash
# print details of command-line parameters:
$ digdar2jpg -h
...
# switch to a data folder:
$ cd /media/shared/2018-05-24/16

# scan-convert the first sweep (.dat or .dat.gz) file in the folder:
$ digdar2jpg
./csh-2018-05-24T16-00-00.737Z.jpg     # the output filename is printed

# convert the first sweep at the quarter hour:
$ digdar2jpg T16-15
./csh-2018-05-24T16-15-02.242Z.jpg

# scan-conversion uses parameters specified in
# /usr/lib/R/site-library/flow/csh_radar_site.json
# You can make a copy of that file as e.g. ~/my_csh.json, change it, then ask
# digdar2jpg to use it on the first sweep at the half hour:
$ digdar2jpg -s ~/my_csh.json T16-30

# you can convert a set of 129 sweeps at the 3/4 hour mark,
# then make and view a quick movie of them:
$ digdar2jpg -n 129 T16-45
...  # prints a list of jpg filenames
$ cat csh-2018-05-24T16-[45]*.jpg | \
ffmpeg -f image2pipe -codec mjpeg -i pipe:0 csh2018-05-24T1645.mp4
$ mplayer -loop 0 csh2018-05-24T1645.mp4

# generate a single jpg for each 15 minute segment dataset on the disk:
# (I've already done this on the current USB drive)
$ cd /media/shared
$ for x in 2018-05*
  do
    cd $x
    for y in [0-9]*; do
      cd $y
      for t in 0 1 3 4; do
          digdar2jpg T$y-$t  # picks out the first sweep whose timestamp
                             # matches the appropriate quarter-hour
      done
      cd ..
  done
  cd ..
done

# you can view a .jpg using e.g.
$ eog /media/shared/2018-05-26/16/csh-2018-05-24T16-00-00.737Z.jpg
```

**makepol**: export a sequence of raw sweep files as a WAMOS-format .pol file

This uses a similar syntax to `digdar2jpg`, but always works with a sequence
of `.dat(.gz)` files.  By default, it generates then bzip2-compresses the .pol
file, but the `-u` option can be used to skip compression.

Examples:

```bash
# print details of command-line parameters:
$ makepol -h
...
# switch to a data folder:
$ cd /media/shared/2018-05-24/16

# generate a compressed .pol.bz2 file for the first 129 sweeps in the folder:
$ makepol
# switch to a data folder:
$ cd /media/shared/2018-05-24/16

# use 129 sweep files to generate an uncompressed .pol file
$ makepol -u -n 129
./20180616200002fvc.pol  # the name of the output file is printed

# if the data files for a radar don't have the correct prefix for the site,
# makepol won't be able to guess what radar site file to use, so you can
# specify one on the command line. Currently, the radar at FORCE VC records
# files beginning with "FORCEVC-" instead of simply "fvc-".
# Here, we generate a compressed .pol file starting at 16:45 from sweeps
# in the current folder (which would have to be a folder with sweeps
# from FVC, which only exist on the laptop at FORCE VC).
$ makepol -n 129 -s /usr/lib/R/site-library/flow/fvc_radar_site.json T16-45

```

### CAUTION:  unvalidated code ###

Source code for the R flow package is in /home/johnb/flow on the
FORCE HQ server.

Because I don't have any code that handles `.pol` files, I can't
tell whether `makepol` is doing sensible things for the Cape Sharp
data.  However, I *have* verified that `makepol` and `digdar2jpg`
work correctly on a 129-sweep segment of data from the FORCE VC radar.
("correctly" in the sense that the same image and .pol file are produced
as are being generated on the FORCE VC radar laptop and sent to FORCE HQ.
This is non-trivial because the code here has changed considerably.)

Inputs and output for that verification are on the FORCE HQ server here:

```bash
$ cd /home/radar_upload/test_export_wamos_files
$ ls -al *pol
-rw-rw-r-- 1 johnb johnb 578987557 Jun 18 11:30 20180616200002fvc.original.pol
-rw-rw-r-- 1 johnb johnb 578987557 Jun 19 14:03 20180616200002fvc.pol

# the original pol file was obtained like so:
$ FNAME=20180616200002fvc
$ bunzip -c /mnt/raid1/radar/fvc/2018/06-16/$FNAME.pol.bz2 > $FNAME.original.pol

# verification that headers differ only in a version string:
$ for x in 2018*.pol
  do
     head -181l $x > $x.hdr
  done
$ diff *hdr
4c4
< VERSN Jul 03 2015 00:00:00 CC COMPILATION DATE AND TIME
---
> VERSN Jun 13 2018 00:00:00 CC COMPILATION DATE AND TIME

# verification that binary data are identical:
$ for x in 2018*pol
  do
     echo $x
     tail -n +182 $x | md5sum
  done
20180616200002fvc.original.pol
3da3ed3c661a31894f2e7fb6f1ad1c4f  -
20180616200002fvc.pol
3da3ed3c661a31894f2e7fb6f1ad1c4f  -
```
