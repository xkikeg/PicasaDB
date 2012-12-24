PicasaDB
==============
Instrument Picasa DB file and export thumbnail files.

Attention
--------------
This program is for Google Picasa (TM) database, however, Google and this program has no relationship. Do not ask anything about this program to Google.
This program is distributed **without any warranty**.

How to use
--------------
### Installation ###
First, you should prepare environment such as Haskell, C++, and Python.

* C++ compiler with some C++11, such as g++ or clang.
* Glasgow Haskell Compiler and Haskell Platform and additional packages.
  * datetime
  * dta-binary-ieee754
  * csv-conduit
* Python 2

If you use Ubuntu 12.10, all you have to do is shown below.

```bash
sudo aptitude install build-essential haskell-platform
sudo aptitude install libghc-datetime-{dev,doc,prof}
sudo aptitude install libghc-data-binary-ieee754-{dev,doc,prof}
sudo aptitude install libghc-csv-conduit-{dev,doc,prof}
```

After you have completed environment preparation, you only have to do `make` in this directory.

### Usage ###
I'm very busy to write complete usage, so I show example commands.

```bash
PICASADIR=/path/to/Picasa2/db3
RAWOUTPUTDIR=/path/to/output
STRUCTUREDOUTPUTDIR=/path/to/structured
./thumbIndex $PICASADIR/thumbindex.db > thumbindex.db.tsv
for i in thumbs thumbs2 bigthumbs previews; do
./splitThumbs $PICASADIR/${i}_index.db $PICASADIR/${i}_0.db $RAWOUTPUTDIR/$i/
./renameThumbs.py ~/thumbindex.db.tsv $RAWOUTPUTDIR/$i $STRUCTUREDOUTPUTDIR/$i
done
```

License
--------------
This program is licensed with GPL3.

Acknowledgements
--------------------
The pmp file parsing part of this program owe to http://sbktech.blogspot.jp/2011/12/picasa-pmp-format.html. I used two binary editors: [Moontail](http://www.vector.co.jp/soft/winnt/util/se496567.html) and [BinEdit](http://www.vector.co.jp/soft/win95/util/se042193.html).
