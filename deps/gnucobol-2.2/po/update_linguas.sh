#!/bin/sh
# shell for updating the translations before a release

# Let this be executed in the po/ subdir.
cd "$(dirname "$0")" || exit

echo "Updating translations via TP"
rsync -Lrtvz  translationproject.org::tp/latest/gnucobol/ . # || exit

# Are there now PO files that are not in svn yet?
NEWSTUFF=$(svn status | grep "^\? .*.po$" | sed -e's/\? *//')

if [ -n "${NEWSTUFF}" ]; then
    echo; echo "New languages found; updating LINGUAS ..."
    echo "# List of available languages." >LINGUAS
    echo "en@boldquot en@quot" $(printf '%s\n' *.po | LC_ALL=C sort | sed 's/\.po//g') >>LINGUAS
    echo "... and adding new files to svn:"
    for file in "${NEWSTUFF}"; do svn add $file; done
fi

echo; echo "Regenerating POT file and remerging and recompiling PO files..."

if test -f Makefile; then
  make update-po

  # Ensure that the PO files are newer than the POT.
  touch *.po

  # Compile PO files
  make

else

  echo; echo "WARNING: no Makefile available!"
  echo "remerge and compilation of PO files isn't done yet"
  echo;
fi
