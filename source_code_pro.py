#!/usr/bin/python

"""Downloads and installs Adobe SourceCode Pro."""

import StringIO
import os
import platform
import sys
import urllib2
import zipfile

def main():
  if platform.system() != 'Linux':
    print >> sys.stderr, ('%s only knows how to install fonts on Linux' %
                          sys.argv[0])
    sys.exit(1)

  # Create the user font directory if it does not exist
  font_dir = os.path.join(os.environ['HOME'], '.fonts')
  if not os.path.exists(font_dir):
    os.mkdir(font_dir)

  # Retrieve the font archive
  url = ('http://downloads.sourceforge.net/project/sourcecodepro.adobe/' +
         'SourceCodePro_FontsOnly-1.017.zip')
  download = urllib2.urlopen(url)

  # Unzip the fonts
  with zipfile.ZipFile(StringIO.StringIO(download.read()), 'r') as zip_file:
    for filename in zip_file.namelist():
      if filename.endswith('.otf'):
        with zip_file.open(filename, 'r') as input_font_file:
          dest_path = os.path.join(font_dir, os.path.basename(filename))
          with open(dest_path, 'w') as output_font_file:
            print dest_path
            output_font_file.write(input_font_file.read())

  # Update the font cache
  os.execlp('fc-cache', '-f', '-v')
  

if __name__ == '__main__':
  main()
