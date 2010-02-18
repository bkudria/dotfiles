#! /usr/bin/env python

# CuteHg - A Qt4 Dialog Extension to Mercurial
# Copyright (C) 2009  Stefan Rusek <stefan@rusek.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  US

from PyQt4 import QtCore, QtGui

def getFixedFont():
    db = QtGui.QFontDatabase()
    def score(f):
        s = 0
        if db.isSmoothlyScalable(f):
            s = 200
        elif db.isScalable(f):
            s = 100
        s = s + len(db.smoothSizes(f, ""))
        return {'font':str(f), 'score':s}

    fonts = [score(f) for f in db.families(QtGui.QFontDatabase.Latin) if db.isFixedPitch(f)]
    fonts.sort(key=lambda o: o['score'], reverse=True)

    preferred = ['Inconsolata', 'DejaVu Sans Mono', 'Consolas', 'Lucida Console', 'Courier New', 'Courier']
    for f in fonts:
        if f['font'] in preferred:
            return QtGui.QFont(f['font'])

    if len(fonts) > 0:
        return QtGui.QFont(fonts[0]['font'])

if __name__ == '__main__':
    print getFixedFont()
