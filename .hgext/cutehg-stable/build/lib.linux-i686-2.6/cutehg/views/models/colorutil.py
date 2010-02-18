#!/usr/bin/env python
# -*- coding: utf-8 -*-

# CuteHg - A Qt4 Dialog Extension to Mercurial
# Copyright (C) 2009  Tom Burdick <thomas.burdick@gmail.com> 
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from PyQt4 import QtCore, QtGui

def colorset(bg, fg, ncolors):
    colors = []
    hue_base = 0
    if bg.hue() == -1:
        hue_base = 90
    else:
        hue_base = bg.hue()
    
    h = 0
    s = 0
    v = 0

    for i in xrange(0, ncolors):
        h = int(hue_base + (360.0 / ncolors * i)) % 360;
        s = 240
        v = int(max(bg.value(), fg.value()) * 0.85);

        # take care of the corner cases
        M = 35;
        if (h < bg.hue() + M and h > bg.hue() - M) or (h < fg.hue() + M and h > fg.hue() - M):
            h = ((bg.hue() + fg.hue()) / (i + 1)) % 360
            s = ((bg.saturation() + fg.saturation() + 2 * i) / 2) % 256
            v = ((bg.value() + fg.value() + 2 * i) / 2) % 256

        colors.append(QtGui.QColor.fromHsv(h, s, v))

    return colors;