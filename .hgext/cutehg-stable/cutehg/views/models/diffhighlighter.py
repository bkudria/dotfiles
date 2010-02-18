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
import colorutil
from PyQt4 import QtCore, QtGui

class DiffHighlighter(QtGui.QSyntaxHighlighter):
    def __init__(self, parent=None):
        QtGui.QSyntaxHighlighter.__init__(self, parent)
        palette = QtGui.QApplication.palette()
        
        self.colors = colorutil.colorset(palette.color(QtGui.QPalette.Base), palette.color(QtGui.QPalette.Text), 2)
        self.hbg = QtGui.QColor(255, 247, 152)
        self.hfg = QtGui.QColor(QtCore.Qt.black)
        self.hcolors = colorutil.colorset(self.hbg, self.hfg, 4)

        self.removeFmt = QtGui.QTextCharFormat()
        self.removeFmt.setForeground(self.colors[0])
        self.addFmt = QtGui.QTextCharFormat()
        self.addFmt.setForeground(self.colors[1])
        self.statFmt = QtGui.QTextCharFormat()
        self.statFmt.setForeground(self.hcolors[0])
        self.statFmt.setBackground(self.hbg)
        self.fromFileFmt = QtGui.QTextCharFormat()
        self.fromFileFmt.setForeground(self.hcolors[1])
        self.fromFileFmt.setBackground(self.hbg)
        self.toFileFmt = QtGui.QTextCharFormat()
        self.toFileFmt.setForeground(self.hcolors[2])
        self.toFileFmt.setBackground(self.hbg)
        self.diffFmt = QtGui.QTextCharFormat()
        self.diffFmt.setForeground(self.hcolors[3])
        self.diffFmt.setBackground(self.hbg)
       
        self.removeRegExp = QtCore.QRegExp('^[-].*')
        self.addRegExp = QtCore.QRegExp('^[+].*')
        self.diffStatRegExp = QtCore.QRegExp('^[@][@].*')
        self.fromFileRegExp = QtCore.QRegExp('^[-][-][-].*')
        self.toFileRegExp = QtCore.QRegExp('^[+][+][+].*')
        self.diffRegExp = QtCore.QRegExp('^diff .*')


    def highlightBlock(self, text):
        if text.indexOf(self.fromFileRegExp) >= 0:
            self.setFormat(0, text.length(), self.fromFileFmt)
        elif text.indexOf(self.toFileRegExp) >= 0:
            self.setFormat(0, text.length(), self.toFileFmt)
        elif text.indexOf(self.removeRegExp) >= 0:
            self.setFormat(0, text.length(), self.removeFmt)
        elif text.indexOf(self.addRegExp) >= 0:
            self.setFormat(0, text.length(), self.addFmt)
        elif text.indexOf(self.diffStatRegExp) >= 0:
            self.setFormat(0, text.length(), self.statFmt)
        elif text.indexOf(self.diffRegExp) >= 0:
            self.setFormat(0, text.length(), self.diffFmt)
