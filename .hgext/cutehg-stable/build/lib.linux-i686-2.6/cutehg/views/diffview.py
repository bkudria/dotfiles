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
from ui_statuslayout import *
from models import DiffModel, DiffHighlighter
import qtutil

class DiffView(QtGui.QWidget):
    def __init__(self, ui, repo, rev1=None, rev2=None, filename=None, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.vboxLayout = QtGui.QVBoxLayout(self)
        self.setLayout(self.vboxLayout)
        self.textEdit = QtGui.QTextEdit()
        self.textEdit.setReadOnly(True)
        f = qtutil.getFixedFont()
        if f: self.textEdit.setFont(f)
        self.diffHighlighter = DiffHighlighter(self.textEdit.document())
        self.vboxLayout.addWidget(self.textEdit)
        self.model = None
        self._ui = ui
        self._repo = repo
        self.update(rev1, rev2, filename)

    def update(self, rev1=None, rev2=None, filename=""):
        ''' recreates the model without destroying the view
            if rev1 and rev2 are none the diff of the working dir to the parent of it is shown
            if rev1 is something and rev2 is none, the diff between rev1 and its first parent is shown
            if rev1 and rev2 are something then the diff between the two is shown
        '''
        self.model = DiffModel(self._ui, self._repo, rev1, rev2, filename, self)
        assert self.connect(self.model, QtCore.SIGNAL("fillFinished()"), self.displayDiff)
        assert self.connect(self.model, QtCore.SIGNAL("selectionChanged(int)"), self.onSelectionChanged)
        self.displayDiff() # in case the thread finishes before the connection happens

    def onSelectionChanged(self, rev):
        self.emit(QtCore.SIGNAL("selectionChanged(int)"), rev)
    
    def displayDiff(self):
        self.textEdit.clear()
        rows = self.model.rowCount()
        for row in xrange(0, rows):
            index = self.model.index(row, 0)
            diff = str(self.model.data(index, QtCore.Qt.DisplayRole).toString())
            self.textEdit.append(diff)

