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
from mercurial import ui, repo, patch, cmdutil

class DiffItem(object):

    ''' Container for Diff Chunk Information 

    Represents a chunk in the diff model.
    '''
    def __init__(self, text):
        self.text = text

class DiffFiller(QtCore.QThread):
    ''' Fills the status model with StatusItem objects using a thread '''
    def __init__(self, ui, repo, rev1, rev2, filename, model):
        QtCore.QThread.__init__(self)
        self._ui = ui
        self._repo = repo
        self._rev1 = rev1
        self._rev2 = rev2
        self._filename = filename
        self._model = model

        self._initial_chunk = 10 
        self._row_chunk = 25

    def run(self):
        ''' Does the actual work of reading in the status from the
            mercurial repository and filling in the status model
        '''
        items = []
        opts = {'':self._filename}
        m = cmdutil.match(self._repo, [], {})
        
        node1, node2 = [None, None]
        if self._rev1:
            ctx = self._repo[self._rev1]
            node1 = ctx.node()
            if not self._rev2:
                realparents = ctx.parents()
                if realparents:
                    node2 = realparents[0].node()
            else:
                ctx = self._repo[self._rev2]
                node2 = ctx.node()

        it = patch.diff(self._repo, node2, node1, match=m, opts=patch.diffopts(self._ui, opts))
        
        for chunk in it:
            items.append(DiffItem(chunk))

        self._model.appendRows(items)

class DiffModel(QtCore.QAbstractTableModel):
    ''' Mercurial Status Model

    Represents the information about file status at a given 
    changeset. Allows Qt4 Views to display the status data in a abstract item view.
    '''

    def __init__(self, ui, repo, rev1=None, rev2=None, filename="", parent=None):
        QtCore.QAbstractTableModel.__init__(self, parent)
        self._ui = ui
        self._repo = repo
        self._rev1 = rev1
        self._rev2 = rev2
        self._filename = filename
        self._data = []
        self._columns = ['Diff']
        self.filler = DiffFiller(self._ui, self._repo, self._rev1, self._rev2, self._filename, self)
        self.connect(self.filler, QtCore.SIGNAL("finished()"), self.emitFinished)
        self.filler.start()

    def emitFinished(self):
        self.emit(QtCore.SIGNAL("fillFinished()"))

    def appendRows(self, items):
        ''' Method called by the StatusFiller thread to add items '''
        self.beginInsertRows(QtCore.QModelIndex(), max(self.rowCount(),0), self.rowCount() + len(items) )
        for item in items:
            self._data.append(item)
        self.endInsertRows()

    def flags(self, index):
        ''' Returns flags for a given index '''
        return QtCore.Qt.ItemIsSelectable

    def data(self, index, role):
        ''' Returns the index data wrapped '''
        if not index.isValid() or index.column() > self.columnCount() or index.row() > self.rowCount():
            return QtCore.QVariant()

        if role == QtCore.Qt.DisplayRole:
            if self._columns[index.column()] == 'Diff':
                return QtCore.QVariant(str(self._data[index.row()].text))
        
        return QtCore.QVariant()

    def headerData(self, section, orientation, role):
        if orientation == QtCore.Qt.Horizontal and role == QtCore.Qt.DisplayRole:
            return QtCore.QVariant(self._columns[section])
        return QtCore.QVariant()

    def rowCount(self, parentIndex = QtCore.QModelIndex()):
        if parentIndex.isValid():
            assert False
        return len(self._data)

    def columnCount(self, parentIndex = QtCore.QModelIndex()):
        if parentIndex.isValid():
            assert False
        return len(self._columns)

