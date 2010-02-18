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
from mercurial import ui, repo, cmdutil, context

def background_color(original, hue):
    saturation = 35
    color = QtGui.QColor()
    color.setHsv( hue, saturation, original.value() )
    return  color

class StatusItem(object):
    Modified = 'M'
    Removed = 'R'
    Deleted = 'D'
    Added = 'A'
    Unknown = '?'
    Ignored = 'I'
    Clean = 'C'
    StatusTypes = [Modified, Added, Removed, Deleted, Unknown, Ignored, Clean]

    ''' Container for Status Information 

    Represents a single item in the status model.
    '''
    def __init__(self, path, status):
        self.path = path
        self.status = status

class StatusFiller(QtCore.QThread):
    ''' Fills the status model with StatusItem objects using a thread '''
    def __init__(self, repo, rev1, rev2, model):
        QtCore.QThread.__init__(self)
        self._repo = repo
        self._rev1 = rev1
        self._rev2 = rev2
        self._model = model

        self._initial_chunk = 10 
        self._row_chunk = 25

    def run(self):
        ''' Does the actual work of reading in the status from the
            mercurial repository and filling in the status model
        '''
        self._repo.dirstate.invalidate()
        self._repo.invalidate()

        items = []
        node1, node2 = [None, None]
        if self._rev1:
            ctx = self._repo[self._rev1]
            node1 = ctx.node()
            if not self._rev2:
                realparents = ctx.parents()
                if realparents:
                    node2 = realparents[0].node()
            elif isinstance(self._rev2, str):
                ctx = self._repo[self._rev2]
                node2 = ctx.node()
            else:
                node2 = self._rev2.node()

        matcher = cmdutil.match(self._repo, [], {})
        modified, added, removed, deleted, unknown, ignored, clean = [
            n for n in self._repo.status(node1=node1, node2=node2,
                                 match=matcher,
                                 ignored=True,
                                 clean=True,
                                 unknown=True)]

        status = (modified,added,removed,deleted,unknown,ignored,clean)

        for state in zip(StatusItem.StatusTypes, status):
            for path in state[1]:
                items.append(StatusItem(path, state[0]))
        self._model.appendRows(items)

class StatusModel(QtCore.QAbstractTableModel):
    ''' Mercurial Status Model

    Represents the information about file status at a given 
    changeset. Allows Qt4 Views to display the status data in a abstract item view.
    '''

    def __init__(self, ui, repo, rev1=None, rev2=None, parent=None):
        QtCore.QAbstractTableModel.__init__(self, parent)
        self._ui = ui
        self._repo = repo
        self._rev1 = rev1
        self._rev2 = rev2
        self._data = []
        self._columns = ['Status', 'File']
        self.filler = StatusFiller(self._repo, self._rev1, self._rev2, self)
        self.filler.start()
        assert self.connect(self.filler, QtCore.SIGNAL("finished()"),self.emitFinished)

    def emitFinished(self):
         self.emit(QtCore.SIGNAL("fillFinished"))

    def appendRows(self, items):
        ''' Method called by the StatusFiller thread to add items '''
        self.beginInsertRows(QtCore.QModelIndex(), max(self.rowCount(),0), self.rowCount() + len(items))
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
            if self._columns[index.column()] == 'Status' :
                return QtCore.QVariant(str(self._data[index.row()].status))

            if self._columns[index.column()] == 'File':
                return QtCore.QVariant(str(self._data[index.row()].path))

        elif role == QtCore.Qt.BackgroundColorRole:
            background = QtGui.QApplication.palette().color(QtGui.QPalette.Base)
            if self._data[index.row()].status == StatusItem.Modified:
                return QtCore.QVariant(background_color(background, 240))
            elif self._data[index.row()].status == StatusItem.Added:
                return QtCore.QVariant(background_color(background, 118))
            elif self._data[index.row()].status == StatusItem.Removed:
                return QtCore.QVariant(background_color(background, 0))
            elif self._data[index.row()].status == StatusItem.Deleted:
                return QtCore.QVariant(background_color(background, 277))
            elif self._data[index.row()].status == StatusItem.Unknown:
                return QtCore.QVariant(background_color(background, 32))
            elif self._data[index.row()].status == StatusItem.Ignored:
                return QtCore.QVariant(background_color(background, 59))
            elif self._data[index.row()].status == StatusItem.Clean:
                return QtCore.QVariant(background_color(background, 118))
            else:
                return QtCore.QVariant(background)
            
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

