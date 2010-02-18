#!/usr/bin/env python

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

import re
from datetime import timedelta
from PyQt4 import QtCore, QtGui
from mercurial import ui, repo, graphmod, util, templatefilters
from historydecorator import HistoryDecorator
from abstractmodel import AbstractItem, TableColumns

class HistoryItem(AbstractItem):
    ''' replacement for the data tuple, at some point the tuple just gets to be too tedious
    '''
    def __init__(self, context, column, color, edges, author_color=None):
        AbstractItem.__init__(self, context)
        self.hash = context.hex()
        self.column = column
        self.color = color
        self.edges = edges
        self.author_color = author_color

class HistoryFiller(QtCore.QThread):
    ''' Fills history using a thread instead of a timer '''
    def __init__(self, repository, model):
        QtCore.QThread.__init__(self)
        self._model = model
        self._repository = repository
        self._initial_chunk = 50
        self._row_chunk = 200 

    def run(self):
        decorator = HistoryDecorator()
        # read in the entire repository, updating the model every so often with a chunk of them
        row_chunk = self._initial_chunk 
        rows = []
        for (context, (column, color), edges) in graphmod.graph(self._repository, len(self._repository), 0):
            item = HistoryItem(context, column, color, edges)
            rows.append(item)

            if(len(rows) == row_chunk):
                row_chunk = self._row_chunk
                self._model.appendRows(rows)
                rows = []
                # sleep for a little to let the ui run a bit, darn 
                self.msleep(100)
            
        # last chunk
        self._model.appendRows(rows)

class HistoryModel(QtCore.QAbstractTableModel):

    GraphRole = QtCore.Qt.UserRole + 1
    RevisionRole = GraphRole + 1
    CommitRole = RevisionRole + 1

    def __init__(self, ui, repository, node=None):
        QtCore.QAbstractTableModel.__init__(self)
        self._ui = ui
        self._repository = repository
        self._node = node
        self._author_re = re.compile('<.*@.*>', 0)
      
        self.columns = TableColumns()
        self.columns.addColumn("Rev")
        self.columns.addColumn("Graph")
        self.columns.addColumn("Branch")
        self.columns.addColumn("Tags")
        self.columns.addColumn("Date")
        self.columns.addColumn("Author")
        self.columns.addColumn("Message")

        self._max_cols = 0 
        self._max_color = 0
        self._data = []
        self._threadedFiller = HistoryFiller(self._repository, self)
        self.connect(self._threadedFiller, QtCore.SIGNAL("finished()"), self.emitFinished)
        self._threadedFiller.start()

    def emitFinished(self):
        self.emit(QtCore.SIGNAL("fillFinished()"))

    def appendRows(self, items):
        self.beginInsertRows(QtCore.QModelIndex(), max(self.rowCount(),0), self.rowCount() + len(items) - 1)
        for item in items:
            self._data.append(item)
            self._max_cols = max(len(item.edges), self._max_cols) 
            self._max_color = max(item.color, self._max_color)
        self.endInsertRows()

    def flags(self, index):
        return QtCore.Qt.ItemIsSelectable

    def data(self, index, role):
        if not index.isValid() or index.column() > self.columnCount() or index.row() > self.rowCount():
            return QtCore.QVariant()

        item = self._data[index.row()+1]

        if index.column() == self.columns.getIndex("Graph") and role == HistoryModel.GraphRole:
            pitem = self._data[index.row()]
            return (item.column, item.color, item.edges, pitem.edges, self._max_cols, self._max_color)

        if role == HistoryModel.RevisionRole:
            return item 

        if role == QtCore.Qt.DisplayRole:
            if index.column() == self.columns.getIndex("Rev"):
                return QtCore.QVariant(item.revid)

            if index.column() == self.columns.getIndex("Author"):
                  return QtCore.QVariant(item.author)

            if index.column() == self.columns.getIndex("Message"):
                return QtCore.QVariant(item.message)
            
            if index.column() == self.columns.getIndex("Date"):
                return QtCore.QVariant(str(item.age))
            
            if index.column() == self.columns.getIndex("Tags"):
                tagstr = ""
                for tag in item.tags:
                    tagstr = str(tag) + " "

                return QtCore.QVariant(tagstr)

            if index.column() == self.columns.getIndex("Branch"):
               return QtCore.QVariant(item.branch)

        return QtCore.QVariant()
    
    def headerData(self, section, orientation, role):
        if orientation == QtCore.Qt.Horizontal and role == QtCore.Qt.DisplayRole:
            return QtCore.QVariant(self.columns.getName(section))
        return QtCore.QVariant()
    
    def rowCount(self, parentIndex = QtCore.QModelIndex()):
        if parentIndex.isValid():
            assert False
        
        return len(self._data) - 1
            
    def columnCount(self, parentIndex = QtCore.QModelIndex()):
        if parentIndex.isValid():
            assert False
        return self.columns.getNumberOfColumns()
