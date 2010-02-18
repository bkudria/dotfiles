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

class SearchProxy(QtCore.QObject):
    ''' A search proxy is a QAbstractItemModel manipulation that allows searching.
    '''
    String = 'string'
    Glob =  'glob'
    RegExp = 'regexp'
    SearchTypes = [String, Glob, RegExp]

    def __init__(self, selectionModel, model, parent=None):
        QtCore.QObject.__init__(self, parent)
        self._model = model
        self._selectionModel = selectionModel
        self.proxyModel = QtGui.QSortFilterProxyModel(self)
        self.proxyModel.setSourceModel(self._model)
        self.proxyModel.setFilterKeyColumn(-1)
        self.search_type = SearchProxy.Glob
        self.currentRow = 0

    def setColumnSearch(self, column):
        self.proxyModel.setFilterKeyColumn(column)

    def setExpressionType(self, type):
        self.search_type = type

    def setExpression(self, expression):
        if self.search_type == SearchProxy.String:
            self.proxyModel.setFilterFixedString(expression)
        elif self.search_type == SearchProxy.Glob:
            self.proxyModel.setFilterWildcard(expression)
        elif self.search_type == SearchProxy.RegExp:
            self.proxyModel.setFilterRegexp(expression)

    def findNext(self):
        proxyIndex = self.proxyModel.mapFromSource(self._selectionModel.currentIndex())
        # use the proxy model to first filter, then track row indexes
        # if the index is valid and there are more rows, select the next one, otherwise select the first one
        nextRow = proxyIndex.row()
        if proxyIndex.isValid():
            if proxyIndex.row() + 1 > self.proxyModel.rowCount():
                nextRow = 0
            else:
                nextRow = proxyIndex.row() + 1
        else:
            nextRow = 0
        
        # update the selection
        testindex = self.proxyModel.index(nextRow, 0)
        index = self.proxyModel.mapToSource( testindex )
        self._selectionModel.setCurrentIndex(index, QtGui.QItemSelectionModel.ClearAndSelect | QtGui.QItemSelectionModel.Rows)

    def findPrevious(self):
        proxyIndex = self.proxyModel.mapFromSource(self._selectionModel.currentIndex())

        # use the proxy model to first filter, then track row indexes
        # if the index is valid and there are more rows, select the next one, otherwise select the first one
        nextRow = proxyIndex.row()
        if proxyIndex.isValid():
            if proxyIndex.row() - 1 < 0:
                nextRow = self.proxyModel.rowCount() 
            else:
                nextRow = proxyIndex.row() - 1
        else:
            nextRow = 0

        # update the selection
        index = self.proxyModel.mapToSource( self.proxyModel.index(nextRow, 0) )
        self._selectionModel.setCurrentIndex(index, QtGui.QItemSelectionModel.ClearAndSelect | QtGui.QItemSelectionModel.Rows)






