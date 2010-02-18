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
from ui_historylayout import Ui_HistoryLayout
from models import HistoryModel, SearchProxy, HistoryGraphDelegate

class HistoryView(QtGui.QWidget, Ui_HistoryLayout):
    ''' A Mercurial History View that displays history.
    '''
    def __init__(self, ui, repository, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.setupUi(self)

        self._ui = ui
        self._repository = repository

        self._historyModel = HistoryModel( self._ui, self._repository )
        self.selectionModel = QtGui.QItemSelectionModel(self._historyModel)
        self._historyGraphDelegate = HistoryGraphDelegate()
        self._searchProxy = SearchProxy(self.selectionModel, self._historyModel)
        self._resizeRowsJob = ResizeRowsJob(self.tableView)

        self.tableView.setModel(self._historyModel)
        self.tableView.setItemDelegateForColumn( self._historyModel.columns.getIndex("Graph"), self._historyGraphDelegate )
        self.tableView.setSelectionModel(self.selectionModel)
        verticalHeader = self.tableView.verticalHeader()
        verticalHeader.setResizeMode(QtGui.QHeaderView.Fixed)
        
        for column in xrange(self._historyModel.columns.getNumberOfColumns()):
            if self._historyModel.columns.getName(column) == "Graph":
                continue
            self.findFieldsCombo.addItem(self._historyModel.columns.getName(column))

        # connect actions to methods 
        assert self.connect(self.findNextButton, QtCore.SIGNAL("clicked()"),  self._searchProxy.findNext)
        assert self.connect(self.findPreviousButton, QtCore.SIGNAL("clicked()"), self._searchProxy.findPrevious)
        assert self.connect(self.findText, QtCore.SIGNAL("textChanged(QString)"), self._searchProxy.setExpression)
        assert self.connect(self._historyModel, QtCore.SIGNAL("fillFinished()"), self.resizeTableCells)
        assert self.connect(self.selectionModel, QtCore.SIGNAL("currentChanged(QModelIndex,QModelIndex)"), self.onSelectionChanged)
        #assert self.connect(self.findFieldsCombo, QtCore.SIGNAL("currentIndexChanged(const QString&)"), self.onFindFieldChanged)

        # in case its actualy finished filling before the connection happened, resize columns before showing
        self.resizeTableCells()

    def resizeTableCells(self):
        '''do a resize of things, this is very slow if things get big'''
        self.tableView.resizeColumnsToContents()
        headerView = self.tableView.horizontalHeader()
        headerView.setStretchLastSection(True)
        
        # begin a tableView row resize job
        #self._resizeRowsJob.run(self._historyModel.rowCount())

    def onSelectionChanged(self):
        self.emit(QtCore.SIGNAL("selectionChanged(int)"), int(self.selectedRevision()) )

    def selectedRevision(self):
        ''' return the selected revision '''
        index = self.selectionModel.currentIndex()
        index = self._historyModel.index(index.row(), self._historyModel.columns.getIndex('Rev'), QtCore.QModelIndex())
        return str(self._historyModel.data(index, QtCore.Qt.DisplayRole).toString())


class ResizeRowsJob(QtCore.QObject):
    def __init__(self, view):
        QtCore.QObject.__init__(self)
        
        self._timer = QtCore.QTimer()
        self._timer.setSingleShot(True)
        self.connect(self._timer, QtCore.SIGNAL("timeout()"), self.resizeChunk)
        self._view = view
        self._rowcount = 0 
        self._currentIndex = 0
        self.CHUNK_SIZE = 25 

    def run(self, rowcount):
        self._rowcount = rowcount
        self._currentIndex = 0
        self._timer.start(20)

    def resizeChunk(self):
        for x in xrange(self._currentIndex, min(self._currentIndex + self.CHUNK_SIZE, self._rowcount)):
           self._view.resizeRowToContents(x)

        self._currentIndex = self._currentIndex + self.CHUNK_SIZE

        if self._currentIndex != self._rowcount:
            self._timer.start(20)


