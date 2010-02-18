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
from models import StatusModel
from models.statusmodel import StatusItem

class StatusView(QtGui.QWidget, Ui_StatusLayout):
    def __init__(self, ui, repo, rev=None, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.setupUi(self)
        self.models = []
        self.editFilters = []
        self.stateFilters = []
        self._showStates = []
        self._ui = ui
        self._repo = repo
        self.update(rev)

    def update(self, rev=None):
        self.models = []
        self.stateFilters = []
        parents = []
        realparents = parents
        if rev:
            context = self._repo[rev]
            realparents = context.parents()
            parents = realparents
        else:
            context = self._repo[rev]
            realparents = context.parents()
            parents = [None]

        self.models.append(StatusModel(self._ui, self._repo, rev, parents[0] , self))
        assert self.connect(self.models[0], QtCore.SIGNAL("fillFinished()"), self.resizeTable1Cells)
        if len(parents) > 1:
            self.models.append(StatusModel(self._ui, self._repo, rev, parents[1], self))
            assert self.connect(self.models[1], QtCore.SIGNAL("fillFinished()"), self.resizeTable2Cells)

        for model in self.models:
            editFilter = QtGui.QSortFilterProxyModel(self)
            editFilter.setSourceModel(model)
            editFilter.setFilterKeyColumn(1)
            self.editFilters.append(editFilter)
            assert self.connect(self.filterEdit, QtCore.SIGNAL("textChanged(QString)"), editFilter.setFilterWildcard)
            stateFilter = QtGui.QSortFilterProxyModel(self)
            stateFilter.setSourceModel(editFilter)
            stateFilter.setFilterKeyColumn(0)
            self.stateFilters.append(stateFilter)
        
        self.parent1TableView.setModel(self.stateFilters[0])
        self.parent1Label.setText(str(realparents[0]))
        if len(realparents) > 1:
            self.parent2Label.setText(str(realparents[1]))
            self.parent2TableView.setModel(self.stateFilters[1])
            self.parent2Container.setVisible(True)
        else:
            self.parent2Container.setVisible(False)

        stateChanged = QtCore.SIGNAL("stateChanged(int)")
        assert self.connect(self.modifiedCheckBox, stateChanged, self.filterModified)
        assert self.connect(self.addedCheckBox, stateChanged, self.filterAdded)
        assert self.connect(self.removedCheckBox, stateChanged, self.filterRemoved)
        assert self.connect(self.deletedCheckBox, stateChanged, self.filterDeleted)
        assert self.connect(self.unknownCheckBox, stateChanged, self.filterUnknown)
        assert self.connect(self.ignoredCheckBox, stateChanged, self.filterIgnored)
        assert self.connect(self.cleanCheckBox, stateChanged, self.filterClean)
        self.modifiedCheckBox.setCheckState(QtCore.Qt.Checked)
        self.addedCheckBox.setCheckState(QtCore.Qt.Checked)
        self.removedCheckBox.setCheckState(QtCore.Qt.Checked)
        self.deletedCheckBox.setCheckState(QtCore.Qt.Checked)
        self.unknownCheckBox.setCheckState(QtCore.Qt.Checked)

        self.resizeTable1Cells()
        self.resizeTable2Cells()
        self.resetStateFilter()


    def resizeTable1Cells(self):
        self.parent1TableView.resizeColumnsToContents()
        headerView = self.parent1TableView.horizontalHeader()
        headerView.setStretchLastSection(True)

    def resizeTable2Cells(self):
        self.parent2TableView.resizeColumnsToContents()
        headerView = self.parent2TableView.horizontalHeader()
        headerView.setStretchLastSection(True)
    
    def resetStateFilter(self):
        self._showStates = ['M', 'A', 'R',  '?', '!']

        exp = "["
        for char in self._showStates:
            exp = exp + char 
        exp = exp + "]"

        for proxy in self.stateFilters:
            proxy.setFilterRegExp(exp)

    def updateStateFilter(self, state, char):
        addchar = False
        regexp = "["
        if state == QtCore.Qt.Checked:
            addchar = True

        if addchar:
            self._showStates.append(char)
        else:
            self._showStates.remove(char)

        for char in self._showStates:
            regexp = regexp + char

        regexp = regexp + "]"

        for proxy in self.stateFilters:
            proxy.setFilterRegExp(regexp)


    def filterModified(self, state):
        self.updateStateFilter(state, StatusItem.Modified)

    def filterAdded(self, state):
        self.updateStateFilter(state, StatusItem.Added)

    def filterRemoved(self, state):
        self.updateStateFilter(state, StatusItem.Removed)

    def filterDeleted(self, state):
        self.updateStateFilter(state, StatusItem.Deleted)

    def filterUnknown(self, state):
        self.updateStateFilter(state, StatusItem.Unknown)

    def filterIgnored(self, state):
        self.updateStateFilter(state, StatusItem.Ignored)

    def filterClean(self, state):
        self.updateStateFilter(state, StatusItem.Clean)

