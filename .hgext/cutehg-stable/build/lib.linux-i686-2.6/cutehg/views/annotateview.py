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

from PyQt4 import QtCore, QtGui
from ui_annotatelayout import Ui_AnnotateLayout
from models import AnnotateModel
from mercurial import revlog, repo
import qtutil

class AnnotateView(QtGui.QWidget, Ui_AnnotateLayout):
    def __init__(self, ui, repository, rev, path, follow, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.setupUi(self)

        self._ui = ui
        self._repository = repository
        self._rev = rev or "tip"
        self._path = path
        self._follow = follow

        self._history = []
        self._historyIndex = -1
 
        f = qtutil.getFixedFont()
        if (f):
            self.tableView.setFont(f)
            self.status.setFont(f)
        
        self.setupModel()

        self.setupEvents()

    def setupModel(self):
        self.setModel(AnnotateModel(self._ui, self._repository,
                self._rev, self._path, self._follow))

        self._historyIndex = self._historyIndex + 1
        self._history = self._history[:self._historyIndex]
        self._history.append((self._rev, self._annotateModel))

    def setModel(self, annotateModel):
        self._annotateModel = annotateModel
        self.selectionModel = QtGui.QItemSelectionModel(self._annotateModel)

        self.tableView.setModel(self._annotateModel)
        self.tableView.setSelectionModel(self.selectionModel)

        self.revision.setText(str(self._rev))

        self.styleTableCells()
        self.emit(QtCore.SIGNAL("selectionChanged()"))

    def setupEvents(self):
        self._row = 0
        def mouseMoveEvent(ev):
            QtGui.QTableView.mouseMoveEvent(self.tableView, ev)
            pos = ev.pos()
            row = self.tableView.rowAt(pos.y())
            self._row = row
            self._annotateModel.setrevrow(row)
        self.tableView.mouseMoveEvent = mouseMoveEvent
        self.tableView.setMouseTracking(True)

        self.tableView.addAction(self.actionGotoRev)
        assert self.connect(self.actionGotoRev, QtCore.SIGNAL("triggered()"),
                lambda: self.gotoRev(self.hoverRevisionId(), -1))

        assert self.connect(self.tableView, QtCore.SIGNAL("clicked(const QModelIndex&)"),
                self.selectionChanged)
        assert self.connect(self.tableView, QtCore.SIGNAL("doubleClicked(const QModelIndex&)"),
                lambda i: self.gotoParent())

    def selectionChanged(self, index):
        self.status.setText(self._annotateModel.data(index, QtCore.Qt.ToolTipRole).toString())
        self.emit(QtCore.SIGNAL("selectionChanged()"))

    def canBack(self):
        return self._historyIndex >= 1

    def back(self):
        if not self.canBack():
            return
        self._historyIndex = self._historyIndex - 1
        entry = self._history[self._historyIndex]
        self._rev = entry[0]
        self.setModel(entry[1])

    def canGotoParent(self, rev = None):
        try:
            ctx = self._repository[rev or self.selectedRevision()]
            ctx = ctx.parents()[0]
            fctx = ctx[self._path]
            return True
        except:
            return False

    def gotoParent(self):
        if not self.canGotoParent():
            return
        self.gotoRev(self.selectedRevision(), -1)

    def gotoRev(self, rev, offset = 0):
        error = None
        oldrev = self._rev
        try:
            ctx = self._repository[rev]
            while offset < 0:
                offset = offset + 1
                ctx = ctx.parents()[0]
            self._rev = ctx.rev()
            self.setupModel()
        except revlog.LookupError:
            error = "%s does not exist in the requested revision" % self._path
        except repo.RepoError:
            error = "%s is not a valid revision" % str(rev)

        if error:
            em = QtGui.QErrorMessage(self)
            em.showMessage(error)

            # ensure a valid model
            self._rev = oldrev
            self.setupModel()

    def styleTableCells(self):
        self.tableView.resizeColumnsToContents()
        headerView = self.tableView.horizontalHeader()
        headerView.setStretchLastSection(True)

        for ix in xrange(0, self._annotateModel.rowCount()):
            self.tableView.setRowHeight(ix, 14)

    def hoverRevisionId(self):
        index = self._annotateModel.createIndex(self._row, 0)
        return self._annotateModel.data(index, AnnotateModel.RevisionIdRole)

    def selectedRevision(self):
        ''' return the selected revision '''
        index = self.selectionModel.currentIndex()
        return self._annotateModel.data(index, AnnotateModel.RevisionIdRole)


