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
from abstractmodel import AbstractItem, TableColumns

class AnnotateItem(AbstractItem):
    def __init__(self, context, text, previtem):
        AbstractItem.__init__(self, context)

        self.text = text
        if not previtem:
            self.odd = True
        elif  previtem.revid != self.revid:
            self.odd = previtem and not previtem.odd
        else:
            self.odd = previtem.odd

    def summary(self):
        return "User: %s\nDate: %s\nSummary:\n%s" % (
                        self.author,
                        self.age,
                        self.fullmessage)
        
class AnnotateModel(QtCore.QAbstractTableModel):

    RevisionRole = QtCore.Qt.UserRole + 1
    RevisionIdRole = RevisionRole + 1

    def __init__(self, ui, repo, rev, path, follow):
        QtCore.QAbstractTableModel.__init__(self)

        ctx = repo[rev]
        fctx = ctx[path]
        lines = fctx.annotate(follow=follow)
        self._rows = []
        self._revs = {}
        self._colors = {}
        self.selectedrevid = -1

        item = None
        for n, text in lines:
            item = AnnotateItem(n, text, item)
            self._rows.append(item)
            revs = self._revs.get(item.revid)
            if not revs:
                self._revs[item.revid] = [item]
            else:
                revs.append(item)

        for revid, revs in self._revs.iteritems():
            pass

        self.columns = TableColumns()
        self.columns.addColumn("Rev")
        self.columns.addColumn("User")
        self.columns.addColumn("Line")
        self.columns.addColumn("File")

    def setrevrow(self, row):
        old = self.selectedrevid
        self.selectedrevid = self._rows[row].revid
        if old != self.selectedrevid:
            self.emit(QtCore.SIGNAL('layoutChanged()'))

    def flags(self, index):
        return QtCore.Qt.ItemIsSelectable
        
    def data(self, index, role):
        if not index.isValid() or index.column() > self.columnCount() or index.row() > self.rowCount():
            return QtCore.QVariant()

        item = self._rows[index.row()]

        if role == QtCore.Qt.ToolTipRole:
            return QtCore.QVariant(item.summary())

        if role == QtCore.Qt.TextAlignmentRole:
            if (index.column() == self.columns.getIndex("Rev") or 
                    index.column() == self.columns.getIndex("Line")):
                return QtCore.QVariant(int(QtCore.Qt.AlignRight
                    | QtCore.Qt.AlignVCenter))
           
        if role == QtCore.Qt.BackgroundRole:
            if (index.column() == self.columns.getIndex("Rev") or
                    index.column() == self.columns.getIndex("User")):
                if item.revid != self.selectedrevid:
                    color = QtGui.QColor(255, 255, 255)
                else:
                    color = QtGui.QColor(234, 234, 255)

            if index.column() == self.columns.getIndex("Line"):
                color = QtGui.QColor(218, 218, 218)

            if index.column() == self.columns.getIndex("File"):
                if item.odd:
                    color = QtGui.QColor(234, 255, 255)
                else:
                    color = QtGui.QColor(234, 234, 255)

            return QtCore.QVariant(QtGui.QBrush(color))

        if role == AnnotateModel.RevisionRole:
            return item 
        if role == AnnotateModel.RevisionIdRole:
            return item.revid

        if role == QtCore.Qt.DisplayRole:
            if index.column() == self.columns.getIndex("Rev"):
                return QtCore.QVariant(item.revid)

            if index.column() == self.columns.getIndex("User"):
                return QtCore.QVariant(item.author)

            if index.column() == self.columns.getIndex("Line"):
                return QtCore.QVariant(index.row() + 1)

            if index.column() == self.columns.getIndex("File"):
                return QtCore.QVariant(item.text)

        return QtCore.QVariant()
    
    def headerData(self, section, orientation, role):
        if orientation == QtCore.Qt.Horizontal and role == QtCore.Qt.DisplayRole:
            return QtCore.QVariant(self.columns.getName(section))
        return QtCore.QVariant()
    
    def rowCount(self, parentIndex = QtCore.QModelIndex()):
        if parentIndex.isValid():
            assert False
        return len(self._rows)
            
    def columnCount(self, parentIndex = QtCore.QModelIndex()):
        if parentIndex.isValid():
            assert False
        return self.columns.getNumberOfColumns()
