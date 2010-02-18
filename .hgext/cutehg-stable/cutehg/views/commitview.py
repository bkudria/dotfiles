#! /usr/bin/env python

# CuteHg - A Qt4 Dialog Extension to Mercurial
# Copyright (C) 2009  Stefan Rusek <stefan@rusek.org>
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  US

from PyQt4 import QtCore, QtGui
import qtutil
from commitmessageview import CommitMessageView

class CommitView(QtGui.QWidget):
    '''
    This view is a compound view

        *----------*--------*
        | Commit   | Tree   |
        | Message  | View   |
        | View     |        |
        *----------*--------*
        | Diff              |
        | View              |
        *-------------------*

    The CommitMessageView is Modelless
    The TreeView uses a CommitModel
    The DiffView takes a model created by the CommitModel

    '''
    def __init__(self, ui, repo, paths, text='', parent=None):
        QtGui.QWidget.__init__(self, parent)
        
        vert = QtGui.QSplitter(QtCore.Qt.Vertical)
        horz = QtGui.QSplitter(QtCore.Qt.Horizontal, vert)

        self._message = CommitMessageView(text, horz)
        self._status = QtGui.QTreeView(horz)
        self._diff = QtGui.QTextEdit(vert)

        l = QtGui.QVBoxLayout(self)
        l.addWidget(vert)
        self.setLayout(l)


if __name__ == '__main__':

    class dlg(QtGui.QDialog):
        def __init__(self):
            QtGui.QDialog.__init__(self)
            l = QtGui.QVBoxLayout(self)
            l.addWidget(CommitView(None, None, None))
            self.setLayout(l)

    import sys
    app = QtGui.QApplication( sys.argv )
    dialog = dlg()
    dialog.show()
    sys.exit(app.exec_())

