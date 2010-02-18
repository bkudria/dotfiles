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
from dialogs import Ui_UpdateDialog
from views import HistoryView
from runners import UpdateRunner 

class UpdateDialog(QtGui.QDialog, Ui_UpdateDialog):
    def __init__(self, ui, repo, **opts):
        QtGui.QDialog.__init__(self)
        self.setupUi(self)
        self.historyView = HistoryView(ui, repo)
        self.verticalLayout.insertWidget(0, self.historyView)

        self._ui = ui
        self._repo = repo
        self._testThread = None
        self._runThread = None
        self._command = None

        assert self.connect(self.actionUpdate, QtCore.SIGNAL("triggered()"), self.onUpdate)
        assert self.connect(self.actionCancel, QtCore.SIGNAL("triggered()"), QtCore.QCoreApplication.quit)
   
    def disable(self):
        self.updateButton.setEnabled(False)
        self.cancelButton.setEnabled(False)
        
    def enable(self):
        self.updateButton.setEnabled(True)
        self.cancelButton.setEnabled(True)
   
    def onUpdate(self):
        self.disable()
        self._revision = self.historyView.selectedRevision()
        self._runner = UpdateRunner( self.enable, self, self._ui, self._repo, self._revision, False)


if __name__ == "__main__":
    import os
    import sys
    import utils

    from mercurial import ui, repo, commands, hg, node, util

    app = QtGui.QApplication( sys.argv )
    u = ui.ui()
    u.updateopts(debug=False, traceback=False)
    path = len(sys.argv) > 1 and sys.argv[1] or os.getcwd()
    path = utils.findrepo(os.path.abspath(path))
    repo = hg.repository(u, path)
    
    dialog = UpdateDialog(u, repo)
    dialog.show()

    import cProfile
    cProfile.run('app.exec_()', 'appprof')
    
    import pstats
    p = pstats.Stats('appprof')
    p.strip_dirs().sort_stats(-1).print_stats()



