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
from mercurial import ui, repo, commands, hg, node, util
from vcsmodel import HistoryModel, HistoryGraphDelegate #,StatusModel
from dialogs import Ui_mergeRevisionSelection, Ui_mergeConflictResolution, Ui_mergeCommit

class MergeRunner(object):
    """run a merge operation in a thread"""

class MergeRevisionSelectionPage( QtGui.QWizardPage, Ui_mergeRevisionSelection):
    def __init__(self, ui, repo):
        page = QtGui.QWizardPage.__init__(self)
        self.setTitle("Merge Revision Selection")
        self.setupUi(self)
 
class MergeConflictResolutionPage( QtGui.QWizardPage, Ui_mergeConflictResolution):
    def __init__(self, ui, repo):
        QtGui.QWizardPage.__init__(self)
        self.setTitle("Merge Conflict Resolution")
        self.setupUi(self)
    
class MergeCommitPage( QtGui.QWizardPage, Ui_mergeCommit):
    def __init__(self, ui, repo):
        QtGui.QWizardPage.__init__(self)
        self.setTitle("Merge Commit")
        self.setupUi(self)

class MergeDialog(QtGui.QWizard):
    def __init__(self, ui, repo, **opts):
        QtGui.QWizard.__init__(self)
        self.setWindowTitle("Mercurial Merge")
        
        self._ui = ui
        self._repo = repo
        
        self.addPage(MergeRevisionSelectionPage(ui, repo))
        self.addPage(MergeConflictResolutionPage(ui, repo))
        self.addPage(MergeCommitPage(ui, repo))
        self.show()

if __name__ == "__main__":
    import os, sys
    from utils import *
    app = QtGui.QApplication( sys.argv )
    u = ui.ui()
    u.updateopts(debug=True, traceback=True)
    path = len(sys.argv) > 1 and sys.argv[1] or os.getcwd()
    path = findrepo(os.path.abspath(path))
    repo = hg.repository(u, path)
    dialog = MergeDialog(u, repo)
    sys.exit(app.exec_())

