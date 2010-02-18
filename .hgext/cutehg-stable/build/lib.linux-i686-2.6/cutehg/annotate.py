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

import pdb
import cProfile
from dialogs import Ui_AnnotateDialog
from PyQt4 import QtCore, QtGui
from mercurial import ui, repo, commands, hg, node, util
from views import AnnotateView

class AnnotateDialog(QtGui.QMainWindow, Ui_AnnotateDialog):
    def __init__(self, ui, repo, rev, path, follow=False):
        QtGui.QMainWindow.__init__(self)
        self.setupUi(self)

        self._ui = ui
        self._repo = repo
        self._runner = None
        self._revision = None

        self.setWindowTitle("Annotate - " + path)

        self.annotateView = AnnotateView(ui, repo, rev, path, follow)
        self.setCentralWidget(self.annotateView)
        self.selectionChanged()

        assert self.connect(self.actionGotoParent, QtCore.SIGNAL("triggered()"),
                self.annotateView.gotoParent)
        assert self.connect(self.actionBack, QtCore.SIGNAL("triggered()"),
                self.annotateView.back)
        assert self.connect(self.annotateView, QtCore.SIGNAL("selectionChanged()"),
                self.selectionChanged)

    def selectionChanged(self):
        self.actionGotoParent.setEnabled(self.annotateView.canGotoParent())
        self.actionBack.setEnabled(self.annotateView.canBack())


if __name__ == "__main__":
    import os
    import sys
    import utils
    from mercurial import cmdutil

    app = QtGui.QApplication( sys.argv )
    u = ui.ui()
    u.updateopts(debug=False, traceback=False)
    if sys.argv < 1:
        print "at least one path required"
        sys.exit()
    path = sys.argv[1]
    path = utils.findrepo(os.path.abspath(path))
    repo = hg.repository(u, path)
    
    ctx = repo['tip']
    m = cmdutil.match(repo, sys.argv[1:], {})
    dialogs = []
    for abs in ctx.walk(m):
        dialog = AnnotateDialog(u, repo, 'tip', abs, True)
        dialog.show()
        dialogs.append(dialog)

    cProfile.run('app.exec_()', 'appprof')
    import pstats
    p = pstats.Stats('appprof')
    p.strip_dirs().sort_stats(-1).print_stats()

