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


from dialogs import Ui_CommitDialog
from PyQt4 import QtCore, QtGui
from mercurial import hg
from views import CommitView

class CommitDialog(QtGui.QMainWindow, Ui_CommitDialog):
    def __init__(self, ui, repo, paths):
        QtGui.QMainWindow.__init__(self)
        self.setupUi(self)

        self._ui = ui
        self._repo = repo
        self._runner = None
        self._revision = None

        self.setWindowTitle("Commit")

        self.commitView = CommitView(ui, repo, paths)
        self.setCentralWidget(self.commitView)

if __name__ == "__main__":
    import os
    import sys
    import utils
    import cProfile
    from mercurial import ui, cmdutil, patch

    app = QtGui.QApplication( sys.argv )
    u = ui.ui()
    u.updateopts(debug=False, traceback=False)
    path = utils.findrepo(os.path.abspath('.'))
    repo = hg.repository(u, path)

    node1, node2 = cmdutil.revpair(repo, [])
    m = cmdutil.match(repo, ['.'], {})
    chunks = list(patch.diff(repo, node1, node2, match=m))
    for c in chunks:
        print "((((((((((((((("
        print c
        print ")))))))))))))))"
    
    dialog = CommitDialog(u, repo, [path])
    dialog.show()

    cProfile.run('app.exec_()', 'appprof')
    import pstats
    p = pstats.Stats('appprof')
    p.strip_dirs().sort_stats(-1).print_stats()

